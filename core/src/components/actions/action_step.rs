use core::panic;
use std::{
    fmt::Debug,
    sync::{Arc, Mutex},
};

use hecs::Entity;
use tracing::{debug, error};

use crate::{
    components::{
        actions::{
            action::{
                ActionCondition, ActionConditionKind, ActionConditionResolution, ActionKindResult,
                ActionOutcomeBundle, ActionPayload, DamageFunction, DamageOnFailure, DamageOutcome,
                EffectOutcome, HealingFunction, HealingOutcome, PayloadDelivery,
            },
            targeting::TargetInstance,
        },
        activity::ActivityPauseReason,
        damage::DamageRollResult,
        dice::DiceSetRollResult,
        effects::effect::EffectInstanceTemplate,
        id::{EffectId, EntityIdentifier},
        modifier::{Modifiable, ModifierSource},
        spells::spell::{ConcentrationInstance, SpellFlag},
    },
    engine::{
        action_prompt::ActionData,
        event::{CallbackResult, Event, EventCallback, EventKind},
        game_state::GameState,
    },
    entities::projectile::ProjectileTag,
    registry::registry::SpellsRegistry,
    systems::{
        self,
        d20::{D20CheckDCKind, D20ResultKind},
    },
};

// TODO: Not sure what to call this guy
#[derive(Debug, Clone)]
pub struct ActionPhase {
    pub action: ActionData,
    pub steps: Vec<ActionStep>,
    pub delivery: PayloadDelivery,
    pub condition: ActionConditionKind,
    pub target: TargetInstance,
}

impl ActionPhase {
    pub fn new(
        game_state: &GameState,
        action: &ActionData,
        condition: &ActionCondition,
        payload: &ActionPayload,
        target: TargetInstance,
    ) -> Self {
        let targets =
            systems::actions::get_targeted_entities(game_state, action, Some(vec![target.clone()]));

        let steps = targets
            .iter()
            .map(|target| ActionStep::new(*target, condition, payload))
            .collect();

        Self {
            action: action.clone(),
            steps,
            delivery: payload.delivery().clone(),
            condition: condition.kind(),
            target,
        }
    }

    pub fn perform(&mut self, game_state: &mut GameState) {
        match self.delivery.clone() {
            PayloadDelivery::Immediate => {
                self.resolve_condition(game_state);
                self.resolve_payload(game_state);
                self.apply_payload(game_state);
            }

            PayloadDelivery::Projectile { template } => {
                match self.condition {
                    ActionConditionKind::None | ActionConditionKind::AttackRoll => {
                        self.resolve_condition(game_state);
                        self.resolve_payload(game_state);
                    }
                    ActionConditionKind::SavingThrow => { /* Don't resolve anything in advance */ }
                }

                let delivery_phase = ActionPhase {
                    action: self.action.clone(),
                    steps: std::mem::take(&mut self.steps),
                    delivery: PayloadDelivery::Immediate,
                    condition: self.condition.clone(),
                    target: self.target.clone(),
                };

                let projectile =
                    template.instantiate(game_state, &self.action, &self.target, delivery_phase);

                if let Ok(projectile) = projectile {
                    debug!("Instantiated projectile {:?}", projectile);
                    game_state.world.spawn((projectile, ProjectileTag));
                } else {
                    error!(
                        "Failed to instantiate projectile for action instance {:?}: {:?}",
                        self.action.instance_id,
                        projectile.err()
                    );
                }
            }
        }
    }

    pub fn resolve_condition(&mut self, game_state: &mut GameState) {
        for step in self.steps.iter_mut() {
            step.resolve_condition(game_state, &self.action);
        }
    }

    pub fn resolve_payload(&mut self, game_state: &mut GameState) {
        for step in self.steps.iter_mut() {
            step.resolve_payload(game_state, &self.action);
        }
    }

    pub fn apply_payload(&mut self, game_state: &mut GameState) {
        for step in self.steps.iter_mut() {
            step.apply_payload(game_state, &self.action);
        }
    }

    pub fn is_fully_resolved(&self) -> bool {
        self.steps.iter().all(|step| step.is_fully_resolved())
    }

    pub fn is_applied(&self) -> bool {
        self.steps.iter().all(|step| step.is_payload_applied())
    }
}

#[derive(Debug, Clone)]
pub struct ActionStep {
    pub target: Entity,
    pub condition: ActionStepCondition,
    pub payload: ActionStepPayload,
}

impl ActionStep {
    pub fn new(target: Entity, condition: &ActionCondition, payload: &ActionPayload) -> Self {
        Self {
            target,
            condition: ActionStepCondition::new(condition),
            payload: ActionStepPayload::new(payload),
        }
    }

    pub fn perform(&mut self, game_state: &mut GameState, action: &ActionData) {
        self.resolve_condition(game_state, action);
        self.resolve_payload(game_state, action);
        self.apply_payload(game_state, action);
    }

    pub fn is_condition_resolved(&self) -> bool {
        self.condition.is_resolved()
    }

    pub fn is_payload_resolved(&self) -> bool {
        self.payload.is_resolved()
    }

    pub fn is_fully_resolved(&self) -> bool {
        self.is_condition_resolved() && self.is_payload_resolved()
    }

    pub fn is_payload_applied(&self) -> bool {
        self.payload.is_applied()
    }

    pub fn resolve_condition(&mut self, game_state: &mut GameState, action: &ActionData) {
        self.condition.resolve(game_state, action, self.target);
    }

    pub fn resolve_payload(&mut self, game_state: &mut GameState, action: &ActionData) {
        let ActionStepCondition::Resolved { resolution } = &self.condition else {
            return; // Condition not yet resolved (still pending or unresolved)
        };
        self.payload.resolve(game_state, action, resolution);
    }

    pub fn apply_payload(&mut self, game_state: &mut GameState, action: &ActionData) {
        let ActionStepCondition::Resolved { resolution } = &self.condition else {
            return; // Condition not yet resolved (still pending or unresolved)
        };
        self.payload
            .apply(game_state, action, self.target, resolution);
    }
}

#[derive(Debug, Clone)]
pub enum ActionStepCondition {
    Unresolved {
        condition: ActionCondition,
    },
    PendingResolution {
        result_slot: Arc<Mutex<Option<ActionConditionResolution>>>,
    },
    Resolved {
        resolution: ActionConditionResolution,
    },
}

impl ActionStepCondition {
    pub fn new(condition: &ActionCondition) -> Self {
        Self::Unresolved {
            condition: condition.clone(),
        }
    }

    pub fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved { .. })
    }

    pub fn resolve(&mut self, game_state: &mut GameState, action: &ActionData, target: Entity) {
        // If a reaction deferred the callback, check whether it has fired yet
        if matches!(self, Self::PendingResolution { .. }) {
            let slot = match self {
                Self::PendingResolution { result_slot } => Arc::clone(result_slot),
                _ => unreachable!(),
            };
            if let Some(resolution) = slot.lock().unwrap().take() {
                *self = Self::Resolved { resolution };
            }
            return;
        }

        let condition = match self {
            Self::Unresolved { condition } => condition,
            Self::Resolved { .. } => return, // Already resolved
            Self::PendingResolution { .. } => unreachable!(),
        };

        debug!(
            "Resolving condition for action instance {:?}: {:?}",
            action.instance_id, condition
        );

        match condition {
            ActionCondition::None => {
                *self = Self::Resolved {
                    resolution: ActionConditionResolution::Unconditional,
                };
            }

            ActionCondition::AttackRoll(attack_roll) => {
                let attack_roll_result = systems::damage::attack_roll_fn(
                    attack_roll.as_ref(),
                    &mut game_state.world,
                    action.actor.id(),
                    target,
                    &action.context,
                );

                let armor_class = systems::loadout::armor_class(&game_state.world, target);

                let attack_event = Event::new(EventKind::D20CheckPerformed(
                    action.actor.clone(),
                    D20ResultKind::AttackRoll {
                        result: attack_roll_result.clone(),
                    },
                    D20CheckDCKind::AttackRoll(
                        EntityIdentifier::from_world(&game_state.world, target),
                        armor_class,
                    ),
                ));

                let result_slot = Arc::new(Mutex::new(None::<ActionConditionResolution>));
                let result_slot_clone = Arc::clone(&result_slot);

                // TEMP
                let action_instance_id = action.instance_id.clone();

                let callback = EventCallback::new(move |game_state, event, _| match &event.kind {
                    EventKind::D20CheckResolved(actor, result, dc) => {
                        let armor_class = match dc {
                            D20CheckDCKind::AttackRoll(_, armor_class) => armor_class.clone(),
                            _ => panic!("Expected AttackRoll DC in callback, got {:?}", dc),
                        };

                        let attack_roll = match result {
                            D20ResultKind::AttackRoll { result } => result.clone(),
                            _ => panic!("Expected AttackRoll result in callback, got {:?}", result),
                        };

                        debug!(
                            "Attack roll resolved for action instance {:?}: {:?} vs AC {:?}",
                            action_instance_id, attack_roll, armor_class
                        );

                        *result_slot_clone.lock().unwrap() =
                            Some(ActionConditionResolution::AttackRoll {
                                attack_roll,
                                armor_class,
                            });

                        systems::actions::resume_action(
                            game_state,
                            actor.id(),
                            ActivityPauseReason::ActionStepResolution,
                        );

                        CallbackResult::None
                    }
                    _ => panic!(
                        "Expected D20CheckResolved event in callback, got {:?}",
                        event.kind
                    ),
                });

                // Pause while waiting for the attack roll to resolve
                systems::actions::pause_action(
                    game_state,
                    action.actor.id(),
                    ActivityPauseReason::ActionStepResolution,
                );

                game_state.process_event_with_response_callback(attack_event, callback);

                let maybe_resolution = result_slot.lock().unwrap().take();
                match maybe_resolution {
                    Some(resolution) => *self = Self::Resolved { resolution },
                    None => *self = Self::PendingResolution { result_slot },
                };
            }

            ActionCondition::SavingThrow(saving_throw) => {
                let saving_throw_dc =
                    saving_throw(&game_state.world, action.actor.id(), &action.context);

                let saving_throw_event = systems::d20::check(
                    game_state,
                    target,
                    &D20CheckDCKind::SavingThrow(saving_throw_dc.clone()),
                );

                let result_slot = Arc::new(Mutex::new(None::<ActionConditionResolution>));
                let result_slot_clone = Arc::clone(&result_slot);

                let action_actor = action.actor.id().clone();

                let callback = EventCallback::new(move |game_state, event, _| match &event.kind {
                    EventKind::D20CheckResolved(_, result, dc) => {
                        let saving_throw_dc = match dc {
                            D20CheckDCKind::SavingThrow(dc) => dc.clone(),
                            _ => panic!("Expected SavingThrow DC in callback, got {:?}", dc),
                        };

                        let saving_throw_result = match result {
                            D20ResultKind::SavingThrow { result, .. } => result.clone(),
                            _ => {
                                panic!("Expected SavingThrow result in callback, got {:?}", result)
                            }
                        };

                        *result_slot_clone.lock().unwrap() =
                            Some(ActionConditionResolution::SavingThrow {
                                saving_throw_result,
                                saving_throw_dc,
                            });

                        systems::actions::resume_action(
                            game_state,
                            action_actor,
                            ActivityPauseReason::ActionStepResolution,
                        );

                        CallbackResult::None
                    }
                    _ => panic!(
                        "Expected D20CheckResolved event in callback, got {:?}",
                        event.kind
                    ),
                });

                // Pause while waiting for the saving throw to resolve
                systems::actions::pause_action(
                    game_state,
                    action.actor.id(),
                    ActivityPauseReason::ActionStepResolution,
                );

                game_state.process_event_with_response_callback(saving_throw_event, callback);

                let maybe_resolution = result_slot.lock().unwrap().take();
                match maybe_resolution {
                    Some(resolution) => *self = Self::Resolved { resolution },
                    None => *self = Self::PendingResolution { result_slot },
                };
            }
        }
    }

    pub fn resolution(&self) -> Option<&ActionConditionResolution> {
        match self {
            Self::Unresolved { .. } | Self::PendingResolution { .. } => None,
            Self::Resolved { resolution } => Some(resolution),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ActionStepPayload {
    pub damage: Option<StepPayloadDamage>,
    pub effect: Option<StepPayloadEffect>,
    pub healing: Option<StepPayloadHealing>,
}

impl ActionStepPayload {
    pub fn new(payload: &ActionPayload) -> Self {
        Self {
            damage: payload.damage().map(|damage| {
                if let Some(damage_on_failure) = payload.damage_on_failure() {
                    StepPayloadDamage::Unresolved {
                        damage: damage.clone(),
                        damage_on_failure: Some(damage_on_failure.clone()),
                    }
                } else {
                    StepPayloadDamage::Unresolved {
                        damage: damage.clone(),
                        damage_on_failure: None,
                    }
                }
            }),
            effect: payload
                .effect()
                .map(|effect| StepPayloadEffect::Unresolved {
                    effect: effect.clone(),
                }),
            healing: payload
                .healing()
                .map(|healing| StepPayloadHealing::Unresolved {
                    healing: healing.clone(),
                }),
        }
    }

    pub fn is_resolved(&self) -> bool {
        let damage_resolved = self.damage.as_ref().map_or(true, |d| d.is_resolved());
        let effect_resolved = self.effect.as_ref().map_or(true, |e| e.is_resolved());
        let healing_resolved = self.healing.as_ref().map_or(true, |h| h.is_resolved());

        damage_resolved && effect_resolved && healing_resolved
    }

    pub fn resolve(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        condition_resolution: &ActionConditionResolution,
    ) {
        if let Some(damage) = &mut self.damage {
            damage.resolve(game_state, action, condition_resolution);
        }

        if let Some(effect) = &mut self.effect {
            effect.resolve(game_state, action, condition_resolution);
        }

        if let Some(healing) = &mut self.healing {
            healing.resolve(game_state, action, condition_resolution);
        }
    }

    pub fn is_applied(&self) -> bool {
        let damage_applied = self.damage.as_ref().map_or(true, |d| d.is_applied());
        let effect_applied = self.effect.as_ref().map_or(true, |e| e.is_applied());
        let healing_applied = self.healing.as_ref().map_or(true, |h| h.is_applied());

        damage_applied && effect_applied && healing_applied
    }

    pub fn apply(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        target: Entity,
        condition_resolution: &ActionConditionResolution,
    ) {
        // TOOD: Could probably do some smarter stuff with some generics
        let damage_outcome = if let Some(damage) = &mut self.damage {
            damage.apply_payload(game_state, action, target, condition_resolution);
            self.damage.as_ref().and_then(|d| match d {
                StepPayloadDamage::Applied { outcome } => Some(outcome.clone()),
                _ => None,
            })
        } else {
            None
        };

        let effect_outcome = if let Some(effect) = &mut self.effect {
            effect.apply_payload(game_state, action, target, condition_resolution);
            self.effect.as_ref().and_then(|e| match e {
                StepPayloadEffect::Applied { outcome } => Some(outcome.clone()),
                _ => None,
            })
        } else {
            None
        };

        let healing_outcome = if let Some(healing) = &mut self.healing {
            healing.apply_payload(game_state, action, target, condition_resolution);
            self.healing.as_ref().and_then(|h| match h {
                StepPayloadHealing::Applied { outcome } => Some(outcome.clone()),
                _ => None,
            })
        } else {
            None
        };

        let result = ActionKindResult::Standard(ActionOutcomeBundle {
            damage: damage_outcome,
            effect: effect_outcome,
            healing: healing_outcome,
        });

        game_state.process_event(Event::action_performed_event(
            action,
            vec![(
                EntityIdentifier::from_world(&game_state.world, target),
                result,
            )],
        ));
    }
}

pub trait StepPayloadComponent {
    fn is_resolved(&self) -> bool;
    fn resolve(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        condition_resolution: &ActionConditionResolution,
    );
    fn is_applied(&self) -> bool;
    fn apply_payload(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        target: Entity,
        condition_resolution: &ActionConditionResolution,
    );
}

#[derive(Clone)]
pub enum StepPayloadDamage {
    Unresolved {
        damage: Arc<DamageFunction>,
        damage_on_failure: Option<DamageOnFailure>,
    },
    PendingResolution {
        result_slot: Arc<Mutex<Option<DamageRollResult>>>,
    },
    Resolved {
        damage: Option<DamageRollResult>,
    },
    Applied {
        outcome: DamageOutcome,
    },
}

impl StepPayloadComponent for StepPayloadDamage {
    fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved { .. })
    }

    fn resolve(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        condition_resolution: &ActionConditionResolution,
    ) {
        // If a reaction deferred the callback, check whether it has fired yet
        if matches!(self, Self::PendingResolution { .. }) {
            let slot = match self {
                Self::PendingResolution { result_slot } => Arc::clone(result_slot),
                _ => unreachable!(),
            };
            if let Some(damage_result) = slot.lock().unwrap().take() {
                *self = Self::Resolved {
                    damage: Some(damage_result),
                };
            }
            return;
        }

        let (damage, damage_on_failure) = match self {
            Self::Unresolved {
                damage,
                damage_on_failure,
            } => (damage.clone(), damage_on_failure.clone()),
            Self::Resolved { .. } => return, // Already resolved
            Self::Applied { .. } => return,  // Already applied, should never get here
            Self::PendingResolution { .. } => unreachable!(),
        };

        let damage_fn = if condition_resolution.is_success() {
            debug!("Condition resolved successfully, using standard damage function");
            Some(damage)
        } else if let Some(damage_on_failure) = &damage_on_failure
            && !condition_resolution.is_success()
        {
            debug!("Condition resolved unsuccessfully, using damage on failure function");
            match damage_on_failure {
                // If we're using half damage on failure, we still need to roll
                // the original damage and then halve it after
                DamageOnFailure::Half => Some(damage),
                DamageOnFailure::Custom(func) => Some(func.clone()),
            }
        } else {
            debug!(
                "Condition resolved unsuccessfully, but no applicable damage on failure, so no damage will be applied"
            );
            None
        };

        let Some(damage_fn) = damage_fn else {
            *self = Self::Resolved { damage: None };
            return;
        };

        let mut damage_roll = systems::damage::damage_roll_fn(
            damage_fn.as_ref(),
            &game_state.world,
            action.actor.id(),
            &action.context,
            condition_resolution.is_crit(),
        );
        // TODO: A bit clunky having to set this here
        damage_roll.action = Some((action.actor.id(), action.action_id.clone()));

        if let Some(damage_on_failure) = &damage_on_failure {
            match damage_on_failure {
                DamageOnFailure::Half if !condition_resolution.is_success() => {
                    let failure_label = match condition_resolution {
                        ActionConditionResolution::Unconditional => unreachable!(),
                        ActionConditionResolution::AttackRoll { .. } => "Attack Miss".to_string(),
                        ActionConditionResolution::SavingThrow { .. } => {
                            "Successful Save".to_string()
                        }
                    };

                    for component in damage_roll.components.iter_mut() {
                        let total = component.result.subtotal;
                        component.result.modifiers.add_modifier(
                            ModifierSource::Custom(failure_label.clone()),
                            -(total as f32 / 2.0).ceil() as i32,
                        );
                    }
                    damage_roll.recalculate_total();
                }
                _ => {}
            }
        }

        let damage_event = Event::new(EventKind::DamageRollPerformed(
            action.actor.clone(),
            damage_roll,
        ));

        let result_slot = Arc::new(Mutex::new(None::<DamageRollResult>));
        let result_slot_clone = Arc::clone(&result_slot);

        let callback = EventCallback::new(move |_game_state, event, _| match &event.kind {
            EventKind::DamageRollResolved(_actor, damage_result) => {
                *result_slot_clone.lock().unwrap() = Some(damage_result.clone());
                CallbackResult::None
            }
            _ => panic!(
                "Expected DamageRollResolved event in callback, got {:?}",
                event.kind
            ),
        });

        game_state.process_event_with_response_callback(damage_event, callback);

        let maybe_damage_result = result_slot.lock().unwrap().take();
        match maybe_damage_result {
            Some(damage_result) => {
                *self = Self::Resolved {
                    damage: Some(damage_result),
                }
            }
            None => *self = Self::PendingResolution { result_slot },
        };
    }

    fn is_applied(&self) -> bool {
        matches!(self, Self::Applied { .. })
    }

    fn apply_payload(
        &mut self,
        game_state: &mut GameState,
        _action: &ActionData,
        target: Entity,
        condition_resolution: &ActionConditionResolution,
    ) {
        let damage_result = match self {
            Self::Unresolved { .. } | Self::PendingResolution { .. } => return,
            Self::Resolved { damage } => damage.clone(),
            Self::Applied { .. } => return, // Already applied
        };

        let damage_outcome = match condition_resolution {
            ActionConditionResolution::Unconditional => {
                let (damage_taken, new_life_state) = if let Some(damage_result) = &damage_result {
                    systems::health::damage(game_state, target, &damage_result, None)
                } else {
                    (None, None)
                };
                DamageOutcome::unconditional(damage_result, damage_taken, new_life_state)
            }
            ActionConditionResolution::AttackRoll {
                attack_roll,
                armor_class,
            } => {
                let (damage_taken, new_life_state) = if let Some(damage_result) = &damage_result {
                    systems::health::damage(game_state, target, &damage_result, Some(attack_roll))
                } else {
                    (None, None)
                };
                DamageOutcome::attack_roll(
                    damage_result,
                    damage_taken,
                    new_life_state,
                    attack_roll.clone(),
                    armor_class.clone(),
                )
            }
            ActionConditionResolution::SavingThrow {
                saving_throw_dc,
                saving_throw_result,
            } => {
                let (damage_taken, new_life_state) = if let Some(damage_result) = &damage_result {
                    systems::health::damage(game_state, target, &damage_result, None)
                } else {
                    (None, None)
                };
                DamageOutcome::saving_throw(
                    damage_result,
                    damage_taken,
                    new_life_state,
                    saving_throw_dc.clone(),
                    saving_throw_result.clone(),
                )
            }
        };

        *self = Self::Applied {
            outcome: damage_outcome,
        };
    }
}

impl Debug for StepPayloadDamage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unresolved {
                damage,
                damage_on_failure,
            } => f
                .debug_struct("Unresolved")
                .field("damage", &"<DamageFunction>")
                .field("damage_on_failure", damage_on_failure)
                .finish(),
            Self::PendingResolution { result_slot } => f
                .debug_struct("PendingResolution")
                .field("result_slot", result_slot)
                .finish(),
            Self::Resolved { damage } => {
                f.debug_struct("Resolved").field("damage", damage).finish()
            }
            Self::Applied { outcome } => {
                f.debug_struct("Applied").field("outcome", outcome).finish()
            }
        }
    }
}

#[derive(Clone)]
pub enum StepPayloadHealing {
    Unresolved { healing: Arc<HealingFunction> },
    Resolved { healing: DiceSetRollResult },
    Applied { outcome: HealingOutcome },
}

impl StepPayloadComponent for StepPayloadHealing {
    fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved { .. })
    }

    fn resolve(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        _condition_resolution: &ActionConditionResolution,
    ) {
        let healing_fn = match self {
            Self::Unresolved { healing } => healing.clone(),
            Self::Resolved { .. } => return, // Already resolved
            Self::Applied { .. } => return,  // Already applied, should never get here
        };

        // TODO: No events yet for healing, might introduce them in the future?
        let healing_amount =
            healing_fn(&game_state.world, action.actor.id(), &action.context).roll();

        *self = Self::Resolved {
            healing: healing_amount,
        };
    }

    fn is_applied(&self) -> bool {
        matches!(self, Self::Applied { .. })
    }

    fn apply_payload(
        &mut self,
        game_state: &mut GameState,
        _action: &ActionData,
        target: Entity,
        // TODO: Should we not use the condition resolution for healing? I don't
        // imagine it'll be anything other than unconditional, but might as well
        // be consistent and allow for future possibilities?
        _condition_resolution: &ActionConditionResolution,
    ) {
        let healing_amount = match self {
            Self::Unresolved { .. } => panic!("Cannot apply unresolved healing payload"),
            Self::Resolved { healing } => healing.clone(),
            Self::Applied { .. } => return, // Already applied
        };

        let new_life_state = systems::health::heal(
            &mut game_state.world,
            target,
            healing_amount.subtotal as u32,
        );

        *self = Self::Applied {
            outcome: HealingOutcome {
                healing: healing_amount,
                new_life_state,
            },
        };
    }
}

impl Debug for StepPayloadHealing {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unresolved { healing } => f
                .debug_struct("Unresolved")
                .field("healing", &"<HealingFunction>")
                .finish(),
            Self::Resolved { healing } => f
                .debug_struct("Resolved")
                .field("healing", healing)
                .finish(),
            Self::Applied { outcome } => {
                f.debug_struct("Applied").field("outcome", outcome).finish()
            }
        }
    }
}

// TODO: Seems a bit unnecessary, but easier if everything has the same structure?
#[derive(Debug, Clone)]
pub enum StepPayloadEffect {
    Unresolved {
        effect: EffectInstanceTemplate,
    },
    Resolved {
        id: EffectId,
        effect: Option<EffectInstanceTemplate>,
    },
    Applied {
        outcome: EffectOutcome,
    },
}

impl StepPayloadComponent for StepPayloadEffect {
    fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved { .. })
    }

    fn resolve(
        &mut self,
        _game_state: &mut GameState,
        _action: &ActionData,
        condition_resolution: &ActionConditionResolution,
    ) {
        let effect = match self {
            Self::Unresolved { effect } => effect.clone(),
            Self::Resolved { .. } => return, // Already resolved
            Self::Applied { .. } => return,  // Already applied, should never get here
        };

        if condition_resolution.is_success() {
            *self = Self::Resolved {
                id: effect.effect_id.clone(),
                effect: Some(effect),
            };
        } else {
            *self = Self::Resolved {
                id: effect.effect_id.clone(),
                effect: None,
            };
        }
    }

    fn is_applied(&self) -> bool {
        matches!(self, Self::Applied { .. })
    }

    fn apply_payload(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        target: Entity,
        condition_resolution: &ActionConditionResolution,
    ) {
        let (id, effect) = match self {
            Self::Unresolved { .. } => panic!("Cannot apply unresolved effect payload"),
            Self::Resolved { id, effect } => (id.clone(), effect.clone()),
            Self::Applied { .. } => return, // Already applied
        };

        let Some(effect) = effect else {
            *self = Self::Applied {
                outcome: EffectOutcome {
                    resolution: condition_resolution.clone(),
                    effect: id,
                    applied: false,
                },
            };
            return; // No effect to apply
        };

        let effect_instance_id = systems::effects::add_effect_template(
            game_state,
            action.actor.id(),
            target,
            ModifierSource::Action(action.action_id.clone()),
            &effect,
            Some(&action.context),
            condition_resolution.clone(),
        );

        // Add concentration tracking if needed
        let spell_id = action.action_id.clone().into();
        if let Some(spell) = SpellsRegistry::get(&spell_id) {
            if spell.has_flag(SpellFlag::Concentration) {
                systems::spells::add_concentration_instance(
                    game_state,
                    action.actor.id(),
                    ConcentrationInstance::Effect {
                        entity: target,
                        effect: effect.effect_id.clone(),
                        instance: effect_instance_id,
                    },
                    &action.instance_id,
                );
            }
        }

        *self = Self::Applied {
            outcome: EffectOutcome {
                resolution: condition_resolution.clone(),
                effect: id,
                applied: true,
            },
        };
    }
}
