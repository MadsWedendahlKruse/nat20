use core::panic;
use std::{
    fmt::Debug,
    sync::{Arc, Mutex},
};

use enum_dispatch::enum_dispatch;
use hecs::Entity;
use tracing::{debug, error};
use uom::si::{f32::Velocity, length::meter, velocity::meter_per_second};

use crate::{
    components::{
        actions::{
            action::{
                ActionCondition, ActionConditionKind, ActionConditionResolution, ActionKindResult,
                ActionOutcome, ActionOutcomeBundle, ActionPayload, ActionPayloadComponent,
                DamageFunction, DamageOnFailure, DamageOutcome, DisplacementFunction,
                EffectOutcome, HealingFunction, HealingOutcome, PayloadDelivery,
                ReactionBodyFunction, ReactionOutcome,
            },
            targeting::TargetInstance,
        },
        activity::{ActivityPauseReason, ActivityState},
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
        geometry::{DEFAULT_GRAVITY, Displacement, DisplacementTemplate, Parabola, RaycastFilter},
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
                debug!("No condition to resolve, marking as automatically successful");
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

                let armor_class = systems::loadout::armor_class(game_state, target);

                let attack_event = Event::new(EventKind::D20CheckPerformed(
                    action.actor.clone(),
                    D20ResultKind::AttackRoll {
                        result: attack_roll_result.clone(),
                    },
                    D20CheckDCKind::AttackRoll(
                        EntityIdentifier::from_world(&game_state.world, target),
                        attack_roll_result.source.clone(),
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
                            D20CheckDCKind::AttackRoll(_, _, armor_class) => armor_class.clone(),
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

                debug!(
                    "Waiting for attack roll resolution callback for action instance {:?}...",
                    action.instance_id
                );

                let maybe_resolution = result_slot.lock().unwrap().take();
                match maybe_resolution {
                    Some(resolution) => *self = Self::Resolved { resolution },
                    None => *self = Self::PendingResolution { result_slot },
                };
            }

            ActionCondition::SavingThrow(saving_throw) => {
                let saving_throw_dc =
                    saving_throw(&game_state.world, action.actor.id(), &action.context);

                debug!(
                    "Performing saving throw for action instance {:?} with DC {:?}...",
                    action.instance_id, saving_throw_dc
                );

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

                debug!(
                    "Waiting for saving throw resolution callback for action instance {:?}...",
                    action.instance_id
                );

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
    pub components: Vec<StepPayloadKind>,
}

impl ActionStepPayload {
    pub fn new(payload: &ActionPayload) -> Self {
        Self {
            components: payload
                .components()
                .into_iter()
                .map(|component| match component {
                    ActionPayloadComponent::Damage {
                        damage,
                        damage_on_failure,
                    } => StepPayloadKind::Damage(StepPayloadDamage::Unresolved {
                        damage: Arc::clone(damage),
                        damage_on_failure: damage_on_failure.clone(),
                    }),
                    ActionPayloadComponent::Effect(effect) => {
                        StepPayloadKind::Effect(StepPayloadEffect::Unresolved {
                            effect: effect.clone(),
                        })
                    }
                    ActionPayloadComponent::Healing(healing) => {
                        StepPayloadKind::Healing(StepPayloadHealing::Unresolved {
                            healing: healing.clone(),
                        })
                    }
                    ActionPayloadComponent::Reaction(reaction) => {
                        StepPayloadKind::Reaction(StepPayloadReaction::Resolved {
                            reaction: reaction.clone(),
                        })
                    }
                    ActionPayloadComponent::Displacement(displacement) => {
                        StepPayloadKind::Displacement(StepPayloadDisplacement::Unresolved {
                            displacement: displacement.clone(),
                        })
                    }
                })
                .collect(),
        }
    }

    pub fn is_resolved(&self) -> bool {
        self.components
            .iter()
            .all(|component| component.is_resolved())
    }

    pub fn resolve(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        condition_resolution: &ActionConditionResolution,
    ) {
        for component in self.components.iter_mut() {
            component.resolve(game_state, action, condition_resolution);
        }
    }

    pub fn is_applied(&self) -> bool {
        self.components
            .iter()
            .all(|component| component.is_applied())
    }

    pub fn apply(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        target: Entity,
        condition_resolution: &ActionConditionResolution,
    ) {
        let result = ActionKindResult::Standard(ActionOutcomeBundle {
            components: self
                .components
                .iter_mut()
                .filter_map(|component| {
                    component.apply_payload(game_state, action, target, condition_resolution);
                    component.outcome()
                })
                .collect(),
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

#[enum_dispatch]
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
    fn outcome(&self) -> Option<ActionOutcome>;
}

#[derive(Debug, Clone)]
#[enum_dispatch(StepPayloadComponent)]
pub enum StepPayloadKind {
    Damage(StepPayloadDamage),
    Effect(StepPayloadEffect),
    Healing(StepPayloadHealing),
    Reaction(StepPayloadReaction),
    Displacement(StepPayloadDisplacement),
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
            game_state,
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
        let mut damage_result = match self {
            Self::Unresolved { .. } | Self::PendingResolution { .. } => return,
            Self::Resolved { damage } => damage,
            Self::Applied { .. } => return, // Already applied
        };

        let damage_outcome = match condition_resolution {
            ActionConditionResolution::Unconditional => {
                let (damage_taken, new_life_state) = if let Some(damage_result) = &mut damage_result
                {
                    systems::health::damage(game_state, target, damage_result)
                } else {
                    (None, None)
                };
                DamageOutcome::unconditional(damage_result.clone(), damage_taken, new_life_state)
            }
            ActionConditionResolution::AttackRoll {
                attack_roll,
                armor_class,
            } => {
                let (damage_taken, new_life_state) = if let Some(damage_result) = &mut damage_result
                {
                    systems::health::damage(game_state, target, damage_result)
                } else {
                    (None, None)
                };
                DamageOutcome::attack_roll(
                    damage_result.clone(),
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
                let (damage_taken, new_life_state) = if let Some(damage_result) = &mut damage_result
                {
                    systems::health::damage(game_state, target, damage_result)
                } else {
                    (None, None)
                };
                DamageOutcome::saving_throw(
                    damage_result.clone(),
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

    fn outcome(&self) -> Option<ActionOutcome> {
        match self {
            Self::Applied { outcome } => Some(ActionOutcome::Damage(outcome.clone())),
            _ => None,
        }
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

    fn outcome(&self) -> Option<ActionOutcome> {
        match self {
            Self::Applied { outcome } => Some(ActionOutcome::Healing(outcome.clone())),
            _ => None,
        }
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

    fn outcome(&self) -> Option<ActionOutcome> {
        match self {
            Self::Applied { outcome } => Some(ActionOutcome::Effect(outcome.clone())),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub enum StepPayloadReaction {
    // Nothing to resolve, so no need for an Unresolved variant
    Resolved { reaction: Arc<ReactionBodyFunction> },
    Applied { outcome: ReactionOutcome },
}

impl StepPayloadComponent for StepPayloadReaction {
    fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved { .. })
    }

    fn resolve(
        &mut self,
        _game_state: &mut GameState,
        _action: &ActionData,
        _condition_resolution: &ActionConditionResolution,
    ) {
        // Nothing to resolve for reactions
        return;
    }

    fn is_applied(&self) -> bool {
        matches!(self, Self::Applied { .. })
    }

    fn apply_payload(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        _target: Entity,
        condition_resolution: &ActionConditionResolution,
    ) {
        let reaction_fn = match self {
            Self::Resolved { reaction } => reaction.clone(),
            Self::Applied { .. } => return, // Already applied, should never get here
        };

        if !condition_resolution.is_success() {
            *self = Self::Applied {
                outcome: ReactionOutcome::NoEffect,
            };
            return;
        }

        let Some(trigger_event) = action.trigger_event.as_ref() else {
            panic!("No trigger event in action for reaction payload, cannot apply reaction");
        };

        let game_state_ptr = unsafe { &mut *(game_state as *mut GameState) };
        let reaction_event = game_state
            .session_for_entity_mut(action.actor.id())
            .pending_events_mut()
            .iter_mut()
            .find(|pe| pe.event.id == trigger_event.id);
        let Some(reaction_event) = reaction_event else {
            panic!(
                "Attempted to perform reaction to event which is not pending: {:#?}",
                action
            );
        };

        let outcome = reaction_fn(game_state_ptr, action, &mut reaction_event.event);

        if let Some(outcome) = outcome {
            *self = Self::Applied { outcome };
            return;
        }

        // TODO: Not sure if this check actually works
        if **trigger_event != reaction_event.event {
            *self = Self::Applied {
                outcome: ReactionOutcome::ModifyEvent {
                    before: trigger_event.as_ref().clone(),
                    after: reaction_event.event.clone(),
                },
            };
            return;
        }

        *self = Self::Applied {
            outcome: ReactionOutcome::NoEffect, // TEMP
        };
    }

    fn outcome(&self) -> Option<ActionOutcome> {
        match self {
            Self::Applied { outcome } => Some(ActionOutcome::Reaction(outcome.clone())),
            _ => None,
        }
    }
}

impl std::fmt::Debug for StepPayloadReaction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Resolved { .. } => f.debug_struct("Resolved").finish(),
            Self::Applied { outcome } => {
                f.debug_struct("Applied").field("outcome", outcome).finish()
            }
        }
    }
}

#[derive(Clone)]
pub enum StepPayloadDisplacement {
    Unresolved {
        displacement: Arc<DisplacementFunction>,
    },
    Resolved {
        displacement: DisplacementTemplate,
    },
    Applied {
        outcome: Displacement,
    },
}

impl StepPayloadComponent for StepPayloadDisplacement {
    fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved { .. })
    }

    fn resolve(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        _condition_resolution: &ActionConditionResolution,
    ) {
        let displacement_fn = match self {
            Self::Unresolved { displacement } => displacement.clone(),
            Self::Resolved { .. } => return, // Already resolved
            Self::Applied { .. } => return,  // Already applied, should never get here
        };

        *self = Self::Resolved {
            displacement: displacement_fn(&game_state.world, action.actor.id(), &action.context),
        };
    }

    fn is_applied(&self) -> bool {
        matches!(self, Self::Applied { .. })
    }

    fn apply_payload(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        target: Entity,
        _condition_resolution: &ActionConditionResolution,
    ) {
        let displacement = match self {
            Self::Unresolved { .. } => panic!("Cannot apply unresolved displacement payload"),
            Self::Resolved { displacement } => displacement.clone(),
            Self::Applied { .. } => return, // Already applied
        };

        let Some(displacement) = displacement.instantiate(game_state, action, target) else {
            error!("Failed to instantiate displacement, cannot apply");
            return;
        };

        match &displacement {
            Displacement::Teleport => {
                let Some(target_position) = action
                    .targets
                    .iter()
                    .find_map(|target| target.position(&game_state.world))
                else {
                    error!("No valid target position for teleport displacement");
                    return;
                };

                systems::geometry::teleport_to_ground(
                    &mut game_state.world,
                    &game_state.geometry,
                    target,
                    &target_position,
                );

                *self = Self::Applied {
                    outcome: Displacement::Teleport,
                };
            }

            Displacement::Push { trajectory } | Displacement::Pull { trajectory } => {
                systems::helpers::get_component_mut::<ActivityState>(&mut game_state.world, target)
                    .set_displaced(trajectory.clone());

                *self = Self::Applied {
                    outcome: displacement.clone(),
                };
            }
        }
    }

    fn outcome(&self) -> Option<ActionOutcome> {
        match self {
            Self::Applied { outcome } => Some(ActionOutcome::Displacement(outcome.clone())),
            _ => None,
        }
    }
}

impl Debug for StepPayloadDisplacement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unresolved { .. } => f
                .debug_struct("Unresolved")
                .field("displacement", &"<DisplacementFunction>")
                .finish(),
            Self::Resolved { displacement } => f
                .debug_struct("Resolved")
                .field("displacement", displacement)
                .finish(),
            Self::Applied { outcome } => {
                f.debug_struct("Applied").field("outcome", outcome).finish()
            }
        }
    }
}
