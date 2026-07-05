use core::panic;
use std::{collections::VecDeque, fmt::Debug, sync::Arc};

use hecs::Entity;
use tracing::{debug, error, warn};

use crate::{
    components::{
        actions::{
            action::{
                ActionCondition, ActionConditionKind, ActionConditionResolution, ActionKindResult,
                ActionOutcome, ActionOutcomeBundle, ActionPayload, ActionPayloadComponent,
                ActionPhaseSpec, DamageOnFailure, DamageOutcome, EffectOutcome, HealingOutcome,
                PayloadDelivery, PhaseOutcomes, PhaseRequirement, PhaseTargets,
                ReactionBodyFunction, ReactionOutcome,
            },
            targeting::TargetInstance,
        },
        activity::ActivityState,
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
        geometry::{Displacement, DisplacementTemplate},
    },
};

/// The current state of an action being executed by an entity
#[derive(Debug)]
pub struct ActionExecution {
    pub action: ActionData,
    pending: VecDeque<PhaseState>,
    current: Option<PhaseState>,
    status: ExecutionStatus,
}

impl ActionExecution {
    pub fn new(action: ActionData, phases: Vec<PhaseState>) -> Self {
        Self {
            action,
            pending: phases.into(),
            current: None,
            status: ExecutionStatus::Running,
        }
    }

    pub fn status(&self) -> ExecutionStatus {
        self.status
    }

    pub fn is_done(&self) -> bool {
        self.status == ExecutionStatus::Done
    }

    /// Start the next phase. Called by the action timeline when the execution
    /// is `Running` and the phase spacing has elapsed.
    pub fn advance(&mut self, game_state: &mut GameState) {
        debug_assert_eq!(self.status, ExecutionStatus::Running);

        let Some(phase) = self.pending.pop_front() else {
            self.status = ExecutionStatus::Done;
            return;
        };

        if !phase.requirement_met() {
            debug!(
                "Skipping phase {} for target {:?}: requirement {:?} not met",
                phase.phase_index, phase.target, phase.requires
            );
            return self.advance(game_state);
        }

        self.run(game_state, phase);
    }

    /// Re-run the current phase after an event it was waiting on resolved.
    /// Safe to call speculatively: if the result hasn't arrived yet the phase
    /// simply parks again.
    pub fn resume_from_event(&mut self, game_state: &mut GameState) {
        if self.status != ExecutionStatus::Waiting(WaitReason::EventResolution) {
            return;
        }
        let phase = self.current.take().unwrap();
        self.run(game_state, phase);
    }

    /// Deliver the current phase's payload now that its projectile has landed.
    pub fn resume_from_projectile(&mut self, game_state: &mut GameState) {
        debug_assert_eq!(
            self.status,
            ExecutionStatus::Waiting(WaitReason::ProjectileImpact)
        );
        let phase = self.current.take().unwrap();
        self.run(game_state, phase);
    }

    fn run(&mut self, game_state: &mut GameState, mut phase: PhaseState) {
        match phase.perform(game_state) {
            PhasePerformResult::Applied => {
                self.status = if self.pending.is_empty() {
                    ExecutionStatus::Done
                } else {
                    ExecutionStatus::Running
                };
            }
            PhasePerformResult::AwaitingEvent => {
                self.current = Some(phase);
                self.status = ExecutionStatus::Waiting(WaitReason::EventResolution);
            }
            PhasePerformResult::ProjectileLaunched => {
                self.current = Some(phase);
                self.status = ExecutionStatus::Waiting(WaitReason::ProjectileImpact);
            }
        }
    }
}

// TODO: Not sure what to call this guy
#[derive(Debug, Clone)]
pub struct PhaseState {
    pub action: ActionData,
    pub steps: Vec<StepState>,
    pub delivery: PayloadDelivery,
    pub condition: ActionConditionKind,
    pub target: TargetInstance,
    pub requires: PhaseRequirement,
    pub phase_index: usize,
    pub target_index: usize,
    pub outcomes: Arc<PhaseOutcomes>,
}

impl PhaseState {
    pub fn new(
        game_state: &GameState,
        action: &ActionData,
        phase: &ActionPhaseSpec,
        target: TargetInstance,
        (phase_index, target_index): (usize, usize),
        outcomes: Arc<PhaseOutcomes>,
    ) -> Self {
        let targets = match &phase.targets {
            PhaseTargets::Inherited => systems::actions::get_targeted_entities(
                game_state,
                action,
                Some(vec![target.clone()]),
            ),
            PhaseTargets::Shape(shape) => systems::actions::entities_in_shape_at_target(
                game_state,
                action,
                &target,
                shape.as_ref(),
            ),
        };

        let steps = targets
            .iter()
            .map(|entity| StepState::new(*entity, &phase.condition, &phase.payload))
            .collect();

        Self {
            action: action.clone(),
            steps,
            delivery: phase.payload.delivery().clone(),
            condition: phase.condition.kind(),
            target,
            requires: phase.requires,
            phase_index,
            target_index,
            outcomes,
        }
    }

    pub fn requirement_met(&self) -> bool {
        let previous = self
            .phase_index
            .checked_sub(1)
            .and_then(|previous| self.outcomes.get(previous, self.target_index));
        match self.requires {
            PhaseRequirement::None => true,
            PhaseRequirement::Success => previous == Some(true),
            PhaseRequirement::Failure => previous == Some(false),
        }
    }

    pub fn perform(&mut self, game_state: &mut GameState) -> PhasePerformResult {
        match self.delivery.clone() {
            PayloadDelivery::Immediate => {
                if !self.resolve_condition(game_state) {
                    return PhasePerformResult::AwaitingEvent;
                }
                if !self.resolve_payload(game_state) {
                    return PhasePerformResult::AwaitingEvent;
                }
                self.apply_payload(game_state);
                PhasePerformResult::Applied
            }

            PayloadDelivery::Projectile { template } => {
                match self.condition {
                    ActionConditionKind::None | ActionConditionKind::AttackRoll => {
                        if !self.resolve_condition(game_state) {
                            return PhasePerformResult::AwaitingEvent;
                        }
                        if !self.resolve_payload(game_state) {
                            return PhasePerformResult::AwaitingEvent;
                        }
                    }
                    ActionConditionKind::SavingThrow => { /* Don't resolve anything in advance */ }
                }

                let projectile = template.instantiate(game_state, &self.action, &self.target);

                if let Ok(projectile) = projectile {
                    debug!("Instantiated projectile {:?}", projectile);
                    game_state.world.spawn((projectile, ProjectileTag));
                    // The steps are applied when the projectile lands and the
                    // execution resumes this phase
                    self.delivery = PayloadDelivery::Immediate;
                    PhasePerformResult::ProjectileLaunched
                } else {
                    error!(
                        "Failed to instantiate projectile for action instance {:?}: {:?}",
                        self.action.instance_id,
                        projectile.err()
                    );
                    self.steps.clear();
                    PhasePerformResult::Applied
                }
            }
        }
    }

    /// Returns false while a step is waiting on an event round-trip
    fn resolve_condition(&mut self, game_state: &mut GameState) -> bool {
        for step in self.steps.iter_mut() {
            if !step.resolve_condition(game_state, &self.action) {
                return false;
            }
        }

        let success = self.steps.iter().any(|step| {
            step.resolution
                .ready()
                .is_some_and(|resolution| resolution.is_success())
        });
        self.outcomes
            .record(self.phase_index, self.target_index, success);
        true
    }

    /// Returns false while a step is waiting on an event round-trip
    fn resolve_payload(&mut self, game_state: &mut GameState) -> bool {
        for step in self.steps.iter_mut() {
            if !step.resolve_payload(game_state, &self.action) {
                return false;
            }
        }
        true
    }

    fn apply_payload(&mut self, game_state: &mut GameState) {
        for step in self.steps.iter_mut() {
            step.apply_payload(game_state, &self.action);
        }
    }
}

/// One value awaited from an event round-trip. `Requested` marks that the
/// event has been emitted, so a re-run doesn't emit it again.
#[derive(Debug, Clone)]
pub enum Awaitable<T> {
    NotRequested,
    Requested,
    Ready(T),
}

impl<T> Awaitable<T> {
    pub fn ready(&self) -> Option<&T> {
        match self {
            Awaitable::Ready(value) => Some(value),
            _ => None,
        }
    }

    pub fn is_ready(&self) -> bool {
        matches!(self, Awaitable::Ready(_))
    }
}

/// A result delivered to a waiting execution by an event response callback.
/// Executions await at most one result at a time, so one slot per actor
/// (`GameState::execution_mailbox`) suffices.
#[derive(Debug, Clone)]
pub enum ResumePayload {
    Condition(ActionConditionResolution),
    DamageRoll(DamageRollResult),
}

/// Why an execution is suspended. The single suspension primitive: whatever a
/// phase is waiting on, the execution stays here with the phase held as
/// `current` until the corresponding `resume_*` call.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WaitReason {
    /// A d20/damage event round-trip is out (possibly parked behind a reaction)
    EventResolution,
    ProjectileImpact,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExecutionStatus {
    /// Ready to advance to the next phase when the action timeline allows
    Running,
    Waiting(WaitReason),
    Done,
}

/// What happened when a phase was performed, so the owning `ActionExecution`
/// knows whether to move on or wait.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PhasePerformResult {
    Applied,
    /// A step is waiting on an event round-trip (e.g. a d20 roll parked behind
    /// a reaction); re-perform once the event resolves
    AwaitingEvent,
    ProjectileLaunched,
}

#[derive(Debug, Clone)]
pub struct StepState {
    pub target: Entity,
    condition: ActionCondition,
    pub resolution: Awaitable<ActionConditionResolution>,
    components: Vec<StepComponent>,
}

impl StepState {
    pub fn new(target: Entity, condition: &ActionCondition, payload: &ActionPayload) -> Self {
        Self {
            target,
            condition: condition.clone(),
            resolution: Awaitable::NotRequested,
            components: payload
                .components()
                .iter()
                .map(StepComponent::new)
                .collect(),
        }
    }

    fn is_applied(&self) -> bool {
        self.components
            .iter()
            .all(|component| component.outcome.is_some())
    }

    /// Returns false while waiting on an event round-trip
    fn resolve_condition(&mut self, game_state: &mut GameState, action: &ActionData) -> bool {
        if self.resolution.is_ready() {
            return true;
        }

        if matches!(self.resolution, Awaitable::NotRequested) {
            if matches!(self.condition, ActionCondition::None) {
                debug!("No condition to resolve, marking as automatically successful");
                self.resolution = Awaitable::Ready(ActionConditionResolution::Unconditional);
                return true;
            }
            self.request_condition_resolution(game_state, action);
            self.resolution = Awaitable::Requested;
        }

        // The response callback delivers synchronously unless the event got
        // parked behind a reaction window
        match game_state.execution_mailbox.remove(&action.actor.id()) {
            Some(ResumePayload::Condition(resolution)) => {
                self.resolution = Awaitable::Ready(resolution);
                true
            }
            Some(other) => panic!("Expected condition resolution in mailbox, got {:?}", other),
            None => false,
        }
    }

    /// Emits the d20 event for this step's condition; the response callback
    /// delivers the resolution into the actor's execution mailbox
    fn request_condition_resolution(&self, game_state: &mut GameState, action: &ActionData) {
        let actor = action.actor.id();

        debug!(
            "Resolving condition for action instance {:?}: {:?}",
            action.instance_id, self.condition
        );

        match &self.condition {
            ActionCondition::None => unreachable!(),

            ActionCondition::AttackRoll(attack_roll) => {
                let attack_roll_result = systems::damage::attack_roll_fn(
                    attack_roll.as_ref(),
                    &mut game_state.world,
                    actor,
                    self.target,
                    &action.context,
                );

                let armor_class = systems::loadout::armor_class(game_state, self.target);

                let attack_event = Event::new(EventKind::D20CheckPerformed(
                    action.actor.clone(),
                    D20ResultKind::AttackRoll {
                        result: attack_roll_result.clone(),
                    },
                    D20CheckDCKind::AttackRoll(
                        EntityIdentifier::from_world(&game_state.world, self.target),
                        attack_roll_result.source.clone(),
                        armor_class,
                    ),
                ));

                let callback = EventCallback::new(move |game_state, event, _| match &event.kind {
                    EventKind::D20CheckResolved(_, result, dc) => {
                        let armor_class = match dc {
                            D20CheckDCKind::AttackRoll(_, _, armor_class) => armor_class.clone(),
                            _ => panic!("Expected AttackRoll DC in callback, got {:?}", dc),
                        };

                        let attack_roll = match result {
                            D20ResultKind::AttackRoll { result } => result.clone(),
                            _ => panic!("Expected AttackRoll result in callback, got {:?}", result),
                        };

                        game_state.execution_mailbox.insert(
                            actor,
                            ResumePayload::Condition(ActionConditionResolution::AttackRoll {
                                attack_roll,
                                armor_class,
                            }),
                        );

                        CallbackResult::None
                    }
                    _ => panic!(
                        "Expected D20CheckResolved event in callback, got {:?}",
                        event.kind
                    ),
                });

                game_state.process_event_with_response_callback(attack_event, callback);
            }

            ActionCondition::SavingThrow(saving_throw) => {
                let saving_throw_dc = saving_throw(&game_state.world, actor, &action.context);

                debug!(
                    "Performing saving throw for action instance {:?} with DC {:?}...",
                    action.instance_id, saving_throw_dc
                );

                let saving_throw_event = systems::d20::check(
                    game_state,
                    self.target,
                    &D20CheckDCKind::SavingThrow(saving_throw_dc.clone()),
                );

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

                        game_state.execution_mailbox.insert(
                            actor,
                            ResumePayload::Condition(ActionConditionResolution::SavingThrow {
                                saving_throw_result,
                                saving_throw_dc,
                            }),
                        );

                        CallbackResult::None
                    }
                    _ => panic!(
                        "Expected D20CheckResolved event in callback, got {:?}",
                        event.kind
                    ),
                });

                game_state.process_event_with_response_callback(saving_throw_event, callback);
            }
        }
    }

    /// Returns false while a component is waiting on an event round-trip
    fn resolve_payload(&mut self, game_state: &mut GameState, action: &ActionData) -> bool {
        let Some(resolution) = self.resolution.ready() else {
            return false;
        };
        let resolution = resolution.clone();

        for component in self.components.iter_mut() {
            if !component.resolve(game_state, action, &resolution) {
                return false;
            }
        }
        true
    }

    fn apply_payload(&mut self, game_state: &mut GameState, action: &ActionData) {
        if self.is_applied() {
            return;
        }
        let Some(resolution) = self.resolution.ready() else {
            return;
        };
        let resolution = resolution.clone();

        for component in self.components.iter_mut() {
            component.apply(game_state, action, self.target, &resolution);
        }

        let result = ActionKindResult::Standard(ActionOutcomeBundle {
            components: self
                .components
                .iter()
                .filter_map(|component| component.outcome.clone())
                .collect(),
        });

        game_state.process_event(Event::action_performed_event(
            action,
            vec![(
                EntityIdentifier::from_world(&game_state.world, self.target),
                result,
            )],
        ));
    }
}

/// One payload component of one step: what to do to the target, the rolled
/// (but not yet applied) result, and the applied outcome
#[derive(Clone)]
struct StepComponent {
    payload: ActionPayloadComponent,
    result: Awaitable<PayloadResult>,
    outcome: Option<ActionOutcome>,
}

#[derive(Debug, Clone)]
enum PayloadResult {
    /// None: the condition failed and there is no fallback damage
    Damage(Option<DamageRollResult>),
    Healing(DiceSetRollResult),
    /// None: the condition failed, so the effect is not applied
    Effect(Option<EffectInstanceTemplate>),
    Reaction,
    Displacement(DisplacementTemplate),
}

impl StepComponent {
    fn new(payload: &ActionPayloadComponent) -> Self {
        Self {
            payload: payload.clone(),
            result: Awaitable::NotRequested,
            outcome: None,
        }
    }

    /// Returns false while waiting on an event round-trip (only damage rolls
    /// round-trip, everything else resolves synchronously)
    fn resolve(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        resolution: &ActionConditionResolution,
    ) -> bool {
        if self.result.is_ready() {
            return true;
        }

        if matches!(self.result, Awaitable::Requested) {
            return match game_state.execution_mailbox.remove(&action.actor.id()) {
                Some(ResumePayload::DamageRoll(damage_result)) => {
                    self.result = Awaitable::Ready(PayloadResult::Damage(Some(damage_result)));
                    true
                }
                Some(other) => panic!("Expected damage roll in mail, got {:?}", other),
                None => false,
            };
        }

        match &self.payload {
            ActionPayloadComponent::Damage {
                damage,
                damage_on_failure,
            } => {
                let damage_fn = if resolution.is_success() {
                    debug!("Condition resolved successfully, using standard damage function");
                    Some(damage.clone())
                } else if let Some(damage_on_failure) = damage_on_failure {
                    debug!("Condition resolved unsuccessfully, using damage on failure function");
                    match damage_on_failure {
                        // If we're using half damage on failure, we still need to roll
                        // the original damage and then halve it after
                        DamageOnFailure::Half => Some(damage.clone()),
                        DamageOnFailure::Custom(func) => Some(func.clone()),
                    }
                } else {
                    debug!(
                        "Condition resolved unsuccessfully, but no applicable damage on failure, so no damage will be applied"
                    );
                    None
                };

                let Some(damage_fn) = damage_fn else {
                    self.result = Awaitable::Ready(PayloadResult::Damage(None));
                    return true;
                };

                let mut damage_roll = systems::damage::damage_roll_fn(
                    damage_fn.as_ref(),
                    game_state,
                    action.actor.id(),
                    &action.context,
                    resolution.is_crit(),
                );
                // TODO: A bit clunky having to set this here
                damage_roll.action = Some((action.actor.id(), action.action_id.clone()));

                if let Some(DamageOnFailure::Half) = damage_on_failure
                    && !resolution.is_success()
                {
                    let failure_label = match resolution {
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

                let damage_event = Event::new(EventKind::DamageRollPerformed(
                    action.actor.clone(),
                    damage_roll,
                ));

                let actor = action.actor.id();
                let callback = EventCallback::new(move |game_state, event, _| match &event.kind {
                    EventKind::DamageRollResolved(_, damage_result) => {
                        game_state
                            .execution_mailbox
                            .insert(actor, ResumePayload::DamageRoll(damage_result.clone()));
                        CallbackResult::None
                    }
                    _ => panic!(
                        "Expected DamageRollResolved event in callback, got {:?}",
                        event.kind
                    ),
                });

                self.result = Awaitable::Requested;
                game_state.process_event_with_response_callback(damage_event, callback);

                match game_state.execution_mailbox.remove(&actor) {
                    Some(ResumePayload::DamageRoll(damage_result)) => {
                        self.result = Awaitable::Ready(PayloadResult::Damage(Some(damage_result)));
                        true
                    }
                    Some(other) => panic!("Expected damage roll in mail, got {:?}", other),
                    None => false,
                }
            }

            ActionPayloadComponent::Healing(healing) => {
                // TODO: No events yet for healing, might introduce them in the future?
                let healing_amount =
                    healing(&game_state.world, action.actor.id(), &action.context).roll();
                self.result = Awaitable::Ready(PayloadResult::Healing(healing_amount));
                true
            }

            ActionPayloadComponent::Effect(effect) => {
                let effect = resolution.is_success().then(|| effect.clone());
                self.result = Awaitable::Ready(PayloadResult::Effect(effect));
                true
            }

            ActionPayloadComponent::Reaction(_) => {
                self.result = Awaitable::Ready(PayloadResult::Reaction);
                true
            }

            ActionPayloadComponent::Displacement(displacement) => {
                let displacement =
                    displacement(&game_state.world, action.actor.id(), &action.context);
                self.result = Awaitable::Ready(PayloadResult::Displacement(displacement));
                true
            }
        }
    }

    fn apply(
        &mut self,
        game_state: &mut GameState,
        action: &ActionData,
        target: Entity,
        resolution: &ActionConditionResolution,
    ) {
        if self.outcome.is_some() {
            return;
        }
        let Some(result) = self.result.ready() else {
            panic!("Cannot apply unresolved payload component");
        };

        self.outcome = Some(match result.clone() {
            PayloadResult::Damage(damage_result) => {
                Self::apply_damage(game_state, target, resolution, damage_result)
            }
            PayloadResult::Healing(healing_amount) => {
                Self::apply_healing(game_state, target, healing_amount)
            }
            PayloadResult::Effect(effect) => {
                let ActionPayloadComponent::Effect(template) = &self.payload else {
                    unreachable!()
                };
                let effect_id = template.effect_id.clone();
                Self::apply_effect(game_state, action, target, resolution, effect_id, effect)
            }
            PayloadResult::Reaction => {
                let ActionPayloadComponent::Reaction(reaction_fn) = &self.payload else {
                    unreachable!()
                };
                Self::apply_reaction(game_state, action, resolution, Arc::clone(reaction_fn))
            }
            PayloadResult::Displacement(displacement) => {
                let Some(outcome) =
                    Self::apply_displacement(game_state, action, target, displacement)
                else {
                    return;
                };
                outcome
            }
        });
    }

    fn apply_damage(
        game_state: &mut GameState,
        target: Entity,
        resolution: &ActionConditionResolution,
        mut damage_result: Option<DamageRollResult>,
    ) -> ActionOutcome {
        let (damage_taken, new_life_state) = if let Some(damage_result) = &mut damage_result {
            systems::health::damage(game_state, target, damage_result)
        } else {
            (None, None)
        };

        let outcome = match resolution {
            ActionConditionResolution::Unconditional => {
                DamageOutcome::unconditional(damage_result, damage_taken, new_life_state)
            }
            ActionConditionResolution::AttackRoll {
                attack_roll,
                armor_class,
            } => DamageOutcome::attack_roll(
                damage_result,
                damage_taken,
                new_life_state,
                attack_roll.clone(),
                armor_class.clone(),
            ),
            ActionConditionResolution::SavingThrow {
                saving_throw_dc,
                saving_throw_result,
            } => DamageOutcome::saving_throw(
                damage_result,
                damage_taken,
                new_life_state,
                saving_throw_dc.clone(),
                saving_throw_result.clone(),
            ),
        };

        ActionOutcome::Damage(outcome)
    }

    fn apply_healing(
        game_state: &mut GameState,
        target: Entity,
        healing_amount: DiceSetRollResult,
    ) -> ActionOutcome {
        let new_life_state = systems::health::heal(
            &mut game_state.world,
            target,
            healing_amount.subtotal as u32,
        );

        ActionOutcome::Healing(HealingOutcome {
            healing: healing_amount,
            new_life_state,
        })
    }

    fn apply_effect(
        game_state: &mut GameState,
        action: &ActionData,
        target: Entity,
        resolution: &ActionConditionResolution,
        effect_id: EffectId,
        effect: Option<EffectInstanceTemplate>,
    ) -> ActionOutcome {
        let Some(effect) = effect else {
            return ActionOutcome::Effect(EffectOutcome {
                resolution: resolution.clone(),
                effect: effect_id,
                applied: false,
            });
        };

        let effect_instance_id = systems::effects::add_effect_template(
            game_state,
            action.actor.id(),
            target,
            ModifierSource::Action(action.action_id.clone()),
            &effect,
            Some(&action.context),
            resolution.clone(),
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

        ActionOutcome::Effect(EffectOutcome {
            resolution: resolution.clone(),
            effect: effect.effect_id.clone(),
            applied: true,
        })
    }

    fn apply_reaction(
        game_state: &mut GameState,
        action: &ActionData,
        resolution: &ActionConditionResolution,
        reaction_fn: Arc<ReactionBodyFunction>,
    ) -> ActionOutcome {
        if !resolution.is_success() {
            return ActionOutcome::Reaction(ReactionOutcome::NoEffect);
        }

        let Some(trigger_event) = action.trigger_event.as_ref() else {
            panic!("No trigger event in action for reaction payload, cannot apply reaction");
        };

        // Take the pending trigger event out of the session while the reaction
        // body (potentially) mutates it, then put it back
        let scope = game_state.scope_for_entity(action.actor.id());
        let pending_events = game_state
            .interaction_engine
            .session_mut(scope)
            .pending_events_mut();
        let Some(index) = pending_events
            .iter()
            .position(|pending| pending.event.id == trigger_event.id)
        else {
            panic!(
                "Attempted to perform reaction to event which is not pending: {:#?}",
                action
            );
        };
        let mut pending = pending_events.remove(index).unwrap();

        let outcome = reaction_fn(game_state, action, &mut pending.event);

        let outcome = outcome.unwrap_or_else(|| {
            // TODO: Not sure if this check actually works
            if **trigger_event != pending.event {
                ReactionOutcome::ModifyEvent {
                    before: trigger_event.as_ref().clone(),
                    after: pending.event.clone(),
                }
            } else {
                ReactionOutcome::NoEffect // TEMP
            }
        });

        let mut should_reinsert = true;

        if let ReactionOutcome::CancelEvent { event, .. } = &outcome {
            if event.id == pending.event.id {
                debug!(
                    "Reaction cancelled event {:?}, removing from pending events",
                    event.id
                );
                should_reinsert = false;
            } else {
                // TODO: Not sure if this ever actually happens?
                warn!(
                    "Reaction cancelled event {:?}, but pending event is {:?}, not removing from pending events",
                    event, pending.event
                );
            }
        }

        if should_reinsert {
            game_state
                .interaction_engine
                .session_mut(scope)
                .pending_events_mut()
                .insert(index, pending);
        }

        ActionOutcome::Reaction(outcome)
    }

    fn apply_displacement(
        game_state: &mut GameState,
        action: &ActionData,
        target: Entity,
        displacement: DisplacementTemplate,
    ) -> Option<ActionOutcome> {
        let Some(displacement) = displacement.instantiate(game_state, action, target) else {
            error!("Failed to instantiate displacement, cannot apply");
            return None;
        };

        match &displacement {
            Displacement::Teleport => {
                let Some(target_position) = action
                    .targets
                    .iter()
                    .find_map(|target| target.position(&game_state.world))
                else {
                    error!("No valid target position for teleport displacement");
                    return None;
                };

                systems::geometry::teleport_to_ground(
                    &mut game_state.world,
                    &game_state.geometry,
                    target,
                    &target_position,
                );

                Some(ActionOutcome::Displacement(Displacement::Teleport))
            }

            Displacement::Push { trajectory } | Displacement::Pull { trajectory } => {
                systems::helpers::get_component_mut::<ActivityState>(&mut game_state.world, target)
                    .set_displaced(trajectory.clone());

                Some(ActionOutcome::Displacement(displacement))
            }
        }
    }
}

impl Debug for StepComponent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StepComponent")
            .field("result", &self.result)
            .field("outcome", &self.outcome)
            .finish()
    }
}
