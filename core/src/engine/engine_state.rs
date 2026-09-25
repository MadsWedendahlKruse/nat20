use std::collections::{HashMap, HashSet};

use hecs::{Entity, NoSuchEntity, World};
use parry3d::{na::Point3, shape::Ball};
use tracing::{debug, info, warn};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{
        actions::targeting::EntityFilter,
        activity::{Activity, ActivityError, ActivityState},
        d20::D20CheckDC,
        time::TimeMode,
    },
    engine::{
        action_prompt::{
            ActionData, ActionDecision, ActionDecisionKind, ActionError, ActionPrompt,
            ActionPromptId, ActionPromptKind,
        },
        encounter::{Encounter, EncounterId},
        event::{
            EncounterEvent, Event, EventCallback, EventDispatcher, EventFilter, EventKind,
            EventListener, EventLog, ListenerSource,
        },
        geometry::WorldGeometry,
        prompt::{PendingEvent, PromptManager, PromptScope, PromptScopeId},
    },
    systems::{
        self, actions::ActionUsabilityError, combat::CombatState, movement::MovementError,
        time::RestKind,
    },
};

// TODO: WorldState instead?
pub struct EngineState {
    pub world: World,
    pub geometry: WorldGeometry,

    pub encounters: HashMap<EncounterId, Encounter>,
    pub resting: HashMap<Entity, RestKind>,
    pub prompts: PromptManager,
    pub event_log: EventLog,
    pub event_dispatcher: EventDispatcher,
}

impl EngineState {
    pub fn new(geometry: WorldGeometry) -> Self {
        Self {
            world: World::new(),
            geometry,
            encounters: HashMap::new(),
            resting: HashMap::new(),
            prompts: PromptManager::default(),
            event_log: EventLog::new(),
            event_dispatcher: EventDispatcher::new(),
        }
    }

    pub fn start_encounter_with_id(
        &mut self,
        participants: HashSet<Entity>,
        encounter_id: EncounterId,
    ) -> EncounterId {
        for entity in &participants {
            systems::helpers::get_component_mut::<CombatState>(&mut self.world, *entity)
                .enter_combat(encounter_id);
            systems::time::set_time_mode(
                &mut self.world,
                *entity,
                TimeMode::TurnBased {
                    encounter_id: Some(encounter_id),
                },
            );
        }

        self.event_log
            .push(Event::encounter_event(EncounterEvent::EncounterStarted(
                encounter_id,
            )));

        let encounter = Encounter::new(self, participants, encounter_id);

        self.encounters.insert(encounter_id, encounter);
        encounter_id
    }

    pub fn start_encounter(&mut self, participants: HashSet<Entity>) -> EncounterId {
        self.start_encounter_with_id(participants, EncounterId::new_v4())
    }

    pub fn encounter(&self, encounter_id: &EncounterId) -> Option<&Encounter> {
        self.encounters.get(encounter_id)
    }

    pub fn encounter_mut(&mut self, encounter_id: &EncounterId) -> Option<&mut Encounter> {
        self.encounters.get_mut(encounter_id)
    }

    pub fn encounter_for_entity(&self, entity: Entity) -> Option<&Encounter> {
        match &*systems::helpers::get_component::<CombatState>(&self.world, entity) {
            CombatState::InCombat(encounter_id) => self.encounters.get(encounter_id),
            _ => None,
        }
    }

    pub fn end_encounter(&mut self, encounter_id: &EncounterId) {
        if let Some(mut encounter) = self.encounters.remove(encounter_id) {
            for entity in encounter.participants(&self.world, &[EntityFilter::All]) {
                systems::helpers::get_component_mut::<CombatState>(&mut self.world, entity)
                    .leave_combat();
                systems::time::set_time_mode(&mut self.world, entity, TimeMode::RealTime);
            }
            self.event_log
                .push(Event::encounter_event(EncounterEvent::EncounterEnded(
                    *encounter_id,
                    encounter.event_log_move(),
                )));
        }
    }

    pub fn end_turn(&mut self, entity: Entity) {
        if let Some(scope) = self.scope_for_entity(entity)
            && !scope.pending_events().is_empty()
        {
            warn!(
                "Ending turn for {:?} while there are pending events: {:#?}",
                entity,
                scope.pending_events()
            );
        }

        if let Some(encounter_id) = self
            .encounter_for_entity(entity)
            .map(|encounter| *encounter.id())
            && let Some(mut encounter) = self.encounters.remove(&encounter_id)
        {
            encounter.end_turn(self, entity);
            self.encounters.insert(*encounter.id(), encounter);
        }
    }

    pub fn submit_activity(&mut self, activity: Activity) -> Result<(), ActivityError> {
        match activity {
            Activity::Move { entity, goal } => {
                self.submit_movement(entity, goal, None)?;
            }
            Activity::Act { action } => {
                self.submit_decision(action)?;
            }
            Activity::MoveAndAct { goal, action } => {
                self.submit_movement(action.actor(), goal, Some(action))?;
            }
        }
        Ok(())
    }

    fn submit_movement(
        &mut self,
        entity: Entity,
        goal: Point3<f32>,
        action: Option<ActionDecision>,
    ) -> Result<(), MovementError> {
        if !systems::health::is_alive(&self.world, entity) {
            return Err(MovementError::NotAlive);
        }

        if let Some(encounter) = self.encounter_for_entity(entity)
            && encounter.current_entity() != entity
        {
            return Err(MovementError::NotYourTurn);
        }

        info!("Submitting movement for {:?} to goal {:?}", entity, goal);

        let path = systems::movement::path(
            self,
            entity,
            &goal,
            true,
            systems::combat::is_in_combat(&self, entity),
        )?;

        if systems::movement::speed(self, entity).remaining_movement() <= Length::new::<meter>(0.0)
        {
            return Err(MovementError::InsufficientSpeed);
        }

        systems::helpers::get_component_mut::<ActivityState>(&mut self.world, entity)
            .set_moving(path.taken_path, action);

        Ok(())
    }

    pub(crate) fn scope_id_for_entity(&self, entity: Entity) -> PromptScopeId {
        if let Some(encounter) = self.encounter_for_entity(entity) {
            PromptScopeId::Encounter(*encounter.id())
        } else {
            PromptScopeId::Global
        }
    }

    pub fn scope_for_entity(&self, entity: Entity) -> Option<&PromptScope> {
        let scope = self.scope_id_for_entity(entity);
        self.prompts.scope(&scope)
    }

    pub fn scope_for_entity_mut(&mut self, entity: Entity) -> &mut PromptScope {
        let scope = self.scope_id_for_entity(entity);
        self.prompts.scope_mut(scope)
    }

    pub fn next_prompt(&self, scope: PromptScopeId) -> Option<&ActionPrompt> {
        self.prompts.scope(&scope).and_then(|s| s.next_prompt())
    }

    pub fn next_promt_encounter(&self, encounter_id: &EncounterId) -> Option<&ActionPrompt> {
        self.next_prompt(PromptScopeId::Encounter(*encounter_id))
    }

    pub fn next_prompt_entity(&self, entity: Entity) -> Option<&ActionPrompt> {
        self.next_prompt(self.scope_id_for_entity(entity))
    }

    pub(crate) fn submit_decision(&mut self, decision: ActionDecision) -> Result<(), ActionError> {
        // Check if the action or reaction is even valid before proceeding
        match &decision.kind {
            ActionDecisionKind::Action { action } => {
                self.validate_action(action, false)?;
            }

            ActionDecisionKind::Reaction { choice, .. } => {
                if let Some(choice) = choice {
                    self.validate_action(choice, false)?;
                    systems::actions::reaction_usable(
                        &self,
                        decision.actor(),
                        choice.trigger_event.as_ref().map(|event| event.as_ref()),
                    )
                    .map_err(|e| ActionError::Usability(ActionUsabilityError::ReactionError(e)))?;
                }
            }
        }

        let scope = self.scope_id_for_entity(decision.actor());

        let prompt_id = self.validate_against_prompt(decision, scope)?;

        self.try_process_prompt_decisions(scope, prompt_id)
    }

    fn validate_against_prompt(
        &mut self,
        mut decision: ActionDecision,
        scope_id: PromptScopeId,
    ) -> Result<ActionPromptId, ActionError> {
        let scope = self.prompts.scope_mut(scope_id);

        // Ensure there is a prompt to respond to; lazily create one for Global.
        if scope
            .pending_prompts()
            .iter()
            .all(|p| p.id != decision.response_to)
        {
            if matches!(scope_id, PromptScopeId::Global) {
                // "Open world" behavior, allow ad-hoc Action prompts.
                scope.queue_prompt(
                    ActionPrompt::new(ActionPromptKind::Action {
                        actor: decision.actor(),
                    }),
                    false,
                );
                decision.response_to = scope.pending_prompts().back().unwrap().id;
            } else {
                // In encounter scope, a missing prompt is a hard error.
                return Err(ActionError::MissingPrompt {
                    decision: decision.clone(),
                    prompts: scope.pending_prompts().iter().cloned().collect(),
                });
            }
        }

        // Validate against the found prompt.
        let prompt = scope
            .find_prompt(&decision.response_to)
            .expect("Prompt must exist at this point");

        // TODO: If the prompt validation failed in global scope, and we've just
        // added a prompt, should we remove it again?
        prompt.is_valid_decision(&decision)?;

        let id = prompt.id;

        scope.record_decision(decision);

        Ok(id)
    }

    fn try_process_prompt_decisions(
        &mut self,
        scope_id: PromptScopeId,
        prompt_id: ActionPromptId,
    ) -> Result<(), ActionError> {
        let scope = self.prompts.scope_mut(scope_id);

        if !scope.all_actors_submitted(&prompt_id) {
            return Ok(());
        }

        let decisions = scope
            .take_decisions_for_prompt(&prompt_id)
            .ok_or_else(|| panic!("Decisions not found for prompt id {:?}", prompt_id))?;

        // Convert decisions -> actions/reactions and execute
        for (entity, decision) in &decisions {
            match &decision.kind {
                ActionDecisionKind::Action { action } => {
                    self.process_event_scoped(
                        scope_id,
                        Event::new(EventKind::ActionRequested {
                            action: action.clone(),
                        }),
                    );
                }

                ActionDecisionKind::Reaction {
                    event,
                    reactor,
                    choice,
                } => {
                    self.event_log_mut(*reactor)
                        .record_reaction(event.id, *reactor);

                    // Declined, reactor is no longer a blocker on the pending event
                    let Some(reaction_data) = choice else {
                        debug!(
                            "Clearing blocker for {:?} on prompt {:?} (reaction declined)",
                            entity, prompt_id
                        );
                        self.prompts
                            .scope_mut(scope_id)
                            .clear_blocker(&event.id, *entity);
                        continue;
                    };

                    self.process_event_scoped(
                        scope_id,
                        Event::new(EventKind::ActionRequested {
                            action: reaction_data.clone(),
                        }),
                    );
                }
            }
        }

        // Pop prompt & clear decisions
        {
            let scope = self.prompts.scope_mut(scope_id);
            scope.pop_prompt_by_id(&prompt_id);
        }

        // If we are in encounter scope, validate the next prompt (reactions may prune options)
        self.validate_or_refill_prompt_queue(scope_id);

        // If no reactions are pending, resume paused events.
        self.resume_pending_events_if_ready(scope_id);

        Ok(())
    }

    pub fn process_event(&mut self, event: Event) {
        if let Some(actor) = event.actor() {
            self.process_event_scoped(self.scope_id_for_entity(actor), event)
        } else {
            panic!("Cannot process event without actor: {:#?}", event);
        }
    }

    pub(crate) fn process_event_scoped(&mut self, scope_id: PromptScopeId, event: Event) {
        self.log_event(&scope_id, event.clone());

        let triggerd_listeners = self.event_dispatcher.dispatch(&event);
        for listener_id in &triggerd_listeners {
            if let Some(listener) = self.event_dispatcher.get_listener(listener_id) {
                let callback = listener.callback.clone();
                callback.run(self, &event, &listener.source.clone());
            }
            if let Some(listener) = self.event_dispatcher.get_listener(listener_id)
                && listener.one_shot
            {
                self.event_dispatcher.remove_listener_by_id(listener_id);
            }
        }

        // Reaction window
        if let Some(actor) = event.actor()
            && let Some(reaction_options) = self.collect_reactions(actor, &event)
        {
            // Announce and prompt. The set of potential reactors is exactly the
            // initial `blocked_by`: each reactor stays on the event until their
            // decision resolves (cleared if decline / instant) or their chosen
            // reaction activity completes.
            let blocked_by: HashSet<Entity> = reaction_options.keys().copied().collect();
            let scope = self.prompts.scope_mut(scope_id);
            scope.queue_prompt(
                ActionPrompt::new(ActionPromptKind::Reactions {
                    event: event.clone(),
                    options: reaction_options,
                }),
                true,
            );

            scope.queue_pending_event(PendingEvent::new(event, blocked_by), true);

            return;
        }

        // No reaction window -> advance now
        self.advance_event(event, false);
    }

    fn validate_or_refill_prompt_queue(&mut self, scope_id: PromptScopeId) {
        let new_options = if let Some(scope) = self.prompts.scope(&scope_id)
            && let Some(front) = scope.next_prompt()
        {
            match &front.kind {
                ActionPromptKind::Reactions { event, options } => {
                    let mut new_options = HashMap::new();
                    for reactor in options.keys() {
                        let reactions = systems::actions::available_reactions_to_event(
                            self,
                            *reactor,
                            event,
                            &[],
                        );
                        if !reactions.is_empty() {
                            new_options.insert(*reactor, reactions);
                        }
                    }
                    Some(new_options)
                }
                ActionPromptKind::Action { .. } => None,
            }
        } else {
            None
        };

        let scope = self.prompts.scope_mut(scope_id);

        if let Some(new_options) = new_options {
            if new_options.is_empty() {
                // No valid reactions remain; clear the prompt to skip the reaction window.
                scope.pop_prompt();
            } else {
                // Update the prompt with the new options.
                if let Some(front) = scope.next_prompt_mut()
                    && let ActionPromptKind::Reactions { options, .. } = &mut front.kind
                {
                    *options = new_options;
                }
            }
        }

        match scope_id {
            PromptScopeId::Global => { /* don't auto-refill */ }
            PromptScopeId::Encounter(encounter_id) => {
                let current_entity = self
                    .encounters
                    .get(&encounter_id)
                    .expect("Inconsistent state: encounter not found")
                    .current_entity();

                if scope.pending_prompts().is_empty() {
                    scope.queue_prompt(
                        ActionPrompt::new(ActionPromptKind::Action {
                            actor: current_entity,
                        }),
                        false,
                    );
                }
            }
        }
    }

    pub(crate) fn resume_pending_events_if_ready(&mut self, scope: PromptScopeId) {
        let Some(pending_event) = self.prompts.scope_mut(scope).pop_front_if_ready() else {
            return;
        };

        if pending_event.canceled {
            // If the pending event was canceled, and there's more pending events
            // in the queue, the next event might have been waiting for the actor
            // of the canceled event to unblock it, in which case we should remove
            // the actor manually from the next pending event.
            if let Some(next_pending) = self
                .prompts
                .scope_mut(scope)
                .pending_events_mut()
                .front_mut()
                && let Some(actor) = pending_event.event.actor()
            {
                debug!(
                    "Removing actor {:?} from blocked_by of next pending event {:?}",
                    actor, next_pending.event.id
                );
                next_pending.blocked_by.remove(&actor);
            }

            self.resume_pending_events_if_ready(scope);
            return;
        }

        let event = pending_event.event;

        debug!("Resuming pending event: {:?}", event.id);

        self.advance_event(event, true);

        // The event advance may have delivered results the waiting
        // executions were parked on; drive them forward
        systems::actions::resume_waiting_executions(self, scope);
    }

    fn collect_reactions(
        &mut self,
        actor: Entity,
        event: &Event,
    ) -> Option<HashMap<Entity, Vec<ActionData>>> {
        let reactors = self.get_potential_reactors(actor);

        info!(
            "Collecting reactions to event {:?} from reactors: {:?}",
            event.id, reactors
        );

        let mut reaction_options = HashMap::new();

        for reactor in &reactors {
            if self.event_log(*reactor).has_reacted(&event.id, reactor) {
                continue;
            }

            let reactions =
                systems::actions::available_reactions_to_event(self, *reactor, event, &[]);

            if !reactions.is_empty() {
                reaction_options.insert(*reactor, reactions);
            }
        }

        if reaction_options.is_empty() {
            info!("No reaction options available for event {:?}", event.id);
            None
        } else {
            info!(
                "Found reactors for event {:?}: {:?}",
                event.id,
                reaction_options.keys()
            );
            Some(reaction_options)
        }
    }

    pub fn get_potential_reactors(&self, actor: Entity) -> Vec<Entity> {
        // If in combat, only consider participants. Otherwise, consider all entities
        // that are nearby
        if let Some(encounter) = self.encounter_for_entity(actor) {
            return encounter.participants(&self.world, &[EntityFilter::not_dead()]);
        } else if let Some((_, shape_pose)) = systems::geometry::get_shape(&self.world, actor) {
            return systems::geometry::entities_in_shape(
                &self.world,
                // TODO: Not entirely sure what the right shape is here
                &Ball { radius: 100.0 },
                &shape_pose,
            );
        }
        Vec::new()
    }

    pub fn validate_action(
        &mut self,
        action: &ActionData,
        simulate: bool,
    ) -> Result<(), ActionError> {
        let ActionData {
            instance_id: _,
            actor,
            action_id,
            variant,
            context: action_context,
            resource_cost,
            targets,
            trigger_event: _,
        } = action;

        systems::actions::action_usable_on_targets(
            self,
            actor.id(),
            action_id,
            variant.as_ref(),
            action_context,
            resource_cost,
            targets,
            &[],
        )
        .map_err(ActionError::Usability)?;

        if !simulate {
            systems::resources::spend(&mut self.world, actor.id(), resource_cost)
                .map_err(ActionError::Resource)?;

            systems::spells::break_concentration_if_spell(self, action);
        }

        Ok(())
    }

    // TODO: I guess this is where the event actually "does" something? New name?
    // TODO: Maybe this function should live in the events
    fn advance_event(&mut self, event: Event, process_pending_events: bool) {
        match &event.kind {
            EventKind::ActionRequested { action } => {
                systems::actions::perform_action(self, action);
            }

            EventKind::ActionResult { result, .. } => {
                if let Some(parent) = event.parent.as_ref()
                    && let Some(parent_event) = self.event_log(result.target.id()).get(parent)
                    && let EventKind::ActionRequested { action } = &parent_event.kind
                {
                    let action = action.clone();
                    let hooks = systems::effects::effects(&self.world, action.actor.id())
                        .collect_hooks(|effect| effect.on_action_result.as_ref());
                    for hook in hooks {
                        hook(self, &action, result);
                    }
                }
            }

            EventKind::D20CheckPerformed { actor, result, dc } => {
                let dc = match dc {
                    // TODO: Do we ever need to recalculate DCs for saving throws or skills?
                    D20CheckDC::SavingThrow { .. } | D20CheckDC::Skill { .. } => dc.clone(),
                    D20CheckDC::AttackRoll { target, source, .. } => {
                        // Recalculate AC in case it changed due to reactions
                        let armor_class = systems::loadout::armor_class(self, target.id());
                        D20CheckDC::AttackRoll {
                            target: target.clone(),
                            source: *source,
                            armor_class,
                        }
                    }
                };

                self.process_event_scoped(
                    self.scope_id_for_entity(actor.id()),
                    Event::new(EventKind::D20CheckResolved {
                        actor: actor.clone(),
                        result: result.clone(),
                        dc,
                    })
                    .as_response_to(event.id)
                    .with_parent(event.parent.as_ref()),
                );
            }

            EventKind::DamageRollPerformed { actor, result } => {
                self.process_event_scoped(
                    self.scope_id_for_entity(actor.id()),
                    Event::new(EventKind::DamageRollResolved {
                        actor: actor.clone(),
                        result: result.clone(),
                    })
                    .as_response_to(event.id)
                    .with_parent(event.parent.as_ref()),
                );
            }

            _ => {} // No follow-up event
        }

        if process_pending_events {
            self.resume_pending_events_if_ready(
                self.scope_id_for_entity(
                    event
                        .actor()
                        .expect("Event must have an actor to resume pending events"),
                ),
            );
        }
    }

    fn log_event(&mut self, scope: &PromptScopeId, event: Event) {
        let event_log = match scope {
            PromptScopeId::Global => &mut self.event_log,
            PromptScopeId::Encounter(encounter_id) => {
                if let Some(encounter) = self.encounters.get_mut(encounter_id) {
                    encounter.event_log_mut()
                } else {
                    // In case the encounter is gone or doesn't exist yet, log globally
                    &mut self.event_log
                }
            }
        };

        event_log.push(event);
    }

    pub fn event_log(&self, entity: Entity) -> &EventLog {
        if let Some(encounter) = self.encounter_for_entity(entity) {
            encounter.event_log()
        } else {
            &self.event_log
        }
    }

    pub fn event_log_mut(&mut self, entity: Entity) -> &mut EventLog {
        let encounter_id = self
            .encounter_for_entity(entity)
            .map(|encounter| *encounter.id());

        if let Some(encounter_id) = encounter_id
            && let Some(encounter) = self.encounters.get_mut(&encounter_id)
        {
            encounter.event_log_mut()
        } else {
            &mut self.event_log
        }
    }

    pub fn process_event_with_response_callback(&mut self, event: Event, callback: EventCallback) {
        if let Some(actor) = event.actor() {
            self.event_dispatcher.register_listener(EventListener::new(
                EventFilter::response_to_event_id(event.id),
                callback,
                ListenerSource::EventResponse {
                    trigger_id: event.id,
                },
                true,
            ));

            self.process_event_scoped(self.scope_id_for_entity(actor), event);
        } else {
            panic!(
                "Cannot process event with callback for event without actor: {:#?}",
                event
            );
        }
    }

    pub fn update(&mut self, delta_time: f32) {
        systems::entities::update(self, delta_time);
    }

    pub fn despawn(&mut self, entity: Entity) -> Result<(), NoSuchEntity> {
        info!("Despawning entity {:?}", entity);
        self.world.despawn(entity)?;

        // Despawn is called for *all* entities, so can't guarantee their components
        // e.g. projectiles don't have a CombatState component
        let encounter_id = if let Ok(combat_state) = self.world.get::<&CombatState>(entity)
            && let CombatState::InCombat(encounter_id) = *combat_state
        {
            Some(encounter_id)
        } else {
            None
        };

        if let Some(encounter_id) = encounter_id
            && let Some(mut encounter) = self.encounters.remove(&encounter_id)
        {
            encounter.remove_participant(self, entity);
            self.encounters.insert(*encounter.id(), encounter);
        }

        self.resting.remove(&entity);
        Ok(())
    }
}
