use std::collections::{HashMap, HashSet};

use hecs::{Entity, NoSuchEntity, World};
use parry3d::{na::Point3, shape::Ball};
use tracing::{debug, info, warn};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{
        actions::{
            execution::{ActionExecution, ResumePayload},
            targeting::EntityFilter,
        },
        activity::{Activity, ActivityError, ActivityPauseReason, ActivityState},
        speed::Speed,
        time::{TimeMode, TimeStep},
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
        interaction::{InteractionEngine, InteractionScopeId, InteractionSession, PendingEvent},
    },
    entities::{
        character::CreatureTag,
        projectile::{Projectile, ProjectileTag},
    },
    systems::{self, d20::D20CheckDCKind, movement::MovementError, time::RestKind},
};

// TODO: WorldState instead?
pub struct GameState {
    pub world: World,
    pub geometry: WorldGeometry,

    pub encounters: HashMap<EncounterId, Encounter>,
    pub in_combat: HashMap<Entity, EncounterId>,
    pub resting: HashMap<Entity, RestKind>,
    pub interaction_engine: InteractionEngine,
    pub event_log: EventLog,
    pub event_dispatcher: EventDispatcher,
    /// Action currently being executed (if any) for each entity
    pub action_executions: HashMap<Entity, ActionExecution>,
    /// Results delivered by event response callbacks, picked up by the
    /// awaiting execution when it next runs
    pub execution_mailbox: HashMap<Entity, ResumePayload>,
}

impl GameState {
    pub fn new(geometry: WorldGeometry) -> Self {
        Self {
            world: World::new(),
            geometry,
            encounters: HashMap::new(),
            in_combat: HashMap::new(),
            resting: HashMap::new(),
            interaction_engine: InteractionEngine::default(),
            event_log: EventLog::new(),
            event_dispatcher: EventDispatcher::new(),
            action_executions: HashMap::new(),
            execution_mailbox: HashMap::new(),
        }
    }

    pub fn start_encounter_with_id(
        &mut self,
        participants: HashSet<Entity>,
        encounter_id: EncounterId,
    ) -> EncounterId {
        for entity in &participants {
            self.in_combat.insert(*entity, encounter_id.clone());
            systems::time::set_time_mode(
                &mut self.world,
                *entity,
                TimeMode::TurnBased {
                    encounter_id: Some(encounter_id.clone()),
                },
            );
        }

        self.event_log
            .push(Event::encounter_event(EncounterEvent::EncounterStarted(
                encounter_id.clone(),
            )));

        let encounter = Encounter::new(self, participants, encounter_id.clone());

        self.encounters.insert(encounter_id.clone(), encounter);
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

    pub fn encounter_for_entity(&self, entity: &Entity) -> Option<&EncounterId> {
        self.in_combat.get(entity)
    }

    pub fn end_encounter(&mut self, encounter_id: &EncounterId) {
        if let Some(mut encounter) = self.encounters.remove(encounter_id) {
            for entity in encounter.participants(&self.world, &[EntityFilter::All]) {
                self.in_combat.remove(&entity);
                systems::time::set_time_mode(&mut self.world, entity, TimeMode::RealTime);
            }
            self.event_log
                .push(Event::encounter_event(EncounterEvent::EncounterEnded(
                    encounter_id.clone(),
                    encounter.event_log_move(),
                )));
        }
    }

    pub fn end_turn(&mut self, entity: Entity) {
        let encounter = if let Some(encounter_id) = self.in_combat.get(&entity) {
            if let Some(encounter) = self.encounters.get_mut(encounter_id) {
                unsafe { Some(&mut *(encounter as *mut Encounter)) }
            } else {
                panic!("Inconsistent state: entity is in combat but encounter not found");
            }
        } else {
            None
        };

        if let Some(session) = self.session_for_entity(entity) {
            if !session.pending_events().is_empty() {
                warn!(
                    "Ending turn for {:?} while there are pending events: {:#?}",
                    entity,
                    session.pending_events()
                );
            }
        }

        if let Some(encounter) = encounter {
            encounter.end_turn(self, entity);
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

        if let Some(encounter_id) = self.in_combat.get(&entity) {
            if let Some(encounter) = self.encounters.get_mut(encounter_id) {
                if encounter.current_entity() != entity {
                    return Err(MovementError::NotYourTurn);
                }
            } else {
                panic!("Inconsistent state: entity is in combat but encounter not found");
            }
        }

        info!("Submitting movement for {:?} to goal {:?}", entity, goal);

        let path = systems::movement::path(
            self,
            entity,
            &goal,
            true,
            self.in_combat.contains_key(&entity),
        )?;

        if systems::helpers::get_component::<Speed>(&self.world, entity).remaining_movement()
            <= Length::new::<meter>(0.0)
        {
            return Err(MovementError::InsufficientSpeed);
        }

        systems::helpers::get_component_mut::<ActivityState>(&mut self.world, entity)
            .set_moving(path.taken_path, action);

        Ok(())
    }

    pub(crate) fn scope_for_entity(&self, entity: Entity) -> InteractionScopeId {
        if let Some(id) = self.in_combat.get(&entity) {
            InteractionScopeId::Encounter(*id)
        } else {
            InteractionScopeId::Global
        }
    }

    pub fn session_for_entity(&self, entity: Entity) -> Option<&InteractionSession> {
        let scope = self.scope_for_entity(entity);
        self.interaction_engine.session(scope)
    }

    pub fn session_for_entity_mut(&mut self, entity: Entity) -> &mut InteractionSession {
        let scope = self.scope_for_entity(entity);
        self.interaction_engine.session_mut(scope)
    }

    pub fn next_prompt(&self, scope: InteractionScopeId) -> Option<&ActionPrompt> {
        self.interaction_engine
            .session(scope)
            .and_then(|s| s.next_prompt())
    }

    pub fn next_promt_encounter(&self, encounter_id: &EncounterId) -> Option<&ActionPrompt> {
        self.next_prompt(InteractionScopeId::Encounter(*encounter_id))
    }

    pub fn next_prompt_entity(&self, entity: Entity) -> Option<&ActionPrompt> {
        self.next_prompt(self.scope_for_entity(entity))
    }

    pub(crate) fn submit_decision(
        &mut self,
        mut decision: ActionDecision,
    ) -> Result<(), ActionError> {
        let scope = self.scope_for_entity(decision.actor());

        // Avoid double mutable borrow
        let prompt_id = {
            let session = self.interaction_engine.session_mut(scope);

            // Ensure there is a prompt to respond to; lazily create one for Global.
            if session
                .pending_prompts()
                .iter()
                .all(|p| p.id != decision.response_to)
            {
                if matches!(scope, InteractionScopeId::Global) {
                    // “Open world” behavior: allow ad-hoc Action prompts.
                    session.queue_prompt(
                        ActionPrompt::new(ActionPromptKind::Action {
                            actor: decision.actor(),
                        }),
                        false,
                    );
                    decision.response_to = session.pending_prompts().back().unwrap().id;
                } else {
                    // In encounter scope, a missing prompt is a hard error.
                    return Err(ActionError::MissingPrompt {
                        decision: decision.clone(),
                        prompts: session.pending_prompts().iter().cloned().collect(),
                    });
                }
            }

            // Validate against the found prompt.
            let prompt = session
                .pending_prompts()
                .iter()
                .find(|p| p.id == decision.response_to)
                .expect("Prompt must exist at this point");
            prompt.is_valid_decision(&decision)?;
            let id = prompt.id;

            session.record_decision(decision);

            id
        };

        self.try_process_prompt(scope, prompt_id)
    }

    fn try_process_prompt(
        &mut self,
        scope: InteractionScopeId,
        prompt_id: ActionPromptId,
    ) -> Result<(), ActionError> {
        let (all_decisions_ready, decisions, trigger_actor) = {
            let session = self.interaction_engine.session(scope);
            let prompt = session
                .and_then(|s| s.pending_prompts().iter().find(|p| p.id == prompt_id))
                .cloned()
                .ok_or_else(|| panic!("Prompt disappeared"))
                .unwrap();

            let decisions_map = self
                .interaction_engine
                .session(scope)
                .unwrap()
                .decisions_for_prompt(&prompt_id)
                .cloned()
                .unwrap_or_default();

            let all_actors_submitted = prompt
                .actors()
                .iter()
                .all(|a| decisions_map.contains_key(a));

            let trigger_actor = match &prompt.kind {
                ActionPromptKind::Reactions { event, .. } => event.actor(),
                ActionPromptKind::Action { .. } => None,
            };

            (all_actors_submitted, decisions_map, trigger_actor)
        };

        if !all_decisions_ready {
            return Ok(());
        }

        // Resume all the entities that were waiting for this prompt to resolve
        let mut entities = decisions.keys().cloned().collect::<Vec<_>>();
        if let Some(trigger) = trigger_actor {
            entities.push(trigger);
        }
        for entity in entities {
            systems::actions::resume_action(self, entity, ActivityPauseReason::Reaction);
        }

        // Convert decisions → actions / reactions and validate/execute
        for (entity, decision) in &decisions {
            match &decision.kind {
                ActionDecisionKind::Action { action } => {
                    // Normal action flow: validate + enqueue ActionRequested
                    self.validate_action(action, false)?;
                    self.process_event_scoped(
                        scope,
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

                    // Declined – reactor is no longer a blocker on any pending event.
                    let Some(reaction_data) = choice else {
                        debug!(
                            "Clearing blocker for {:?} on prompt {:?} (reaction declined)",
                            entity, prompt_id
                        );
                        self.interaction_engine
                            .session_mut(scope)
                            .clear_blocker(*entity);
                        continue;
                    };

                    self.validate_action(&reaction_data, false)?;
                    self.process_event_scoped(
                        scope,
                        Event::new(EventKind::ActionRequested {
                            action: reaction_data.clone(),
                        }),
                    );
                }
            }
        }

        // Pop prompt & clear decisions
        {
            let session = self.interaction_engine.session_mut(scope);
            session.pop_prompt_by_id(&prompt_id);
        }

        // If we are in encounter scope, validate the next prompt (reactions may prune options)
        self.validate_or_refill_prompt_queue(scope);

        // If no reactions are pending, resume paused events.
        self.resume_pending_events_if_ready(scope);

        Ok(())
    }

    pub fn process_event(&mut self, event: Event) {
        if let Some(actor) = event.actor() {
            self.process_event_scoped(self.scope_for_entity(actor), event)
        } else {
            panic!("Cannot process event without actor: {:#?}", event);
        }
    }

    pub(crate) fn process_event_scoped(&mut self, scope: InteractionScopeId, event: Event) {
        self.log_event(&scope, event.clone());

        let triggerd_listeners = self.event_dispatcher.dispatch(&event);
        for listener_id in &triggerd_listeners {
            if let Some(listener) = self.event_dispatcher.get_listener(&listener_id) {
                let callback = listener.callback.clone();
                callback.run(self, &event, &listener.source.clone());
            }
            if let Some(listener) = self.event_dispatcher.get_listener(&listener_id) {
                if listener.one_shot {
                    self.event_dispatcher.remove_listener_by_id(&listener_id);
                }
            }
        }

        // Reaction window
        if let Some(actor) = event.actor()
            && let Some(reaction_options) = self.collect_reactions(actor, &event)
        {
            // Pause everyone involved in the reaction
            let mut entities = reaction_options.keys().cloned().collect::<Vec<_>>();
            entities.push(actor);
            for entity in entities {
                systems::actions::pause_action(self, entity, ActivityPauseReason::Reaction);
            }

            // Announce and prompt. The set of potential reactors is exactly the
            // initial `blocked_by`: each reactor stays on the event until their
            // decision resolves (cleared if decline / instant) or their chosen
            // reaction activity completes.
            let blocked_by: HashSet<Entity> = reaction_options.keys().copied().collect();
            let session = self.interaction_engine.session_mut(scope);
            session.queue_prompt(
                ActionPrompt::new(ActionPromptKind::Reactions {
                    event: event.clone(),
                    options: reaction_options,
                }),
                true,
            );
            session.queue_pending_event(PendingEvent::new(event, blocked_by), true);

            return;
        }

        // No reaction window → advance now
        self.advance_event(event, false);
    }

    fn validate_or_refill_prompt_queue(&mut self, scope: InteractionScopeId) {
        let new_options = if let Some(session) = self.interaction_engine.session(scope)
            && let Some(front) = session.next_prompt()
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

        let session = self.interaction_engine.session_mut(scope);

        if let Some(new_options) = new_options {
            if new_options.is_empty() {
                // No valid reactions remain; clear the prompt to skip the reaction window.
                session.pop_prompt();
            } else {
                // Update the prompt with the new options.
                if let Some(front) = session.next_prompt_mut() {
                    if let ActionPromptKind::Reactions { options, .. } = &mut front.kind {
                        *options = new_options;
                    }
                }
            }
        }

        match scope {
            InteractionScopeId::Global => { /* don't auto-refill */ }
            InteractionScopeId::Encounter(encounter_id) => {
                let current_entity = self
                    .encounters
                    .get(&encounter_id)
                    .expect("Inconsistent state: encounter not found")
                    .current_entity();

                if session.pending_prompts().is_empty() {
                    session.queue_prompt(
                        ActionPrompt::new(ActionPromptKind::Action {
                            actor: current_entity,
                        }),
                        false,
                    );
                }
            }
        }
    }

    pub(crate) fn resume_pending_events_if_ready(&mut self, scope: InteractionScopeId) {
        loop {
            let event_opt = {
                let session = self.interaction_engine.session_mut(scope);
                let Some(idx) = session.first_drainable_event() else {
                    return;
                };
                session.pending_events_mut().remove(idx).map(|pe| pe.event)
            };
            let Some(event) = event_opt else {
                return;
            };

            debug!("Resuming pending event: {:?}", event.id);

            self.advance_event(event, true);

            // The event advance may have delivered results the waiting
            // executions were parked on; drive them forward
            systems::actions::resume_waiting_executions(self, scope);
        }
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
        if let Some(encounter_id) = self.in_combat.get(&actor)
            && let Some(encounter) = self.encounters.get(encounter_id)
        {
            return encounter.participants(&self.world, &[EntityFilter::not_dead()]);
        } else if let Some((_, shape_pose)) = systems::geometry::get_shape(&self.world, actor) {
            return systems::geometry::entities_in_shape(
                &self.world,
                // TODO: Not entirely sure what the right shape is here
                &Ball { radius: 100.0 },
                &shape_pose,
            );
        }
        return Vec::new();
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
            context: action_context,
            resource_cost,
            targets,
            trigger_event: _,
        } = action;

        systems::actions::action_usable_on_targets(
            self,
            actor.id(),
            action_id,
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
                    D20CheckDCKind::SavingThrow(_) | D20CheckDCKind::Skill(_) => dc.clone(),
                    D20CheckDCKind::AttackRoll(target, source, _) => {
                        // Recalculate AC in case it changed due to reactions
                        let armor_class = systems::loadout::armor_class(self, target.id());
                        D20CheckDCKind::AttackRoll(target.clone(), source.clone(), armor_class)
                    }
                };

                let _ = self.process_event_scoped(
                    self.scope_for_entity(actor.id()),
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
                let _ = self.process_event_scoped(
                    self.scope_for_entity(actor.id()),
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
                self.scope_for_entity(
                    event
                        .actor()
                        .expect("Event must have an actor to resume pending events"),
                ),
            );
        }
    }

    fn log_event(&mut self, scope: &InteractionScopeId, event: Event) {
        let event_log = match scope {
            InteractionScopeId::Global => &mut self.event_log,
            InteractionScopeId::Encounter(encounter_id) => {
                if let Some(encounter) = self.encounters.get_mut(&encounter_id) {
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
        if let Some(encounter_id) = self.in_combat.get(&entity)
            && let Some(encounter) = self.encounters.get(encounter_id)
        {
            encounter.event_log()
        } else {
            &self.event_log
        }
    }

    pub fn event_log_mut(&mut self, entity: Entity) -> &mut EventLog {
        if let Some(encounter_id) = self.in_combat.get(&entity)
            && let Some(encounter) = self.encounters.get_mut(encounter_id)
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

            self.process_event_scoped(self.scope_for_entity(actor), event);
        } else {
            panic!(
                "Cannot process event with callback for event without actor: {:#?}",
                event
            );
        }
    }

    pub fn update(&mut self, delta_time: f32) {
        let time_step = TimeStep::RealTime {
            delta_seconds: delta_time,
        };

        // Projectile entities
        let projectiles: Vec<Entity> = self
            .world
            .query::<&ProjectileTag>()
            .iter()
            .map(|(e, _)| e)
            .collect();

        for projectile in projectiles {
            let commands =
                systems::helpers::get_component_mut::<Projectile>(&mut self.world, projectile)
                    .update(projectile, delta_time);
            for command in commands {
                command.execute(self);
            }
        }

        // Creature entities
        let entities = self
            .world
            .query::<&CreatureTag>()
            .iter()
            .map(|(e, _)| e)
            .collect::<Vec<_>>();

        for entity in entities {
            systems::time::advance_time(self, entity, time_step);

            let marked_effects =
                systems::effects::effects_mut(&mut self.world, entity).take_marked_for_removal();
            if !marked_effects.is_empty() {
                systems::effects::remove_effects(
                    self,
                    entity,
                    &marked_effects.into_iter().collect::<Vec<_>>(),
                );
            }

            // TODO: No idea where to put this
            if !systems::ai::is_player_controlled(&self.world, entity)
                && systems::helpers::get_component::<ActivityState>(&self.world, entity).is_idle()
                && let Some(prompt) = self.next_prompt_entity(entity).cloned()
                && prompt.kind.actors().contains(&entity)
            {
                if let Some(activity) = systems::ai::decide_activity(self, &prompt, entity) {
                    let result = self.submit_activity(activity);
                    info!("AI submitted activity: {:?}", result);
                } else {
                    self.end_turn(entity);
                }
            }

            // TODO: Not entirely sure where to place this
            // TODO: Very hacky
            let world = unsafe { &mut *(&mut self.world as *mut World) };
            let commands = systems::helpers::get_component_mut::<ActivityState>(world, entity)
                .update(self, entity, delta_time);
            for command in commands {
                command.execute(self);
            }
        }
    }

    pub fn despawn(&mut self, entity: Entity) -> Result<(), NoSuchEntity> {
        info!("Despawning entity {:?}", entity);
        self.world.despawn(entity)?;
        if let Some(encounter_id) = self.in_combat.remove(&entity)
            && let Some(encounter) = self.encounters.get_mut(&encounter_id)
        {
            let encounter = unsafe { &mut *(encounter as *mut Encounter) };
            encounter.remove_participant(self, entity);
        }
        self.resting.remove(&entity);
        Ok(())
    }
}
