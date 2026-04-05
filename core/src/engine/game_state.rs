use std::collections::{HashMap, HashSet};

use hecs::{Entity, NoSuchEntity, World};
use parry3d::{na::Point3, shape::Ball};
use tracing::{debug, info, warn};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{
        actions::{
            action::{ActionKindResult, ReactionResult},
            targeting::EntityFilter,
        },
        activity::{Activity, ActivityError, ActivityState},
        speed::Speed,
        time::{TimeMode, TimeStep},
    },
    engine::{
        action_prompt::{
            ActionData, ActionDecision, ActionDecisionKind, ActionError, ActionPrompt,
            ActionPromptId, ActionPromptKind, ReactionData,
        },
        encounter::{Encounter, EncounterId},
        event::{
            EncounterEvent, Event, EventCallback, EventDispatcher, EventFilter, EventKind,
            EventListener, EventLog, ListenerSource,
        },
        geometry::WorldGeometry,
        interaction::{InteractionEngine, InteractionScopeId, InteractionSession},
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
                    encounter.combat_log_move(),
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
            false,
            self.in_combat.contains_key(&entity),
            false,
        )?;

        if systems::helpers::get_component::<Speed>(&self.world, entity).remaining_movement()
            <= Length::new::<meter>(0.0)
        {
            return Err(MovementError::InsufficientSpeed);
        }

        let potential_reactors = self.get_potential_reactors(entity);

        debug!("Potential reactors to movement: {:?}", potential_reactors);

        let mut potential_attacks = systems::movement::potential_opportunity_attacks(
            &self.world,
            &path.taken_path,
            entity,
            &potential_reactors,
        );
        // Make sure the opportunity attacks are triggered in the order they are
        // encountered along the path
        potential_attacks.sort_by(|(_, point_a), (_, point_b)| {
            let a_distance = path.taken_path.distance_along_path(point_a);
            let b_distance = path.taken_path.distance_along_path(point_b);
            a_distance.partial_cmp(&b_distance).unwrap()
        });

        debug!("Potential opportunity attacks: {:?}", potential_attacks);

        let (attackers, attack_points): (Vec<_>, Vec<_>) = potential_attacks
            .iter()
            .map(|(attacker, attack_point)| (*attacker, attack_point.clone()))
            .unzip();

        let (final_path, attack_indices) = path.taken_path.insert_points(&attack_points);
        let opportunity_attacks: HashMap<usize, Entity> = attack_indices
            .into_iter()
            .zip(attackers.into_iter())
            .collect();

        systems::helpers::get_component_mut::<ActivityState>(&mut self.world, entity).set_moving(
            final_path,
            opportunity_attacks,
            action,
        );

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
        let (all_decisions_ready, decisions) = {
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
            (all_actors_submitted, decisions_map)
        };

        if !all_decisions_ready {
            return Ok(());
        }

        // Convert decisions → actions / reactions and validate/execute
        for (_actor, decision) in decisions {
            match &decision.kind {
                ActionDecisionKind::Action { action } => {
                    // Normal action flow: validate + enqueue ActionRequested
                    self.validate_action(action, true)?;
                    self.process_event_scoped(
                        scope,
                        Event::new(EventKind::ActionRequested {
                            action: action.clone(),
                        }),
                    );
                }

                ActionDecisionKind::Reaction { choice, .. } => {
                    // No reaction chosen – skip
                    let Some(reaction_data) = choice else {
                        continue;
                    };

                    // Validate reaction as if it were an action:
                    let action_view = ActionData::from(reaction_data);
                    self.validate_action(&action_view, true)?;
                    self.process_event_scoped(
                        scope,
                        Event::new(EventKind::ReactionRequested {
                            reaction: reaction_data.clone(),
                        }),
                    );

                    // If perform_reaction started an activity on the reactor,
                    // track it so ready_to_resume() blocks until it completes.
                    let reactor = reaction_data.reactor.id();
                    if systems::helpers::get_component::<ActivityState>(&self.world, reactor)
                        .is_acting()
                    {
                        self.interaction_engine
                            .session_mut(scope)
                            .add_pending_reactor(reactor);
                    }
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
            // Announce and prompt
            let session = self.interaction_engine.session_mut(scope);
            session.queue_prompt(
                ActionPrompt::new(ActionPromptKind::Reactions {
                    event: event.clone(),
                    options: reaction_options,
                }),
                true,
            );
            session.queue_event(event, true);

            systems::helpers::get_component_mut::<ActivityState>(&mut self.world, actor).pause();
            self.set_projectiles_paused_for_entity(actor, true);

            return;
        }

        // No reaction window → advance now
        self.advance_event(event, false);
    }

    fn validate_or_refill_prompt_queue(&mut self, scope: InteractionScopeId) {
        let session = self.interaction_engine.session_mut(scope);

        if let Some(front) = session.next_prompt_mut() {
            let mut invalid = false;

            match &mut front.kind {
                ActionPromptKind::Reactions { event, options } => {
                    let mut new_options = HashMap::new();
                    for reactor in options.keys() {
                        let reactions = systems::actions::available_reactions_to_event(
                            &self.world,
                            &self.geometry,
                            *reactor,
                            event,
                        );
                        if !reactions.is_empty() {
                            new_options.insert(*reactor, reactions);
                        }
                    }
                    if new_options.is_empty() {
                        invalid = true;
                    }
                    *options = new_options;
                }
                ActionPromptKind::Action { .. } => { /* nothing */ }
            }

            if invalid {
                session.pop_prompt();
            }
        }

        match scope {
            InteractionScopeId::Global => { /* don't auto-refill */ }
            InteractionScopeId::Encounter(encounter_id) => {
                let encounter = self
                    .encounters
                    .get_mut(&encounter_id)
                    .expect("Inconsistent state: encounter not found");

                if session.pending_prompts().is_empty() {
                    session.queue_prompt(
                        ActionPrompt::new(ActionPromptKind::Action {
                            actor: encounter.current_entity(),
                        }),
                        false,
                    );
                }
            }
        }
    }

    pub(crate) fn resume_pending_events_if_ready(&mut self, scope: InteractionScopeId) {
        let (event, actor, pending_phase) = {
            let session = self.interaction_engine.session_mut(scope);
            if !session.ready_to_resume() {
                return;
            }

            let Some(event) = session.pending_events_mut().pop_front() else {
                return;
            };

            let actor = event.actor();

            (event, actor, session.take_pending_phase())
        };

        self.advance_event(event, true);

        if let Some(actor) = actor {
            systems::helpers::get_component_mut::<ActivityState>(&mut self.world, actor).resume();
            self.set_projectiles_paused_for_entity(actor, false);
        }

        if let Some((entity, mut phase)) = pending_phase {
            phase.perform(self);
            if !phase.is_applied() {
                let scope = self.scope_for_entity(entity);
                self.interaction_engine
                    .session_mut(scope)
                    .set_pending_phase(entity, phase);
            }
        }
    }

    fn collect_reactions(
        &mut self,
        actor: Entity,
        event: &Event,
    ) -> Option<HashMap<Entity, Vec<ReactionData>>> {
        let reactors = self.get_potential_reactors(actor);

        info!(
            "Collecting reactions to event {:?} from reactors: {:?}",
            event.id, reactors
        );

        let mut reaction_options = HashMap::new();

        for reactor in &reactors {
            if self.event_log.has_reacted(&event.id, reactor) {
                continue;
            }

            let reactions = systems::actions::available_reactions_to_event(
                &self.world,
                &self.geometry,
                *reactor,
                event,
            );

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

    fn get_potential_reactors(&self, actor: Entity) -> Vec<Entity> {
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
                Box::new(Ball { radius: 100.0 }),
                &shape_pose,
            );
        }
        return Vec::new();
    }

    pub fn validate_action(
        &mut self,
        action: &ActionData,
        // TODO: Could also be called simulate?
        spend_resources: bool,
    ) -> Result<(), ActionError> {
        let ActionData {
            instance_id: _,
            actor,
            action_id,
            context: action_context,
            resource_cost,
            targets,
        } = action;

        systems::actions::action_usable_on_targets(
            &self.world,
            &self.geometry,
            actor.id(),
            action_id,
            action_context,
            resource_cost,
            targets,
        )
        .map_err(|error| ActionError::Usability(error))?;

        if spend_resources {
            systems::resources::spend(&mut self.world, actor.id(), resource_cost)
                .map_err(|error| ActionError::Resource(error))?;
        }

        Ok(())
    }

    // TODO: I guess this is where the event actually "does" something? New name?
    // TODO: Maybe this function should live in the events
    fn advance_event(&mut self, event: Event, process_pending_events: bool) {
        match &event.kind {
            EventKind::MovingOutOfReach {
                mover,
                continue_movement,
                ..
            } => {
                let mut state = systems::helpers::get_component_mut::<ActivityState>(
                    &mut self.world,
                    mover.id(),
                );
                if *continue_movement {
                    state.resume();
                } else {
                    state.set_idle();
                }
            }

            EventKind::ActionRequested { action } => {
                systems::actions::perform_action(self, action);
            }

            EventKind::ReactionRequested { reaction } => {
                systems::actions::perform_reaction(self, reaction);
            }

            EventKind::ActionPerformed { action, results } => {
                let effects = systems::effects::take_effects(&mut self.world, action.actor.id());
                effects.action_result(self, action, results);
                systems::effects::put_effects(&mut self.world, action.actor.id(), effects);

                for action_result in results {
                    match &action_result.kind {
                        ActionKindResult::Reaction {
                            result: reaction_result,
                        } => {
                            let session = self
                                .interaction_engine
                                .session_mut(self.scope_for_entity(action_result.performer.id()));

                            match reaction_result {
                                ReactionResult::CancelEvent {
                                    event,
                                    resources_refunded,
                                } => {
                                    info!(
                                        "Cancelling event {:?} due to reaction by {:?}",
                                        event.id,
                                        action_result.performer.id()
                                    );

                                    if let Some(actor) =
                                        session.pending_events().iter().find_map(|e| {
                                            if e.id == event.id { e.actor() } else { None }
                                        })
                                    {
                                        systems::resources::restore(
                                            &mut self.world,
                                            actor,
                                            resources_refunded,
                                        );
                                        session.pending_events_mut().retain(|e| e.id != event.id);
                                    } else {
                                        panic!(
                                            "Attempted to cancel event which is not pending: {:#?}",
                                            event
                                        );
                                    }
                                }

                                // TODO: How to handle this properly?
                                ReactionResult::ModifyEvent { modification } => {
                                    info!(
                                        "Modifying event {:?} due to reaction by {:?}",
                                        event.id,
                                        action_result.performer.id()
                                    );
                                    (modification)(
                                        &self.world,
                                        &mut session.pending_events_mut().front_mut().unwrap(),
                                    );
                                }

                                ReactionResult::NoEffect => { /* Do nothing */ }
                            }
                        }

                        _ => {}
                    }
                }
            }

            EventKind::D20CheckPerformed(entity, kind, dc_kind) => {
                let dc = match dc_kind {
                    // TODO: Do we ever need to recalculate DCs for saving throws or skills?
                    D20CheckDCKind::SavingThrow(_) | D20CheckDCKind::Skill(_) => dc_kind.clone(),
                    D20CheckDCKind::AttackRoll(target, _) => {
                        // Recalculate AC in case it changed due to reactions
                        let armor_class = systems::loadout::armor_class(&self.world, target.id());
                        D20CheckDCKind::AttackRoll(target.clone(), armor_class)
                    }
                };

                let _ = self.process_event_scoped(
                    self.scope_for_entity(entity.id()),
                    Event::new(EventKind::D20CheckResolved(
                        entity.clone(),
                        kind.clone(),
                        dc,
                    ))
                    .as_response_to(event.id),
                );
            }

            EventKind::DamageRollPerformed(entity, damage) => {
                let _ = self.process_event_scoped(
                    self.scope_for_entity(entity.id()),
                    Event::new(EventKind::DamageRollResolved(
                        entity.clone(),
                        damage.clone(),
                    ))
                    .as_response_to(event.id),
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
        match scope {
            InteractionScopeId::Global => self.event_log.push(event),
            InteractionScopeId::Encounter(encounter_id) => {
                if let Some(encounter) = self.encounters.get_mut(&encounter_id) {
                    encounter.log_event(event);
                } else {
                    // In case the encounter is gone or doesn't exist yet, log globally
                    self.event_log.push(event);
                }
            }
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

    // TODO: Not a huge fan of having these projectile specific functions in the main GameState impl
    fn set_projectiles_paused_for_entity(&mut self, entity: Entity, paused: bool) {
        let pairs: Vec<(Entity, Entity)> = self
            .world
            .query::<&Projectile>()
            .iter()
            .map(|(entity, projectile)| (entity, projectile.delivery_phase.action.actor.id()))
            .collect();
        debug!("Found projectiles to check for pausing: {:?}", pairs);
        for (proj_entity, actor) in pairs {
            if actor == entity
                && let Ok(mut projectile) = self.world.get::<&mut Projectile>(proj_entity)
            {
                projectile.paused = paused;
            }
        }
    }
}
