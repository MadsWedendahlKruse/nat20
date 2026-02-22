use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::Arc,
};

use hecs::Entity;
use uom::si::f32::Length;
use uuid::Uuid;

use crate::{
    components::{
        actions::{
            action::{ActionKindResult, ActionResult},
            targeting::TargetInstance,
        },
        damage::DamageRollResult,
        effects::effect::EffectInstanceId,
        health::life_state::LifeState,
        id::EffectId,
        spells::spell::ConcentrationInstance,
        time::TurnBoundary,
    },
    engine::{
        action_prompt::{ActionData, ReactionData},
        encounter::EncounterId,
        game_state::GameState,
        geometry::WorldPath,
    },
    systems::{
        d20::{D20CheckDCKind, D20ResultKind},
        time::RestKind,
    },
};

pub type EventId = Uuid;

#[derive(Debug, Clone, PartialEq)]
pub struct Event {
    pub id: EventId,
    pub kind: EventKind,
    pub response_to: Option<EventId>,
}

impl Event {
    pub fn new(kind: EventKind) -> Self {
        Self {
            id: Uuid::new_v4(),
            kind,
            response_to: None,
        }
    }

    pub fn as_response_to(mut self, event_id: EventId) -> Self {
        self.response_to = Some(event_id);
        self
    }

    pub fn encounter_event(encounter_event: EncounterEvent) -> Self {
        Self::new(EventKind::Encounter(encounter_event))
    }

    pub fn actor(&self) -> Option<Entity> {
        match &self.kind {
            EventKind::MovementRequested { entity, .. } => Some(*entity),
            EventKind::MovingOutOfReach { mover, .. } => Some(*mover),
            EventKind::MovementPerformed { entity, .. } => Some(*entity),
            EventKind::ActionRequested { action } => Some(action.actor),
            EventKind::ActionPerformed { action, .. } => Some(action.actor),
            // TODO: What to do here? Multiple reactors?
            EventKind::ReactionTriggered { reactors, .. } => Some(*reactors.iter().next()?),
            EventKind::ReactionRequested { reaction } => Some(reaction.reactor),
            EventKind::LifeStateChanged { entity, actor, .. } => {
                if let Some(actor) = actor {
                    Some(*actor)
                } else {
                    Some(*entity)
                }
            }
            EventKind::D20CheckPerformed(entity, _, _) => Some(*entity),
            EventKind::D20CheckResolved(entity, _, _) => Some(*entity),
            EventKind::DamageRollPerformed(entity, _) => Some(*entity),
            EventKind::DamageRollResolved(entity, _) => Some(*entity),
            EventKind::Encounter(_) => None,
            EventKind::TurnBoundary { entity, .. } => Some(*entity),
            // TODO: Same problem as ReactionTriggered
            EventKind::RestStarted { participants, .. } => Some(*participants.first()?),
            EventKind::RestFinished { participants, .. } => Some(*participants.first()?),
            EventKind::LostConcentration { entity, .. } => Some(*entity),
            EventKind::LostEffect { entity, .. } => Some(*entity),
        }
    }

    pub fn target(&self) -> Option<Entity> {
        match &self.kind {
            EventKind::ActionRequested { action } => {
                if let Some(TargetInstance::Entity(target)) = action.targets.first() {
                    Some(*target)
                } else {
                    None
                }
            }
            EventKind::ActionPerformed { action, .. } => {
                if let Some(TargetInstance::Entity(target)) = action.targets.first() {
                    Some(*target)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn action_performed_event(
        game_state: &GameState,
        action_data: &ActionData,
        results: Vec<(Entity, ActionKindResult)>,
    ) -> Event {
        let results = results
            .into_iter()
            .map(|(entity, result)| {
                ActionResult::new(&game_state.world, action_data.actor, entity, result)
            })
            .collect();
        Event::new(EventKind::ActionPerformed {
            action: action_data.clone(),
            results: results,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EventKind {
    Encounter(EncounterEvent),

    MovementRequested {
        entity: Entity,
        path: WorldPath,
    },
    MovementPerformed {
        entity: Entity,
        path: WorldPath,
    },
    MovingOutOfReach {
        mover: Entity,
        entity: Entity,
        continue_movement: bool,
    },

    /// An entity has declared they want to take an action. The engine can then
    /// validate that the entity can perform the action and either approve or
    /// deny it. Other entities might also react to the request, e.g. if someone
    /// is casting a spell, another entity might use their reaction to Counterspell
    /// the action.
    ActionRequested {
        action: ActionData,
    },
    /// The action was successfully performed, and the results are applied to the targets.
    ActionPerformed {
        action: ActionData,
        results: Vec<ActionResult>,
    },
    ReactionTriggered {
        /// The event that triggered the reaction, e.g. an ActionRequested event
        /// might trigger a Counterspell reaction.
        trigger_event: Arc<Event>,
        reactors: HashSet<Entity>,
    },
    ReactionRequested {
        reaction: ReactionData,
    },
    LifeStateChanged {
        entity: Entity,
        new_state: LifeState,
        /// The entity that caused the change, if any
        actor: Option<Entity>,
    },
    /// The initial D20 roll which can be reacted to, e.g. with the Lucky feat.
    D20CheckPerformed(Entity, D20ResultKind, D20CheckDCKind),
    /// The final result of a D20 check after reactions have been applied.
    D20CheckResolved(Entity, D20ResultKind, D20CheckDCKind),
    DamageRollPerformed(Entity, DamageRollResult),
    DamageRollResolved(Entity, DamageRollResult),

    TurnBoundary {
        entity: Entity,
        boundary: TurnBoundary,
    },

    RestStarted {
        kind: RestKind,
        participants: Vec<Entity>,
    },
    RestFinished {
        kind: RestKind,
        participants: Vec<Entity>,
    },

    LostConcentration {
        entity: Entity,
        instances: Vec<ConcentrationInstance>,
    },
    LostEffect {
        entity: Entity,
        effect: EffectId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum EncounterEvent {
    EncounterStarted(EncounterId),
    EncounterEnded(EncounterId, EventLog),
    NewRound(EncounterId, usize),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct EventLog {
    pub events: Vec<Event>,
    /// Track which entities have reacted to which events in this log. This is
    /// used to prevent an entity from reacting to the same event multiple times.
    /// TODO: Not sure if this is the best solution.
    pub reactors: HashMap<EventId, HashSet<Entity>>,
}

impl EventLog {
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
            reactors: HashMap::new(),
        }
    }

    pub fn push(&mut self, event: Event) {
        self.events.push(event);
    }

    pub fn record_reaction(&mut self, event_id: EventId, reactor: Entity) {
        self.reactors
            .entry(event_id)
            .or_insert_with(HashSet::new)
            .insert(reactor);
    }

    pub fn has_reacted(&self, event_id: &EventId, reactor: &Entity) -> bool {
        if let Some(reactors) = self.reactors.get(event_id) {
            return reactors.contains(reactor);
        }
        false
    }
}

pub type EventQueue = VecDeque<Event>;

#[derive(Clone)]
pub struct EventFilter(Arc<dyn Fn(&Event) -> bool + Send + Sync>);

impl EventFilter {
    pub fn new<F>(filter: F) -> Self
    where
        F: Fn(&Event) -> bool + Send + Sync + 'static,
    {
        Self(Arc::new(filter))
    }

    pub fn response_to_event_id(event_id: EventId) -> Self {
        Self::new(move |event| event.response_to == Some(event_id))
    }

    pub fn matches(&self, event: &Event) -> bool {
        (self.0)(event)
    }
}

#[derive(Clone)]
pub struct EventCallback(
    Arc<dyn Fn(&mut GameState, &Event, &ListenerSource) -> CallbackResult + Send + Sync + 'static>,
);

impl EventCallback {
    pub fn new<F>(callback: F) -> Self
    where
        F: Fn(&mut GameState, &Event, &ListenerSource) -> CallbackResult + Send + Sync + 'static,
    {
        Self(Arc::new(callback))
    }

    pub fn run(&self, game_state: &mut GameState, event: &Event, source: &ListenerSource) {
        let result = (self.0)(game_state, event, &source);
        match result {
            CallbackResult::Event(event) => {
                game_state.process_event(event);
            }
            CallbackResult::EventWithCallback(event, callback) => {
                game_state.process_event_with_response_callback(event, callback);
            }
            CallbackResult::None => {}
        }
    }
}

pub enum CallbackResult {
    Event(Event),
    EventWithCallback(Event, EventCallback),
    None,
}

pub type EventListenerId = Uuid;

// TODO: No idea what to call this
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ListenerSource {
    EffectInstance {
        id: EffectInstanceId,
        entity: Entity,
    },
    EventResponse {
        trigger_id: EventId,
    },
}

#[derive(Clone)]
pub struct EventListener {
    pub id: EventListenerId,
    pub source: ListenerSource,
    pub filter: EventFilter,
    pub callback: EventCallback,
    pub one_shot: bool,
}

impl EventListener {
    pub fn new(
        filter: EventFilter,
        callback: EventCallback,
        source: ListenerSource,
        one_shot: bool,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            source,
            filter,
            callback,
            one_shot,
        }
    }
}

pub struct EventDispatcher {
    listeners: HashMap<EventListenerId, EventListener>,
    by_source: HashMap<ListenerSource, HashSet<EventListenerId>>,
}

impl EventDispatcher {
    pub fn new() -> Self {
        Self {
            listeners: HashMap::new(),
            by_source: HashMap::new(),
        }
    }

    pub fn register_listener(&mut self, listener: EventListener) {
        let id = listener.id;
        let source = listener.source.clone();
        self.listeners.insert(id, listener);
        self.by_source.entry(source).or_default().insert(id);
    }

    pub fn get_listener(&self, listener_id: &EventListenerId) -> Option<&EventListener> {
        self.listeners.get(listener_id)
    }

    pub fn dispatch(&self, event: &Event) -> Vec<EventListenerId> {
        let mut listeners = Vec::new();
        for listener in self.listeners.values() {
            if listener.filter.matches(event) {
                listeners.push(listener.id);
            }
        }
        return listeners;
    }

    pub fn remove_listener_by_id(&mut self, listener_id: &EventListenerId) {
        if let Some(listener) = self.listeners.remove(listener_id) {
            if let Some(listeners) = self.by_source.get_mut(&listener.source) {
                listeners.remove(listener_id);
            }
        }
    }

    pub fn remove_listeners_by_source(&mut self, source: &ListenerSource) {
        if let Some(listeners) = self.by_source.remove(source) {
            for listener_id in listeners {
                self.listeners.remove(&listener_id);
            }
        }
    }
}
