use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::Arc,
};

use hecs::Entity;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::EnumDiscriminants;
use uuid::Uuid;

use crate::{
    components::{
        actions::action::{ActionResult, ActionResultComponent},
        damage::DamageRollResult,
        effects::effect::EffectInstanceId,
        health::life_state::LifeState,
        id::{EntityIdentifier, ItemId},
        spells::spell::ConcentrationInstance,
        time::TurnBoundary,
    },
    engine::{
        action_prompt::{ActionData, ActionExecutionInstanceId},
        encounter::EncounterId,
        game_state::GameState,
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
    pub parent: Option<EventId>,
    pub children: Vec<EventId>,
}

impl Event {
    pub fn new(kind: EventKind) -> Self {
        Self {
            id: Uuid::new_v4(),
            kind,
            response_to: None,
            parent: None,
            children: Vec::new(),
        }
    }

    pub fn as_response_to(mut self, event_id: EventId) -> Self {
        self.response_to = Some(event_id);
        self
    }

    pub fn with_parent(mut self, parent_id: Option<&EventId>) -> Self {
        self.parent = parent_id.copied();
        self
    }

    pub fn encounter_event(encounter_event: EncounterEvent) -> Self {
        Self::new(EventKind::Encounter(encounter_event))
    }

    pub fn actor(&self) -> Option<Entity> {
        match &self.kind {
            EventKind::ActionRequested { action } => Some(action.actor.id()),
            EventKind::ActionResult { actor, result, .. } => {
                if let Some(actor) = actor {
                    Some(actor.id())
                } else {
                    Some(result.target.id())
                }
            }
            EventKind::LifeStateChanged { entity, actor, .. } => {
                if let Some(actor) = actor {
                    Some(actor.id())
                } else {
                    Some(entity.id())
                }
            }
            EventKind::D20CheckPerformed { actor, .. } => Some(actor.id()),
            EventKind::D20CheckResolved { actor, .. } => Some(actor.id()),
            EventKind::DamageRollPerformed { actor, .. } => Some(actor.id()),
            EventKind::DamageRollResolved { actor, .. } => Some(actor.id()),
            EventKind::Encounter(_) => None,
            EventKind::TurnBoundary { entity, .. } => Some(entity.id()),
            // TODO: Same problem as ReactionTriggered
            EventKind::RestStarted { participants, .. } => Some(participants.first()?.id()),
            EventKind::RestFinished { participants, .. } => Some(participants.first()?.id()),
            EventKind::LostConcentration { entity, .. } => Some(entity.id()),
            EventKind::MovingOutOfReach { mover, .. } => Some(mover.id()),
            EventKind::EquipmentChanged { entity, .. } => Some(entity.id()),
        }
    }

    pub fn action_result_event(target: EntityIdentifier, result: ActionResultComponent) -> Event {
        Event::new(EventKind::ActionResult {
            result: ActionResult {
                target,
                components: vec![result],
            },
            actor: None,
        })
    }
}

#[derive(Debug, Clone, PartialEq, EnumDiscriminants)]
#[strum_discriminants(
    name(EventKindTag),
    derive(Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema),
    serde(rename_all = "snake_case")
)]
pub enum EventKind {
    /// An entity has declared they want to take an action. The engine can then
    /// validate that the entity can perform the action and either approve or
    /// deny it. Other entities might also react to the request, e.g. if someone
    /// is casting a spell, another entity might use their reaction to Counterspell
    /// the action.
    ActionRequested {
        action: ActionData,
    },
    /// The result of performing an action.
    /// Despite the name it can also be used outside of actions.
    /// TODO: Maybe a better name then?
    ActionResult {
        result: ActionResult,
        actor: Option<EntityIdentifier>,
    },
    Encounter(EncounterEvent),

    LifeStateChanged {
        entity: EntityIdentifier,
        new_state: LifeState,
        /// The entity that caused the change, if any
        actor: Option<EntityIdentifier>,
    },
    /// The initial D20 roll which can be reacted to, e.g. with the Lucky feat.
    D20CheckPerformed {
        actor: EntityIdentifier,
        result: D20ResultKind,
        dc: D20CheckDCKind,
    },
    /// The final result of a D20 check after reactions have been applied.
    D20CheckResolved {
        actor: EntityIdentifier,
        result: D20ResultKind,
        dc: D20CheckDCKind,
    },
    DamageRollPerformed {
        actor: EntityIdentifier,
        result: DamageRollResult,
    },
    DamageRollResolved {
        actor: EntityIdentifier,
        result: DamageRollResult,
    },

    TurnBoundary {
        entity: EntityIdentifier,
        boundary: TurnBoundary,
    },

    RestStarted {
        kind: RestKind,
        participants: Vec<EntityIdentifier>,
    },
    RestFinished {
        kind: RestKind,
        participants: Vec<EntityIdentifier>,
    },

    LostConcentration {
        entity: EntityIdentifier,
        instances: Vec<ConcentrationInstance>,
    },

    MovingOutOfReach {
        mover: EntityIdentifier,
        entity: EntityIdentifier,
    },

    EquipmentChanged {
        entity: EntityIdentifier,
        item: ItemId,
        equipped: bool,
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
    pub indices: HashMap<EventId, usize>,
    /// Track which entities have reacted to which events in this log. This is
    /// used to prevent an entity from reacting to the same event multiple times.
    /// TODO: Not sure if this is the best solution.
    pub reactors: HashMap<EventId, HashSet<Entity>>,
    pub action_events: HashMap<ActionExecutionInstanceId, EventId>,
}

impl EventLog {
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
            indices: HashMap::new(),
            reactors: HashMap::new(),
            action_events: HashMap::new(),
        }
    }

    pub fn push(&mut self, event: Event) {
        match &event.kind {
            // EventKind::ActionRequested { action } | EventKind::ActionPerformed { action, .. } => {
            EventKind::ActionRequested { action } => {
                self.action_events.insert(action.instance_id, event.id);
            }
            _ => {}
        }
        if let Some(parent_id) = event.parent
            && let Some(parent_index) = self.indices.get(&parent_id)
            && let Some(parent_event) = self.events.get_mut(*parent_index)
        {
            parent_event.children.push(event.id);
        }
        self.indices.insert(event.id, self.events.len());
        self.events.push(event);
    }

    pub fn get(&self, event_id: &EventId) -> Option<&Event> {
        if let Some(index) = self.indices.get(event_id) {
            return self.events.get(*index);
        }
        None
    }

    pub fn filter_events(&self, filter: &EventFilter) -> Vec<&Event> {
        self.events.iter().filter(|e| filter.matches(e)).collect()
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

    pub fn triggered_reactions(&self, event_id: &EventId) -> bool {
        self.reactors
            .get(event_id)
            .map_or(false, |reactors| !reactors.is_empty())
    }

    pub fn child_events(&self, event_id: &EventId) -> Vec<&Event> {
        if let Some(index) = self.indices.get(event_id)
            && let Some(event) = self.events.get(*index)
        {
            return event
                .children
                .iter()
                .filter_map(|child_id| {
                    if let Some(child_index) = self.indices.get(child_id) {
                        return self.events.get(*child_index);
                    }
                    None
                })
                .collect();
        }
        Vec::new()
    }

    pub fn action_event_id(
        &self,
        action_instance_id: &ActionExecutionInstanceId,
    ) -> Option<&EventId> {
        self.action_events.get(action_instance_id)
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
    Other,
}

#[derive(Clone)]
pub struct EventListener {
    pub id: EventListenerId,
    pub source: ListenerSource,
    /// Event kinds the filter can match.
    /// None means the filter can match any event kind
    pub kinds: Option<Vec<EventKindTag>>,
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
            kinds: None,
            filter,
            callback,
            one_shot,
        }
    }

    pub fn with_kinds(mut self, kinds: Vec<EventKindTag>) -> Self {
        self.kinds = Some(kinds);
        self
    }
}

pub struct EventDispatcher {
    listeners: HashMap<EventListenerId, EventListener>,
    by_source: HashMap<ListenerSource, HashSet<EventListenerId>>,
    /// Listeners with declared kinds, keyed by event kind
    by_kind: HashMap<EventKindTag, HashSet<EventListenerId>>,
    /// One-shot response callbacks, keyed by the event they respond to
    by_response_to: HashMap<EventId, HashSet<EventListenerId>>,
    /// Checked for every event
    wildcard: HashSet<EventListenerId>,
}

impl EventDispatcher {
    pub fn new() -> Self {
        Self {
            listeners: HashMap::new(),
            by_source: HashMap::new(),
            by_kind: HashMap::new(),
            by_response_to: HashMap::new(),
            wildcard: HashSet::new(),
        }
    }

    pub fn register_listener(&mut self, listener: EventListener) {
        let id = listener.id;
        self.by_source
            .entry(listener.source.clone())
            .or_default()
            .insert(id);

        match (&listener.source, &listener.kinds) {
            (ListenerSource::EventResponse { trigger_id }, _) => {
                self.by_response_to
                    .entry(*trigger_id)
                    .or_default()
                    .insert(id);
            }

            (_, Some(kinds)) => {
                for kind in kinds {
                    self.by_kind.entry(*kind).or_default().insert(id);
                }
            }

            (_, None) => {
                self.wildcard.insert(id);
            }
        }

        self.listeners.insert(id, listener);
    }

    pub fn get_listener(&self, listener_id: &EventListenerId) -> Option<&EventListener> {
        self.listeners.get(listener_id)
    }

    pub fn dispatch(&self, event: &Event) -> Vec<EventListenerId> {
        let mut listeners: Vec<EventListenerId> = Vec::new();

        listeners.extend(&self.wildcard);

        if let Some(kind_listeners) = self.by_kind.get(&EventKindTag::from(&event.kind)) {
            listeners.extend(kind_listeners);
        }

        if let Some(response_to) = event.response_to
            && let Some(response_listeners) = self.by_response_to.get(&response_to)
        {
            listeners.extend(response_listeners);
        }

        // The indexes only narrow down the candidates, the filters still
        // decide whether a listener actually matches
        listeners.retain(|id| {
            self.listeners
                .get(id)
                .map_or(false, |listener| listener.filter.matches(event))
        });

        listeners
    }

    pub fn remove_listener_by_id(&mut self, listener_id: &EventListenerId) {
        let Some(listener) = self.listeners.remove(listener_id) else {
            return;
        };

        if let Some(listeners) = self.by_source.get_mut(&listener.source) {
            listeners.remove(listener_id);
        }

        match (&listener.source, &listener.kinds) {
            (ListenerSource::EventResponse { trigger_id }, _) => {
                if let Some(listeners) = self.by_response_to.get_mut(trigger_id) {
                    listeners.remove(listener_id);
                }
            }

            (_, Some(kinds)) => {
                for kind in kinds {
                    if let Some(listeners) = self.by_kind.get_mut(kind) {
                        listeners.remove(listener_id);
                    }
                }
            }

            (_, None) => {
                self.wildcard.remove(listener_id);
            }
        }
    }

    pub fn remove_listeners_by_source(&mut self, source: &ListenerSource) {
        if let Some(listener_ids) = self.by_source.remove(source) {
            for listener_id in listener_ids {
                self.remove_listener_by_id(&listener_id);
            }
        }
    }
}
