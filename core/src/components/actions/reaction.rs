use std::sync::Arc;

use hecs::Entity;
use tracing::{debug, warn};

use crate::{
    components::resource::ResourceAmountMap,
    engine::{
        action_prompt::ActionData,
        event::{Event, EventKindTag},
        engine_state::EngineState,
    },
};

pub type ReactionTriggerFunction = dyn Fn(&EngineState, &Entity, &Event) -> bool + Send + Sync;
pub type ReactionBodyFunction =
    dyn Fn(&mut EngineState, &ActionData, &mut Event) -> Option<ReactionResult> + Send + Sync;

#[derive(Clone)]
pub struct ReactionTrigger {
    /// Event kinds that can trigger this reaction. Should be checked before the
    /// trigger function (typically Lua) runs.
    pub events: Vec<EventKindTag>,
    pub function: Arc<ReactionTriggerFunction>,
}

#[derive(Clone)]
pub struct ReactionBody {
    function: Arc<ReactionBodyFunction>,
}

impl ReactionBody {
    pub fn new(function: Arc<ReactionBodyFunction>) -> Self {
        Self { function }
    }

    pub fn execute(&self, engine_state: &mut EngineState, action: &ActionData) -> ReactionResult {
        let Some(trigger_event) = action.trigger_event.as_ref() else {
            panic!(
                "Attempted to execute a reaction without a trigger event: {:#?}",
                action
            );
        };

        // Take out the pending event to prevent double mutable borrow
        let scope = engine_state.scope_for_entity_mut(action.actor.id());
        let Some(mut pending) = scope.pending_events_mut().pop_front() else {
            panic!("No pending events found for action: {:#?}", action);
        };

        if pending.event.id != trigger_event.id {
            panic!(
                "Front pending event does not match trigger event for action: {:#?}",
                action
            );
        }

        let result = (self.function)(engine_state, action, &mut pending.event);

        let result = result.unwrap_or_else(|| {
            // TODO: Not sure if this check actually works
            if **trigger_event != pending.event {
                ReactionResult::ModifyEvent {
                    before: trigger_event.as_ref().clone(),
                    after: pending.event.clone(),
                }
            } else {
                ReactionResult::NoEffect // TEMP
            }
        });

        if let ReactionResult::CancelEvent { event, .. } = &result {
            if event.id == pending.event.id {
                debug!(
                    "Reaction cancelled event {:?}, removing from pending events",
                    event.id
                );
                pending.canceled = true;
            } else {
                // TODO: Not sure if this ever actually happens?
                warn!(
                    "Reaction cancelled event {:?}, but pending event is {:?}, not removing from pending events",
                    event, pending.event
                );
            }
        }

        // Put the event back in
        let scope = engine_state.scope_for_entity_mut(action.actor.id());
        scope.queue_pending_event(pending, true);

        result
    }
}

#[derive(Debug, Clone)]
pub enum ReactionResult {
    ModifyEvent {
        before: Event,
        after: Event,
    },
    CancelEvent {
        event: Box<Event>,
        resources_refunded: ResourceAmountMap,
    },
    NoEffect,
}

impl PartialEq for ReactionResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                ReactionResult::ModifyEvent {
                    before: b1,
                    after: a1,
                },
                ReactionResult::ModifyEvent {
                    before: b2,
                    after: a2,
                },
            ) => b1.id == b2.id && a1.id == a2.id,
            (
                ReactionResult::CancelEvent { event: e1, .. },
                ReactionResult::CancelEvent { event: e2, .. },
            ) => e1.id == e2.id,
            (ReactionResult::NoEffect, ReactionResult::NoEffect) => true,
            _ => false,
        }
    }
}
