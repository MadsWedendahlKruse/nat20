use std::sync::Arc;

use hecs::Entity;
use tracing::{debug, warn};

use crate::{
    components::resource::ResourceAmountMap,
    engine::{
        action_prompt::ActionData,
        event::{Event, EventKindTag},
        game_state::GameState,
    },
};

pub type ReactionTriggerFunction = dyn Fn(&GameState, &Entity, &Event) -> bool + Send + Sync;
pub type ReactionBodyFunction =
    dyn Fn(&mut GameState, &ActionData, &mut Event) -> Option<ReactionResult> + Send + Sync;

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

    pub fn execute(&self, game_state: &mut GameState, action: &ActionData) -> ReactionResult {
        let Some(trigger_event) = action.trigger_event.as_ref() else {
            panic!(
                "Attempted to execute a reaction without a trigger event: {:#?}",
                action
            );
        };

        let scope = game_state.scope_for_entity(action.actor.id());
        let Some((mut pending, index)) = game_state
            .interaction_engine
            .session_mut(scope)
            .take_pending_event(&trigger_event.id)
        else {
            panic!(
                "Attempted to execute reaction to event which is not pending: {:#?}",
                action
            )
        };

        let result = (self.function)(game_state, action, &mut pending.event);

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

        let mut should_reinsert = true;

        if let ReactionResult::CancelEvent { event, .. } = &result {
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

        let session = game_state.interaction_engine.session_mut(scope);

        if should_reinsert {
            session.pending_events_mut().insert(index, pending);
            session.clear_blocker(action.actor.id());
        }

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
