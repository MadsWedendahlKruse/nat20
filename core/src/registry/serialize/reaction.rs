use std::{
    collections::HashMap,
    str::FromStr,
    sync::{Arc, LazyLock},
};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use tracing::debug;

use crate::{
    components::{
        actions::action::{
            ReactionBodyFunction, ReactionResult, ReactionTrigger, ReactionTriggerFunction,
        },
        id::ScriptId,
        resource::{RESOURCE_ACTION, RESOURCE_BONUS_ACTION, RESOURCE_REACTION, ResourceAmountMap},
    },
    engine::{
        action_prompt::ActionData,
        event::{Event, EventKind, EventKindTag},
        game_state::GameState,
    },
    registry::serialize::schema::impl_string_schema,
    systems,
};

impl_string_schema!(
    ReactionTriggerScript,
    "ReactionTriggerScript",
    "description": "Script id of a Lua reaction trigger, e.g. `nat20_core::script.shield`.",
    "examples": ["nat20_core::script.shield"]
);

impl_string_schema!(
    ReactionBody,
    "ReactionBody",
    "description": "Script id of a Lua reaction body, e.g. `nat20_core::script.shield`, \
         or the built-in `cancel_event`.",
    "examples": ["nat20_core::script.shield", "cancel_event"]
);

static REACTION_TRIGGER_DEFAULTS: LazyLock<HashMap<String, Arc<ReactionTriggerFunction>>> =
    LazyLock::new(|| HashMap::new());

static REACTION_BODY_DEFAULTS: LazyLock<HashMap<String, Arc<ReactionBodyFunction>>> =
    LazyLock::new(|| {
        HashMap::from([(
            "cancel_event".to_string(),
            Arc::new(
                |game_state: &mut GameState, reaction_data: &ActionData, event: &mut Event| {
                    debug!("Cancelling event with ID {} due to reaction", event.id);
                    game_state
                        .session_for_entity_mut(reaction_data.actor.id())
                        .pending_events_mut()
                        .retain(|pending| pending.event.id != event.id);

                    // TODO: Bit of a hack to comply with Counterspell
                    let resources_refunded = if let EventKind::ActionRequested { action } =
                        &event.kind
                    {
                        debug!(
                            "Refunding non-standard resources for cancelled action: {:?}",
                            action
                        );
                        let mut resources_to_refund = action.resource_cost.clone();
                        for resource in [RESOURCE_ACTION, RESOURCE_BONUS_ACTION, RESOURCE_REACTION]
                        {
                            resources_to_refund.map.remove(&resource);
                        }
                        let _ = systems::resources::restore(
                            &mut game_state.world,
                            action.actor.id(),
                            &resources_to_refund,
                        );
                        resources_to_refund
                    } else {
                        ResourceAmountMap::new()
                    };

                    Some(ReactionResult::CancelEvent {
                        event: event.clone().into(),
                        resources_refunded,
                    })
                },
            ) as Arc<ReactionBodyFunction>,
        )])
    });

#[derive(Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ReactionTriggerScript {
    pub raw: String,
    pub function: Arc<ReactionTriggerFunction>,
    pub script: Option<ScriptId>,
}

impl FromStr for ReactionTriggerScript {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(function) = REACTION_TRIGGER_DEFAULTS.get(s) {
            return Ok(ReactionTriggerScript {
                raw: s.to_string(),
                function: function.clone(),
                script: None,
            });
        }

        let script_id: ScriptId = s.parse().map_err(|_| {
            format!(
                "Failed to parse reaction trigger: '{}'. Not found in defaults and not a valid script ID.",
                s
            )
        })?;
        let script = Some(script_id.clone());

        Ok(ReactionTriggerScript {
            raw: s.to_string(),
            function: Arc::new(move |game_state, reactor, event| {
                systems::scripts::evaluate_reaction_trigger(&script_id, game_state, *reactor, event)
            }),
            script,
        })
    }
}

impl TryFrom<String> for ReactionTriggerScript {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<ReactionTriggerScript> for String {
    fn from(trigger: ReactionTriggerScript) -> Self {
        trigger.raw
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
pub struct ReactionTriggerDefinition {
    pub script: ReactionTriggerScript,
    /// Event kinds that can trigger this reaction, checked before the
    /// trigger script runs.
    pub events: Vec<EventKindTag>,
}

impl From<ReactionTriggerDefinition> for ReactionTrigger {
    fn from(def: ReactionTriggerDefinition) -> Self {
        ReactionTrigger {
            events: def.events,
            function: def.script.function,
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ReactionBody {
    pub raw: String,
    pub function: Arc<ReactionBodyFunction>,
    pub script: Option<ScriptId>,
}

impl FromStr for ReactionBody {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(function) = REACTION_BODY_DEFAULTS.get(s) {
            return Ok(ReactionBody {
                raw: s.to_string(),
                function: function.clone(),
                script: None,
            });
        }

        let script_id: ScriptId = s.parse().map_err(|_| {
            format!(
                "Failed to parse reaction body: '{}'. Not a valid script ID.",
                s
            )
        })?;
        let script = Some(script_id.clone());

        Ok(ReactionBody {
            raw: s.to_string(),
            function: Arc::new(move |game_state, reaction_data, event| {
                systems::scripts::evaluate_reaction_body(
                    &script_id,
                    game_state,
                    reaction_data,
                    event,
                );
                None
            }),
            script,
        })
    }
}

impl TryFrom<String> for ReactionBody {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<ReactionBody> for String {
    fn from(body: ReactionBody) -> Self {
        body.raw
    }
}
