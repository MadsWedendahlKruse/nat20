use std::{
    collections::HashMap,
    str::FromStr,
    sync::{Arc, LazyLock},
};

use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{ReactionBodyFunction, ReactionTriggerFunction},
        id::ScriptId,
    },
    scripts::script_api::{
        ScriptEntity, ScriptEventView, ScriptReactionBodyContext, ScriptReactionTriggerContext,
    },
    systems,
};

static REACTION_TRIGGER_DEFAULTS: LazyLock<HashMap<String, Arc<ReactionTriggerFunction>>> =
    LazyLock::new(|| HashMap::new());

static REACTION_BODY_DEFAULTS: LazyLock<HashMap<String, Arc<ReactionBodyFunction>>> =
    LazyLock::new(|| HashMap::new());

#[derive(Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ReactionTrigger {
    pub raw: String,
    pub function: Arc<ReactionTriggerFunction>,
    pub script: Option<ScriptId>,
}

impl FromStr for ReactionTrigger {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(function) = REACTION_TRIGGER_DEFAULTS.get(s) {
            return Ok(ReactionTrigger {
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

        Ok(ReactionTrigger {
            raw: s.to_string(),
            function: Arc::new(move |_world, reactor, event| {
                let Some(script_event) = ScriptEventView::from_event(event) else {
                    return false;
                };
                let context = ScriptReactionTriggerContext {
                    reactor: ScriptEntity::from(*reactor),
                    event: script_event,
                };
                systems::scripts::evaluate_reaction_trigger(&script_id, &context)
            }),
            script,
        })
    }
}

impl TryFrom<String> for ReactionTrigger {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<ReactionTrigger> for String {
    fn from(trigger: ReactionTrigger) -> Self {
        trigger.raw
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
            function: Arc::new(move |game_state, reaction_data| {
                let result = systems::scripts::evaluate_reaction_body(
                    &script_id,
                    &ScriptReactionBodyContext::from(reaction_data),
                );
                systems::scripts::apply_reaction_body_result(game_state, reaction_data, result);
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
