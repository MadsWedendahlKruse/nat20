use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{ActionContext, ActionTimeline},
        id::SpellId,
        resource::ResourceAmountMap,
        spells::spell::{MagicSchool, Spell, SpellFlag},
    },
    registry::serialize::{
        action::{ActionKindDefinition, ActionUsabilityDefinition, TargetUsabilityDefinition},
        reaction::ReactionTriggerDefinition,
        targeting::TargetingDefinition,
    },
};

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
pub struct SpellDefinition {
    pub id: SpellId,
    pub description: String,
    pub base_level: u8,
    pub school: MagicSchool,
    #[serde(default)]
    pub flags: Vec<SpellFlag>,
    pub kind: ActionKindDefinition,
    /// For spells the context is usually determined by the spell levels available
    /// to the caster, in which case this should be left empty. For some spells the
    /// context might be fixed, e.g the spell can only be cast a specific level,
    /// which can then be specified here
    #[serde(default)]
    pub contexts: Vec<ActionContext>,
    pub resource_cost: ResourceAmountMap,
    pub targeting: TargetingDefinition,
    #[serde(default)]
    pub reaction_trigger: Option<ReactionTriggerDefinition>,
    pub timeline: ActionTimeline,
    pub usability: Option<ActionUsabilityDefinition>,
    #[serde(default)]
    pub target_usability: Option<TargetUsabilityDefinition>,
}

impl From<SpellDefinition> for Spell {
    fn from(value: SpellDefinition) -> Self {
        Spell::new(
            value.id,
            value.description,
            value.base_level,
            value.school,
            value.flags,
            value.kind.into(),
            value.contexts,
            value.resource_cost,
            value.targeting.function(),
            value.reaction_trigger.map(Into::into),
            value.timeline,
            value.usability.map(|usability| usability.function()),
            value
                .target_usability
                .map(|target_usability| target_usability.function()),
        )
    }
}
