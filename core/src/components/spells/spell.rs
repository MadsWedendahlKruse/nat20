use std::{hash::Hash, sync::Arc};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::Display;

use crate::{
    components::{
        actions::{
            action::{
                Action, ActionContext, ActionKind, ActionTimeline, ActionUsabilityFunction,
                TargetUsabilityFunction, TargetingFunction,
            },
            reaction::ReactionTrigger,
        },
        id::{IdProvider, SpellId},
        resource::ResourceAmountMap,
    },
    registry::serialize::spell::SpellDefinition,
};

#[derive(Debug, Clone, Copy, Display, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum MagicSchool {
    Abjuration,
    Conjuration,
    Divination,
    Enchantment,
    Evocation,
    Illusion,
    Necromancy,
    Transmutation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SpellFlag {
    Concentration,
    Verbal,
    Somatic,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(from = "SpellDefinition")]
pub struct Spell {
    id: SpellId,
    base_level: u8,
    school: MagicSchool,
    flags: Vec<SpellFlag>,
    action: Action,
}

impl Spell {
    pub fn new(
        id: SpellId,
        description: String,
        base_level: u8,
        school: MagicSchool,
        flags: Vec<SpellFlag>,
        kind: ActionKind,
        contexts: Vec<ActionContext>,
        resource_cost: ResourceAmountMap,
        targeting: Arc<TargetingFunction>,
        reaction_trigger: Option<ReactionTrigger>,
        timeline: ActionTimeline,
        usability: Option<Arc<ActionUsabilityFunction>>,
        target_usability: Option<Arc<TargetUsabilityFunction>>,
    ) -> Self {
        let action_id = id.clone().into();

        Self {
            id,
            school,
            base_level,
            flags,
            action: Action {
                id: action_id,
                description,
                kind,
                resource_cost,
                targeting,
                cooldown: None,
                contexts,
                reaction_trigger,
                timeline,
                usability,
                target_usability,
            },
        }
    }

    pub fn id(&self) -> &SpellId {
        &self.id
    }

    pub fn base_level(&self) -> u8 {
        self.base_level
    }

    pub fn is_cantrip(&self) -> bool {
        self.base_level() == 0
    }

    pub fn school(&self) -> MagicSchool {
        self.school
    }

    pub fn action(&self) -> &Action {
        &self.action
    }

    pub fn flags(&self) -> &Vec<SpellFlag> {
        &self.flags
    }

    pub fn has_flag(&self, flag: SpellFlag) -> bool {
        self.flags.contains(&flag)
    }
}

impl IdProvider for Spell {
    type Id = SpellId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}
