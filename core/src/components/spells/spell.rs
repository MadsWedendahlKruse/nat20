use std::{hash::Hash, sync::Arc};

use hecs::Entity;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::Display;

use crate::{
    components::{
        ability::Ability,
        actions::{
            action::{
                Action, ActionKind, ActionTimeline, ActionUsabilityFunction, TargetingFunction,
            },
            reaction::ReactionTrigger,
        },
        effects::effect::EffectInstanceId,
        id::{EffectId, IdProvider, SpellId},
        modifier::ModifierSource,
        resource::ResourceAmountMap,
    },
    engine::{action_prompt::ActionExecutionInstanceId, game_state::GameState},
    registry::serialize::spell::SpellDefinition,
    systems,
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
        resource_cost: ResourceAmountMap,
        targeting: Arc<TargetingFunction>,
        reaction_trigger: Option<ReactionTrigger>,
        timeline: ActionTimeline,
        usability: Option<Arc<ActionUsabilityFunction>>,
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
                reaction_trigger,
                timeline,
                usability,
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

pub const SPELL_CASTING_ABILITIES: &[Ability; 3] =
    &[Ability::Intelligence, Ability::Wisdom, Ability::Charisma];

pub const CONCENTRATION_SAVING_THROW_DC_DEFAULT: i32 = 10;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ConcentrationInstance {
    Effect {
        entity: Entity,
        effect: EffectId,
        instance: EffectInstanceId,
    },
    // TODO: Environmental effects (e.g. web)
}

impl ConcentrationInstance {
    pub fn break_concentration(&self, game_state: &mut GameState) {
        match self {
            ConcentrationInstance::Effect {
                entity, instance, ..
            } => {
                systems::effects::remove_effect(game_state, *entity, instance);
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConcentrationTracker {
    instances: Vec<ConcentrationInstance>,
    action_instance: Option<ActionExecutionInstanceId>,
    concentration_blockers: Vec<ModifierSource>,
}

impl ConcentrationTracker {
    pub fn instances(&self) -> &Vec<ConcentrationInstance> {
        &self.instances
    }

    pub fn add_instance(
        &mut self,
        instance: ConcentrationInstance,
        action_instance: &ActionExecutionInstanceId,
    ) -> Result<(), ConcentrationError> {
        self.can_concentrate()?;
        self.instances.push(instance);
        self.action_instance = Some(action_instance.clone());
        Ok(())
    }

    /// In most cases we would want to remove all instances at once, but if e.g.
    /// a spell applies effects to multiple targets, and one of the targets dies,
    /// then we need to be able to remove just that one instance.
    pub fn remove_instances_by_entity(&mut self, entity: Entity) {
        self.instances.retain(|instance| match instance {
            ConcentrationInstance::Effect { entity: e, .. } => *e != entity,
        });
        if self.instances.is_empty() {
            self.action_instance = None;
        }
    }

    pub fn is_concentrating(&self) -> bool {
        !self.instances.is_empty()
    }

    pub fn take_instances(&mut self) -> Vec<ConcentrationInstance> {
        self.action_instance = None;
        std::mem::take(&mut self.instances)
    }

    pub fn action_instance(&self) -> Option<&ActionExecutionInstanceId> {
        self.action_instance.as_ref()
    }

    pub fn can_concentrate(&self) -> Result<(), ConcentrationError> {
        if self.concentration_blockers.is_empty() {
            Ok(())
        } else {
            Err(ConcentrationError::ConcentrationBlocked(
                self.concentration_blockers.clone(),
            ))
        }
    }

    pub fn concentration_blockers(&self) -> &Vec<ModifierSource> {
        &self.concentration_blockers
    }

    pub fn block_concentration(&mut self, source: ModifierSource) {
        self.concentration_blockers.push(source);
    }

    pub fn unblock_concentration(&mut self, source: &ModifierSource) {
        self.concentration_blockers.retain(|s| s != source);
    }
}

impl Default for ConcentrationTracker {
    fn default() -> Self {
        Self {
            instances: Vec::new(),
            action_instance: None,
            concentration_blockers: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConcentrationError {
    ConcentrationBlocked(Vec<ModifierSource>),
}
