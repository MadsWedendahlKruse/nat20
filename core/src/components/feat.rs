use std::sync::Arc;

use hecs::{Entity, World};
use serde::Deserialize;

use crate::{
    components::{
        id::{EffectId, FeatId, IdProvider},
        level_up::LevelUpPrompt,
    },
    registry::serialize::feat::FeatDefinition,
};

pub type FeatPrerequisite = dyn Fn(&World, Entity) -> bool + Send + Sync;

#[derive(Clone, Deserialize)]
#[serde(from = "FeatDefinition")]
pub struct Feat {
    id: FeatId,
    description: String,
    prerequisite: Option<Arc<FeatPrerequisite>>,
    effects: Vec<EffectId>,
    /// Some feats might require a choice to be made when selected.
    /// In most cases this will be some kind of ability score increase, but could
    /// also be a choice between learning a new spell etc.
    // TODO: Is it ever more than one?
    prompts: Vec<LevelUpPrompt>,
    /// Most feats are single-use, but some can be taken multiple times.
    /// This mostly applies to Ability Score Improvement.
    repeatable: bool,
}

impl Feat {
    pub fn new(
        id: FeatId,
        description: String,
        prerequisite: Option<Arc<FeatPrerequisite>>,
        effects: Vec<EffectId>,
        prompts: Vec<LevelUpPrompt>,
        repeatable: bool,
    ) -> Self {
        Self {
            id,
            description,
            prerequisite,
            effects,
            prompts,
            repeatable,
        }
    }

    pub fn id(&self) -> &FeatId {
        &self.id
    }

    pub fn meets_prerequisite(&self, world: &World, entity: Entity) -> bool {
        if let Some(prerequisite) = &self.prerequisite {
            prerequisite(world, entity)
        } else {
            true
        }
    }

    pub fn effects(&self) -> &Vec<EffectId> {
        &self.effects
    }

    pub fn prompts(&self) -> &[LevelUpPrompt] {
        &self.prompts
    }

    pub fn is_repeatable(&self) -> bool {
        self.repeatable
    }

    pub fn description(&self) -> &str {
        &self.description
    }
}

impl IdProvider for Feat {
    type Id = FeatId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}
