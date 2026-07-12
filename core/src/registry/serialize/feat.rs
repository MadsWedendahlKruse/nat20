use std::sync::Arc;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        feat::{Feat, FeatPrerequisite},
        id::{EffectId, FeatId},
        level::CharacterLevels,
        level_up::LevelUpPrompt,
    },
    systems,
};

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum FeatPrerequisiteDefinition {
    MinimumLevel { minimum_level: u8 },
    HasFeat { feat: FeatId },
}

impl FeatPrerequisiteDefinition {
    pub fn to_function(&self) -> Arc<FeatPrerequisite> {
        match self {
            FeatPrerequisiteDefinition::MinimumLevel { minimum_level } => {
                let min_level = *minimum_level;
                Arc::new(move |world, entity| {
                    systems::helpers::get_component::<CharacterLevels>(world, entity).total_level()
                        >= min_level
                })
            }
            FeatPrerequisiteDefinition::HasFeat { feat } => {
                let feat_id = feat.clone();
                Arc::new(move |world, entity| {
                    systems::helpers::get_component::<Vec<FeatId>>(world, entity).contains(&feat_id)
                })
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FeatDefinition {
    pub id: FeatId,
    pub description: String,
    #[serde(default)]
    pub prerequisite: Option<FeatPrerequisiteDefinition>,
    #[serde(default)]
    pub effects: Vec<EffectId>,
    #[serde(default)]
    pub prompts: Vec<LevelUpPrompt>,
    #[serde(default)]
    pub repeatable: bool,
}

impl From<FeatDefinition> for Feat {
    fn from(value: FeatDefinition) -> Self {
        Feat::new(
            value.id,
            value.description,
            value.prerequisite.map(|p| p.to_function()),
            value.effects,
            value.prompts,
            value.repeatable,
        )
    }
}
