use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::components::{
    ability::Ability,
    id::{BackgroundId, FeatId, IdProvider},
    level_up::ChoiceSpec,
    skill::Skill,
};

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Background {
    pub id: BackgroundId,
    // TODO: Not sure if we want to use this?
    pub ability_scores: [Ability; 3],
    pub feat: FeatId,
    pub skill_proficiencies: [Skill; 2],
    // TODO: Not sure what to do with these yet
    // tool_proficiencies
    pub equipment: ChoiceSpec,
}

impl IdProvider for Background {
    type Id = BackgroundId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}
