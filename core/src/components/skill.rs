use std::{fmt, hash::Hash};

use crate::components::{
    ability::Ability,
    d20::{D20CheckKind, D20CheckMap},
};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(EnumIter, Debug, Hash, Eq, PartialEq, Clone, Copy, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
#[derive(Default)]
pub enum Skill {
    // --- Strength ---
    #[default]
    Athletics,
    // --- Dexterity ---
    Acrobatics,
    SleightOfHand,
    Stealth,
    // Not technically a skill, but it behaves like one
    Initiative,
    // --- Intelligence ---
    Arcana,
    History,
    Investigation,
    Nature,
    Religion,
    // --- Wisdom ---
    AnimalHandling,
    Insight,
    Medicine,
    Perception,
    Survival,
    // --- Charisma ---
    Deception,
    Intimidation,
    Performance,
    Persuasion,
}

impl Skill {
    pub fn ability(&self) -> Ability {
        match self {
            Skill::Athletics => Ability::Strength,

            Skill::Acrobatics | Skill::SleightOfHand | Skill::Stealth | Skill::Initiative => {
                Ability::Dexterity
            }

            Skill::Arcana
            | Skill::History
            | Skill::Investigation
            | Skill::Nature
            | Skill::Religion => Ability::Intelligence,

            Skill::AnimalHandling
            | Skill::Insight
            | Skill::Medicine
            | Skill::Perception
            | Skill::Survival => Ability::Wisdom,

            Skill::Deception | Skill::Intimidation | Skill::Performance | Skill::Persuasion => {
                Ability::Charisma
            }
        }
    }
}

impl fmt::Display for Skill {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}


pub type SkillSet = D20CheckMap<Skill>;

impl Default for SkillSet {
    fn default() -> Self {
        SkillSet::new(|skill| D20CheckKind::Skill(*skill))
    }
}
