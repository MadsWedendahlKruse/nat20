use std::{fmt, hash::Hash};

use crate::{
    components::{
        ability::Ability,
        d20::{D20CheckDC, D20CheckMap},
        effects::hooks::D20CheckHooks,
    },
    systems,
};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(EnumIter, Debug, Hash, Eq, PartialEq, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Skill {
    // --- Strength ---
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

impl fmt::Display for Skill {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Default for Skill {
    fn default() -> Self {
        Skill::Athletics
    }
}

#[macro_export]
macro_rules! skill_ability_map {
    ( $( $skill:ident => $ability:ident ),* $(,)? ) => {
        pub const fn skill_ability(skill: &Skill) -> Option<Ability> {
            match skill {
                $( Skill::$skill => Some(Ability::$ability) ),*
            }
        }
    };
}

skill_ability_map! {
    Athletics => Strength,

    Acrobatics => Dexterity,
    SleightOfHand => Dexterity,
    Stealth => Dexterity,

    Arcana => Intelligence,
    History => Intelligence,
    Investigation => Intelligence,
    Nature => Intelligence,
    Religion => Intelligence,

    AnimalHandling => Wisdom,
    Insight => Wisdom,
    Medicine => Wisdom,
    Perception => Wisdom,
    Survival => Wisdom,

    Deception => Charisma,
    Intimidation => Charisma,
    Performance => Charisma,
    Persuasion => Charisma,

    Initiative => Dexterity,
}

pub type SkillSet = D20CheckMap<Skill>;

pub type SkillCheckDC = D20CheckDC<Skill>;

pub fn get_skill_hooks(skill: &Skill, world: &World, entity: Entity) -> Vec<D20CheckHooks> {
    systems::effects::effects(world, entity)
        .values()
        .filter_map(|e| e.effect().on_skill_check.get(&skill))
        .cloned()
        .collect()
}

impl Default for SkillSet {
    fn default() -> Self {
        SkillSet::new(skill_ability, get_skill_hooks)
    }
}
