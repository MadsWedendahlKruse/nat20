use std::{
    collections::{HashMap, hash_map::Entry},
    fmt,
    str::FromStr,
};

use serde::{Deserialize, Serialize};

use crate::{
    components::id::{ClassId, SubclassId},
    registry::{
        registry::{self, ClassesRegistry},
        serialize::schema::impl_string_schema,
    },
};

pub trait Level {
    fn total_level(&self) -> u8;

    fn proficiency_bonus(&self) -> u8 {
        let total_level = self.total_level();
        if total_level < 4 {
            return 2;
        }
        // Proficiency bonus is typically calculated as (total level - 1) / 4 + 2
        // This is a common rule in many RPG systems, including D&D 5e
        (total_level - 1) / 4 + 2
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum ChallengeRating {
    Eighth,
    Quarter,
    Half,
    Full(u8),
}

impl FromStr for ChallengeRating {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1/8" => Ok(ChallengeRating::Eighth),
            "1/4" => Ok(ChallengeRating::Quarter),
            "1/2" => Ok(ChallengeRating::Half),
            _ => {
                if let Ok(value) = s.parse::<u8>() {
                    Ok(ChallengeRating::Full(value))
                } else {
                    Err(format!("Invalid challenge rating: {}", s))
                }
            }
        }
    }
}

impl TryFrom<String> for ChallengeRating {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::from_str(&value)
    }
}

impl fmt::Display for ChallengeRating {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChallengeRating::Eighth => write!(f, "1/8"),
            ChallengeRating::Quarter => write!(f, "1/4"),
            ChallengeRating::Half => write!(f, "1/2"),
            ChallengeRating::Full(value) => write!(f, "{}", value),
        }
    }
}

impl From<ChallengeRating> for String {
    fn from(value: ChallengeRating) -> Self {
        value.to_string()
    }
}

impl Level for ChallengeRating {
    fn total_level(&self) -> u8 {
        match self {
            ChallengeRating::Eighth => 0,
            ChallengeRating::Quarter => 0,
            ChallengeRating::Half => 0,
            ChallengeRating::Full(value) => *value,
        }
    }
}

impl_string_schema!(
    ChallengeRating,
    "ChallengeRating",
    "description": "The challenge rating of a creature, representing its relative difficulty.",
    "example": ["1/8", "1/4", "1/2", "1", "2", "3"],
);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassLevelProgression {
    level: u8,
    subclass: Option<SubclassId>,
}

impl ClassLevelProgression {
    pub fn new(level: u8, subclass: Option<SubclassId>) -> Self {
        if level == 0 {
            panic!("Class level cannot be zero");
        }
        Self { level, subclass }
    }

    pub fn level(&self) -> u8 {
        self.level
    }

    pub fn subclass(&self) -> Option<&SubclassId> {
        self.subclass.as_ref()
    }
}

impl Default for ClassLevelProgression {
    fn default() -> Self {
        Self::new(1, None)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CharacterLevels {
    class_levels: HashMap<ClassId, ClassLevelProgression>,
    /// The class that was first leveled up. Occasionally this is relevant, e.g
    /// when calculating the HP of the character
    first_class: Option<ClassId>,
    /// The latest class that was leveled up. Right now it's only used to have a
    /// default class for level up decisions
    latest_class: Option<ClassId>,
    experience: u32,
}

impl Default for CharacterLevels {
    fn default() -> Self {
        Self::new()
    }
}

impl CharacterLevels {
    pub fn new() -> Self {
        Self {
            class_levels: HashMap::new(),
            first_class: None,
            latest_class: None,
            experience: 0,
        }
    }

    pub fn total_level(&self) -> u8 {
        self.class_levels.values().map(|p| p.level()).sum()
    }

    pub fn experience(&self) -> u32 {
        self.experience
    }

    pub fn experience_for_next_level(&self) -> Option<u32> {
        let next_level = self.total_level() + 1;
        if next_level > registry::rules().max_level() {
            return None;
        }
        registry::rules()
            .experience_per_level
            .get(&next_level)
            .cloned()
            .or(None)
    }

    pub fn add_experience(&mut self, amount: u32) {
        self.experience = self.experience.saturating_add(amount);
    }

    pub fn class_level(&self, class: &ClassId) -> Option<&ClassLevelProgression> {
        self.class_levels.get(class)
    }

    pub fn all_classes(&self) -> &HashMap<ClassId, ClassLevelProgression> {
        &self.class_levels
    }

    pub fn can_level_up(&self) -> bool {
        if self.total_level() >= registry::rules().max_level() {
            return false;
        }
        self.experience >= self.experience_for_next_level().unwrap_or(u32::MAX)
    }

    pub fn level_up(&mut self, class: &ClassId) -> Result<u8, LevelUpError> {
        if self.total_level() >= registry::rules().max_level() {
            return Err(LevelUpError::MaxLevelReached);
        }

        match self.class_levels.entry(class.clone()) {
            Entry::Occupied(occupied_entry) => {
                occupied_entry.into_mut().level += 1;
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(ClassLevelProgression::new(1, None));
            }
        }

        if self.first_class.is_none() {
            self.first_class = Some(class.clone());
        }
        self.latest_class = Some(class.clone());

        Ok(self.class_levels.get(class).unwrap().level)
    }

    pub fn subclass(&self, class: &ClassId) -> Option<&SubclassId> {
        self.class_levels
            .get(class)
            .and_then(|progression| progression.subclass())
    }

    pub fn set_subclass(
        &mut self,
        class_id: &ClassId,
        subclass: &SubclassId,
    ) -> Result<(), SubclassError> {
        if !self.class_levels.contains_key(class_id) {
            return Err(SubclassError::ClassNotLeveled(class_id.clone()));
        }

        let Some(class) = ClassesRegistry::get(class_id) else {
            return Err(SubclassError::ClassDoesNotExist(class_id.clone()));
        };

        if !class.subclasses.contains(subclass) {
            return Err(SubclassError::SubclassDoesNotExist(subclass.clone()));
        }

        if let Some(progression) = self.class_levels.get_mut(class_id) {
            progression.subclass = Some(subclass.clone());
        }

        Ok(())
    }

    pub fn first_class(&self) -> Option<&ClassId> {
        self.first_class.as_ref()
    }

    pub fn latest_class(&self) -> Option<&ClassId> {
        self.latest_class.as_ref()
    }
}

impl Level for CharacterLevels {
    fn total_level(&self) -> u8 {
        self.total_level()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LevelUpError {
    MaxLevelReached,
}

#[derive(Debug, Clone)]
pub enum SubclassError {
    ClassNotLeveled(ClassId),
    ClassDoesNotExist(ClassId),
    SubclassDoesNotExist(SubclassId),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn creature_level_new_and_total_level() {
        let lvl = ChallengeRating::Full(5);
        assert_eq!(lvl.total_level(), 5);
    }

    #[test]
    #[should_panic(expected = "Class level cannot be zero")]
    fn class_level_progression_zero_panics() {
        ClassLevelProgression::new(0, None);
    }

    #[test]
    fn class_level_progression_new_and_accessors() {
        let subclass = Some(SubclassId::new("nat20_core", "subclass.test"));
        let clp = ClassLevelProgression::new(3, subclass.clone());
        assert_eq!(clp.level(), 3);
        assert_eq!(clp.subclass(), subclass.as_ref());
    }

    #[test]
    fn character_level_new_and_total_level() {
        let cl = CharacterLevels::new();
        assert_eq!(cl.total_level(), 0);
        assert_eq!(cl.experience(), 0);
    }

    #[test]
    fn character_level_level_up_and_class_level() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.fighter");
        assert!(cl.level_up(&class).is_ok());
        assert_eq!(cl.class_level(&class).unwrap().level(), 1);
        assert!(cl.level_up(&class).is_ok());
        assert_eq!(cl.class_level(&class).unwrap().level(), 2);
    }

    #[test]
    fn character_level_level_up_beyond_max_error() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.wizard");
        for _ in 0..registry::rules().max_level() {
            assert!(cl.level_up(&class).is_ok());
        }
        // This should panic
        assert_eq!(cl.level_up(&class), Err(LevelUpError::MaxLevelReached));
    }

    /// TODO: This is basically an integration test and should be moved
    #[test]
    fn character_level_set_and_get_subclass() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.warlock");
        assert!(cl.level_up(&class).is_ok());
        let subclass = SubclassId::new("nat20_core", "subclass.warlock.fiend_patron");
        assert!(cl.set_subclass(&class, &subclass).is_ok());
        assert_eq!(cl.subclass(&class), Some(&subclass));
    }

    #[test]
    fn character_level_experience_for_next_level() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.wizard");
        assert!(cl.level_up(&class).is_ok()); // level 1
        assert_eq!(cl.experience_for_next_level(), Some(300));
        assert!(cl.level_up(&class).is_ok()); // level 2
        assert_eq!(cl.experience_for_next_level(), Some(900));
    }

    #[test]
    fn character_level_experience_for_next_level_at_max() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.warlock");
        for _ in 0..registry::rules().max_level() {
            assert!(cl.level_up(&class).is_ok());
        }
        assert_eq!(cl.experience_for_next_level(), None);
    }

    #[test]
    fn character_level_can_level_up() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.wizard");
        assert!(cl.can_level_up());
        assert!(cl.level_up(&class).is_ok());
        assert!(!cl.can_level_up());
        cl.add_experience(300);
        assert!(cl.can_level_up());

        cl.add_experience(u32::MAX);
        for _ in 0..(registry::rules().max_level() - 1) {
            assert!(cl.level_up(&class).is_ok());
        }

        assert!(!cl.can_level_up());
    }
}
