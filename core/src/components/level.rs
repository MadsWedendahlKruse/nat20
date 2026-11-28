use std::{collections::HashMap, fmt, str::FromStr, sync::LazyLock};

use serde::{Deserialize, Serialize};

use crate::{
    components::id::{ClassId, SubclassId},
    registry::{registry::ClassesRegistry, serialize::schema::impl_string_schema},
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

// TODO: Not sure if hardcoding this is the best approach, but it works for now
static EXPERIENCE_BY_CHALLENGE_RATING: LazyLock<HashMap<ChallengeRating, u32>> =
    LazyLock::new(|| {
        HashMap::from([
            (ChallengeRating::Full(0), 0),
            (ChallengeRating::Eighth, 25),
            (ChallengeRating::Quarter, 50),
            (ChallengeRating::Half, 100),
            (ChallengeRating::Full(1), 200),
            (ChallengeRating::Full(2), 450),
            (ChallengeRating::Full(3), 700),
            (ChallengeRating::Full(4), 1100),
            (ChallengeRating::Full(5), 1800),
            (ChallengeRating::Full(6), 2300),
            (ChallengeRating::Full(7), 2900),
            (ChallengeRating::Full(8), 3900),
            (ChallengeRating::Full(9), 5000),
            (ChallengeRating::Full(10), 5900),
            (ChallengeRating::Full(11), 7200),
            (ChallengeRating::Full(12), 8400),
            (ChallengeRating::Full(13), 10000),
            (ChallengeRating::Full(14), 11500),
            (ChallengeRating::Full(15), 13000),
            (ChallengeRating::Full(16), 15000),
            (ChallengeRating::Full(17), 18000),
            (ChallengeRating::Full(18), 20000),
            (ChallengeRating::Full(19), 22000),
            (ChallengeRating::Full(20), 25000),
            (ChallengeRating::Full(21), 33000),
            (ChallengeRating::Full(22), 41000),
            (ChallengeRating::Full(23), 50000),
            (ChallengeRating::Full(24), 62000),
            (ChallengeRating::Full(25), 75000),
            (ChallengeRating::Full(26), 90000),
            (ChallengeRating::Full(27), 105000),
            (ChallengeRating::Full(28), 120000),
            (ChallengeRating::Full(29), 135000),
            (ChallengeRating::Full(30), 155000),
        ])
    });

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

// TODO: Not sure if hardcoding this is the best approach, but it works for now
static EXPERIENCE_PER_LEVEL: LazyLock<Vec<u32>> = LazyLock::new(|| {
    vec![
        0,      // dummy for level 0
        0,      // level 1
        300,    // level 2
        900,    // level 3
        2700,   // level 4
        6500,   // level 5
        14000,  // level 6
        23000,  // level 7
        34000,  // level 8
        48000,  // level 9
        64000,  // level 10
        85000,  // level 11
        100000, // level 12
        120000, // level 13
        140000, // level 14
        165000, // level 15
        195000, // level 16
        225000, // level 17
        265000, // level 18
        305000, // level 19
        355000, // level 20
    ]
});

static MAX_LEVEL: u8 = 20;

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

    pub fn class_level(&self, class: &ClassId) -> Option<&ClassLevelProgression> {
        self.class_levels.get(class)
    }

    pub fn all_classes(&self) -> &HashMap<ClassId, ClassLevelProgression> {
        &self.class_levels
    }

    pub fn level_up(&mut self, class: ClassId) -> u8 {
        if !self.class_levels.contains_key(&class) {
            self.class_levels
                .insert(class.clone(), ClassLevelProgression::new(1, None));
        } else {
            let level = self.class_levels.get_mut(&class).unwrap();
            if level.level >= MAX_LEVEL {
                panic!("Cannot level up beyond maximum level of {}", MAX_LEVEL);
            }
            level.level += 1;
        }

        if self.first_class.is_none() {
            self.first_class = Some(class.clone());
        }
        self.latest_class = Some(class.clone());

        self.class_levels.get(&class).unwrap().level
    }

    pub fn subclass(&self, class: &ClassId) -> Option<&SubclassId> {
        self.class_levels
            .get(class)
            .and_then(|progression| progression.subclass())
    }

    pub fn set_subclass(&mut self, class_id: &ClassId, subclass: &SubclassId) {
        if !self.class_levels.contains_key(class_id) {
            panic!("Cannot set subclass for a class that has not been leveled up");
        }
        if let Some(class) = ClassesRegistry::get(class_id) {
            if !class.subclasses.contains(subclass) {
                panic!(
                    "Subclass {:?} does not exist for class {:?}",
                    subclass, class_id
                );
            }
        } else {
            panic!("Class {:?} does not exist", class_id);
        }

        if let Some(progression) = self.class_levels.get_mut(class_id) {
            progression.subclass = Some(subclass.clone());
        }
    }

    pub fn total_level(&self) -> u8 {
        self.class_levels.values().map(|p| p.level()).sum()
    }

    pub fn experience(&self) -> u32 {
        self.experience
    }

    pub fn experience_for_next_level(&self) -> u32 {
        let next_level = self.total_level() + 1;
        if next_level > MAX_LEVEL {
            // TODO: Handle max level case, maybe return a special value or error
        }
        *EXPERIENCE_PER_LEVEL.get(next_level as usize).unwrap_or(&0)
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
        cl.level_up(class.clone());
        assert_eq!(cl.class_level(&class).unwrap().level(), 1);
        cl.level_up(class.clone());
        assert_eq!(cl.class_level(&class).unwrap().level(), 2);
    }

    #[test]
    #[should_panic(expected = "Cannot level up beyond maximum level")]
    fn character_level_level_up_beyond_max_panics() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.wizard");
        for _ in 0..MAX_LEVEL {
            cl.level_up(class.clone());
        }
        // This should panic
        cl.level_up(class);
    }

    #[test]
    fn character_level_set_and_get_subclass() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.warlock");
        cl.level_up(class.clone());
        let subclass = SubclassId::new("nat20_core", "subclass.warlock.fiend_patron");
        cl.set_subclass(&class, &subclass);
        assert_eq!(cl.subclass(&class), Some(&subclass));
    }

    #[test]
    fn character_level_experience_for_next_level() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.wizard");
        cl.level_up(class.clone()); // level 1
        assert_eq!(cl.experience_for_next_level(), 300);
        cl.level_up(class.clone()); // level 2
        assert_eq!(cl.experience_for_next_level(), 900);
    }

    #[test]
    fn character_level_experience_for_next_level_at_max() {
        let mut cl = CharacterLevels::new();
        let class = ClassId::new("nat20_core", "class.warlock");
        for _ in 0..MAX_LEVEL {
            cl.level_up(class.clone());
        }
        // Should return 0 or handle gspeciesfully
        assert_eq!(cl.experience_for_next_level(), 0);
    }
}
