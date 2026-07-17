use std::{collections::HashMap, sync::LazyLock};

use crate::{
    components::id::{ClassId, SubclassId},
    registry::registry::ClassesRegistry,
};

pub trait Level {
    fn total_level(&self) -> u8;

    fn proficiency_bonus(&self) -> u8 {
        let total_level = self.total_level();
        if total_level == 0 {
            return 0;
        }
        // Proficiency bonus is typically calculated as (total level - 1) / 4 + 2
        // This is a common rule in many RPG systems, including D&D 5e
        (total_level - 1) / 4 + 2
    }
}

// TODO: Not sure if hardcoding this is the best approach, but it works for now
static EXPERIENCE_BY_CHALLENGE_RATING: LazyLock<HashMap<u8, u32>> = LazyLock::new(|| {
    HashMap::from([
        (0, 0),
        (1, 200),
        (2, 450),
        (3, 700),
        (4, 1100),
        (5, 1800),
        (6, 2300),
        (7, 2900),
        (8, 3900),
        (9, 5000),
        (10, 5900),
        (11, 7200),
        (12, 8400),
        (13, 10000),
        (14, 11500),
        (15, 13000),
        (16, 15000),
        (17, 18000),
        (18, 20000),
        (19, 22000),
        (20, 25000),
        (21, 33000),
        (22, 41000),
        (23, 50000),
        (24, 62000),
        (25, 75000),
        (26, 90000),
        (27, 105000),
        (28, 120000),
        (29, 135000),
        (30, 155000),
    ])
});

// The SRD supports fractional challenge ratings, but that's a bit more complex
// to handle, so we'll stick with whole numbers for now
#[derive(Debug, Clone)]
pub struct ChallengeRating(u8);

impl ChallengeRating {
    pub fn new(level: u8) -> Self {
        if level == 0 {
            panic!("Creature level cannot be zero");
        }
        Self(level)
    }

    pub fn experience(&self) -> u32 {
        *EXPERIENCE_BY_CHALLENGE_RATING.get(&self.0).unwrap_or(&0)
    }
}

impl Level for ChallengeRating {
    fn total_level(&self) -> u8 {
        self.0 as u8
    }
}

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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
    #[should_panic(expected = "Creature level cannot be zero")]
    fn creature_level_zero_panics() {
        ChallengeRating::new(0);
    }

    #[test]
    fn creature_level_new_and_total_level() {
        let lvl = ChallengeRating::new(5);
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
