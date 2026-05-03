use std::fmt;

use serde::{Deserialize, Serialize};

use crate::components::modifier::ModifierSource;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProficiencyLevel {
    None,
    Proficient,
    Expertise,
    Half, // Optional: for features like Bard’s Jack of All Trades
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Proficiency {
    level: ProficiencyLevel,
    source: ModifierSource,
}

impl ProficiencyLevel {
    pub fn multiplier(&self) -> f32 {
        match self {
            ProficiencyLevel::None => 0.0,
            ProficiencyLevel::Half => 0.5,
            ProficiencyLevel::Proficient => 1.0,
            ProficiencyLevel::Expertise => 2.0,
        }
    }

    pub fn bonus(&self, proficiency_bonus: u8) -> u8 {
        (self.multiplier() * proficiency_bonus as f32).floor() as u8
    }
}

impl fmt::Display for ProficiencyLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Proficiency {
    pub fn new(level: ProficiencyLevel, source: ModifierSource) -> Self {
        Self { level, source }
    }

    pub fn level(&self) -> &ProficiencyLevel {
        &self.level
    }

    pub fn source(&self) -> &ModifierSource {
        &self.source
    }

    pub fn bonus(&self, proficiency_bonus: u8) -> u8 {
        self.level.bonus(proficiency_bonus)
    }
}

impl Default for Proficiency {
    fn default() -> Self {
        Self {
            level: ProficiencyLevel::None,
            source: ModifierSource::None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn proficiency_bonus() {
        let prof = ProficiencyLevel::Proficient;
        assert_eq!(prof.bonus(2), 2);
        assert_eq!(prof.bonus(3), 3);
    }

    #[test]
    fn expertise_bonus() {
        let prof = ProficiencyLevel::Expertise;
        assert_eq!(prof.bonus(2), 4);
        assert_eq!(prof.bonus(3), 6);
    }

    #[test]
    fn half_bonus() {
        let prof = ProficiencyLevel::Half;
        assert_eq!(prof.bonus(2), 1);
        assert_eq!(prof.bonus(3), 1);
    }

    #[test]
    fn none_bonus() {
        let prof = ProficiencyLevel::None;
        assert_eq!(prof.bonus(2), 0);
        assert_eq!(prof.bonus(3), 0);
    }
}
