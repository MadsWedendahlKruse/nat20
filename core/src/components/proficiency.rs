use std::{collections::HashMap, fmt, hash::Hash};

use parry3d::utils::hashset::HashSet;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::components::modifier::ModifierSource;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
#[serde(rename_all = "snake_case")]
pub enum ProficiencyLevel {
    Proficient,
    Expertise,
    Half, // Optional: for features like Bard’s Jack of All Trades
}

impl ProficiencyLevel {
    pub fn multiplier(&self) -> f32 {
        match self {
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Proficiency {
    level: ProficiencyLevel,
    source: ModifierSource,
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

/// A map that tracks binary proficiency (proficient or not, i.e. no level) for
/// different categories, keyed by `K`.
/// Each category can have multiple sources contributing to its proficiency.
///
/// This is mainly used for things like weapon and armor proficiencies, where the
/// concept of having expertise or half proficiency does not apply.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct BinaryProficiencyMap<K>
where
    K: Eq + Hash,
{
    map: HashMap<K, HashSet<ModifierSource>>,
}

impl<K> BinaryProficiencyMap<K>
where
    K: Eq + Hash,
{
    pub fn set_proficient(&mut self, category: K, source: ModifierSource) {
        self.map.entry(category).or_default().insert(source);
    }

    pub fn remove_proficient(&mut self, category: &K, source: &ModifierSource) {
        if let Some(sources) = self.map.get_mut(category) {
            sources.remove(source);
            if sources.is_empty() {
                self.map.remove(category);
            }
        }
    }

    pub fn sources(&self, category: &K) -> Option<&HashSet<ModifierSource>> {
        self.map.get(category)
    }

    pub fn is_proficient(&self, category: &K) -> bool {
        self.sources(category)
            .map_or(false, |sources| !sources.is_empty())
    }

    pub fn proficiency(&self, category: &K) -> Option<Proficiency> {
        if let Some(sources) = self.sources(category)
            && !sources.is_empty()
        {
            return Some(Proficiency::new(
                ProficiencyLevel::Proficient,
                sources.iter().next().cloned().unwrap(),
            ));
        }
        None
    }
}

impl<K> Default for BinaryProficiencyMap<K>
where
    K: Eq + Hash,
{
    fn default() -> Self {
        Self {
            map: HashMap::new(),
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
    fn binary_proficiency_map() {
        let mut map = BinaryProficiencyMap::<u8>::default();
        let source = ModifierSource::Base;

        map.set_proficient(1, source.clone());
        assert!(map.is_proficient(&1));
        assert_eq!(
            map.sources(&1),
            Some(&[source.clone()].iter().cloned().collect())
        );
        assert_eq!(
            map.proficiency(&1),
            Some(Proficiency::new(
                ProficiencyLevel::Proficient,
                source.clone()
            ))
        );

        map.remove_proficient(&1, &source);
        assert!(!map.is_proficient(&1));
        assert_eq!(map.sources(&1), None);
        assert_eq!(map.proficiency(&1), None);
    }
}
