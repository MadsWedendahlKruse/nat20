use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    str::FromStr,
};

use crate::{
    components::modifier::{FlatModifiable, FlatModifierMap, KeyedFlatModifiable},
    registry::serialize::schema::impl_string_schema,
};

use super::modifier::ModifierSource;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter, IntoEnumIterator};

#[derive(
    EnumIter,
    Hash,
    Eq,
    PartialEq,
    Debug,
    Clone,
    Copy,
    Display,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
)]
#[serde(try_from = "String", into = "String")]
pub enum Ability {
    Strength = 0,
    Dexterity = 1,
    Constitution = 2,
    Intelligence = 3,
    Wisdom = 4,
    Charisma = 5,
}

impl Ability {
    pub fn acronym(&self) -> &'static str {
        match self {
            Ability::Strength => "STR",
            Ability::Dexterity => "DEX",
            Ability::Constitution => "CON",
            Ability::Intelligence => "INT",
            Ability::Wisdom => "WIS",
            Ability::Charisma => "CHA",
        }
    }

    pub fn set() -> HashSet<Ability> {
        Ability::iter().collect()
    }
}

impl FromStr for Ability {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "strength" | "str" => Ok(Ability::Strength),
            "dexterity" | "dex" => Ok(Ability::Dexterity),
            "constitution" | "con" => Ok(Ability::Constitution),
            "intelligence" | "int" => Ok(Ability::Intelligence),
            "wisdom" | "wis" => Ok(Ability::Wisdom),
            "charisma" | "cha" => Ok(Ability::Charisma),
            _ => Err(format!("Unknown ability: {}", s)),
        }
    }
}

impl TryFrom<String> for Ability {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ability::from_str(&value)
    }
}

impl From<Ability> for String {
    fn from(ability: Ability) -> Self {
        ability.to_string()
    }
}

impl_string_schema!(
    Ability,
    "Ability",
    "description": "An ability, by full name or acronym (`str`, `dex`, ...), case-insensitive.",
    "examples": ["strength", "dexterity", "constitution", "intelligence", "wisdom", "charisma"]
);

#[derive(Debug, Clone)]
pub struct AbilityScore {
    pub ability: Ability,
    pub base: i32,
    modifiers: FlatModifierMap,
}

impl AbilityScore {
    pub fn default(ability: Ability) -> Self {
        Self {
            ability,
            base: 10,
            modifiers: FlatModifierMap::default(),
        }
    }

    pub fn new(ability: Ability, base: i32) -> Self {
        Self {
            ability,
            base,
            modifiers: FlatModifierMap::default(),
        }
    }

    pub fn ability_modifier(&self) -> FlatModifierMap {
        let mut ability_modifiers = self.modifiers.clone();
        let mut zero_sources = Vec::new();
        for (source, value) in ability_modifiers.iter_mut() {
            *value = *value / 2;
            if *value == 0 {
                zero_sources.push(source.clone());
            }
        }
        for source in zero_sources {
            ability_modifiers.remove_modifier(&source);
        }
        let base_modifier = (self.base - 10) / 2;
        ability_modifiers.add_modifier(ModifierSource::Ability(self.ability), base_modifier);
        ability_modifiers
    }
}

impl FlatModifiable for AbilityScore {
    fn modifiers(&self) -> &FlatModifierMap {
        &self.modifiers
    }

    fn modifiers_mut(&mut self) -> &mut FlatModifierMap {
        &mut self.modifiers
    }

    fn total(&self) -> i32 {
        self.base + self.modifiers.total()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct AbilityScoreDistribution {
    pub scores: HashMap<Ability, u8>,
    pub plus_2_bonus: Ability,
    pub plus_1_bonus: Ability,
}

#[derive(Debug, Clone)]
pub struct AbilityScoreMap {
    pub scores: HashMap<Ability, AbilityScore>,
}

impl AbilityScoreMap {
    pub fn new() -> Self {
        let mut scores = HashMap::new();
        for ability in Ability::iter() {
            scores.insert(ability, AbilityScore::default(ability));
        }
        Self { scores }
    }

    pub fn get(&self, ability: &Ability) -> &AbilityScore {
        self.scores.get(&ability).unwrap()
    }

    fn get_mut(&mut self, ability: &Ability) -> &mut AbilityScore {
        self.scores.get_mut(&ability).unwrap()
    }

    pub fn set(&mut self, ability: Ability, score: AbilityScore) {
        self.scores.insert(ability, score);
    }

    pub fn ability_modifier(&self, ability: &Ability) -> FlatModifierMap {
        self.get(ability).ability_modifier()
    }

    pub fn get_max_score(&self, abilities: &[Ability]) -> (Ability, i32) {
        let mut max_ability = abilities[0];
        let mut max_score = self.get(&max_ability).base;
        for &ability in abilities.iter().skip(1) {
            let score = self.get(&ability).base;
            if score > max_score {
                max_score = score;
                max_ability = ability;
            }
        }
        (max_ability, max_score)
    }
}

impl KeyedFlatModifiable<Ability> for AbilityScoreMap {
    type Entry = AbilityScore;

    fn entry(&self, key: &Ability) -> &AbilityScore {
        self.get(key)
    }

    fn entry_mut(&mut self, key: &Ability) -> &mut AbilityScore {
        self.get_mut(key)
    }
}

impl From<AbilityScoreDistribution> for AbilityScoreMap {
    fn from(distribution: AbilityScoreDistribution) -> Self {
        let mut scores = HashMap::new();
        for (ability, score) in distribution.scores {
            let mut base = score;
            if ability == distribution.plus_2_bonus {
                base += 2;
            } else if ability == distribution.plus_1_bonus {
                base += 1;
            }
            scores.insert(ability, AbilityScore::new(ability, base as i32));
        }
        Self { scores }
    }
}

impl From<[(Ability, i32); 6]> for AbilityScoreMap {
    fn from(scores: [(Ability, i32); 6]) -> Self {
        let mut map = AbilityScoreMap::new();
        for (ability, score) in scores {
            map.set(ability, AbilityScore::new(ability, score));
        }
        map
    }
}

#[cfg(test)]
mod tests {

    use crate::components::id::ItemId;

    use super::*;

    #[test]
    fn ability_modifier() {
        let ability_score = AbilityScore::new(Ability::Strength, 16);
        let modifier = ability_score.ability_modifier();
        assert_eq!(modifier.total(), 3); // (16 - 10) / 2 = 3
        println!("{:?}", modifier);
    }

    #[test]
    fn ability_total() {
        let mut ability_score = AbilityScore::new(Ability::Dexterity, 14);
        ability_score.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_dexterity")),
            2,
        );
        assert_eq!(ability_score.total(), 16); // 14 + 2 = 16
        assert_eq!(ability_score.ability_modifier().total(), 3); // (16 - 10) / 2 = 3
    }

    #[test]
    fn ability_score_set() {
        let mut ability_scores = AbilityScoreMap::new();
        ability_scores.set(
            Ability::Dexterity,
            AbilityScore::new(Ability::Dexterity, 15),
        );
        ability_scores.add_modifier(
            &Ability::Dexterity,
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_dexterity")),
            2,
        );

        assert_eq!(ability_scores.total(&Ability::Dexterity), 17);
        assert_eq!(
            ability_scores.ability_modifier(&Ability::Dexterity).total(),
            3
        );
    }

    #[test]
    fn ability_score_set_modifier() {
        let mut ability_scores = AbilityScoreMap::new();
        ability_scores.set(Ability::Strength, AbilityScore::new(Ability::Strength, 17));
        ability_scores.add_modifier(
            &Ability::Strength,
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_strength")),
            2,
        );

        assert_eq!(ability_scores.total(&Ability::Strength), 19);
        assert_eq!(
            ability_scores.ability_modifier(&Ability::Strength).total(),
            4
        );
    }

    #[test]
    fn ability_score_set_multiple_abilities() {
        let mut ability_scores = AbilityScoreMap::new();
        ability_scores.set(
            Ability::Dexterity,
            AbilityScore::new(Ability::Dexterity, 15),
        );
        ability_scores.set(Ability::Strength, AbilityScore::new(Ability::Strength, 17));
        ability_scores.set(Ability::Charisma, AbilityScore::new(Ability::Charisma, 8));

        assert_eq!(ability_scores.total(&Ability::Dexterity), 15);
        assert_eq!(ability_scores.total(&Ability::Strength), 17);
        assert_eq!(ability_scores.total(&Ability::Charisma), 8);
    }

    #[test]
    fn ability_score_set_remove_modifier() {
        let mut ability_scores = AbilityScoreMap::new();
        ability_scores.set(
            Ability::Dexterity,
            AbilityScore::new(Ability::Dexterity, 15),
        );
        ability_scores.add_modifier(
            &Ability::Dexterity,
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_dexterity")),
            2,
        );

        assert_eq!(ability_scores.total(&Ability::Dexterity), 17);
        ability_scores.remove_modifier(
            &Ability::Dexterity,
            &ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_dexterity")),
        );
        assert_eq!(ability_scores.total(&Ability::Dexterity), 15);
    }
}
