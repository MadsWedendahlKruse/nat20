use std::collections::BTreeMap;
use std::fmt;

use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::components::id::{
    ActionId, BackgroundId, ClassId, EffectId, FeatId, ItemId, SpeciesId, SubclassId, SubspeciesId,
};

use super::{ability::Ability, proficiency::ProficiencyLevel};

#[derive(Debug, Hash, Eq, PartialEq, Clone, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ModifierSource {
    Base, // The base value, no specific source
    Background(BackgroundId),
    Item(ItemId), // e.g. "Belt of Strength"
    ClassFeature(ClassId),
    ClassLevel(ClassId),         // e.g. "Fighter Level 3"
    SubclassFeature(SubclassId), // e.g. "Champion"
    Action(ActionId),            // e.g. "Tactical Mind"
    Effect(EffectId),            // optional: unique ID for internal tracking
    Ability(Ability),            // e.g. "Strength"
    Proficiency(ProficiencyLevel),
    Feat(FeatId),                 // e.g. "Great Weapon Master"
    FeatRepeatable(FeatId, Uuid), // e.g. "Ability Score Improvement" with unique instance ID
    Custom(String),               // fallback for ad-hoc things
    Species(SpeciesId),           // e.g. "Dwarf"
    Subspecies(SubspeciesId),     // e.g. "Hill Dwarf"
    None,                         // Used for cases where no modifier is applicable
}

impl fmt::Display for ModifierSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModifierSource::Base => write!(f, "Base"),
            ModifierSource::Background(id) => write!(f, "Background: {}", id),
            ModifierSource::Item(name) => write!(f, "Item: {}", name),
            ModifierSource::ClassFeature(id) => write!(f, "Class Feature: {}", id),
            ModifierSource::ClassLevel(id) => {
                write!(f, "Class Level: {}", id)
            }
            ModifierSource::SubclassFeature(id) => write!(f, "Subclass Feature: {}", id),
            ModifierSource::Action(id) => write!(f, "Action: {}", id),
            ModifierSource::Effect(id) => write!(f, "Effect: {}", id),
            ModifierSource::Custom(text) => write!(f, "{}", text),
            ModifierSource::Ability(ability) => write!(f, "{:?} Modifier", ability),
            ModifierSource::Proficiency(proficiency) => write!(f, "Proficiency: {:?}", proficiency),
            ModifierSource::Feat(feat) => write!(f, "Feat: {}", feat),
            ModifierSource::FeatRepeatable(feat, instance_id) => {
                write!(f, "Feat: {} ({})", feat, instance_id)
            }
            ModifierSource::Species(id) => write!(f, "Species: {}", id),
            ModifierSource::Subspecies(id) => write!(f, "Subspecies: {}", id),
            ModifierSource::None => write!(f, "None"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModifierSet {
    modifiers: BTreeMap<ModifierSource, i32>,
}

pub trait Modifiable {
    fn add_modifier<T: Into<i32>>(&mut self, source: ModifierSource, value: T);
    fn remove_modifier(&mut self, source: &ModifierSource);
    fn total(&self) -> i32;
}

impl ModifierSet {
    pub fn new() -> Self {
        Self {
            modifiers: BTreeMap::new(),
        }
    }

    pub fn from(source: ModifierSource, value: i32) -> Self {
        let mut modifiers = BTreeMap::new();
        modifiers.insert(source, value);
        Self { modifiers }
    }

    pub fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (ModifierSource, i32)>,
    {
        let modifiers = iter.into_iter().collect();
        Self { modifiers }
    }

    pub fn add_modifier_set(&mut self, other: &ModifierSet) {
        for (source, value) in &other.modifiers {
            let entry = self.modifiers.entry(source.clone()).or_insert(0);
            *entry += value;
        }
    }

    pub fn get(&self, source: &ModifierSource) -> Option<i32> {
        self.modifiers.get(source).cloned()
    }

    pub fn contains_key(&self, source: &ModifierSource) -> bool {
        self.modifiers.contains_key(source)
    }

    // Only used for ability modifiers
    pub fn scale_modifiers(&mut self, scale: f32) {
        for m in self.modifiers.values_mut() {
            *m = (m.clone() as f32 * scale).round() as i32;
        }
    }

    pub fn is_empty(&self) -> bool {
        if self.modifiers.is_empty() {
            return true;
        }
        for value in self.modifiers.values() {
            if *value != 0 {
                return false;
            }
        }
        true
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ModifierSource, &i32)> {
        self.modifiers.iter()
    }
}

impl Modifiable for ModifierSet {
    fn add_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<i32>,
    {
        let value = value.into();
        if value == 0 {
            return;
        }
        self.modifiers.insert(source.clone(), value);
    }

    fn remove_modifier(&mut self, source: &ModifierSource) {
        self.modifiers.remove(source);
    }

    fn total(&self) -> i32 {
        self.modifiers.values().map(|m| m).sum()
    }
}

pub trait KeyedModifiable<K> {
    fn add_modifier<T>(&mut self, key: &K, source: ModifierSource, value: T)
    where
        T: Into<i32>;
    fn remove_modifier(&mut self, key: &K, source: &ModifierSource);
    fn total(&self, key: &K) -> i32;
}

impl fmt::Display for ModifierSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for i in 0..self.modifiers.len() {
            let (source, value) = self.modifiers.iter().nth(i).unwrap();
            if value == &0 {
                continue;
            }
            if i != 0 {
                s += " ";
            }
            let sign = if *value >= 0 { "+" } else { "-" };
            s += &format!("{}{} ({})", sign, value.abs(), source);
        }
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn modifiers() {
        let mut modifiers = ModifierSet::new();
        modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.belt_of_strength")),
            4,
        );
        modifiers.add_modifier(
            ModifierSource::Effect(EffectId::new("nat20_core", "nat20_core::effect.bless")),
            1,
        );

        assert_eq!(modifiers.modifiers.len(), 2);
        assert_eq!(modifiers.total(), 5);

        modifiers.remove_modifier(&ModifierSource::Effect(EffectId::new(
            "nat20_core",
            "nat20_core::effect.bless",
        )));
        assert_eq!(modifiers.modifiers.len(), 1);
        assert_eq!(modifiers.total(), 4);
        println!("Modifiers breakdown: {}", modifiers);
    }
}
