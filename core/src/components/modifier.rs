use std::collections::BTreeMap;
use std::fmt;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::components::{
    dice::{DiceSet, DiceSetResult},
    id::{
        ActionId, BackgroundId, ClassId, EffectId, FeatId, Id, ItemId, SpeciesId, SubclassId,
        SubspeciesId,
    },
};

use super::{ability::Ability, proficiency::ProficiencyLevel};

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, JsonSchema)]
pub struct ModifierMap {
    pub(crate) modifiers: BTreeMap<ModifierSource, ModifierKind>,
}

impl ModifierMap {
    pub fn add_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<ModifierKind>,
    {
        let value = value.into();
        if value.is_zero() {
            return;
        }
        if let Some(existing) = self.modifiers.get_mut(&source) {
            existing.add(value);
        } else {
            self.modifiers.insert(source, value);
        }
    }

    pub fn remove_modifier(&mut self, source: &ModifierSource) -> Option<ModifierKind> {
        self.modifiers.remove(source)
    }

    pub fn replace_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<ModifierKind>,
    {
        let value = value.into();
        self.modifiers.insert(source, value);
    }

    pub fn evaluate(&self) -> ModifierResult {
        let mut results = BTreeMap::new();
        for (source, kind) in &self.modifiers {
            results.insert(source.clone(), kind.evaluate());
        }
        ModifierResult { results }
    }

    pub fn from<T>(source: ModifierSource, value: T) -> Self
    where
        T: Into<ModifierKind>,
    {
        let mut modifiers = BTreeMap::new();
        modifiers.insert(source, value.into());
        Self { modifiers }
    }

    pub fn from_iter<I, T>(iter: I) -> Self
    where
        I: IntoIterator<Item = (ModifierSource, T)>,
        T: Into<ModifierKind>,
    {
        let modifiers = iter
            .into_iter()
            .map(|(source, value)| (source, value.into()))
            .collect();
        Self { modifiers }
    }

    pub fn min(&self) -> i32 {
        self.modifiers.values().map(|kind| kind.min()).sum()
    }

    pub fn max(&self) -> i32 {
        self.modifiers.values().map(|kind| kind.max()).sum()
    }

    pub fn range(&self) -> Range<i32> {
        Range {
            min: self.min(),
            max: self.max(),
        }
    }

    pub fn add_modifier_map(&mut self, other: &ModifierMap) {
        for (source, kind) in &other.modifiers {
            self.add_modifier(source.clone(), kind.clone());
        }
    }

    pub fn matches_result(&self, result: &ModifierResult) -> bool {
        compare_map_result(&self.modifiers, &result.results)
    }

    pub fn contains_source(&self, source: &ModifierSource) -> bool {
        self.modifiers.contains_key(source)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub enum ModifierKind {
    Flat(i32),
    Dice(DiceSet),
    Composite(Vec<ModifierKind>),
}

impl ModifierKind {
    pub fn is_zero(&self) -> bool {
        match self {
            ModifierKind::Flat(value) => *value == 0,
            ModifierKind::Dice(dice_set) => dice_set.is_zero(),
            ModifierKind::Composite(kinds) => kinds.iter().all(|kind| kind.is_zero()),
        }
    }

    pub fn evaluate(&self) -> ModifierKindResult {
        match self {
            ModifierKind::Flat(value) => ModifierKindResult::Flat(*value),
            ModifierKind::Dice(dice_set) => ModifierKindResult::Dice(dice_set.roll()),
            ModifierKind::Composite(kinds) => {
                let results = kinds.iter().map(|kind| kind.evaluate()).collect();
                ModifierKindResult::Composite(results)
            }
        }
    }

    pub fn min(&self) -> i32 {
        match self {
            ModifierKind::Flat(value) => *value,
            ModifierKind::Dice(dice_set) => dice_set.min_roll() as i32,
            ModifierKind::Composite(kinds) => kinds.iter().map(|kind| kind.min()).sum(),
        }
    }

    pub fn max(&self) -> i32 {
        match self {
            ModifierKind::Flat(value) => *value,
            ModifierKind::Dice(dice_set) => dice_set.max_roll() as i32,
            ModifierKind::Composite(kinds) => kinds.iter().map(|kind| kind.max()).sum(),
        }
    }

    pub fn add(&mut self, other: ModifierKind) {
        // Little bit of a hack to avoid moving self when matching on tuple (self, other)
        let current = std::mem::replace(self, ModifierKind::Flat(0));
        *self = match (current, other) {
            (ModifierKind::Flat(v), ModifierKind::Flat(ov)) => ModifierKind::Flat(v + ov),
            (ModifierKind::Composite(mut kinds), other_kind) => {
                kinds.push(other_kind);
                ModifierKind::Composite(kinds)
            }
            (self_kind, other_kind) => ModifierKind::Composite(vec![self_kind, other_kind]),
        };
    }

    pub fn matches_result(&self, result: &ModifierKindResult) -> bool {
        compare_kind_result(self, result)
    }
}

impl From<i32> for ModifierKind {
    fn from(value: i32) -> Self {
        ModifierKind::Flat(value)
    }
}

impl From<DiceSet> for ModifierKind {
    fn from(dice_set: DiceSet) -> Self {
        ModifierKind::Dice(dice_set)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModifierKindResult {
    Flat(i32),
    Dice(DiceSetResult),
    Composite(Vec<ModifierKindResult>),
}

impl ModifierKindResult {
    pub fn total(&self) -> i32 {
        match self {
            ModifierKindResult::Flat(value) => *value,
            ModifierKindResult::Dice(dice_result) => dice_result.total() as i32,
            ModifierKindResult::Composite(results) => {
                results.iter().map(|result| result.total()).sum()
            }
        }
    }

    pub fn add(&mut self, other: ModifierKindResult) {
        let current = std::mem::replace(self, ModifierKindResult::Flat(0));
        *self = match (current, other) {
            (ModifierKindResult::Flat(v), ModifierKindResult::Flat(ov)) => {
                ModifierKindResult::Flat(v + ov)
            }
            (ModifierKindResult::Composite(mut results), other_result) => {
                results.push(other_result);
                ModifierKindResult::Composite(results)
            }
            (self_result, other_result) => {
                ModifierKindResult::Composite(vec![self_result, other_result])
            }
        };
    }

    pub fn matches_kind(&self, kind: &ModifierKind) -> bool {
        compare_kind_result(kind, self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ModifierResult {
    pub results: BTreeMap<ModifierSource, ModifierKindResult>,
}

impl ModifierResult {
    pub fn total(&self) -> i32 {
        self.results.values().map(|result| result.total()).sum()
    }

    pub fn add_modifier_result(&mut self, source: ModifierSource, result: ModifierKindResult) {
        if let Some(existing) = self.results.get_mut(&source) {
            existing.add(result);
        } else {
            self.results.insert(source, result);
        }
    }

    pub fn matches_map(&self, map: &ModifierMap) -> bool {
        compare_map_result(&map.modifiers, &self.results)
    }
}

fn compare_kind_result(kind: &ModifierKind, result: &ModifierKindResult) -> bool {
    match (kind, result) {
        (ModifierKind::Flat(value), ModifierKindResult::Flat(result_value)) => {
            value == result_value
        }
        (ModifierKind::Dice(dice_set), ModifierKindResult::Dice(dice_result)) => {
            *dice_set == dice_result.dice()
        }
        (ModifierKind::Composite(kinds), ModifierKindResult::Composite(results)) => {
            if kinds.len() != results.len() {
                return false;
            }
            for (kind, result) in kinds.iter().zip(results.iter()) {
                if !compare_kind_result(kind, result) {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

fn compare_map_result(
    map: &BTreeMap<ModifierSource, ModifierKind>,
    result: &BTreeMap<ModifierSource, ModifierKindResult>,
) -> bool {
    if map.len() != result.len() {
        return false;
    }
    for (source, kind) in map {
        if let Some(result_kind) = result.get(source) {
            if !compare_kind_result(kind, result_kind) {
                return false;
            }
        } else {
            return false;
        }
    }
    true
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, JsonSchema)]
pub struct FlatModifierMap {
    pub(crate) modifiers: BTreeMap<ModifierSource, i32>,
}

impl FlatModifierMap {
    pub fn add_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<i32>,
    {
        let value = value.into();
        if value == 0 {
            return;
        }
        self.modifiers.insert(source, value);
    }

    pub fn remove_modifier(&mut self, source: &ModifierSource) -> Option<i32> {
        self.modifiers.remove(source)
    }

    pub fn replace_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<i32>,
    {
        let value = value.into();
        self.modifiers.insert(source, value);
    }

    pub fn total(&self) -> i32 {
        self.modifiers.values().sum()
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

    pub fn iter(&self) -> impl Iterator<Item = (&ModifierSource, &i32)> {
        self.modifiers.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&ModifierSource, &mut i32)> {
        self.modifiers.iter_mut()
    }

    pub fn add_modifier_map(&mut self, other: &FlatModifierMap) {
        for (source, value) in &other.modifiers {
            self.add_modifier(source.clone(), *value);
        }
    }
}

#[derive(
    Debug, Hash, Eq, PartialEq, Clone, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
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
    Species(SpeciesId),           // e.g. "Dwarf"
    Subspecies(SubspeciesId),     // e.g. "Hill Dwarf"
    None,                         // Used for cases where no modifier is applicable
    Custom(String),               // fallback for ad-hoc things
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

impl From<Id> for ModifierSource {
    fn from(id: Id) -> Self {
        match id {
            Id::ActionId(action_id) => ModifierSource::Action(action_id),
            Id::BackgroundId(background_id) => ModifierSource::Background(background_id),
            Id::ClassId(class_id) => ModifierSource::ClassFeature(class_id),
            Id::EffectId(effect_id) => ModifierSource::Effect(effect_id),
            Id::FeatId(feat_id) => ModifierSource::Feat(feat_id),
            Id::ItemId(item_id) => ModifierSource::Item(item_id),
            Id::SpeciesId(species_id) => ModifierSource::Species(species_id),
            Id::SubclassId(subclass_id) => ModifierSource::SubclassFeature(subclass_id),
            Id::SubspeciesId(subspecies_id) => ModifierSource::Subspecies(subspecies_id),
            other => ModifierSource::Custom(other.to_string()),
        }
    }
}

// TODO: Add support for dice modifiers?
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct ModifierSet {
//     modifiers: BTreeMap<ModifierSource, i32>,
// }

// impl ModifierSet {
//     pub fn new() -> Self {
//         Self {
//             modifiers: BTreeMap::new(),
//         }
//     }

//     pub fn from(source: ModifierSource, value: i32) -> Self {
//         let mut modifiers = BTreeMap::new();
//         modifiers.insert(source, value);
//         Self { modifiers }
//     }

//     pub fn from_iter<I>(iter: I) -> Self
//     where
//         I: IntoIterator<Item = (ModifierSource, i32)>,
//     {
//         let modifiers = iter.into_iter().collect();
//         Self { modifiers }
//     }

//     pub fn add_modifier_set(&mut self, other: &ModifierSet) {
//         for (source, value) in &other.modifiers {
//             let entry = self.modifiers.entry(source.clone()).or_insert(0);
//             *entry += value;
//         }
//     }

//     pub fn get(&self, source: &ModifierSource) -> Option<i32> {
//         self.modifiers.get(source).cloned()
//     }

//     pub fn contains_key(&self, source: &ModifierSource) -> bool {
//         self.modifiers.contains_key(source)
//     }

//     // Only used for ability modifiers
//     pub fn scale_modifiers(&mut self, scale: f32) {
//         for m in self.modifiers.values_mut() {
//             *m = (m.clone() as f32 * scale).round() as i32;
//         }
//     }

//     pub fn is_empty(&self) -> bool {
//         if self.modifiers.is_empty() {
//             return true;
//         }
//         for value in self.modifiers.values() {
//             if *value != 0 {
//                 return false;
//             }
//         }
//         true
//     }

//     pub fn iter(&self) -> impl Iterator<Item = (&ModifierSource, &i32)> {
//         self.modifiers.iter()
//     }
// }

pub trait KeyedModifiable<K> {
    type Result;

    fn add_modifier<T>(&mut self, key: &K, source: ModifierSource, value: T)
    where
        T: Into<ModifierKind>;
    fn remove_modifier(&mut self, key: &K, source: &ModifierSource);
    fn evaluate(&self, key: &K) -> Self::Result;
}

pub trait KeyedFlatModifiable<K> {
    fn add_modifier<T>(&mut self, key: &K, source: ModifierSource, value: T)
    where
        T: Into<i32>;
    fn remove_modifier(&mut self, key: &K, source: &ModifierSource);
    fn total(&self, key: &K) -> i32;
}

// impl fmt::Display for ModifierSet {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let mut s = String::new();
//         for i in 0..self.modifiers.len() {
//             let (source, value) = self.modifiers.iter().nth(i).unwrap();
//             if value == &0 {
//                 continue;
//             }
//             if i != 0 {
//                 s += " ";
//             }
//             let sign = if *value >= 0 { "+" } else { "-" };
//             s += &format!("{}{} ({})", sign, value.abs(), source);
//         }
//         write!(f, "{}", s)
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct Range<T> {
    pub min: T,
    pub max: T,
}

impl<T> Range<T>
where
    T: PartialOrd + PartialEq + Copy,
{
    pub fn new(min: T, max: T) -> Self {
        assert!(min <= max, "min must be less than or equal to max");
        Self { min, max }
    }

    pub fn single(value: T) -> Self {
        Self {
            min: value,
            max: value,
        }
    }

    pub fn is_single(&self) -> bool {
        self.min == self.max
    }

    pub fn contains<U>(&self, value: U) -> bool
    where
        U: Into<T> + Copy,
    {
        let value = value.into();
        value >= self.min && value <= self.max
    }

    pub fn limit_min<U>(&self, min: U) -> Self
    where
        U: Into<T> + Copy,
    {
        let min = min.into();
        let min = if min > self.min { min } else { self.min };
        Self { min, max: self.max }
    }

    pub fn limit_max<U>(&self, max: U) -> Self
    where
        U: Into<T> + Copy,
    {
        let max = max.into();
        let max = if max < self.max { max } else { self.max };
        Self { min: self.min, max }
    }

    pub fn convert<U>(&self) -> Range<U>
    where
        U: From<T> + Ord + PartialEq + Copy,
    {
        Range {
            min: U::from(self.min),
            max: U::from(self.max),
        }
    }

    pub fn add(&self, value: T) -> Self
    where
        T: std::ops::Add<Output = T>,
    {
        Self {
            min: self.min + value,
            max: self.max + value,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn modifiers_no_dice() {
        let mut modifier_map = ModifierMap {
            modifiers: BTreeMap::new(),
        };

        modifier_map.add_modifier(ModifierSource::Base, 2);
        modifier_map.add_modifier(ModifierSource::Ability(Ability::Strength), 3);
        modifier_map.add_modifier(
            ModifierSource::Item(ItemId::from("nat20_core::item.belt_of_strength")),
            4,
        );

        assert_eq!(modifier_map.min(), 9);
        assert_eq!(modifier_map.max(), 9);

        let result = modifier_map.evaluate();
        assert_eq!(result.total(), 9);
    }

    #[test]
    fn modifiers_with_dice() {
        let mut modifier_map = ModifierMap {
            modifiers: BTreeMap::new(),
        };

        modifier_map.add_modifier(ModifierSource::Base, 2);
        modifier_map.add_modifier(
            ModifierSource::Ability(Ability::Strength),
            DiceSet::from_str("1d4").unwrap(),
        );
        modifier_map.add_modifier(
            ModifierSource::Item(ItemId::from("nat20_core::item.belt_of_strength")),
            DiceSet::from_str("2d6").unwrap(),
        );

        assert_eq!(modifier_map.min(), 5);
        assert_eq!(modifier_map.max(), 18);

        let result = modifier_map.evaluate();
        let total = result.total();
        assert!(total >= 5 && total <= 18);
    }

    #[test]
    fn flat_modifiers() {
        let mut flat_modifier_map = FlatModifierMap {
            modifiers: BTreeMap::new(),
        };

        flat_modifier_map.add_modifier(ModifierSource::Base, 2);
        flat_modifier_map.add_modifier(ModifierSource::Ability(Ability::Strength), 3);
        flat_modifier_map.add_modifier(
            ModifierSource::Item(ItemId::from("nat20_core::item.belt_of_strength")),
            4,
        );

        assert_eq!(flat_modifier_map.total(), 9);

        flat_modifier_map.remove_modifier(&ModifierSource::Ability(Ability::Strength));
        assert_eq!(flat_modifier_map.total(), 6);
    }

    #[test]
    fn remove_modifier_dice() {
        let mut modifier_map = ModifierMap {
            modifiers: BTreeMap::new(),
        };

        modifier_map.add_modifier(ModifierSource::Base, 2);
        modifier_map.add_modifier(
            ModifierSource::Ability(Ability::Strength),
            DiceSet::from_str("1d4").unwrap(),
        );

        assert_eq!(modifier_map.min(), 3);
        assert_eq!(modifier_map.max(), 6);

        modifier_map.remove_modifier(&ModifierSource::Ability(Ability::Strength));
        assert_eq!(modifier_map.min(), 2);
        assert_eq!(modifier_map.max(), 2);
    }
}
