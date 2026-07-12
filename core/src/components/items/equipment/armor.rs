use std::collections::HashSet;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::Display;

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        id::EffectId,
        items::{
            equipment::slots::{EquipmentSlot, SlotProvider},
            item::Item,
        },
        modifier::{Modifiable, ModifierSet, ModifierSource},
    },
    registry::serialize::{item::ArmorDefinition, schema::impl_schema_via},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ArmorType {
    Clothing,
    Light,
    Medium,
    Heavy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ArmorDexterityBonus {
    Unlimited,   // No limit on Dexterity bonus
    Limited(u8), // Maximum Dexterity bonus allowed
}

impl ArmorDexterityBonus {
    pub fn max_bonus(&self) -> u8 {
        match self {
            ArmorDexterityBonus::Unlimited => u8::MAX,
            ArmorDexterityBonus::Limited(max) => *max,
        }
    }
}

impl Default for ArmorDexterityBonus {
    fn default() -> Self {
        ArmorDexterityBonus::Unlimited
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArmorClass {
    pub base: (i32, ModifierSource),
    pub dexterity_bonus: ArmorDexterityBonus,
    pub modifiers: ModifierSet,
}

impl ArmorClass {
    fn new(
        base_value: i32,
        base_source: ModifierSource,
        dexterity_bonus: ArmorDexterityBonus,
    ) -> Self {
        Self {
            base: (base_value, base_source),
            dexterity_bonus,
            modifiers: ModifierSet::new(),
        }
    }
}

impl Modifiable for ArmorClass {
    fn add_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<i32>,
    {
        let mut value = value.into();
        if source == ModifierSource::Ability(Ability::Dexterity) {
            // Ensure that Dexterity bonus does not exceed max dexterity bonus
            let max_dexterity_bonus = self.dexterity_bonus.max_bonus() as i32;
            if value > max_dexterity_bonus {
                value = max_dexterity_bonus;
            }
        }
        self.modifiers.add_modifier(source, value);
    }

    fn remove_modifier(&mut self, source: &ModifierSource) {
        self.modifiers.remove_modifier(source);
    }

    fn total(&self) -> i32 {
        self.base.0 + self.modifiers.total()
    }
}

impl_schema_via!(Armor, ArmorDefinition);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(from = "ArmorDefinition")]
pub struct Armor {
    pub item: Item,
    pub armor_type: ArmorType,
    pub armor_class: i32,
    pub dexterity_bonus: ArmorDexterityBonus,
    pub effects: Vec<EffectId>,
}

impl Armor {
    fn new(
        item: Item,
        armor_type: ArmorType,
        armor_class: i32,
        dexterity_bonus: ArmorDexterityBonus,
        stealth_disadvantage: bool,
        mut effects: Vec<EffectId>,
    ) -> Armor {
        if stealth_disadvantage {
            effects.push(EffectId::new(
                "nat20_core",
                "effect.item.armor_stealth_disadvantage",
            ));
        }

        Armor {
            item,
            armor_type,
            armor_class,
            dexterity_bonus,
            effects,
        }
    }

    pub fn clothing(item: Item, effects: Vec<EffectId>) -> Armor {
        Armor::new(
            item,
            ArmorType::Clothing,
            10,
            ArmorDexterityBonus::Unlimited,
            false,
            effects,
        )
    }

    pub fn light(item: Item, armor_class: i32, effects: Vec<EffectId>) -> Armor {
        Armor::new(
            item,
            ArmorType::Light,
            armor_class,
            ArmorDexterityBonus::Unlimited,
            false,
            effects,
        )
    }

    pub fn medium(
        item: Item,
        armor_class: i32,
        stealth_disadvantage: bool,
        effects: Vec<EffectId>,
    ) -> Armor {
        Armor::new(
            item,
            ArmorType::Medium,
            armor_class,
            ArmorDexterityBonus::Limited(2),
            stealth_disadvantage,
            effects,
        )
    }

    pub fn heavy(item: Item, armor_class: i32, effects: Vec<EffectId>) -> Armor {
        Armor::new(
            item,
            ArmorType::Heavy,
            armor_class,
            ArmorDexterityBonus::Limited(0),
            true,
            effects,
        )
    }

    pub fn armor_class(&self, ability_scores: &AbilityScoreMap) -> ArmorClass {
        let mut armor_class = ArmorClass::new(
            self.armor_class,
            ModifierSource::Item(self.item.id.clone()),
            self.dexterity_bonus,
        );
        let dex_bonus = ability_scores
            .get(&Ability::Dexterity)
            .ability_modifier()
            .total();
        armor_class.add_modifier(ModifierSource::Ability(Ability::Dexterity), dex_bonus);
        armor_class
    }

    pub fn effects(&self) -> &Vec<EffectId> {
        &self.effects
    }
}

impl SlotProvider for Armor {
    fn valid_slots(&self) -> &'static [EquipmentSlot] {
        &[EquipmentSlot::Armor]
    }
}

pub type ArmorTrainingSet = HashSet<ArmorType>;

#[cfg(test)]
mod tests {
    use crate::components::ability::AbilityScore;

    use super::*;

    #[test]
    fn armor_class_add_and_remove_modifier() {
        let mut armor_class =
            ArmorClass::new(10, ModifierSource::None, ArmorDexterityBonus::Limited(2));
        assert_eq!(armor_class.total(), 10);

        armor_class.add_modifier(ModifierSource::Custom("Test".to_string()), 3);
        assert_eq!(armor_class.total(), 13);

        armor_class.remove_modifier(&ModifierSource::Custom("Test".to_string()));
        assert_eq!(armor_class.total(), 10);
    }

    #[test]
    fn armor_effects_are_set_correctly() {
        let effects = vec![EffectId::new("nat20_core", "nat20_core::effect.test")];
        let armor = Armor::clothing(Item::default(), effects.clone());
        assert_eq!(armor.effects(), &effects);
    }

    #[test]
    fn armor_class_with_dexterity_bonus() {
        let mut ability_scores = AbilityScoreMap::new();
        ability_scores.set(
            Ability::Dexterity,
            AbilityScore::new(Ability::Dexterity, 16),
        ); // Modifier should be +3

        let armor = Armor::light(Item::default(), 11, vec![]);
        let armor_class = armor.armor_class(&ability_scores);

        // Should be base (11) + dex mod (3)
        assert_eq!(armor_class.total(), 14);
    }

    #[test]
    fn medium_armor_limits_dexterity_bonus() {
        let mut ability_scores = AbilityScoreMap::new();
        ability_scores.set(
            Ability::Dexterity,
            AbilityScore::new(Ability::Dexterity, 20),
        ); // Modifier should be +5

        let armor = Armor::medium(Item::default(), 14, false, vec![]);
        let armor_class = armor.armor_class(&ability_scores);

        // Should only allow max dex bonus of 2
        let dex_mod = armor_class.modifiers.total();
        assert!(dex_mod <= armor_class.dexterity_bonus.max_bonus() as i32);
    }

    #[test]
    fn heavy_armor_no_dexterity_bonus() {
        let mut ability_scores = AbilityScoreMap::new();
        ability_scores.set(
            Ability::Dexterity,
            AbilityScore::new(Ability::Dexterity, 18),
        ); // Modifier should be +4

        let armor = Armor::heavy(Item::default(), 16, vec![]);
        let armor_class = armor.armor_class(&ability_scores);

        // Should not add any dex bonus
        assert_eq!(armor_class.total(), 16);
    }
}
