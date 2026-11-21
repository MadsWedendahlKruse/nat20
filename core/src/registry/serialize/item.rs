use std::collections::HashSet;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        damage::DamageType,
        dice::DiceSet,
        id::{ActionId, EffectId, ItemId},
        items::{
            equipment::{
                armor::{Armor, ArmorDexterityBonus, ArmorType, Shield},
                weapon::{Weapon, WeaponCategory, WeaponKind, WeaponProperties},
            },
            item::{Item, ItemRarity},
            money::MonetaryValue,
        },
    },
    registry::serialize::quantity::MassExpressionDefinition,
};

#[derive(Serialize, Deserialize, JsonSchema)]
pub struct ItemDefinition {
    pub id: ItemId,
    pub name: String,
    pub description: String,
    pub weight: MassExpressionDefinition,
    pub value: MonetaryValue,
    pub rarity: ItemRarity,
}

impl From<ItemDefinition> for Item {
    fn from(def: ItemDefinition) -> Self {
        Item {
            id: def.id,
            name: def.name,
            description: def.description,
            weight: def
                .weight
                .evaluate_without_variables()
                .expect("Failed to evaluate weight expression"),
            value: def.value,
            rarity: def.rarity,
        }
    }
}

#[derive(Serialize, Deserialize, JsonSchema)]
pub struct WeaponDefinition {
    pub item: ItemDefinition,
    pub kind: WeaponKind,
    pub category: WeaponCategory,
    pub properties: HashSet<WeaponProperties>,
    pub damage: Vec<(DiceSet, DamageType)>,
    #[serde(default)]
    pub extra_weapon_actions: Vec<ActionId>,
    #[serde(default)]
    pub effects: Vec<EffectId>,
}

impl From<WeaponDefinition> for Weapon {
    fn from(def: WeaponDefinition) -> Self {
        Weapon::new(
            def.item.into(),
            def.kind,
            def.category,
            def.properties,
            def.damage,
            def.extra_weapon_actions,
            def.effects,
        )
    }
}

#[derive(Serialize, Deserialize, JsonSchema)]
pub struct ArmorDefinition {
    pub item: ItemDefinition,
    pub armor_type: ArmorType,
    pub armor_class: i32,
    pub dexterity_bonus: ArmorDexterityBonus,
    #[serde(default)]
    pub effects: Vec<EffectId>,
}

impl TryFrom<ArmorDefinition> for Armor {
    type Error = String;

    fn try_from(def: ArmorDefinition) -> Result<Self, Self::Error> {
        if def.armor_type == ArmorType::Shield {
            return Err(format!(
                "Armor {} has armor_type 'shield'; shields are their own item type \
                 (`armor_class_bonus` instead of `armor_type`/`armor_class`/`dexterity_bonus`)",
                def.item.id
            ));
        }

        Ok(Armor {
            item: def.item.into(),
            armor_type: def.armor_type,
            armor_class: def.armor_class,
            dexterity_bonus: def.dexterity_bonus,
            effects: def.effects,
        })
    }
}

#[derive(Serialize, Deserialize, JsonSchema)]
pub struct ShieldDefinition {
    pub item: ItemDefinition,
    pub armor_class_bonus: i32,
    #[serde(default)]
    pub effects: Vec<EffectId>,
}

impl From<ShieldDefinition> for Shield {
    fn from(def: ShieldDefinition) -> Self {
        Shield {
            item: def.item.into(),
            armor_class_bonus: def.armor_class_bonus,
            effects: def.effects,
        }
    }
}
