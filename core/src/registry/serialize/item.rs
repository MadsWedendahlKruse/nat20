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
                armor::{Armor, ArmorDexterityBonus, ArmorType},
                equipment::EquipmentItem,
                weapon::{Weapon, WeaponCategory, WeaponKind, WeaponProperties},
            },
            inventory::ItemInstance,
            item::{Item, ItemRarity},
            money::MonetaryValue,
        },
    },
    registry::{
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
        serialize::quantity::MassExpressionDefinition,
    },
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

impl From<ArmorDefinition> for Armor {
    fn from(def: ArmorDefinition) -> Self {
        Armor {
            item: def.item.into(),
            armor_type: def.armor_type,
            armor_class: def.armor_class,
            dexterity_bonus: def.dexterity_bonus,
            effects: def.effects,
        }
    }
}

impl RegistryReferenceCollector for Armor {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for effect in self.effects() {
            collector.add(RegistryReference::Effect(effect.clone()));
        }
    }
}

impl RegistryReferenceCollector for Weapon {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for action in self.extra_actions() {
            collector.add(RegistryReference::Action(action.clone()));
        }
        for effect in self.effects() {
            collector.add(RegistryReference::Effect(effect.clone()));
        }
    }
}

impl RegistryReferenceCollector for EquipmentItem {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for effect in &self.effects {
            collector.add(RegistryReference::Effect(effect.clone()));
        }
    }
}

impl RegistryReferenceCollector for ItemInstance {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        match self {
            ItemInstance::Item(_) => { /* No references to collect */ }
            ItemInstance::Armor(armor) => {
                armor.collect_registry_references(collector);
            }
            ItemInstance::Weapon(weapon) => {
                weapon.collect_registry_references(collector);
            }
            ItemInstance::Equipment(equipment_item) => {
                equipment_item.collect_registry_references(collector);
            }
        }
    }
}
