use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::{
    components::{
        damage::DamageType,
        dice::DiceSet,
        id::{ActionId, EffectId},
        items::{
            equipment::{
                armor::Armor,
                equipment::EquipmentItem,
                weapon::{Weapon, WeaponCategory, WeaponKind, WeaponProperties},
            },
            inventory::ItemInstance,
            item::Item,
        },
    },
    registry::registry_validation::{
        ReferenceCollector, RegistryReference, RegistryReferenceCollector,
    },
};

#[derive(Serialize, Deserialize)]
pub struct WeaponDefinition {
    pub item: Item,
    pub kind: WeaponKind,
    pub category: WeaponCategory,
    pub properties: HashSet<WeaponProperties>,
    pub damage: Vec<(DiceSet, DamageType)>,
    pub extra_weapon_actions: Vec<ActionId>,
    pub effects: Vec<EffectId>,
}

impl From<WeaponDefinition> for Weapon {
    fn from(def: WeaponDefinition) -> Self {
        Weapon::new(
            def.item,
            def.kind,
            def.category,
            def.properties,
            def.damage,
            def.extra_weapon_actions,
            def.effects,
        )
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
