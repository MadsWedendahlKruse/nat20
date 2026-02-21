use std::{collections::HashMap, sync::LazyLock};

use hecs::{Entity, World};
use serde::Deserialize;
use uom::si::f32::Length;

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::{
            action::{ActionContext, ActionMap, ActionProvider, AttackRollProvider},
            targeting::TargetingRange,
        },
        d20::AdvantageType,
        damage::{AttackRoll, AttackRollResult, DamageRoll},
        id::{EffectId, ItemId},
        items::{
            equipment::{
                armor::{Armor, ArmorClass, ArmorDexterityBonus},
                equipment::EquipmentItem,
                slots::{EquipmentSlot, SlotProvider},
                weapon::{
                    MELEE_RANGE_DEFAULT, Weapon, WeaponKind, WeaponProficiencyMap, WeaponProperties,
                },
            },
            inventory::{ItemContainer, ItemInstance},
            item::Item,
        },
        modifier::{Modifiable, ModifierSet, ModifierSource},
    },
    registry::registry::ItemsRegistry,
    systems::{self},
};

#[derive(Debug, Clone, PartialEq)]
pub enum TryEquipError {
    InvalidSlot {
        slot: EquipmentSlot,
        equipment: EquipmentInstance,
    },
    SlotOccupied,
    NotProficient,
    WrongWeaponType,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(untagged)]
pub enum EquipmentInstance {
    Armor(Armor),
    Weapon(Weapon),
    Equipment(EquipmentItem),
}

impl EquipmentInstance {
    pub fn effects(&self) -> &Vec<EffectId> {
        match self {
            EquipmentInstance::Armor(armor) => armor.effects(),
            EquipmentInstance::Weapon(weapon) => weapon.effects(),
            EquipmentInstance::Equipment(equipment) => &equipment.effects,
        }
    }
}

impl SlotProvider for EquipmentInstance {
    fn valid_slots(&self) -> &'static [EquipmentSlot] {
        match self {
            EquipmentInstance::Armor(armor) => armor.valid_slots(),
            EquipmentInstance::Weapon(weapon) => weapon.valid_slots(),
            EquipmentInstance::Equipment(equipment) => equipment.valid_slots(),
        }
    }

    fn required_slots(&self) -> &'static [EquipmentSlot] {
        match self {
            EquipmentInstance::Weapon(weapon) => weapon.required_slots(),
            _ => &[],
        }
    }
}

impl ItemContainer for EquipmentInstance {
    fn item(&self) -> &Item {
        match self {
            EquipmentInstance::Armor(armor) => &armor.item,
            EquipmentInstance::Weapon(weapon) => weapon.item(),
            EquipmentInstance::Equipment(equipment) => &equipment.item,
        }
    }
}

macro_rules! impl_into_equipment_instance {
    ($($ty:ty => $variant:ident),* $(,)?) => {
        $(
            impl Into<EquipmentInstance> for $ty {
                fn into(self) -> EquipmentInstance {
                    EquipmentInstance::$variant(self)
                }
            }
        )*
    };
}

impl_into_equipment_instance! {
    Armor => Armor,
    Weapon => Weapon,
    EquipmentItem => Equipment,
}

impl Into<EquipmentInstance> for &LazyLock<ItemId> {
    fn into(self) -> EquipmentInstance {
        let item = ItemsRegistry::get(&self).expect("Invalid ItemId");
        match item {
            ItemInstance::Armor(armor) => EquipmentInstance::Armor(armor.clone()),
            ItemInstance::Weapon(weapon) => EquipmentInstance::Weapon(weapon.clone()),
            ItemInstance::Equipment(equipment) => EquipmentInstance::Equipment(equipment.clone()),
            _ => panic!("ItemId does not correspond to an equipment item"),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Loadout {
    equipment: HashMap<EquipmentSlot, EquipmentInstance>,
}

impl Loadout {
    pub fn new() -> Self {
        Self {
            equipment: HashMap::new(),
        }
    }

    pub fn item_in_slot(&self, slot: &EquipmentSlot) -> Option<&EquipmentInstance> {
        self.equipment.get(slot)
    }

    pub fn unequip(&mut self, slot: &EquipmentSlot) -> Option<EquipmentInstance> {
        self.equipment.remove(slot)
    }

    pub fn unequip_slots(&mut self, slots: &[EquipmentSlot]) -> Vec<EquipmentInstance> {
        slots.iter().filter_map(|slot| self.unequip(slot)).collect()
    }

    pub fn equip_in_slot<T>(
        &mut self,
        slot: &EquipmentSlot,
        equipment: T,
    ) -> Result<Vec<EquipmentInstance>, TryEquipError>
    where
        T: Into<EquipmentInstance>,
    {
        let equipment = equipment.into();
        if !equipment.valid_slots().contains(slot) {
            return Err(TryEquipError::InvalidSlot {
                slot: *slot,
                equipment,
            });
        }
        let mut unequipped_items = self.unequip_slots(&equipment.required_slots());
        if let Some(existing) = self.equipment.insert(*slot, equipment) {
            unequipped_items.push(existing);
        }
        Ok(unequipped_items)
    }

    pub fn can_equip(&self, equipment: &EquipmentInstance) -> bool {
        if !equipment
            .valid_slots()
            .iter()
            .any(|s| self.item_in_slot(s).is_none())
        {
            return false;
        }
        for slot in equipment.required_slots() {
            if self.item_in_slot(slot).is_some() {
                return false;
            }
        }
        for (_, equipped) in &self.equipment {
            if equipped
                .required_slots()
                .iter()
                .any(|s| equipment.valid_slots().contains(s))
            {
                return false;
            }
        }
        true
    }

    pub fn find_slot_for_item(
        &mut self,
        equipment: &EquipmentInstance,
    ) -> (EquipmentSlot, Vec<EquipmentInstance>) {
        let valid_slots = equipment.valid_slots();

        // Make sure none of the other equipment "require" this slot. This is mainly
        // for weapons that might require both hands.
        let should_unequip = self
            .equipment
            .iter()
            .filter_map(|(slot, equipped)| {
                // Unequip the item in this slot if it conflicts with the new equipment
                if equipped
                    .required_slots()
                    .iter()
                    .any(|s| valid_slots.contains(s))
                {
                    Some(slot.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        // Unequip any conflicting items
        let unequipped_items = should_unequip
            .iter()
            .filter_map(|slot| self.unequip(slot))
            .collect::<Vec<_>>();

        // If there's only one valid slot, use that
        if valid_slots.len() == 1 {
            return (valid_slots[0].clone(), unequipped_items);
        }
        // If there are multiple valid slots, find an available one
        let mut avaible_slot = valid_slots
            .iter()
            .find(|slot| self.item_in_slot(slot).is_none());
        if avaible_slot.is_none() {
            // If no available slot, just use the first valid slot
            // This is a fallback and may not be ideal, but it ensures we have a slot
            avaible_slot = Some(&valid_slots[0]);
        }

        (avaible_slot.unwrap().clone(), unequipped_items)
    }

    pub fn equip<T>(&mut self, equipment: T) -> Result<Vec<EquipmentInstance>, TryEquipError>
    where
        T: Into<EquipmentInstance>,
    {
        let equipment = equipment.into();
        let (slot, mut unequipped) = self.find_slot_for_item(&equipment);
        unequipped.extend(self.equip_in_slot(&slot, equipment)?);
        Ok(unequipped)
    }

    pub fn armor(&self) -> Option<&Armor> {
        if let Some(EquipmentInstance::Armor(armor)) = self.equipment.get(&EquipmentSlot::Armor) {
            Some(armor)
        } else {
            None
        }
    }

    pub fn armor_class(&self, world: &World, entity: Entity) -> ArmorClass {
        if let Some(armor) = &self.armor() {
            let ability_scores = systems::helpers::get_component::<AbilityScoreMap>(world, entity);
            let mut armor_class = armor.armor_class(&ability_scores);
            for effect in systems::effects::effects(world, entity).values() {
                (effect.effect().on_armor_class)(world, entity, &mut armor_class);
            }
            armor_class
        } else {
            // TODO: Not sure if this is the right way to handle unarmored characters
            ArmorClass {
                base: (10, ModifierSource::Base),
                dexterity_bonus: ArmorDexterityBonus::Unlimited,
                modifiers: ModifierSet::new(),
            }
        }
    }

    pub fn weapon_in_hand(&self, slot: &EquipmentSlot) -> Option<&Weapon> {
        if !slot.is_weapon_slot() {
            return None;
        }
        if let Some(EquipmentInstance::Weapon(weapon)) = self.item_in_slot(slot) {
            Some(weapon)
        } else {
            None
        }
    }

    pub fn has_weapon_in_hand(&self, slot: &EquipmentSlot) -> bool {
        self.weapon_in_hand(slot).is_some()
    }

    pub fn is_wielding_weapon_with_both_hands(&self, weapon_kind: &WeaponKind) -> bool {
        let (main_hand_slot, off_hand_slot) = match weapon_kind {
            WeaponKind::Melee => (EquipmentSlot::MeleeMainHand, EquipmentSlot::MeleeOffHand),
            WeaponKind::Ranged => (EquipmentSlot::RangedMainHand, EquipmentSlot::RangedOffHand),
        };
        if let Some(main_hand_weapon) = self.weapon_in_hand(&main_hand_slot) {
            // Check that:
            // 1. The main hand weapon is two-handed or versatile.
            // 2. The off hand is empty
            return (main_hand_weapon.has_property(&WeaponProperties::TwoHanded)
                || main_hand_weapon
                    .properties()
                    .iter()
                    .any(|p| matches!(p, WeaponProperties::Versatile(_))))
                && !self.has_weapon_in_hand(&off_hand_slot);
        }
        false
    }

    pub fn damage_roll(&self, world: &World, entity: Entity, slot: &EquipmentSlot) -> DamageRoll {
        let weapon = self
            .weapon_in_hand(slot)
            .expect("No weapon equipped in the specified slot");
        weapon.damage_roll(
            &systems::helpers::get_component::<AbilityScoreMap>(world, entity),
            self.is_wielding_weapon_with_both_hands(weapon.kind()),
        )
    }

    pub fn melee_range(&self) -> &TargetingRange {
        if self.has_weapon_in_hand(&EquipmentSlot::MeleeMainHand) {
            self.weapon_in_hand(&EquipmentSlot::MeleeMainHand)
                .unwrap()
                .range()
        } else {
            &MELEE_RANGE_DEFAULT
        }
    }
}

impl AttackRollProvider for Loadout {
    fn attack_roll(
        &self,
        world: &World,
        performer: Entity,
        target: Entity,
        context: &ActionContext,
    ) -> AttackRoll {
        let slot = match context {
            ActionContext::Weapon { slot } => slot,
            _ => panic!("Action context must be Weapon"),
        };

        // TODO: Unarmed attacks
        let weapon = self
            .weapon_in_hand(slot)
            .expect("No weapon equipped in the specified slot");
        let mut attack_roll = weapon.attack_roll(
            &systems::helpers::get_component::<AbilityScoreMap>(world, performer),
            &systems::helpers::get_component::<WeaponProficiencyMap>(world, performer)
                .proficiency(&weapon.category()),
        );

        let range = weapon.range();
        if range.normal() < range.max() {
            let distance =
                systems::geometry::distance_between_entities(world, performer, target).unwrap();
            if distance > range.normal() {
                attack_roll.d20_check.advantage_tracker_mut().add(
                    AdvantageType::Disadvantage,
                    ModifierSource::Custom("Target is outside normal range".to_string()),
                );
            }
        }

        attack_roll
    }
}

impl ActionProvider for Loadout {
    fn actions(&self, world: &World, entity: Entity) -> ActionMap {
        let mut actions = ActionMap::new();

        // TODO: There has to be a nicer way to do this
        for slot in EquipmentSlot::weapon_slots() {
            if let Some(weapon) = self.weapon_in_hand(slot) {
                let weapon_actions = weapon.weapon_actions();
                for action_id in weapon_actions {
                    if let Some(action) = systems::actions::get_action(&action_id) {
                        let context = ActionContext::Weapon { slot: slot.clone() };
                        let resource_cost = &action.resource_cost().clone();
                        actions
                            .entry(action_id.clone())
                            .and_modify(|entry| {
                                entry.push((context.clone(), resource_cost.clone()));
                            })
                            .or_insert(vec![(context, resource_cost.clone())]);
                    }
                }
            }
        }

        actions
    }
}

#[cfg(test)]
mod tests {
    use crate::components::id::ActionId;
    use crate::test_utils::fixtures;

    use super::*;

    #[test]
    fn empty_loadout() {
        let loadout = Loadout::new();
        assert!(loadout.armor().is_none());
        assert!(loadout.equipment.is_empty());
    }

    #[test]
    fn equip_unequip_armor() {
        let mut loadout = Loadout::new();

        let armor = ItemsRegistry::get(&ItemId::new("nat20_core", "item.chainmail"))
            .unwrap()
            .clone();
        let slot = EquipmentSlot::Armor;
        let unequipped = loadout.equip_in_slot(&slot, armor);
        assert!(unequipped.unwrap().is_empty());
        assert_eq!(
            loadout.armor().unwrap().item.id,
            ItemId::new("nat20_core", "item.chainmail")
        );

        let unequipped = loadout.unequip(&slot);
        assert_eq!(
            unequipped.unwrap().item().id,
            ItemId::new("nat20_core", "item.chainmail")
        );
        assert!(loadout.armor().is_none());

        let unequipped = loadout.unequip(&slot);
        assert!(unequipped.is_none());
        assert!(loadout.armor().is_none());
    }

    #[test]
    fn equip_armor_twice() {
        let mut loadout = Loadout::new();

        let armor1 = ItemsRegistry::get(&ItemId::new("nat20_core", "item.chainmail"))
            .unwrap()
            .clone();
        let slot = EquipmentSlot::Armor;
        let unequipped1 = loadout.equip_in_slot(&slot, armor1.clone());
        assert!(unequipped1.unwrap().is_empty());
        assert_eq!(
            loadout.armor().unwrap().item.id,
            ItemId::new("nat20_core", "item.chainmail")
        );
        let armor2 = ItemsRegistry::get(&ItemId::new("nat20_core", "item.studded_leather_armor"))
            .unwrap()
            .clone();
        let unequipped2 = loadout.equip_in_slot(&slot, armor2.clone());
        assert!(
            unequipped2
                .unwrap()
                .iter()
                .any(|item| item.item().id == ItemId::new("nat20_core", "item.chainmail"))
        );
        assert_eq!(loadout.armor().unwrap().item.id, armor2.item().id);
    }

    #[test]
    fn equip_unequip_item() {
        let mut loadout = Loadout::new();

        let item = fixtures::equipment::boots();
        let slot = item.valid_slots()[0].clone();
        let unequipped = loadout.equip_in_slot(&slot, EquipmentInstance::Equipment(item.clone()));
        assert!(unequipped.unwrap().is_empty());
        assert!(loadout.item_in_slot(&slot).is_some());

        let unequipped = loadout.unequip(&slot);
        assert_eq!(unequipped, Some(EquipmentInstance::Equipment(item.clone())));
        assert!(loadout.item_in_slot(&slot).is_none());

        let unequipped = loadout.unequip(&slot);
        assert!(unequipped.is_none());
        assert!(loadout.item_in_slot(&slot).is_none());
    }

    #[test]
    fn equip_item_twice() {
        let mut loadout = Loadout::new();

        let item1 = fixtures::equipment::boots();
        let slot = EquipmentSlot::Boots;
        let unequipped1 = loadout.equip_in_slot(&slot, EquipmentInstance::Equipment(item1.clone()));
        assert!(unequipped1.unwrap().is_empty());
        assert!(loadout.item_in_slot(&slot).is_some());

        let item2 = fixtures::equipment::boots();
        let unequipped2 = loadout.equip_in_slot(&slot, EquipmentInstance::Equipment(item2.clone()));
        assert!(
            unequipped2
                .unwrap()
                .contains(&EquipmentInstance::Equipment(item1))
        );
        assert!(loadout.item_in_slot(&slot).is_some());
    }

    #[test]
    fn equip_unequip_weapon() {
        let mut loadout = Loadout::new();

        let weapon: EquipmentInstance =
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
                .unwrap()
                .clone()
                .into();
        let slot = weapon.valid_slots()[0];
        let unequipped = loadout.equip_in_slot(&slot, weapon);
        assert!(unequipped.is_ok());
        assert!(loadout.weapon_in_hand(&slot).is_some());

        let unequipped = loadout.unequip(&slot);
        assert!(unequipped.is_some());
        assert!(loadout.weapon_in_hand(&slot).is_none());
    }

    #[test]
    fn equip_weapon_twice() {
        let mut loadout = Loadout::new();

        let weapon1: EquipmentInstance =
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
                .unwrap()
                .clone()
                .into();
        let slot = weapon1.valid_slots()[0];
        let unequipped1 = loadout.equip_in_slot(&slot, weapon1);
        assert_eq!(unequipped1.unwrap().len(), 0);
        assert!(loadout.weapon_in_hand(&slot).is_some());

        let weapon2: EquipmentInstance =
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
                .unwrap()
                .clone()
                .into();
        let unequipped2 = loadout.equip_in_slot(&slot, weapon2);
        assert_eq!(unequipped2.unwrap().len(), 1);
        assert!(loadout.weapon_in_hand(&slot).is_some());
    }

    #[test]
    fn equip_two_handed_weapon_should_unequip_other_hand() {
        let mut loadout = Loadout::new();

        let weapon_main_hand = ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
            .unwrap()
            .clone();
        let weapon_off_hand = ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
            .unwrap()
            .clone();
        let main_slot = EquipmentSlot::MeleeMainHand;
        let off_slot = EquipmentSlot::MeleeOffHand;

        let unequipped_main = loadout.equip_in_slot(&main_slot, weapon_main_hand);
        assert!(unequipped_main.is_ok());
        assert!(loadout.weapon_in_hand(&main_slot).is_some());

        let unequipped_off = loadout.equip_in_slot(&off_slot, weapon_off_hand);
        assert!(unequipped_off.is_ok());
        assert!(loadout.weapon_in_hand(&off_slot).is_some());

        let weapon_two_handed = ItemsRegistry::get(&ItemId::new("nat20_core", "item.greatsword"))
            .unwrap()
            .clone();
        let unequipped = loadout.equip_in_slot(&main_slot, weapon_two_handed);
        println!("{:?}", unequipped);
        assert!(unequipped.is_ok());
        // Should unequip both hands if required_slots includes both
        assert!(loadout.weapon_in_hand(&main_slot).is_some());
        assert!(loadout.weapon_in_hand(&off_slot).is_none());
    }

    #[test]
    fn equip_in_wrong_slot() {
        let mut loadout = Loadout::new();

        let item = fixtures::equipment::boots();
        // Try to equip boots in the Headwear slot, which should be invalid
        let slot = EquipmentSlot::Headwear;
        let result = loadout.equip_in_slot(&slot, EquipmentInstance::Equipment(item.clone()));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            TryEquipError::InvalidSlot {
                slot,
                equipment: EquipmentInstance::Equipment(item),
            }
        );
    }

    #[test]
    fn available_actions_no_weapons() {
        let mut world = World::new();
        let entity = world.spawn(());

        // TODO: Should return unarmed attack
        let loadout = Loadout::new();
        let actions = loadout.actions(&world, entity);
        assert_eq!(actions.len(), 0);
    }

    #[test]
    fn available_actions_melee_and_ranged_weapon() {
        let mut world = World::new();
        let entity = world.spawn(());

        let mut loadout = Loadout::new();

        let weapon1 = ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
            .unwrap()
            .clone();
        loadout.equip(weapon1);

        let weapon2 = ItemsRegistry::get(&ItemId::new("nat20_core", "item.shortbow"))
            .unwrap()
            .clone();
        loadout.equip(weapon2);

        let actions = loadout.actions(&world, entity);
        for action in &actions {
            println!("{:?}", action);
        }

        // Both melee and ranged attacks use the same ActionId, but their
        // contexts are different
        assert_eq!(actions.len(), 1);
        assert_eq!(
            actions[&ActionId::new("nat20_core", "action.weapon_attack")].len(),
            2
        );
        for (_, data) in actions {
            for (context, ..) in data {
                match context {
                    ActionContext::Weapon { .. } => {}
                    _ => panic!("Unexpected action context: {:?}", context),
                }
            }
        }
    }
}
