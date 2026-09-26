use std::{collections::HashMap, sync::LazyLock};

use hecs::{Entity, World};

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        actions::{
            action::{
                ActionAttackContext, ActionAttackKind, ActionContext, ActionMap, ActionProvider,
                AttackRollProvider, SavingThrowProvider,
            },
            targeting::TargetingRange,
        },
        d20::{AdvantageType, D20Check, D20CheckDC, D20CheckKind, D20CheckMap},
        damage::{AttackSource, DamageRoll, DamageType},
        id::{ActionId, ItemId},
        items::{
            equipment::{
                armor::{Armor, ArmorClass, ArmorDexterityBonus},
                slots::{EquipmentSlot, SlotProvider},
                weapon::{
                    MELEE_RANGE_DEFAULT, Weapon, WeaponKind, WeaponProficiencyMap, WeaponProperties,
                },
            },
            inventory::ItemInstance,
        },
        modifier::{Modifiable, ModifierMap, ModifierSource},
        proficiency::{Proficiency, ProficiencyLevel},
        saving_throw::SavingThrowKind,
    },
    engine::engine_state::EngineState,
    registry::registry::ItemsRegistry,
    systems::{self},
};

// TODO: Probably shouldn't hardcode these :)
static ATTACK_ACTIONS: LazyLock<Vec<ActionId>> = LazyLock::new(|| {
    vec![
        ActionId::new("nat20_core", "action.melee_attack"),
        ActionId::new("nat20_core", "action.ranged_attack"),
        ActionId::new("nat20_core", "action.opportunity_attack"),
        ActionId::new("nat20_core", "action.unarmed_attack"),
    ]
});

#[derive(Debug, Clone, PartialEq)]
pub enum TryEquipError {
    ItemDoesNotExist { item: ItemId },
    InvalidSlot { slot: EquipmentSlot, item: ItemId },
    SlotOccupied,
    NotProficient,
    WrongWeaponType,
    NoSlotAvailable,
}

#[derive(Debug, Clone)]
pub struct Loadout {
    equipment: HashMap<EquipmentSlot, ItemId>,
    /// Persistent per-weapon-kind attack roll checks, same structure as
    /// `SkillSet`/`SavingThrowSet`. The weapon-specific parts (ability
    /// modifier, enchantment, proficiency) are merged in at roll time.
    attack_rolls: D20CheckMap<WeaponKind>,
    saving_throw_modifiers: HashMap<WeaponKind, ModifierMap>,
}

impl Loadout {
    pub fn new() -> Self {
        Self {
            equipment: HashMap::new(),
            attack_rolls: D20CheckMap::new(|kind| {
                D20CheckKind::AttackRoll(AttackSource::Weapon(*kind))
            }),
            saving_throw_modifiers: HashMap::new(),
        }
    }

    pub fn attack_roll_template(&self, weapon_kind: &WeaponKind) -> &D20Check {
        self.attack_rolls.get(weapon_kind)
    }

    pub fn attack_roll_template_mut(&mut self, weapon_kind: &WeaponKind) -> &mut D20Check {
        self.attack_rolls.get_mut(weapon_kind)
    }

    pub fn saving_throw_modifiers_mut(&mut self, weapon_kind: &WeaponKind) -> &mut ModifierMap {
        self.saving_throw_modifiers.entry(*weapon_kind).or_default()
    }

    pub fn item_in_slot(&self, slot: &EquipmentSlot) -> Option<&ItemId> {
        self.equipment.get(slot)
    }

    pub fn unequip(&mut self, slot: &EquipmentSlot) -> Option<ItemId> {
        self.equipment.remove(slot)
    }

    pub fn unequip_slots(&mut self, slots: &[EquipmentSlot]) -> Vec<ItemId> {
        slots.iter().filter_map(|slot| self.unequip(slot)).collect()
    }

    pub fn equip_in_slot(
        &mut self,
        slot: &EquipmentSlot,
        item_id: &ItemId,
    ) -> Result<Vec<ItemId>, TryEquipError> {
        let Some(item) = ItemsRegistry::get(&item_id) else {
            return Err(TryEquipError::ItemDoesNotExist {
                item: item_id.clone(),
            });
        };

        if !item.valid_slots().contains(slot) {
            return Err(TryEquipError::InvalidSlot {
                slot: *slot,
                item: item_id.clone(),
            });
        }

        let mut unequipped_items = self.unequip_slots(item.required_slots());
        if let Some(existing) = self.equipment.insert(*slot, item_id.clone()) {
            unequipped_items.push(existing);
        }
        Ok(unequipped_items)
    }

    pub fn can_equip(&self, item: &ItemId) -> bool {
        let Some(item) = ItemsRegistry::get(item) else {
            return false;
        };

        if !item.is_equippable() {
            return false;
        }

        if !item
            .valid_slots()
            .iter()
            .any(|s| self.item_in_slot(s).is_none())
        {
            return false;
        }

        for slot in item.required_slots() {
            if self.item_in_slot(slot).is_some() {
                return false;
            }
        }

        for equipped in self.equipment.values() {
            if let Some(equipped) = ItemsRegistry::get(equipped)
                && equipped
                    .required_slots()
                    .iter()
                    .any(|s| item.valid_slots().contains(s))
            {
                return false;
            }
        }
        true
    }

    pub fn find_slot_for_item(&mut self, item: &ItemId) -> Option<(EquipmentSlot, Vec<ItemId>)> {
        let Some(item) = ItemsRegistry::get(item) else {
            return None;
        };

        if !item.is_equippable() {
            return None;
        }

        let valid_slots = item.valid_slots();

        // Make sure none of the other equipment "require" this slot. This is mainly
        // for weapons that might require both hands.
        let should_unequip = self
            .equipment
            .iter()
            .filter_map(|(slot, equipped)| {
                // Unequip the item in this slot if it conflicts with the new equipment
                if let Some(equipped) = ItemsRegistry::get(equipped)
                    && equipped
                        .required_slots()
                        .iter()
                        .any(|s| valid_slots.contains(s))
                {
                    Some(*slot)
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
            return Some((valid_slots[0], unequipped_items));
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

        Some((*avaible_slot.unwrap(), unequipped_items))
    }

    pub fn equip(&mut self, item_id: &ItemId) -> Result<Vec<ItemId>, TryEquipError> {
        let Some((slot, mut unequipped)) = self.find_slot_for_item(item_id) else {
            return Err(TryEquipError::NoSlotAvailable);
        };

        unequipped.extend(self.equip_in_slot(&slot, item_id)?);
        Ok(unequipped)
    }

    pub fn armor(&self) -> Option<&Armor> {
        if let Some(armor_id) = self.equipment.get(&EquipmentSlot::Armor)
            && let Some(armor) = ItemsRegistry::get(armor_id)
            && let ItemInstance::Armor(armor) = armor
        {
            Some(armor)
        } else {
            None
        }
    }

    pub fn armor_class(&self, engine_state: &EngineState, entity: Entity) -> ArmorClass {
        let mut armor_class = if let Some(armor) = &self.armor() {
            let ability_scores =
                systems::helpers::get_component::<AbilityScoreMap>(&engine_state.world, entity);
            armor.armor_class(&ability_scores)
        } else {
            // TODO: Not sure if this is the right way to handle unarmored characters
            ArmorClass::new(10, ModifierSource::Base, ArmorDexterityBonus::Unlimited)
        };

        systems::effects::effects(&engine_state.world, entity).armor_class(
            engine_state,
            entity,
            &mut armor_class,
        );

        armor_class
    }

    pub fn weapon_in_hand(&self, slot: &EquipmentSlot) -> Option<&Weapon> {
        if slot.is_weapon_slot()
            && let Some(weapon_id) = self.item_in_slot(slot)
            && let Some(weapon) = ItemsRegistry::get(weapon_id)
            && let ItemInstance::Weapon(weapon) = weapon
        {
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
            WeaponKind::Unarmed => return false,
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

    pub fn weapon_damage_roll(
        &self,
        world: &World,
        entity: Entity,
        slot: &EquipmentSlot,
    ) -> DamageRoll {
        let weapon = self
            .weapon_in_hand(slot)
            .expect("No weapon equipped in the specified slot");
        weapon.damage_roll(
            &systems::helpers::get_component::<AbilityScoreMap>(world, entity),
            self.is_wielding_weapon_with_both_hands(weapon.kind()),
        )
    }

    pub fn unarmed_damage_roll(&self, world: &World, entity: Entity) -> DamageRoll {
        let strength_modifier = systems::helpers::get_component::<AbilityScoreMap>(world, entity)
            .ability_modifier(&Ability::Strength)
            .total();

        DamageRoll::new(
            ModifierMap::from_iter(vec![
                (ModifierSource::Base, 1),
                (
                    ModifierSource::Ability(Ability::Strength),
                    strength_modifier,
                ),
            ]),
            DamageType::Bludgeoning,
        )
    }

    pub fn damage_roll_from_context(
        &self,
        world: &World,
        entity: Entity,
        context: &ActionContext,
    ) -> DamageRoll {
        let attack = context
            .attack
            .as_ref()
            .expect("Action context must contain attack metadata");

        match attack.kind {
            ActionAttackKind::MeleeWeapon | ActionAttackKind::RangedWeapon => self
                .weapon_damage_roll(
                    world,
                    entity,
                    &attack
                        .slot
                        .expect("Weapon attacks require an equipment slot"),
                ),
            ActionAttackKind::Unarmed => self.unarmed_damage_roll(world, entity),
        }
    }

    pub fn melee_range(&self) -> TargetingRange {
        let mut melee_range = MELEE_RANGE_DEFAULT.clone();
        for slot in [EquipmentSlot::MeleeMainHand, EquipmentSlot::MeleeOffHand] {
            if let Some(weapon) = self.weapon_in_hand(&slot)
                && weapon.range().max() > melee_range.max()
            {
                melee_range = weapon.range().clone();
            }
        }
        melee_range
    }

    pub fn is_valid_context(&self, context: &ActionAttackContext) -> bool {
        match context.kind {
            ActionAttackKind::MeleeWeapon | ActionAttackKind::RangedWeapon => {
                if let Some(slot) = context.slot.as_ref() {
                    self.weapon_in_hand(slot).is_some()
                } else {
                    false
                }
            }
            ActionAttackKind::Unarmed => true,
        }
    }
}

impl Default for Loadout {
    fn default() -> Self {
        Self::new()
    }
}

impl AttackRollProvider for Loadout {
    fn attack_roll(
        &self,
        world: &World,
        actor: Entity,
        target: Entity,
        context: &ActionContext,
    ) -> (AttackSource, D20Check) {
        let attack_context = context
            .attack
            .as_ref()
            .expect("Action context must contain attack metadata");

        match attack_context.kind {
            ActionAttackKind::MeleeWeapon | ActionAttackKind::RangedWeapon => {
                let slot = attack_context
                    .slot
                    .as_ref()
                    .expect("Weapon attacks require an equipment slot");

                let weapon = self
                    .weapon_in_hand(slot)
                    .expect("No weapon equipped in the specified slot");
                let mut attack_roll = weapon.attack_roll(
                    &systems::helpers::get_component::<AbilityScoreMap>(world, actor),
                    &systems::helpers::get_component::<WeaponProficiencyMap>(world, actor)
                        .proficiency(weapon.category()),
                );

                let range = weapon.range();
                if range.normal() < range.max() {
                    let distance =
                        systems::geometry::distance_between_entities(world, actor, target).unwrap();
                    if distance > range.normal() {
                        attack_roll.advantage_tracker_mut().add(
                            AdvantageType::Disadvantage,
                            ModifierSource::Custom("Target is outside normal range".to_string()),
                        );
                    }
                }

                attack_roll.merge_from(self.attack_rolls.get(weapon.kind()));

                (AttackSource::Weapon(*weapon.kind()), attack_roll)
            }

            ActionAttackKind::Unarmed => {
                let mut attack_roll = D20Check::new(
                    D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Unarmed)),
                    Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::Base),
                );
                let strength_modifier =
                    systems::helpers::get_component::<AbilityScoreMap>(world, actor)
                        .ability_modifier(&Ability::Strength)
                        .total();
                attack_roll.add_modifier(
                    ModifierSource::Ability(Ability::Strength),
                    strength_modifier,
                );
                attack_roll.merge_from(self.attack_rolls.get(&WeaponKind::Unarmed));

                (AttackSource::Weapon(WeaponKind::Unarmed), attack_roll)
            }
        }
    }
}

impl SavingThrowProvider for Loadout {
    fn saving_throw(
        &self,
        world: &World,
        actor: Entity,
        context: &ActionContext,
        kind: SavingThrowKind,
    ) -> D20CheckDC {
        let attack_context = context
            .attack
            .as_ref()
            .expect("Action context must contain attack metadata");
        let ability_scores = systems::helpers::get_component::<AbilityScoreMap>(world, actor);

        let (weapon_kind, ability) = match attack_context.kind {
            ActionAttackKind::MeleeWeapon | ActionAttackKind::RangedWeapon => {
                let slot = attack_context
                    .slot
                    .as_ref()
                    .expect("Weapon attacks require an equipment slot");
                let weapon = self
                    .weapon_in_hand(slot)
                    .expect("No weapon equipped in the specified slot");
                (*weapon.kind(), weapon.determine_ability(&ability_scores))
            }
            ActionAttackKind::Unarmed => (WeaponKind::Unarmed, Ability::Strength),
        };

        let proficiency_bonus = systems::helpers::level(world, actor)
            .unwrap()
            .proficiency_bonus() as i32;
        let ability_modifier = ability_scores.ability_modifier(&ability).total();

        let mut dc = ModifierMap::from_iter(vec![
            (ModifierSource::Base, 8),
            (
                ModifierSource::Proficiency(ProficiencyLevel::Proficient),
                proficiency_bonus,
            ),
            (ModifierSource::Ability(ability), ability_modifier),
        ]);

        if let Some(modifiers) = self.saving_throw_modifiers.get(&weapon_kind) {
            dc.add_modifier_map(modifiers);
        }

        D20CheckDC::SavingThrow {
            saving_throw: kind,
            dc: dc.evaluate(),
        }
    }
}

impl ActionProvider for Loadout {
    fn actions(&self, world: &World, entity: Entity) -> ActionMap {
        let mut action_map = ActionMap::new();

        for action_id in ATTACK_ACTIONS.iter() {
            let Some(action) = systems::actions::get_action(action_id) else {
                continue;
            };

            for context in &action.contexts {
                if let Some(attack_context) = &context.attack
                    && self.is_valid_context(&attack_context)
                {
                    systems::actions::add_action_to_map(
                        &mut action_map,
                        action_id,
                        context,
                        &action.resource_cost,
                    );
                }
            }
        }

        action_map
    }
}

#[cfg(test)]
mod tests {
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

        let unequipped = loadout.equip_in_slot(
            &EquipmentSlot::Armor,
            &ItemId::new("nat20_core", "item.chainmail"),
        );
        assert!(unequipped.unwrap().is_empty());
        assert_eq!(
            loadout.armor().unwrap().item.id,
            ItemId::new("nat20_core", "item.chainmail")
        );

        let unequipped = loadout.unequip(&EquipmentSlot::Armor);
        assert_eq!(
            unequipped.unwrap(),
            ItemId::new("nat20_core", "item.chainmail")
        );
        assert!(loadout.armor().is_none());

        let unequipped = loadout.unequip(&EquipmentSlot::Armor);
        assert!(unequipped.is_none());
        assert!(loadout.armor().is_none());
    }

    #[test]
    fn equip_armor_twice() {
        let mut loadout = Loadout::new();

        let armor1 = &ItemId::new("nat20_core", "item.chainmail");
        let slot = EquipmentSlot::Armor;
        let unequipped1 = loadout.equip_in_slot(&slot, armor1);
        assert!(unequipped1.unwrap().is_empty());
        assert_eq!(
            loadout.armor().unwrap().item.id,
            ItemId::new("nat20_core", "item.chainmail")
        );
        let armor2 = &ItemId::new("nat20_core", "item.studded_leather_armor");
        let unequipped2 = loadout.equip_in_slot(&slot, armor2);
        assert!(unequipped2.unwrap().iter().any(|item| item == armor1));
        assert_eq!(loadout.armor().unwrap().item.id, *armor2);
    }

    #[test]
    fn equip_unequip_item() {
        let mut loadout = Loadout::new();

        let unequipped = loadout.equip_in_slot(
            &EquipmentSlot::Boots,
            &ItemId::new("nat20_core", "item.boots_with_the_fur"),
        );
        assert!(unequipped.unwrap().is_empty());
        assert!(loadout.item_in_slot(&EquipmentSlot::Boots).is_some());

        let unequipped = loadout.unequip(&EquipmentSlot::Boots);
        assert_eq!(
            unequipped,
            Some(ItemId::new("nat20_core", "item.boots_with_the_fur"))
        );
        assert!(loadout.item_in_slot(&EquipmentSlot::Boots).is_none());

        let unequipped = loadout.unequip(&EquipmentSlot::Boots);
        assert!(unequipped.is_none());
        assert!(loadout.item_in_slot(&EquipmentSlot::Boots).is_none());
    }

    #[test]
    fn equip_item_twice() {
        let mut loadout = Loadout::new();

        let unequipped1 = loadout.equip_in_slot(
            &EquipmentSlot::Boots,
            &ItemId::new("nat20_core", "item.boots_with_the_fur"),
        );
        assert!(unequipped1.unwrap().is_empty());
        assert!(loadout.item_in_slot(&EquipmentSlot::Boots).is_some());

        let unequipped2 = loadout.equip_in_slot(
            &EquipmentSlot::Boots,
            &ItemId::new("nat20_core", "item.boots_with_the_fur"),
        );
        assert!(
            unequipped2
                .unwrap()
                .contains(&ItemId::new("nat20_core", "item.boots_with_the_fur"))
        );
        assert!(loadout.item_in_slot(&EquipmentSlot::Boots).is_some());
    }

    #[test]
    fn equip_unequip_weapon() {
        let mut loadout = Loadout::new();

        let unequipped = loadout.equip_in_slot(
            &EquipmentSlot::MeleeMainHand,
            &ItemId::new("nat20_core", "item.dagger"),
        );
        assert!(unequipped.is_ok());
        assert!(
            loadout
                .weapon_in_hand(&EquipmentSlot::MeleeMainHand)
                .is_some()
        );

        let unequipped = loadout.unequip(&EquipmentSlot::MeleeMainHand);
        assert!(unequipped.is_some());
        assert!(
            loadout
                .weapon_in_hand(&EquipmentSlot::MeleeMainHand)
                .is_none()
        );
    }

    #[test]
    fn equip_weapon_twice() {
        let mut loadout = Loadout::new();

        let unequipped1 = loadout.equip_in_slot(
            &EquipmentSlot::MeleeMainHand,
            &ItemId::new("nat20_core", "item.dagger"),
        );
        assert_eq!(unequipped1.unwrap().len(), 0);
        assert!(
            loadout
                .weapon_in_hand(&EquipmentSlot::MeleeMainHand)
                .is_some()
        );

        let unequipped2 = loadout.equip_in_slot(
            &EquipmentSlot::MeleeMainHand,
            &ItemId::new("nat20_core", "item.dagger"),
        );
        assert_eq!(unequipped2.unwrap().len(), 1);
        assert!(
            loadout
                .weapon_in_hand(&EquipmentSlot::MeleeMainHand)
                .is_some()
        );
    }

    #[test]
    fn equip_two_handed_weapon_should_unequip_other_hand() {
        let mut loadout = Loadout::new();

        let unequipped_main = loadout.equip_in_slot(
            &EquipmentSlot::MeleeMainHand,
            &ItemId::new("nat20_core", "item.dagger"),
        );
        assert!(unequipped_main.is_ok());
        assert!(
            loadout
                .weapon_in_hand(&EquipmentSlot::MeleeMainHand)
                .is_some()
        );

        let unequipped_off = loadout.equip_in_slot(
            &EquipmentSlot::MeleeOffHand,
            &ItemId::new("nat20_core", "item.dagger"),
        );
        assert!(unequipped_off.is_ok());
        assert!(
            loadout
                .weapon_in_hand(&EquipmentSlot::MeleeOffHand)
                .is_some()
        );

        let unequipped = loadout.equip_in_slot(
            &EquipmentSlot::MeleeMainHand,
            &ItemId::new("nat20_core", "item.greatsword"),
        );
        println!("{:?}", unequipped);
        assert!(unequipped.is_ok());
        // Should unequip both hands if required_slots includes both
        assert!(
            loadout
                .weapon_in_hand(&EquipmentSlot::MeleeMainHand)
                .is_some()
        );
        assert!(
            loadout
                .weapon_in_hand(&EquipmentSlot::MeleeOffHand)
                .is_none()
        );
    }

    #[test]
    fn equip_in_wrong_slot() {
        let mut loadout = Loadout::new();

        // Try to equip boots in the Headwear slot, which should be invalid
        let result = loadout.equip_in_slot(
            &EquipmentSlot::Headwear,
            &ItemId::new("nat20_core", "item.boots_with_the_fur"),
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            TryEquipError::InvalidSlot {
                slot: EquipmentSlot::Headwear,
                item: ItemId::new("nat20_core", "item.boots_with_the_fur"),
            }
        );
    }

    #[test]
    fn available_actions_no_weapons() {
        let mut world = World::new();
        let entity = world.spawn(());

        let loadout = Loadout::new();
        let actions = loadout.actions(&world, entity);
        println!("{:?}", actions);
        assert_eq!(actions.len(), 2);
        for action_id in [
            ActionId::new("nat20_core", "action.unarmed_attack"),
            ActionId::new("nat20_core", "action.opportunity_attack"),
        ] {
            assert!(actions.contains_key(&action_id));
            assert_eq!(actions[&action_id].len(), 1);
            assert!(actions[&action_id][0].0.is_attack_action());
        }
    }

    #[test]
    fn available_actions_melee_and_ranged_weapon() {
        let mut world = World::new();
        let entity = world.spawn(());

        let mut loadout = Loadout::new();

        let _ = loadout.equip(&ItemId::new("nat20_core", "item.dagger"));

        let _ = loadout.equip(&ItemId::new("nat20_core", "item.shortbow"));

        let actions = loadout.actions(&world, entity);
        for action in &actions {
            println!("{:?}", action);
        }

        assert_eq!(actions.len(), ATTACK_ACTIONS.len());
        for action_id in ATTACK_ACTIONS.iter() {
            assert!(actions.contains_key(action_id));
            assert!(!actions[action_id].is_empty());
            for (context, _) in &actions[action_id] {
                assert!(context.is_attack_action());
            }
        }
    }
}
