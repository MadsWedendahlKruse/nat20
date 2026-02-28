extern crate nat20_core;

mod tests {

    use std::str::FromStr;

    use hecs::World;
    use nat20_core::{
        components::{
            ability::Ability,
            actions::action::{ActionContext, AttackRollProvider},
            d20::RollMode,
            id::{EffectId, ItemId},
            items::{
                equipment::{
                    armor::Armor,
                    equipment::{EquipmentItem, EquipmentKind},
                    slots::EquipmentSlot,
                },
                item::{Item, ItemRarity},
                money::MonetaryValue,
            },
            saving_throw::{SavingThrowKind, SavingThrowSet},
            skill::{Skill, SkillSet},
        },
        entities::character::Character,
        registry::registry::ItemsRegistry,
        systems,
        test_utils::fixtures,
    };
    use uom::si::{f32::Mass, mass::pound};

    #[test]
    fn character_pre_attack_roll_effect() {
        let mut game_state = fixtures::engine::game_state();
        let entity = game_state.world.spawn(Character::default());

        let ring = EquipmentItem {
            item: Item {
                id: ItemId::new("nat20_core", "item.ring_of_attacking"),
                name: "Ring of Attacking".to_string(),
                description: "A magical ring that grants advantage on attack rolls.".to_string(),
                weight: Mass::new::<pound>(0.1),
                value: MonetaryValue::from_str("1000 GP").unwrap(),
                rarity: ItemRarity::Rare,
            },
            kind: EquipmentKind::Ring,
            effects: vec![EffectId::new("nat20_core", "effect.item.ring_of_attacking")],
        };

        let _ = systems::loadout::equip(
            &mut game_state,
            entity,
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
                .unwrap()
                .clone(),
        );

        // Before equipping the ring
        let context = &ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand);

        let roll = systems::loadout::loadout(&game_state.world, entity).attack_roll(
            &game_state.world,
            entity,
            entity,
            context,
        );
        let result = systems::damage::attack_roll(roll, &game_state.world, entity, entity);
        assert_eq!(
            result.roll_result.advantage_tracker.roll_mode(),
            RollMode::Normal
        );

        // Equip the ring
        let _ =
            systems::loadout::equip_in_slot(&mut game_state, entity, &EquipmentSlot::Ring1, ring);

        let roll = systems::loadout::loadout(&game_state.world, entity).attack_roll(
            &game_state.world,
            entity,
            entity,
            context,
        );
        let result = systems::damage::attack_roll(roll, &game_state.world, entity, entity);
        assert_eq!(
            result.roll_result.advantage_tracker.roll_mode(),
            RollMode::Advantage
        );

        // Unequip the ring
        systems::loadout::unequip(&mut game_state, entity, &EquipmentSlot::Ring1);
        let roll = systems::loadout::loadout(&game_state.world, entity).attack_roll(
            &game_state.world,
            entity,
            entity,
            context,
        );
        let result = systems::damage::attack_roll(roll, &game_state.world, entity, entity);
        assert_eq!(
            result.roll_result.advantage_tracker.roll_mode(),
            RollMode::Normal
        );
    }

    #[test]
    fn character_skill_bonus_effect() {
        let mut game_state = fixtures::engine::game_state();
        let entity = game_state.world.spawn(Character::default());

        let armor = Armor::light(
            Item {
                id: ItemId::new("nat20_core", "item.armor_of_sneaking"),
                name: "Armor of Sneaking".to_string(),
                description: "A magical armor that grants a bonus to Stealth.".to_string(),
                weight: Mass::new::<pound>(0.5),
                value: MonetaryValue::from_str("500 GP").unwrap(),
                rarity: ItemRarity::Rare,
            },
            12,
            vec![EffectId::new("nat20_core", "effect.item.armor_of_sneaking")],
        );
        let _ = systems::loadout::equip(&mut game_state, entity, armor);

        let check = systems::helpers::get_component::<SkillSet>(&game_state.world, entity).check(
            &Skill::Stealth,
            &game_state.world,
            entity,
        );
        assert_eq!(check.total_modifier(), 2);

        let _ = systems::loadout::unequip(&mut game_state, entity, &EquipmentSlot::Armor)
            .expect("Failed to unequip armor");

        let check = systems::helpers::get_component::<SkillSet>(&game_state.world, entity).check(
            &Skill::Stealth,
            &game_state.world,
            entity,
        );
        assert_eq!(check.total_modifier(), 0);
    }

    #[test]
    fn character_saving_throw_effect() {
        let mut game_state = fixtures::engine::game_state();
        let entity = game_state.world.spawn(Character::default());

        let armor = Armor::heavy(
            Item {
                id: ItemId::new("nat20_core", "item.armor_of_constitution_saving_throws"),
                name: "Armor of Constitution Saving Throws".to_string(),
                description: "A magical armor that grants advantage on Constitution saving throws."
                    .to_string(),
                weight: Mass::new::<pound>(10.0),
                value: MonetaryValue::from_str("1500 GP").unwrap(),
                rarity: ItemRarity::VeryRare,
            },
            18,
            vec![EffectId::new(
                "nat20_core",
                "effect.item.armor_of_constitution_saving_throws",
            )],
        );
        let _ = systems::loadout::equip(&mut game_state, entity, armor);

        let throw = systems::helpers::get_component::<SavingThrowSet>(&game_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Constitution),
                &game_state.world,
                entity,
            );
        assert_eq!(throw.advantage_tracker.roll_mode(), RollMode::Advantage);

        systems::loadout::unequip(&mut game_state, entity, &EquipmentSlot::Armor)
            .expect("Failed to unequip armor");

        let throw = systems::helpers::get_component::<SavingThrowSet>(&game_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Constitution),
                &game_state.world,
                entity,
            );
        assert_eq!(throw.advantage_tracker.roll_mode(), RollMode::Normal);
    }
}
