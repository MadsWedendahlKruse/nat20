extern crate nat20_core;

mod tests {
    use std::{collections::HashSet, str::FromStr};

    use hecs::World;
    use nat20_core::{
        components::{
            ability::{Ability, AbilityScore, AbilityScoreMap},
            actions::action::{ActionContext, AttackRollProvider},
            damage::DamageType,
            dice::DieSize,
            id::ItemId,
            items::{
                equipment::{
                    loadout::Loadout,
                    slots::EquipmentSlot,
                    weapon::{Weapon, WeaponCategory, WeaponKind, WeaponProperties},
                },
                inventory::ItemInstance,
                item::{Item, ItemRarity},
                money::MonetaryValue,
            },
            modifier::ModifierSource,
            proficiency::ProficiencyLevel,
        },
        entities::character::Character,
        registry::registry::ItemsRegistry,
        systems::{self, helpers},
        test_utils::fixtures,
    };
    use uom::si::{f32::Mass, mass::pound};

    #[test]
    fn character_weapon_finesse_modifier() {
        let mut game_state = fixtures::engine::game_state();
        let entity = game_state.world.spawn(Character::default());

        // Set Strength 14, Dexterity 16
        {
            let mut scores =
                helpers::get_component_mut::<AbilityScoreMap>(&mut game_state.world, entity);
            scores.set(Ability::Strength, AbilityScore::new(Ability::Strength, 14));
            scores.set(
                Ability::Dexterity,
                AbilityScore::new(Ability::Dexterity, 16),
            );
        }

        let weapon = ItemsRegistry::get(&ItemId::new("nat20_core", "item.scimitar"))
            .unwrap()
            .clone();
        let weapon = match weapon {
            ItemInstance::Weapon(weapon) => weapon,
            _ => panic!("Expected a weapon item"),
        };

        let damage_roll = {
            let ability_scores =
                systems::helpers::get_component::<AbilityScoreMap>(&game_state.world, entity);
            assert_eq!(
                weapon.determine_ability(&ability_scores),
                Ability::Dexterity
            );
            weapon.damage_roll(
                &ability_scores,
                false, // not wielding with both hands
            )
        };
        let damage_result =
            systems::damage::damage_roll(damage_roll, &game_state.world, entity, false);

        println!("{:?}", damage_result);
        assert!(
            (4..=11).contains(&damage_result.total),
            "Damage roll out of bounds: {}",
            damage_result.total
        );
        assert!(
            damage_result.components[0]
                .result
                .modifiers
                .get(&ModifierSource::Ability(Ability::Dexterity))
                .is_some()
        );
    }

    #[test]
    fn character_versatile_weapon() {
        let mut game_state = fixtures::engine::game_state();
        let entity = game_state.world.spawn(Character::default());

        // Equip longsword
        let longsword = ItemsRegistry::get(&ItemId::new("nat20_core", "item.longsword"))
            .unwrap()
            .clone();
        let _ = systems::loadout::equip(&mut game_state.world, entity, longsword);

        // Longsword used with two hands
        let roll = systems::loadout::weapon_damage_roll(
            &mut game_state.world,
            entity,
            &EquipmentSlot::MeleeMainHand,
        );
        assert_eq!(roll.primary.dice_roll.dice.num_dice, 1);
        assert_eq!(roll.primary.dice_roll.dice.die_size, DieSize::D10);

        systems::loadout::equip_in_slot(
            &mut game_state.world,
            entity,
            &EquipmentSlot::MeleeOffHand,
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
                .unwrap()
                .clone(),
        )
        .unwrap();

        // Longsword now used one-handed
        let roll = systems::loadout::weapon_damage_roll(
            &game_state.world,
            entity,
            &EquipmentSlot::MeleeMainHand,
        );
        assert_eq!(roll.primary.dice_roll.dice.num_dice, 1);
        assert_eq!(roll.primary.dice_roll.dice.die_size, DieSize::D8);

        // Unequip dagger
        let _ =
            systems::loadout::unequip(&mut game_state.world, entity, &EquipmentSlot::MeleeOffHand)
                .unwrap();

        // Longsword used with two hands again
        let roll = systems::loadout::weapon_damage_roll(
            &game_state.world,
            entity,
            &EquipmentSlot::MeleeMainHand,
        );
        assert_eq!(roll.primary.dice_roll.dice.die_size, DieSize::D10);
    }

    #[test]
    fn character_two_handed_weapon() {
        let mut world = World::new();
        let entity = world.spawn(Character::default());

        systems::loadout::equip_in_slot(
            &mut world,
            entity,
            &EquipmentSlot::MeleeOffHand,
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.dagger"))
                .unwrap()
                .clone(),
        )
        .unwrap();
        let _ = systems::loadout::equip_in_slot(
            &mut world,
            entity,
            &EquipmentSlot::MeleeMainHand,
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.longsword"))
                .unwrap()
                .clone(),
        );

        let unequipped = systems::loadout::equip(
            &mut world,
            entity,
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.greatsword"))
                .unwrap()
                .clone(),
        )
        .unwrap();
        assert_eq!(unequipped.len(), 2);

        // Main hand has greatsword, off-hand should be empty
        let loadout = systems::helpers::get_component::<Loadout>(&world, entity);
        assert!(loadout.has_weapon_in_hand(&EquipmentSlot::MeleeMainHand));
        assert!(!loadout.has_weapon_in_hand(&EquipmentSlot::MeleeOffHand));
    }

    #[test]
    fn character_attack_roll_basic() {
        let mut world = World::new();
        let entity = world.spawn(Character::default());

        {
            let mut scores = helpers::get_component_mut::<AbilityScoreMap>(&mut world, entity);
            scores.set(Ability::Strength, AbilityScore::new(Ability::Strength, 14));
            scores.set(
                Ability::Dexterity,
                AbilityScore::new(Ability::Dexterity, 16),
            );
        }

        let longsword = Weapon::new(
            Item {
                id: ItemId::new("nat20_core", "item.longsword"),
                name: "Longsword".to_string(),
                description: "A longsword.".to_string(),
                weight: Mass::new::<pound>(3.0),
                value: MonetaryValue::from_str("15 GP").unwrap(),
                rarity: ItemRarity::Common,
            },
            WeaponKind::Melee,
            WeaponCategory::Martial,
            HashSet::from([WeaponProperties::Finesse]),
            vec![("1d8".parse().unwrap(), DamageType::Slashing)],
            vec![],
            vec![],
        );

        systems::loadout::equip(&mut world, entity, longsword).unwrap();

        let roll = systems::loadout::loadout(&world, entity).attack_roll(
            &world,
            entity,
            entity,
            &ActionContext::Weapon {
                slot: EquipmentSlot::MeleeMainHand,
            },
        );

        println!("{:?}", roll);
        assert!(
            roll.d20_check
                .modifiers()
                .contains_key(&ModifierSource::Ability(Ability::Dexterity))
        );
        assert!(
            !roll
                .d20_check
                .modifiers()
                .contains_key(&ModifierSource::Proficiency(ProficiencyLevel::Proficient))
        );
        assert!(
            !roll
                .d20_check
                .modifiers()
                .contains_key(&ModifierSource::Custom("Enchantment".to_string()))
        );
    }
}
