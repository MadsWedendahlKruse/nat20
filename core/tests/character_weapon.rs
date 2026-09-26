extern crate nat20_core;

mod tests {

    use nat20_core::{
        components::{
            ability::{Ability, AbilityScore, AbilityScoreMap},
            actions::action::{ActionConditionResolution, ActionContext, AttackRollProvider},
            dice::{DiceSet, DieSize},
            id::{ActionId, EntityIdentifier, ItemId},
            items::{
                equipment::{loadout::Loadout, slots::EquipmentSlot},
                inventory::ItemInstance,
            },
            modifier::{Modifiable, ModifierMap, ModifierSource},
            proficiency::ProficiencyLevel,
            resource::ResourceAmountMap,
        },
        engine::action_prompt::ActionData,
        entities::creature::Character,
        registry::registry::ItemsRegistry,
        systems::{self, helpers},
        test_utils::fixtures,
    };

    #[test]
    fn character_weapon_finesse_modifier() {
        let mut engine_state = fixtures::engine::engine_state();
        let entity = engine_state.world.spawn(Character::default());

        // Set Strength 14, Dexterity 16
        {
            let scores =
                helpers::get_component_mut::<AbilityScoreMap>(&mut engine_state.world, entity);
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
                systems::helpers::get_component::<AbilityScoreMap>(&engine_state.world, entity);
            assert_eq!(
                weapon.determine_ability(&ability_scores),
                Ability::Dexterity
            );
            weapon.damage_roll(
                &ability_scores,
                false, // not wielding with both hands
            )
        };
        let action = ActionData::new(
            EntityIdentifier::from_world(&engine_state.world, entity),
            ActionId::new("nat20_core", "action.placeholder"),
            ActionContext::default(),
            ResourceAmountMap::new(),
            Vec::new(),
        );
        let damage_result = systems::damage::damage_roll(
            damage_roll,
            &engine_state,
            &action,
            &ActionConditionResolution::Unconditional,
        );

        println!("{:?}", damage_result);
        assert!(
            (4..=11).contains(&damage_result.total),
            "Damage roll out of bounds: {}",
            damage_result.total
        );
        assert!(
            damage_result.components[0]
                .result
                .get(&ModifierSource::Ability(Ability::Dexterity))
                .is_some()
        );
    }

    #[test]
    fn character_versatile_weapon() {
        let mut engine_state = fixtures::engine::engine_state();
        let entity = engine_state.world.spawn(Character::default());

        // Equip longsword
        let longsword = &ItemId::new("nat20_core", "item.longsword");
        let _ = systems::loadout::equip(&mut engine_state, entity, longsword);

        // Longsword used with two hands
        let roll = systems::loadout::weapon_damage_roll(
            &engine_state.world,
            entity,
            &EquipmentSlot::MeleeMainHand,
        );
        assert_eq!(
            roll.components[0].modifiers(),
            &ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D10))
        );

        systems::loadout::equip_in_slot(
            &mut engine_state,
            entity,
            &EquipmentSlot::MeleeOffHand,
            &ItemId::new("nat20_core", "item.dagger"),
        )
        .unwrap();

        // Longsword now used one-handed
        let roll = systems::loadout::weapon_damage_roll(
            &engine_state.world,
            entity,
            &EquipmentSlot::MeleeMainHand,
        );
        assert_eq!(
            roll.components[0].modifiers(),
            &ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D8))
        );

        // Unequip dagger
        let _ = systems::loadout::unequip(&mut engine_state, entity, &EquipmentSlot::MeleeOffHand)
            .unwrap();

        // Longsword used with two hands again
        let roll = systems::loadout::weapon_damage_roll(
            &engine_state.world,
            entity,
            &EquipmentSlot::MeleeMainHand,
        );
        assert_eq!(
            roll.components[0].modifiers(),
            &ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D10))
        );
    }

    #[test]
    fn character_two_handed_weapon() {
        let mut engine_state = fixtures::engine::engine_state();
        let entity = engine_state.world.spawn(Character::default());

        systems::loadout::equip_in_slot(
            &mut engine_state,
            entity,
            &EquipmentSlot::MeleeOffHand,
            &ItemId::new("nat20_core", "item.dagger"),
        )
        .unwrap();
        let _ = systems::loadout::equip_in_slot(
            &mut engine_state,
            entity,
            &EquipmentSlot::MeleeMainHand,
            &ItemId::new("nat20_core", "item.longsword"),
        );

        let unequipped = systems::loadout::equip(
            &mut engine_state,
            entity,
            &ItemId::new("nat20_core", "item.greatsword"),
        )
        .unwrap();
        assert_eq!(unequipped.len(), 2);

        // Main hand has greatsword, off-hand should be empty
        let loadout = systems::helpers::get_component::<Loadout>(&engine_state.world, entity);
        assert!(loadout.has_weapon_in_hand(&EquipmentSlot::MeleeMainHand));
        assert!(!loadout.has_weapon_in_hand(&EquipmentSlot::MeleeOffHand));
    }

    #[test]
    fn character_attack_roll_basic() {
        let mut engine_state = fixtures::engine::engine_state();
        let entity = engine_state.world.spawn(Character::default());

        {
            let scores =
                helpers::get_component_mut::<AbilityScoreMap>(&mut engine_state.world, entity);
            scores.set(Ability::Strength, AbilityScore::new(Ability::Strength, 14));
            scores.set(
                Ability::Dexterity,
                AbilityScore::new(Ability::Dexterity, 16),
            );
        }

        systems::loadout::equip(
            &mut engine_state,
            entity,
            &ItemId::new("nat20_core", "item.scimitar"),
        )
        .unwrap();

        let (_, roll) = systems::loadout::loadout(&engine_state.world, entity).attack_roll(
            &engine_state.world,
            entity,
            entity,
            &ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        );

        println!("{:?}", roll);
        assert!(
            roll.modifiers()
                .contains_key(&ModifierSource::Ability(Ability::Dexterity))
        );
        assert!(
            !roll
                .modifiers()
                .contains_key(&ModifierSource::Proficiency(ProficiencyLevel::Proficient))
        );
        assert!(
            !roll
                .modifiers()
                .contains_key(&ModifierSource::Custom("Enchantment".to_string()))
        );
    }
}
