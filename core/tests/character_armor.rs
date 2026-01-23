extern crate nat20_core;

mod tests {
    use nat20_core::{
        components::{
            ability::{Ability, AbilityScore, AbilityScoreMap},
            id::{EffectId, ItemId},
            items::equipment::slots::EquipmentSlot,
            modifier::{KeyedModifiable, Modifiable, ModifierSource},
        },
        entities::character::Character,
        registry::registry::ItemsRegistry,
        systems,
        test_utils::fixtures,
    };

    #[test]
    fn character_armor_class_no_dex() {
        let mut game_state = fixtures::engine::game_state();
        let character = game_state.world.spawn(Character::default());

        let _ = systems::loadout::equip(
            &mut game_state.world,
            character,
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.chainmail"))
                .unwrap()
                .clone(),
        );

        let armor_class = systems::loadout::armor_class(&game_state.world, character);
        assert_eq!(16, armor_class.total());
        println!("{:?}", armor_class);

        // Check that the heavy armor gives stealth disadvantage
        let effects = systems::effects::effects(&game_state.world, character)
            .values()
            .cloned()
            .collect::<Vec<_>>();
        assert!(!effects.is_empty());
        assert!(effects.iter().any(|e| {
            e.effect_id == EffectId::new("nat20_core", "effect.item.armor_stealth_disadvantage")
        }));
    }

    #[test]
    fn character_armor_class_dex_and_bonus() {
        // Create a character with a Dexterity modifier of +3
        let mut game_state = fixtures::engine::game_state();
        let character = game_state.world.spawn(Character::default());

        {
            let mut ability_scores = systems::helpers::get_component_mut::<AbilityScoreMap>(
                &mut game_state.world,
                character,
            );
            ability_scores.set(
                Ability::Dexterity,
                AbilityScore::new(Ability::Dexterity, 15),
            );
            ability_scores.add_modifier(
                &Ability::Dexterity,
                ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_dexterity")),
                2,
            );
        }

        let _ = systems::loadout::equip(
            &mut game_state.world,
            character,
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.studded_leather_armor"))
                .unwrap()
                .clone(),
        );

        {
            let armor_class = systems::loadout::armor_class(&game_state.world, character);
            // Armour Class
            // Dex: 15 + 2 (item) = 17
            // 12 (armor) + 3 (Dex mod) = 15
            println!("{:?}", armor_class);
            assert_eq!(15, armor_class.total());
        }

        // Un-equip the armor
        let armor =
            systems::loadout::unequip(&mut game_state.world, character, &EquipmentSlot::Armor)
                .unwrap();
        let armor_class = systems::loadout::armor_class(&game_state.world, character);
        println!("Un-equipped {:?}", armor);
        // Check if the armor class is updated
        println!("{:?}", armor_class);
        assert_eq!(10, armor_class.total());
    }
}
