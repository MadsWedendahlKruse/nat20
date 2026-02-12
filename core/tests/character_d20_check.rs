extern crate nat20_core;

mod tests {

    use hecs::World;
    use nat20_core::{
        components::{
            ability::{Ability, AbilityScore, AbilityScoreMap},
            d20::RollMode,
            id::ItemId,
            modifier::{KeyedModifiable, Modifiable, ModifierSource},
            proficiency::{Proficiency, ProficiencyLevel},
            saving_throw::{SavingThrowKind, SavingThrowSet},
            skill::{Skill, SkillSet},
        },
        entities::character::Character,
        registry::registry::ItemsRegistry,
        systems::{self},
        test_utils::fixtures,
    };

    #[test]
    fn character_saving_throw_modifier() {
        let mut world = World::new();

        let entity = world.spawn(Character::default());

        {
            let mut ability_scores =
                systems::helpers::get_component_mut::<AbilityScoreMap>(&mut world, entity);
            ability_scores.set(Ability::Strength, AbilityScore::new(Ability::Strength, 17));
            ability_scores.add_modifier(
                &Ability::Strength,
                ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_strength")),
                2,
            );
            assert_eq!(ability_scores.get(&Ability::Strength).total(), 19);
        }

        let result = systems::helpers::get_component::<SavingThrowSet>(&world, entity).check(
            &SavingThrowKind::Ability(Ability::Strength),
            &world,
            entity,
        );
        assert_eq!(result.modifier_breakdown.total(), 4);
    }

    #[test]
    fn character_saving_throw_proficiency() {
        let mut game_state = fixtures::engine::game_state();

        // Default character is level 0, meaning it has no proficieny bonus, so
        // if we want to test that we need a character with at least one level.
        // Easiest way is to use one of the fixtures.
        let entity = fixtures::creatures::heroes::wizard(&mut game_state).id();

        systems::helpers::get_component_mut::<AbilityScoreMap>(&mut game_state.world, entity)
            .set(Ability::Strength, AbilityScore::new(Ability::Strength, 17));
        systems::helpers::get_component_mut::<SavingThrowSet>(&mut game_state.world, entity)
            .set_proficiency(
                &SavingThrowKind::Ability(Ability::Strength),
                Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
            );

        let result = systems::helpers::get_component::<SavingThrowSet>(&game_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Strength),
                &game_state.world,
                entity,
            );
        assert_eq!(result.modifier_breakdown.total(), 6);
    }

    #[test]
    fn character_saving_throw_proficiency_expertise() {
        let mut game_state = fixtures::engine::game_state();

        // Default character is level 0, meaning it has no proficieny bonus, so
        // if we want to test that we need a character with at least one level.
        // Easiest way is to use one of the fixtures.
        let entity = fixtures::creatures::heroes::wizard(&mut game_state).id();

        systems::helpers::get_component_mut::<AbilityScoreMap>(&mut game_state.world, entity)
            .set(Ability::Strength, AbilityScore::new(Ability::Strength, 17));
        systems::helpers::get_component_mut::<SavingThrowSet>(&mut game_state.world, entity)
            .set_proficiency(
                &SavingThrowKind::Ability(Ability::Strength),
                Proficiency::new(ProficiencyLevel::Expertise, ModifierSource::None),
            );

        let result = systems::helpers::get_component::<SavingThrowSet>(&game_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Strength),
                &game_state.world,
                entity,
            );
        assert_eq!(result.modifier_breakdown.total(), 9);
    }

    #[test]
    fn character_skill_disadvantage() {
        let mut game_state = fixtures::engine::game_state();
        let character = game_state.world.spawn(Character::default());

        let _ = systems::loadout::equip(
            &mut game_state,
            character,
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.chainmail"))
                .unwrap()
                .clone(),
        );

        let result = systems::helpers::get_component::<SkillSet>(&game_state.world, character)
            .check(&Skill::Stealth, &game_state.world, character);
        assert!(result.advantage_tracker.roll_mode() == RollMode::Disadvantage);
    }
}
