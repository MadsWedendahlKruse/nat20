extern crate nat20_core;

mod tests {

    use nat20_core::{
        components::{
            ability::{Ability, AbilityScore, AbilityScoreMap},
            d20::RollMode,
            id::ItemId,
            modifier::{FlatModifiable, KeyedFlatModifiable, ModifierSource},
            proficiency::{Proficiency, ProficiencyLevel},
            saving_throw::{SavingThrowKind, SavingThrowSet},
            skill::{Skill, SkillSet},
        },
        entities::creature::Character,
        systems::{self},
        test_utils::fixtures,
    };

    #[test]
    fn character_saving_throw_modifier() {
        let mut engine_state = fixtures::engine::engine_state();

        let entity = engine_state.world.spawn(Character::default());

        {
            let ability_scores = systems::helpers::get_component_mut::<AbilityScoreMap>(
                &mut engine_state.world,
                entity,
            );
            ability_scores.set(Ability::Strength, AbilityScore::new(Ability::Strength, 17));
            ability_scores.add_modifier(
                &Ability::Strength,
                ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_strength")),
                2,
            );
            assert_eq!(ability_scores.get(&Ability::Strength).total(), 19);
        }

        let result = systems::helpers::get_component::<SavingThrowSet>(&engine_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Strength),
                &engine_state,
                entity,
            );
        assert_eq!(result.total_modifier(), 4);
    }

    #[test]
    fn character_saving_throw_proficiency() {
        let mut engine_state = fixtures::engine::engine_state();

        // Default character is level 0, meaning it has no proficieny bonus, so
        // if we want to test that we need a character with at least one level.
        // Easiest way is to use one of the fixtures.
        let entity = fixtures::creatures::heroes::wizard(&mut engine_state, 5, None).id();

        systems::helpers::get_component_mut::<AbilityScoreMap>(&mut engine_state.world, entity)
            .set(Ability::Strength, AbilityScore::new(Ability::Strength, 17));
        systems::helpers::get_component_mut::<SavingThrowSet>(&mut engine_state.world, entity)
            .set_proficiency(
                &SavingThrowKind::Ability(Ability::Strength),
                Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
            );

        let result = systems::helpers::get_component::<SavingThrowSet>(&engine_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Strength),
                &engine_state,
                entity,
            );
        assert_eq!(result.total_modifier(), 6);
    }

    #[test]
    fn character_saving_throw_proficiency_expertise() {
        let mut engine_state = fixtures::engine::engine_state();

        // Default character is level 0, meaning it has no proficieny bonus, so
        // if we want to test that we need a character with at least one level.
        // Easiest way is to use one of the fixtures.
        let entity = fixtures::creatures::heroes::wizard(&mut engine_state, 5, None).id();

        systems::helpers::get_component_mut::<AbilityScoreMap>(&mut engine_state.world, entity)
            .set(Ability::Strength, AbilityScore::new(Ability::Strength, 17));
        systems::helpers::get_component_mut::<SavingThrowSet>(&mut engine_state.world, entity)
            .set_proficiency(
                &SavingThrowKind::Ability(Ability::Strength),
                Proficiency::new(ProficiencyLevel::Expertise, ModifierSource::None),
            );

        let result = systems::helpers::get_component::<SavingThrowSet>(&engine_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Strength),
                &engine_state,
                entity,
            );
        assert_eq!(result.total_modifier(), 9);
    }

    #[test]
    fn character_skill_disadvantage() {
        let mut engine_state = fixtures::engine::engine_state();
        let character = engine_state.world.spawn(Character::default());

        let _ = systems::loadout::equip(
            &mut engine_state,
            character,
            &ItemId::new("nat20_core", "item.chainmail"),
        );

        let result = systems::helpers::get_component::<SkillSet>(&engine_state.world, character)
            .check(&Skill::Stealth, &engine_state, character);
        assert!(result.advantage_tracker().roll_mode() == RollMode::Disadvantage);
    }
}
