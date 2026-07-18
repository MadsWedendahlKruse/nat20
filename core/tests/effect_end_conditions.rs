extern crate nat20_core;

mod tests {

    use nat20_core::{
        components::{
            ability::Ability, d20::D20CheckOutcome, id::ItemId, saving_throw::SavingThrowKind,
            time::TimeMode,
        },
        registry::registry::ItemsRegistry,
        systems::{self, d20::D20CheckKind},
        test_utils::{creature_builder::CreatureBuilder, fixtures, scenario::Scenario},
    };

    #[test]
    fn rage_ends_when_equipping_heavy_armor() {
        let mut game_state = fixtures::engine::game_state();
        let barbarian = CreatureBuilder::new("hero.barbarian")
            .level(1)
            .time_mode(TimeMode::TurnBased { encounter_id: None })
            .probe(&mut game_state);
        let entity = barbarian.creature.id();

        barbarian.assert_action_available(&game_state, "action.barbarian.rage");
        barbarian
            .act(&mut game_state, "action.barbarian.rage")
            .perform_ok(&mut game_state);
        game_state.update(3.0);
        barbarian.assert_effect(&game_state, "effect.barbarian.rage");

        // Light armor doesn't end Rage
        let light_armor =
            ItemsRegistry::get(&ItemId::new("nat20_core", "item.studded_leather_armor"))
                .unwrap()
                .clone();
        let _ = systems::loadout::equip(&mut game_state, entity, light_armor);
        barbarian.assert_effect(&game_state, "effect.barbarian.rage");

        // Heavy armor does
        let heavy_armor = ItemsRegistry::get(&ItemId::new("nat20_core", "item.chainmail"))
            .unwrap()
            .clone();
        let _ = systems::loadout::equip(&mut game_state, entity, heavy_armor.clone());
        barbarian.assert_no_effect(&game_state, "effect.barbarian.rage");
    }

    #[test]
    fn rage_ends_when_incapacitated() {
        let mut scenario = Scenario::new();
        scenario
            .spawn("barbarian", "hero.barbarian")
            .level(1)
            .position([3.0, 0.0, 0.0], false)
            .spawn();
        scenario.spawn("warlock", "hero.warlock").level(4).spawn();

        scenario
            .probe("barbarian")
            .act("action.barbarian.rage")
            .perform();
        scenario
            .probe("barbarian")
            .assert_effect("effect.barbarian.rage");

        scenario.probe("barbarian").d20_force_outcome(
            D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Wisdom)),
            D20CheckOutcome::CriticalFailure,
        );

        // Hold person also applies incapacitated as a child effect
        scenario
            .probe("warlock")
            .assert_action_available("action.hold_person")
            .act("action.hold_person")
            .target_entity("barbarian")
            .perform();

        scenario
            .probe("barbarian")
            .assert_effect("effect.spell.hold_person")
            .assert_effect("effect.condition.paralyzed")
            .assert_effect("effect.condition.incapacitated")
            .assert_no_effect("effect.barbarian.rage");
    }
}
