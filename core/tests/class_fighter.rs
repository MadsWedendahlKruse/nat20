extern crate nat20_core;

mod tests {
    use nat20_core::{
        components::{
            ability::Ability,
            d20::D20CheckOutcome,
            id::ActionId,
            modifier::{ModifierSet, ModifierSource},
            saving_throw::SavingThrowKind,
        },
        engine::event::{CallbackResult, EventCallback, EventKind},
        systems::d20::{D20CheckDCKind, D20CheckKind},
        test_utils::{
            assertions::{
                Cmp, assert_action, assert_effect, assert_hp, assert_no_effect,
                assert_no_pending_reaction, assert_no_resource, assert_on_cooldown,
                assert_pending_reaction, assert_resource,
            },
            builders::CreatureBuilder,
            fixtures,
        },
    };

    #[test]
    fn fighter_action_surge() {
        let mut game_state = fixtures::engine::game_state();
        let mut fighter = CreatureBuilder::from_template(&mut game_state, "hero.fighter")
            .level(5)
            .turn_based()
            .spawn();

        // Pre-conditions: action available, one charge, one action this turn.
        assert_action(&fighter, "action.fighter.action_surge");
        assert_resource(&fighter, "resource.fighter.action_surge", Cmp::Equal(1));
        assert_resource(&fighter, "resource.action", Cmp::Equal(1));

        fighter
            .act("action.fighter.action_surge")
            .on_self()
            .submit_ok();

        // Effect applied, action budget bumped to 2, action goes on cooldown.
        assert_effect(&fighter, "effect.fighter.action_surge");
        assert_resource(&fighter, "resource.action", Cmp::Equal(2));
        assert_on_cooldown(&fighter, "action.fighter.action_surge");

        // The Action Surge effect should fall off at the start of the next turn.
        fighter.start_turn();

        assert_no_effect(&fighter, "effect.fighter.action_surge");
        assert_resource(&fighter, "resource.action", Cmp::Equal(1));
        assert_no_resource(&fighter, "resource.fighter.action_surge");
    }

    #[test]
    fn fighter_second_wind() {
        let mut game_state = fixtures::engine::game_state();
        let mut fighter = CreatureBuilder::from_template(&mut game_state, "hero.fighter")
            .level(5)
            .turn_based()
            .spawn();

        assert_action(&fighter, "action.fighter.second_wind");
        assert_resource(&fighter, "resource.fighter.second_wind", Cmp::AtLeast(2));

        // Take a small hit so Second Wind has something to heal.
        let max = fighter.max_hp();
        fighter.damage_raw(5);
        assert_hp(&fighter, Cmp::Less(max));

        let prev_hp = fighter.hp();

        fighter
            .act("action.fighter.second_wind")
            .on_self()
            .submit_ok();

        assert_hp(&fighter, Cmp::Greater(prev_hp));
    }

    #[test]
    fn fighter_extra_attack() {
        let mut game_state = fixtures::engine::game_state();
        let mut fighter = CreatureBuilder::from_template(&mut game_state, "hero.fighter")
            .level(5)
            .turn_based()
            .spawn();

        // Level-5 fighter has the Extra Attack passive but zero stacks queued.
        assert_effect(&fighter, "effect.extra_attack");
        assert_no_resource(&fighter, "resource.extra_attack");

        // First melee attack: spends the Action and grants one Extra Attack stack.
        fighter
            .act("action.melee_attack")
            .at_point([1.0, 0.0, 0.0])
            .submit_ok();
        assert_resource(&fighter, "resource.extra_attack", Cmp::AtLeast(1));
        assert_no_resource(&fighter, "resource.action");

        // Second melee attack: consumes the Extra Attack stack instead of an Action.
        fighter
            .act("action.melee_attack")
            .at_point([1.0, 0.0, 0.0])
            .submit_ok();
        assert_no_resource(&fighter, "resource.extra_attack");
    }

    #[test]
    fn fighter_two_extra_attacks() {
        let mut game_state = fixtures::engine::game_state();
        let mut fighter = CreatureBuilder::from_template(&mut game_state, "hero.fighter")
            .level(11)
            .turn_based()
            .spawn();

        // Level 11 fighter has the Two Extra Attack passive, but zero stacks queued.
        // Two Extra Attacks also replaces the original Extra Attack effect.
        assert_effect(&fighter, "effect.fighter.two_extra_attacks");
        assert_no_effect(&fighter, "effect.extra_attack");
        assert_no_resource(&fighter, "resource.extra_attack");

        // First melee attack: consumes an action and grants two charges of Extra Attack.
        fighter
            .act("action.melee_attack")
            .at_point([1.0, 0.0, 0.0])
            .submit_ok();
        assert_resource(&fighter, "resource.extra_attack", Cmp::AtLeast(2));
        assert_no_resource(&fighter, "resource.action");

        // Second melee attack: consumes the first Extra Attack stack.
        fighter
            .act("action.melee_attack")
            .at_point([1.0, 0.0, 0.0])
            .submit_ok();
        assert_resource(&fighter, "resource.extra_attack", Cmp::AtLeast(1));

        // Third melee attack: consumes the second Extra Attack stack.
        fighter
            .act("action.melee_attack")
            .at_point([1.0, 0.0, 0.0])
            .submit_ok();
        assert_no_resource(&fighter, "resource.extra_attack");
    }

    #[test]
    fn fighter_studied_attacks() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::from_template(&mut game_state, "hero.fighter")
            .level(13)
            .turn_based()
            .spawn();

        // Level 13 fighter has the studied attacks passive
        assert_effect(&fighter, "effect.fighter.studied_attacks");
        // TODO: Requires multi-actor scenario to test properly
    }

    #[test]
    fn fighter_indomitable() {
        let mut game_state = fixtures::engine::game_state();
        let mut fighter = CreatureBuilder::from_template(&mut game_state, "hero.fighter")
            .level(9)
            .turn_based()
            .spawn();

        // Level 9 fighter has the Indomitable action and resource
        assert_action(&fighter, "action.fighter.indomitable");
        assert_resource(&fighter, "resource.fighter.indomitable", Cmp::Equal(1));

        // Force a failed saving throw to trigger the reaction
        let saving_throw = SavingThrowKind::Ability(Ability::Intelligence);
        let dc = D20CheckDCKind::saving_throw(
            saving_throw,
            ModifierSet::from(ModifierSource::Custom("test".to_string()), 99),
        );
        // Prevent rolling a nat 20
        fighter.d20_force_outcome(
            D20CheckKind::SavingThrow(saving_throw),
            D20CheckOutcome::CriticalFailure,
        );

        fighter.d20_check(
            &dc,
            Some(EventCallback::new(|game_state, event, _| {
                let EventKind::D20CheckResolved(entity, result, dc) = &event.kind else {
                    panic!("Expected D20CheckResult event");
                };

                let Some(modifier) =
                    result
                        .d20_result()
                        .modifier_breakdown
                        .get(&ModifierSource::Action(ActionId::new_core(
                            "action.fighter.indomitable",
                        )))
                else {
                    panic!("Expected indomitable modifier to be applied");
                };

                // Modifier should be equal to the level of the fighter
                assert_eq!(modifier, 9);

                CallbackResult::None
            })),
        );

        // Failed save should trigger the Indomitable reaction
        assert_pending_reaction(&fighter, "action.fighter.indomitable");
        fighter.react().id("action.fighter.indomitable").submit_ok();

        // Indomitable should be triggered and consume the resources
        assert_no_pending_reaction(&fighter, "action.fighter.indomitable");
        assert_no_resource(&fighter, "resource.fighter.indomitable");
        assert_no_resource(&fighter, "resource.reaction");
    }
}
