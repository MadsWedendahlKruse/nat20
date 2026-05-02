extern crate nat20_core;

mod tests {
    use nat20_core::{
        components::{
            actions::{action_builder::ActionBuilder, targeting::TargetInstance},
            damage::{DamageRoll, DamageSource, DamageType},
            health::hit_points::HitPoints,
            id::{ActionId, EffectId, ResourceId, SpellId},
            resource::{RESOURCE_ACTION, ResourceAmount, ResourceMap},
            time::{EntityClock, TimeMode, TimeStep, TurnBoundary},
        },
        systems,
        test_utils::{
            creature_builder::CreatureBuilder,
            creature_probe::{CreatureProbe, Operator},
            fixtures,
        },
    };

    #[test]
    fn fighter_action_surge() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .time_mode(TimeMode::TurnBased { encounter_id: None })
            .probe(&mut game_state);

        fighter.assert_has_action(&game_state, "action.fighter.action_surge");
        fighter.assert_resource(
            &game_state,
            "resource.fighter.action_surge",
            Operator::Equal(1),
        );
        fighter.assert_resource(&game_state, "resource.action", Operator::Equal(1));

        fighter
            .act(&mut game_state, "action.fighter.action_surge")
            .perform_ok(&mut game_state);
        game_state.update(3.0);

        // Check that the Action Surge effect is applied and the resources are updated
        fighter.assert_effect(&game_state, "effect.fighter.action_surge");
        fighter.assert_resource(&game_state, "resource.action", Operator::Equal(2));
        fighter.assert_on_cooldown(&game_state, "action.fighter.action_surge");

        // Simulate the start of the turn to remove the Action Surge effect
        fighter.start_turn(&mut game_state);

        // Check that the Action Surge effect is removed after the turn starts
        fighter.assert_no_effect(&game_state, "effect.fighter.action_surge");
        fighter.assert_resource(&game_state, "resource.action", Operator::Equal(1));
        fighter.assert_resource(
            &game_state,
            "resource.fighter.action_surge",
            Operator::Equal(0),
        );
    }

    #[test]
    fn fighter_second_wind() {
        let mut game_state = fixtures::engine::game_state();
        let mut fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .time_mode(TimeMode::TurnBased { encounter_id: None })
            .probe(&mut game_state);

        fighter.assert_has_action(&game_state, "action.fighter.second_wind");
        fighter.assert_resource(
            &game_state,
            "resource.fighter.second_wind",
            Operator::Equal(2),
        );

        // Let the fighter take some damage
        let max_hp = fighter.max_hp(&game_state);
        fighter.damage_raw(&mut game_state, max_hp / 2);
        fighter.assert_hp(&game_state, Operator::Less(max_hp));

        let prev_hp = fighter.hp(&game_state);

        fighter
            .act(&mut game_state, "action.fighter.second_wind")
            .perform_ok(&mut game_state);
        game_state.update(3.0);

        fighter.assert_hp(&game_state, Operator::Greater(prev_hp));
        fighter.assert_resource(
            &game_state,
            "resource.fighter.second_wind",
            Operator::Equal(1),
        );
    }

    #[test]
    fn fighter_extra_attack() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .time_mode(TimeMode::TurnBased { encounter_id: None })
            .probe(&mut game_state);

        fighter.assert_effect(&game_state, "effect.extra_attack");
        fighter.assert_no_resource(&game_state, "resource.extra_attack");

        // Fighter makes a weapon attack, which costs one Action and grants one stack of Extra Attack
        fighter
            .act(&mut game_state, "action.melee_attack")
            .target_point(&mut game_state, [1.0, 0.0, 0.0])
            .perform_ok(&mut game_state);
        game_state.update(3.0);

        fighter.assert_resource(&game_state, "resource.action", Operator::Equal(0));
        fighter.assert_resource(&game_state, "resource.extra_attack", Operator::Equal(1));

        // Fighter makes another attack, which should consume the Extra Attack stack
        fighter
            .act(&mut game_state, "action.melee_attack")
            .target_point(&mut game_state, [1.0, 0.0, 0.0])
            .perform_ok(&mut game_state);
        game_state.update(3.0);

        fighter.assert_resource(&game_state, "resource.extra_attack", Operator::Equal(0));
    }
}
