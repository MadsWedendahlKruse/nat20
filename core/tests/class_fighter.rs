extern crate nat20_core;

mod tests {
    use nat20_core::{
        components::{
            ability::Ability,
            d20::{AdvantageType, D20CheckOutcome},
            damage::AttackSource,
            items::equipment::weapon::WeaponKind,
            modifier::{ModifierSet, ModifierSource},
            saving_throw::SavingThrowKind,
            time::TimeMode,
        },
        engine::event::{EventFilter, EventKind},
        systems::d20::{D20CheckDCKind, D20CheckKind, D20ResultKind},
        test_utils::{
            creature_builder::CreatureBuilder, creature_probe::Operator, fixtures,
            scenario::Scenario,
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

    #[test]
    fn fighter_two_extra_attacks() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(11)
            .time_mode(TimeMode::TurnBased { encounter_id: None })
            .probe(&mut game_state);

        fighter.assert_effect(&game_state, "effect.fighter.two_extra_attacks");
        fighter.assert_no_resource(&game_state, "resource.extra_attack");

        fighter
            .act(&mut game_state, "action.melee_attack")
            .target_point(&mut game_state, [1.0, 0.0, 0.0])
            .perform_ok(&mut game_state);
        game_state.update(3.0);

        fighter.assert_resource(&game_state, "resource.action", Operator::Equal(0));
        fighter.assert_resource(&game_state, "resource.extra_attack", Operator::Equal(2));

        fighter
            .act(&mut game_state, "action.melee_attack")
            .target_point(&mut game_state, [1.0, 0.0, 0.0])
            .perform_ok(&mut game_state);
        game_state.update(3.0);

        fighter.assert_resource(&game_state, "resource.extra_attack", Operator::Equal(1));

        fighter
            .act(&mut game_state, "action.melee_attack")
            .target_point(&mut game_state, [1.0, 0.0, 0.0])
            .perform_ok(&mut game_state);
        game_state.update(3.0);

        fighter.assert_resource(&game_state, "resource.extra_attack", Operator::Equal(0));
    }

    #[test]
    fn fighter_indomitable() {
        // Use Scenario for event log inspection
        let mut scenario = Scenario::new();
        scenario.spawn("fighter", "hero.fighter").level(9).spawn();

        let saving_throw = SavingThrowKind::Ability(Ability::Intelligence);

        scenario
            .probe("fighter")
            .assert_has_action("action.fighter.indomitable")
            .assert_resource("resource.fighter.indomitable", Operator::Equal(1))
            // Force the fighter to fail a saving throw to trigger Indomitable
            .d20_force_outcome(
                D20CheckKind::SavingThrow(saving_throw),
                D20CheckOutcome::CriticalFailure,
            )
            .d20_check(&D20CheckDCKind::saving_throw(
                saving_throw,
                ModifierSet::from(
                    ModifierSource::Custom("Test saving throw DC".to_string()),
                    99,
                ),
            ))
            .react()
            .option_id("action.fighter.indomitable")
            .perform();

        // Check that the saving throw is re-rolled and the resource is consumed
        scenario
            .probe("fighter")
            .assert_resource("resource.fighter.indomitable", Operator::Equal(0));

        let fighter_entity = scenario.creatures.get("fighter").unwrap().creature.id();
        assert!(
            scenario
                .filter_events(EventFilter::new(move |event| {
                    if let EventKind::D20CheckResolved(actor, result, _) = &event.kind
                        && actor.id() == fighter_entity
                        && let D20ResultKind::SavingThrow { result, .. } = result
                        && result
                            .modifier_breakdown
                            .get(&ModifierSource::Action("action.fighter.indomitable".into()))
                            .map(|modifier| modifier == 9)
                            .unwrap_or(false)
                    {
                        true
                    } else {
                        false
                    }
                }))
                .is_some()
        );
    }

    #[test]
    fn fighter_studied_attacks() {
        let mut scenario = Scenario::new();

        scenario
            .spawn("fighter", "hero.fighter")
            .level(13)
            .position([0.0, 0.0, 0.0], false)
            .spawn();
        scenario
            .spawn("goblin", "monster.goblin_warrior")
            .level(5)
            .position([1.0, 0.0, 0.0], false)
            .spawn();

        scenario
            .probe("fighter")
            .assert_effect("effect.fighter.studied_attacks");

        scenario
            .encounter()
            .initiative_order(vec!["fighter", "goblin"])
            .build();

        // Force the fighter's first attack to miss to trigger Studied Attacks
        scenario.probe("fighter").d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            D20CheckOutcome::CriticalFailure,
        );

        scenario
            .act("fighter", "action.melee_attack")
            .target_entity("goblin")
            .perform();

        // The effect is now applied to the *goblin*, not the fighter, and should
        // grant advantage on the fighter's next attack against the goblin
        scenario
            .probe("goblin")
            .assert_effect("effect.fighter.studied_attacks_advantage");

        // Prevent next attack from missing on a nat 1
        scenario.probe("fighter").d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            D20CheckOutcome::CriticalSuccess,
        );

        scenario
            .act("fighter", "action.melee_attack")
            .target_entity("goblin")
            .perform();

        let fighter_entity = scenario.creatures.get("fighter").unwrap().creature.id();
        assert!(
            scenario
                .filter_events(EventFilter::new({
                    move |event| {
                        if let EventKind::D20CheckResolved(actor, result, _) = &event.kind
                            && actor.id() == fighter_entity
                            && let D20ResultKind::AttackRoll { result } = result
                            && result.roll_result.advantage_tracker.summary().contains(&(
                                &ModifierSource::Effect(
                                    "effect.fighter.studied_attacks_advantage".into(),
                                ),
                                AdvantageType::Advantage,
                            ))
                        {
                            return true;
                        } else {
                            return false;
                        };
                    }
                }))
                .is_some()
        );

        // After the attack the effect is consumed (and it's not just because the goblin died)
        scenario
            .probe("goblin")
            .is_alive()
            .assert_no_effect("effect.fighter.studied_attacks_advantage");
    }
}
