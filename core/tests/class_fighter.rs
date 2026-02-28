extern crate nat20_core;

mod tests {

    use nat20_core::{
        components::{
            actions::targeting::TargetInstance,
            damage::{DamageRoll, DamageSource, DamageType},
            health::hit_points::HitPoints,
            id::{ActionId, EffectId, ResourceId, SpellId},
            resource::{ResourceAmount, ResourceMap},
            time::{EntityClock, TimeMode, TimeStep, TurnBoundary},
        },
        engine::action_prompt::{ActionData, ActionDecision, ActionDecisionKind},
        systems,
        test_utils::fixtures,
    };
    use parry3d::na::Point3;

    #[test]
    fn fighter_action_surge() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = fixtures::creatures::heroes::fighter(&mut game_state).id();

        // Check that the fighter has the Action Surge action
        let available_actions = systems::actions::available_actions(&game_state.world, fighter);
        let action_id = ActionId::new("nat20_core", "action.fighter.action_surge");
        assert!(
            available_actions.contains_key(&action_id),
            "Fighter should have Action Surge action"
        );
        let contexts_and_costs = available_actions.get(&action_id).unwrap();

        {
            let resources =
                systems::helpers::get_component::<ResourceMap>(&game_state.world, fighter);
            // Check that the fighter has one charge of Action Surge
            assert!(resources.can_afford(
                &ResourceId::new("nat20_core", "resource.fighter.action_surge"),
                &ResourceAmount::Flat(1),
            ));
            // Check that the fighter has one action before using Action Surge
            assert!(resources.can_afford(
                &ResourceId::new("nat20_core", "resource.action"),
                &ResourceAmount::Flat(1),
            ));
        }

        let _ = game_state.submit_decision(ActionDecision::without_response_to(
            ActionDecisionKind::Action {
                action: ActionData::new(
                    fighter,
                    action_id.clone(),
                    contexts_and_costs[0].0.clone(),
                    contexts_and_costs[0].1.clone(),
                    vec![TargetInstance::Entity(fighter)],
                ),
            },
        ));

        {
            // Check that the Action Surge effect is applied
            let effects = systems::effects::effects(&game_state.world, fighter);
            let action_surge_effect = effects.iter().find(|(_, e)| {
                e.effect_id == EffectId::new("nat20_core", "effect.fighter.action_surge")
            });
            assert!(
                action_surge_effect.is_some(),
                "Action Surge effect should be applied"
            );
        }

        // Check that the fighter has two actions after using Action Surge
        assert!(
            systems::helpers::get_component::<ResourceMap>(&game_state.world, fighter).can_afford(
                &ResourceId::new("nat20_core", "resource.action"),
                &ResourceAmount::Flat(2),
            ),
        );

        // Check that the Action Surge action is on cooldown
        assert!(systems::actions::on_cooldown(&game_state.world, fighter, &action_id).is_some());

        // Simulate the start of the turn to remove the Action Surge effect
        systems::helpers::get_component_mut::<EntityClock>(&mut game_state.world, fighter)
            .set_mode(TimeMode::TurnBased { encounter_id: None });
        systems::time::advance_time(
            &mut game_state,
            fighter,
            TimeStep::TurnBoundary {
                entity: fighter,
                boundary: TurnBoundary::Start,
            },
        );

        // Check that the Action Surge effect is removed after the turn starts
        let effects = systems::effects::effects(&game_state.world, fighter);
        let action_surge_effect = effects.iter().find(|(_, e)| {
            e.effect_id == EffectId::new("nat20_core", "effect.fighter.action_surge")
        });
        assert!(
            action_surge_effect.is_none(),
            "Action Surge effect should be removed. Remaining duration: {:?}",
            action_surge_effect.unwrap().1.lifetime
        );

        let resources = systems::helpers::get_component::<ResourceMap>(&game_state.world, fighter);
        // Check that the fighter has one action after the turn starts
        assert!(
            !resources.can_afford(
                &ResourceId::new("nat20_core", "resource.action"),
                &ResourceAmount::Flat(2),
            ) && resources.can_afford(
                &ResourceId::new("nat20_core", "resource.action"),
                &ResourceAmount::Flat(1),
            )
        );

        // Check that the Action Surge action is out of charges
        assert!(!resources.can_afford(
            &ResourceId::new("nat20_core", "resource.fighter.action_surge"),
            &ResourceAmount::Flat(1),
        ));
    }

    #[test]
    fn fighter_second_wind() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = fixtures::creatures::heroes::fighter(&mut game_state).id();

        // Check that the fighter has the Second Wind action
        let available_actions = systems::actions::available_actions(&game_state.world, fighter);
        let action_id = ActionId::new("nat20_core", "action.fighter.second_wind");
        assert!(
            available_actions.contains_key(&action_id),
            "Fighter should have Second Wind action"
        );
        let contexts_and_costs = available_actions.get(&action_id).unwrap();

        // Check that the fighter has two charges of Second Wind
        assert!(
            systems::helpers::get_component::<ResourceMap>(&game_state.world, fighter).can_afford(
                &ResourceId::new("nat20_core", "resource.fighter.second_wind"),
                &ResourceAmount::Flat(2),
            )
        );

        // Let the fighter take some damage
        systems::health::damage(
            &mut game_state,
            fighter,
            &DamageRoll::new(
                "1d4".parse().unwrap(),
                DamageType::Force,
                DamageSource::Spell(SpellId::new("nat20_core", "test.spell")),
            )
            .roll(false),
            None,
        );

        // Check that the fighter's HP is reduced
        let prev_hp = {
            let hit_points =
                systems::helpers::get_component::<HitPoints>(&game_state.world, fighter);
            assert!(hit_points.current() < hit_points.max());

            hit_points.current()
        };

        let result = systems::actions::perform_action(
            &mut game_state,
            &ActionData::new(
                fighter,
                action_id.clone(),
                contexts_and_costs[0].0.clone(),
                contexts_and_costs[0].1.clone(),
                vec![TargetInstance::Entity(fighter)],
            ),
        );
        println!("Second Wind Result: {:?}", result);

        // Check that the Fighters HP is increased by the Second Wind healing
        assert!(
            systems::helpers::get_component::<HitPoints>(&game_state.world, fighter).current()
                > prev_hp
        );
    }

    #[test]
    fn fighter_extra_attack() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = fixtures::creatures::heroes::fighter(&mut game_state).id();
        let _ = systems::health::heal_full(&mut game_state.world, fighter);

        // Check that the fighter has the Extra Attack effect
        {
            let effects = systems::effects::effects(&game_state.world, fighter);
            let extra_attack_effect = effects
                .iter()
                .find(|(_, e)| e.effect_id == EffectId::new("nat20_core", "effect.extra_attack"));
            assert!(
                extra_attack_effect.is_some(),
                "Fighter should have Extra Attack effect"
            );
        }

        // Check that the fighter has no stacks of Extra Attack (yet)
        assert!(
            !systems::helpers::get_component::<ResourceMap>(&game_state.world, fighter).can_afford(
                &ResourceId::new("nat20_core", "resource.extra_attack"),
                &ResourceAmount::Flat(1),
            ),
            "Fighter should have no stacks of Extra Attack"
        );

        // Fighter makes a weapon attack, which costs one Action and grants one stack of Extra Attack
        let available_actions = systems::actions::available_actions(&game_state.world, fighter);
        println!(
            "Available actions before attacking: {:#?}",
            available_actions
        );
        let action_id = ActionId::new("nat20_core", "action.melee_attack");
        assert!(
            available_actions.contains_key(&action_id),
            "Fighter should have Melee Attack action"
        );
        let contexts_and_costs = available_actions.get(&action_id).unwrap();
        let result = game_state.submit_decision(ActionDecision::without_response_to(
            ActionDecisionKind::Action {
                action: ActionData::new(
                    fighter,
                    action_id.clone(),
                    contexts_and_costs[0].0.clone(),
                    contexts_and_costs[0].1.clone(),
                    vec![TargetInstance::Point(Point3::new(1.0, 0.0, 0.0))],
                ),
            },
        ));
        assert!(
            result.is_ok(),
            "Performing melee attack action should succeed. Error: {:?}",
            result.err()
        );

        // Check that the fighter has one stack of Extra Attack
        assert!(
            systems::helpers::get_component::<ResourceMap>(&game_state.world, fighter).can_afford(
                &ResourceId::new("nat20_core", "resource.extra_attack"),
                &ResourceAmount::Flat(1),
            ),
            "Fighter should have one stack of Extra Attack"
        );
        // Check that the fighter has no Actions left
        assert!(
            !systems::helpers::get_component::<ResourceMap>(&game_state.world, fighter).can_afford(
                &ResourceId::new("nat20_core", "resource.action"),
                &ResourceAmount::Flat(1),
            ),
            "Fighter should have no Actions left"
        );

        // Fighter makes another attack, which should consume the Extra Attack stack
        let available_actions = systems::actions::available_actions(&game_state.world, fighter);
        assert!(
            available_actions.contains_key(&action_id),
            "Fighter should have Melee Attack action available for second attack"
        );
        let contexts_and_costs = available_actions.get(&action_id).unwrap();
        let result = game_state.submit_decision(ActionDecision::without_response_to(
            ActionDecisionKind::Action {
                action: ActionData::new(
                    fighter,
                    action_id.clone(),
                    contexts_and_costs[0].0.clone(),
                    contexts_and_costs[0].1.clone(),
                    vec![TargetInstance::Point(Point3::new(1.0, 0.0, 0.0))],
                ),
            },
        ));
        assert!(
            result.is_ok(),
            "Performing second melee attack action should succeed. Error: {:?}",
            result.err()
        );

        // Check that the fighter has no stacks of Extra Attack left
        assert!(
            !systems::helpers::get_component::<ResourceMap>(&game_state.world, fighter).can_afford(
                &ResourceId::new("nat20_core", "resource.extra_attack"),
                &ResourceAmount::Flat(1),
            ),
            "Fighter should have no stacks of Extra Attack left"
        );
    }
}
