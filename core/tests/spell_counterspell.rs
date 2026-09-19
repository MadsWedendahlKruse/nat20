use nat20_core::{
    components::{
        ability::Ability,
        activity::ActivityStateTag,
        d20::{D20CheckKind, D20CheckOutcome},
        damage::{DamageComponent, DamageType},
        dice::{DiceSet, DieSize},
        modifier::{ModifierKind, ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
    },
    test_utils::scenario::{Operator, Scenario},
};
use rstest::rstest;

/// Attacker is level 1 so it cannot counter the Counterspell in return;
/// defender is level 5 so it knows Counterspell.
fn counterspell_scenario() -> Scenario {
    let mut scenario = Scenario::new();
    scenario.spawn("attacker", "hero.wizard").level(1).spawn();
    scenario
        .spawn("defender", "hero.wizard")
        .level(5)
        .position([5.0, 0.0, 0.0], false)
        .spawn();
    scenario
}

/// Attacker casts Magic Missile at the defender, who counters it; the
/// attacker's forced Constitution save decides whether the counter succeeds.
fn cast_and_counter(scenario: &mut Scenario, attacker_save: D20CheckOutcome) {
    scenario.probe("attacker").d20_force_outcome(
        D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Constitution)),
        attacker_save,
    );

    scenario
        .act("attacker", "action.magic_missile")
        .context_spell_level(1)
        .target_entities(vec!["defender"; 3])
        .perform();

    scenario
        .probe("defender")
        .react()
        .option_id("action.counterspell")
        .perform();
}

fn assert_missile_rolls(scenario: &Scenario, count: usize) {
    scenario
        .event_filter()
        .actor("attacker")
        .damage_roll(DamageComponent::new(
            ModifierMap::from(
                ModifierSource::Base,
                ModifierKind::Composite(vec![
                    ModifierKind::Dice(DiceSet::new(1, DieSize::D4)),
                    ModifierKind::Flat(1),
                ]),
            ),
            DamageType::Force,
        ))
        .assert_event_count(count);
}

#[test]
fn counterspell_cancels_spell_on_failed_save() {
    let mut scenario = counterspell_scenario();
    let defender_hp = scenario.probe("defender").hp();
    let defender_slots_1 = scenario
        .probe("defender")
        .resource_tiered("resource.spell_slot", 1);
    let defender_slots_3 = scenario
        .probe("defender")
        .resource_tiered("resource.spell_slot", 3);
    let attacker_slots_1 = scenario
        .probe("attacker")
        .resource_tiered("resource.spell_slot", 1);

    cast_and_counter(&mut scenario, D20CheckOutcome::CriticalFailure);

    // The spell dissipates with no effect
    assert_missile_rolls(&scenario, 0);
    scenario
        .probe("defender")
        .assert_hp(Operator::Equal(defender_hp))
        .assert_resource("resource.reaction", Operator::Equal(0))
        // The counterspeller's own level 3 slot is spent...
        .assert_resource_tiered(
            "resource.spell_slot",
            3,
            Operator::Equal(defender_slots_3 - 1),
        )
        // ...and the countered spell's slot is definitely not refunded to them
        .assert_resource_tiered("resource.spell_slot", 1, Operator::Equal(defender_slots_1))
        .assert_activity_state(ActivityStateTag::Idle);
    // The countered spell's slot is refunded to its caster
    scenario
        .probe("attacker")
        .assert_resource_tiered("resource.spell_slot", 1, Operator::Equal(attacker_slots_1))
        .assert_activity_state(ActivityStateTag::Idle);
}

#[test]
fn counterspell_no_effect_on_successful_save() {
    let mut scenario = counterspell_scenario();
    let defender_hp = scenario.probe("defender").hp();
    let defender_slots_3 = scenario
        .probe("defender")
        .resource_tiered("resource.spell_slot", 3);
    let attacker_slots_1 = scenario
        .probe("attacker")
        .resource_tiered("resource.spell_slot", 1);

    cast_and_counter(&mut scenario, D20CheckOutcome::CriticalSuccess);

    // The counter failed, so the spell resolves as normal
    assert_missile_rolls(&scenario, 3);
    scenario
        .probe("defender")
        .assert_hp(Operator::AtMost(defender_hp - 6))
        .assert_resource("resource.reaction", Operator::Equal(0))
        // The counterspeller's slot is spent even though the counter failed
        .assert_resource_tiered(
            "resource.spell_slot",
            3,
            Operator::Equal(defender_slots_3 - 1),
        )
        .assert_activity_state(ActivityStateTag::Idle);
    // The caster's slot stays spent
    scenario
        .probe("attacker")
        .assert_resource_tiered(
            "resource.spell_slot",
            1,
            Operator::Equal(attacker_slots_1 - 1),
        )
        .assert_activity_state(ActivityStateTag::Idle);
}

fn multiple_wizards_scenario(con_saves: &[D20CheckOutcome]) -> Scenario {
    let num_wizards = con_saves.len();

    let mut scenario = Scenario::new();

    for i in 0..num_wizards {
        // Spawn them in a circle so they can see each other
        let angle = i as f32 * (360.0 / num_wizards as f32);
        let radius = 5.0;
        let x = radius * angle.to_radians().cos();
        let z = radius * angle.to_radians().sin();

        scenario
            .spawn(&format!("wizard{}", i + 1), "hero.wizard")
            .level(5)
            .position([x, 0.0, z], true)
            .spawn();
    }

    for (i, con_save) in con_saves.iter().enumerate() {
        scenario
            .probe(&format!("wizard{}", i + 1))
            .d20_force_outcome(
                D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Constitution)),
                *con_save,
            );
    }

    scenario
}

#[rstest]
#[case([D20CheckOutcome::Success, D20CheckOutcome::Success])]
#[case([D20CheckOutcome::Failure, D20CheckOutcome::Success])]
#[case([D20CheckOutcome::Failure, D20CheckOutcome::Failure])]
#[case([D20CheckOutcome::Success, D20CheckOutcome::Failure])]
fn double_counterspell(#[case] con_saves: [D20CheckOutcome; 2]) {
    // Spawn two wizards that both know counterspell
    let mut scenario = multiple_wizards_scenario(&con_saves);

    // Wizard 1 casts fireball at Wizard 2
    scenario
        .act("wizard1", "action.fireball")
        .target_entity("wizard2")
        .perform();

    // Wizard 2 reacts with counterspell
    scenario
        .react("wizard2")
        .option_id("action.counterspell")
        .perform();

    // Wizard 1 counterspells Wizard 2's counterspell
    scenario
        .react("wizard1")
        .option_id("action.counterspell")
        .perform();

    // Both wizards are now idle
    scenario
        .probe("wizard1")
        .assert_activity_state(ActivityStateTag::Idle);
    scenario
        .probe("wizard2")
        .assert_activity_state(ActivityStateTag::Idle);

    // Wizard 1 always casts counterspell
    scenario
        .event_filter()
        .actor("wizard1")
        .action_performed("action.counterspell")
        .assert_event_count(1);

    // Wizard 2's counterspell only goes off if they succeed on their Constitution
    // saving throw
    let wizard2_casts_counterspell = con_saves[1].is_success();
    scenario
        .event_filter()
        .actor("wizard2")
        .action_performed("action.counterspell")
        .assert_event_count(wizard2_casts_counterspell as usize);

    // Wizard 1's fireball only goes off if they succeed on their Constitution saving
    // or if Wizard 2's counterspell fails
    let fireball_was_cast = con_saves[0].is_success() || !wizard2_casts_counterspell;
    scenario
        .event_filter()
        .actor("wizard1")
        .action_performed("action.fireball")
        .assert_event_count(fireball_was_cast as usize);
}
