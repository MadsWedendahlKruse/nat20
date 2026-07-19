use nat20_core::{
    components::{
        ability::Ability,
        d20::D20CheckOutcome,
        damage::{DamageComponent, DamageType},
        dice::{DiceSet, DieSize},
        modifier::{ModifierKind, ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
    },
    systems::d20::D20CheckKind,
    test_utils::{creature_probe::Operator, scenario::Scenario},
};

// TODO: Counterspelling a counterspell

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
        .assert_resource_tiered("resource.spell_slot", 1, Operator::Equal(defender_slots_1));
    // The countered spell's slot is refunded to its caster
    scenario.probe("attacker").assert_resource_tiered(
        "resource.spell_slot",
        1,
        Operator::Equal(attacker_slots_1),
    );
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
        );
    // The caster's slot stays spent
    scenario.probe("attacker").assert_resource_tiered(
        "resource.spell_slot",
        1,
        Operator::Equal(attacker_slots_1 - 1),
    );
}
