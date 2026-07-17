use nat20_core::{
    components::{
        damage::{DamageComponent, DamageSource, DamageType},
        dice::{DiceSet, DieSize},
        modifier::{ModifierKind, ModifierMap, ModifierSource},
    },
    test_utils::{creature_probe::Operator, scenario::Scenario},
};
use rstest::rstest;

/// Magic Missile and Shield in one file: Shield's Magic Missile immunity is
/// the interesting half of both spells.
fn assert_missile_rolls(scenario: &Scenario, count: usize) {
    scenario
        .event_filter()
        .actor("attacker")
        .damage_roll(
            DamageComponent::new(
                ModifierMap::from(
                    ModifierSource::Base,
                    ModifierKind::Composite(vec![
                        ModifierKind::Dice(DiceSet::new(1, DieSize::D4)),
                        ModifierKind::Flat(1),
                    ]),
                ),
                DamageType::Force,
            ),
            DamageSource::Spell("spell.magic_missile".into()),
        )
        .assert_event_count(count);
}

#[rstest]
#[case(1, 3)]
#[case(2, 4)] // One extra dart per spell slot level above 1
fn magic_missile_dart_count(#[case] slot_level: u8, #[case] darts: usize) {
    let mut scenario = Scenario::new();
    scenario.spawn("attacker", "hero.wizard").level(3).spawn();
    // A fighter has enough HP to survive all darts and no reaction to them
    scenario
        .spawn("target", "hero.fighter")
        .level(5)
        .position([3.0, 0.0, 0.0], false)
        .spawn();

    let target_hp = scenario.probe("target").hp();

    scenario
        .act("attacker", "action.magic_missile")
        .context_spell_level(slot_level)
        .target_entities(vec!["target"; darts])
        .perform();

    // The darts hit automatically, each rolling 1d4 + 1 Force damage
    assert_missile_rolls(&scenario, darts);
    scenario
        .probe("target")
        .assert_hp(Operator::AtMost(target_hp - 2 * darts as u32));
}

#[test]
fn shield_blocks_magic_missile() {
    let mut scenario = Scenario::new();
    scenario.spawn("attacker", "hero.wizard").level(1).spawn();
    // Level 2 so the defender knows Shield (and nothing else that could react)
    scenario
        .spawn("defender", "hero.wizard")
        .level(2)
        .position([3.0, 0.0, 0.0], false)
        .spawn();

    let defender_hp = scenario.probe("defender").hp();

    scenario
        .act("attacker", "action.magic_missile")
        .context_spell_level(1)
        .target_entities(vec!["defender"; 3])
        .perform();

    scenario
        .probe("defender")
        .react()
        .option_id("action.shield")
        .perform();

    // The darts still fly and roll damage, but Shield makes the defender
    // immune to all of it
    assert_missile_rolls(&scenario, 3);
    scenario
        .probe("defender")
        .assert_effect("effect.spell.shield")
        .assert_resource("resource.reaction", Operator::Equal(0))
        .assert_hp(Operator::Equal(defender_hp));

    // Shield lasts until the start of the defender's next turn
    scenario.probe("defender").start_turn();
    scenario
        .probe("defender")
        .assert_no_effect("effect.spell.shield");
}

#[test]
fn magic_missile_hits_when_reaction_declined() {
    let mut scenario = Scenario::new();
    scenario.spawn("attacker", "hero.wizard").level(1).spawn();
    scenario
        .spawn("defender", "hero.wizard")
        .level(2)
        .position([3.0, 0.0, 0.0], false)
        .spawn();

    let defender_hp = scenario.probe("defender").hp();

    scenario
        .act("attacker", "action.magic_missile")
        .context_spell_level(1)
        .target_entities(vec!["defender"; 3])
        .perform();

    scenario.probe("defender").react().option_none().perform();

    assert_missile_rolls(&scenario, 3);
    scenario
        .probe("defender")
        .assert_no_effect("effect.spell.shield")
        .assert_resource("resource.reaction", Operator::Equal(1))
        .assert_hp(Operator::AtMost(defender_hp - 6));
}
