use nat20_core::{
    components::{
        ability::Ability,
        d20::D20CheckOutcome,
        damage::{DamageComponent, DamageSource, DamageType},
        dice::{DiceSet, DieSize},
        modifier::{ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
    },
    systems::d20::D20CheckKind,
    test_utils::{creature_probe::Operator, scenario::Scenario},
};
use parry3d::na::Vector3;
use uom::si::{
    f32::Length,
    length::{foot, meter},
};

/// Displacement payloads (push via Thunderwave, teleport via Misty Step).

/// Level 7 is when the test wizard learns Thunderwave. Challenge rating 2
/// gives the goblin enough HP to survive a full 2d8.
fn thunderwave_scenario(goblin_save: D20CheckOutcome) -> Scenario {
    let mut scenario = Scenario::new();
    scenario
        .spawn("wizard", "hero.wizard")
        .level(7)
        .position([0.0, 0.0, 0.0], true)
        .spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(2)
        .position([1.0, 0.0, 0.0], true)
        .spawn();

    scenario.probe("goblin").d20_force_outcome(
        D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Constitution)),
        goblin_save,
    );
    scenario
}

fn cast_thunderwave(scenario: &mut Scenario) {
    scenario
        .act("wizard", "action.thunderwave")
        .context_spell_level(1)
        .target_point([1.0, 0.0, 0.0])
        .perform();
}

fn assert_thunder_rolls(scenario: &Scenario, count: usize) {
    scenario
        .event_filter()
        .actor("wizard")
        .damage_roll(
            DamageComponent::new(
                ModifierMap::from(ModifierSource::Base, DiceSet::new(2, DieSize::D8)),
                DamageType::Thunder,
            ),
            DamageSource::Spell("spell.thunderwave".into()),
        )
        .assert_event_count(count);
}

#[test]
fn thunderwave_pushes_on_failed_save() {
    let mut scenario = thunderwave_scenario(D20CheckOutcome::CriticalFailure);
    let goblin_hp = scenario.probe("goblin").hp();
    let goblin_start = scenario.probe("goblin").position();
    let goblin_end = goblin_start + Vector3::x() * Length::new::<foot>(10.0).get::<meter>();

    cast_thunderwave(&mut scenario);

    assert_thunder_rolls(&scenario, 1);
    // Pushed 10 feet directly away from the wizard, with some slack for the
    // physics of the push trajectory
    scenario
        .probe("goblin")
        .assert_hp(Operator::Less(goblin_hp))
        .assert_position(goblin_end, Length::new::<foot>(0.1));
}

#[test]
fn thunderwave_no_push_on_successful_save() {
    let mut scenario = thunderwave_scenario(D20CheckOutcome::CriticalSuccess);
    let goblin_hp = scenario.probe("goblin").hp();
    let goblin_start = scenario.probe("goblin").position();

    cast_thunderwave(&mut scenario);

    // Still takes half damage, but is not pushed
    assert_thunder_rolls(&scenario, 1);
    scenario
        .probe("goblin")
        .assert_hp(Operator::Less(goblin_hp))
        .assert_position(goblin_start, Length::new::<foot>(0.1));
}

#[test]
fn misty_step_teleports() {
    let mut scenario = Scenario::new();
    // Level 4 is when the test warlock learns Misty Step
    scenario
        .spawn("warlock", "hero.warlock")
        .level(4)
        .position([0.0, 0.0, 0.0], true)
        .spawn();

    let destination = [4.0, 0.0, 0.0];
    scenario
        .act("warlock", "action.misty_step")
        .context_spell_level(2)
        .target_point(destination)
        .perform();

    scenario
        .probe("warlock")
        .assert_position(destination, Length::new::<foot>(0.01))
        .assert_resource("resource.bonus_action", Operator::Equal(0));
}
