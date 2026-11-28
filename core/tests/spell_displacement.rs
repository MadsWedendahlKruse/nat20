use nat20_core::{
    components::{
        ability::Ability,
        d20::{D20CheckKind, D20CheckOutcome},
        damage::{DamageComponent, DamageType},
        dice::{DiceSet, DieSize},
        modifier::{ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
    },
    test_utils::scenario::{Operator, Scenario},
};
use parry3d::na::Vector3;
use uom::si::{
    f32::Length,
    length::{foot, meter},
};

fn thunderwave_scenario(dummy_save: D20CheckOutcome) -> Scenario {
    let mut scenario = Scenario::new();
    scenario
        .spawn_hero("wizard", "hero.wizard")
        // Level 7 is when the test wizard learns Thunderwave
        .level(7)
        .position([0.0, 0.0, 0.0], true)
        .spawn();
    scenario
        .spawn_monster("dummy", "monster.training_d")
        .position([1.0, 0.0, 0.0], true)
        .spawn();

    scenario.probe("dummy").d20_force_outcome(
        D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Constitution)),
        dummy_save,
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
        .damage_roll(DamageComponent::new(
            ModifierMap::from(ModifierSource::Base, DiceSet::new(2, DieSize::D8)),
            DamageType::Thunder,
        ))
        .assert_event_count(count);
}

#[test]
fn thunderwave_pushes_on_failed_save() {
    let mut scenario = thunderwave_scenario(D20CheckOutcome::CriticalFailure);
    let dummy_hp = scenario.probe("dummy").hp();
    let dummy_start = scenario.probe("dummy").position();
    let dummy_end = dummy_start + Vector3::x() * Length::new::<foot>(10.0).get::<meter>();

    cast_thunderwave(&mut scenario);

    assert_thunder_rolls(&scenario, 1);
    // Pushed 10 feet directly away from the wizard, with some slack for the
    // physics of the push trajectory
    scenario
        .probe("dummy")
        .assert_hp(Operator::Less(dummy_hp))
        .assert_position(dummy_end, Length::new::<foot>(0.1));
}

#[test]
fn thunderwave_no_push_on_successful_save() {
    let mut scenario = thunderwave_scenario(D20CheckOutcome::CriticalSuccess);
    let dummy_hp = scenario.probe("dummy").hp();
    let dummy_start = scenario.probe("dummy").position();

    cast_thunderwave(&mut scenario);

    // Still takes half damage, but is not pushed
    assert_thunder_rolls(&scenario, 1);
    scenario
        .probe("dummy")
        .assert_hp(Operator::Less(dummy_hp))
        .assert_position(dummy_start, Length::new::<foot>(0.1));
}

#[test]
fn misty_step_teleports() {
    let mut scenario = Scenario::new();
    // Level 4 is when the test warlock learns Misty Step
    scenario
        .spawn_hero("warlock", "hero.warlock")
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
