//! Opportunity attacks are quite unique, so they warrant a dedicated test suite

use nat20_core::{
    components::{
        d20::{D20CheckKind, D20CheckOutcome},
        damage::AttackSource,
        items::equipment::weapon::WeaponKind,
    },
    test_utils::scenario::{Operator, Scenario},
};
use parry3d::na::Point3;
use uom::si::{f32::Length, length::meter};

/// The reach of the fighter in meters: radius + 5 ft melee attack range.
const FIGHTER_REACH: f32 = 1.974;

fn opportunity_attack_scenario(goblin_position: impl Into<Point3<f32>>) -> Scenario {
    let mut scenario = Scenario::new();

    scenario
        .spawn("fighter", "hero.fighter")
        .position([0.0, 0.0, 0.0], true)
        .spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .position(goblin_position, true)
        .spawn();

    scenario
}

#[test]
fn opportunity_attack_way_outside_of_reach() {
    // Start the goblin well outside the fighter's reach
    let mut scenario = opportunity_attack_scenario([FIGHTER_REACH + 1.0, 0.0, 0.0]);

    // The goblin moves further away from the fighter, so no opportunity attack
    scenario.movement("goblin", [FIGHTER_REACH + 2.0, 0.0, 0.0]);
    scenario
        .probe("goblin")
        .assert_position([FIGHTER_REACH + 2.0, 0.0, 0.0], Length::new::<meter>(0.01));

    scenario
        .react("fighter")
        .option_index(0)
        .assert_perform_fails();
}

#[test]
fn opportunity_attack_move_into_reach() {
    // Start the goblin just outside the fighter's reach
    let mut scenario = opportunity_attack_scenario([FIGHTER_REACH + 0.1, 0.0, 0.0]);

    // The goblin moves into the fighter's reach, no opportunity attack
    scenario.movement("goblin", [FIGHTER_REACH - 0.1, 0.0, 0.0]);
    scenario
        .probe("goblin")
        .assert_position([FIGHTER_REACH - 0.1, 0.0, 0.0], Length::new::<meter>(0.01));

    scenario
        .react("fighter")
        .option_index(0)
        .assert_perform_fails();
}

#[test]
fn opportunity_attack_move_out_of_reach() {
    // Start the goblin within reach of the fighter
    let mut scenario = opportunity_attack_scenario([1.0, 0.0, 0.0]);

    // Just before the goblin leaves the fighter's reach, they stop moving and
    // wait for the fighter to decide if the want to take the opportunity attack
    scenario.movement("goblin", [3.0, 0.0, 0.0]);
    scenario
        .probe("goblin")
        .assert_position([FIGHTER_REACH, 0.0, 0.0], Length::new::<meter>(0.01));

    // Force fighter to miss so the goblin survives
    scenario.probe("fighter").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::Failure,
    );
    scenario.react("fighter").option_index(0).perform();

    // Goblin is alive and can continue moving without being attacked
    scenario.movement("goblin", [4.0, 0.0, 0.0]);
    scenario
        .probe("goblin")
        .assert_hp(Operator::AtLeast(0))
        .assert_position([4.0, 0.0, 0.0], Length::new::<meter>(0.01));
}

#[test]
fn opportunity_attack_move_inside_reach() {
    // Start the goblin inside the fighter's reach
    let mut scenario = opportunity_attack_scenario([FIGHTER_REACH - 0.1, 0.0, 0.0]);

    // The goblin moves further inside the fighter's reach, so no opportunity attack
    scenario.movement("goblin", [FIGHTER_REACH - 0.2, 0.0, 0.0]);
    scenario
        .probe("goblin")
        .assert_position([FIGHTER_REACH - 0.2, 0.0, 0.0], Length::new::<meter>(0.01));

    scenario
        .react("fighter")
        .option_index(0)
        .assert_perform_fails();
}

#[test]
fn opportunity_attack_move_across_reach() {
    // Start the goblin just outside the fighter's reach
    let mut scenario = opportunity_attack_scenario([FIGHTER_REACH + 0.1, 0.0, 0.0]);

    // The goblin moves across the fighter's reach, i.e. in an then out of reach,
    // which should trigger an opportunity attack. Note that they are again stopped
    // at the edge of the fighter's reach before moving out.
    scenario.movement("goblin", [-(FIGHTER_REACH + 0.1), 0.0, 0.0]);
    scenario
        .probe("goblin")
        .assert_position([-(FIGHTER_REACH), 0.0, 0.0], Length::new::<meter>(0.01));

    scenario.react("fighter").option_index(0).perform();
}

#[test]
fn opportunity_attack_decline() {
    // Start the goblin within reach of the fighter
    let mut scenario = opportunity_attack_scenario([1.0, 0.0, 0.0]);

    // The goblin moves out of the fighter's reach, but the fighter declines the opportunity attack
    scenario.movement("goblin", [3.0, 0.0, 0.0]);
    scenario
        .probe("goblin")
        .assert_position([FIGHTER_REACH, 0.0, 0.0], Length::new::<meter>(0.01));

    scenario.react("fighter").option_none().perform();

    // The goblin can continue moving without being attacked
    scenario.movement("goblin", [3.0, 0.0, 0.0]);
    scenario
        .probe("goblin")
        .assert_position([3.0, 0.0, 0.0], Length::new::<meter>(0.01));
}
