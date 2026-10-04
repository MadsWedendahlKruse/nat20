use nat20_core::{
    components::{
        ability::Ability,
        d20::{D20CheckKind, D20CheckOutcome, RollMode},
        saving_throw::SavingThrowKind,
    },
    test_utils::scenario::Scenario,
};
use rstest::rstest;

#[rstest]
// Removing whoever has the turn hands it to the next in the order
#[case("first", vec!["second", "third", "second"])]
#[case("second", vec!["first", "third", "first"])]
#[case("third", vec!["first", "second", "first"])]
fn despawned_participant_is_removed_from_the_turn_order(
    #[case] despawned: &str,
    #[case] turn_order: Vec<&str>,
) {
    let mut scenario = Scenario::new();
    scenario.spawn("first", "hero.fighter").level(1).spawn();
    scenario.spawn("second", "hero.wizard").level(1).spawn();
    scenario.spawn("third", "hero.warlock").level(1).spawn();

    scenario
        .encounter()
        .initiative_order(vec!["first", "second", "third"])
        .build();

    scenario.despawn(despawned);

    // Ending a turn panics unless it's that creature's turn
    for handle in turn_order {
        scenario.probe(handle).end_turn();
    }
}

#[test]
fn turn_boundary_events_belong_to_the_encounter() {
    let mut scenario = Scenario::new();
    scenario.spawn("warlock", "hero.warlock").level(4).spawn();
    scenario
        .spawn("barbarian", "hero.barbarian")
        .level(3)
        .position([3.0, 0.0, 0.0], false)
        .spawn();

    scenario
        .encounter()
        .initiative_order(vec!["warlock", "barbarian"])
        .build();

    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Wisdom)),
        D20CheckOutcome::Failure,
    );

    scenario
        .act("warlock", "action.hold_person")
        .target_entity("barbarian")
        .perform();

    scenario
        .probe("barbarian")
        .assert_effect("effect.spell.hold_person");
    scenario
        .event_filter()
        .in_encounter()
        .actor("barbarian")
        .d20_roll_mode(
            D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Wisdom)),
            RollMode::Normal,
        )
        .assert_event_count(1);

    // The barbarian is Paralyzed, so their turn is skipped, and Hold Person's
    // save is repeated at the end of it
    scenario.probe("warlock").end_turn();

    scenario
        .probe("barbarian")
        .assert_effect("effect.spell.hold_person");
    scenario
        .event_filter()
        .in_encounter()
        .actor("barbarian")
        .d20_roll_mode(
            D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Wisdom)),
            RollMode::Normal,
        )
        .assert_event_count(2);
}
