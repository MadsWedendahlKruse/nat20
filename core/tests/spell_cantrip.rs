use nat20_core::{
    components::{
        ability::Ability,
        d20::D20CheckOutcome,
        damage::{AttackSource, DamageComponent, DamageType},
        dice::{DiceSet, DieSize},
        modifier::{ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
    },
    systems::d20::D20CheckKind,
    test_utils::{creature_probe::Operator, scenario::Scenario},
};
use rstest::rstest;
use uom::si::{f32::Length, length::foot};

#[test]
fn acid_splash() {
    let mut scenario = Scenario::new();
    scenario.spawn("wizard", "hero.wizard").level(1).spawn();
    // Spawn them next to each other so they both get hit
    scenario
        .spawn("goblin_1", "monster.goblin_warrior")
        .level(1)
        .position([3.0, 0.0, 0.5], false)
        .spawn();
    scenario
        .spawn("goblin_2", "monster.goblin_warrior")
        .level(1)
        .position([3.0, 0.0, -0.5], false)
        .spawn();

    let mut goblin_hp = Vec::new();
    for handle in ["goblin_1", "goblin_2"] {
        scenario.probe(handle).d20_force_outcome(
            D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Dexterity)),
            D20CheckOutcome::CriticalFailure,
        );
        goblin_hp.push(scenario.probe(handle).hp());
    }

    scenario
        .probe("wizard")
        .assert_action_available("action.acid_splash")
        .act("action.acid_splash")
        .target_point([3.0, 0.0, 0.0])
        .perform();

    for (handle, hp) in ["goblin_1", "goblin_2"].iter().zip(goblin_hp) {
        scenario
            .event_filter()
            .actor("wizard")
            .damage_dealt(
                *handle,
                DamageComponent::new(
                    ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D6)),
                    DamageType::Acid,
                ),
            )
            .assert_event();

        scenario.probe(*handle).assert_hp(Operator::Less(hp));
    }
}

#[rstest]
#[case(1, 1)]
#[case(5, 2)]
fn fire_bolt(#[case] wizard_level: u8, #[case] expected_dice_num: u32) {
    let mut scenario = Scenario::new();
    scenario
        .spawn("wizard", "hero.wizard")
        .level(wizard_level)
        .spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .spawn();

    scenario
        .probe("wizard")
        .assert_action_available("action.fire_bolt")
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Spell),
            D20CheckOutcome::Success,
        )
        .act("action.fire_bolt")
        .target_entity("goblin")
        .perform();

    scenario
        .event_filter()
        .actor("wizard")
        .damage_dealt(
            "goblin",
            DamageComponent::new(
                ModifierMap::from(
                    ModifierSource::Base,
                    DiceSet::new(expected_dice_num, DieSize::D10),
                ),
                DamageType::Fire,
            ),
        )
        .assert_event();
}

#[test]
fn ray_of_frost() {
    let mut scenario = Scenario::new();
    scenario.spawn("wizard", "hero.wizard").level(4).spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .spawn();

    let goblin_speed = scenario.probe("goblin").movement_speed();

    scenario
        .probe("wizard")
        .assert_action_available("action.ray_of_frost")
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Spell),
            D20CheckOutcome::Success,
        )
        .act("action.ray_of_frost")
        .target_entity("goblin")
        .perform();

    scenario
        .event_filter()
        .actor("wizard")
        .damage_dealt(
            "goblin",
            DamageComponent::new(
                ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D8)),
                DamageType::Cold,
            ),
        )
        .assert_event();

    scenario
        .probe("goblin")
        .assert_effect("effect.spell.ray_of_frost")
        .assert_movement_speed(Operator::Equal(goblin_speed - Length::new::<foot>(10.0)));
}
