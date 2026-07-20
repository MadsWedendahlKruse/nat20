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
    test_utils::scenario::{Operator, Scenario},
};
use rstest::rstest;

/// TODO: One test per spell is perhaps a bit overkill? Need to figure out how
/// to best organize these...

/// Wizard plus two goblins standing within 5 feet of each other, so both are
/// caught in the shard's explosion when it is aimed at goblin_1.
fn ice_knife_scenario(wizard_level: u8) -> Scenario {
    let mut scenario = Scenario::new();
    scenario
        .spawn("wizard", "hero.wizard")
        .level(wizard_level)
        .spawn();
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
    scenario
}

fn force_saving_throws(scenario: &mut Scenario, outcome: D20CheckOutcome) {
    for handle in ["goblin_1", "goblin_2"] {
        scenario.probe(handle).d20_force_outcome(
            D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Dexterity)),
            outcome,
        );
    }
}

fn cast_ice_knife(scenario: &mut Scenario, attack_outcome: D20CheckOutcome, slot_level: u8) {
    scenario
        .probe("wizard")
        .assert_action_available("action.ice_knife")
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Spell),
            attack_outcome,
        )
        .act("action.ice_knife")
        .context_spell_level(slot_level)
        .target_entity("goblin_1")
        .perform();
}

fn assert_damage_rolls(scenario: &Scenario, damage: DamageComponent, count: usize) {
    scenario
        .event_filter()
        .actor("wizard")
        .damage_roll(damage)
        .assert_event_count(count);
}

#[rstest]
#[case(1, 2)]
#[case(2, 3)] // +1d6 Cold per slot level above 1
fn ice_knife_hit(#[case] slot_level: u8, #[case] cold_dice_num: u32) {
    let mut scenario = ice_knife_scenario(5);
    force_saving_throws(&mut scenario, D20CheckOutcome::CriticalFailure);
    let goblin_1_hp = scenario.probe("goblin_1").hp();
    let goblin_2_hp = scenario.probe("goblin_2").hp();

    cast_ice_knife(&mut scenario, D20CheckOutcome::Success, slot_level);

    assert_damage_rolls(
        &scenario,
        DamageComponent::new(
            ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D10)),
            DamageType::Piercing,
        ),
        1,
    );
    // The explosion rolls damage per creature in the blast
    assert_damage_rolls(
        &scenario,
        DamageComponent::new(
            ModifierMap::from(
                ModifierSource::Base,
                DiceSet::new(cold_dice_num, DieSize::D6),
            ),
            DamageType::Cold,
        ),
        2,
    );

    scenario
        .probe("goblin_1")
        .assert_hp(Operator::Less(goblin_1_hp));
    scenario
        .probe("goblin_2")
        .assert_hp(Operator::Less(goblin_2_hp));
}

#[test]
fn ice_knife_miss_still_explodes() {
    let mut scenario = ice_knife_scenario(1);
    force_saving_throws(&mut scenario, D20CheckOutcome::CriticalFailure);
    let goblin_2_hp = scenario.probe("goblin_2").hp();

    cast_ice_knife(&mut scenario, D20CheckOutcome::Failure, 1);

    assert_damage_rolls(
        &scenario,
        DamageComponent::new(
            ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D10)),
            DamageType::Piercing,
        ),
        0,
    );
    assert_damage_rolls(
        &scenario,
        DamageComponent::new(
            ModifierMap::from(ModifierSource::Base, DiceSet::new(2, DieSize::D6)),
            DamageType::Cold,
        ),
        2,
    );

    scenario
        .probe("goblin_2")
        .assert_hp(Operator::Less(goblin_2_hp));
}

#[test]
fn ice_knife_save_negates_cold_damage() {
    let mut scenario = ice_knife_scenario(1);
    force_saving_throws(&mut scenario, D20CheckOutcome::CriticalSuccess);
    let goblin_1_hp = scenario.probe("goblin_1").hp();
    let goblin_2_hp = scenario.probe("goblin_2").hp();

    cast_ice_knife(&mut scenario, D20CheckOutcome::Failure, 1);

    assert_damage_rolls(
        &scenario,
        DamageComponent::new(
            ModifierMap::from(ModifierSource::Base, DiceSet::new(2, DieSize::D6)),
            DamageType::Cold,
        ),
        0,
    );

    scenario
        .probe("goblin_1")
        .assert_hp(Operator::Equal(goblin_1_hp));
    scenario
        .probe("goblin_2")
        .assert_hp(Operator::Equal(goblin_2_hp));
}
