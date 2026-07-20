extern crate nat20_core;

use nat20_core::{
    components::{
        ability::Ability,
        d20::{AdvantageType, D20CheckOutcome},
        damage::{AttackSource, DamageComponent, DamageType},
        id::ItemId,
        items::equipment::{slots::EquipmentSlot, weapon::WeaponKind},
        modifier::{FlatModifierMap, ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
        skill::Skill,
        time::TimeMode,
    },
    systems::d20::D20CheckKind,
    test_utils::scenario::{Operator, Scenario},
};

fn barbarian_scenario() -> Scenario {
    let mut scenario = Scenario::new();
    scenario
        .spawn("barbarian", "hero.barbarian")
        .level(1)
        // Turn-based, so the real-time ticks in `perform` don't eat into the
        // Rage duration between the turn boundaries the tests drive manually
        .time_mode(TimeMode::TurnBased { encounter_id: None })
        .spawn();
    scenario
}

fn enter_rage(scenario: &mut Scenario) {
    scenario
        .probe("barbarian")
        .act("action.barbarian.rage")
        .perform();
    scenario
        .probe("barbarian")
        .assert_effect("effect.barbarian.rage");
}

fn melee_attack(scenario: &mut Scenario) {
    scenario
        .probe("barbarian")
        .act("action.melee_attack")
        .target_point([1.0, 0.0, 0.0])
        .perform();
}

#[test]
fn rage_costs_a_bonus_action_and_a_rage_charge() {
    let mut scenario = barbarian_scenario();

    scenario
        .probe("barbarian")
        .assert_resource("resource.barbarian.rage", Operator::Equal(2))
        .assert_resource("resource.bonus_action", Operator::Equal(1));

    enter_rage(&mut scenario);

    scenario
        .probe("barbarian")
        .assert_resource("resource.barbarian.rage", Operator::Equal(1))
        .assert_resource("resource.bonus_action", Operator::Equal(0));
}

#[test]
fn rage_grants_resistances_advantage_and_extend_rage() {
    let mut scenario = barbarian_scenario();

    scenario
        .probe("barbarian")
        .assert_action_unavailable("action.barbarian.extend_rage")
        .assert_no_damage_resistance(DamageType::Slashing);

    enter_rage(&mut scenario);

    let rage_source = ModifierSource::Effect("effect.barbarian.rage".into());
    scenario
        .probe("barbarian")
        .assert_damage_resistance(DamageType::Bludgeoning)
        .assert_damage_resistance(DamageType::Piercing)
        .assert_damage_resistance(DamageType::Slashing)
        // Only physical damage though
        .assert_no_damage_resistance(DamageType::Fire)
        .assert_d20_advantage(
            &D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Strength)),
            &rage_source,
            AdvantageType::Advantage,
        )
        .assert_d20_advantage(
            &D20CheckKind::Skill(Skill::Athletics),
            &rage_source,
            AdvantageType::Advantage,
        );

    // Extend Rage is granted by the effect, but its usability check keeps it
    // unavailable until the Rage is about to run out (see the extend test)
    scenario
        .probe("barbarian")
        .end_turn()
        .start_turn()
        .assert_action_available("action.barbarian.extend_rage");
}

#[test]
fn rage_bonus_damage_on_strength_attacks() {
    let mut scenario = barbarian_scenario();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    enter_rage(&mut scenario);

    scenario
        .probe("barbarian")
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            D20CheckOutcome::CriticalSuccess,
        )
        .act("action.melee_attack")
        .target_entity("goblin")
        .perform();

    // The greataxe damage gets a flat +2 from Rage at level 1
    scenario
        .event_filter()
        .actor("barbarian")
        .damage_roll(DamageComponent::new(
            ModifierMap::from(ModifierSource::Effect("effect.barbarian.rage".into()), 2),
            DamageType::Slashing,
        ))
        .assert_event();
}

#[test]
fn rage_lasts_until_the_end_of_the_next_turn() {
    let mut scenario = barbarian_scenario();

    enter_rage(&mut scenario);
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(2)
    );

    scenario.probe("barbarian").end_turn();
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(1)
    );

    scenario
        .probe("barbarian")
        .end_turn()
        .assert_no_effect("effect.barbarian.rage");
}

#[test]
fn rage_extends_when_making_an_attack_roll() {
    let mut scenario = barbarian_scenario();

    enter_rage(&mut scenario);

    // Attacking right after entering the Rage doesn't extend it past the next turn
    melee_attack(&mut scenario);
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(2)
    );

    scenario.probe("barbarian").end_turn();

    // Attacking on the next turn extends the Rage for another round
    scenario.probe("barbarian").start_turn();
    melee_attack(&mut scenario);
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(2)
    );
    scenario.probe("barbarian").end_turn();
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(1)
    );

    // A turn without any attacks lets the Rage run out
    scenario
        .probe("barbarian")
        .start_turn()
        .end_turn()
        .assert_no_effect("effect.barbarian.rage");
}

#[test]
fn rage_extends_with_the_extend_rage_action() {
    let mut scenario = barbarian_scenario();

    enter_rage(&mut scenario);
    scenario.probe("barbarian").end_turn();
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(1)
    );

    // Recharge the Bonus Action spent on entering the Rage
    scenario
        .probe("barbarian")
        .start_turn()
        .assert_action_available("action.barbarian.extend_rage")
        .act("action.barbarian.extend_rage")
        .perform();
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(2)
    );

    // Extending doesn't cost a Rage charge, and doesn't stack a second instance
    scenario
        .probe("barbarian")
        .assert_resource("resource.barbarian.rage", Operator::Equal(1))
        .assert_effect_instances("effect.barbarian.rage", 1);

    // Can't extend past the next turn, even with a Bonus Action available
    scenario
        .probe("barbarian")
        .start_turn()
        .assert_action_unavailable("action.barbarian.extend_rage");

    // Once the Rage runs out the granted action goes away with it
    scenario
        .probe("barbarian")
        .end_turn()
        .end_turn()
        .assert_no_effect("effect.barbarian.rage")
        .assert_action_unavailable("action.barbarian.extend_rage");
}

#[test]
fn rage_requires_no_heavy_armor_and_no_active_rage() {
    let mut scenario = barbarian_scenario();

    scenario
        .probe("barbarian")
        .equip("item.chainmail")
        .assert_action_unavailable("action.barbarian.rage")
        .unequip(&EquipmentSlot::Armor)
        .assert_action_available("action.barbarian.rage");

    enter_rage(&mut scenario);

    // Recharge the Bonus Action so only the usability check blocks a second Rage
    scenario
        .probe("barbarian")
        .start_turn()
        .assert_action_unavailable("action.barbarian.rage");
}

#[test]
fn rage_ends_when_equipping_heavy_armor() {
    let mut scenario = barbarian_scenario();

    enter_rage(&mut scenario);

    scenario
        .probe("barbarian")
        // Light armor doesn't end Rage
        .equip("item.studded_leather_armor")
        .assert_effect("effect.barbarian.rage")
        // Heavy armor does
        .equip("item.chainmail")
        .assert_no_effect("effect.barbarian.rage");
}

#[test]
fn rage_ends_when_incapacitated() {
    let mut scenario = barbarian_scenario();
    scenario
        .spawn("warlock", "hero.warlock")
        .level(4)
        .position([3.0, 0.0, 0.0], false)
        .spawn();

    enter_rage(&mut scenario);

    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Wisdom)),
        D20CheckOutcome::CriticalFailure,
    );

    // Hold Person also applies Incapacitated as a child effect
    scenario
        .probe("warlock")
        .assert_action_available("action.hold_person")
        .act("action.hold_person")
        .target_entity("barbarian")
        .perform();

    scenario
        .probe("barbarian")
        .assert_effect("effect.spell.hold_person")
        .assert_effect("effect.condition.paralyzed")
        .assert_effect("effect.condition.incapacitated")
        .assert_no_effect("effect.barbarian.rage");
}

#[test]
fn unarmored_defense() {
    let mut scenario = barbarian_scenario();

    scenario
        .probe("barbarian")
        .assert_armor_class(&FlatModifierMap::from_iter(vec![
            (ModifierSource::Base, 10),
            (ModifierSource::Ability(Ability::Dexterity), 2),
            (ModifierSource::Ability(Ability::Constitution), 3),
        ]))
        // Equipping armor removes the unarmored defense bonus
        .equip("item.chainmail")
        .assert_armor_class(&FlatModifierMap::from_iter(vec![(
            ModifierSource::Item(ItemId::new("nat20_core", "item.chainmail")),
            16,
        )]));
}
