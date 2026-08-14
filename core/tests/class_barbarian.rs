extern crate nat20_core;

use nat20_core::{
    components::{
        ability::Ability,
        d20::{AdvantageAware, AdvantageType, D20CheckDC, D20CheckKind, D20CheckOutcome, RollMode},
        damage::{AttackSource, DamageComponent, DamageType},
        dice::{DiceSet, DieSize},
        id::ItemId,
        items::equipment::{slots::EquipmentSlot, weapon::WeaponKind},
        modifier::{FlatModifierMap, Modifiable, ModifierKind, ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
        skill::Skill,
        time::TimeMode,
    },
    test_utils::scenario::{Operator, Scenario},
};
use rstest::rstest;
use uom::si::{f32::Length, length::foot, length::meter};

fn barbarian_scenario(level: u8) -> Scenario {
    let mut scenario = Scenario::new();
    scenario
        .spawn("barbarian", "hero.barbarian")
        .level(level)
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
    let mut scenario = barbarian_scenario(3);

    let rage_charges = scenario
        .probe("barbarian")
        .resource("resource.barbarian.rage");

    scenario
        .probe("barbarian")
        .assert_resource("resource.barbarian.rage", Operator::Equal(rage_charges))
        .assert_resource("resource.bonus_action", Operator::Equal(1));

    enter_rage(&mut scenario);

    scenario
        .probe("barbarian")
        .assert_resource("resource.barbarian.rage", Operator::Equal(rage_charges - 1))
        .assert_resource("resource.bonus_action", Operator::Equal(0));
}

#[test]
fn rage_grants_resistances_advantage_and_extend_rage() {
    let mut scenario = barbarian_scenario(3);

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
        .assert_action_available("action.barbarian.extend_rage");
}

#[test]
fn rage_bonus_damage_on_strength_attacks() {
    let mut scenario = barbarian_scenario(3);
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
    let mut scenario = barbarian_scenario(3);

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
    let mut scenario = barbarian_scenario(3);

    enter_rage(&mut scenario);

    // Attacking right after entering the Rage doesn't extend it past the next turn
    melee_attack(&mut scenario);
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(2)
    );

    // Attacking on the next turn extends the Rage for another round
    scenario.probe("barbarian").end_turn();
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
        .end_turn()
        .assert_no_effect("effect.barbarian.rage");
}

#[test]
fn rage_extends_with_the_extend_rage_action() {
    let mut scenario = barbarian_scenario(3);

    let rage_charges = scenario
        .probe("barbarian")
        .resource("resource.barbarian.rage");

    enter_rage(&mut scenario);

    scenario
        .probe("barbarian")
        .assert_resource("resource.barbarian.rage", Operator::Equal(rage_charges - 1))
        // Also recharges the Bonus Action spent on entering the Rage
        .end_turn();
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(1)
    );

    scenario
        .probe("barbarian")
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
        .assert_resource("resource.barbarian.rage", Operator::Equal(rage_charges - 1))
        .assert_effect_instances("effect.barbarian.rage", 1);

    // Extending again on a later turn tops the Rage back up, but never past the
    // end of the next turn
    scenario
        .probe("barbarian")
        .end_turn()
        .act("action.barbarian.extend_rage")
        .perform();
    assert_eq!(
        scenario
            .probe("barbarian")
            .effect_remaining_turns("effect.barbarian.rage"),
        Some(2)
    );

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
    let mut scenario = barbarian_scenario(3);

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
        .end_turn()
        .assert_action_unavailable("action.barbarian.rage");
}

/// Barbarians have no spells of their own, so borrow a wizard to check that the
/// Rage effect prevents spellcasting
#[test]
fn rage_blocks_spellcasting() {
    let mut scenario = Scenario::new();
    scenario.spawn("wizard", "hero.wizard").level(3).spawn();

    scenario
        .probe("wizard")
        .assert_action_available("action.fire_bolt")
        .apply_effect("effect.barbarian.rage")
        .assert_action_unavailable("action.fire_bolt");
}

#[test]
fn rage_ends_when_equipping_heavy_armor() {
    let mut scenario = barbarian_scenario(3);

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
    let mut scenario = barbarian_scenario(3);
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
    let mut scenario = barbarian_scenario(3);

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

#[test]
fn reckless_attack_reaction() {
    let mut scenario = barbarian_scenario(3);
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    // Force barbarian to miss to trigger the reaction
    scenario
        .probe("barbarian")
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            D20CheckOutcome::Failure,
        )
        .act("action.melee_attack")
        .target_entity("goblin")
        .perform();

    scenario.react("barbarian").option_index(0).perform();

    scenario
        .probe("barbarian")
        .assert_effect("effect.barbarian.reckless_attack_advantage");

    scenario
        .event_filter()
        .actor("barbarian")
        .d20_advantage(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            ModifierSource::Effect("effect.barbarian.reckless_attack_advantage".into()),
            AdvantageType::Advantage,
        )
        .assert_event();

    // The effect is removed at the start of the next turn
    scenario.probe("barbarian").end_turn();
    scenario
        .probe("barbarian")
        .assert_no_effect("effect.barbarian.reckless_attack_advantage");
}

#[test]
fn reckless_attack_advantage_preview() {
    let mut scenario = barbarian_scenario(3);
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(20)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    let probe = scenario.probe("barbarian");
    assert_eq!(
        probe
            .preview_attack_roll(
                "goblin",
                "action.barbarian.reckless_attack",
                EquipmentSlot::MeleeMainHand
            )
            .roll_mode(),
        RollMode::Advantage
    );
    // Only for the action that grants it
    assert_eq!(
        probe
            .preview_attack_roll(
                "goblin",
                "action.melee_attack",
                EquipmentSlot::MeleeMainHand
            )
            .roll_mode(),
        RollMode::Normal
    );

    // Once the effect is up it takes over rather than double dipping
    scenario
        .probe("barbarian")
        .apply_effect("effect.barbarian.reckless_attack_advantage");

    let check = scenario.probe("barbarian").preview_attack_roll(
        "goblin",
        "action.barbarian.reckless_attack",
        EquipmentSlot::MeleeMainHand,
    );
    assert_eq!(check.roll_mode(), RollMode::Advantage);
    assert_eq!(check.advantage_tracker().summary().len(), 1);
}

#[test]
fn reckless_attack_action() {
    let mut scenario = barbarian_scenario(3);
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    scenario
        .probe("barbarian")
        .assert_action_available("action.barbarian.reckless_attack")
        .act("action.barbarian.reckless_attack")
        .context_filter(|context, _cost| context.is_weapon_attack() && context.is_melee_attack())
        .target_entity("goblin")
        .perform();

    scenario
        .probe("barbarian")
        .assert_effect("effect.barbarian.reckless_attack_advantage");

    scenario
        .event_filter()
        .actor("barbarian")
        .d20_advantage(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            ModifierSource::Effect("effect.barbarian.reckless_attack_advantage".into()),
            AdvantageType::Advantage,
        )
        .assert_event();
}

#[test]
fn danger_sense() {
    let mut scenario = barbarian_scenario(3);

    scenario.probe("barbarian").assert_d20_advantage(
        &D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Dexterity)),
        &ModifierSource::Effect("effect.barbarian.danger_sense".into()),
        AdvantageType::Advantage,
    );

    // Danger Sense is disabled when incapacitated
    scenario
        .spawn("warlock", "hero.warlock")
        .level(4)
        .position([3.0, 0.0, 0.0], false)
        .spawn();

    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Wisdom)),
        D20CheckOutcome::CriticalFailure,
    );

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
        .assert_d20_no_advantage(
            &D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Dexterity)),
            &ModifierSource::Effect("effect.barbarian.danger_sense".into()),
            AdvantageType::Advantage,
        );
}

#[test]
fn fast_movement() {
    // Dragonborn base speed
    let base_speed = Length::new::<foot>(30.0);

    let mut scenario = barbarian_scenario(3);
    scenario
        .probe("barbarian")
        .assert_no_effect("effect.barbarian.fast_movement")
        .assert_movement_speed(Operator::Equal(base_speed));

    let mut scenario = barbarian_scenario(5);
    scenario
        .probe("barbarian")
        .assert_effect("effect.barbarian.fast_movement")
        .assert_movement_speed(Operator::Equal(base_speed + Length::new::<foot>(10.0)))
        // Light armor keeps the bonus, Heavy armor suppresses it
        .equip("item.studded_leather_armor")
        .assert_movement_speed(Operator::Equal(base_speed + Length::new::<foot>(10.0)))
        .equip("item.chainmail")
        .assert_movement_speed(Operator::Equal(base_speed))
        // ...and taking it off brings it back, without the effect ever being re-applied
        .unequip(&EquipmentSlot::Armor)
        .assert_effect("effect.barbarian.fast_movement")
        .assert_movement_speed(Operator::Equal(base_speed + Length::new::<foot>(10.0)));
}

#[test]
fn primal_knowledge() {
    let mut scenario = barbarian_scenario(3);

    scenario
        .probe("barbarian")
        .assert_effect("effect.barbarian.primal_knowledge");

    // Skills affected by Primal Knowledge
    let skills = [
        Skill::Acrobatics,
        Skill::Intimidation,
        Skill::Perception,
        Skill::Stealth,
        Skill::Survival,
    ];

    // Regular abilities before entering Rage
    for skill in &skills {
        scenario
            .probe("barbarian")
            .assert_d20_ability(&D20CheckKind::Skill(skill.clone()), &skill.ability());
    }

    enter_rage(&mut scenario);

    // Ability is replaced with Strength after entering Rage
    for skill in &skills {
        scenario
            .probe("barbarian")
            .assert_d20_ability(&D20CheckKind::Skill(skill.clone()), &Ability::Strength);
    }
}

#[test]
fn instinctive_pounce() {
    let mut scenario = barbarian_scenario(7);

    // Base speed is 30 feet for a Dragonborn + 10 feet from Fast Movement at level 5
    let base_speed = Length::new::<foot>(30.0 + 10.0);

    scenario
        .probe("barbarian")
        .assert_effect("effect.barbarian.instinctive_pounce")
        .assert_movement_speed(Operator::Equal(base_speed));

    enter_rage(&mut scenario);

    // Instinctive Pounce adds half the base speed to the total speed when entering Rage
    scenario
        .probe("barbarian")
        .assert_effect("effect.barbarian.rage")
        .assert_effect("effect.barbarian.instinctive_pounce_active")
        .assert_movement_speed(Operator::Equal(base_speed + Length::new::<foot>(20.0)));

    // Speed disappears after the first turn
    scenario.probe("barbarian").end_turn();
    scenario
        .probe("barbarian")
        .assert_no_effect("effect.barbarian.instinctive_pounce_active")
        .assert_movement_speed(Operator::Equal(base_speed));
}

fn brutal_strike_scenario(level: u8) -> Scenario {
    let mut scenario = barbarian_scenario(level);
    // A high-level goblin so it doesn't get one-shot
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(20)
        .position([1.0, 0.0, 0.0], false)
        .spawn();
    scenario
}

/// From level 13 the improved version replaces the original
fn brutal_strike_action(scenario: &mut Scenario) -> &'static str {
    if scenario
        .probe("barbarian")
        .has_effect("effect.barbarian.improved_brutal_strike")
    {
        "action.barbarian.improved_brutal_strike"
    } else {
        "action.barbarian.brutal_strike"
    }
}

fn brutal_strike(scenario: &mut Scenario, variant: &str) {
    let action = brutal_strike_action(scenario);

    scenario
        .probe("barbarian")
        .assert_action_available(action)
        .act(action)
        .variant(format!("action.barbarian.brutal_strike.{}", variant).as_str())
        // Brutal Strike also works with an Unarmed Strike, so the greataxe has to
        // be picked explicitly
        .context_filter(|context, _cost| context.is_weapon_attack() && context.is_melee_attack())
        .target_entity("goblin")
        .perform();
}

/// Using Brutal Strike without Reckless Attack already active gives advantage and
/// then immediately forgoes it
#[test]
fn brutal_strike_grants_reckless_attack_advantage_and_forgoes_it() {
    let mut scenario = brutal_strike_scenario(9);

    scenario
        .probe("barbarian")
        .assert_no_effect("effect.barbarian.reckless_attack_advantage");

    brutal_strike(&mut scenario, "forceful_blow");

    // The Brutal Strike roll itself gave up the Advantage...
    scenario
        .event_filter()
        .actor("barbarian")
        .d20_roll_mode(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            RollMode::Normal,
        )
        .assert_event_count(1);

    // ...but the rest of the turn is Reckless as usual
    scenario
        .probe("barbarian")
        .assert_effect("effect.barbarian.reckless_attack_advantage")
        .act("action.melee_attack")
        .target_entity("goblin")
        .perform();

    scenario
        .event_filter()
        .actor("barbarian")
        .d20_roll_mode(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            RollMode::Advantage,
        )
        .assert_event_count(1);
}

#[test]
fn brutal_strike_cant_have_disadvantage() {
    let mut scenario = brutal_strike_scenario(9);

    // Poisoned gives Disadvantage on attack rolls
    scenario
        .probe("barbarian")
        .apply_effect("effect.condition.poisoned");

    scenario
        .probe("barbarian")
        .act("action.barbarian.brutal_strike")
        .variant("action.barbarian.brutal_strike.forceful_blow")
        .context_filter(|context, _cost| context.is_weapon_attack() && context.is_melee_attack())
        .target_entity("goblin")
        .assert_perform_fails();
}

#[test]
fn brutal_strike_forgoes_advantage_already_granted_by_reckless_attack() {
    let mut scenario = brutal_strike_scenario(9);
    scenario
        .probe("barbarian")
        .apply_effect("effect.barbarian.reckless_attack_advantage");

    scenario
        .probe("barbarian")
        .act("action.melee_attack")
        .target_entity("goblin")
        .perform();

    brutal_strike(&mut scenario, "forceful_blow");

    // The plain attack rolled with Advantage, the Brutal Strike without
    let attack_roll = D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee));
    scenario
        .event_filter()
        .actor("barbarian")
        .d20_roll_mode(attack_roll.clone(), RollMode::Advantage)
        .assert_event_count(1);
    scenario
        .event_filter()
        .actor("barbarian")
        .d20_roll_mode(attack_roll.clone(), RollMode::Normal)
        .assert_event_count(1);

    // Both rolls still list Reckless Attack, but Brutal Strike gave the Advantage
    // up rather than removing it
    scenario
        .event_filter()
        .actor("barbarian")
        .d20_advantage(
            attack_roll,
            ModifierSource::Effect("effect.barbarian.reckless_attack_advantage".into()),
            AdvantageType::Advantage,
        )
        .assert_event_count(2);
}

#[test]
fn brutal_strike_once_per_turn() {
    let mut scenario = brutal_strike_scenario(9);

    scenario
        .probe("barbarian")
        .assert_resource("resource.barbarian.brutal_strike", Operator::Equal(1));

    brutal_strike(&mut scenario, "hamstring_blow");

    scenario
        .probe("barbarian")
        .assert_resource("resource.barbarian.brutal_strike", Operator::Equal(0))
        .assert_action_unavailable("action.barbarian.brutal_strike")
        .end_turn()
        .assert_resource("resource.barbarian.brutal_strike", Operator::Equal(1));
}

#[rstest]
#[case(9, 1)]
#[case(17, 2)]
fn brutal_strike_extra_weapon_type_damage(#[case] level: u8, #[case] dice_count: u32) {
    let mut scenario = brutal_strike_scenario(level);

    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::Success,
    );

    brutal_strike(&mut scenario, "hamstring_blow");

    // Greataxe deals Slashing, so the extra d10s are Slashing too
    scenario
        .event_filter()
        .actor("barbarian")
        .damage_roll(DamageComponent::new(
            ModifierMap::from(
                ModifierSource::Effect("effect.barbarian.brutal_strike".into()),
                DiceSet::new(dice_count, DieSize::D10),
            ),
            DamageType::Slashing,
        ))
        .assert_event();
}

#[test]
fn brutal_strike_forceful_blow() {
    let mut scenario = brutal_strike_scenario(9);

    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::Success,
    );

    brutal_strike(&mut scenario, "forceful_blow");

    // Pushed 15 feet straight away from the barbarian, who stands at the origin
    scenario.probe("goblin").assert_position(
        [1.0 + Length::new::<foot>(15.0).get::<meter>(), 0.0, 0.0],
        Length::new::<foot>(0.1),
    );

    scenario.probe("barbarian").assert_free_movement(
        ModifierSource::Effect("effect.barbarian.brutal_strike.forceful_blow_disengage".into()),
        Operator::Equal(0.5),
    );
}

#[test]
fn brutal_strike_hamstring_blow() {
    let mut scenario = brutal_strike_scenario(9);

    let goblin_speed = scenario.probe("goblin").movement_speed();

    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::Success,
    );

    brutal_strike(&mut scenario, "hamstring_blow");

    scenario
        .probe("goblin")
        .assert_movement_speed(Operator::Equal(goblin_speed - Length::new::<foot>(15.0)));

    // Only one Hamstring Blow at a time - the most recent one
    scenario.probe("barbarian").end_turn();
    brutal_strike(&mut scenario, "hamstring_blow");

    scenario
        .probe("goblin")
        .assert_effect_instances("effect.barbarian.brutal_strike.hamstring_blow", 1)
        .assert_movement_speed(Operator::Equal(goblin_speed - Length::new::<foot>(15.0)));
}

#[test]
fn brutal_strike_staggering_blow() {
    let mut scenario = brutal_strike_scenario(13);

    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::Success,
    );

    brutal_strike(&mut scenario, "staggering_blow");

    scenario
        .probe("goblin")
        .assert_effect("effect.barbarian.brutal_strike.staggering_blow")
        // Only Opportunity Attacks are blocked, the reaction itself is still there
        .assert_action_unavailable("action.opportunity_attack")
        .assert_resource("resource.reaction", Operator::Equal(1));

    let saving_throw = SavingThrowKind::Ability(Ability::Dexterity);
    scenario
        .probe("goblin")
        .d20_check(&D20CheckDC::SavingThrow {
            saving_throw,
            dc: ModifierMap::from(ModifierSource::Custom("Test save DC".to_string()), 10)
                .evaluate(),
        });

    scenario
        .event_filter()
        .actor("goblin")
        .d20_advantage(
            D20CheckKind::SavingThrow(saving_throw),
            ModifierSource::Effect("effect.barbarian.brutal_strike.staggering_blow".into()),
            AdvantageType::Disadvantage,
        )
        .assert_event();

    // Spent by that one save
    scenario
        .probe("goblin")
        .assert_no_effect("effect.barbarian.brutal_strike.staggering_blow")
        .assert_action_available("action.opportunity_attack");
}

#[test]
fn brutal_strike_sundering_blow() {
    let mut scenario = brutal_strike_scenario(13);
    scenario
        .spawn("ally", "hero.fighter")
        .level(1)
        .position([2.0, 0.0, 0.0], false)
        .spawn();

    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::Success,
    );

    brutal_strike(&mut scenario, "sundering_blow");

    scenario
        .probe("goblin")
        .assert_effect("effect.barbarian.brutal_strike.sundering_blow");

    // Barbarian's own attacks don't get the Sundering Blow bonus, only allies' do
    for (has_bonus, handle) in [(false, "barbarian"), (true, "ally")] {
        assert_eq!(
            scenario
                .probe(handle)
                .preview_attack_roll(
                    "goblin",
                    "action.melee_attack",
                    EquipmentSlot::MeleeMainHand
                )
                .modifiers()
                .get(&ModifierSource::Effect(
                    "effect.barbarian.brutal_strike.sundering_blow".into(),
                )),
            if has_bonus {
                Some(&ModifierKind::Flat(5))
            } else {
                None
            }
        );
    }
}
