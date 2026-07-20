extern crate nat20_core;

use nat20_core::{
    components::{
        ability::Ability,
        d20::{AdvantageType, D20CheckOutcome},
        damage::AttackSource,
        dice::{DiceSet, DieSize},
        items::equipment::weapon::WeaponKind,
        modifier::{ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
        skill::Skill,
        time::TimeMode,
    },
    systems::d20::{D20CheckDCKind, D20CheckKind},
    test_utils::scenario::{Operator, Scenario},
};

fn fighter_scenario(level: u8) -> Scenario {
    let mut scenario = Scenario::new();
    scenario
        .spawn("fighter", "hero.fighter")
        .level(level)
        .time_mode(TimeMode::TurnBased { encounter_id: None })
        .spawn();
    scenario
}

#[test]
fn fighter_action_surge() {
    let mut scenario = fighter_scenario(5);

    scenario
        .probe("fighter")
        .assert_action_available("action.fighter.action_surge")
        .assert_resource("resource.fighter.action_surge", Operator::Equal(1))
        .assert_resource("resource.action", Operator::Equal(1))
        .act("action.fighter.action_surge")
        .perform();

    // Check that the Action Surge effect is applied and the resources are updated
    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.action_surge")
        .assert_resource("resource.action", Operator::Equal(2))
        .assert_on_cooldown("action.fighter.action_surge")
        // Simulate the start of the turn to remove the Action Surge effect
        .start_turn()
        // Check that the Action Surge effect is removed after the turn starts
        .assert_no_effect("effect.fighter.action_surge")
        .assert_resource("resource.action", Operator::Equal(1))
        .assert_resource("resource.fighter.action_surge", Operator::Equal(0));
}

#[test]
fn fighter_second_wind() {
    let mut scenario = fighter_scenario(5);

    scenario
        .probe("fighter")
        .assert_action_available("action.fighter.second_wind")
        .assert_resource("resource.fighter.second_wind", Operator::Equal(3));

    // Let the fighter take some damage
    let max_hp = scenario.probe("fighter").max_hp();
    scenario
        .probe("fighter")
        .damage_raw(max_hp / 2)
        .assert_hp(Operator::Less(max_hp));

    let prev_hp = scenario.probe("fighter").hp();

    scenario
        .probe("fighter")
        .act("action.fighter.second_wind")
        .perform();

    scenario
        .probe("fighter")
        .assert_hp(Operator::Greater(prev_hp))
        .assert_resource("resource.fighter.second_wind", Operator::Equal(2));
}

#[test]
fn fighter_tactical_mind() {
    let mut scenario = Scenario::new();
    scenario.spawn("fighter", "hero.fighter").level(2).spawn();

    scenario
        .probe("fighter")
        .assert_action_available("action.fighter.tactical_mind")
        .assert_resource("resource.fighter.second_wind", Operator::Equal(2))
        // Force the fighter to fail a skill check to trigger Tactical Mind
        .d20_force_outcome(
            D20CheckKind::Skill(Skill::Stealth),
            D20CheckOutcome::CriticalFailure,
        )
        .d20_check(&D20CheckDCKind::skill_check(
            Skill::Stealth,
            ModifierMap::from(ModifierSource::Custom("Test skill check".into()), 3).evaluate(),
        ))
        .react()
        .option_id("action.fighter.tactical_mind")
        .perform();

    scenario
        .probe("fighter")
        .assert_resource("resource.fighter.second_wind", Operator::Equal(1))
        .assert_resource("resource.reaction", Operator::Equal(0));
    scenario
        .event_filter()
        .actor("fighter")
        .d20_modifier(
            D20CheckKind::Skill(Skill::Stealth),
            ModifierSource::Action("action.fighter.tactical_mind".into()),
            DiceSet::new(1, DieSize::D10),
        )
        .assert_event();
}

#[test]
fn fighter_tactical_shift() {
    let mut scenario = Scenario::new();
    scenario.spawn("fighter", "hero.fighter").level(5).spawn();

    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.tactical_shift")
        .act("action.fighter.second_wind")
        .perform();

    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.tactical_shift_disengage")
        .assert_free_movement(
            ModifierSource::Effect("effect.fighter.tactical_shift_disengage".into()),
            Operator::Equal(0.5),
        );
}

#[test]
fn fighter_extra_attack() {
    let mut scenario = fighter_scenario(5);

    scenario
        .probe("fighter")
        .assert_effect("effect.extra_attack")
        .assert_no_resource("resource.extra_attack")
        // Fighter makes a weapon attack, which costs one Action and grants one stack of Extra Attack
        .act("action.melee_attack")
        .target_point([1.0, 0.0, 0.0])
        .perform();

    scenario
        .probe("fighter")
        .assert_resource("resource.action", Operator::Equal(0))
        .assert_resource("resource.extra_attack", Operator::Equal(1))
        // Fighter makes another attack, which should consume the Extra Attack stack
        .act("action.melee_attack")
        .target_point([1.0, 0.0, 0.0])
        .perform();

    scenario
        .probe("fighter")
        .assert_resource("resource.extra_attack", Operator::Equal(0));
}

#[test]
fn fighter_two_extra_attacks() {
    let mut scenario = fighter_scenario(11);

    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.two_extra_attacks")
        .assert_no_resource("resource.extra_attack")
        .act("action.melee_attack")
        .target_point([1.0, 0.0, 0.0])
        .perform();

    scenario
        .probe("fighter")
        .assert_resource("resource.action", Operator::Equal(0))
        .assert_resource("resource.extra_attack", Operator::Equal(2))
        .act("action.melee_attack")
        .target_point([1.0, 0.0, 0.0])
        .perform();

    scenario
        .probe("fighter")
        .assert_resource("resource.extra_attack", Operator::Equal(1))
        .act("action.melee_attack")
        .target_point([1.0, 0.0, 0.0])
        .perform();

    scenario
        .probe("fighter")
        .assert_resource("resource.extra_attack", Operator::Equal(0));
}

#[test]
fn fighter_indomitable() {
    // Use Scenario for event log inspection
    let mut scenario = Scenario::new();
    scenario.spawn("fighter", "hero.fighter").level(9).spawn();

    let saving_throw = SavingThrowKind::Ability(Ability::Intelligence);

    scenario
        .probe("fighter")
        .assert_action_available("action.fighter.indomitable")
        .assert_resource("resource.fighter.indomitable", Operator::Equal(1))
        // Force the fighter to fail a saving throw to trigger Indomitable
        .d20_force_outcome(
            D20CheckKind::SavingThrow(saving_throw),
            D20CheckOutcome::CriticalFailure,
        )
        .d20_check(&D20CheckDCKind::saving_throw(
            saving_throw,
            ModifierMap::from(
                ModifierSource::Custom("Test saving throw DC".to_string()),
                99,
            )
            .evaluate(),
        ))
        .react()
        .option_id("action.fighter.indomitable")
        .perform();

    // Check that the saving throw is re-rolled and the resource is consumed
    scenario
        .probe("fighter")
        .assert_resource("resource.fighter.indomitable", Operator::Equal(0));

    scenario
        .event_filter()
        .actor("fighter")
        .d20_modifier(
            D20CheckKind::SavingThrow(saving_throw),
            ModifierSource::Action("action.fighter.indomitable".into()),
            9,
        )
        .assert_event();
}

#[test]
fn fighter_studied_attacks() {
    let mut scenario = Scenario::new();

    scenario
        .spawn("fighter", "hero.fighter")
        .level(13)
        .position([0.0, 0.0, 0.0], false)
        .spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(5)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.studied_attacks");

    scenario
        .encounter()
        .initiative_order(vec!["fighter", "goblin"])
        .build();

    // Force the fighter's first attack to miss to trigger Studied Attacks
    scenario.probe("fighter").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::CriticalFailure,
    );

    scenario
        .act("fighter", "action.melee_attack")
        .target_entity("goblin")
        .perform();

    // The effect is now applied to the *goblin*, not the fighter, and should
    // grant advantage on the fighter's next attack against the goblin
    scenario
        .probe("goblin")
        .assert_effect("effect.fighter.studied_attacks_advantage");

    // Prevent next attack from missing on a nat 1
    scenario.probe("fighter").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::CriticalSuccess,
    );

    scenario
        .act("fighter", "action.melee_attack")
        .target_entity("goblin")
        .perform();

    scenario
        .event_filter()
        .actor("fighter")
        .d20_advantage(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            ModifierSource::Effect("effect.fighter.studied_attacks_advantage".into()),
            AdvantageType::Advantage,
        )
        .assert_event();

    // After the attack the effect is consumed (and it's not just because the goblin died)
    scenario
        .probe("goblin")
        .assert_hp(Operator::Greater(0))
        .assert_no_effect("effect.fighter.studied_attacks_advantage");
}
