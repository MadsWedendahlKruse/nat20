extern crate nat20_core;

use nat20_core::{
    components::{
        d20::{AdvantageType, D20CheckOutcome},
        damage::AttackSource,
        items::equipment::weapon::WeaponKind,
        modifier::ModifierSource,
        saving_throw::SavingThrowKind,
        skill::Skill,
    },
    systems::d20::D20CheckKind,
    test_utils::scenario::{Operator, Scenario},
};

fn fighter_scenario(level: u8) -> Scenario {
    let mut scenario = Scenario::new();
    scenario.spawn("fighter", "hero.fighter").level(level).spawn();
    scenario
}

#[test]
fn champion_improved_critical() {
    let mut scenario = fighter_scenario(3);

    let source = ModifierSource::Effect("effect.fighter.champion.improved_critical".into());
    let mut probe = scenario.probe("fighter");
    probe.assert_effect("effect.fighter.champion.improved_critical");
    for weapon_kind in [WeaponKind::Melee, WeaponKind::Ranged, WeaponKind::Unarmed] {
        probe.assert_d20_crit_threshold_reduction(
            &D20CheckKind::AttackRoll(AttackSource::Weapon(weapon_kind)),
            &source,
            Operator::Equal(1),
        );
    }
}

#[test]
fn champion_remarkable_athlete() {
    let mut scenario = Scenario::new();
    scenario.spawn("fighter", "hero.fighter").level(3).spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    let source = ModifierSource::Effect("effect.fighter.champion.remarkable_athlete".into());
    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.champion.remarkable_athlete")
        .assert_d20_advantage(
            &D20CheckKind::Skill(Skill::Athletics),
            &source,
            AdvantageType::Advantage,
        )
        .assert_d20_advantage(
            &D20CheckKind::Skill(Skill::Initiative),
            &source,
            AdvantageType::Advantage,
        )
        // Force crit to trigger disengage effect
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            D20CheckOutcome::CriticalSuccess,
        )
        .act("action.melee_attack")
        .target_entity("goblin")
        .perform();

    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.champion.remarkable_athlete_disengage")
        .assert_free_movement(
            ModifierSource::Effect("effect.fighter.champion.remarkable_athlete_disengage".into()),
            Operator::Equal(0.5),
        );
}

#[test]
fn champion_additional_fighting_style() {
    let mut scenario = Scenario::new();
    scenario
        .spawn("fighter_lvl_6", "hero.fighter")
        .level(6)
        .spawn();
    scenario
        .spawn("fighter_lvl_7", "hero.fighter")
        .level(7)
        .spawn();

    scenario
        .probe("fighter_lvl_6")
        .assert_effect("effect.fighting_style.great_weapon_fighting");
    scenario
        .probe("fighter_lvl_7")
        .assert_effect("effect.fighting_style.great_weapon_fighting")
        .assert_effect("effect.fighting_style.defense");
}

#[test]
fn champion_superior_critical() {
    let mut scenario = fighter_scenario(15);

    let source = ModifierSource::Effect("effect.fighter.champion.superior_critical".into());
    let mut probe = scenario.probe("fighter");
    probe.assert_no_effect("effect.fighter.champion.improved_critical");
    probe.assert_effect("effect.fighter.champion.superior_critical");
    for weapon_kind in [WeaponKind::Melee, WeaponKind::Ranged, WeaponKind::Unarmed] {
        probe.assert_d20_crit_threshold_reduction(
            &D20CheckKind::AttackRoll(AttackSource::Weapon(weapon_kind)),
            &source,
            Operator::Equal(2),
        );
    }
}

#[test]
fn champion_survivor() {
    let mut scenario = Scenario::new();
    scenario.spawn("fighter", "hero.fighter").level(18).spawn();

    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.champion.survivor");

    // Defy death
    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.champion.survivor.defy_death")
        .assert_d20_advantage(
            &D20CheckKind::SavingThrow(SavingThrowKind::Death),
            &ModifierSource::Effect("effect.fighter.champion.survivor.defy_death".into()),
            AdvantageType::Advantage,
        )
        .assert_d20_crit_threshold_reduction(
            &D20CheckKind::SavingThrow(SavingThrowKind::Death),
            &ModifierSource::Effect("effect.fighter.champion.survivor.defy_death".into()),
            Operator::Equal(2),
        );

    // Heroic rally
    // Only triggers below half health
    let hp = scenario.probe("fighter").hp();
    scenario
        .probe("fighter")
        .assert_effect("effect.fighter.champion.survivor.heroic_rally")
        .damage_raw(hp / 2 + 1);
    let prev_hp = scenario.probe("fighter").hp();
    scenario
        .probe("fighter")
        .start_turn()
        .assert_hp(Operator::Equal(prev_hp + 5 + 4)); // 5 + CON mod
}
