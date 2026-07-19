use nat20_core::{
    components::{
        d20::{AdvantageType, D20CheckOutcome},
        damage::{AttackSource, DamageComponent, DamageType},
        dice::{DiceSet, DieSize},
        id::EffectId,
        items::equipment::weapon::WeaponKind,
        modifier::{ModifierMap, ModifierSource},
        skill::Skill,
    },
    systems::d20::D20CheckKind,
    test_utils::scenario::Scenario,
};
use rstest::rstest;

#[rstest]
#[case(1, 1)]
#[case(5, 2)]
// #[case(11, 3)] TODO: Warlock can't get to level 11 yet :^)
fn warlock_eldritch_blast(#[case] warlock_level: u8, #[case] expected_beams: u8) {
    let mut scenario = Scenario::new();
    scenario
        .spawn("warlock", "hero.warlock")
        .level(warlock_level)
        .spawn();
    // Spawn someone who doesn't get one shot
    scenario
        .spawn("fighter", "hero.fighter")
        .level(5)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    scenario
        .probe("warlock")
        // Force Eldritch Blast to hit
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Spell),
            D20CheckOutcome::Success,
        )
        .act("action.eldritch_blast")
        .target_entities(vec!["fighter"; expected_beams as usize])
        .perform();

    scenario
        .event_filter()
        .actor("warlock")
        .damage_roll(DamageComponent::new(
            ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D10)),
            DamageType::Force,
        ))
        .assert_event_count(expected_beams as usize);
}

#[test]
fn warlock_hellish_rebuke() {
    let mut scenario = Scenario::new();
    // Level 3 is just to make sure he has enough HP to survive the attack and react
    scenario.spawn("warlock", "hero.warlock").level(3).spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    scenario
        .probe("goblin")
        // Make sure the goblin hits so we trigger the reaction
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
            D20CheckOutcome::CriticalSuccess,
        )
        .act("action.melee_attack")
        .target_entity("warlock")
        .perform();

    scenario
        .probe("warlock")
        .react()
        .option_id("action.hellish_rebuke")
        .perform();

    scenario
        .event_filter()
        .actor("warlock")
        .damage_roll(DamageComponent::new(
            // At level 3 the warlock has level 2 spell slots, so base 2d10
            // plus 1d10 for the spell slot level, for a total of 3d10
            ModifierMap::from(ModifierSource::Base, DiceSet::new(3, DieSize::D10)),
            DamageType::Fire,
        ))
        .assert_event();
}

#[rstest]
#[case("action.hex.strength", "effect.spell.hex.strength", &[Skill::Athletics])]
#[case(
    "action.hex.dexterity",
    "effect.spell.hex.dexterity",
    &[Skill::Acrobatics, Skill::SleightOfHand, Skill::Stealth,  Skill::Initiative]
)]
// TODO: What to do with constitution? It's not really associated with a skill
#[case("action.hex.constitution", "effect.spell.hex.constitution", &[])]
#[case(
    "action.hex.intelligence",
    "effect.spell.hex.intelligence",
    &[Skill::Arcana, Skill::History, Skill::Investigation, Skill::Nature, Skill::Religion]
)]
#[case("action.hex.wisdom", "effect.spell.hex.wisdom", &[Skill::AnimalHandling, Skill::Insight, Skill::Medicine, Skill::Perception, Skill::Survival])]
#[case("action.hex.charisma", "effect.spell.hex.charisma", &[Skill::Deception, Skill::Intimidation, Skill::Performance, Skill::Persuasion])]
fn warlock_hex_disadvantage(
    #[case] hex_variant_id: &str,
    #[case] effect_id: &str,
    #[case] skills: &[Skill],
) {
    let mut scenario = Scenario::new();
    scenario.spawn("warlock", "hero.warlock").level(2).spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    scenario
        .probe("warlock")
        .assert_no_concentration()
        .act("action.hex")
        .variant(hex_variant_id)
        .target_entity("goblin")
        .perform();

    scenario.probe("warlock").assert_concentration(effect_id);

    scenario.probe("goblin").assert_effect(effect_id);

    for skill in skills {
        scenario
            .probe("goblin")
            .assert_effect(effect_id)
            .assert_d20_advantage(
                &D20CheckKind::Skill(*skill),
                &ModifierSource::Effect(effect_id.into()),
                AdvantageType::Disadvantage,
            );
    }
}

#[test]
fn warlock_hex_damage() {
    let mut scenario = Scenario::new();
    // Level 5 so we get two beams on Eldritch Blast
    scenario.spawn("warlock", "hero.warlock").level(5).spawn();
    // Spawn someone who doesn't get one shot
    scenario
        .spawn("fighter", "hero.fighter")
        .level(5)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    scenario
        .probe("warlock")
        .act("action.hex")
        .variant("action.hex.strength")
        .target_entity("fighter")
        .perform();

    scenario
        .probe("warlock")
        // Force Eldritch Blast to hit so we can see the damage from Hex
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Spell),
            D20CheckOutcome::Success,
        )
        .act("action.eldritch_blast")
        .target_entities(vec!["fighter"; 2])
        .perform();

    scenario
        .event_filter()
        .actor("warlock")
        .damage_roll(DamageComponent::new(
            ModifierMap::from(ModifierSource::Base, DiceSet::new(1, DieSize::D10)),
            DamageType::Force,
        ))
        .assert_event_count(2);

    // Hex damage isn't rolled separately, but rather applied to the Eldritch Blast
    // damage roll when the target takes damage, so we check for it in the action result
    scenario
        .event_filter()
        .actor("warlock")
        .damage_dealt(
            "fighter",
            DamageComponent::new(
                ModifierMap::from(
                    ModifierSource::Effect(EffectId::new("nat20_core", "effect.spell.hex")),
                    DiceSet::new(1, DieSize::D6),
                ),
                DamageType::Necrotic,
            ),
        )
        .assert_event_count(2);
}

#[test]
fn warlock_hex_reapply_on_kill() {
    let mut scenario = Scenario::new();
    scenario.spawn("warlock", "hero.warlock").level(5).spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .position([1.0, 0.0, 0.0], false)
        .spawn();

    // Force goblin to be killed by the Eldritch Blast so we can test the reapplication of Hex
    scenario.probe("goblin").damage_raw(9);

    scenario
        .probe("warlock")
        .assert_no_concentration()
        .act("action.hex")
        .variant("action.hex.strength")
        .target_entity("goblin")
        .perform();

    scenario
        .probe("warlock")
        .assert_concentration("effect.spell.hex.strength");

    // Kill the goblin to trigger the reapplication of Hex
    scenario
        .probe("warlock")
        // Force Eldritch Blast to hit so we can kill the goblin
        .d20_force_outcome(
            D20CheckKind::AttackRoll(AttackSource::Spell),
            D20CheckOutcome::CriticalSuccess,
        )
        .act("action.eldritch_blast")
        .target_entities(vec!["goblin"; 2])
        .perform();

    scenario
        .probe("warlock")
        .assert_effect("effect.spell.hex_reapply")
        .assert_has_action("action.hex_reapply");

    // Start a new turn to recharge the bonus action for Hex reapplication
    scenario.probe("warlock").start_turn();

    // Spawn a new goblin to reapply Hex to
    scenario
        .spawn("goblin2", "monster.goblin_warrior")
        .level(1)
        .position([2.0, 0.0, 0.0], false)
        .spawn();

    scenario
        .probe("warlock")
        .assert_action_available("action.hex_reapply")
        .act("action.hex_reapply")
        .variant("action.hex.strength")
        .target_entity("goblin2")
        .perform();

    // Hex reapply effect consumes itself
    scenario
        .probe("warlock")
        .assert_concentration("effect.spell.hex.strength")
        .assert_no_effect("effect.spell.hex_reapply")
        .assert_no_action("action.hex_reapply");
}
