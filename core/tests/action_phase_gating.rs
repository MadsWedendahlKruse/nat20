use nat20_core::{
    components::{
        actions::{
            action::{
                ActionCondition, ActionKind, ActionPhaseSpec, PhaseRequirement, PhaseTargets,
            },
            targeting::TargetInstance,
        },
        d20::D20CheckOutcome,
        damage::{AttackSource, DamageComponent, DamageType},
        dice::{DiceSet, DieSize},
        modifier::{ModifierMap, ModifierSource},
    },
    engine::action_prompt::ActionData,
    systems::{self, d20::D20CheckKind},
    test_utils::scenario::Scenario,
};
use rstest::rstest;

/// TODO: Remove this test once we have a real spell that uses phase gating
///
/// Casts a two-phase variant of Ray of Frost where the second phase repeats the
/// damage payload unconditionally but is gated on the first phase's attack
/// outcome. Built directly from `ActionPhaseSpec`s because no shipped content
/// uses gating yet.
fn cast_gated_ray_of_frost(
    scenario: &mut Scenario,
    requires: PhaseRequirement,
    attack_outcome: D20CheckOutcome,
) {
    scenario.probe("wizard").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Spell),
        attack_outcome,
    );

    let wizard = scenario.creatures.get("wizard").unwrap().creature.clone();
    let goblin = scenario.creatures.get("goblin").unwrap().creature.clone();

    let action_id = "action.ray_of_frost".into();
    let action = systems::actions::get_action(&action_id).unwrap();
    let base = action.kind().phases()[0].clone();
    let gated = ActionPhaseSpec {
        requires,
        condition: ActionCondition::None,
        payload: base.payload.clone(),
        targets: PhaseTargets::Inherited,
    };
    let kind = ActionKind::Standard {
        phases: vec![base, gated],
    };

    let (context, cost) = systems::actions::available_actions(&scenario.game_state, wizard.id())
        .get(&action_id)
        .expect("wizard should have ray_of_frost")[0]
        .clone();
    let action_data = ActionData::new(
        wizard.clone(),
        action_id.clone(),
        context,
        cost,
        vec![TargetInstance::entity(goblin)],
    );

    let phases = kind.perform(&mut scenario.game_state, &action_data);
    systems::actions::start_execution(&mut scenario.game_state, action, action_data, phases);

    for _ in 0..10 {
        scenario.game_state.update(0.5);
    }
}

#[rstest]
// A forced critical hit makes phase 1's roll 2d8 (crit doubling) while the
// gated unconditional phase always rolls 1d8, so the two dice counts isolate
// which phases ran.
#[case(PhaseRequirement::Success, D20CheckOutcome::CriticalSuccess, 1, 1)]
#[case(PhaseRequirement::Success, D20CheckOutcome::CriticalFailure, 0, 0)]
#[case(PhaseRequirement::Failure, D20CheckOutcome::CriticalFailure, 0, 1)]
#[case(PhaseRequirement::Failure, D20CheckOutcome::CriticalSuccess, 1, 0)]
fn phase_gating(
    #[case] requires: PhaseRequirement,
    #[case] attack_outcome: D20CheckOutcome,
    #[case] expected_crit_rolls: usize,
    #[case] expected_gated_rolls: usize,
) {
    let mut scenario = Scenario::new();
    scenario.spawn("wizard", "hero.wizard").level(4).spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        .level(1)
        .spawn();

    cast_gated_ray_of_frost(&mut scenario, requires, attack_outcome);

    for (dice_num, expected) in [(2, expected_crit_rolls), (1, expected_gated_rolls)] {
        scenario
            .event_filter()
            .actor("wizard")
            .damage_roll(DamageComponent::new(
                ModifierMap::from(ModifierSource::Base, DiceSet::new(dice_num, DieSize::D8)),
                DamageType::Cold,
            ))
            .assert_event_count(expected);
    }
}
