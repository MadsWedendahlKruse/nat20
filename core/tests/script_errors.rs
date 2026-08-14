extern crate nat20_core;

use nat20_core::{
    components::{
        actions::action::ActionContext,
        id::{ActionId, ScriptId},
    },
    systems,
    test_utils::scenario::Scenario,
};

/// Errors in an action usability script blocks the action from being used instead
/// of silently failing and allowing the action to be performed.
/// Since none of the scripts in `assets/registries` are broken on purpose, we
/// can use a random script which defines a different function
#[test]
fn usability_script_error_blocks_action() {
    let mut scenario = Scenario::new();
    scenario.spawn("fighter", "hero.fighter").level(1).spawn();
    let entity = scenario.entity("fighter");

    // Defines `speed_hook` instead of `action_usability`
    let script = ScriptId::new("nat20_core", "script.effect.barbarian.fast_movement");

    let reason = systems::scripts::evaluate_action_usability(
        &script,
        &scenario.game_state,
        entity,
        &ActionId::new("nat20_core", "action.melee_attack"),
        &ActionContext::empty(),
    );

    assert!(
        reason.is_some_and(|reason| reason.contains("fast_movement")),
        "Expected an unevaluable usability script to block the action and name itself"
    );
}
