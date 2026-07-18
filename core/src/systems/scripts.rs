use hecs::Entity;
use tracing::error;

use crate::{
    components::{
        actions::action::{ActionConditionResolution, ActionContext, ActionResult},
        damage::{DamageMitigationResult, DamageRoll, DamageRollResult},
        effects::effect::EffectInstance,
        id::{ActionId, ScriptId},
        resource::ResourceAmountMap,
    },
    engine::{action_prompt::ActionData, event::Event, game_state::GameState},
    registry::registry::ScriptsRegistry,
    scripts::script_engine::SCRIPT_ENGINE,
};

// TODO: Since we only have single languauge support, do we still need all these
// evaluate_* functions?

pub fn evaluate_reaction_trigger(
    reaction_trigger: &ScriptId,
    game_state: &GameState,
    reactor: Entity,
    event: &Event,
) -> bool {
    let script = ScriptsRegistry::get(reaction_trigger).expect(
        format!(
            "Reaction trigger script not found in registry: {:?}",
            reaction_trigger
        )
        .as_str(),
    );
    match SCRIPT_ENGINE.evaluate_reaction_trigger(script, game_state, reactor, event) {
        Ok(result) => result,
        Err(err) => {
            error!(
                "Error evaluating reaction trigger script {:?} for reactor {:?}: {:?}",
                reaction_trigger, reactor, err
            );
            false
        }
    }
}

pub fn evaluate_reaction_body(
    reaction_body: &ScriptId,
    game_state: &mut GameState,
    reaction: &ActionData,
    event: &mut Event,
) {
    let script = ScriptsRegistry::get(reaction_body)
        .expect(format!("Reaction script not found in registry: {:?}", reaction_body).as_str());
    if let Err(err) = SCRIPT_ENGINE.evaluate_reaction_body(script, game_state, reaction, event) {
        error!(
            "Error evaluating reaction body script {:?} for reactor {:?}: {:?}",
            reaction_body,
            reaction.actor.id(),
            err
        );
    }
}

pub fn evaluate_resource_cost_hook(
    resource_cost_hook: &ScriptId,
    game_state: &GameState,
    entity: Entity,
    action_id: &ActionId,
    action_context: &ActionContext,
    resource_cost: &mut ResourceAmountMap,
) {
    let script = ScriptsRegistry::get(resource_cost_hook).expect(
        format!(
            "Resource cost hook script not found in registry: {:?}",
            resource_cost_hook
        )
        .as_str(),
    );
    if let Err(err) = SCRIPT_ENGINE.evaluate_resource_cost_hook(
        script,
        game_state,
        entity,
        action_id,
        action_context,
        resource_cost,
    ) {
        error!(
            "Error evaluating resource cost hook script {:?} for entity {:?}: {:?}",
            resource_cost_hook, entity, err
        );
    }
}

pub fn evaluate_action_hook(
    action_hook: &ScriptId,
    game_state: &mut GameState,
    action: &ActionData,
) {
    let script = ScriptsRegistry::get(action_hook).expect(
        format!(
            "Action hook script not found in registry: {:?}",
            action_hook
        )
        .as_str(),
    );
    if let Err(err) = SCRIPT_ENGINE.evaluate_action_hook(script, game_state, action) {
        error!(
            "Error evaluating action hook script {:?}: {:?}",
            action_hook, err
        );
    }
}

pub fn evaluate_action_result_hook(
    action_result_hook: &ScriptId,
    game_state: &mut GameState,
    action: &ActionData,
    results: &ActionResult,
) {
    let script = ScriptsRegistry::get(action_result_hook).expect(
        format!(
            "Action result hook script not found in registry: {:?}",
            action_result_hook
        )
        .as_str(),
    );
    if let Err(err) = SCRIPT_ENGINE.evaluate_action_result_hook(script, game_state, action, results)
    {
        error!(
            "Error evaluating action result hook script {:?}: {:?}",
            action_result_hook, err
        );
    }
}

pub fn evaluate_armor_class_hook(
    armor_class_hook: &ScriptId,
    game_state: &GameState,
    entity: Entity,
) -> i32 {
    let script = ScriptsRegistry::get(armor_class_hook).expect(
        format!(
            "Armor class hook script not found in registry: {:?}",
            armor_class_hook
        )
        .as_str(),
    );
    match SCRIPT_ENGINE.evaluate_armor_class_hook(script, game_state, entity) {
        Ok(modifier) => modifier,
        Err(err) => {
            error!(
                "Error evaluating armor class hook script {:?} for entity {:?}: {:?}",
                armor_class_hook, entity, err
            );
            0
        }
    }
}

pub fn evaluate_damage_roll_hook(
    damage_roll_hook: &ScriptId,
    game_state: &GameState,
    entity: Entity,
    damage_roll: &mut DamageRoll,
    action: &ActionData,
    resolution: &ActionConditionResolution,
) {
    let script = ScriptsRegistry::get(damage_roll_hook).expect(
        format!(
            "Damage roll hook script not found in registry: {:?}",
            damage_roll_hook
        )
        .as_str(),
    );
    if let Err(err) = SCRIPT_ENGINE.evaluate_damage_roll_hook(
        script,
        game_state,
        entity,
        damage_roll,
        action,
        resolution,
    ) {
        error!(
            "Error evaluating damage roll hook script {:?} for entity {:?}: {:?}",
            damage_roll_hook, entity, err
        );
    }
}

pub fn evaluate_damage_roll_result_hook(
    damage_roll_result_hook: &ScriptId,
    game_state: &GameState,
    entity: Entity,
    damage_roll_result: &mut DamageRollResult,
    action: &ActionData,
    resolution: &ActionConditionResolution,
) {
    let script = ScriptsRegistry::get(damage_roll_result_hook).expect(
        format!(
            "Damage roll result hook script not found in registry: {:?}",
            damage_roll_result_hook
        )
        .as_str(),
    );
    if let Err(err) = SCRIPT_ENGINE.evaluate_damage_roll_result_hook(
        script,
        game_state,
        entity,
        damage_roll_result,
        action,
        resolution,
    ) {
        error!(
            "Error evaluating damage roll result hook script {:?} for entity {:?}: {:?}",
            damage_roll_result_hook, entity, err
        );
    }
}

pub fn evaluate_pre_damage_mitigation_hook(
    pre_damage_mitigation_hook: &ScriptId,
    game_state: &GameState,
    entity: Entity,
    effect: &EffectInstance,
    damage_roll_result: &mut DamageRollResult,
) {
    let script = ScriptsRegistry::get(pre_damage_mitigation_hook).expect(
        format!(
            "Pre damage mitigation hook script not found in registry: {:?}",
            pre_damage_mitigation_hook
        )
        .as_str(),
    );
    if let Err(err) = SCRIPT_ENGINE.evaluate_pre_damage_mitigation_hook(
        script,
        game_state,
        entity,
        effect,
        damage_roll_result,
    ) {
        error!(
            "Error evaluating pre-damage mitigation hook script {:?} for entity {:?}: {:?}",
            pre_damage_mitigation_hook, entity, err
        );
    }
}

pub fn evaluate_post_damage_mitigation_hook(
    damage_mitigation_hook: &ScriptId,
    game_state: &GameState,
    entity: Entity,
    damage_mitigation_result: &mut DamageMitigationResult,
) {
    let script = ScriptsRegistry::get(damage_mitigation_hook).expect(
        format!(
            "Post damage mitigation hook script not found in registry: {:?}",
            damage_mitigation_hook
        )
        .as_str(),
    );
    if let Err(err) = SCRIPT_ENGINE.evaluate_post_damage_mitigation_hook(
        script,
        game_state,
        entity,
        damage_mitigation_result,
    ) {
        error!(
            "Error evaluating post-damage mitigation hook script {:?} for entity {:?}: {:?}",
            damage_mitigation_hook, entity, err
        );
    }
}

pub fn evaluate_death_hook(
    death_hook: &ScriptId,
    game_state: &mut GameState,
    victim: Entity,
    killer: Option<Entity>,
    applier: Option<Entity>,
) {
    let script = ScriptsRegistry::get(death_hook)
        .expect(format!("Death hook script not found in registry: {:?}", death_hook).as_str());
    if let Err(err) = SCRIPT_ENGINE.evaluate_death_hook(script, game_state, victim, killer, applier)
    {
        error!(
            "Error evaluating death hook script {:?} for entity {:?}: {:?}",
            death_hook, victim, err
        );
    }
}

pub fn evaluate_turn_start_hook(script_id: &ScriptId, game_state: &mut GameState, entity: Entity) {
    let script = ScriptsRegistry::get(script_id)
        .expect(format!("Turn start hook script not found: {:?}", script_id).as_str());
    if let Err(err) = SCRIPT_ENGINE.evaluate_turn_start_hook(script, game_state, entity) {
        error!(
            "Error evaluating turn start hook script {:?} for entity {:?}: {:?}",
            script_id, entity, err
        );
    }
}

pub fn evaluate_action_usability(
    action_usability: &ScriptId,
    game_state: &GameState,
    entity: Entity,
    action_context: &ActionContext,
) -> Option<String> {
    let script = ScriptsRegistry::get(action_usability).expect(
        format!(
            "Action usability script not found in registry: {:?}",
            action_usability
        )
        .as_str(),
    );
    match SCRIPT_ENGINE.evaluate_action_usability(script, game_state, entity, action_context) {
        Ok(result) => result,
        Err(err) => {
            error!(
                "Error evaluating action usability script {:?} for entity {:?}: {:?}",
                action_usability, entity, err
            );
            None
        }
    }
}
