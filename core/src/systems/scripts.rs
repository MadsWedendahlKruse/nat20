use std::sync::Arc;

use hecs::{Entity, World};
use tracing::error;

use crate::{
    components::{
        actions::action::{ActionContext, ActionKindResult, ActionResult, ReactionResult},
        damage::{DamageMitigationResult, DamageRollResult},
        effects::effect::EffectInstance,
        id::{ActionId, EntityIdentifier, ScriptId},
        resource::ResourceAmountMap,
    },
    engine::{
        action_prompt::{ActionData, ReactionData},
        event::{CallbackResult, Event, EventCallback, EventKind},
        game_state::GameState,
    },
    registry::registry::ScriptsRegistry,
    scripts::{
        script_api::{
            ScriptEntityRole, ScriptEventRef, ScriptReactionBodyContext, ScriptReactionBodyResult,
            ScriptReactionPlan, ScriptReactionTriggerContext,
        },
        script_engine::SCRIPT_ENGINE,
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20ResultKind},
    },
};

// TODO: Since we only have single languauge support, do we still need all these
// evaluate_* functions?

pub fn evaluate_reaction_trigger(
    reaction_trigger: &ScriptId,
    context: &ScriptReactionTriggerContext,
) -> bool {
    let script = ScriptsRegistry::get(reaction_trigger).expect(
        format!(
            "Reaction trigger script not found in registry: {:?}",
            reaction_trigger
        )
        .as_str(),
    );
    match SCRIPT_ENGINE.evaluate_reaction_trigger(script, context) {
        Ok(result) => result,
        Err(err) => {
            error!(
                "Error evaluating reaction trigger script {:?} for reactor {:?}: {:?}",
                reaction_trigger, context.reactor, err
            );
            false
        }
    }
}

pub fn evaluate_reaction_body(
    reaction_body: &ScriptId,
    context: &ScriptReactionBodyContext,
) -> ScriptReactionBodyResult {
    let script = ScriptsRegistry::get(reaction_body)
        .expect(format!("Reaction script not found in registry: {:?}", reaction_body).as_str());
    match SCRIPT_ENGINE.evaluate_reaction_body(script, context) {
        Ok(result) => result,
        Err(err) => {
            error!(
                "Error evaluating reaction body script {:?} for reactor {:?}: {:?}",
                reaction_body, context.reactor, err
            );
            ScriptReactionBodyResult::none()
        }
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
    results: &Vec<ActionResult>,
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

pub fn evaluate_damage_roll_result_hook(
    damage_roll_result_hook: &ScriptId,
    game_state: &GameState,
    entity: Entity,
    damage_roll_result: &mut DamageRollResult,
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

pub fn apply_reaction_body_result(
    game_state: &mut GameState,
    reaction_data: &ReactionData,
    result: ScriptReactionBodyResult,
) {
    match result {
        ScriptReactionBodyResult::Plan(plan) => {
            apply_reaction_plan(game_state, reaction_data, plan);
        }
        ScriptReactionBodyResult::TriggerEvent(event_view) => {
            let reaction_data = reaction_data.clone();
            let reaction_data_for_modification = reaction_data.clone();
            let reaction_result = ReactionResult::ModifyEvent {
                modification: Arc::new(move |world: &World, event: &mut Event| {
                    event_view.apply_to_event(world, &reaction_data_for_modification, event);
                }),
            };

            game_state.process_event(Event::action_performed_event(
                &ActionData::from(&reaction_data),
                vec![(
                    reaction_data.reactor,
                    ActionKindResult::Reaction {
                        result: reaction_result,
                    },
                )],
            ));
        }
    }
}

pub fn apply_reaction_plan(
    game_state: &mut GameState,
    reaction_data: &ReactionData,
    plan: ScriptReactionPlan,
) {
    match plan {
        ScriptReactionPlan::None => {}

        ScriptReactionPlan::Sequence(plans) => {
            for p in plans {
                apply_reaction_plan(game_state, reaction_data, p);
            }
        }

        ScriptReactionPlan::CancelEvent {
            event,
            resources_to_refund,
        } => {
            let target_event = match event {
                ScriptEventRef::TriggerEvent => &reaction_data.event,
            };

            let mut resources_refunded = ResourceAmountMap::new();
            for resource_id in &resources_to_refund {
                resources_refunded.map.insert(
                    resource_id.clone(),
                    reaction_data
                        .resource_cost
                        .map
                        .get(resource_id)
                        .cloned()
                        .unwrap(),
                );
            }

            let result = ReactionResult::CancelEvent {
                event: target_event.clone(),
                resources_refunded,
            };

            game_state.process_event(Event::action_performed_event(
                &ActionData::from(reaction_data),
                vec![(
                    EntityIdentifier::from_world(&game_state.world, target_event.actor().unwrap()),
                    ActionKindResult::Reaction { result },
                )],
            ));
        }

        ScriptReactionPlan::RequireSavingThrow {
            target,
            dc,
            on_success,
            on_failure,
        } => {
            let target_entity = match target {
                ScriptEntityRole::Reactor => reaction_data.reactor.id(),
                ScriptEntityRole::Actor => reaction_data
                    .event
                    .actor()
                    .expect("Trigger event has no actor"),
                ScriptEntityRole::Target => reaction_data
                    .event
                    .target()
                    .expect("RequireSavingThrow reaction target role 'Target' but trigger event has no target"),
            };

            let dc_kind = D20CheckDCKind::SavingThrow((dc.saving_throw.function)(
                &game_state.world,
                target_entity,
                &reaction_data.context,
            ));

            let check_event = systems::d20::check(game_state, target_entity, &dc_kind);

            let context_clone = reaction_data.clone();
            let on_success_plan = *on_success;
            let on_failure_plan = *on_failure;

            let callback = EventCallback::new(move |game_state, event, _| {
                if let EventKind::D20CheckResolved(_, result_kind, _) = &event.kind {
                    let success = match result_kind {
                        D20ResultKind::SavingThrow { .. } => result_kind.is_success(&dc_kind),
                        _ => panic!("RequireSavingThrow expects a saving throw result"),
                    };

                    let next_plan = if success {
                        on_success_plan.clone()
                    } else {
                        on_failure_plan.clone()
                    };

                    apply_reaction_plan(game_state, &context_clone, next_plan);

                    CallbackResult::None
                } else {
                    panic!("RequireSavingThrow callback received unexpected event");
                }
            });

            let _ = game_state.process_event_with_response_callback(check_event, callback);
        }
    }
}
