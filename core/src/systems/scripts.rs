use std::sync::Arc;

use hecs::World;
use tracing::error;

use crate::{
    components::{
        actions::action::{ActionKindResult, ReactionResult},
        id::ScriptId,
        modifier::{Modifiable, ModifierSource},
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
            ScriptActionView, ScriptDamageMitigationResult, ScriptDamageRollResult,
            ScriptEffectView, ScriptEntityRole, ScriptEntityView, ScriptEventRef,
            ScriptOptionalEntityView, ScriptReactionBodyContext, ScriptReactionPlan,
            ScriptReactionTriggerContext,
        },
        script_engine::SCRIPT_ENGINES,
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20ResultKind},
    },
};

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
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_reaction_trigger(script, &context) {
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
) -> ScriptReactionPlan {
    let script = ScriptsRegistry::get(reaction_body)
        .expect(format!("Reaction script not found in registry: {:?}", reaction_body).as_str());
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_reaction_body(script, &context) {
        Ok(plan) => plan,
        Err(err) => {
            error!(
                "Error evaluating reaction body script {:?} for reactor {:?}: {:?}",
                reaction_body, context.reactor, err
            );
            ScriptReactionPlan::None
        }
    }
}

pub fn evaluate_resource_cost_hook(
    resource_cost_hook: &ScriptId,
    action_view: &ScriptActionView,
    entity_view: &ScriptEntityView,
) {
    let script = ScriptsRegistry::get(resource_cost_hook).expect(
        format!(
            "Resource cost hook script not found in registry: {:?}",
            resource_cost_hook
        )
        .as_str(),
    );
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_resource_cost_hook(script, action_view, entity_view) {
        Ok(()) => {}
        Err(err) => {
            error!(
                "Error evaluating resource cost hook script {:?} for entity {:?}: {:?}",
                resource_cost_hook, entity_view.entity, err
            );
        }
    }
}

pub fn evalute_action_hook(
    action_hook: &ScriptId,
    action_view: &ScriptActionView,
    entity_view: &ScriptEntityView,
) {
    let script = ScriptsRegistry::get(action_hook).expect(
        format!(
            "Action hook script not found in registry: {:?}",
            action_hook
        )
        .as_str(),
    );
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_action_hook(script, action_view, entity_view) {
        Ok(()) => {}
        Err(err) => {
            error!(
                "Error evaluating action hook script {:?} for entity {:?}: {:?}",
                action_hook, entity_view.entity, err
            );
        }
    }
}

pub fn evaluate_armor_class_hook(
    armor_class_hook: &ScriptId,
    entity_view: &ScriptEntityView,
) -> i32 {
    let script = ScriptsRegistry::get(armor_class_hook).expect(
        format!(
            "Armor class hook script not found in registry: {:?}",
            armor_class_hook
        )
        .as_str(),
    );
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_armor_class_hook(script, entity_view) {
        Ok(modifier) => modifier,
        Err(err) => {
            error!(
                "Error evaluating armor class hook script {:?} for entity {:?}: {:?}",
                armor_class_hook, entity_view.entity, err
            );
            0
        }
    }
}

pub fn evaluate_damage_roll_result_hook(
    damage_roll_result_hook: &ScriptId,
    entity_view: &ScriptEntityView,
    damage_roll_result: &ScriptDamageRollResult,
) {
    let script = ScriptsRegistry::get(damage_roll_result_hook).expect(
        format!(
            "Damage roll result hook script not found in registry: {:?}",
            damage_roll_result_hook
        )
        .as_str(),
    );
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_damage_roll_result_hook(script, entity_view, damage_roll_result) {
        Ok(()) => {}
        Err(err) => {
            error!(
                "Error evaluating damage roll result hook script {:?} for entity {:?}: {:?}",
                damage_roll_result_hook, entity_view.entity, err
            );
        }
    }
}

pub fn evaluate_pre_damage_mitigation_hook(
    pre_damage_mitigation_hook: &ScriptId,
    entity_view: &ScriptEntityView,
    effect: &ScriptEffectView,
    damage_roll_result: &ScriptDamageRollResult,
) {
    let script = ScriptsRegistry::get(pre_damage_mitigation_hook).expect(
        format!(
            "Pre damage mitigation hook script not found in registry: {:?}",
            pre_damage_mitigation_hook
        )
        .as_str(),
    );
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_pre_damage_mitigation_hook(
        script,
        entity_view,
        effect,
        damage_roll_result,
    ) {
        Ok(()) => {}
        Err(err) => {
            error!(
                "Error evaluating pre-damage taken hook script {:?} for entity {:?}: {:?}",
                pre_damage_mitigation_hook, entity_view.entity, err
            );
        }
    }
}

pub fn evaluate_post_damage_mitigation_hook(
    damage_mitigation_hook: &ScriptId,
    entity_view: &ScriptEntityView,
    damage_mitigation_result: &ScriptDamageMitigationResult,
) {
    let script = ScriptsRegistry::get(damage_mitigation_hook).expect(
        format!(
            "Post damage mitigation hook script not found in registry: {:?}",
            damage_mitigation_hook
        )
        .as_str(),
    );
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_post_damage_mitigation_hook(script, entity_view, damage_mitigation_result)
    {
        Ok(()) => {}
        Err(err) => {
            error!(
                "Error evaluating damage taken hook script {:?} for entity {:?}: {:?}",
                damage_mitigation_hook, entity_view.entity, err
            );
        }
    }
}

pub fn evaluate_death_hook(
    death_hook: &ScriptId,
    victim_entity_view: &ScriptEntityView,
    killer_entity_view: &ScriptOptionalEntityView,
    applier_entity_view: &ScriptOptionalEntityView,
) {
    let script = ScriptsRegistry::get(death_hook)
        .expect(format!("Death hook script not found in registry: {:?}", death_hook).as_str());
    let mut engine_lock = SCRIPT_ENGINES.lock().unwrap();
    let engine = engine_lock
        .get_mut(&script.language)
        .expect(format!("No script engine found for language: {:?}", script.language).as_str());
    match engine.evaluate_death_hook(
        script,
        victim_entity_view,
        killer_entity_view,
        applier_entity_view,
    ) {
        Ok(()) => {}
        Err(err) => {
            error!(
                "Error evaluating death hook script {:?} for entity {:?}: {:?}",
                death_hook, victim_entity_view.entity, err
            );
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

        ScriptReactionPlan::ModifyD20Result { bonus } => {
            let bonus_value = bonus.evaluate(
                &game_state.world,
                reaction_data.reactor,
                &reaction_data.context,
            );

            let result = ReactionResult::ModifyEvent {
                modification: Arc::new({
                    let action_id = reaction_data.reaction_id.clone();
                    move |_world: &World, event: &mut Event| {
                        if let EventKind::D20CheckPerformed(_, ref mut existing_result, _) =
                            event.kind
                        {
                            match existing_result {
                                D20ResultKind::Skill { result, .. }
                                | D20ResultKind::SavingThrow { result, .. } => {
                                    result.add_bonus(
                                        ModifierSource::Action(action_id.clone()),
                                        bonus_value,
                                    );
                                }
                                D20ResultKind::AttackRoll { result } => {
                                    result.roll_result.add_bonus(
                                        ModifierSource::Action(action_id.clone()),
                                        bonus_value,
                                    );
                                }
                            }
                        } else {
                            panic!("ModifyD20Result applied to wrong event type: {:?}", event);
                        }
                    }
                }),
            };

            let process_event_result = game_state.process_event(Event::action_performed_event(
                game_state,
                &ActionData::from(reaction_data),
                vec![(reaction_data.reactor, ActionKindResult::Reaction { result })],
            ));

            match process_event_result {
                Ok(_) => {}
                Err(err) => {
                    error!(
                        "Error processing ModifyD20Result reaction for reactor {:?}: {:?}",
                        reaction_data.reactor, err
                    );
                }
            }
        }

        ScriptReactionPlan::ModifyD20DC { modifier } => {
            let modifier_value = modifier.evaluate(
                &game_state.world,
                reaction_data.reactor,
                &reaction_data.context,
            );

            let result = ReactionResult::ModifyEvent {
                modification: Arc::new({
                    let action = reaction_data.reaction_id.clone();
                    move |_world: &World, event: &mut Event| {
                        if let EventKind::D20CheckPerformed(_, _, ref mut dc_kind) = event.kind {
                            match dc_kind {
                                D20CheckDCKind::SavingThrow(d20_check_dc) => {
                                    d20_check_dc.dc.add_modifier(
                                        ModifierSource::Action(action.clone()),
                                        modifier_value,
                                    );
                                }
                                D20CheckDCKind::Skill(d20_check_dc) => {
                                    d20_check_dc.dc.add_modifier(
                                        ModifierSource::Action(action.clone()),
                                        modifier_value,
                                    );
                                }
                                D20CheckDCKind::AttackRoll(_, armor_class) => {
                                    armor_class.add_modifier(
                                        ModifierSource::Action(action.clone()),
                                        modifier_value,
                                    );
                                }
                            }
                        } else {
                            panic!("ModifyD20DC applied to wrong event type: {:?}", event);
                        }
                    }
                }),
            };

            let process_event_result = game_state.process_event(Event::action_performed_event(
                game_state,
                &ActionData::from(reaction_data),
                vec![(reaction_data.reactor, ActionKindResult::Reaction { result })],
            ));

            match process_event_result {
                Ok(_) => {}
                Err(err) => {
                    error!(
                        "Error processing ModifyD20DC reaction for reactor {:?}: {:?}",
                        reaction_data.reactor, err
                    );
                }
            }
        }

        ScriptReactionPlan::RerollD20Result {
            bonus,
            force_use_new,
        } => {
            let bonus_value = if let Some(bonus_expr) = bonus {
                bonus_expr.evaluate(
                    &game_state.world,
                    reaction_data.reactor,
                    &reaction_data.context,
                )
            } else {
                0
            };

            let result = ReactionResult::ModifyEvent {
                modification: Arc::new({
                    let actor = reaction_data.event.actor().unwrap();
                    let action_id = reaction_data.reaction_id.clone();
                    move |world: &World, event: &mut Event| {
                        if let EventKind::D20CheckPerformed(
                            _,
                            ref mut existing_result,
                            ref dc_kind,
                        ) = event.kind
                        {
                            let mut new_roll = systems::d20::check_no_event(world, actor, dc_kind);
                            new_roll
                                .d20_result_mut()
                                .add_bonus(ModifierSource::Action(action_id.clone()), bonus_value);

                            if force_use_new {
                                *existing_result = new_roll;
                            } else {
                                // Choose the better of the two rolls
                                let existing_total = existing_result.d20_result().total();
                                let new_total = new_roll.d20_result().total();
                                if new_total > existing_total {
                                    *existing_result = new_roll;
                                }
                            }
                        } else {
                            panic!("RerollD20Result applied to wrong event type: {:?}", event);
                        }
                    }
                }),
            };

            let process_event_result = game_state.process_event(Event::action_performed_event(
                game_state,
                &ActionData::from(reaction_data),
                vec![(reaction_data.reactor, ActionKindResult::Reaction { result })],
            ));

            match process_event_result {
                Ok(_) => {}
                Err(err) => {
                    error!(
                        "Error processing RerollD20Result reaction for reactor {:?}: {:?}",
                        reaction_data.reactor, err
                    );
                }
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
                // TODO: Which resources should we refund if it's *not* the trigger event?
                resources_refunded.insert(
                    resource_id.clone(),
                    reaction_data
                        .resource_cost
                        .get(resource_id)
                        .cloned()
                        .unwrap(),
                );
            }

            let result = ReactionResult::CancelEvent {
                event: target_event.clone(),
                resources_refunded,
            };

            let process_event_result = game_state.process_event(Event::action_performed_event(
                game_state,
                &ActionData::from(reaction_data),
                vec![(
                    target_event.actor().unwrap(),
                    ActionKindResult::Reaction { result },
                )],
            ));

            match process_event_result {
                Ok(_) => {}
                Err(err) => {
                    error!(
                        "Error processing CancelEvent reaction for reactor {:?}: {:?}",
                        reaction_data.reactor, err
                    );
                }
            }
        }

        ScriptReactionPlan::RequireSavingThrow {
            target,
            dc,
            on_success,
            on_failure,
        } => {
            // Resolve the target entity
            let target_entity = match target {
                ScriptEntityRole::Reactor => reaction_data.reactor,
                ScriptEntityRole::Actor => reaction_data
                    .event
                    .actor()
                    .expect("Trigger event has no actor"),
                ScriptEntityRole::Target => reaction_data.event.target().expect(
                    "RequireSavingThrow reaction target role 'Target' but trigger event has no target",
                ),
            };

            // Resolve the DC spec to a real D20CheckDCKind
            let dc_kind = D20CheckDCKind::SavingThrow((dc.saving_throw.function)(
                &game_state.world,
                target_entity,
                &reaction_data.context,
            ));

            // Emit the check event and attach callback to continue the plan.
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

                    // Continue interpreting the reaction plan
                    apply_reaction_plan(game_state, &context_clone, next_plan);

                    CallbackResult::None
                } else {
                    panic!("RequireSavingThrow callback received unexpected event");
                }
            });

            let _ = game_state.process_event_with_callback(check_event, callback);
        }
    }
}
