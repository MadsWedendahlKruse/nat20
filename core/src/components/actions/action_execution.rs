/// The core logic for executing a standard action is big enough to warrant its
/// own module. This includes performing the necessary rolls, applying effects,
/// and emitting the final ActionPerformed event with the combined result.
use std::sync::Arc;

use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::{
            ActionCondition, ActionConditionResolution, ActionContext, ActionKind,
            ActionKindResult, ActionOutcomeBundle, ActionPayload, AttackRollFunction,
            DamageOnFailure, DamageOutcome, EffectOutcome, HealingOutcome, SavingThrowFunction,
        },
        d20::D20CheckOutcome,
        damage::DamageRollResult,
        id::ActionId,
        modifier::{Modifiable, ModifierSource},
        spells::spell::{ConcentrationInstance, SpellFlag},
    },
    engine::{
        action_prompt::ActionData,
        event::{CallbackResult, Event, EventCallback, EventKind},
        game_state::GameState,
    },
    registry::registry::SpellsRegistry,
    systems::{
        self,
        d20::{D20CheckDCKind, D20ResultKind},
    },
};

pub fn perform_standard_action(
    game_state: &mut GameState,
    action_kind: &ActionKind,
    action_data: &ActionData,
    target: Entity,
) {
    match action_kind {
        ActionKind::Standard { condition, payload } => match condition {
            ActionCondition::None => {
                perform_unconditional(game_state, action_data, target, payload)
            }
            ActionCondition::AttackRoll {
                attack_roll,
                damage_on_miss,
            } => perform_attack_roll(
                game_state,
                action_data,
                target,
                attack_roll,
                payload,
                damage_on_miss,
            ),
            ActionCondition::SavingThrow {
                saving_throw,
                damage_on_save,
            } => perform_saving_throw(
                game_state,
                action_data,
                target,
                saving_throw,
                payload,
                damage_on_save,
            ),
        },

        _ => {
            // Composite/Healing/Utility/Reaction/Custom handled elsewhere
            unimplemented!("perform_standard_action called with non-standard ActionKind");
        }
    }
}

fn perform_unconditional(
    game_state: &mut GameState,
    action_data: &ActionData,
    target: Entity,
    payload: &ActionPayload,
) {
    // Apply effect immediately (no gating for unconditional).
    let effect_outcome: Option<EffectOutcome> = get_effect_outcome(
        game_state,
        target,
        &action_data,
        &payload,
        ActionConditionResolution::Unconditional,
    );

    // Apply healing immediately (no gating for unconditional).
    let healing_outcome: Option<HealingOutcome> = payload.healing().map(|healing_amount| {
        let healing_amount =
            healing_amount(&game_state.world, action_data.actor, &action_data.context).roll();
        let new_life_state = systems::health::heal(
            &mut game_state.world,
            target,
            healing_amount.subtotal as u32,
        );

        HealingOutcome {
            healing: healing_amount,
            new_life_state,
        }
    });

    let Some(damage_roll) = get_damage_roll(
        &game_state.world,
        action_data.actor,
        &action_data.action_id,
        payload,
        &None,
        "Unconditional Damage".to_string(),
        &action_data.context,
        true,
        false,
    ) else {
        // If there is no damage, we can emit the ActionPerformed immediately.
        let result: ActionKindResult = ActionKindResult::Standard(ActionOutcomeBundle {
            damage: None,
            effect: effect_outcome,
            healing: healing_outcome,
        });

        return game_state.process_event(Event::action_performed_event(
            game_state,
            action_data,
            vec![(target, result)],
        ));
    };

    // Otherwise, do the damage roll event, and in the callback emit the combined result.
    let damage_event = Event::new(EventKind::DamageRollPerformed(
        action_data.actor,
        damage_roll,
    ));

    let callback = EventCallback::new({
        let action_data = action_data.clone();
        let effect_result = effect_outcome.clone();

        move |game_state, event, _| match &event.kind {
            EventKind::DamageRollResolved(_, damage_roll_result) => {
                let (damage_taken, new_life_state) =
                    systems::health::damage(game_state, target, damage_roll_result, None);

                let damage_outcome = DamageOutcome::unconditional(
                    Some(damage_roll_result.clone()),
                    damage_taken,
                    new_life_state,
                );

                let result = ActionKindResult::Standard(ActionOutcomeBundle {
                    damage: Some(damage_outcome),
                    effect: effect_result.clone(),
                    healing: healing_outcome.clone(),
                });

                CallbackResult::Event(Event::action_performed_event(
                    game_state,
                    &action_data,
                    vec![(target, result)],
                ))
            }
            _ => panic!(
                "Unexpected event kind in unconditional callback: {:?}",
                event
            ),
        }
    });

    game_state.process_event_with_response_callback(damage_event, callback)
}

fn perform_attack_roll(
    game_state: &mut GameState,
    action_data: &ActionData,
    target: Entity,
    attack_roll_function: &Arc<AttackRollFunction>,
    payload: &ActionPayload,
    damage_on_miss: &Option<DamageOnFailure>,
) {
    let attack_roll = systems::damage::attack_roll_fn(
        attack_roll_function.as_ref(),
        &game_state.world,
        action_data.actor,
        target,
        &action_data.context,
    );

    let armor_class = systems::loadout::armor_class(&game_state.world, target);

    let attack_event = Event::new(EventKind::D20CheckPerformed(
        action_data.actor,
        D20ResultKind::AttackRoll {
            result: attack_roll.clone(),
        },
        D20CheckDCKind::AttackRoll(target, armor_class),
    ));

    let callback = EventCallback::new({
        let action_data = action_data.clone();
        let attack_roll = attack_roll.clone();
        let payload = payload.clone();
        let damage_on_miss = damage_on_miss.clone();

        move |game_state, event, _| match &event.kind {
            EventKind::D20CheckResolved(_, result, dc) => {
                let armor_class = match dc {
                    D20CheckDCKind::AttackRoll(_, armor_class) => armor_class.clone(),
                    _ => panic!("Expected AttackRoll DC in callback, got {:?}", dc),
                };

                let hit = result.is_success(dc);

                // Decide effect application
                let effect_result: Option<EffectOutcome> = if hit {
                    get_effect_outcome(
                        game_state,
                        target,
                        &action_data,
                        &payload,
                        ActionConditionResolution::AttackRoll {
                            attack_roll: attack_roll.clone(),
                            armor_class: armor_class.clone(),
                        },
                    )
                } else {
                    None
                };

                let damage_roll = get_damage_roll(
                    &game_state.world,
                    action_data.actor,
                    &action_data.action_id,
                    &payload,
                    &damage_on_miss,
                    "Attack Miss".to_string(),
                    &action_data.context,
                    hit,
                    matches!(
                        result.d20_result().outcome,
                        Some(D20CheckOutcome::CriticalSuccess)
                    ),
                );

                // If no damage or not hit, return immediately.
                if damage_roll.is_none() || !hit {
                    let result = ActionKindResult::Standard(ActionOutcomeBundle {
                        damage: Some(DamageOutcome::attack_roll(
                            None,
                            None,
                            None,
                            attack_roll.clone(),
                            armor_class,
                        )),
                        effect: effect_result.clone(),
                        healing: None,
                    });

                    return CallbackResult::Event(Event::action_performed_event(
                        game_state,
                        &action_data,
                        vec![(target, result)],
                    ));
                };

                let damage_event = Event::new(EventKind::DamageRollPerformed(
                    action_data.actor,
                    damage_roll.unwrap(),
                ));

                CallbackResult::EventWithCallback(
                    damage_event,
                    EventCallback::new({
                        let action_data = action_data.clone();
                        let attack_roll = attack_roll.clone();
                        let armor_class = armor_class.clone();
                        let hit = hit;
                        let effect_result = effect_result.clone();

                        move |game_state, event, _| match &event.kind {
                            EventKind::DamageRollResolved(_, damage_roll_result) => {
                                let (damage_taken, new_life_state) = if hit {
                                    systems::health::damage(
                                        game_state,
                                        target,
                                        damage_roll_result,
                                        Some(&attack_roll),
                                    )
                                } else {
                                    (None, None)
                                };

                                let damage_outcome = DamageOutcome::attack_roll(
                                    Some(damage_roll_result.clone()),
                                    damage_taken,
                                    new_life_state,
                                    attack_roll.clone(),
                                    armor_class.clone(),
                                );

                                let result = ActionKindResult::Standard(ActionOutcomeBundle {
                                    damage: Some(damage_outcome),
                                    effect: effect_result.clone(),
                                    healing: None,
                                });

                                CallbackResult::Event(Event::action_performed_event(
                                    game_state,
                                    &action_data,
                                    vec![(target, result)],
                                ))
                            }
                            _ => panic!("Unexpected event kind in damage callback: {:?}", event),
                        }
                    }),
                )
            }
            _ => panic!("Unexpected event kind in attack roll callback: {:?}", event),
        }
    });

    game_state.process_event_with_response_callback(attack_event, callback)
}

fn perform_saving_throw(
    game_state: &mut GameState,
    action_data: &ActionData,
    target: Entity,
    saving_throw_function: &Arc<SavingThrowFunction>,
    payload: &ActionPayload,
    damage_on_save: &Option<DamageOnFailure>,
) {
    let saving_throw_dc =
        saving_throw_function(&game_state.world, action_data.actor, &action_data.context);

    let saving_throw_event = systems::d20::check(
        game_state,
        target,
        &D20CheckDCKind::SavingThrow(saving_throw_dc.clone()),
    );

    let callback = EventCallback::new({
        let action_data = action_data.clone();
        let payload = payload.clone();
        let damage_on_save = damage_on_save.clone();

        move |game_state, event, _| match &event.kind {
            EventKind::D20CheckResolved(_, result, dc) => {
                let saving_throw_dc = match dc {
                    D20CheckDCKind::SavingThrow(dc) => dc.clone(),
                    _ => panic!("Expected SavingThrow DC in callback, got {:?}", dc),
                };

                let save_success = result.is_success(dc);

                // Decide effect application
                let effect_result: Option<EffectOutcome> = if save_success {
                    None
                } else {
                    get_effect_outcome(
                        game_state,
                        target,
                        &action_data,
                        &payload,
                        ActionConditionResolution::SavingThrow {
                            saving_throw_result: result.d20_result().clone(),
                            saving_throw_dc: saving_throw_dc.clone(),
                        },
                    )
                };

                // If no damage, emit effect result immediately.
                let Some(damage_roll) = get_damage_roll(
                    &game_state.world,
                    action_data.actor,
                    &action_data.action_id,
                    &payload,
                    &damage_on_save,
                    "Successful Save".to_string(),
                    &action_data.context,
                    !save_success,
                    false,
                ) else {
                    let result = ActionKindResult::Standard(ActionOutcomeBundle {
                        damage: None,
                        effect: effect_result.clone(),
                        healing: None,
                    });

                    return CallbackResult::Event(Event::action_performed_event(
                        game_state,
                        &action_data,
                        vec![(target, result)],
                    ));
                };

                let damage_event = Event::new(EventKind::DamageRollPerformed(
                    action_data.actor,
                    damage_roll,
                ));

                CallbackResult::EventWithCallback(
                    damage_event,
                    EventCallback::new({
                        let action_data = action_data.clone();
                        let saving_throw_result = result.clone();
                        let saving_throw_dc = saving_throw_dc.clone();
                        let effect_result = effect_result.clone();

                        move |game_state, event, _| match &event.kind {
                            EventKind::DamageRollResolved(_, damage_roll_result) => {
                                let (damage_taken, new_life_state) = systems::health::damage(
                                    game_state,
                                    target,
                                    damage_roll_result,
                                    None,
                                );

                                let damage_outcome = DamageOutcome::saving_throw(
                                    Some(damage_roll_result.clone()),
                                    damage_taken,
                                    new_life_state,
                                    saving_throw_dc.clone(),
                                    saving_throw_result.d20_result().clone(),
                                );

                                let result = ActionKindResult::Standard(ActionOutcomeBundle {
                                    damage: Some(damage_outcome),
                                    effect: effect_result.clone(),
                                    healing: None,
                                });

                                CallbackResult::Event(Event::action_performed_event(
                                    game_state,
                                    &action_data,
                                    vec![(target, result)],
                                ))
                            }
                            _ => panic!("Unexpected event kind in damage callback: {:?}", event),
                        }
                    }),
                )
            }
            _ => panic!(
                "Unexpected event kind in saving throw callback: {:?}",
                event
            ),
        }
    });

    game_state.process_event_with_response_callback(saving_throw_event, callback)
}

// TODO: Doesn't seem like the cleanest solution
fn get_damage_roll(
    world: &World,
    entity: Entity,
    action: &ActionId,
    payload: &ActionPayload,
    damage_on_failure: &Option<DamageOnFailure>,
    failure_label: String,
    context: &ActionContext,
    success: bool,
    crit: bool,
) -> Option<DamageRollResult> {
    let damage_function = if let Some(damage_on_failure) = &damage_on_failure
        && !success
    {
        match damage_on_failure {
            DamageOnFailure::Half => payload.damage().cloned(),
            DamageOnFailure::Custom(func) => Some(func.clone()),
        }
    } else {
        payload.damage().cloned()
    };

    let Some(damage_function) = damage_function else {
        return None;
    };

    let mut damage_roll =
        systems::damage::damage_roll_fn(damage_function.as_ref(), world, entity, context, crit);
    damage_roll.action = Some((entity, action.clone()));

    if let Some(damage_on_failure) = damage_on_failure {
        match damage_on_failure {
            DamageOnFailure::Half if !success => {
                let mut half_damage_roll = damage_roll.clone();
                for component in half_damage_roll.components.iter_mut() {
                    let total = component.result.subtotal;
                    component.result.modifiers.add_modifier(
                        ModifierSource::Custom(failure_label.clone()),
                        -(total as f32 / 2.0).ceil() as i32,
                    );
                }
                half_damage_roll.recalculate_total();
                Some(half_damage_roll)
            }
            _ => Some(damage_roll),
        }
    } else {
        Some(damage_roll)
    }
}

fn get_effect_outcome(
    game_state: &mut GameState,
    target: Entity,
    action_data: &ActionData,
    payload: &ActionPayload,
    action_resolution: ActionConditionResolution,
) -> Option<EffectOutcome> {
    payload.effect().map(|effect| {
        let effect_instance_id = systems::effects::add_effect_template(
            game_state,
            action_data.actor,
            target,
            ModifierSource::Action(action_data.action_id.clone()),
            effect,
            Some(&action_data.context),
            action_resolution.clone(),
        );

        // Add concentration tracking if needed
        let spell_id = action_data.action_id.clone().into();
        if let Some(spell) = SpellsRegistry::get(&spell_id) {
            if spell.has_flag(SpellFlag::Concentration) {
                systems::spells::add_concentration_instance(
                    game_state,
                    action_data.actor,
                    ConcentrationInstance::Effect {
                        entity: target,
                        effect: effect.effect_id.clone(),
                        instance: effect_instance_id,
                    },
                    &action_data.instance_id,
                );
            }
        }

        EffectOutcome {
            resolution: action_resolution,
            effect: effect.effect_id.clone(),
            applied: true,
        }
    })
}
