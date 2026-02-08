use std::sync::Arc;

use hecs::{Entity, World};
use tracing::{debug, warn};
use uuid::Uuid;

use crate::{
    components::{
        actions::{
            action::{
                Action, ActionCondition, ActionContext, ActionCooldownMap, ActionKind,
                ActionKindResult, ActionMap, ActionOutcomeBundle, ActionPayload, ActionProvider,
                AttackRollFunction, DamageOnFailure, DamageOutcome, EffectApplyCondition,
                EffectOutcome, HealingOutcome, SavingThrowFunction,
            },
            targeting::{
                AreaShape, TargetInstance, TargetingContext, TargetingError, TargetingKind,
            },
        },
        d20::D20CheckOutcome,
        damage::DamageRollResult,
        id::{ActionId, ResourceId, ScriptId},
        items::equipment::loadout::Loadout,
        modifier::{Modifiable, ModifierSource},
        resource::{RechargeRule, ResourceAmountMap, ResourceMap},
        spells::{
            spell::{ConcentrationInstance, SpellFlag},
            spellbook::Spellbook,
        },
    },
    engine::{
        action_prompt::{ActionData, ActionError, ReactionData},
        event::{CallbackResult, Event, EventCallback, EventKind},
        game_state::GameState,
        geometry::WorldGeometry,
    },
    registry::registry::{ActionsRegistry, SpellsRegistry},
    scripts::script_api::{
        ScriptEventView, ScriptReactionBodyContext, ScriptReactionTriggerContext,
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20ResultKind},
        geometry::RaycastFilter,
    },
};

pub fn get_action(action_id: &ActionId) -> Option<&Action> {
    // Start by checking if the action exists in the action registry
    if let Some(action) = ActionsRegistry::get(action_id) {
        return Some(action);
    }
    // If not found, check the spell registry
    let spell_id = action_id.into();
    if let Some(spell) = SpellsRegistry::get(&spell_id) {
        return Some(spell.action());
    }

    None
}

pub fn add_actions(world: &mut World, entity: Entity, actions: &[ActionId]) {
    let mut action_map = systems::helpers::get_component_mut::<ActionMap>(world, entity);
    for action_id in actions {
        if let Some(action) = systems::actions::get_action(action_id) {
            // TODO: Just assume the context is Other for now
            add_action_to_map(&mut action_map, action_id, action, ActionContext::Other);
        } else {
            panic!("Action {} not found in registry", action_id);
        }
    }
}

fn add_action_to_map(
    action_map: &mut ActionMap,
    action_id: &ActionId,
    action: &Action,
    context: ActionContext,
) {
    let resource_cost = &action.resource_cost().clone();
    action_map
        .entry(action_id.clone())
        .and_modify(|action_data| {
            action_data.push((context.clone(), resource_cost.clone()));
        })
        .or_insert(vec![(context, resource_cost.clone())]);
}

pub fn on_cooldown(world: &World, entity: Entity, action_id: &ActionId) -> Option<RechargeRule> {
    if let Some(cooldowns) = world.get::<&ActionCooldownMap>(entity).ok() {
        cooldowns.get(action_id).cloned()
    } else {
        None
    }
}

pub fn set_cooldown(
    world: &mut World,
    entity: Entity,
    action_id: &ActionId,
    cooldown: RechargeRule,
) {
    let mut cooldowns = systems::helpers::get_component_mut::<ActionCooldownMap>(world, entity);
    cooldowns.insert(action_id.clone(), cooldown);
}

pub fn all_actions(world: &World, entity: Entity) -> ActionMap {
    let mut actions = systems::helpers::get_component_clone::<ActionMap>(world, entity);
    actions
        .extend(systems::helpers::get_component::<Spellbook>(world, entity).actions(world, entity));
    actions
        .extend(systems::helpers::get_component::<Loadout>(world, entity).actions(world, entity));
    actions
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionUsabilityError {
    EntityNotAlive(Entity),
    OnCooldown(RechargeRule),
    NotEnoughResources(ResourceAmountMap),
    ResourceNotFound(ResourceId),
    TargetingError(TargetingError),
}

pub fn action_usable(
    world: &World,
    entity: Entity,
    action_id: &ActionId,
    // TODO: Is context really not needed here?
    action_context: &ActionContext,
    resource_cost: &ResourceAmountMap,
) -> Result<(), ActionUsabilityError> {
    if !systems::health::is_alive(world, entity) {
        return Err(ActionUsabilityError::EntityNotAlive(entity));
    }

    if let Some(cooldown) = on_cooldown(world, entity, action_id) {
        return Err(ActionUsabilityError::OnCooldown(cooldown));
    }

    if !systems::helpers::get_component::<ResourceMap>(world, entity)
        .can_afford_all(resource_cost)
        .0
    {
        return Err(ActionUsabilityError::NotEnoughResources(
            resource_cost.clone(),
        ));
    }

    Ok(())
}

pub fn action_usable_on_targets(
    world: &World,
    world_geometry: &WorldGeometry,
    actor: Entity,
    action_id: &ActionId,
    context: &ActionContext,
    resource_cost: &ResourceAmountMap,
    targets: &[TargetInstance],
) -> Result<(), ActionUsabilityError> {
    action_usable(world, actor, action_id, context, resource_cost)?;

    let targeting_context = targeting_context(world, actor, action_id, context);

    if let Err(targeting_error) =
        targeting_context.validate_targets(world, world_geometry, actor, targets)
    {
        return Err(ActionUsabilityError::TargetingError(targeting_error));
    }

    Ok(())
}

pub fn available_actions(world: &World, entity: Entity) -> ActionMap {
    let mut actions = all_actions(world, entity);

    actions.retain(|action_id, action_data| {
        action_data.retain_mut(|(action_context, resource_cost)| {
            for effect in systems::effects::effects(world, entity).values() {
                (effect.effect().on_resource_cost)(
                    world,
                    entity,
                    action_id,
                    action_context,
                    resource_cost,
                );
            }
            // action_usable(world, entity, action_id, &action_context, resource_cost).is_ok()
            let result = action_usable(world, entity, action_id, action_context, resource_cost);
            match result {
                Ok(_) => true,
                Err(e) => {
                    println!(
                        "Action {:?} with context {:?} is not usable for entity {:?} due to {:?}",
                        action_id, action_context, entity, e
                    );
                    false
                }
            }
        });

        !action_data.is_empty() // Keep the action if there's at least one usable context
    });

    actions
}

pub fn perform_action(game_state: &mut GameState, action_data: &ActionData) {
    // TODO: Handle missing action
    let mut action = get_action(&action_data.action_id)
        .cloned()
        .expect("Action not found in character's actions or registry");
    // Set the action on cooldown if applicable
    if let Some(cooldown) = action.cooldown {
        set_cooldown(
            &mut game_state.world,
            action_data.actor,
            &action_data.action_id,
            cooldown,
        );
    }
    // Determine which entities are being targeted
    let entities = get_targeted_entities(game_state, action_data);
    debug!(
        "Performing action {:?} by entity {:?} on targets {:?}",
        action_data.action_id, action_data.actor, entities
    );
    action.perform(game_state, action_data, &entities);
}

fn get_targeted_entities(game_state: &mut GameState, action_data: &ActionData) -> Vec<Entity> {
    let mut entities = Vec::new();
    let targeting_context = targeting_context(
        &game_state.world,
        action_data.actor,
        &action_data.action_id,
        &action_data.context,
    );
    match &targeting_context.kind {
        TargetingKind::SelfTarget | TargetingKind::Single | TargetingKind::Multiple { .. } => {
            for target in &action_data.targets {
                match target {
                    TargetInstance::Entity(entity) => entities.push(*entity),
                    TargetInstance::Point(point) => {
                        if let Some(entity) =
                            systems::geometry::get_entity_at_point(&game_state.world, *point)
                        {
                            entities.push(entity);
                        }
                    }
                }
            }
        }

        TargetingKind::Area {
            shape,
            fixed_on_actor,
        } => {
            for target in &action_data.targets {
                let point = match target {
                    TargetInstance::Entity(entity) => {
                        &systems::geometry::get_foot_position(&game_state.world, *entity).unwrap()
                    }

                    TargetInstance::Point(point) => point,
                };

                let (shape_hitbox, shape_pose) = shape.parry3d_shape(
                    &game_state.world,
                    action_data.actor,
                    *fixed_on_actor,
                    point,
                );

                let mut entities_in_shape = systems::geometry::entities_in_shape(
                    &game_state.world,
                    shape_hitbox,
                    &shape_pose,
                );

                // Only keep the entities that are valid targets
                entities_in_shape
                    .retain(|entity| targeting_context.allowed_target(&game_state.world, entity));

                // Check if any of the entities are behind cover and remove them
                // TODO: Not sure what the best way to do this is, I guess it
                // depends on the shape?

                match shape {
                    AreaShape::Sphere { .. } => {
                        entities_in_shape.retain(|entity| {
                            systems::geometry::line_of_sight_entity_point_filter(
                                &game_state.world,
                                &game_state.geometry,
                                *entity,
                                *point,
                                // TODO: Can't hide behind other entities?
                                &RaycastFilter::WorldOnly,
                            )
                            .has_line_of_sight
                        });
                    }

                    _ => {}
                }

                entities.extend(entities_in_shape);
            }
        }
    }
    entities
}

pub fn targeting_context(
    world: &World,
    entity: Entity,
    action_id: &ActionId,
    context: &ActionContext,
) -> TargetingContext {
    // TODO: Handle missing action
    get_action(action_id).unwrap().targeting()(world, entity, context)
}

pub fn available_reactions_to_event(
    world: &World,
    world_geometry: &WorldGeometry,
    reactor: Entity,
    event: &Event,
) -> Vec<ReactionData> {
    let mut reactions = Vec::new();

    for (reaction_id, contexts_and_costs) in systems::actions::available_actions(world, reactor) {
        let reaction = systems::actions::get_action(&reaction_id);
        if reaction.is_none() {
            continue;
        }
        let reaction = reaction.unwrap();

        if let Some(trigger) = &reaction.reaction_trigger {
            let Some(script_event) = ScriptEventView::from_event(event) else {
                continue;
            };
            let context = ScriptReactionTriggerContext {
                reactor: reactor.into(),
                event: script_event,
            };
            if systems::scripts::evaluate_reaction_trigger(trigger, &context) {
                for (context, resource_cost) in &contexts_and_costs {
                    let self_target = matches!(
                        targeting_context(world, reactor, &reaction_id, context).kind,
                        TargetingKind::SelfTarget
                    );
                    let target = if self_target {
                        TargetInstance::Entity(reactor)
                    } else {
                        TargetInstance::Entity(event.actor().unwrap())
                    };
                    if action_usable_on_targets(
                        world,
                        world_geometry,
                        reactor,
                        &reaction_id,
                        context,
                        resource_cost,
                        &[target.clone()],
                    )
                    .is_ok()
                    {
                        reactions.push(ReactionData::new(
                            reactor,
                            event.clone().into(),
                            reaction_id.clone(),
                            context.clone(),
                            resource_cost.clone(),
                            target,
                        ));
                    }
                }
            }
        }
    }

    reactions
}

pub fn perform_reaction(game_state: &mut GameState, reaction_data: &ReactionData) {
    let action = get_action(&reaction_data.reaction_id)
        .unwrap_or_else(|| panic!("Reaction action not found: {:?}", reaction_data.reaction_id));

    match &action.kind {
        ActionKind::Reaction { reaction } => {
            evaluate_and_apply_reaction(game_state, reaction, reaction_data);
        }

        ActionKind::Composite { actions } => {
            for action in actions {
                match action {
                    ActionKind::Reaction { reaction } => {
                        evaluate_and_apply_reaction(game_state, reaction, reaction_data);
                    }

                    _ => {
                        perform_action(game_state, &ActionData::from(reaction_data));
                    }
                }
            }
        }

        _ => {
            perform_action(game_state, &ActionData::from(reaction_data));
        }
    }
}

fn evaluate_and_apply_reaction(
    game_state: &mut GameState,
    reaction: &ScriptId,
    reaction_data: &ReactionData,
) {
    let plan = systems::scripts::evaluate_reaction_body(
        reaction,
        &ScriptReactionBodyContext::from(reaction_data),
    );
    systems::scripts::apply_reaction_plan(game_state, reaction_data, plan);
}

pub fn perform_standard_action(
    game_state: &mut GameState,
    action_kind: &ActionKind,
    action_data: &ActionData,
    target: Entity,
) -> Result<(), ActionError> {
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
) -> Result<(), ActionError> {
    // Apply effect immediately (no gating for unconditional).
    let effect_outcome: Option<EffectOutcome> = get_effect_outcome(
        game_state,
        target,
        &action_data,
        &payload,
        EffectApplyCondition::Unconditional,
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

    game_state.process_event_with_callback(damage_event, callback)
}

fn perform_attack_roll(
    game_state: &mut GameState,
    action_data: &ActionData,
    target: Entity,
    attack_roll_function: &Arc<AttackRollFunction>,
    payload: &ActionPayload,
    damage_on_miss: &Option<DamageOnFailure>,
) -> Result<(), ActionError> {
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
                        EffectApplyCondition::OnHit {
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

    game_state.process_event_with_callback(attack_event, callback)
}

fn perform_saving_throw(
    game_state: &mut GameState,
    action_data: &ActionData,
    target: Entity,
    saving_throw_function: &Arc<SavingThrowFunction>,
    payload: &ActionPayload,
    damage_on_save: &Option<DamageOnFailure>,
) -> Result<(), ActionError> {
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
                        EffectApplyCondition::OnFailedSave {
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

    game_state.process_event_with_callback(saving_throw_event, callback)
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
    apply_condition: EffectApplyCondition,
) -> Option<EffectOutcome> {
    payload.effect().map(|effect| {
        let effect_instance_id = systems::effects::add_effect_template(
            game_state,
            action_data.actor,
            target,
            ModifierSource::Action(action_data.action_id.clone()),
            effect,
            Some(&action_data.context),
            apply_condition.clone(),
        );

        // Add concentration tracking if needed
        let spell_id = action_data.action_id.clone().into();
        if let Some(spell) = SpellsRegistry::get(&spell_id) {
            if spell.has_flag(SpellFlag::Concentration) {
                systems::spells::add_concentration_instance(
                    &mut game_state.world,
                    action_data.actor,
                    ConcentrationInstance::Effect {
                        entity: target,
                        effect: effect_instance_id,
                    },
                    &action_data.instance_id,
                );
            }
        }

        EffectOutcome {
            effect: effect.effect_id.clone(),
            applied: true,
            rule: apply_condition,
        }
    })
}
