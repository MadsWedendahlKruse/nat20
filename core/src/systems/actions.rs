use std::sync::Arc;

use hecs::{Entity, World};
use tracing::debug;

use crate::{
    components::{
        actions::{
            action::{
                Action, ActionCondition, ActionConditionResolution, ActionContext,
                ActionCooldownMap, ActionKind, ActionKindResult, ActionMap, ActionOutcomeBundle,
                ActionPayload, ActionProvider, AttackRollFunction, DamageOnFailure, DamageOutcome,
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
        action_prompt::{ActionData, ReactionData},
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
            action_usable(world, entity, action_id, &action_context, resource_cost).is_ok()
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
            if trigger(world, &reactor, event) {
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
