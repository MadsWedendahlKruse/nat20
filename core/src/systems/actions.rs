use hecs::{Entity, World};
use parry3d::na::Point3;
use tracing::debug;

use crate::{
    components::{
        actions::{
            action::{
                Action, ActionContext, ActionCooldownMap, ActionMap, ActionProvider,
                AreaShapeFunction,
            },
            execution::PhaseState,
            execution::{ActionExecution, ExecutionStatus, WaitReason},
            targeting::{
                AreaFilter, AreaShape, LineOfSightTrajectory, TargetInstance, TargetingCheck,
                TargetingContext, TargetingError, TargetingKind,
            },
        },
        activity::{ActivityPauseReason, ActivityState},
        id::{ActionId, EntityIdentifier, ResourceId},
        items::equipment::loadout::Loadout,
        resource::{RechargeRule, ResourceAmountMap},
        spells::spellbook::Spellbook,
    },
    engine::{
        action_prompt::ActionData, event::Event, game_state::GameState, geometry::WorldGeometry,
        interaction::InteractionScopeId,
    },
    entities::projectile::Projectile,
    registry::registry::{ActionsRegistry, SpellsRegistry},
    systems::{self, geometry::RaycastFilter},
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
            // TODO: Figure out the actual context to add here
            add_action_to_map(&mut action_map, action_id, action, ActionContext::default());
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
    merge_action_maps(
        &mut actions,
        systems::helpers::get_component::<Spellbook>(world, entity).actions(world, entity),
    );
    merge_action_maps(
        &mut actions,
        systems::helpers::get_component::<Loadout>(world, entity).actions(world, entity),
    );
    actions
}

fn merge_action_maps(destination: &mut ActionMap, source: ActionMap) {
    for (action_id, contexts) in source {
        destination
            .entry(action_id)
            .and_modify(|existing| existing.extend(contexts.clone()))
            .or_insert(contexts);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionUsabilityError {
    EntityNotAlive(Entity),
    OnCooldown(RechargeRule),
    NotEnoughResources(Vec<ResourceId>),
    ResourceNotFound(ResourceId),
    TargetingError(TargetingError),
}

// TODO: Not sure if this is the best way to handle skipping checks. All the call
// sites get contaminated, and it's only used in one place so far
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ActionUsabilityCheck {
    Alive,
    Cooldown,
    Resources,
    Targeting(Vec<TargetingCheck>),
}

pub fn action_usable(
    world: &World,
    entity: Entity,
    action_id: &ActionId,
    // TODO: Is context really not needed here?
    action_context: &ActionContext,
    resource_cost: &ResourceAmountMap,
    skip_checks: &[ActionUsabilityCheck],
) -> Result<(), ActionUsabilityError> {
    if !skip_checks.contains(&ActionUsabilityCheck::Alive)
        && !systems::health::is_alive(world, entity)
    {
        return Err(ActionUsabilityError::EntityNotAlive(entity));
    }

    if !skip_checks.contains(&ActionUsabilityCheck::Cooldown)
        && let Some(cooldown) = on_cooldown(world, entity, action_id)
    {
        return Err(ActionUsabilityError::OnCooldown(cooldown));
    }

    if !skip_checks.contains(&ActionUsabilityCheck::Resources)
        && let Err(missing_resources) = systems::resources::can_afford(world, entity, resource_cost)
    {
        return Err(ActionUsabilityError::NotEnoughResources(missing_resources));
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
    skip_checks: &[ActionUsabilityCheck],
) -> Result<(), ActionUsabilityError> {
    action_usable(world, actor, action_id, context, resource_cost, skip_checks)?;

    let targeting_context = targeting_context(world, actor, action_id, context);

    let targeting_check = skip_checks
        .iter()
        .find_map(|check| {
            if let ActionUsabilityCheck::Targeting(check) = check {
                Some(check.clone())
            } else {
                None
            }
        })
        .unwrap_or(Vec::new()); // If no targeting checks are being skipped, use an empty vector

    if let Err(targeting_error) =
        targeting_context.validate_targets(world, world_geometry, actor, targets, &targeting_check)
    {
        return Err(ActionUsabilityError::TargetingError(targeting_error));
    }

    Ok(())
}

pub fn available_actions(game_state: &GameState, entity: Entity) -> ActionMap {
    let mut actions = all_actions(&game_state.world, entity);

    actions.retain(|action_id, action_data| {
        action_data.retain_mut(|(action_context, resource_cost)| {
            systems::effects::effects(&game_state.world, entity).resource_cost(
                game_state,
                entity,
                action_id,
                action_context,
                resource_cost,
            );
            action_usable(
                &game_state.world,
                entity,
                action_id,
                &action_context,
                resource_cost,
                &[],
            )
            .is_ok()
        });

        !action_data.is_empty() // Keep the action if there's at least one usable context
    });

    actions
}

pub fn perform_action(game_state: &mut GameState, action_data: &ActionData) {
    // TODO: Handle missing action
    let action = get_action(&action_data.action_id)
        .expect("Action not found in character's actions or registry");

    // Set the action on cooldown if applicable
    if let Some(cooldown) = action.cooldown {
        set_cooldown(
            &mut game_state.world,
            action_data.actor.id(),
            &action_data.action_id,
            cooldown,
        );
    }

    let phases = action.perform(game_state, action_data);

    start_execution(game_state, action, action_data.clone(), phases);
}

/// Begin executing an action: the `ActionExecution` owns the phases from here
/// on; the actor's activity state drives it along the action's timeline.
pub fn start_execution(
    game_state: &mut GameState,
    action: &Action,
    action_data: ActionData,
    phases: Vec<PhaseState>,
) {
    if phases.is_empty() {
        debug!("No phases for action, nothing to execute");
        return;
    }

    let actor = action_data.actor.id();
    game_state
        .action_executions
        .insert(actor, ActionExecution::new(action_data, phases));
    systems::helpers::get_component_mut::<ActivityState>(&mut game_state.world, actor)
        .set_acting(action);
}

pub fn execution_status(game_state: &GameState, entity: Entity) -> Option<ExecutionStatus> {
    game_state
        .action_executions
        .get(&entity)
        .map(ActionExecution::status)
}

/// Runs `operation` on the entity's execution with full access to the game
/// state; the execution is temporarily taken out of the table for borrow
/// separation
fn with_execution(
    game_state: &mut GameState,
    entity: Entity,
    operation: impl FnOnce(&mut ActionExecution, &mut GameState),
) {
    let Some(mut execution) = game_state.action_executions.remove(&entity) else {
        return;
    };
    operation(&mut execution, game_state);
    game_state.action_executions.insert(entity, execution);
}

pub fn advance_execution(game_state: &mut GameState, entity: Entity) {
    with_execution(game_state, entity, |execution, game_state| {
        execution.advance(game_state)
    });
}

pub fn projectile_impact(game_state: &mut GameState, entity: Entity) {
    with_execution(game_state, entity, |execution, game_state| {
        execution.resume_from_projectile(game_state)
    });
}

/// Re-run executions in this scope that are waiting on an event resolution.
/// Safe to call speculatively: an execution whose result hasn't arrived yet
/// simply parks again.
pub fn resume_waiting_executions(game_state: &mut GameState, scope: InteractionScopeId) {
    let waiting: Vec<Entity> = game_state
        .action_executions
        .iter()
        .filter(|(entity, execution)| {
            execution.status() == ExecutionStatus::Waiting(WaitReason::EventResolution)
                && game_state.scope_for_entity(**entity) == scope
        })
        .map(|(entity, _)| *entity)
        .collect();

    for entity in waiting {
        with_execution(game_state, entity, |execution, game_state| {
            execution.resume_from_event(game_state)
        });
    }
}

pub fn get_targeted_entities(
    game_state: &GameState,
    action_data: &ActionData,
    specific_targets: Option<Vec<TargetInstance>>,
) -> Vec<Entity> {
    let mut entities = Vec::new();
    let targeting_context = targeting_context(
        &game_state.world,
        action_data.actor.id(),
        &action_data.action_id,
        &action_data.context,
    );

    let targets = specific_targets.unwrap_or_else(|| action_data.targets.clone());

    match &targeting_context.kind {
        TargetingKind::SelfTarget | TargetingKind::Single | TargetingKind::Multiple { .. } => {
            for target in &targets {
                match target {
                    TargetInstance::Entity { entity, .. } => entities.push(entity.id()),
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
            filters,
        } => {
            for target in &targets {
                let point = target_point(game_state, target);
                entities.extend(entities_in_area(
                    game_state,
                    action_data,
                    &targeting_context,
                    shape,
                    *fixed_on_actor,
                    &point,
                ));
            }

            // Edge case handling for targeting unoccupied areas, e.g. Misty Step
            // If there are no targets the corresponding `PhaseState` will not have
            // any steps, so the action won't do anything. To avoid this, we return
            // the actor as the target
            for filter in filters {
                if filter == &AreaFilter::Unoccupied && entities.is_empty() {
                    return vec![action_data.actor.id()];
                }
            }
        }
    }
    entities
}

/// Entities inside `shape` centered on `target`, filtered by the action's own
/// allowed-target rules. Used by action phases that derive their targets from a
/// previously chosen target (e.g. Ice Knife's burst around the struck creature).
pub fn entities_in_shape_at_target(
    game_state: &GameState,
    action_data: &ActionData,
    target: &TargetInstance,
    shape: &AreaShapeFunction,
) -> Vec<Entity> {
    let targeting_context = targeting_context_data(&game_state.world, action_data);
    let shape = shape(
        &game_state.world,
        action_data.actor.id(),
        &action_data.context,
    );
    let point = target_point(game_state, target);
    entities_in_area(
        game_state,
        action_data,
        &targeting_context,
        &shape,
        false,
        &point,
    )
}

fn target_point(game_state: &GameState, target: &TargetInstance) -> Point3<f32> {
    match target {
        TargetInstance::Entity { entity, .. } => {
            systems::geometry::get_foot_position(&game_state.world, entity.id()).unwrap()
        }
        TargetInstance::Point(point) => *point,
    }
}

fn entities_in_area(
    game_state: &GameState,
    action_data: &ActionData,
    targeting_context: &TargetingContext,
    shape: &AreaShape,
    fixed_on_actor: bool,
    point: &Point3<f32>,
) -> Vec<Entity> {
    let shape_transform = shape.parry3d_shape(
        &game_state.world,
        action_data.actor.id(),
        fixed_on_actor,
        point,
    );

    let mut entities_in_shape = systems::geometry::entities_in_shape(
        &game_state.world,
        shape_transform.shape.as_ref(),
        &shape_transform.transform,
    );

    // Only keep the entities that are valid targets
    entities_in_shape.retain(|entity| {
        targeting_context
            .allowed_target(&game_state.world, *entity, Some(action_data.actor.id()))
            .is_ok()
    });

    // Check if any of the entities are behind cover and remove them
    // TODO: Not sure what the best way to do this is, I guess it
    // depends on the shape?

    if fixed_on_actor {
        let (_, actor_shape_pose) =
            systems::geometry::get_shape(&game_state.world, action_data.actor.id()).unwrap();
        let actor_position = Point3::from(actor_shape_pose.translation.vector);

        entities_in_shape.retain(|entity| {
            systems::geometry::line_of_sight_entity_point_filter(
                &game_state.world,
                &game_state.geometry,
                *entity,
                &actor_position,
                &LineOfSightTrajectory::Ray,
                &RaycastFilter::WorldOnly,
            )
            .has_line_of_sight
        });
    } else {
        match shape {
            AreaShape::Sphere { .. } => {
                entities_in_shape.retain(|entity| {
                    systems::geometry::line_of_sight_entity_point_filter(
                        &game_state.world,
                        &game_state.geometry,
                        *entity,
                        point,
                        &LineOfSightTrajectory::Ray,
                        // TODO: Can't hide behind other entities?
                        &RaycastFilter::WorldOnly,
                    )
                    .has_line_of_sight
                });
            }

            _ => {}
        }
    }

    entities_in_shape
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

pub fn targeting_context_data(world: &World, action_data: &ActionData) -> TargetingContext {
    targeting_context(
        world,
        action_data.actor.id(),
        &action_data.action_id,
        &action_data.context,
    )
}

pub fn available_reactions_to_event(
    game_state: &GameState,
    reactor: Entity,
    event: &Event,
    skip_checks: &[ActionUsabilityCheck],
) -> Vec<ActionData> {
    let mut reactions = Vec::new();

    let available = systems::actions::available_actions(game_state, reactor);
    let world = &game_state.world;
    let world_geometry = &game_state.geometry;
    for (reaction_id, contexts_and_costs) in available {
        let reaction = systems::actions::get_action(&reaction_id);
        if reaction.is_none() {
            continue;
        }
        let reaction = reaction.unwrap();

        if let Some(trigger) = &reaction.reaction_trigger {
            if trigger(game_state, &reactor, event) {
                for (context, resource_cost) in &contexts_and_costs {
                    let self_target = matches!(
                        targeting_context(world, reactor, &reaction_id, context).kind,
                        TargetingKind::SelfTarget
                    );
                    let target = if self_target {
                        TargetInstance::entity(EntityIdentifier::from_world(world, reactor))
                    } else {
                        TargetInstance::entity(EntityIdentifier::from_world(
                            world,
                            event.actor().unwrap(),
                        ))
                    };

                    let usability_result = action_usable_on_targets(
                        world,
                        world_geometry,
                        reactor,
                        &reaction_id,
                        context,
                        resource_cost,
                        &[target.clone()],
                        skip_checks,
                    );

                    if usability_result.is_ok() {
                        reactions.push(
                            ActionData::new(
                                EntityIdentifier::from_world(world, reactor),
                                reaction_id.clone(),
                                context.clone(),
                                resource_cost.clone(),
                                vec![target],
                            )
                            .with_trigger_event(event.clone().into()),
                        );
                    }
                }
            }
        }
    }

    reactions
}

// TODO: Something about this seems a bit clunky
fn set_projectiles_paused_for_entity(game_state: &mut GameState, entity: Entity, paused: bool) {
    let pairs: Vec<(Entity, Entity)> = game_state
        .world
        .query::<&Projectile>()
        .iter()
        .map(|(entity, projectile)| (entity, projectile.owner))
        .collect();
    if pairs.is_empty() {
        return;
    }
    debug!("Found projectiles to set paused={}: {:?}", paused, pairs);
    for (proj_entity, actor) in pairs {
        if actor == entity
            && let Ok(mut projectile) = game_state.world.get::<&mut Projectile>(proj_entity)
        {
            projectile.paused = paused;
        }
    }
}

pub fn pause_action(game_state: &mut GameState, entity: Entity, reason: ActivityPauseReason) {
    debug!("Pausing actions for entity {:?}: {:?}", entity, reason);
    systems::helpers::get_component_mut::<ActivityState>(&mut game_state.world, entity)
        .pause(reason);
    set_projectiles_paused_for_entity(game_state, entity, true);
}

pub fn resume_action(game_state: &mut GameState, entity: Entity, reason: ActivityPauseReason) {
    debug!("Resuming actions for entity {:?}: {:?}", entity, reason);
    systems::helpers::get_component_mut::<ActivityState>(&mut game_state.world, entity)
        .resume(reason);
    set_projectiles_paused_for_entity(game_state, entity, false);
}
