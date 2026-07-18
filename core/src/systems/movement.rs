use hecs::{Entity, World};
use parry3d::{
    math::Isometry,
    na::Point3,
    query::{PointQuery, Ray, RayCast},
    shape::Ball,
};
use tracing::{debug, error, trace};
use uom::si::{
    f32::Length,
    length::{foot, meter},
};

use crate::{
    components::{
        actions::{
            action::{ActionConditionResolution, ActionResultComponent, DamageResult},
            targeting::{
                LineOfSight, LineOfSightTrajectory, TargetInstance, TargetingCheck, TargetingError,
                TargetingKind,
            },
        },
        activity::ActivityState,
        damage::{DamageRoll, DamageSource, DamageType},
        dice::{DiceSet, DieSize},
        id::EntityIdentifier,
        modifier::{ModifierKind, ModifierMap, ModifierSource},
        speed::Speed,
    },
    engine::{
        action_prompt::{ActionData, ActionError},
        event::{Event, EventKind},
        game_state::GameState,
        geometry::WorldPath,
    },
    systems::{
        self,
        actions::{ActionUsabilityCheck, ActionUsabilityError},
        geometry::{EPSILON, LineOfSightResult, RaycastFilter},
    },
};

pub const FALL_DAMAGE_DIE: DieSize = DieSize::D6;
pub const FALL_DAMAGE_MAX_DICE: u32 = 20;
pub const FALL_DAMAGE_THRESHOLD_FT: f32 = 10.0;

#[derive(Debug, Clone)]
pub enum MovementError {
    InsufficientSpeed,
    NoPathFound,
    // Strictly speaking this isn't a movement error, but it makes it easier to
    // handle in the game state if we put it here ;)
    NotYourTurn,
    NotAlive,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathResult {
    pub full_path: WorldPath,
    pub taken_path: WorldPath,
}

impl PathResult {
    pub fn empty() -> Self {
        Self {
            full_path: WorldPath::new(vec![]),
            taken_path: WorldPath::new(vec![]),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.full_path.points.is_empty() && self.taken_path.points.is_empty()
    }

    pub fn reaches_goal(&self) -> bool {
        !self.is_empty() && self.full_path.points.last() == self.taken_path.points.last()
    }
}

pub fn path(
    game_state: &GameState,
    entity: Entity,
    goal: &Point3<f32>,
    allow_partial: bool,
    trim_to_movement: bool,
) -> Result<PathResult, MovementError> {
    let full_path = systems::geometry::path(&game_state.world, &game_state.geometry, entity, *goal)
        .ok_or(MovementError::NoPathFound)?;

    let remaining_movement =
        systems::helpers::get_component::<Speed>(&game_state.world, entity).remaining_movement();

    let taken_path = if full_path.length > remaining_movement && trim_to_movement {
        if !allow_partial {
            return Err(MovementError::InsufficientSpeed);
        }
        full_path.trim_to_length(remaining_movement)
    } else {
        full_path.clone()
    };

    Ok(PathResult {
        full_path,
        taken_path,
    })
}

#[derive(Debug, Clone)]
pub struct PathInRangeOfTargetResult {
    pub path_result: PathResult,
    pub line_of_sight_result: LineOfSightResult,
}

pub fn path_in_range_of_target(
    game_state: &mut GameState,
    entity: Entity,
    target: &TargetInstance,
    range: Length,
    allow_partial: bool,
    line_of_sight: LineOfSight,
    trim_to_movement: bool,
) -> Result<PathInRangeOfTargetResult, MovementError> {
    trace!(
        "Attempting to path entity {:?} to within {:?} of target {:?}",
        entity, range, target
    );

    let target_point = target
        .position(&game_state.world)
        .expect(format!("Failed to get position of {target:?}").as_str());

    let (shape, pose) = systems::geometry::get_shape(&game_state.world, entity)
        .expect("Failed to get shape of entity for path_in_range_of_target");

    let distance_to_target =
        Length::new::<meter>(shape.distance_to_point(&pose, &target_point, true));

    let line_of_sight_result = systems::geometry::line_of_sight_entity_target(
        &game_state.world,
        &game_state.geometry,
        entity,
        target,
        &line_of_sight,
    );

    if distance_to_target <= range && line_of_sight_result.has_line_of_sight {
        // Already in range
        trace!("Entity is already in range of target point.");
        return Ok(PathInRangeOfTargetResult {
            path_result: PathResult::empty(),
            line_of_sight_result,
        });
    }

    trace!("Distance to target: {:?}", distance_to_target);

    let path_to_target = path(game_state, entity, &target_point, true, trim_to_movement)?;

    if let Some(result) = determine_path_sphere_intersections(
        game_state,
        entity,
        &line_of_sight.trajectory,
        range,
        &path_to_target.full_path,
        &target,
    ) {
        return Ok(PathInRangeOfTargetResult {
            path_result: path(
                game_state,
                entity,
                &result.intersection_point,
                allow_partial,
                trim_to_movement,
            )?,
            line_of_sight_result: result.line_of_sight_result,
        });
    }

    if allow_partial {
        // Return the partial path even if we couldn't get in range
        trace!("No intersection found, but allowing partial path.");
        return Ok(PathInRangeOfTargetResult {
            path_result: path_to_target,
            // TODO: Not sure what to do about line of sight in this case
            line_of_sight_result,
        });
    }

    Err(MovementError::NoPathFound)
}

pub struct PathSphereIntersectionResult {
    pub intersection_point: Point3<f32>,
    pub line_of_sight_result: LineOfSightResult,
}

fn determine_path_sphere_intersections(
    game_state: &mut GameState,
    entity: Entity,
    trajectory: &LineOfSightTrajectory,
    range: Length,
    path_to_target: &WorldPath,
    target: &TargetInstance,
) -> Option<PathSphereIntersectionResult> {
    // Entity shouldn't block its own line of sight
    let raycast_filter = RaycastFilter::ExcludeCreatures(vec![entity]);
    let target_point = target.position(&game_state.world)?;
    let sphere = Ball::new(range.get::<meter>());

    for (start, end) in path_to_target
        .points
        .windows(2)
        .map(|window| (window[0], window[1]))
    {
        let ray = Ray::new(start, (end - start).normalize());

        if let Some(toi) = sphere.cast_ray(
            &Isometry::translation(target_point.x, target_point.y, target_point.z),
            &ray,
            f32::MAX,
            true,
        ) {
            let intersection_point = ray.point_at(toi);
            let ground_at_intersection =
                systems::geometry::ground_position(&game_state.geometry, &intersection_point)?;
            let eye_pos_at_intersection = systems::geometry::get_eye_position_at_point(
                &game_state.world,
                entity,
                &ground_at_intersection,
            )?;
            let mut line_of_sight_result = systems::geometry::line_of_sight_point_point(
                &game_state.world,
                &game_state.geometry,
                &eye_pos_at_intersection,
                &target_point,
                trajectory,
                &raycast_filter,
            );
            if let TargetInstance::Entity {
                entity: target_entity,
                ..
            } = target
            {
                line_of_sight_result.check_entity_hit(target_entity.id());
            }

            if !line_of_sight_result.has_line_of_sight {
                // No line of sight to this intersection point; try next segment
                continue;
            } else {
                return Some(PathSphereIntersectionResult {
                    intersection_point,
                    line_of_sight_result,
                });
            }
        }
    }

    None
}

pub fn recharge_movement(world: &mut World, entity: Entity) {
    systems::helpers::get_component_mut::<Speed>(world, entity).reset();
}

#[derive(Debug, Clone)]
pub enum TargetPathFindingResult {
    AlreadyInRange(LineOfSightResult),
    PathFound(PathInRangeOfTargetResult),
}

#[derive(Debug, Clone)]
pub enum TargetPathFindingError {
    NoPathFound,
    MultiplePotentialTargets,
    ShapeFixedToActor,
    ActionError(ActionError),
}

pub fn path_to_target(
    game_state: &mut GameState,
    action: &ActionData,
) -> Result<TargetPathFindingResult, TargetPathFindingError> {
    let validation_result = game_state.validate_action(action, true);

    if let Err(action_error) = &validation_result {
        // Check if it's an error that can be resolved with pathfinding
        if let ActionError::Usability(usability_error) = action_error
            && let ActionUsabilityError::TargetingError(targeting_error) = usability_error
            && let TargetingError::OutOfRange { target, .. }
            | TargetingError::NoLineOfSight { target } = targeting_error
        {
            let targeting_context =
                systems::actions::targeting_context_data(&game_state.world, action);

            if matches!(targeting_context.kind, TargetingKind::Multiple { .. }) {
                // For simplicity, don't attempt to find a path if there are multiple
                // potential targets since we don't know which one to path to
                return Err(TargetPathFindingError::MultiplePotentialTargets);
            }

            let targeting_context = systems::actions::targeting_context(
                &game_state.world,
                action.actor.id(),
                &action.action_id,
                &action.context,
            );

            let in_combat = game_state.in_combat.contains_key(&action.actor.id());
            if let Ok(path_result) = systems::movement::path_in_range_of_target(
                game_state,
                action.actor.id(),
                target,
                targeting_context.range.max(),
                true,
                targeting_context.line_of_sight,
                in_combat,
            ) {
                return Ok(TargetPathFindingResult::PathFound(path_result));
            } else {
                return Err(TargetPathFindingError::NoPathFound);
            }
        } else {
            return Err(TargetPathFindingError::ActionError(action_error.clone()));
        }
    }

    // TODO: Not sure if this is the best solution
    let line_of_sight_result = systems::geometry::line_of_sight_entity_first_target(
        &game_state.world,
        &game_state.geometry,
        action,
    );
    Ok(TargetPathFindingResult::AlreadyInRange(
        line_of_sight_result,
    ))
}

pub enum MoveMode {
    Voluntary,
    Displace,
}

pub fn move_entity(
    game_state: &mut GameState,
    entity: Entity,
    new_position: &Point3<f32>,
    mode: MoveMode,
) {
    match mode {
        MoveMode::Voluntary => {
            // Voluntary movement triggers opportunity attacks
            let Some(from_position) =
                systems::geometry::get_foot_position(&game_state.world, entity)
            else {
                error!(
                    "Failed to get position of entity {:?} for voluntary movement",
                    entity
                );
                return;
            };

            let mut potential_attackers = game_state.get_potential_reactors(entity);

            potential_attackers.sort_by(|a, b| {
                let distance_a =
                    systems::geometry::distance_between_entities(&game_state.world, entity, *a)
                        .unwrap_or(Length::new::<meter>(f32::MAX));
                let distance_b =
                    systems::geometry::distance_between_entities(&game_state.world, entity, *b)
                        .unwrap_or(Length::new::<meter>(f32::MAX));
                distance_a
                    .partial_cmp(&distance_b)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });

            for attacker in potential_attackers {
                let Some((event, intersection)) = calculate_opportunity_attack(
                    game_state,
                    entity,
                    &from_position,
                    new_position,
                    attacker,
                ) else {
                    continue;
                };

                move_new_position(game_state, entity, from_position, &intersection);

                systems::helpers::get_component_mut::<ActivityState>(&mut game_state.world, entity)
                    .set_idle();

                game_state.process_event(event);
                return;
            }

            // No opportunity attacks, so just move the entity
            move_new_position(game_state, entity, from_position, new_position);
        }

        MoveMode::Displace => {
            systems::geometry::teleport_to(&mut game_state.world, entity, new_position)
        }
    }
}

fn move_new_position(
    game_state: &mut GameState,
    entity: Entity,
    from_position: Point3<f32>,
    new_position: &Point3<f32>,
) {
    systems::geometry::teleport_to(&mut game_state.world, entity, new_position);

    if game_state.in_combat.contains_key(&entity) {
        let distance_moved = Length::new::<meter>((new_position - from_position).norm());
        systems::helpers::get_component_mut::<Speed>(&mut game_state.world, entity)
            .record_movement(distance_moved);
    }
}

pub fn calculate_opportunity_attack(
    game_state: &GameState,
    entity: Entity,
    from_position: &Point3<f32>,
    new_position: &Point3<f32>,
    attacker: Entity,
) -> Option<(Event, Point3<f32>)> {
    if attacker == entity {
        // An entity can't attack itself
        return None;
    }

    let free_movement = systems::helpers::get_component::<Speed>(&game_state.world, entity)
        .free_movement_remaining();
    if (from_position - new_position).norm() <= free_movement.get::<meter>() {
        trace!(
            "Entity {:?} has enough free movement to move from {:?} to {:?} without provoking opportunity attacks.",
            entity, from_position, new_position
        );
        return None;
    }

    let (event, intersection) =
        get_opportunity_attack_point(game_state, entity, attacker, from_position, new_position)?;

    let reactions = systems::actions::available_reactions_to_event(
        game_state,
        attacker,
        &event,
        &[ActionUsabilityCheck::Targeting(vec![
            TargetingCheck::Range,
            TargetingCheck::LineOfSight,
        ])],
    );

    if reactions.is_empty() {
        // No reactions so no opportunity attack
        trace!(
            "Potential attacker {:?} has no reactions available to event {:?}, so no opportunity attack.",
            attacker, event
        );
        return None;
    }

    Some((event, intersection))
}

fn get_opportunity_attack_point(
    game_state: &GameState,
    entity: Entity,
    attacker: Entity,
    from_position: &Point3<f32>,
    new_position: &Point3<f32>,
) -> Option<(Event, Point3<f32>)> {
    let Some(attacker_position) = systems::geometry::get_foot_position(&game_state.world, attacker)
    else {
        error!(
            "Failed to get position of potential attacker {:?} for opportunity attack",
            attacker
        );
        return None;
    };

    let attacker_reach = {
        let Some((attacker_shape, _)) = systems::geometry::get_shape(&game_state.world, attacker)
        else {
            error!(
                "Failed to get shape of potential attacker {:?} for opportunity attack",
                attacker
            );
            return None;
        };
        let attacker_radius = Length::new::<meter>(attacker_shape.radius);
        let attacker_loadout = systems::loadout::loadout(&game_state.world, attacker);
        let attacker_reach = attacker_loadout.melee_range().max() + attacker_radius;
        attacker_reach.get::<meter>()
    };
    let attacker_reach_squared = attacker_reach.powi(2);

    let mut intersections = systems::geometry::line_sphere_intersections(
        from_position,
        new_position,
        &attacker_position,
        attacker_reach,
    );

    // Scenario 1: No intersections
    // Entity either doesn't come within reach or doesn't leave reach, so no opportunity attack.
    if intersections.is_empty() {
        return None;
    }

    let event = Event::new(EventKind::MovingOutOfReach {
        mover: EntityIdentifier::from_world(&game_state.world, entity),
        entity: EntityIdentifier::from_world(&game_state.world, attacker),
    });

    // Scenario 2: One intersection
    // 2a: Entity starts outside of reach and enters reach: no opportunity attack
    // 2b: Entity starts inside of reach and remains inside reach: no opportunity attack
    // 2c: Entity starts inside of reach and leaves reach: opportunity attack
    if intersections.len() == 1 {
        // Scenario 2a
        if (from_position - attacker_position).norm_squared() > attacker_reach_squared - EPSILON {
            trace!(
                "Entity {:?} starts outside of reach of potential attacker {:?} and enters reach, so no opportunity attack.",
                entity, attacker
            );
            return None;
        }

        // Scenario 2b
        if (new_position - attacker_position).norm_squared() <= attacker_reach_squared + EPSILON {
            trace!(
                "Entity {:?} starts within reach of potential attacker {:?} and stays within reach, so no opportunity attack.",
                entity, attacker
            );
            return None;
        }

        // Scenario 2c
        trace!(
            "Entity {:?} starts within reach of potential attacker {:?} and leaves reach, so potential opportunity attack.",
            entity, attacker
        );
        return Some((event, intersections[0]));
    }

    // Scenario 3: More than one intersection
    // Entity enters and leaves reach at least once, so opportunity attack.
    trace!(
        "Entity {:?} enters and leaves reach of potential attacker {:?} multiple times, so potential opportunity attack.",
        entity, attacker
    );

    intersections.sort_by(|a, b| {
        // Negative distance so we get the furthest intersection first
        let distance_a = -(from_position - *a).norm_squared();
        let distance_b = -(from_position - *b).norm_squared();
        distance_a
            .partial_cmp(&distance_b)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    Some((event, intersections[0]))
}

// TODO: Not sure where this should live
pub fn apply_fall_damage(game_state: &mut GameState, entity: Entity, fall_distance: Length) {
    let fall_distance_ft = fall_distance.get::<foot>();
    let damage_dice =
        FALL_DAMAGE_MAX_DICE.min((fall_distance_ft / FALL_DAMAGE_THRESHOLD_FT).floor() as u32);
    if damage_dice < 1 {
        debug!(
            "Entity {:?} fell {:?} ({} ft), which is below the threshold for fall damage, so no damage applied.",
            entity, fall_distance, fall_distance_ft
        );
        return;
    }

    let mut damage_roll = DamageRoll::new(
        ModifierMap::from(
            ModifierSource::Base,
            ModifierKind::Dice(DiceSet::new(damage_dice, FALL_DAMAGE_DIE)),
        ),
        DamageType::Bludgeoning,
        DamageSource::Environmental,
    )
    .roll(false);

    let (damage_taken, new_life_state) =
        systems::health::damage(game_state, entity, &mut damage_roll);

    // TODO: Some kind of damage source so we can see it was from fall damage in the logs?

    game_state.process_event(Event::action_result_event(
        EntityIdentifier::from_world(&game_state.world, entity),
        ActionResultComponent::Damage(DamageResult {
            resolution: ActionConditionResolution::Unconditional,
            damage_roll: Some(damage_roll),
            damage_taken,
            new_life_state,
        }),
    ));
}
