use hecs::{Entity, World};
use parry3d::{
    math::Isometry,
    na::Point3,
    query::{Ray, RayCast},
    shape::Ball,
};
use tracing::trace;
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{
        actions::targeting::{LineOfSightMode, TargetInstance, TargetingError},
        speed::Speed,
    },
    engine::{
        action_prompt::{ActionData, ActionError},
        game_state::GameState,
        geometry::WorldPath,
    },
    systems::{
        self,
        actions::ActionUsabilityError,
        geometry::{LineOfSightResult, RaycastFilter},
    },
};

#[derive(Debug)]
pub enum MovementError {
    InsufficientSpeed,
    NoPathFound,
    // Strictly speaking this isn't a movement error, but it makes it easier to
    // handle in the game state if we put it here ;)
    NotYourTurn,
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
    game_state: &mut GameState,
    entity: Entity,
    goal: &Point3<f32>,
    allow_partial: bool,
    move_entity: bool,
    trim_to_movement: bool,
    spend_movement: bool,
) -> Result<PathResult, MovementError> {
    let full_path = systems::geometry::path(&game_state.world, &game_state.geometry, entity, *goal)
        .ok_or(MovementError::NoPathFound)?;

    let remaining_movement =
        systems::helpers::get_component_mut::<Speed>(&mut game_state.world, entity)
            .remaining_movement()
            .clone();

    let taken_path = if full_path.length > remaining_movement && trim_to_movement {
        if !allow_partial {
            return Err(MovementError::InsufficientSpeed);
        }
        full_path.trim_to_length(remaining_movement)
    } else {
        full_path.clone()
    };

    if move_entity {
        // TODO: Actually make them move along the path rather than teleporting to the end
        systems::geometry::teleport_to_ground(
            &mut game_state.world,
            &game_state.geometry,
            entity,
            taken_path.end().unwrap(),
        );
        if spend_movement {
            systems::helpers::get_component_mut::<Speed>(&mut game_state.world, entity)
                .record_movement(taken_path.length);
        }
    }

    Ok(PathResult {
        full_path,
        taken_path,
    })
}

#[derive(Debug, Clone)]
pub struct PathInRangeOfPointResult {
    pub path_result: PathResult,
    pub line_of_sight_result: LineOfSightResult,
}

pub fn path_in_range_of_point(
    game_state: &mut GameState,
    entity: Entity,
    target: Point3<f32>,
    range: Length,
    allow_partial: bool,
    move_entity: bool,
    line_of_sight: LineOfSightMode,
    trim_to_movement: bool,
    spend_movement: bool,
) -> Result<PathInRangeOfPointResult, MovementError> {
    trace!(
        "Attempting to path entity {:?} to within {:?} of point {:?}",
        entity, range, target
    );

    let direction = (target
        - systems::geometry::get_shape(&game_state.world, entity)
            .unwrap()
            .1
            .translation
            .vector)
        .to_homogeneous();

    let distance_to_target = Length::new::<meter>(direction.magnitude());

    let line_of_sight_result = systems::geometry::line_of_sight_entity_point(
        &game_state.world,
        &game_state.geometry,
        entity,
        target,
        &line_of_sight,
    );

    if distance_to_target <= range && line_of_sight_result.has_line_of_sight {
        // Already in range
        trace!("Entity is already in range of target point.");
        return Ok(PathInRangeOfPointResult {
            path_result: PathResult::empty(),
            line_of_sight_result,
        });
    }

    trace!("Distance to target: {:?}", distance_to_target);

    let path_to_target = path(
        game_state,
        entity,
        &target,
        true,
        false,
        trim_to_movement,
        spend_movement,
    )?;

    if let Some(result) = determine_path_sphere_intersections(
        game_state,
        entity,
        &line_of_sight,
        range,
        &path_to_target.full_path,
        &target,
    ) {
        return Ok(PathInRangeOfPointResult {
            path_result: path(
                game_state,
                entity,
                &result.intersection_point,
                allow_partial,
                move_entity,
                trim_to_movement,
                spend_movement,
            )?,
            line_of_sight_result: result.line_of_sight_result,
        });
    }

    if allow_partial {
        // Return the partial path even if we couldn't get in range
        trace!("No intersection found, but allowing partial path.");
        return Ok(PathInRangeOfPointResult {
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
    line_of_sight: &LineOfSightMode,
    range: Length,
    path_to_target: &WorldPath,
    target: &Point3<f32>,
) -> Option<PathSphereIntersectionResult> {
    // Entity shouldn't block its own line of sight
    let mut excluded_entities = vec![entity];
    // If an entity is standing on the end of the path, that's probably who we're
    // trying to target, so don't let them block line of sight either
    if let Some(occupant) = systems::geometry::get_entity_at_point(&game_state.world, *target) {
        trace!(
            "Excluding occupant {:?} at path end from LOS checks",
            occupant
        );
        excluded_entities.push(occupant);
    }
    let raycast_filter = RaycastFilter::ExcludeCreatures(excluded_entities);

    let sphere = Ball::new(range.get::<meter>());

    for (start, end) in path_to_target
        .points
        .windows(2)
        .map(|window| (window[0], window[1]))
    {
        let ray = Ray::new(start, (end - start).normalize());

        if let Some(toi) = sphere.cast_ray(
            &Isometry::translation(target.x, target.y, target.z),
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
            let line_of_sight_result = systems::geometry::line_of_sight_point_point(
                &game_state.world,
                &game_state.geometry,
                eye_pos_at_intersection,
                *target,
                line_of_sight,
                &raycast_filter,
            );
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
    PathFound(PathInRangeOfPointResult),
}

#[derive(Debug, Clone)]
pub enum TargetPathFindingError {
    NoPathFound,
    ActionError(ActionError),
}

pub fn path_to_target(
    game_state: &mut GameState,
    action: &ActionData,
    pathfind_if_out_of_range: bool,
) -> Result<TargetPathFindingResult, TargetPathFindingError> {
    let validation_result = game_state.validate_action(action, false);

    if let Err(action_error) = &validation_result {
        // Check if it's an error that can be resolved with pathfinding
        if pathfind_if_out_of_range
            && let ActionError::Usability(usability_error) = action_error
            && let ActionUsabilityError::TargetingError(targeting_error) = usability_error
            && let TargetingError::OutOfRange { target, .. }
            | TargetingError::NoLineOfSight { target } = targeting_error
        {
            let target_position = match target {
                TargetInstance::Entity(entity) => {
                    let (_, shape_pose) =
                        systems::geometry::get_shape(&game_state.world, entity.id()).unwrap();
                    shape_pose.translation.vector.into()
                }
                TargetInstance::Point(point) => *point,
            };
            let targeting_context = systems::actions::targeting_context(
                &game_state.world,
                action.actor.id(),
                &action.action_id,
                &action.context,
            );

            if let Ok(path_result) = systems::movement::path_in_range_of_point(
                game_state,
                action.actor.id(),
                target_position,
                targeting_context.range.max(),
                true,
                false,
                targeting_context.line_of_sight,
                true,
                true,
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

pub fn potential_opportunity_attacks(
    world: &World,
    path: &WorldPath,
    mover: Entity,
    attackers: &[Entity],
) -> Vec<(Entity, Point3<f32>)> {
    let free_movement_distance =
        systems::helpers::get_component::<Speed>(world, mover).free_movement_remaining();

    attackers
        .iter()
        .filter_map(|attacker| {
            if let Some(intersection_point) =
                opportunity_attack_point(world, path, mover, *attacker, &free_movement_distance)
            {
                Some((*attacker, intersection_point))
            } else {
                None
            }
        })
        .collect()
}

fn opportunity_attack_point(
    world: &World,
    path: &WorldPath,
    mover: Entity,
    attacker: Entity,
    free_movement_distance: &Length,
) -> Option<Point3<f32>> {
    if mover == attacker {
        return None;
    }

    let attacker_loadout = systems::loadout::loadout(world, attacker);
    let attacker_reach = attacker_loadout.melee_range();
    let attacker_position = systems::geometry::get_foot_position(world, attacker)?;

    let path_intersections = systems::geometry::path_intersections_within_radius(
        path,
        attacker_position,
        attacker_reach.max(),
    );

    // Scenario 1: No intersections
    // Entity either doesn't come within reach or doesn't leave reach, so no opportunity attack.
    if path_intersections.is_empty() {
        return None;
    }

    // Scenario 2: One intersection
    // 2a: Entity starts outside of reach and enters reach: no opportunity attack
    // 2b: Entity starts inside of reach and leaves reach: opportunity attack
    if path_intersections.len() == 1 {
        let Some(distance_to_mover_start_position) =
            systems::geometry::distance_between_entities(world, mover, attacker)
        else {
            return None;
        };

        if distance_to_mover_start_position > attacker_reach.max() {
            // Scenario 2a
            return None;
        } else {
            // Scenario 2b
            if let Some(distance_along_path) = path.distance_along_path(&path_intersections[0])
                && distance_along_path > *free_movement_distance
            {
                return Some(path_intersections[0]);
            };

            return None;
        }
    }

    // Scenario 3: More than one intersection
    // Entity enters and leaves reach at least once, so opportunity attack.
    let mut distances_along_path: Vec<(Option<Length>, Point3<f32>)> = path_intersections
        .iter()
        .map(|intersection| (path.distance_along_path(intersection), *intersection))
        .collect();
    distances_along_path.sort_by(|(distance_a, _), (distance_b, _)| {
        distance_a
            .partial_cmp(distance_b)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    distances_along_path.reverse();

    for (distance_along_path, intersection) in distances_along_path {
        if let Some(distance) = distance_along_path {
            if distance > *free_movement_distance {
                return Some(intersection);
            }
        }
    }

    None
}
