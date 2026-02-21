use std::{collections::HashMap, sync::LazyLock};

use glam::Vec2;
use hecs::{Entity, World};
use parry3d::{
    na::{Isometry3, Point3, Translation3, Vector3},
    query::{PointQuery, Ray, RayCast},
    shape::{Capsule, Shape},
};
use polyanya::Coords;
use tracing::trace;
use uom::si::{f32::Length, length::meter};

use crate::{
    components::species::CreatureSize,
    engine::geometry::{WorldGeometry, WorldPath},
};

pub static EPSILON: f32 = 1e-4;

pub type CreaturePose = Isometry3<f32>;

pub static CREATURE_HEIGHTS: LazyLock<HashMap<CreatureSize, f32>> = LazyLock::new(|| {
    HashMap::from([
        (CreatureSize::Tiny, 0.5),
        (CreatureSize::Small, 1.0),
        (CreatureSize::Medium, 1.8),
        (CreatureSize::Large, 2.5),
        (CreatureSize::Huge, 4.0),
        (CreatureSize::Gargantuan, 6.0),
    ])
});

pub fn get_height(world: &World, entity: Entity) -> Option<f32> {
    if let Ok(size) = world.get::<&CreatureSize>(entity) {
        CREATURE_HEIGHTS.get(&size).copied()
    } else {
        None
    }
}

pub fn get_foot_position(world: &World, entity: Entity) -> Option<Point3<f32>> {
    let pose = world.get::<&CreaturePose>(entity).ok()?;
    Some(Point3::from(pose.translation.vector))
}

pub fn get_eye_height(world: &World, entity: Entity) -> Option<f32> {
    get_height(world, entity).map(|h| h * 0.9)
}

pub fn get_eye_position(world: &World, entity: Entity) -> Option<Point3<f32>> {
    let pose = world.get::<&CreaturePose>(entity).ok()?;
    let eye_height = get_eye_height(world, entity)?;
    Some((pose.translation.vector + Vector3::y() * eye_height).into())
}

pub fn get_eye_position_at_point(
    world: &World,
    entity: Entity,
    point: &Point3<f32>,
) -> Option<Point3<f32>> {
    let eye_height = get_eye_height(world, entity)?;
    Some(Point3::new(point.x, point.y + eye_height, point.z))
}

pub fn get_entity_at_point(world: &World, point: Point3<f32>) -> Option<Entity> {
    for (entity, _) in world.query::<&CreaturePose>().iter() {
        if let Some((shape, shape_pose)) = get_shape(world, entity) {
            if shape.contains_point(&shape_pose, &point) {
                return Some(entity);
            }
        }
    }
    None
}

// TODO: Should this really return a *new* shape each time? I guess that makes it
// work nicely if the creature changes size, e.g. Enlarge/Reduce spells.

/// Get the collision shape for a creature entity and the pose of the shape, i.e
/// the pose is the center of the shape.
pub fn get_shape(world: &World, entity: Entity) -> Option<(Capsule, CreaturePose)> {
    if let Some(height) = get_height(world, entity)
        && let Some(pose) = world.get::<&CreaturePose>(entity).ok()
    {
        // Approximate radius as 1/4 of height
        let radius = height / 4.0;
        // Height is supposed to be the entire capsule height, so the half cylinder
        // height is really just a quarter of the total height.
        let shape = Capsule::new_y(height / 4.0, radius);
        // Creature pose is at the feet, so move shape up by half height
        let shape_pose = *pose * Translation3::new(0.0, height / 2.0, 0.0);
        Some((shape, shape_pose))
    } else {
        None
    }
}

pub fn get_shape_at_point(
    world: &World,
    world_geometry: &WorldGeometry,
    entity: Entity,
    point: &Point3<f32>,
) -> Option<(Capsule, CreaturePose)> {
    if let Some((shape, shape_pose)) = get_shape(world, entity)
        && let Some(foot_pos) = get_foot_position(world, entity)
    {
        let ground_pos = ground_position(world_geometry, point)?;
        let offset = ground_pos - foot_pos;
        let new_shape_pose = shape_pose * Translation3::from(offset);
        Some((shape, new_shape_pose))
    } else {
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RaycastHitKind {
    World,
    Creature(Entity),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RaycastHit {
    pub kind: RaycastHitKind,
    /// Time of impact along the ray (distance from ray origin)
    pub toi: f32,
    /// Point of impact in world space
    pub poi: Point3<f32>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RaycastFilter {
    All,
    WorldOnly,
    CreaturesOnly,
    ExcludeCreatures(Vec<Entity>),
}

#[derive(Debug, Clone)]
pub struct RaycastResult {
    pub ray: Ray,
    pub hits: Vec<RaycastHit>,
    pub closest_index: Option<usize>,
    pub filter: RaycastFilter,
}

impl RaycastResult {
    pub fn closest(&self) -> Option<&RaycastHit> {
        self.closest_index.and_then(|i| self.hits.get(i))
    }

    pub fn world_hit(&self) -> Option<&RaycastHit> {
        self.hits
            .iter()
            .find(|o| matches!(o.kind, RaycastHitKind::World))
    }

    pub fn creature_hit(&self) -> Option<&RaycastHit> {
        self.hits
            .iter()
            .find(|o| matches!(o.kind, RaycastHitKind::Creature(_)))
    }
}

static DEFAULT_MAX_TOI: f32 = 10000.0;

pub fn raycast(
    world: &World,
    world_geometry: &WorldGeometry,
    ray: &Ray,
    filter: &RaycastFilter,
) -> Option<RaycastResult> {
    raycast_with_toi(world, world_geometry, ray, DEFAULT_MAX_TOI, filter)
}

pub fn raycast_with_toi(
    world: &World,
    world_geometry: &WorldGeometry,
    ray: &Ray,
    max_time_of_impact: f32,
    filter: &RaycastFilter,
) -> Option<RaycastResult> {
    let mut outcomes = vec![];

    let add_world_hit = |world_geometry: &WorldGeometry,
                         ray: &Ray,
                         max_time_of_impact: f32,
                         outcomes: &mut Vec<RaycastHit>| {
        if let Some(toi) = world_geometry
            .trimesh
            .cast_local_ray(ray, max_time_of_impact, true)
        {
            outcomes.push(RaycastHit {
                kind: RaycastHitKind::World,
                toi,
                poi: ray.origin + ray.dir * toi,
            });
        }
    };

    let add_entity_hits = |world: &World,
                           ray: &Ray,
                           max_time_of_impact: f32,
                           excluded_creatures: &Vec<Entity>,
                           outcomes: &mut Vec<RaycastHit>| {
        let entity_result = world
            .query::<&CreaturePose>()
            .iter()
            .filter_map(|(entity, _)| {
                if let Some((shape, shape_pose)) = get_shape(world, entity)
                    && !excluded_creatures.contains(&entity)
                {
                    let toi = shape.cast_ray(&shape_pose, ray, max_time_of_impact, true);
                    toi.map(|toi| RaycastHit {
                        kind: RaycastHitKind::Creature(entity),
                        toi,
                        poi: ray.origin + ray.dir * toi,
                    })
                } else {
                    None
                }
            })
            .min_by(|a, b| a.toi.partial_cmp(&b.toi).unwrap());

        if let Some(entity_outcome) = entity_result {
            outcomes.push(entity_outcome);
        }
    };

    match &filter {
        RaycastFilter::All => {
            add_world_hit(world_geometry, ray, max_time_of_impact, &mut outcomes);
            add_entity_hits(world, ray, max_time_of_impact, &vec![], &mut outcomes);
        }
        RaycastFilter::WorldOnly => {
            add_world_hit(world_geometry, ray, max_time_of_impact, &mut outcomes);
        }
        RaycastFilter::CreaturesOnly => {
            add_entity_hits(world, ray, max_time_of_impact, &vec![], &mut outcomes);
        }
        RaycastFilter::ExcludeCreatures(excluded) => {
            add_world_hit(world_geometry, ray, max_time_of_impact, &mut outcomes);
            add_entity_hits(world, ray, max_time_of_impact, excluded, &mut outcomes);
        }
    }

    if outcomes.is_empty() {
        None
    } else {
        let closest_index = outcomes
            .iter()
            .enumerate()
            .min_by(|(_, a), (_, b)| a.toi.partial_cmp(&b.toi).unwrap())
            .map(|(i, _)| i);
        Some(RaycastResult {
            ray: ray.clone(),
            hits: outcomes,
            closest_index,
            filter: filter.clone(),
        })
    }
}

pub fn raycast_entity_point(
    world: &World,
    world_geometry: &WorldGeometry,
    entity: Entity,
    point: Point3<f32>,
    filter: &RaycastFilter,
) -> Option<RaycastResult> {
    let start = get_eye_position(world, entity)?;
    raycast_point_point(world, world_geometry, start, point, filter)
}

pub fn raycast_entity_direction(
    world: &World,
    world_geometry: &WorldGeometry,
    entity: Entity,
    direction: Vector3<f32>,
    filter: &RaycastFilter,
) -> Option<RaycastResult> {
    let start = get_eye_position(world, entity)?;
    raycast_point_direction(world, world_geometry, start, direction, filter)
}

pub fn raycast_point_point(
    world: &World,
    world_geometry: &WorldGeometry,
    start: Point3<f32>,
    end: Point3<f32>,
    filter: &RaycastFilter,
) -> Option<RaycastResult> {
    let dir = Vector3::normalize(&(end - start));
    let ray = Ray::new(start, dir);
    raycast(world, world_geometry, &ray, filter)
}

pub fn raycast_point_direction(
    world: &World,
    world_geometry: &WorldGeometry,
    start: Point3<f32>,
    direction: Vector3<f32>,
    filter: &RaycastFilter,
) -> Option<RaycastResult> {
    let dir = Vector3::normalize(&direction);
    let ray = Ray::new(start, dir);
    raycast(world, world_geometry, &ray, filter)
}

#[derive(Debug, Clone)]
pub struct LineOfSightResult {
    pub has_line_of_sight: bool,
    pub raycast_result: Option<RaycastResult>,
}

pub fn line_of_sight_point_point(
    world: &World,
    world_geometry: &WorldGeometry,
    from: Point3<f32>,
    to: Point3<f32>,
    filter: &RaycastFilter,
) -> LineOfSightResult {
    if let Some(result) = raycast_point_point(world, world_geometry, from, to, filter)
        && let Some(closest) = result.closest()
    {
        let distance = (to - from).magnitude();
        LineOfSightResult {
            has_line_of_sight: closest.toi >= distance - EPSILON,
            raycast_result: Some(result),
        }
    } else {
        // No hits, so line of sight is clear
        LineOfSightResult {
            has_line_of_sight: true,
            raycast_result: None,
        }
    }
}

pub fn line_of_sight_entity_point(
    world: &World,
    world_geometry: &WorldGeometry,
    entity: Entity,
    point: Point3<f32>,
) -> LineOfSightResult {
    line_of_sight_entity_point_filter(
        world,
        world_geometry,
        entity,
        point,
        &RaycastFilter::ExcludeCreatures(vec![entity]),
    )
}

pub fn line_of_sight_entity_point_filter(
    world: &World,
    world_geometry: &WorldGeometry,
    entity: Entity,
    point: Point3<f32>,
    filter: &RaycastFilter,
) -> LineOfSightResult {
    if let Some(eye_pos) = get_eye_position(world, entity) {
        line_of_sight_point_point(world, world_geometry, eye_pos, point, filter)
    } else {
        LineOfSightResult {
            has_line_of_sight: false,
            raycast_result: None,
        }
    }
}

// TODO: How to do this properly? Just because you can't see their eyes doesn't
// mean you can't see them at all.
pub fn line_of_sight_entity_entity(
    world: &World,
    world_geometry: &WorldGeometry,
    from_entity: Entity,
    to_entity: Entity,
) -> LineOfSightResult {
    if from_entity == to_entity {
        return LineOfSightResult {
            has_line_of_sight: true,
            raycast_result: None,
        };
    }

    if let Some(from_eye_pos) = get_eye_position(world, from_entity)
        && let Some(to_eye_pos) = get_eye_position(world, to_entity)
        && let Some(result) = raycast_point_point(
            world,
            world_geometry,
            from_eye_pos,
            to_eye_pos,
            &RaycastFilter::ExcludeCreatures(vec![from_entity]),
        )
        && let Some(closest) = result.closest()
    {
        LineOfSightResult {
            has_line_of_sight: closest.kind == RaycastHitKind::Creature(to_entity),
            raycast_result: Some(result),
        }
    } else {
        LineOfSightResult {
            has_line_of_sight: false,
            raycast_result: None,
        }
    }
}

pub fn ground_position(
    world_geometry: &WorldGeometry,
    position: &Point3<f32>,
) -> Option<Point3<f32>> {
    // Raycast straight down to find the ground
    let down_ray = Ray::new(
        Point3::new(position.x, position.y + 1.0, position.z),
        Vector3::new(0.0, -1.0, 0.0),
    );

    if let Some(toi) = world_geometry.trimesh.cast_local_ray(&down_ray, 10.0, true) {
        let ground_y = down_ray.origin.y + down_ray.dir.y * toi;
        Some(Point3::new(position.x, ground_y, position.z))
    } else {
        None
    }
}

pub fn distance_between_entities(
    world: &World,
    entity_a: Entity,
    entity_b: Entity,
) -> Option<Length> {
    let pos_a = get_foot_position(world, entity_a)?;
    let pos_b = get_foot_position(world, entity_b)?;
    let distance = (pos_b - pos_a).magnitude();
    Some(Length::new::<uom::si::length::meter>(distance))
}

pub fn teleport_to(world: &mut World, entity: Entity, new_position: &Point3<f32>) {
    if let Ok(mut pose) = world.get::<&mut CreaturePose>(entity) {
        pose.translation = new_position.clone().into();
    }
}

pub fn teleport_to_ground(
    world: &mut World,
    world_geometry: &WorldGeometry,
    entity: Entity,
    new_position: &Point3<f32>,
) {
    // TODO: Can't seem to find an easy way to check for intersection between
    // the creature and the world geometry?

    let mut target = new_position.clone();

    let ground_position = ground_position(world_geometry, new_position);
    if let Some(ground_pos) = ground_position {
        target.y = ground_pos.y;
    }

    teleport_to(world, entity, &target);
}

pub fn navmesh_nearest_point(
    world_geometry: &WorldGeometry,
    point: Point3<f32>,
) -> Option<Point3<f32>> {
    let closest_coord = world_geometry
        .polyanya_mesh
        .get_closest_point_at_height(Coords::on_mesh(Vec2::new(point.x, point.z)), point.y)?;

    let coord_3d = closest_coord.position_with_height(&world_geometry.polyanya_mesh);
    Some(Point3::new(coord_3d.x, coord_3d.y, coord_3d.z))
}

pub fn path_point_point(
    world_geometry: &WorldGeometry,
    start: Point3<f32>,
    goal: Point3<f32>,
) -> Option<WorldPath> {
    let start = navmesh_nearest_point(world_geometry, start)?;
    let goal = navmesh_nearest_point(world_geometry, goal)?;

    let mut path = world_geometry.path(start, goal)?;
    let num_points = path.points.len();

    // Snap remaining path points to navmesh
    for point in &mut path.points[1..(num_points - 1)] {
        if let Some(nav_point) = navmesh_nearest_point(world_geometry, *point) {
            *point = nav_point;
        }
    }

    Some(path)
}

pub fn path(
    world: &World,
    world_geometry: &WorldGeometry,
    entity: Entity,
    goal: Point3<f32>,
) -> Option<WorldPath> {
    path_point_point(world_geometry, get_foot_position(world, entity)?, goal)
}

pub fn entities_in_shape(
    world: &World,
    shape: Box<dyn Shape>,
    shape_pose: &Isometry3<f32>,
) -> Vec<Entity> {
    let mut entities = vec![];

    for (entity, _) in world.query::<&CreaturePose>().iter() {
        if let Some((creature_shape, creature_shape_pose)) = get_shape(world, entity) {
            let intersects = parry3d::query::intersection_test(
                shape_pose,
                shape.as_ref(),
                &creature_shape_pose,
                &creature_shape,
            )
            .is_ok_and(|result| result);

            if intersects {
                entities.push(entity);
            }
        }
    }

    entities
}

pub fn line_sphere_intersections(
    line_origin: Point3<f32>,
    line_dir: Vector3<f32>,
    sphere_center: Point3<f32>,
    sphere_radius: f32,
) -> Vec<Point3<f32>> {
    let mut intersections = vec![];

    let a = line_dir.dot(&line_dir);
    let oc = line_origin - sphere_center;
    let b = 2.0 * oc.dot(&line_dir);
    let c = oc.dot(&oc) - sphere_radius * sphere_radius;

    let discriminant = b * b - 4.0 * a * c;

    // A negative discriminant corresponds to no intersection
    if discriminant < 0.0 {
        return intersections;
    }

    if discriminant.abs() < EPSILON {
        // One intersection (tangent)
        let t = -b / (2.0 * a);
        intersections.push(line_origin + line_dir * t);
    } else {
        // Two intersections
        let sqrt_disc = discriminant.sqrt();
        let t1 = (-b - sqrt_disc) / (2.0 * a);
        let t2 = (-b + sqrt_disc) / (2.0 * a);
        intersections.push(line_origin + line_dir * t1);
        intersections.push(line_origin + line_dir * t2);
    }

    intersections
}

pub fn path_intersections_within_radius(
    path: &WorldPath,
    point: Point3<f32>,
    radius: Length,
) -> Vec<Point3<f32>> {
    let mut intersections = vec![];

    for (start, end) in path.points.windows(2).map(|window| (window[0], window[1])) {
        let possible_intersections =
            line_sphere_intersections(start, end - start, point, radius.get::<meter>());
        trace!(
            "Possible intersections between line segment {:?}-{:?} and sphere at {:?} with radius {:?}: {:?}",
            start, end, point, radius, possible_intersections
        );
        for intersection in possible_intersections {
            // Check if the intersection is actually within the line segment
            let to_intersection = intersection - start;
            let segment = end - start;
            if to_intersection.dot(&segment) >= 0.0
                && to_intersection.dot(&to_intersection) <= segment.dot(&segment)
            {
                intersections.push(intersection);
            }
        }
    }

    intersections
}
