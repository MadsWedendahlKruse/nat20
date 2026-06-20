use std::{collections::HashSet, fmt, str::FromStr};

use hecs::{Entity, World};
use parry3d::{
    na::{Isometry3, Point3, Translation3, UnitQuaternion, Vector3},
    shape::{Ball, Capsule, Cone, Cuboid, Cylinder, Shape},
};
use serde::{Deserialize, Serialize};
use tracing::{error, trace};
use uom::{
    Conversion,
    si::{
        angle::radian,
        f32::{Angle, Length, Velocity},
        length::{Unit, meter},
    },
};

use crate::{
    components::{
        faction::Attitude, health::life_state::LifeState, id::EntityIdentifier,
        items::equipment::weapon::MELEE_RANGE_REACH, species::CreatureType,
    },
    engine::geometry::WorldGeometry,
    entities::{character::CharacterTag, monster::MonsterTag},
    systems::{self, geometry::EPSILON},
};

#[derive(Debug, Clone)]
pub struct TargetingContext {
    pub kind: TargetingKind,
    pub range: TargetingRange,
    pub line_of_sight: LineOfSightMode,
    pub allowed_entities: Vec<EntityFilter>,
}

impl TargetingContext {
    pub fn new(
        kind: TargetingKind,
        range: TargetingRange,
        line_of_sight: LineOfSightMode,
        allowed_entities: Vec<EntityFilter>,
    ) -> Self {
        TargetingContext {
            kind,
            range,
            line_of_sight,
            allowed_entities,
        }
    }

    pub fn self_target() -> Self {
        TargetingContext {
            kind: TargetingKind::SelfTarget,
            range: TargetingRange::new::<meter>(0.0),
            line_of_sight: LineOfSightMode::Ignore,
            allowed_entities: vec![EntityFilter::All],
        }
    }

    pub fn allowed_target(
        &self,
        world: &World,
        entity: Entity,
        actor: Option<Entity>,
    ) -> Result<(), Vec<EntityFilter>> {
        let violated_filters: Vec<EntityFilter> = self
            .allowed_entities
            .iter()
            .filter(|filter| !filter.matches(world, entity, actor))
            .cloned()
            .collect();

        if violated_filters.is_empty() {
            Ok(())
        } else {
            Err(violated_filters)
        }
    }

    pub fn validate_targets(
        &self,
        world: &World,
        world_geometry: &WorldGeometry,
        actor: Entity,
        targets: &[TargetInstance],
    ) -> Result<(), TargetingError> {
        if targets.is_empty() {
            return Err(TargetingError::NoTargetsProvided);
        }

        for target in targets {
            // Check allowed targets
            match target {
                TargetInstance::Entity { entity, .. } => {
                    if let Err(violated_filters) =
                        self.allowed_target(world, entity.id(), Some(actor))
                    {
                        return Err(TargetingError::InvalidTarget {
                            target: target.clone(),
                            violated_filters: violated_filters
                                .into_iter()
                                .map(Into::into)
                                .collect(),
                        });
                    }
                }

                TargetInstance::Point(point) => {
                    if let TargetingKind::Area {
                        shape,
                        fixed_on_actor,
                        filters,
                    } = &self.kind
                        && !filters.is_empty()
                    {
                        let shape_transform =
                            shape.parry3d_shape(world, actor, *fixed_on_actor, point);
                        let violated_filters: Vec<TargetFilter> = filters
                            .iter()
                            .filter(|filter| {
                                !filter.matches(world, world_geometry, &shape_transform)
                            })
                            .cloned()
                            .map(Into::into)
                            .collect();

                        if !violated_filters.is_empty() {
                            return Err(TargetingError::InvalidTarget {
                                target: target.clone(),
                                violated_filters,
                            });
                        }
                    }
                }
            }

            if self.kind.check_range() {
                let actor_position = systems::geometry::get_foot_position(world, actor).unwrap();

                let distance = match target {
                    TargetInstance::Entity { entity, .. } => {
                        systems::geometry::distance_between_entities(world, actor, entity.id())
                            .unwrap()
                    }

                    TargetInstance::Point(point) => {
                        Length::new::<meter>((point - actor_position).norm())
                    }
                };

                if !self.range.in_range(distance) {
                    return Err(TargetingError::OutOfRange {
                        target: target.clone(),
                        distance,
                        max_range: self.range.max(),
                    });
                }
            }

            if self.kind.check_line_of_sight() {
                let line_of_sight_result = systems::geometry::line_of_sight_entity_target(
                    world,
                    world_geometry,
                    actor,
                    target,
                    &self.line_of_sight,
                );

                if !line_of_sight_result.has_line_of_sight {
                    return Err(TargetingError::NoLineOfSight {
                        target: target.clone(),
                    });
                }
            }
        }

        match self.kind {
            TargetingKind::SelfTarget => {
                if targets.len() > 1 {
                    return Err(TargetingError::ExceedsMaxTargets);
                }
                let actor = EntityIdentifier::from_world(world, actor);
                let TargetInstance::Entity { entity, .. } = &targets[0] else {
                    return Err(TargetingError::NotSelf {
                        target: targets[0].clone(),
                        actor,
                    });
                };
                if *entity != actor {
                    return Err(TargetingError::NotSelf {
                        target: targets[0].clone(),
                        actor,
                    });
                }
            }

            TargetingKind::Single => {
                if targets.len() > 1 {
                    return Err(TargetingError::ExceedsMaxTargets);
                }
            }

            TargetingKind::Multiple {
                max_targets,
                allow_duplicates,
            } => {
                if targets.len() > max_targets {
                    return Err(TargetingError::ExceedsMaxTargets);
                }
                if !allow_duplicates {
                    let mut seen = Vec::new();
                    for target in targets {
                        if seen.contains(target) {
                            return Err(TargetingError::DuplicateTargetNotAllowed {
                                target: target.clone(),
                            });
                        }
                        seen.push(target.clone());
                    }
                }
            }

            TargetingKind::Area { .. } => {
                // No max target validation for area targeting - the shape will determine the valid targets
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LineOfSightMode {
    Ignore,
    Ray,
    Parabola { launch_velocity: Velocity },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TargetingKind {
    SelfTarget, // e.g. Second Wind
    Single,
    Multiple {
        max_targets: usize,
        allow_duplicates: bool,
    },
    Area {
        shape: AreaShape,
        fixed_on_actor: bool,
        filters: Vec<AreaFilter>,
    },
}

impl TargetingKind {
    pub fn check_line_of_sight(&self) -> bool {
        match self {
            TargetingKind::SelfTarget => false,
            TargetingKind::Area { fixed_on_actor, .. } => !*fixed_on_actor,
            _ => true,
        }
    }

    pub fn check_range(&self) -> bool {
        match self {
            TargetingKind::SelfTarget => false,
            TargetingKind::Area { fixed_on_actor, .. } => !*fixed_on_actor,
            _ => true,
        }
    }
}

// TODO: parry3d shapes?
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AreaShape {
    // e.g. Cone of Cold
    Cone { angle: Angle, length: Length },
    // e.g. Fireball
    Sphere { radius: Length },
    // e.g. Wall of Force
    Cube { side_length: Length },
    // e.g. Cloudkill
    Cylinder { radius: Length, height: Length },
    // e.g. Lightning Bolt
    Line { length: Length, width: Length },
    // Mainly used for unoccupied area targeting
    Capsule { half_height: Length, radius: Length },
}

impl AreaShape {
    pub fn parry3d_shape(
        &self,
        world: &World,
        actor: Entity,
        fixed_on_actor: bool,
        target_point: &Point3<f32>,
    ) -> ShapeTransform {
        let (_, actor_shape_pose) = systems::geometry::get_shape(world, actor).unwrap();
        let actor_position = actor_shape_pose.translation.vector;

        let mut translation = if fixed_on_actor {
            actor_position
        } else {
            target_point.coords
        };

        match self {
            AreaShape::Cone { angle, length } => {
                let length = length.get::<meter>();
                let half_height = length / 2.0;
                let radius = length * (angle.get::<radian>() / 2.0).tan();
                let shape = Cone::new(half_height, radius);

                let mut transform = Isometry3::new(translation, Vector3::zeros());

                // Rotate to have base along +x-axis
                transform.append_rotation_wrt_center_mut(&UnitQuaternion::from_euler_angles(
                    0.0,
                    0.0,
                    std::f32::consts::FRAC_PI_2,
                ));
                transform.append_translation_mut(&Translation3::new(half_height, 0.0, 0.0));
                // Rotate to face target point
                let direction = (target_point.coords - actor_position).normalize();
                let yaw = direction.z.atan2(direction.x);
                let pitch = (-direction.y)
                    .atan2((direction.x * direction.x + direction.z * direction.z).sqrt());
                transform.append_rotation_wrt_point_mut(
                    &UnitQuaternion::from_euler_angles(0.0, 0.0, -pitch),
                    &Point3::from(actor_position),
                );
                transform.append_rotation_wrt_point_mut(
                    &UnitQuaternion::from_euler_angles(0.0, -yaw, 0.0),
                    &Point3::from(actor_position),
                );

                ShapeTransform {
                    shape: Box::new(shape),
                    transform,
                }
            }

            AreaShape::Sphere { radius } => ShapeTransform {
                shape: Box::new(Ball::new(radius.get::<meter>())),
                transform: Isometry3::new(translation, Vector3::zeros()),
            },

            AreaShape::Cube { side_length } => {
                let half_size = side_length.get::<meter>() / 2.0;
                ShapeTransform {
                    shape: Box::new(Cuboid::new(Vector3::new(half_size, half_size, half_size))),
                    transform: Isometry3::new(translation, Vector3::zeros()),
                }
            }

            AreaShape::Cylinder { radius, height } => {
                // Parry's cylinders are centered on the middle, but we want them to start on the ground
                translation.y += height.get::<meter>() / 2.0;

                ShapeTransform {
                    shape: Box::new(Cylinder::new(
                        height.get::<meter>() / 2.0,
                        radius.get::<meter>(),
                    )),
                    transform: Isometry3::new(translation, Vector3::zeros()),
                }
            }

            AreaShape::Line { length, width } => {
                let half_length = length.get::<meter>() / 2.0;
                let half_width = width.get::<meter>() / 2.0;
                let mut transform = Isometry3::new(translation, Vector3::zeros());
                if fixed_on_actor {
                    // Line starts at the actor's position
                    transform.append_translation_mut(&Translation3::new(half_length, 0.0, 0.0));
                    // Rotate to point towards target point
                    let direction = (target_point.coords - actor_position).normalize();
                    let yaw = direction.z.atan2(direction.x);
                    let pitch = (-direction.y)
                        .atan2((direction.x * direction.x + direction.z * direction.z).sqrt());
                    transform.append_rotation_wrt_point_mut(
                        &UnitQuaternion::from_euler_angles(0.0, 0.0, -pitch),
                        &Point3::from(actor_position),
                    );
                    transform.append_rotation_wrt_point_mut(
                        &UnitQuaternion::from_euler_angles(0.0, -yaw, 0.0),
                        &Point3::from(actor_position),
                    );
                }
                ShapeTransform {
                    shape: Box::new(Cuboid::new(Vector3::new(
                        half_length,
                        half_width,
                        half_width,
                    ))),
                    transform,
                }
            }

            AreaShape::Capsule {
                half_height,
                radius,
            } => {
                // Parry's capsules are centered on the middle, but we want them to start on the ground
                translation.y += 2.0 * half_height.get::<meter>();

                ShapeTransform {
                    shape: Box::new(Capsule::new_y(
                        half_height.get::<meter>(),
                        radius.get::<meter>(),
                    )),
                    transform: Isometry3::new(translation, Vector3::zeros()),
                }
            }
        }
    }
}

pub struct ShapeTransform {
    pub shape: Box<dyn Shape>,
    pub transform: Isometry3<f32>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EntityFilter {
    All,
    Characters,
    Monsters,

    NotSelf,
    Specific(HashSet<Entity>),

    LifeStates(HashSet<LifeState>),
    NotLifeStates(HashSet<LifeState>),

    CreatureTypes(HashSet<CreatureType>),
    NotCreatureTypes(HashSet<CreatureType>),

    Attitudes(HashSet<Attitude>),
    NotAttitudes(HashSet<Attitude>),
}

impl EntityFilter {
    pub fn not_dead() -> Self {
        EntityFilter::NotLifeStates(HashSet::from([LifeState::Dead]))
    }

    pub fn matches(&self, world: &World, entity: Entity, actor: Option<Entity>) -> bool {
        match self {
            EntityFilter::All => true,
            EntityFilter::Characters => world.get::<&CharacterTag>(entity).is_ok(),
            EntityFilter::Monsters => world.get::<&MonsterTag>(entity).is_ok(),

            EntityFilter::NotSelf => {
                if let Some(actor) = actor {
                    entity != actor
                } else {
                    true
                }
            }
            EntityFilter::Specific(entities) => entities.contains(&entity),

            EntityFilter::LifeStates(states) => {
                if let Ok(life_state) = world.get::<&LifeState>(entity) {
                    states.contains(&life_state)
                } else {
                    false
                }
            }
            EntityFilter::NotLifeStates(states) => {
                if let Ok(life_state) = world.get::<&LifeState>(entity) {
                    !states.contains(&life_state)
                } else {
                    true
                }
            }

            EntityFilter::CreatureTypes(types) => {
                if let Ok(creature_type) = world.get::<&CreatureType>(entity) {
                    types.contains(&creature_type)
                } else {
                    false
                }
            }
            EntityFilter::NotCreatureTypes(types) => {
                if let Ok(creature_type) = world.get::<&CreatureType>(entity) {
                    !types.contains(&creature_type)
                } else {
                    true
                }
            }

            EntityFilter::Attitudes(attitudes) => {
                let Some(actor) = actor else {
                    return false;
                };
                attitudes.contains(&systems::factions::mutual_attitude(world, entity, actor))
            }
            EntityFilter::NotAttitudes(attitudes) => {
                let Some(actor) = actor else {
                    return false;
                };
                !attitudes.contains(&systems::factions::mutual_attitude(world, entity, actor))
            }
        }
    }
}

impl Into<TargetFilter> for EntityFilter {
    fn into(self) -> TargetFilter {
        TargetFilter::Entity(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AreaFilter {
    Unoccupied,
    Walkable,
}

impl AreaFilter {
    pub fn matches(&self, world: &World, geometry: &WorldGeometry, shape: &ShapeTransform) -> bool {
        match self {
            AreaFilter::Unoccupied => {
                let Result::Ok(intersection) = parry3d::query::intersection_test(
                    &shape.transform,
                    shape.shape.as_ref(),
                    &Isometry3::identity(),
                    &geometry.trimesh,
                ) else {
                    // If the intersection test fails for some reason, we don't
                    // want to allow targeting the area
                    error!(
                        "Failed to perform area filter intersection test between world geometry and targeting shape: {:?}, {:?}",
                        shape.shape.as_ref().shape_type(),
                        shape.transform
                    );
                    return false;
                };

                if intersection {
                    // If the shape intersects with the world geometry, it's probably
                    // not an unoccupied area, though it could also be that we're
                    // trying to target some part of the geometry which is not a
                    // horizontal plane, so we can check if moving the shape up a
                    // bit would avoid the intersection
                    let mut moved_shape_transform = shape.transform.clone();
                    moved_shape_transform.append_translation_mut(&Translation3::new(0.0, 0.1, 0.0));

                    let Result::Ok(intersection) = parry3d::query::intersection_test(
                        &moved_shape_transform,
                        shape.shape.as_ref(),
                        &Isometry3::identity(),
                        &geometry.trimesh,
                    ) else {
                        error!(
                            "Failed to perform area filter intersection test between world geometry and moved targeting shape: {:?}, {:?}",
                            shape.shape.as_ref().shape_type(),
                            moved_shape_transform
                        );
                        return false;
                    };
                    if !intersection {
                        // If moving the shape up avoids the intersection, then it's
                        // likely that the original intersection was just with the
                        // ground, so we can still consider it an unoccupied area
                        return true;
                    }

                    return false;
                }

                systems::geometry::entities_in_shape(world, shape.shape.as_ref(), &shape.transform)
                    .is_empty()
            }

            AreaFilter::Walkable => {
                let point = shape.transform.translation.vector.into();
                if let Some(nearest_walkable) =
                    systems::geometry::navmesh_nearest_point(geometry, point)
                {
                    let xz_distance = (nearest_walkable.x - point.x).powi(2)
                        + (nearest_walkable.z - point.z).powi(2);
                    // TODO: Currently we're just checking if the point is above
                    // the navmesh. Maybe we should also check the y distance?
                    return xz_distance < EPSILON;
                }

                false
            }
        }
    }
}

impl Into<TargetFilter> for AreaFilter {
    fn into(self) -> TargetFilter {
        TargetFilter::Area(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TargetFilter {
    Entity(EntityFilter),
    Area(AreaFilter),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TargetInstance {
    Entity {
        entity: EntityIdentifier,
        point_on_entity: Option<Point3<f32>>,
    },
    Point(Point3<f32>),
}

impl TargetInstance {
    pub fn entity(entity: EntityIdentifier) -> Self {
        TargetInstance::Entity {
            entity,
            point_on_entity: None,
        }
    }

    pub fn position(&self, world: &World) -> Option<Point3<f32>> {
        match self {
            TargetInstance::Entity {
                entity,
                point_on_entity,
            } => {
                if let Some(point_on_entity) = point_on_entity {
                    Some(*point_on_entity)
                } else {
                    let (_, shape_pose) =
                        systems::geometry::get_shape(&world, entity.id()).unwrap();
                    Some(shape_pose.translation.vector.into())
                }
            }
            TargetInstance::Point(point) => Some(*point),
        }
    }
}

// TODO: Slightly more descriptive errors
#[derive(Debug, Clone, PartialEq)]
pub enum TargetingError {
    NoTargetsProvided,
    ExceedsMaxTargets,
    DuplicateTargetNotAllowed {
        target: TargetInstance,
    },
    OutOfRange {
        target: TargetInstance,
        distance: Length,
        max_range: Length,
    },
    NoLineOfSight {
        target: TargetInstance,
    },
    InvalidTarget {
        target: TargetInstance,
        violated_filters: Vec<TargetFilter>,
    },
    NotSelf {
        target: TargetInstance,
        actor: EntityIdentifier,
    },
}

/// Defines the range parameters for targeting an action.
///
/// `normal` is the range within which the action can be used without penalty.
/// `max` is the maximum range at which the action can be used. Targeting beyond
/// `normal` range may incur penalties (e.g., disadvantage on attack rolls).
///
/// For melee actions, `normal` and `max` are typically the same.
///
/// Note that since there's several places where it is useful to be able to `Hash`
/// a `TargetingRange`, and `f32` does not implement `Hash` (as a consequence neither
/// does `uom::si::Length`), we store the range values as `u32` internally. For
/// the sake of accuracy, these `u32` values represent the range in millimeters.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct TargetingRange {
    /// Normal range of the action. Attacks made outside the normal range have
    /// disadvantage on their attack rolls
    normal: u32,
    /// Max range of the action. The action cannot target anything beyond this
    /// range
    max: u32,
}

impl TargetingRange {
    pub fn new<U>(normal: f32) -> Self
    where
        U: Unit + Conversion<f32, T = f32>,
    {
        let normal = Self::length_to_mm(Length::new::<U>(normal));
        TargetingRange {
            max: normal,
            normal,
        }
    }

    pub fn with_max<U>(normal: f32, max: f32) -> Self
    where
        U: Unit + Conversion<f32, T = f32>,
    {
        let normal = Self::length_to_mm(Length::new::<U>(normal));
        let max = Self::length_to_mm(Length::new::<U>(max));
        TargetingRange { normal, max }
    }

    pub fn normal(&self) -> Length {
        Self::mm_to_length(self.normal)
    }

    pub fn max(&self) -> Length {
        Self::mm_to_length(self.max)
    }

    pub fn in_range(&self, distance: Length) -> bool {
        let distance_mm = Self::length_to_mm(distance);
        distance_mm <= self.max
    }

    fn length_to_mm(length: Length) -> u32 {
        let length_mm = length.get::<meter>() * 1000.0;
        length_mm.round() as u32
    }

    fn mm_to_length(mm: u32) -> Length {
        Length::new::<meter>(mm as f32 / 1000.0)
    }

    pub fn is_melee(&self) -> bool {
        // TODO: Not the most elegant way to check for melee range
        self.max <= MELEE_RANGE_REACH.max
    }
}

impl fmt::Display for TargetingRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.normal == self.max {
            write!(f, "{} m", self.normal as f32 / 1000.0)
        } else {
            write!(
                f,
                "{} / {} m",
                self.normal as f32 / 1000.0,
                self.max as f32 / 1000.0
            )
        }
    }
}

impl FromStr for TargetingRange {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split('/').map(|part| part.trim()).collect();
        if parts.len() == 1 {
            let normal: f32 = parts[0]
                .parse()
                .map_err(|_| format!("Invalid range value: {}", parts[0]))?;
            Ok(TargetingRange::new::<meter>(normal))
        } else if parts.len() == 2 {
            let normal: f32 = parts[0]
                .parse()
                .map_err(|_| format!("Invalid range value: {}", parts[0]))?;
            let max: f32 = parts[1]
                .parse()
                .map_err(|_| format!("Invalid range value: {}", parts[1]))?;
            Ok(TargetingRange::with_max::<meter>(normal, max))
        } else {
            Err(format!("Invalid range format: {}", s))
        }
    }
}

impl TryFrom<String> for TargetingRange {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<TargetingRange> for String {
    fn from(spec: TargetingRange) -> Self {
        spec.to_string()
    }
}
