use std::{collections::HashSet, fmt, str::FromStr};

use hecs::{Entity, World};
use parry3d::{
    na::{Isometry3, Point3},
    shape::Shape,
};
use serde::{Deserialize, Serialize};
use uom::{
    Conversion,
    si::{
        f32::{Angle, Length},
        length::{Unit, meter},
    },
};

use crate::{
    components::{health::life_state::LifeState, items::equipment::weapon::MELEE_RANGE_REACH},
    engine::geometry::WorldGeometry,
    entities::{character::CharacterTag, monster::MonsterTag},
    systems,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TargetingKind {
    SelfTarget, // e.g. Second Wind
    Single,
    Multiple {
        max_targets: u8,
    },
    Area {
        shape: AreaShape,
        fixed_on_actor: bool,
    },
}

// TODO: parry3d shapes?
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AreaShape {
    Arc { angle: Angle, length: Length },        // e.g. Cone of Cold
    Sphere { radius: Length },                   // e.g. Fireball
    Cube { side_length: Length },                // e.g. Wall of Force
    Cylinder { radius: Length, height: Length }, // e.g. Cloudkill
    Line { length: Length, width: Length },      // e.g. Lightning Bolt
}

impl AreaShape {
    pub fn parry3d_shape(
        &self,
        world: &World,
        actor: Entity,
        fixed_on_actor: bool,
        target_point: &Point3<f32>,
    ) -> (Box<dyn Shape>, Isometry3<f32>) {
        let (_, actor_shape_pose) = systems::geometry::get_shape(world, actor).unwrap();
        let actor_position = actor_shape_pose.translation.vector;

        let mut translation = if fixed_on_actor {
            actor_position
        } else {
            target_point.coords
        };

        match self {
            AreaShape::Arc { .. } => {
                todo!("Parry3D does not have a built-in arc shape");
            }
            AreaShape::Sphere { radius } => (
                Box::new(parry3d::shape::Ball::new(radius.get::<meter>())),
                Isometry3::new(translation, parry3d::na::Vector3::zeros()),
            ),
            AreaShape::Cube { side_length } => {
                let half_size = side_length.get::<meter>() / 2.0;
                (
                    Box::new(parry3d::shape::Cuboid::new(parry3d::na::Vector3::new(
                        half_size, half_size, half_size,
                    ))),
                    // TODO: Cube rotation?
                    Isometry3::new(translation, parry3d::na::Vector3::zeros()),
                )
            }
            AreaShape::Cylinder { radius, height } => (
                Box::new(parry3d::shape::Cylinder::new(
                    height.get::<meter>(),
                    radius.get::<meter>(),
                )),
                Isometry3::new(translation, parry3d::na::Vector3::zeros()),
            ),
            AreaShape::Line { length, width } => {
                let half_length = length.get::<meter>() / 2.0;
                let half_width = width.get::<meter>() / 2.0;
                let mut rotation = parry3d::na::Vector3::zeros();
                if fixed_on_actor {
                    // Line starts at the actor's position
                    translation.x += half_length;
                    // Rotate around Y axis to point towards target point
                    let direction = (target_point.coords - actor_position).normalize();
                    let yaw = direction.y.atan2(direction.x);
                    rotation = parry3d::na::Vector3::new(0.0, yaw, 0.0);
                }
                (
                    Box::new(parry3d::shape::Cuboid::new(parry3d::na::Vector3::new(
                        half_length,
                        half_width,
                        half_width,
                    ))),
                    Isometry3::new(translation, rotation),
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EntityFilter {
    All,
    Characters,
    Monsters,
    Specific(HashSet<Entity>),
    LifeStates(HashSet<LifeState>),
    NotLifeStates(HashSet<LifeState>),
}

impl EntityFilter {
    pub fn not_dead() -> Self {
        EntityFilter::NotLifeStates(HashSet::from([LifeState::Dead, LifeState::Defeated]))
    }

    pub fn matches(&self, world: &World, entity: &Entity) -> bool {
        match self {
            EntityFilter::All => true,
            EntityFilter::Characters => world.get::<&CharacterTag>(*entity).is_ok(),
            EntityFilter::Monsters => world.get::<&MonsterTag>(*entity).is_ok(),
            EntityFilter::Specific(entities) => entities.contains(entity),
            EntityFilter::LifeStates(states) => {
                if let Ok(life_state) = world.get::<&LifeState>(*entity) {
                    states.contains(&life_state)
                } else {
                    false
                }
            }
            EntityFilter::NotLifeStates(states) => {
                if let Ok(life_state) = world.get::<&LifeState>(*entity) {
                    !states.contains(&life_state)
                } else {
                    true
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TargetInstance {
    Entity(Entity),
    Point(Point3<f32>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TargetingError {
    ExceedsMaxTargets,
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

#[derive(Debug, Clone)]
pub struct TargetingContext {
    pub kind: TargetingKind,
    pub range: TargetingRange,
    pub require_line_of_sight: bool,
    pub allowed_targets: EntityFilter,
}

impl TargetingContext {
    pub fn new(
        kind: TargetingKind,
        range: TargetingRange,
        require_line_of_sight: bool,
        allowed_targets: EntityFilter,
    ) -> Self {
        TargetingContext {
            kind,
            range,
            require_line_of_sight,
            allowed_targets,
        }
    }

    pub fn self_target() -> Self {
        TargetingContext {
            kind: TargetingKind::SelfTarget,
            range: TargetingRange::new::<meter>(0.0),
            require_line_of_sight: false,
            allowed_targets: EntityFilter::All,
        }
    }

    pub fn validate_targets(
        &self,
        world: &World,
        world_geometry: &WorldGeometry,
        actor: Entity,
        targets: &[TargetInstance],
    ) -> Result<(), TargetingError> {
        for target in targets {
            // Check range
            let actor_position = systems::geometry::get_foot_position(world, actor).unwrap();

            let distance = match target {
                TargetInstance::Entity(entity) => {
                    systems::geometry::distance_between_entities(world, actor, *entity).unwrap()
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

            // Check line of sight
            if self.require_line_of_sight {
                let line_of_sight_result = match target {
                    TargetInstance::Entity(entity) => {
                        systems::geometry::line_of_sight_entity_entity(
                            world,
                            world_geometry,
                            actor,
                            *entity,
                        )
                    }
                    TargetInstance::Point(point) => systems::geometry::line_of_sight_entity_point(
                        world,
                        world_geometry,
                        actor,
                        *point,
                    ),
                };

                if !line_of_sight_result.has_line_of_sight {
                    return Err(TargetingError::NoLineOfSight {
                        target: target.clone(),
                    });
                }
            }

            // Check allowed targets
            match target {
                TargetInstance::Entity(entity) => {
                    if !self.allowed_targets.matches(world, entity) {
                        return Err(TargetingError::InvalidTarget {
                            target: target.clone(),
                        });
                    }
                }
                TargetInstance::Point(_) => {
                    // Points are always allowed
                }
            }
        }

        Ok(())
    }
}
