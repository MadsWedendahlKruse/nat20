use hecs::{Bundle, Entity};
use parry3d::query::Ray;
use tracing::{debug, warn};
use uom::si::{f32::Velocity, velocity::meter_per_second};

use crate::{
    components::actions::targeting::{
        LineOfSight, LineOfSightExtentTemplate, LineOfSightTrajectory, TargetInstance,
    },
    engine::{action_prompt::ActionData, game_state::GameState},
    entities,
    systems::{
        self,
        entities::EntityKind,
        geometry::{Pose, RaycastMode},
    },
};

#[derive(Bundle, Debug, Clone)]
pub struct Projectile {
    entity_kind: EntityKind,
    data: ProjectileData,
}

impl Projectile {
    pub fn new(data: ProjectileData) -> Self {
        Self {
            entity_kind: EntityKind::Projectile,
            data,
        }
    }
}

pub fn should_update(game_state: &GameState, entity: Entity) -> bool {
    if let Ok(projectile) = game_state.world.get::<&ProjectileData>(entity) {
        entities::creature::should_update(game_state, projectile.owner)
    } else {
        false
    }
}

pub fn update(game_state: &mut GameState, delta_time: f32, entity: Entity) {
    let owner = {
        let Ok(mut projectile) = game_state.world.get::<&mut ProjectileData>(entity) else {
            return;
        };

        projectile.flight_time += delta_time;
        projectile.pose.translation = projectile
            .trajectory
            .position_at_time(projectile.flight_time)
            .into();
        if let Some(orientation) = projectile
            .trajectory
            .orientation_at_time(projectile.flight_time)
        {
            projectile.pose.rotation = orientation;
        }

        if projectile.flight_time < projectile.time_of_impact {
            return;
        }

        projectile.owner
    };

    debug!("Projectile {:?} has reached its target", entity);

    game_state
        .despawn(entity)
        .expect("Failed to despawn projectile entity");

    systems::actions::projectile_impact(game_state, owner);
}

// TODO: Not sure if it's the most ECS-idiomatic solution to just cram everything
// into one component
#[derive(Debug, Clone)]
pub struct ProjectileData {
    pub pose: Pose,
    pub trajectory: RaycastMode,
    pub flight_time: f32,
    pub time_of_impact: f32,
    /// The entity whose action execution is waiting on this projectile
    pub owner: Entity,
}

#[derive(Debug, Clone)]
pub enum ProjectileError {
    NoLineOfSight,
    NoRaycast,
}

#[derive(Debug, Clone)]
pub enum ProjectileTemplate {
    Ray { velocity: Velocity },
    Parabola { launch_velocity: Velocity },
}

impl ProjectileTemplate {
    pub fn instantiate(
        &self,
        game_state: &mut GameState,
        action: &ActionData,
        target: &TargetInstance,
    ) -> Result<Projectile, ProjectileError> {
        let trajectory = match self {
            ProjectileTemplate::Ray { .. } => LineOfSightTrajectory::Ray,
            ProjectileTemplate::Parabola { launch_velocity } => LineOfSightTrajectory::Parabola {
                launch_velocity: *launch_velocity,
            },
        };

        let mut line_of_sight_result = systems::geometry::line_of_sight_entity_target(
            &game_state.world,
            &game_state.geometry,
            action.actor.id(),
            target,
            &LineOfSight {
                trajectory,
                extent: LineOfSightExtentTemplate::Point,
            },
        );

        if !line_of_sight_result.has_line_of_sight {
            return Err(ProjectileError::NoLineOfSight);
        }

        let Some(raycast_result) = &mut line_of_sight_result.raycast_result else {
            warn!(
                "Line of sight check succeeded but no raycast result found for projectile launch"
            );
            return Err(ProjectileError::NoRaycast);
        };

        let Some(closest) = raycast_result.closest() else {
            warn!(
                "Raycast result has no closest point for projectile launch, this should not happen"
            );
            return Err(ProjectileError::NoRaycast);
        };
        let mut time_of_impact = closest.toi;

        match (&self, &mut raycast_result.mode) {
            (ProjectileTemplate::Ray { velocity }, RaycastMode::Ray(ray)) => {
                // Scale the time of impact so it matches the velocity of the projectile
                let velocity = velocity.get::<meter_per_second>();
                time_of_impact /= velocity;
                *ray = Ray {
                    origin: ray.origin,
                    dir: ray.dir.normalize() * velocity,
                };
            }
            _ => { /* Don't think it's necesarry to do anything else */ }
        }

        Ok(Projectile::new(ProjectileData {
            pose: raycast_result.mode.pose_at_time(0.0),
            trajectory: raycast_result.mode.clone(),
            flight_time: 0.0,
            time_of_impact,
            owner: action.actor.id(),
        }))
    }
}
