use hecs::Entity;
use parry3d::query::Ray;
use tracing::warn;
use uom::si::{f32::Velocity, velocity::meter_per_second};

use crate::{
    components::{
        actions::{
            action_step::ActionPhase,
            targeting::{LineOfSightMode, TargetInstance},
        },
        activity::ActivityGameStateCommand,
    },
    engine::{action_prompt::ActionData, game_state::GameState},
    systems::{
        self,
        geometry::{Pose, RaycastMode},
    },
};

#[derive(Debug, Clone)]
pub struct Projectile {
    pub pose: Pose,
    pub trajectory: RaycastMode,
    pub flight_time: f32,
    pub time_of_impact: f32,
    pub delivery_phase: ActionPhase,
    pub paused: bool,
}

impl Projectile {
    pub fn update(&mut self, entity: Entity, delta_time: f32) -> Vec<ActivityGameStateCommand> {
        if self.paused {
            return vec![];
        }

        self.flight_time += delta_time;
        self.pose.translation = self.trajectory.position_at_time(self.flight_time).into();
        if let Some(orientation) = self.trajectory.orientation_at_time(self.flight_time) {
            self.pose.rotation = orientation;
        }

        if self.flight_time >= self.time_of_impact {
            let actor = self.delivery_phase.action.actor.id();
            vec![
                ActivityGameStateCommand::DespawnEntity { entity },
                ActivityGameStateCommand::PerformActionPhase {
                    entity: actor,
                    phase: self.delivery_phase.clone(),
                },
            ]
        } else {
            vec![]
        }
    }

    pub fn pause(&mut self) {
        self.paused = true;
    }

    pub fn resume(&mut self) {
        self.paused = false;
    }
}

#[derive(Debug, Clone)]
pub struct ProjectileTag;

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
        delivery_phase: ActionPhase,
    ) -> Result<Projectile, ProjectileError> {
        let line_of_sight_mode = match self {
            ProjectileTemplate::Ray { .. } => LineOfSightMode::Ray,
            ProjectileTemplate::Parabola { launch_velocity } => LineOfSightMode::Parabola {
                launch_velocity: *launch_velocity,
            },
        };

        let mut line_of_sight_result = systems::geometry::line_of_sight_entity_target(
            &game_state.world,
            &game_state.geometry,
            action.actor.id(),
            target,
            &line_of_sight_mode,
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
                let velocity = velocity.get::<meter_per_second>();
                time_of_impact /= velocity;
                *ray = Ray {
                    origin: ray.origin,
                    dir: ray.dir.normalize() * velocity,
                };
            }
            _ => { /* Don't think it's necesarry to do anything else */ }
        }

        Ok(Projectile {
            pose: raycast_result.mode.pose_at_time(0.0),
            trajectory: raycast_result.mode.clone(),
            flight_time: 0.0,
            time_of_impact,
            delivery_phase,
            paused: game_state
                .session_for_entity(action.actor.id())
                .map_or(false, |session| !session.ready_to_resume()),
        })
    }
}
