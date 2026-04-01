use hecs::{Entity, World};
use parry3d::query::Ray;
use tracing::warn;
use uom::si::{f32::Velocity, velocity::meter_per_second};

use crate::{
    components::{
        actions::{
            action::{ActionConditionResolution, ActionKind},
            targeting::{LineOfSightMode, TargetInstance},
        },
        damage::DamageRollResult,
        dice::DiceSetRollResult,
        effects::effect::EffectInstanceTemplate,
        id::EntityIdentifier,
    },
    engine::{action_prompt::ActionData, game_state::GameState, geometry::WorldGeometry},
    systems::{
        self,
        geometry::{Pose, RaycastMode},
    },
};

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
        world: &World,
        world_geometry: &WorldGeometry,
        entity: Entity,
        pending_impact: PendingImpact,
    ) -> Result<Projectile, ProjectileError> {
        let line_of_sight_mode = match self {
            ProjectileTemplate::Ray { .. } => LineOfSightMode::Ray,
            ProjectileTemplate::Parabola { launch_velocity } => LineOfSightMode::Parabola {
                launch_velocity: *launch_velocity,
            },
        };

        let target = match &pending_impact {
            PendingImpact::Resolved { target, .. } => {
                &TargetInstance::Entity(EntityIdentifier::from_world(world, *target))
            }
            PendingImpact::Deferred { target, .. } => target,
        };

        let mut line_of_sight_result = systems::geometry::line_of_sight_entity_target(
            world,
            world_geometry,
            entity,
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

        match (&self, &mut raycast_result.mode) {
            // Make sure to set the velocity of the raycast mode according to the projectile template
            (ProjectileTemplate::Ray { velocity }, RaycastMode::Ray(ray)) => {
                let velocity = velocity.get::<meter_per_second>();
                *ray = Ray {
                    origin: ray.origin,
                    dir: ray.dir.normalize() * velocity,
                };
            }
            _ => { /* Don't think it's necesarry to do anything else */ }
        }

        let Some(closest) = raycast_result.closest() else {
            warn!(
                "Raycast result has no closest point for projectile launch, this should not happen"
            );
            return Err(ProjectileError::NoRaycast);
        };

        Ok(Projectile {
            pose: raycast_result.mode.pose_at_time(0.0),
            source: entity,
            trajectory: raycast_result.mode.clone(),
            flight_time: 0.0,
            time_of_impact: closest.toi,
            pending_impact,
        })
    }
}

/// The resolved or deferred gameplay state a projectile carries to its target.
#[derive(Clone)]
pub enum PendingImpact {
    /// Condition was resolved at launch (attack roll or unconditional).
    /// On landing, apply the pre-rolled result directly.
    Resolved {
        target: Entity,
        condition_resolution: ActionConditionResolution,
        action_data: ActionData,
        damage: Option<DamageRollResult>,
        effect: Option<EffectInstanceTemplate>,
        healing: Option<DiceSetRollResult>,
    },
    /// Condition not yet resolved (saving throw / AoE).
    /// On landing, discover affected entities and run the full execution.
    Deferred {
        target: TargetInstance,
        action_kind: ActionKind,
        action_data: ActionData,
    },
}

pub struct Projectile {
    pub pose: Pose,
    pub source: Entity,
    pub trajectory: RaycastMode,
    pub flight_time: f32,
    pub time_of_impact: f32,
    pub pending_impact: PendingImpact,
}

impl Projectile {
    pub fn update(&mut self, game_state: &mut GameState, delta_time: f32) {
        self.flight_time += delta_time;
        self.pose.translation = self.trajectory.position_at_time(self.flight_time).into();
        if let Some(orientation) = self.trajectory.orientation_at_time(self.flight_time) {
            self.pose.rotation = orientation;
        }

        if self.flight_time >= self.time_of_impact {
            // TODO: Apply the impact?
        }
    }
}
