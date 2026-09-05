use hecs::Entity;
use parry3d::na::Point3;
use strum::EnumDiscriminants;
use tracing::{debug, warn};

use crate::{
    components::actions::action::{Action, ActionTimeline},
    engine::{
        action_prompt::{ActionDecision, ActionError},
        event::EventId,
        geometry::WorldPath,
    },
    systems::{geometry::Parabola, movement::MovementError},
};

// TODO: Should these two enums live here?
#[derive(Debug, Clone)]
pub enum Activity {
    Move {
        entity: Entity,
        goal: Point3<f32>,
    },
    Act {
        action: ActionDecision,
    },
    MoveAndAct {
        goal: Point3<f32>,
        action: ActionDecision,
    },
}

#[derive(Debug, Clone)]
pub enum ActivityError {
    MovementError(MovementError),
    ActionError(ActionError),
}

impl From<MovementError> for ActivityError {
    fn from(error: MovementError) -> Self {
        Self::MovementError(error)
    }
}

impl From<ActionError> for ActivityError {
    fn from(error: ActionError) -> Self {
        Self::ActionError(error)
    }
}

#[derive(Debug, Clone, EnumDiscriminants)]
#[strum_discriminants(name(ActivityStateTag))]
#[derive(Default)]
pub enum ActivityState {
    #[default]
    Idle,
    Moving {
        path: WorldPath,
        /// Current index in the path which the entity is moving towards
        current_target: usize,
        /// Potential action to be performed after reaching the destination
        action: Option<ActionDecision>,
    },
    Acting {
        timeline: ActionTimeline,
        elapsed_time: f32,
        phase_cooldown: f32,
        blocking_event: Option<EventId>,
    },
    Displaced {
        trajectory: Parabola,
        elapsed_time: f32,
    },
}

impl ActivityState {
    pub fn tag(&self) -> ActivityStateTag {
        ActivityStateTag::from(self)
    }

    pub fn set_idle(&mut self) {
        debug!("Setting entity to idle");
        *self = ActivityState::Idle;
    }

    pub fn is_idle(&self) -> bool {
        matches!(self, ActivityState::Idle)
    }

    pub fn set_moving(&mut self, path: WorldPath, action: Option<ActionDecision>) {
        debug!("Setting entity to move to goal {:?}", path.points.last());

        *self = ActivityState::Moving {
            path,
            current_target: 0,
            action,
        };
    }

    pub fn set_acting(&mut self, action: &Action, blocking_event: Option<EventId>) {
        if matches!(self, ActivityState::Acting { .. }) {
            warn!(
                "Overriding activity state for entity which is already acting, with action {:?}",
                action
            );
        }
        debug!("Setting entity to perform action {:?}", action);

        *self = ActivityState::Acting {
            timeline: action.timeline.clone(),
            elapsed_time: 0.0,
            phase_cooldown: action.timeline.step_spacing,
            blocking_event,
        };
    }

    pub fn is_acting(&self) -> bool {
        matches!(self, ActivityState::Acting { .. })
    }

    pub fn set_displaced(&mut self, trajectory: Parabola) {
        debug!(
            "Setting entity to be displaced with trajectory {:?}",
            trajectory
        );
        *self = ActivityState::Displaced {
            trajectory,
            elapsed_time: 0.0,
        };
    }
}
