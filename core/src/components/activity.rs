use std::collections::HashSet;

use hecs::Entity;
use parry3d::na::Point3;
use strum::EnumDiscriminants;
use tracing::{debug, warn};

use crate::{
    components::actions::action::{Action, ActionTimeline},
    engine::{
        action_prompt::{ActionDecision, ActionError},
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

#[derive(Debug, Clone, Default)]
pub struct ActivityState {
    pub state: ActivityStateKind,
    pub pause_reasons: HashSet<ActivityPauseReason>,
}

impl ActivityState {
    pub fn pause(&mut self, reason: ActivityPauseReason) {
        debug!("Pausing activity due to reason {:?}", reason);
        self.pause_reasons.insert(reason);
    }

    pub fn resume(&mut self, reason: ActivityPauseReason) {
        debug!("Resuming activity for reason {:?}", reason);
        self.pause_reasons.remove(&reason);
    }

    pub fn is_paused(&self) -> bool {
        !self.pause_reasons.is_empty()
    }

    pub fn set_idle(&mut self) {
        debug!("Setting entity to idle");
        self.state = ActivityStateKind::Idle;
    }

    pub fn is_idle(&self) -> bool {
        matches!(self.state, ActivityStateKind::Idle)
    }

    pub fn set_moving(&mut self, path: WorldPath, action: Option<ActionDecision>) {
        debug!("Setting entity to move to goal {:?}", path.points.last());

        self.state = ActivityStateKind::Moving {
            path,
            current_target: 0,
            action,
        };
    }

    pub fn is_moving(&self) -> bool {
        matches!(self.state, ActivityStateKind::Moving { .. })
    }

    pub fn set_acting(&mut self, action: &Action) {
        if matches!(self.state, ActivityStateKind::Acting { .. }) {
            warn!(
                "Overriding activity state for entity which is already acting, with action {:?}",
                action
            );
        }
        debug!("Setting entity to perform action {:?}", action);

        self.state = ActivityStateKind::Acting {
            timeline: action.timeline.clone(),
            elapsed_time: 0.0,
            phase_cooldown: action.timeline.step_spacing,
        };
    }

    pub fn is_acting(&self) -> bool {
        matches!(self.state, ActivityStateKind::Acting { .. })
    }

    pub fn set_displaced(&mut self, trajectory: Parabola) {
        debug!(
            "Setting entity to be displaced with trajectory {:?}",
            trajectory
        );
        self.state = ActivityStateKind::Displaced {
            trajectory,
            elapsed_time: 0.0,
        };
    }
}

#[derive(Debug, Clone, EnumDiscriminants)]
#[strum_discriminants(name(ActivityStateKindTag))]
pub enum ActivityStateKind {
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
    },
    Displaced {
        trajectory: Parabola,
        elapsed_time: f32,
    },
}

impl Default for ActivityStateKind {
    fn default() -> Self {
        Self::Idle
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ActivityPauseReason {
    Reaction,
}
