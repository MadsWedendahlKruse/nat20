use std::collections::HashSet;

use hecs::Entity;
use parry3d::na::Point3;
use tracing::{debug, error, warn};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::actions::{
        action::{Action, ActionTimeline},
        execution::ExecutionStatus,
    },
    engine::{
        action_prompt::{ActionDecision, ActionError},
        game_state::GameState,
        geometry::WorldPath,
    },
    systems::{
        self,
        geometry::Parabola,
        movement::{MoveMode, MovementError},
    },
};

const MOVEMENT_SPEED: f32 = 5.0; // [m/s]

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
    pub fn update(
        &mut self,
        game_state: &mut GameState,
        entity: Entity,
        delta_time: f32,
    ) -> Vec<ActivityCommand> {
        if self.is_paused() {
            return Vec::new();
        }

        self.state.update(game_state, entity, delta_time)
    }

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

#[derive(Debug, Clone)]
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

/// In order to avoid a double borrow of the ActivityState it needs to return commands
/// to be executed in the game state after the update at which point the borrow is dropped
impl ActivityStateKind {
    pub fn update(
        &mut self,
        game_state: &mut GameState,
        entity: Entity,
        delta_time: f32,
    ) -> Vec<ActivityCommand> {
        let mut commands = Vec::new();

        match self {
            Self::Idle => {
                // Do nothing
            }

            Self::Moving {
                path,
                current_target,
                action,
            } => {
                if *current_target >= path.points.len() {
                    warn!(
                        "Current target index {} is out of bounds for path with length {}. Target appears to have reached goal, but follow up state was not set correctly. Did the action submission fail? Setting state to idle",
                        current_target,
                        path.points.len()
                    );
                    *self = Self::Idle;
                    return commands;
                }

                let target_point = path.points[*current_target];
                let position =
                    systems::geometry::get_foot_position(&game_state.world, entity).unwrap();
                let direction = target_point - position;
                let distance_to_target = direction.norm();

                if distance_to_target != 0.0 {
                    commands.push(ActivityCommand::new(move |game_state: &mut GameState| {
                        systems::movement::move_entity(
                            game_state,
                            entity,
                            &(position + direction.normalize() * MOVEMENT_SPEED * delta_time),
                            MoveMode::Voluntary,
                        );
                    }));
                }

                if distance_to_target < MOVEMENT_SPEED * delta_time {
                    // Reached the target point
                    *current_target += 1;

                    if *current_target >= path.points.len() {
                        // Reached the end of the path
                        debug!("Entity {:?} reached destination {:?}", entity, target_point);

                        if let Some(action_decision) = action.take() {
                            debug!(
                                "Entity {:?} has a follow-up action, setting to act after movement",
                                entity
                            );
                            commands.push(ActivityCommand::new(
                                move |game_state: &mut GameState| match game_state
                                    .submit_decision(action_decision.clone())
                                {
                                    Ok(_) => {}
                                    Err(error) => {
                                        error!("Failed to submit action decision: {:?}", error)
                                    }
                                },
                            ));
                        } else {
                            debug!(
                                "Entity {:?} has no follow-up action, setting to idle",
                                entity
                            );
                            *self = Self::Idle;
                        }
                    }
                }
            }

            Self::Acting {
                elapsed_time,
                timeline:
                    ActionTimeline {
                        total_duration,
                        perform_time,
                        step_spacing,
                    },
                phase_cooldown,
            } => {
                *elapsed_time += delta_time;

                let status = systems::actions::execution_status(game_state, entity);

                if *elapsed_time >= *perform_time && status == Some(ExecutionStatus::Running) {
                    *phase_cooldown += delta_time;

                    if *phase_cooldown >= *step_spacing {
                        debug!(
                            "Action phase cooldown elapsed for entity {:?}, advancing execution",
                            entity
                        );
                        *phase_cooldown = 0.0;
                        commands.push(ActivityCommand::new(move |game_state: &mut GameState| {
                            systems::actions::advance_execution(game_state, entity);
                        }));
                    }
                }

                if *elapsed_time >= *total_duration
                    && status.is_none_or(|status| status == ExecutionStatus::Done)
                {
                    debug!(
                        "Entity {:?} finished action after {:?} seconds",
                        entity, total_duration
                    );
                    *self = Self::Idle;

                    commands.push(ActivityCommand::new(move |game_state: &mut GameState| {
                        
                        let scope = game_state.scope_for_entity(entity);
                        debug!(
                            "Action completed for entity {:?}, clearing blockers and resuming pending events if ready",
                            entity
                        );
                        
                        game_state.action_executions.remove(&entity);
                        game_state
                            .interaction_engine
                            .session_mut(scope)
                            .clear_blocker(entity);
                        game_state.resume_pending_events_if_ready(scope);
                    }));
                }
            }

            Self::Displaced {
                trajectory,
                elapsed_time,
            } => {
                *elapsed_time += delta_time;

                let new_position = trajectory.position_at_time(*elapsed_time);
                commands.push(ActivityCommand::new(move |game_state: &mut GameState| {
                    systems::movement::move_entity(
                        game_state,
                        entity,
                        &new_position,
                        MoveMode::Displace,
                    );
                }));

                if *elapsed_time >= trajectory.max_time {
                    debug!(
                        "Entity {:?} finished displacement after {:?} seconds",
                        entity, trajectory.max_time
                    );

                    let final_position = trajectory.position_at_time(trajectory.max_time);
                    systems::movement::apply_fall_damage(
                        game_state,
                        entity,
                        Length::new::<meter>(trajectory.origin.y - final_position.y),
                    );

                    *self = Self::Idle;
                }
            }
        }

        return commands;
    }
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

pub struct ActivityCommand(pub Box<dyn FnOnce(&mut GameState)>);

impl ActivityCommand {
    pub fn execute(self, game_state: &mut GameState) {
        (self.0)(game_state);
    }

    pub fn new<F: FnOnce(&mut GameState) + 'static>(command: F) -> Self {
        Self(Box::new(command))
    }
}
