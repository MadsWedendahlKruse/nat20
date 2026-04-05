use std::collections::{HashMap, VecDeque};

use hecs::Entity;
use parry3d::na::Point3;
use tracing::{debug, warn};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{
        actions::{
            action::{Action, ActionTimeline},
            action_step::ActionPhase,
        },
        id::EntityIdentifier,
        speed::Speed,
    },
    engine::{
        action_prompt::{ActionDecision, ActionError},
        event::{Event, EventKind},
        game_state::GameState,
        geometry::WorldPath,
    },
    systems::{self, movement::MovementError},
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

#[derive(Debug)]
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

#[derive(Debug, Clone)]
pub enum ActivityState {
    Idle,
    Moving {
        path: WorldPath,
        /// Current index in the path which the entity is moving towards
        current_target: usize,
        /// Indices of points in the path that trigger opportunity attacks
        opportunity_attacks: HashMap<usize, Entity>,
        /// Potential action to be performed after reaching the destination
        action: Option<ActionDecision>,
        /// Whether the movement is currently paused due to an opportunity attack
        paused: bool,
    },
    Acting {
        timeline: ActionTimeline,
        elapsed_time: f32,
        phases: VecDeque<ActionPhase>,
        phase_cooldown: f32,
        paused: bool,
    },
}

/// In order to avoid a double borrow of the ActivityState it needs to return commands
/// to be executed in the game state after the update at which point the borrow is dropped
impl ActivityState {
    pub fn update(
        &mut self,
        game_state: &mut GameState,
        entity: Entity,
        delta_time: f32,
    ) -> Vec<ActivityGameStateCommand> {
        let mut commands = Vec::new();

        match self {
            Self::Idle => {
                // Do nothing
            }

            Self::Moving {
                path,
                current_target,
                opportunity_attacks,
                action,
                paused,
            } => {
                if *paused {
                    return commands;
                }

                let target_point = path.points[*current_target];
                let position =
                    systems::geometry::get_foot_position(&game_state.world, entity).unwrap();
                let direction = target_point - position;
                let distance_to_target = direction.norm();

                if distance_to_target != 0.0 {
                    let distance_to_move = distance_to_target.min(MOVEMENT_SPEED * delta_time);
                    let movement_vector = direction.normalize() * distance_to_move;
                    let new_position = position + movement_vector;
                    systems::geometry::teleport_to(&mut game_state.world, entity, &new_position);

                    if game_state.in_combat.contains_key(&entity) {
                        systems::helpers::get_component_mut::<Speed>(&mut game_state.world, entity)
                            .record_movement(Length::new::<meter>(distance_to_move));
                    }
                }

                if distance_to_target < MOVEMENT_SPEED * delta_time {
                    // Reached the target point

                    if let Some(attacker) = opportunity_attacks.remove(current_target) {
                        commands.push(ActivityGameStateCommand::ProcessEvent(Event::new(
                            EventKind::MovingOutOfReach {
                                mover: EntityIdentifier::from_world(&game_state.world, entity),
                                entity: EntityIdentifier::from_world(&game_state.world, attacker),
                                continue_movement: true,
                            },
                        )));
                        *paused = true;
                    }

                    *current_target += 1;

                    if *current_target >= path.points.len() {
                        // Reached the end of the path
                        debug!("Entity {:?} reached destination {:?}", entity, target_point);

                        if let Some(action_decision) = action.take() {
                            debug!(
                                "Entity {:?} has a follow-up action, setting to act after movement",
                                entity
                            );
                            commands.push(ActivityGameStateCommand::SubmitAction(action_decision));
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
                        submit_time,
                        step_spacing,
                    },
                phases,
                phase_cooldown,
                paused,
            } => {
                if *paused {
                    return commands;
                }

                *elapsed_time += delta_time;

                if *elapsed_time >= *submit_time {
                    *phase_cooldown += delta_time;
                }

                if *phase_cooldown >= *step_spacing && !phases.is_empty() {
                    debug!(
                        "Action phase cooldown elapsed for entity {:?}, checking for next phase",
                        entity
                    );
                    *phase_cooldown = 0.0;
                    commands.push(ActivityGameStateCommand::PerformActionPhase {
                        entity,
                        phase: phases.pop_front().unwrap(),
                    });
                }

                if *elapsed_time >= *total_duration && phases.is_empty() {
                    debug!(
                        "Entity {:?} finished action after {:?} seconds",
                        entity, total_duration
                    );
                    commands.push(ActivityGameStateCommand::ActivityCompleted { entity });
                    *self = Self::Idle;
                }
            }
        }

        return commands;
    }

    pub fn set_idle(&mut self) {
        debug!("Setting entity to idle");
        *self = Self::Idle;
    }

    pub fn is_idle(&self) -> bool {
        matches!(self, Self::Idle)
    }

    pub fn set_moving(
        &mut self,
        path: WorldPath,
        opportunity_attacks: HashMap<usize, Entity>,
        action: Option<ActionDecision>,
    ) {
        debug!("Setting entity to move to goal {:?}", path.points.last());

        *self = Self::Moving {
            path,
            current_target: 0,
            opportunity_attacks,
            action,
            paused: false,
        };
    }

    pub fn set_acting(&mut self, action: &Action, phases: Vec<ActionPhase>) {
        if phases.is_empty() {
            debug!("No phases provided for action, setting to idle");
            *self = Self::Idle;
            return;
        }

        if let Self::Acting {
            phases: current_phases,
            ..
        } = self
        {
            warn!(
                "Overriding activity state for entity which is already acting: {:?}, with new phases {:?}",
                *current_phases, phases
            );
        }
        debug!("Setting entity to perform action {:?}", action);

        let timeline = if let Some(timeline) = &action.timeline {
            timeline
        } else {
            &ActionTimeline {
                total_duration: 0.0,
                submit_time: 0.0,
                step_spacing: 0.0,
            }
        };

        *self = Self::Acting {
            timeline: timeline.clone(),
            elapsed_time: 0.0,
            phases: phases.into(),
            phase_cooldown: 0.0,
            paused: false,
        };
    }

    pub fn is_acting(&self) -> bool {
        matches!(self, Self::Acting { .. })
    }

    pub fn resume(&mut self) {
        if let Self::Moving { paused, .. } | Self::Acting { paused, .. } = self {
            debug!("Resuming activity");
            *paused = false;
        } else {
            warn!("Attempted to resume an activity that is not currently moving or acting");
        }
    }

    pub fn pause(&mut self) {
        if let Self::Moving { paused, .. } | Self::Acting { paused, .. } = self {
            debug!("Pausing activity");
            *paused = true;
        } else {
            warn!("Attempted to pause an activity that is not currently moving or acting");
        }
    }

    pub fn is_paused(&self) -> bool {
        match self {
            Self::Moving { paused, .. } | Self::Acting { paused, .. } => *paused,
            Self::Idle => false,
        }
    }
}

impl Default for ActivityState {
    fn default() -> Self {
        Self::Idle
    }
}

// TODO: Name?
pub enum ActivityGameStateCommand {
    ProcessEvent(Event),
    SubmitAction(ActionDecision),
    PerformActionPhase { entity: Entity, phase: ActionPhase },
    ActivityCompleted { entity: Entity },
    DespawnEntity { entity: Entity },
}

// TODO: If the implementaiton of this is in the GameState it can access all the
// private methods without having to make them pub(crate)
impl ActivityGameStateCommand {
    pub fn execute(self, game_state: &mut GameState) {
        match self {
            Self::ProcessEvent(event) => game_state.process_event(event),

            Self::SubmitAction(action) => game_state
                .submit_decision(action)
                .expect("Failed to submit action from activity"),

            Self::PerformActionPhase { entity, mut phase } => {
                phase.perform(game_state);
                if !phase.is_applied() {
                    let scope = game_state.scope_for_entity(entity);
                    game_state
                        .interaction_engine
                        .session_mut(scope)
                        .set_pending_phase(entity, phase);
                }
            }

            Self::ActivityCompleted { entity } => {
                let scope = game_state.scope_for_entity(entity);
                game_state
                    .interaction_engine
                    .session_mut(scope)
                    .remove_pending_reactor(entity);
                game_state.resume_pending_events_if_ready(scope);
            }

            Self::DespawnEntity { entity } => {
                game_state
                    .world
                    .despawn(entity)
                    .expect("DespawnEntity: entity not found");
            }
        }
    }
}
