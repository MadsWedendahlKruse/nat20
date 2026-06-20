use std::collections::{HashMap, HashSet, VecDeque};

use hecs::Entity;
use parry3d::na::Point3;
use tracing::{debug, error, warn};
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
    systems::{self, geometry::Parabola, movement::MovementError},
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
    ) -> Vec<ActivityGameStateCommand> {
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

    pub fn set_moving(
        &mut self,
        path: WorldPath,
        opportunity_attacks: HashMap<usize, Entity>,
        action: Option<ActionDecision>,
    ) {
        debug!("Setting entity to move to goal {:?}", path.points.last());

        self.state = ActivityStateKind::Moving {
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
            self.set_idle();
            return;
        }

        if let ActivityStateKind::Acting {
            phases: current_phases,
            ..
        } = &self.state
        {
            warn!(
                "Overriding activity state for entity which is already acting: {:?}, with new phases {:?}",
                current_phases, phases
            );
        }
        debug!(
            "Setting entity to perform action {:?}\nWith phases:{:?}",
            action, phases
        );

        self.state = ActivityStateKind::Acting {
            timeline: action.timeline.clone(),
            elapsed_time: 0.0,
            phases: phases.into(),
            phase_cooldown: action.timeline.step_spacing,
            pause_reasons: HashSet::new(),
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
        /// Indices of points in the path that trigger opportunity attacks
        opportunity_attacks: HashMap<usize, Entity>,
        /// Potential action to be performed after reaching the destination
        action: Option<ActionDecision>,
        /// Whether the movement is currently paused due to an opportunity attack
        /// TODO: Can movement only be paused by opportunity attacks?
        paused: bool,
    },
    Acting {
        timeline: ActionTimeline,
        elapsed_time: f32,
        phases: VecDeque<ActionPhase>,
        phase_cooldown: f32,
        pause_reasons: HashSet<ActivityPauseReason>,
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
                        perform_time,
                        step_spacing,
                    },
                phases,
                phase_cooldown,
                ..
            } => {
                *elapsed_time += delta_time;

                if *elapsed_time >= *perform_time {
                    *phase_cooldown += delta_time;

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

            Self::Displaced {
                trajectory,
                elapsed_time,
            } => {
                *elapsed_time += delta_time;

                let new_position = trajectory.position_at_time(*elapsed_time);
                systems::geometry::teleport_to(&mut game_state.world, entity, &new_position);

                if *elapsed_time >= trajectory.max_time {
                    debug!(
                        "Entity {:?} finished displacement after {:?} seconds",
                        entity, trajectory.max_time
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
    ActionStepResolution,
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

            Self::SubmitAction(action) => match game_state.submit_decision(action) {
                Ok(_) => {}
                Err(error) => error!("Failed to submit action decision: {:?}", error),
            },

            Self::PerformActionPhase { entity, mut phase } => {
                phase.perform(game_state);
                if !phase.is_applied() {
                    let scope = game_state.scope_for_entity(entity);
                    game_state
                        .interaction_engine
                        .session_mut(scope)
                        .queue_phase(entity, phase, false);
                }
            }

            Self::ActivityCompleted { entity } => {
                let scope = game_state.scope_for_entity(entity);
                debug!(
                    "Activity completed for entity {:?}, clearing blockers and resuming pending events if ready",
                    entity
                );
                game_state
                    .interaction_engine
                    .session_mut(scope)
                    .clear_blocker(entity);
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
