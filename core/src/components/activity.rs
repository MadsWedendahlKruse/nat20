use std::collections::HashMap;

use hecs::Entity;
use parry3d::na::Point3;
use tracing::{debug, warn};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{actions::action::ActionTimelineEvent, speed::Speed},
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
        action: Option<ActionDecision>,
        total_duration: f32,
        elapsed_time: f32,
        events: Vec<(f32, ActionTimelineEvent)>,
        paused: bool,
    },
}

impl ActivityState {
    pub fn update(
        &mut self,
        game_state: &mut GameState,
        entity: Entity,
        delta_time: f32,
    ) -> Vec<Event> {
        let mut events = Vec::new();

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
                    return events;
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
                        events.push(Event::new(EventKind::MovingOutOfReach {
                            mover: entity,
                            entity: attacker,
                            continue_movement: true,
                        }));
                        *paused = true;
                    }

                    *current_target += 1;

                    if *current_target >= path.points.len() {
                        // Reached the end of the path
                        debug!("Entity {:?} reached destination {:?}", entity, target_point);

                        if let Some(action_decision) = action.take() {
                            self.set_acting(action_decision);
                        } else {
                            *self = Self::Idle;
                        }
                    }
                }
            }

            Self::Acting {
                action,
                total_duration,
                elapsed_time,
                events: timeline_events,
                paused,
            } => {
                if *paused {
                    return events;
                }

                if *total_duration == 0.0 {
                    // Instant action, execute immediately
                    if let Some(action) = action.take() {
                        game_state.submit_decision(action);
                    }
                    *self = Self::Idle;
                    return events;
                }

                *elapsed_time += delta_time;

                timeline_events.retain(|(event_time, event)| {
                    if *event_time <= *elapsed_time {
                        match event {
                            ActionTimelineEvent::SubmitAction => {
                                if let Some(action) = action.take() {
                                    game_state.submit_decision(action);
                                } else {
                                    warn!("Timeline event triggered {:?} but no action decision found", event);
                                }
                            }
                            ActionTimelineEvent::SpawnProjectile {} => todo!(),
                        }
                        false
                    } else {
                        true
                    }
                });

                if *elapsed_time >= *total_duration {
                    *self = Self::Idle;
                }
            }
        }

        return events;
    }

    pub fn set_idle(&mut self) {
        debug!("Setting entity to idle");
        *self = Self::Idle;
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

    pub fn set_acting(&mut self, action: ActionDecision) {
        debug!("Setting entity to perform action {:?}", action);

        let (total_duration, events) = if let Some(action_id) = action.action_id()
            && let Some(action) = systems::actions::get_action(&action_id)
            && let Some(timeline) = &action.timeline
        {
            (timeline.total_duration, timeline.events.clone())
        } else {
            (0.0, Vec::new())
        };

        *self = Self::Acting {
            action: Some(action),
            total_duration,
            elapsed_time: 0.0,
            events,
            paused: false,
        };
    }

    pub fn resume(&mut self) {
        if let Self::Moving { paused, .. } | Self::Acting { paused, .. } = self {
            *paused = false;
        }
    }
}

impl Default for ActivityState {
    fn default() -> Self {
        Self::Idle
    }
}
