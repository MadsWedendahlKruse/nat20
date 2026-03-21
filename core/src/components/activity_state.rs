use std::collections::HashMap;

use hecs::Entity;
use tracing::{debug, warn};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::speed::Speed,
    engine::{
        action_prompt::ActionDecision,
        event::{Event, EventKind},
        game_state::GameState,
        geometry::WorldPath,
    },
    systems::{self},
};

const MOVEMENT_SPEED: f32 = 5.0; // [m/s]

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
        action: ActionDecision,
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
                            *self = Self::Acting {
                                action: action_decision,
                            };
                        } else {
                            *self = Self::Idle;
                        }
                    }
                }
            }

            Self::Acting { action } => {
                game_state.submit_decision(action.clone());
                *self = Self::Idle;
            }
        }

        return events;
    }

    pub fn set_idle(&mut self) {
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

    pub fn resume(&mut self) {
        if let Self::Moving { paused, .. } = self {
            *paused = false;
        } else {
            warn!("Attempted to resume movement for an entity that is not moving");
        }
    }
}

impl Default for ActivityState {
    fn default() -> Self {
        Self::Idle
    }
}
