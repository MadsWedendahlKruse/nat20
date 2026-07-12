use hecs::Entity;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    engine::encounter::EncounterId,
    registry::serialize::{effect::TimeDurationDefinition, schema::impl_schema_via},
};

pub const TURN_DURATION_SECONDS: f32 = 6.0;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum TimeMode {
    RealTime,
    TurnBased { encounter_id: Option<EncounterId> },
    Paused,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum TurnBoundary {
    Start,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TimeStep {
    // TODO: uom time?
    RealTime {
        delta_seconds: f32,
    },
    TurnBoundary {
        entity: Entity,
        boundary: TurnBoundary,
    },
}

impl_schema_via!(TimeDuration, TimeDurationDefinition);

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(from = "TimeDurationDefinition")]
pub struct TimeDuration {
    seconds: f32,
}

impl TimeDuration {
    pub fn from_seconds(seconds: f32) -> Self {
        Self { seconds }
    }

    pub fn from_turns(turns: u32) -> Self {
        Self {
            seconds: turns as f32 * TURN_DURATION_SECONDS,
        }
    }

    pub fn as_seconds(&self) -> f32 {
        self.seconds
    }

    pub fn as_turns(&self) -> u32 {
        (self.seconds / TURN_DURATION_SECONDS).ceil() as u32
    }

    pub fn decrement(&mut self, step: &TimeStep) {
        match step {
            TimeStep::RealTime { delta_seconds } => {
                self.seconds -= *delta_seconds;
            }
            TimeStep::TurnBoundary { .. } => {
                self.seconds -= TURN_DURATION_SECONDS;
            }
        }
        if self.seconds < 0.0 {
            self.seconds = 0.0;
        }
    }
}

#[derive(Debug, Clone)]
pub struct EntityClock {
    mode: TimeMode,
    local_time_seconds: f32,
}

impl EntityClock {
    pub fn new() -> Self {
        Self {
            mode: TimeMode::RealTime,
            local_time_seconds: 0.0,
        }
    }

    pub fn mode(&self) -> TimeMode {
        self.mode
    }

    pub fn set_mode(&mut self, mode: TimeMode) {
        self.mode = mode;
    }

    pub fn local_time_seconds(&self) -> f32 {
        self.local_time_seconds
    }

    pub fn update(&mut self, time_step: TimeStep) {
        match (self.mode, time_step) {
            (TimeMode::RealTime, TimeStep::RealTime { delta_seconds }) => {
                self.local_time_seconds += delta_seconds;
            }
            (TimeMode::TurnBased { .. }, TimeStep::TurnBoundary { boundary, .. }) => {
                // TODO: For now we only increment time on turn start. I'm not
                // sure if this will have unintended consequences.
                if boundary == TurnBoundary::Start {
                    self.local_time_seconds += TURN_DURATION_SECONDS;
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use hecs::World;

    use super::*;

    #[test]
    fn time_duration_as_seconds() {
        assert_eq!(TimeDuration::from_seconds(2.5).as_seconds(), 2.5);
        assert_eq!(TimeDuration::from_turns(3).as_seconds(), 18.0);
    }

    #[test]
    fn time_duration_as_turns_ceiling() {
        assert_eq!(TimeDuration::from_seconds(0.0).as_turns(), 0);
        assert_eq!(TimeDuration::from_seconds(5.9).as_turns(), 1);
        assert_eq!(TimeDuration::from_seconds(6.0).as_turns(), 1);
        assert_eq!(TimeDuration::from_seconds(11.9).as_turns(), 2);
        assert_eq!(TimeDuration::from_seconds(12.0).as_turns(), 2);
        assert_eq!(TimeDuration::from_turns(7).as_turns(), 7);
    }

    #[test]
    fn time_duration_decrement_real_time_clamps_to_zero() {
        let mut duration = TimeDuration::from_seconds(1.0);
        duration.decrement(&TimeStep::RealTime {
            delta_seconds: 0.25,
        });

        duration.decrement(&TimeStep::RealTime {
            delta_seconds: 10.0,
        });
    }

    #[test]
    fn time_duration_decrement() {
        let mut world = World::new();
        let entity = world.spawn(());

        let mut duration = TimeDuration::from_seconds(20.0);
        duration.decrement(&TimeStep::RealTime { delta_seconds: 5.0 });
        assert_eq!(duration.as_seconds(), 15.0);

        duration.decrement(&TimeStep::TurnBoundary {
            entity,
            boundary: TurnBoundary::Start,
        });
        assert_eq!(duration.as_seconds(), 9.0);

        duration.decrement(&TimeStep::TurnBoundary {
            entity,
            boundary: TurnBoundary::End,
        });
        assert_eq!(duration.as_seconds(), 3.0);

        duration.decrement(&TimeStep::TurnBoundary {
            entity,
            boundary: TurnBoundary::Start,
        });
        assert_eq!(duration.as_seconds(), 0.0);
    }

    #[test]
    fn entity_clock_updates_only_in_its_mode() {
        let mut world = World::new();
        let entity = world.spawn(());

        let mut clock = EntityClock::new();
        assert_eq!(clock.mode(), TimeMode::RealTime);
        assert_eq!(clock.local_time_seconds(), 0.0);

        clock.update(TimeStep::RealTime { delta_seconds: 1.0 });
        assert_eq!(clock.local_time_seconds(), 1.0);

        // Turn boundaries are ignored in real-time mode.
        clock.update(TimeStep::TurnBoundary {
            entity,
            boundary: TurnBoundary::Start,
        });
        assert_eq!(clock.local_time_seconds(), 1.0);

        clock.set_mode(TimeMode::TurnBased { encounter_id: None });

        // Real-time steps are ignored in turn-based mode.
        clock.update(TimeStep::RealTime {
            delta_seconds: 999.0,
        });
        assert_eq!(clock.local_time_seconds(), 1.0);

        // Only turn start increments time.
        clock.update(TimeStep::TurnBoundary {
            entity,
            boundary: TurnBoundary::End,
        });
        assert_eq!(clock.local_time_seconds(), 1.0);

        clock.update(TimeStep::TurnBoundary {
            entity,
            boundary: TurnBoundary::Start,
        });
        assert_eq!(clock.local_time_seconds(), 1.0 + TURN_DURATION_SECONDS);
    }
}
