use serde::{Deserialize, Serialize};

use crate::components::d20::{D20CheckOutcome, D20CheckResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LifeState {
    // TODO: Conscious instead of 'Normal'?
    Normal,
    Unconscious(DeathSavingThrows), // at 0 HP and making saves
    Stable,                         // at 0 HP but not making saves
    Dead,                           // dead but still an entity (corpse)
    Defeated,                       // Same as 'Dead' but for players
}

impl LifeState {
    pub fn unconscious() -> Self {
        Self::Unconscious(DeathSavingThrows::new())
    }
}

pub static DEATH_SAVING_THROW_DC: u8 = 10;
pub static DEATH_SAVING_THROW_SUCCESS_THRESHOLD: u8 = 3;
pub static DEATH_SAVING_THROW_FAILURE_THRESHOLD: u8 = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct DeathSavingThrows {
    successes: u8,
    failures: u8,
}

impl DeathSavingThrows {
    pub fn new() -> Self {
        Self {
            successes: 0,
            failures: 0,
        }
    }

    pub fn successes(&self) -> u8 {
        self.successes
    }

    pub fn failures(&self) -> u8 {
        self.failures
    }

    pub fn record_success(&mut self, count: u8) {
        self.successes = (self.successes + count).min(DEATH_SAVING_THROW_SUCCESS_THRESHOLD);
    }

    pub fn record_failure(&mut self, count: u8) {
        self.failures = (self.failures + count).min(DEATH_SAVING_THROW_FAILURE_THRESHOLD);
    }

    pub fn is_defeated(&self) -> bool {
        self.failures >= DEATH_SAVING_THROW_FAILURE_THRESHOLD
    }

    pub fn is_stable(&self) -> bool {
        self.successes >= DEATH_SAVING_THROW_SUCCESS_THRESHOLD
    }

    pub fn reset(&mut self) {
        self.successes = 0;
        self.failures = 0;
    }

    pub fn update(&mut self, check_result: &D20CheckResult) {
        match check_result.outcome {
            Some(D20CheckOutcome::CriticalSuccess) => {
                self.record_success(2);
                return;
            }
            Some(D20CheckOutcome::CriticalFailure) => {
                self.record_failure(2);
                return;
            }
            Some(D20CheckOutcome::Success) => {
                self.record_success(1);
            }
            _ => {
                self.record_failure(1);
            }
        }
    }

    pub fn next_state(&self) -> LifeState {
        if self.is_defeated() {
            LifeState::Defeated
        } else if self.is_stable() {
            LifeState::Stable
        } else {
            LifeState::Unconscious(*self)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn death_saving_throws_new() {
        let dst = DeathSavingThrows::new();
        assert_eq!(dst.successes(), 0);
        assert_eq!(dst.failures(), 0);
    }

    #[test]
    fn record_success_and_failure() {
        let mut dst = DeathSavingThrows::new();
        dst.record_success(1);
        assert_eq!(dst.successes(), 1);
        dst.record_success(2);
        assert_eq!(dst.successes(), DEATH_SAVING_THROW_SUCCESS_THRESHOLD);

        dst.record_failure(1);
        assert_eq!(dst.failures(), 1);
        dst.record_failure(3);
        assert_eq!(dst.failures(), DEATH_SAVING_THROW_FAILURE_THRESHOLD);
    }

    #[test]
    fn is_dead_and_is_stable() {
        let mut dst = DeathSavingThrows::new();
        assert!(!dst.is_defeated());
        assert!(!dst.is_stable());

        dst.record_failure(DEATH_SAVING_THROW_FAILURE_THRESHOLD);
        assert!(dst.is_defeated());

        dst.reset();
        dst.record_success(DEATH_SAVING_THROW_SUCCESS_THRESHOLD);
        assert!(dst.is_stable());
    }

    #[test]
    fn reset() {
        let mut dst = DeathSavingThrows::new();
        dst.record_success(2);
        dst.record_failure(2);
        dst.reset();
        assert_eq!(dst.successes(), 0);
        assert_eq!(dst.failures(), 0);
    }
}
