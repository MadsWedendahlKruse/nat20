use hecs::Entity;

use crate::engine::{encounter::EncounterId, engine_state::EngineState};

// TODO: Not sure where this should live
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CombatState {
    #[default]
    OutOfCombat,
    InCombat(EncounterId),
}

impl CombatState {
    pub fn enter_combat(&mut self, encounter_id: EncounterId) {
        *self = CombatState::InCombat(encounter_id);
    }

    pub fn leave_combat(&mut self) {
        *self = CombatState::OutOfCombat;
    }

    pub fn is_in_combat(&self) -> bool {
        matches!(self, CombatState::InCombat(_))
    }
}

/// Used often enough to warrant a helper function
pub fn is_in_combat(engine_state: &EngineState, entity: Entity) -> bool {
    if let Ok(combat_state) = engine_state.world.get::<&CombatState>(entity) {
        combat_state.is_in_combat()
    } else {
        false
    }
}
