use hecs::Entity;

use crate::{
    engine::{
        action_prompt::{ActionDecision, ActionPrompt},
        game_state::GameState,
    },
    systems::movement::PathResult,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlayerControlledTag;

pub struct AIDecision {
    pub actor: Entity,
    pub decision: Option<ActionDecision>,
    pub path: Option<PathResult>,
}

impl AIDecision {
    pub fn empty(actor: Entity) -> Self {
        Self {
            actor,
            decision: None,
            path: None,
        }
    }
}

pub trait AIController: Send + Sync + 'static {
    fn decide(
        &self,
        game_state: &mut GameState,
        prompt: &ActionPrompt,
        actor: Entity,
    ) -> AIDecision;
}
