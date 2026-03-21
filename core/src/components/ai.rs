use hecs::Entity;

use crate::{
    components::activity::Activity,
    engine::{action_prompt::ActionPrompt, game_state::GameState},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlayerControlledTag;

pub trait AIController: Send + Sync + 'static {
    fn decide(
        &self,
        game_state: &mut GameState,
        prompt: &ActionPrompt,
        actor: Entity,
    ) -> Option<Activity>;
}
