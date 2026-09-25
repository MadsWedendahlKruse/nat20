use hecs::Entity;

use crate::{
    components::activity::Activity,
    engine::{action_prompt::ActionPrompt, engine_state::EngineState},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlayerControlledTag;

pub trait AIController: Send + Sync + 'static {
    fn decide(
        &self,
        engine_state: &mut EngineState,
        prompt: &ActionPrompt,
        actor: Entity,
    ) -> Option<Activity>;
}
