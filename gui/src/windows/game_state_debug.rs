use imgui::TreeNodeFlags;
use nat20_core::engine::game_state::GameState;

use crate::{
    render::common::utils::RenderableWithContext,
    state::{self, gui_state::GuiState},
    windows::{anchor, game_state_debug},
};

pub struct GameStateDebugWindow {}

impl GameStateDebugWindow {
    pub fn new() -> Self {
        Self {}
    }
}

impl RenderableWithContext<&GameState> for GameStateDebugWindow {
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &GameState,
    ) {
        let mut game_state_debug_open = *gui_state
            .settings
            .get::<bool>(state::parameters::RENDER_GAME_STATE_DEBUG);

        if !game_state_debug_open {
            return;
        }

        gui_state.window_manager.render_window(
            ui,
            "Game State Debug",
            &anchor::TOP_RIGHT,
            [0.0, 500.0],
            &mut game_state_debug_open,
            || {
                if ui.collapsing_header("Interaction Engine", TreeNodeFlags::empty()) {
                    ui.text(format!("{:#?}", game_state.interaction_engine));
                }
            },
        );
    }
}
