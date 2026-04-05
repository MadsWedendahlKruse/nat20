use imgui::TreeNodeFlags;
use nat20_core::{components::activity::ActivityState, engine::game_state::GameState, systems};

use crate::{
    render::common::utils::RenderableWithContext,
    state::{self, gui_state::GuiState},
    windows::anchor,
};

pub struct GameStateDebugWindow {}

impl GameStateDebugWindow {
    pub fn new() -> Self {
        Self {}
    }
}

impl RenderableWithContext<&mut GameState> for GameStateDebugWindow {
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
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
                if ui.collapsing_header("Entities", TreeNodeFlags::empty()) {
                    let entities = game_state
                        .world
                        .iter()
                        .map(|e| e.entity())
                        .collect::<Vec<_>>();

                    for entity in entities {
                        ui.text(format!("Entity {:?}", entity));

                        let mut activity_state = systems::helpers::get_component_mut::<ActivityState>(
                            &mut game_state.world,
                            entity,
                        );

                        if activity_state.is_paused() {
                            if ui.button("Resume") {
                                activity_state.resume();
                            }
                        } else {
                            if ui.button("Pause") {
                                activity_state.pause();
                            }
                        }
                    }
                }
                if ui.collapsing_header("Interaction Engine", TreeNodeFlags::empty()) {
                    ui.text(format!("{:#?}", game_state.interaction_engine));
                }
            },
        );
    }
}
