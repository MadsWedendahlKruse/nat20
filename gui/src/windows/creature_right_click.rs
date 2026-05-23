use hecs::Entity;
use nat20_core::engine::game_state::GameState;
use strum::IntoEnumIterator;

use crate::{
    render::ui::{
        entities::CreatureRenderMode,
        utils::{
            ImguiRenderableMutWithContext, ImguiRenderableWithContext, render_uniform_buttons,
        },
    },
    windows::creature_debug::CreatureDebugWindow,
};

pub enum CreatureRightClickState {
    MainMenu,
    InspectCreature(CreatureRenderMode),
    DebugCreature(CreatureDebugWindow),
}

pub struct CreatureRightClickWindow {
    pub state: CreatureRightClickState,
    pub entity: Entity,
}

impl CreatureRightClickWindow {
    pub fn new(entity: Entity) -> Self {
        Self {
            state: CreatureRightClickState::MainMenu,
            entity,
        }
    }
}

impl ImguiRenderableMutWithContext<&mut GameState> for CreatureRightClickWindow {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, game_state: &mut GameState) {
        match &mut self.state {
            CreatureRightClickState::MainMenu => {
                if let Some(index) = render_uniform_buttons(ui, &["Inspect", "Debug"]) {
                    match index {
                        0 => {
                            self.state = CreatureRightClickState::InspectCreature(
                                CreatureRenderMode::Inspect,
                            );
                        }
                        1 => {
                            self.state = CreatureRightClickState::DebugCreature(
                                CreatureDebugWindow::new(self.entity),
                            );
                        }
                        _ => {}
                    }
                }
            }
            CreatureRightClickState::InspectCreature(render_mode) => {
                let mut current_render_mode = render_mode.clone() as usize;
                let width_token = ui.push_item_width(100.0);
                if ui.combo(
                    "Render Mode",
                    &mut current_render_mode,
                    &CreatureRenderMode::iter().collect::<Vec<_>>()[..],
                    |lvl| lvl.to_string().into(),
                ) {
                    *render_mode = current_render_mode.into();
                }
                width_token.end();

                ui.separator();

                self.entity
                    .render_with_context(ui, (&game_state, &render_mode));
            }
            CreatureRightClickState::DebugCreature(debug_gui) => {
                debug_gui.render_mut_with_context(ui, game_state);
            }
        }
    }
}
