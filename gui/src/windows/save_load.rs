use nat20_core::{
    engine::engine_state::EngineState,
    persistence::{
        self,
        engine::{EngineLoadBlocker, SaveFileSource},
        registry::{LoadPolicy, RegistryLoadBlocker},
    },
};

const SAVE_PATH: &str = "save_file.json";
const BUTTON_SIZE: [f32; 2] = [100.0, 30.0];

use crate::render::ui::utils::{
    ImguiRenderableMutWithContext, render_button_disabled_conditionally,
};

pub enum SaveLoadState {
    SaveLoad,
    LoadFailed { blocker: EngineLoadBlocker },
}

pub struct SaveLoadWindow {
    state: SaveLoadState,
    save_path: String,
    load_policy: LoadPolicy,
}

impl SaveLoadWindow {
    pub fn new() -> Self {
        Self {
            state: SaveLoadState::SaveLoad,
            save_path: SAVE_PATH.to_string(),
            load_policy: LoadPolicy::Strict,
        }
    }

    fn load(&mut self, source: SaveFileSource, policy: LoadPolicy, engine_state: &mut EngineState) {
        match persistence::engine::load_engine_state(source, policy) {
            Ok(new_engine_state) => {
                *engine_state = new_engine_state;
                self.state = SaveLoadState::SaveLoad;
            }
            Err(blocker) => self.state = SaveLoadState::LoadFailed { blocker },
        }
    }
}

impl ImguiRenderableMutWithContext<&mut EngineState> for SaveLoadWindow {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, engine_state: &mut EngineState) {
        ui.separator_with_text("Save & Load");

        let width_token = ui.push_item_width(150.0);

        ui.input_text("Save Path", &mut self.save_path)
            .enter_returns_true(true)
            .build();

        let mut current_policy = match self.load_policy {
            LoadPolicy::Strict => 0,
            LoadPolicy::DropDroppable => 1,
        };

        if ui.combo(
            "Load Policy",
            &mut current_policy,
            &[LoadPolicy::Strict, LoadPolicy::DropDroppable],
            |policy| match policy {
                LoadPolicy::Strict => "Strict".into(),
                LoadPolicy::DropDroppable => "Drop Droppable".into(),
            },
        ) {
            self.load_policy = match current_policy {
                0 => LoadPolicy::Strict,
                1 => LoadPolicy::DropDroppable,
                _ => LoadPolicy::Strict,
            };
        }

        width_token.end();

        ui.separator();

        match &mut self.state {
            SaveLoadState::SaveLoad => {
                let can_save = persistence::engine::can_save(engine_state).err();

                if render_button_disabled_conditionally(
                    ui,
                    "Save",
                    BUTTON_SIZE,
                    can_save.is_some(),
                    &can_save.map_or("".to_string(), |blocker| {
                        format!("Save blocked by: {:#?}", blocker)
                    }),
                ) {
                    persistence::engine::save_engine_state(engine_state, &self.save_path).unwrap();
                }

                ui.same_line();

                if ui.button_with_size("Load", BUTTON_SIZE) {
                    self.load(
                        self.save_path.as_str().into(),
                        self.load_policy,
                        engine_state,
                    );
                }
            }

            SaveLoadState::LoadFailed { blocker } => {
                ui.text(format!("Load failed: {:#?}", blocker));

                match blocker {
                    EngineLoadBlocker::Registry(registry_load_blocker) => {
                        match registry_load_blocker {
                            RegistryLoadBlocker::MissingDroppableReferences {
                                save_file, ..
                            } => {
                                ui.separator();

                                if ui.button_with_size("Load and drop", BUTTON_SIZE) {
                                    if let Some(save_file) = save_file.take() {
                                        self.load(
                                            SaveFileSource::Struct(save_file),
                                            LoadPolicy::DropDroppable,
                                            engine_state,
                                        );
                                    }
                                }

                                ui.same_line();
                            }

                            _ => {}
                        }
                    }

                    _ => {}
                }

                if ui.button_with_size("Back", BUTTON_SIZE) {
                    self.state = SaveLoadState::SaveLoad;
                }
            }
        }
    }
}
