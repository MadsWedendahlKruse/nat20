use std::collections::HashSet;

use hecs::Entity;
use nat20_core::{
    components::id::Name,
    engine::{
        encounter::{Encounter, EncounterId},
        engine_state::EngineState,
    },
};

use crate::{
    render::{
        common::utils::RenderableMutWithContext,
        ui::{
            entities::CreatureRenderMode,
            utils::{
                ImguiRenderable, ImguiRenderableWithContext, SELECTED_BUTTON_COLOR,
                render_button_disabled_conditionally, render_button_selectable,
            },
        },
    },
    state::gui_state::GuiState,
    table_with_columns,
    windows::anchor::{self, AUTO_RESIZE, WindowManager},
};

enum EncounterWindowState {
    EncounterCreation { participants: HashSet<Entity> },
    EncounterRunning,
    EncounterFinished,
}

pub struct EncounterWindow {
    state: EncounterWindowState,
    id: EncounterId,
}

impl EncounterWindow {
    pub fn new() -> Self {
        Self {
            state: EncounterWindowState::EncounterCreation {
                participants: HashSet::new(),
            },
            id: EncounterId::new_v4(),
        }
    }

    pub fn id(&self) -> &EncounterId {
        &self.id
    }

    pub fn finished(&self) -> bool {
        matches!(self.state, EncounterWindowState::EncounterFinished)
    }
}

impl RenderableMutWithContext<&mut EngineState> for EncounterWindow {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        engine_state: &mut EngineState,
    ) {
        // raw pointer sidesteps borrow checker temporarily
        let window_manager_ptr =
            unsafe { &mut *(&mut gui_state.window_manager as *mut WindowManager) };

        window_manager_ptr.render_window(
            ui,
            &format!("Encounter##{:?}", self.id),
            &anchor::CENTER_LEFT,
            AUTO_RESIZE,
            &mut true,
            || {
                match &mut self.state {
                    EncounterWindowState::EncounterCreation { participants } => {
                        ui.separator_with_text("Encounter creation");
                        ui.text("Select participants:");

                        engine_state
                            .world
                            .query::<&Name>()
                            .into_iter()
                            .for_each(|(entity, name)| {
                                let is_selected = participants.contains(&entity);
                                if render_button_selectable(
                                    ui,
                                    format!("{}##{:?}", name.as_str(), entity),
                                    [100.0, 20.0],
                                    is_selected,
                                ) {
                                    if is_selected {
                                        participants.remove(&entity);
                                    } else {
                                        participants.insert(entity);
                                    }
                                }
                            });

                        ui.separator();
                        if render_button_disabled_conditionally(
                            ui,
                            "Start Encounter",
                            [0.0, 0.0],
                            participants.len() < 2,
                            "You must have at least two participants to start an encounter.",
                        ) {
                            engine_state.start_encounter_with_id(participants.clone(), self.id);
                            self.state = EncounterWindowState::EncounterRunning;
                        }
                    }

                    EncounterWindowState::EncounterRunning => {
                        // First borrow: get the encounter
                        let encounter_ptr = engine_state
                            .encounters
                            .get_mut(&self.id)
                            .map(|enc| enc as *mut Encounter); // raw pointer sidesteps borrow checker temporarily

                        if let Some(encounter_ptr) = encounter_ptr {
                            // SAFETY: we know no other mutable borrow of the encounter exists at this point
                            let encounter = unsafe { &mut *encounter_ptr };

                            encounter.render_mut_with_context(ui, gui_state, engine_state);
                        } else {
                            ui.text("Encounter not found!");
                        }

                        ui.separator();
                        if ui.button("End Encounter") {
                            self.state = EncounterWindowState::EncounterFinished;
                            engine_state.end_encounter(&self.id);
                        }
                    }

                    EncounterWindowState::EncounterFinished => {
                        ui.text("Encounter finished!");
                    }
                }
            },
        );
    }
}

impl RenderableMutWithContext<&mut EngineState> for Encounter {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        _gui_state: &mut GuiState,
        engine_state: &mut EngineState,
    ) {
        ui.separator_with_text("Participants");

        let initiative_order = self.initiative_order();
        let current_entity = self.current_entity();

        if let Some(table) = table_with_columns!(ui, "Initiative Order", "", "Participant",) {
            for (entity, initiative) in initiative_order {
                if engine_state.world.query_one_mut::<&Name>(*entity).is_ok() {
                    // Initiative column
                    ui.table_next_column();
                    ui.text(initiative.total().to_string());
                    if ui.is_item_hovered() {
                        ui.tooltip(|| {
                            ui.separator_with_text("Initiative");
                            initiative.render(ui);
                        });
                    }

                    if current_entity == *entity {
                        ui.table_set_bg_color(imgui::TableBgTarget::all(), SELECTED_BUTTON_COLOR);
                    }

                    // Participant column
                    ui.table_next_column();
                    entity.render_with_context(ui, (engine_state, &CreatureRenderMode::Compact));
                }
            }

            table.end();
        }

        ui.separator();
        ui.text(format!("Round: {}", self.round()));
    }
}
