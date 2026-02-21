use std::collections::HashSet;

use hecs::Entity;
use nat20_core::{
    components::id::Name,
    engine::{
        encounter::{Encounter, EncounterId},
        game_state::GameState,
    },
    systems::{self},
};
use tracing::{info, warn};

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

impl RenderableMutWithContext<&mut GameState> for EncounterWindow {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
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

                        game_state
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
                            game_state.start_encounter_with_id(participants.clone(), self.id);
                            self.state = EncounterWindowState::EncounterRunning;
                        }
                    }

                    EncounterWindowState::EncounterRunning => {
                        // First borrow: get the encounter
                        let encounter_ptr = game_state
                            .encounters
                            .get_mut(&self.id)
                            .map(|enc| enc as *mut Encounter); // raw pointer sidesteps borrow checker temporarily

                        if let Some(encounter_ptr) = encounter_ptr {
                            // SAFETY: we know no other mutable borrow of the encounter exists at this point
                            let encounter = unsafe { &mut *encounter_ptr };

                            encounter.render_mut_with_context(ui, gui_state, game_state);
                        } else {
                            ui.text("Encounter not found!");
                        }

                        ui.separator();
                        if ui.button("End Encounter") {
                            self.state = EncounterWindowState::EncounterFinished;
                            game_state.end_encounter(&self.id);
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

impl RenderableMutWithContext<&mut GameState> for Encounter {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        ui.separator_with_text("Participants");

        let initiative_order = self.initiative_order();
        let current_entity = self.current_entity();

        if let Some(table) = table_with_columns!(ui, "Initiative Order", "", "Participant",) {
            for (entity, initiative) in initiative_order {
                if let Ok(_) = &game_state.world.query_one_mut::<&Name>(*entity) {
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
                    entity
                        .render_with_context(ui, (&game_state.world, &CreatureRenderMode::Compact));
                }
            }

            table.end();
        }

        ui.separator();
        ui.text(format!("Round: {}", self.round()));

        // TODO: No idea where to put this
        if !systems::ai::is_player_controlled(&game_state.world, self.current_entity())
            && let Some(prompt) = &game_state.next_promt_encounter(self.id()).cloned()
            && prompt.kind.actors().contains(&self.current_entity())
        {
            let ai_decision = systems::ai::decide_action(game_state, prompt, self.current_entity());
            if let Some(path) = ai_decision.path {
                let result = game_state
                    .submit_movement(self.current_entity(), *path.taken_path.end().unwrap());
                info!("AI movement submitted: {:?}", result);
                if let Ok(()) = result {
                    gui_state.path_cache.insert(ai_decision.actor, path);
                }
            }
            if let Some(action_decision) = ai_decision.decision {
                let result = game_state.submit_decision(action_decision);
                info!("AI decision submitted: {:?}", result);
                if result.is_err() {
                    warn!("Failed to submit AI decision, skipping turn.");
                    self.end_turn(game_state, ai_decision.actor);
                }
            } else {
                self.end_turn(game_state, self.current_entity());
            }
        }
    }
}
