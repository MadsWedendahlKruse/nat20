use std::collections::HashSet;

use hecs::Entity;
use nat20_core::{
    components::id::Name,
    engine::{
        encounter::{Encounter, EncounterId},
        engine_state::EngineState,
    },
    systems,
};

use crate::{
    render::ui::{
        entities::CreatureRenderMode,
        utils::{
            ImguiRenderable, ImguiRenderableMutWithContext, ImguiRenderableWithContext,
            SELECTED_BUTTON_COLOR, render_button_disabled_conditionally, render_button_selectable,
        },
    },
    table_with_columns,
};

pub struct EncounterWindow {
    id: EncounterId,
    participants: HashSet<Entity>,
}

impl EncounterWindow {
    pub fn new() -> Self {
        Self {
            id: EncounterId::new_v4(),
            participants: HashSet::new(),
        }
    }
}

impl ImguiRenderableMutWithContext<(&mut EngineState, &mut Option<EncounterId>)>
    for EncounterWindow
{
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        (engine_state, encounter_id): (&mut EngineState, &mut Option<EncounterId>),
    ) {
        ui.separator_with_text("Encounter creation");
        ui.text("Select participants:");

        engine_state
            .world
            .query::<&Name>()
            .into_iter()
            .for_each(|(entity, name)| {
                let is_selected = self.participants.contains(&entity);
                if render_button_selectable(
                    ui,
                    format!("{}##{:?}", name.as_str(), entity),
                    [100.0, 20.0],
                    is_selected,
                ) {
                    if is_selected {
                        self.participants.remove(&entity);
                    } else {
                        self.participants.insert(entity);
                    }
                }
            });

        ui.separator();
        if render_button_disabled_conditionally(
            ui,
            "Start Encounter",
            [0.0, 0.0],
            self.participants.len() < 2,
            "You must have at least two participants to start an encounter.",
        ) {
            systems::combat::start_encounter_with_id(
                engine_state,
                self.participants.clone(),
                self.id,
            );
            ui.close_current_popup();
            encounter_id.replace(self.id);
        }
    }
}

impl ImguiRenderableWithContext<&EngineState> for Encounter {
    fn render_with_context(&self, ui: &imgui::Ui, engine_state: &EngineState) {
        ui.separator_with_text("Participants");

        let initiative_order = self.initiative_order();
        let current_entity = self.current_entity();

        if let Some(table) = table_with_columns!(ui, "Initiative Order", "", "Participant",) {
            for (entity, initiative) in initiative_order {
                if engine_state.world.get::<&Name>(*entity).is_ok() {
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
