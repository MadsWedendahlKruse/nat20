use hecs::Entity;
use nat20_core::{
    engine::game_state::GameState,
    systems::{self, geometry::LineOfSightResult},
};
use parry3d::query::Ray;

use crate::{
    render::common::{colors::Color, utils::RenderableMutWithContext},
    state::{self, gui_state::GuiState},
    windows::anchor::{self, AUTO_RESIZE},
};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum LineOfSightMode {
    Entity(Option<Entity>),
    Point([f32; 3]),
}

pub struct LineOfSightDebugWindow {
    pub from: LineOfSightMode,
    pub to: LineOfSightMode,
    pub result: Option<LineOfSightResult>,
    pub show_raycast: bool,
}

impl LineOfSightDebugWindow {
    pub fn new() -> Self {
        Self {
            from: LineOfSightMode::Point([0.0, 0.0, 0.0]),
            to: LineOfSightMode::Point([0.0, 0.0, 0.0]),
            result: None,
            show_raycast: true,
        }
    }
}

impl RenderableMutWithContext<&mut GameState> for LineOfSightDebugWindow {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let mut los_debug_open = *gui_state
            .settings
            .get::<bool>(state::parameters::RENDER_LINE_OF_SIGHT_DEBUG);

        if !los_debug_open {
            return;
        }

        gui_state.window_manager.render_window(
            ui,
            "Line of Sight Debug",
            &anchor::CENTER_RIGHT,
            AUTO_RESIZE,
            &mut los_debug_open,
            || {
                ui.separator_with_text("From");

                let width_token = ui.push_item_width(200.0);

                match &mut self.from {
                    LineOfSightMode::Entity(entity_option) => {
                        // TODO
                    }
                    LineOfSightMode::Point(point) => {
                        ui.input_float3("Point##From", point).build();
                    }
                }

                ui.separator_with_text("To");

                match &mut self.to {
                    LineOfSightMode::Entity(entity_option) => {
                        // TODO
                    }
                    LineOfSightMode::Point(point) => {
                        ui.input_float3("Point##To", point).build();
                    }
                }

                width_token.end();

                if ui.button("Compute Line of Sight") {
                    match (&self.from, &self.to) {
                        (
                            LineOfSightMode::Entity(from_entity_option),
                            LineOfSightMode::Entity(to_entity_option),
                        ) => {
                            if let (Some(from_entity), Some(to_entity)) =
                                (from_entity_option, to_entity_option)
                            {
                                self.result = Some(systems::geometry::line_of_sight_entity_entity(
                                    &game_state.world,
                                    &game_state.geometry,
                                    *from_entity,
                                    *to_entity,
                                ));
                            }
                        }

                        (LineOfSightMode::Entity(entity), LineOfSightMode::Point(_)) => todo!(),

                        (LineOfSightMode::Point(_), LineOfSightMode::Entity(entity)) => todo!(),

                        (LineOfSightMode::Point(_), LineOfSightMode::Point(_)) => {
                            if let (
                                LineOfSightMode::Point(from_point),
                                LineOfSightMode::Point(to_point),
                            ) = (self.from.clone(), self.to.clone())
                            {
                                self.result = Some(systems::geometry::line_of_sight_point_point(
                                    &game_state.world,
                                    &game_state.geometry,
                                    from_point.into(),
                                    to_point.into(),
                                    &systems::geometry::RaycastFilter::All,
                                ));
                            }
                        }
                    }
                }

                ui.checkbox("Show Raycast", &mut self.show_raycast);

                if let Some(result) = &self.result {
                    ui.text(format!("{:#?}", result));

                    if self.show_raycast
                        && let Some(raycast_result) = &result.raycast_result
                    {
                        let ray = &raycast_result.ray;
                        let end_point = if let Some(closest) = raycast_result.closest() {
                            ray.point_at(closest.toi)
                        } else {
                            ray.point_at(f32::MAX)
                        };
                        gui_state.line_renderer.add_line(
                            ray.origin.into(),
                            end_point.into(),
                            Color::White,
                        );
                    }
                }
            },
        );
    }
}
