use core::f32;

use hecs::Entity;
use nat20_core::{
    components::{actions::targeting::LineOfSightMode, id::Name},
    engine::game_state::GameState,
    systems::{
        self,
        geometry::{LineOfSightResult, RaycastMode},
    },
};
use tracing::debug;
use uom::si::{f32::Velocity, velocity::meter_per_second};

use crate::{
    render::common::{colors::Color, utils::RenderableMutWithContext},
    state::{self, gui_state::GuiState},
    windows::anchor::{self, AUTO_RESIZE},
};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum LineOfSightTarget {
    Entity(Option<Entity>),
    Point([f32; 3]),
}

impl LineOfSightTarget {
    pub fn as_usize(&self) -> usize {
        match self {
            LineOfSightTarget::Entity(_) => 0,
            LineOfSightTarget::Point(_) => 1,
        }
    }

    pub fn default(usize: usize) -> Self {
        match usize {
            0 => LineOfSightTarget::Entity(None),
            1 => LineOfSightTarget::Point([0.0, 0.0, 0.0]),
            _ => panic!("Invalid LineOfSightMode index"),
        }
    }

    pub fn label(&self) -> String {
        match self {
            LineOfSightTarget::Entity(_) => "Entity".to_string(),
            LineOfSightTarget::Point(_) => "Point".to_string(),
        }
    }

    pub fn labels() -> Vec<String> {
        vec![
            LineOfSightTarget::Entity(None).label(),
            LineOfSightTarget::Point([0.0, 0.0, 0.0]).label(),
        ]
    }
}

pub struct LineOfSightDebugWindow {
    pub from: LineOfSightTarget,
    pub to: LineOfSightTarget,
    pub mode: LineOfSightMode,
    pub result: Option<LineOfSightResult>,
    pub show_raycast: bool,
}

impl LineOfSightDebugWindow {
    pub fn new() -> Self {
        Self {
            from: LineOfSightTarget::Point([0.0, 0.0, 0.0]),
            to: LineOfSightTarget::Point([0.0, 0.0, 0.0]),
            mode: LineOfSightMode::Ray,
            result: None,
            show_raycast: true,
        }
    }

    fn render_line_of_sight_target(
        target: &mut LineOfSightTarget,
        label: &str,
        ui: &imgui::Ui,
        all_entities: &Vec<(Entity, &Name)>,
    ) {
        let mut current_target = target.as_usize();
        if ui.combo(
            format!("Target##{}", label),
            &mut current_target,
            &LineOfSightTarget::labels(),
            |s| s.into(),
        ) {
            *target = LineOfSightTarget::default(current_target);
        }

        match target {
            LineOfSightTarget::Entity(entity_option) => {
                let mut current_entity = all_entities
                    .iter()
                    .position(|(entity, _)| Some(*entity) == *entity_option)
                    .unwrap_or(0);

                if entity_option.is_none() && !all_entities.is_empty() {
                    *entity_option = Some(all_entities[0].0);
                }

                if ui.combo(
                    format!("Entity##{}", label),
                    &mut current_entity,
                    &all_entities,
                    |(entity, name)| format!("{} ({:?})", name.to_string(), entity).into(),
                ) {
                    *entity_option = Some(all_entities[current_entity].0);
                }
            }
            LineOfSightTarget::Point(point) => {
                ui.input_float3(format!("Point##{}", label), point).build();
            }
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

                let mut query = game_state.world.query::<&Name>();
                let all_entities = query.iter().collect::<Vec<_>>();

                Self::render_line_of_sight_target(&mut self.from, "From", ui, &all_entities);

                ui.separator_with_text("To");

                Self::render_line_of_sight_target(&mut self.to, "To", ui, &all_entities);

                width_token.end();

                ui.separator_with_text("Mode");

                let mut current_mode = match self.mode {
                    LineOfSightMode::Ray => 0usize,
                    LineOfSightMode::Parabola { .. } => 1usize,
                    _ => panic!("Unsupported LineOfSightMode"),
                };

                if ui.combo(
                    "Mode##LineOfSightMode",
                    &mut current_mode,
                    &["Ray".to_string(), "Parabola".to_string()],
                    |s| s.into(),
                ) {
                    self.mode = match current_mode {
                        0 => LineOfSightMode::Ray,
                        1 => LineOfSightMode::Parabola {
                            launch_velocity: Velocity::new::<meter_per_second>(10.0),
                        },
                        _ => panic!("Invalid LineOfSightMode index"),
                    };
                }

                match &mut self.mode {
                    LineOfSightMode::Parabola { launch_velocity } => {
                        let mut velocity = launch_velocity.get::<meter_per_second>();
                        if ui
                            .input_float("Launch Velocity##Parabola", &mut velocity)
                            .build()
                        {
                            *launch_velocity = Velocity::new::<meter_per_second>(velocity);
                        }
                    }
                    _ => {}
                }

                if ui.button("Compute Line of Sight") {
                    match (&self.from, &self.to) {
                        (
                            LineOfSightTarget::Entity(from_entity_option),
                            LineOfSightTarget::Entity(to_entity_option),
                        ) => {
                            if let (Some(from_entity), Some(to_entity)) =
                                (from_entity_option, to_entity_option)
                            {
                                self.result = Some(systems::geometry::line_of_sight_entity_entity(
                                    &game_state.world,
                                    &game_state.geometry,
                                    *from_entity,
                                    *to_entity,
                                    &self.mode,
                                ));
                            }
                        }

                        (LineOfSightTarget::Entity(entity), LineOfSightTarget::Point(to_point)) => {
                            if let Some(entity) = entity {
                                self.result = Some(systems::geometry::line_of_sight_entity_point(
                                    &game_state.world,
                                    &game_state.geometry,
                                    *entity,
                                    to_point.clone().into(),
                                    &self.mode,
                                ));
                            }
                        }

                        (LineOfSightTarget::Point(_), LineOfSightTarget::Entity(entity)) => todo!(),

                        (
                            LineOfSightTarget::Point(from_point),
                            LineOfSightTarget::Point(to_point),
                        ) => {
                            self.result = Some(systems::geometry::line_of_sight_point_point(
                                &game_state.world,
                                &game_state.geometry,
                                from_point.clone().into(),
                                to_point.clone().into(),
                                &self.mode,
                                &systems::geometry::RaycastFilter::All,
                            ));
                        }
                    }
                }

                ui.checkbox("Show Raycast", &mut self.show_raycast);

                if let Some(result) = &self.result {
                    ui.text(format!("{:#?}", result));

                    let color = if result.has_line_of_sight {
                        Color::White
                    } else {
                        Color::Red
                    };

                    if self.show_raycast
                        && let Some(raycast_result) = &result.raycast_result
                    {
                        match &raycast_result.mode {
                            RaycastMode::Ray(ray) => {
                                let toi = raycast_result
                                    .closest()
                                    .map(|hit| hit.toi)
                                    .unwrap_or(f32::MAX);

                                gui_state.line_renderer.add_ray(
                                    ray.origin.into(),
                                    ray.dir.into(),
                                    toi,
                                    color,
                                );
                            }
                            RaycastMode::Parabola(parabola) => {
                                gui_state.line_renderer.add_parabola(parabola, color);
                            }
                        }
                    }
                }
            },
        );
    }
}
