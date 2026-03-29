use hecs::Entity;
use imgui::MouseButton;
use nat20_core::{
    components::activity::Activity,
    engine::game_state::GameState,
    systems::{self, geometry::RaycastHitKind, movement::PathResult},
};
use parry3d::na::Point3;
use tracing::error;

use crate::{
    render::common::{colors::Color, utils::RenderableMutWithContext},
    state::gui_state::GuiState,
};

pub struct MovementPreview {
    pub entity: Entity,
    pub prev_target_point: Option<Point3<f32>>,
    pub prev_entity_position: Option<Point3<f32>>,
    pub path_result: Option<PathResult>,
    pub opportunity_attacks: Vec<(Entity, Point3<f32>)>,
}

impl MovementPreview {
    pub fn new(entity: Entity) -> Self {
        Self {
            entity,
            prev_target_point: None,
            prev_entity_position: None,
            path_result: None,
            opportunity_attacks: Vec::new(),
        }
    }
}

impl RenderableMutWithContext<&mut GameState> for MovementPreview {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let Some(cursor_ray_result) = &gui_state.cursor_ray_result else {
            return;
        };

        let Some(closest) = cursor_ray_result.closest() else {
            return;
        };

        if matches!(closest.kind, RaycastHitKind::World) {
            if let Some(prev_target_point) = self.prev_target_point {
                let entity_position =
                    systems::geometry::get_foot_position(&game_state.world, self.entity).unwrap();
                if closest.poi != prev_target_point
                    || self.prev_entity_position != Some(entity_position)
                {
                    let in_combat = game_state.in_combat.contains_key(&self.entity);
                    if let Ok(path_result) = systems::movement::path(
                        game_state,
                        self.entity,
                        &closest.poi,
                        true,
                        false,
                        in_combat,
                        false,
                    ) {
                        self.prev_target_point = Some(closest.poi);
                        self.path_result = Some(path_result.clone());
                        self.opportunity_attacks = systems::movement::potential_opportunity_attacks(
                            &game_state.world,
                            &path_result.taken_path,
                            self.entity,
                            // TODO: Find a proper way to find the "attackers"
                            &systems::geometry::entities_in_range_of_entity(
                                &game_state.world,
                                self.entity,
                                &path_result.taken_path.length,
                            ),
                        );
                    }
                }
            } else {
                self.prev_target_point = Some(closest.poi);
            }

            for (entity, point) in &self.opportunity_attacks {
                if let Some(position) =
                    systems::geometry::get_foot_position(&game_state.world, *entity)
                {
                    let reach = (position - point).magnitude();
                    let mut reach_center: [f32; 3] = position.into();
                    reach_center[1] += 0.1;
                    gui_state
                        .line_renderer
                        .add_circle(reach_center, reach, [0.85, 0.85, 0.85]);
                    gui_state.line_renderer.add_line(
                        position.into(),
                        point.clone().into(),
                        [1.0, 0.0, 0.0],
                    );
                }
            }

            if let Some(path) = &self.path_result {
                gui_state
                    .line_renderer
                    .add_path_result(&path, Color::White, Color::Red);
            }

            if ui.is_mouse_clicked(MouseButton::Left) {
                let movement_result = game_state.submit_activity(Activity::Move {
                    entity: self.entity,
                    goal: closest.poi,
                });

                match movement_result {
                    Ok(_) => {}
                    Err(err) => {
                        error!("Failed to submit movement: {:?}", err);
                    }
                }
            }

            gui_state.cursor_ray_result.take();
        }
    }
}
