use hecs::Entity;
use nat20_core::{
    engine::game_state::GameState,
    systems::{self, movement::PathResult},
};
use parry3d::na::Point3;

use crate::{
    render::common::{colors::Color, utils::RenderableWithContext},
    state::gui_state::GuiState,
};

pub struct MovementPreview {
    pub entity: Entity,
    pub prev_goal: Option<Point3<f32>>,
    pub prev_entity_position: Option<Point3<f32>>,
    pub path_result: Option<PathResult>,
    pub opportunity_attacks: Vec<(Entity, Point3<f32>)>,
}

impl MovementPreview {
    pub fn new(entity: Entity) -> Self {
        Self {
            entity,
            prev_goal: None,
            prev_entity_position: None,
            path_result: None,
            opportunity_attacks: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.prev_goal = None;
        self.prev_entity_position = None;
        self.path_result = None;
        self.opportunity_attacks.clear();
    }

    pub fn update(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
        goal: Option<Point3<f32>>,
    ) {
        if ui.io().want_capture_mouse {
            self.clear();
            return;
        }

        let Some(cursor_ray_result) = &gui_state.cursor_ray_result else {
            return;
        };

        let Some(closest) = cursor_ray_result.closest() else {
            return;
        };

        let goal = if let Some(goal) = goal {
            goal
        } else {
            closest.poi
        };

        if let Some(prev_goal) = self.prev_goal {
            let entity_position =
                systems::geometry::get_foot_position(&game_state.world, self.entity).unwrap();
            // If the goal moved or the entity moved, recalculate the path and opportunity attacks
            if goal != prev_goal || self.prev_entity_position != Some(entity_position) {
                let in_combat = game_state.in_combat.contains_key(&self.entity);
                if let Ok(path_result) =
                    systems::movement::path(game_state, self.entity, &goal, true, in_combat)
                {
                    self.prev_goal = Some(goal);
                    self.path_result = Some(path_result.clone());
                    self.opportunity_attacks = systems::movement::potential_opportunity_attacks(
                        &game_state.world,
                        &path_result.taken_path,
                        self.entity,
                        &game_state.get_potential_reactors(self.entity),
                    );
                }
            }
        } else {
            self.prev_goal = Some(goal);
        }
    }
}

impl RenderableWithContext<&mut GameState> for MovementPreview {
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        for (entity, point) in &self.opportunity_attacks {
            if let Some(position) = systems::geometry::get_foot_position(&game_state.world, *entity)
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
    }
}
