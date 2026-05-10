use std::sync::Arc;

use hecs::Entity;
use imgui::{ChildFlags, MouseButton};
use nat20_core::{
    components::{
        ability::AbilityScoreMap,
        actions::{
            action::{ActionCondition, ActionKind, AttackRollFunction, SavingThrowFunction},
            action_builder::{ActionBuilder, ActionBuilderState},
            targeting::{TargetInstance, TargetingContext, TargetingError, TargetingKind},
        },
        activity::Activity,
        d20::{AdvantageType, D20Check, D20CheckOutcome, RollMode},
        modifier::{Modifiable, ModifierSource},
        resource::ResourceMap,
        saving_throw::SavingThrowSet,
        speed::Speed,
    },
    engine::{
        action_prompt::{ActionData, ActionPromptKind},
        game_state::GameState,
    },
    systems::{self, geometry::RaycastHitKind, movement::TargetPathFindingResult},
};
use parry3d::shape::ShapeType;
use tracing::{debug, error, info};
use uom::si::length::meter;

use crate::{
    render::{
        common::{
            colors::Color,
            utils::{Renderable, RenderableMutWithContext, RenderableWithContext},
        },
        ui::{
            components::ModifierSetRenderMode,
            text::{TextKind, TextSegment, TextSegments},
            utils::{
                ImguiRenderable, ImguiRenderableWithContext, render_button_disabled_conditionally,
                render_button_with_padding, render_capacity_meter, roman_numeral, signed_value,
            },
        },
        world::mesh::MeshRenderMode,
    },
    state::gui_state::GuiState,
    windows::{
        action_bar::movement_preview::MovementPreview,
        anchor::{AUTO_RESIZE, BOTTOM_CENTER, WindowManager},
    },
};

/// Max number of actions (including variants) to show in the action bar before
/// it starts scrolling. This is mostly just to prevent the action bar from
/// getting too big and blocking the screen
const MAX_ACTIONS: usize = 8;
/// TODO: Just guessing the height here
const ACTION_HEIGHT: f32 = 25.0;
const ACTION_LIST_HEIGHT: f32 = MAX_ACTIONS as f32 * ACTION_HEIGHT;

pub struct ActionBarWindow {
    pub builder: ActionBuilder,
    pub movement_preview: MovementPreview,
}

impl RenderableMutWithContext<&mut GameState> for ActionBarWindow {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let disabled_token = ui.begin_disabled(self.is_disabled(game_state));

        let window_manager_ptr =
            unsafe { &mut *(&mut gui_state.window_manager as *mut WindowManager) };

        let mut opened = true;

        window_manager_ptr.render_window(
            ui,
            &self.builder.actor().name().to_string(),
            &BOTTOM_CENTER,
            AUTO_RESIZE,
            &mut opened,
            || {
                let mut move_on_click = true;

                match self.builder.state() {
                    Ok(state) => match state {
                        ActionBuilderState::Action { .. } => {
                            self.render_actions(ui, game_state);
                            ui.same_line();
                            self.render_resources(ui, game_state);
                            ui.separator();
                            self.render_end_turn(ui, game_state);
                            self.movement_preview
                                .update(ui, gui_state, game_state, None);
                            self.movement_preview
                                .render_with_context(ui, gui_state, game_state);
                        }

                        ActionBuilderState::Variant { .. } => {
                            self.render_actions(ui, game_state);
                            ui.separator();
                            self.right_click_cancel(ui, gui_state, game_state);
                            self.movement_preview
                                .update(ui, gui_state, game_state, None);
                            self.movement_preview
                                .render_with_context(ui, gui_state, game_state);
                        }

                        ActionBuilderState::Context { .. } => {
                            self.render_context_selection(ui, gui_state, game_state);
                            self.movement_preview
                                .update(ui, gui_state, game_state, None);
                            self.movement_preview
                                .render_with_context(ui, gui_state, game_state);
                        }

                        ActionBuilderState::Targets { .. } => {
                            move_on_click = false;
                            self.render_target_selection(ui, gui_state, game_state);
                        }
                    },

                    Err(error) => {
                        // TODO: Would be cool if this was a pop up?
                        error!("Error building action: {:#?}", error);
                        self.builder =
                            ActionBuilder::all(&game_state.world, self.builder.actor().id());
                    }
                }

                if move_on_click
                    && ui.is_mouse_clicked(MouseButton::Left)
                    && let Some(cursor_ray_result) = &gui_state.cursor_ray_result
                    && let Some(closest) = cursor_ray_result.closest()
                    && closest.kind == RaycastHitKind::World
                {
                    let movement_result = game_state.submit_activity(Activity::Move {
                        entity: self.actor(),
                        goal: closest.poi,
                    });

                    match movement_result {
                        Ok(()) => {
                            debug!(
                                "Submitted movement for entity {:?} to point {:?}",
                                self.actor(),
                                closest.poi
                            );
                        }
                        Err(err) => {
                            error!("Failed to submit movement: {:?}", err);
                        }
                    }

                    gui_state.cursor_ray_result.take();
                }
            },
        );

        disabled_token.end();

        if !opened {
            gui_state.selected_entity.take();
        }
    }
}

impl ActionBarWindow {
    pub fn new(game_state: &mut GameState, entity: Entity) -> Self {
        Self {
            builder: ActionBuilder::all(&game_state.world, entity),
            movement_preview: MovementPreview::new(entity),
        }
    }

    pub fn actor(&self) -> Entity {
        self.builder.actor().id()
    }

    pub fn is_disabled(&self, game_state: &GameState) -> bool {
        if let Some(encounter_id) = game_state.in_combat.get(&self.builder.actor().id())
            && let Some(encounter) = game_state.encounters.get(encounter_id)
        {
            if encounter.current_entity() != self.builder.actor().id() {
                return true;
            }
            if let Some(prompt) = game_state.next_prompt_entity(self.builder.actor().id())
                && matches!(prompt.kind, ActionPromptKind::Reactions { .. })
            {
                return true;
            }
        }
        false
    }

    fn render_actions(&mut self, ui: &imgui::Ui, game_state: &mut GameState) {
        ui.child_window("Actions")
            .child_flags(
                ChildFlags::ALWAYS_AUTO_RESIZE
                    | ChildFlags::AUTO_RESIZE_X
                    | ChildFlags::AUTO_RESIZE_Y,
            )
            .build(|| {
                ui.separator_with_text("Actions");

                ui.child_window("ActionsList")
                    .child_flags(
                        ChildFlags::ALWAYS_AUTO_RESIZE
                            | ChildFlags::AUTO_RESIZE_X
                            | ChildFlags::BORDERS,
                    )
                    .size([0.0, ACTION_LIST_HEIGHT])
                    .build(|| {
                        self.render_actions_list(ui, game_state);
                    });
            });
    }

    fn render_actions_list(&mut self, ui: &imgui::Ui, game_state: &mut GameState) {
        let actor = self.builder.actor().id();

        let actions = match self.builder.state_mut().ok().unwrap() {
            ActionBuilderState::Action { actions } => actions,
            ActionBuilderState::Variant { variants } => variants,
            _ => panic!("Invalid state for rendering actions list"),
        };

        let mut selected_action = None;

        for (action_id, contexts_and_costs) in actions {
            // Don't render actions that either:
            // 1. Can only be used as reactions
            // 2. Cost a resource which never recharges and which is the entity
            //    currently doesn't have any of. This probably means the action
            //    is only usable under certain conditions which aren't currently
            //    met.
            if let Some(action) = systems::actions::get_action(action_id)
                && action.is_reaction()
            {
                continue;
            }

            let mut action_usable = false;
            for (context, cost) in contexts_and_costs.iter_mut() {
                systems::effects::effects(&game_state.world, actor).resource_cost(
                    &game_state.world,
                    actor,
                    action_id,
                    context,
                    cost,
                );
                if systems::actions::action_usable(
                    &game_state.world,
                    actor,
                    action_id,
                    context,
                    cost,
                )
                .is_ok()
                {
                    // Note to self: *don't* break here! We need to update
                    // the costs for all contexts even if one is usable
                    action_usable = true;
                } else {
                    continue;
                }
            }

            let disabled_token = ui.begin_disabled(!action_usable);

            if ui.button(&action_id.to_string()) {
                selected_action = Some(action_id.clone());
            }

            disabled_token.end();

            if ui.is_item_hovered() {
                ui.tooltip(|| {
                    let (context, cost) = &contexts_and_costs[0];
                    (action_id, context, cost).render_with_context(ui, (&game_state.world, actor));
                });
            }
        }

        if let Some(action_id) = selected_action {
            self.builder.action(&game_state.world, &action_id);
        }
    }

    fn render_end_turn(&self, ui: &imgui::Ui, game_state: &mut GameState) {
        let entity = self.builder.actor().id();
        if game_state.in_combat.contains_key(&entity) {
            if ui.button("End Turn") {
                game_state.end_turn(entity);
            }
        }
    }

    fn render_resources(&self, ui: &imgui::Ui, game_state: &mut GameState) {
        ui.child_window("Resources")
            .child_flags(
                ChildFlags::ALWAYS_AUTO_RESIZE
                    | ChildFlags::AUTO_RESIZE_X
                    | ChildFlags::AUTO_RESIZE_Y,
            )
            .build(|| {
                let entity = self.builder.actor().id();

                ui.separator_with_text("Resources");
                systems::helpers::get_component::<ResourceMap>(&game_state.world, entity)
                    .render(ui);

                ui.separator_with_text("Speed");
                systems::helpers::get_component::<Speed>(&game_state.world, entity).render(ui);
            });
    }

    fn render_context_selection(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let actor = self.builder.actor().id();

        let (action, contexts_and_costs) = match self.builder.state().ok().unwrap() {
            ActionBuilderState::Context {
                action,
                contexts_and_costs,
            } => (action, contexts_and_costs),
            _ => panic!("Invalid state for rendering context selection"),
        };

        TextSegments::new(vec![
            ("Select context for action:".to_string(), TextKind::Normal),
            (action.to_string(), TextKind::Action),
        ])
        .render(ui);

        let mut selected_context = None;

        for (i, (context, cost)) in contexts_and_costs.iter().enumerate() {
            if i > 0 {
                ui.same_line();
            }

            let disabled_token = ui.begin_disabled(
                systems::resources::can_afford(&game_state.world, actor, cost).is_err(),
            );

            let clicked = if let Some(spell) = &context.spell {
                let level = spell.level;
                {
                    let style = ui.push_style_var(imgui::StyleVar::ButtonTextAlign([0.5, 0.5]));
                    let clicked = ui.button_with_size(roman_numeral(level), [30.0, 30.0]);
                    style.pop();
                    clicked
                }
            } else if let Some(attack) = &context.attack {
                let label = if let Some(slot) = attack.slot {
                    format!("{}", slot)
                } else {
                    "Unarmed".to_string()
                };
                render_button_with_padding(ui, label.as_str(), [10.0, 10.0])
            } else {
                render_button_with_padding(ui, "Default", [10.0, 10.0])
            };

            if clicked {
                selected_context = Some(i);
            }

            disabled_token.end();

            if ui.is_item_hovered() {
                ui.tooltip(|| {
                    (action, context, cost).render_with_context(ui, (&game_state.world, actor));
                });
            }
        }

        ui.separator();

        if let Some(context_index) = selected_context {
            self.builder.context_index(&game_state.world, context_index);
        }

        self.right_click_cancel(ui, gui_state, game_state);
    }

    fn right_click_cancel(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let right_click_cancel =
            if gui_state.cursor_ray_result.is_some() && ui.is_mouse_clicked(MouseButton::Right) {
                gui_state.cursor_ray_result.take();
                true
            } else {
                false
            };

        if ui.button("Cancel") || right_click_cancel {
            self.builder = ActionBuilder::all(&game_state.world, self.builder.actor().id());
        }
    }

    fn render_target_selection(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let action = match self.builder.state().ok().unwrap() {
            ActionBuilderState::Targets { action, .. } => action,
            _ => panic!("Invalid state for rendering target selection"),
        };
        let num_targets = action.targets.len();

        let targeting_context = systems::actions::targeting_context_data(&game_state.world, action);

        let mut submit = Self::render_targeting_ui(ui, action, &targeting_context);

        Self::render_range_preview(gui_state, game_state, action, &targeting_context);

        if !ui.io().want_capture_mouse {
            self.handle_cursor_targeting(
                ui,
                gui_state,
                game_state,
                num_targets,
                targeting_context,
                &mut submit,
            );
        }

        if submit {
            gui_state.cursor_ray_result.take();
            // TODO: Could be cleaner
            let action = match self.builder.state() {
                Ok(ActionBuilderState::Targets { action, .. }) => action.clone(),
                other => panic!("Invalid state for submitting action: {:#?}", other),
            };
            let result = self.builder.perform(game_state);
            match result {
                Ok(()) => {
                    info!("Successfully submitted action: {:?}", action);
                }
                Err(err) => {
                    error!("Failed to submit action: {:?}", err);
                }
            }
            self.builder = ActionBuilder::all(&game_state.world, self.builder.actor().id());
        }

        self.right_click_cancel(ui, gui_state, game_state);
    }

    fn handle_cursor_targeting(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
        num_targets: usize,
        targeting_context: TargetingContext,
        submit: &mut bool,
    ) {
        // TODO: Avoid cloning every frame
        let Some(cursor_ray_result) = &gui_state.cursor_ray_result.clone() else {
            return;
        };

        match &targeting_context.kind {
            TargetingKind::SelfTarget => {
                if ui.is_mouse_clicked(MouseButton::Left) {
                    *submit = true;
                }
            }

            TargetingKind::Single => {
                self.handle_single_target(
                    ui,
                    gui_state,
                    game_state,
                    &targeting_context,
                    submit,
                    cursor_ray_result,
                );
            }

            TargetingKind::Multiple { max_targets, .. } => {
                ui.tooltip(|| {
                    ui.text("Targets:");
                    ui.same_line();
                    render_capacity_meter(
                        ui,
                        "action_bar_target_capacity",
                        num_targets,
                        *max_targets,
                    );
                });

                if num_targets > 0 && ui.is_mouse_clicked(MouseButton::Right) {
                    gui_state.cursor_ray_result.take();
                    self.builder.remove_latest_target();
                    return;
                }

                let Some(closest) = cursor_ray_result.closest() else {
                    return;
                };

                let target = closest.target_instance(&game_state.world);
                let action = match self.builder.state().ok().unwrap() {
                    ActionBuilderState::Targets { action, .. } => action.clone(),
                    _ => panic!("Invalid state for single target selection"),
                };
                render_target_chance_tooltips(ui, game_state, &action, &target);

                // Calculate line of sight manually so we can render the raycast
                let line_of_sight = systems::geometry::line_of_sight_entity_target(
                    &game_state.world,
                    &game_state.geometry,
                    self.actor(),
                    &target,
                    &targeting_context.line_of_sight,
                );
                line_of_sight.render(ui, gui_state);

                let targeting_result = targeting_context.validate_targets(
                    &game_state.world,
                    &game_state.geometry,
                    self.actor(),
                    &[target.clone()],
                );

                match targeting_result {
                    Ok(()) => {}
                    Err(err) => match err {
                        // Allow removing the duplicate target
                        TargetingError::DuplicateTargetNotAllowed { target } => {
                            if ui.is_mouse_clicked(MouseButton::Left) {
                                self.builder.remove_target(&target);
                                gui_state.cursor_ray_result.take();
                            }
                        }

                        other => {
                            ui.tooltip(|| {
                                other.render(ui);
                            });
                            return;
                        }
                    },
                };

                if ui.is_mouse_clicked(MouseButton::Left) {
                    self.builder.target(game_state, target);
                    gui_state.cursor_ray_result.take();
                    if num_targets + 1 == *max_targets {
                        *submit = true;
                    }
                }
            }

            TargetingKind::Area {
                shape,
                fixed_on_actor,
            } => {
                let Some(closest) = cursor_ray_result.closest() else {
                    return;
                };

                let shape_transform = shape.parry3d_shape(
                    &game_state.world,
                    self.actor(),
                    *fixed_on_actor,
                    &closest.poi,
                );

                match shape_transform.shape.shape_type() {
                    ShapeType::Ball => {
                        let ball = shape_transform.shape.as_ball().unwrap();
                        let mut center: [f32; 3] = shape_transform.transform.translation.into();
                        center[1] += 0.1; // Lift the center slightly above the ground to avoid z-fighting
                        gui_state
                            .line_renderer
                            .add_circle(center, ball.radius, Color::White);
                    }

                    _ => {
                        shape_transform.render_with_context(
                            ui,
                            gui_state,
                            (
                                Color::White.into(),
                                &MeshRenderMode::WireFrameOnly {
                                    color: Color::White.into(),
                                    width: 0.1,
                                },
                            ),
                        );
                    }
                }

                let action = match self.builder.state().ok().unwrap() {
                    ActionBuilderState::Targets { action, .. } => action,
                    _ => panic!("Invalid state for area targeting"),
                };

                let target = closest.target_instance(&game_state.world);

                render_target_chance_tooltips(ui, game_state, action, &target);

                let affected_entities = systems::actions::get_targeted_entities(
                    game_state,
                    action,
                    Some(vec![target.clone()]),
                );

                for entity in affected_entities {
                    gui_state.creature_render_mode.insert(
                        entity,
                        MeshRenderMode::MeshWithWireFrame {
                            color: [0.0, 1.0, 0.0, 0.5],
                            width: 3.0,
                        },
                    );
                }

                if *fixed_on_actor {
                    if ui.is_mouse_clicked(MouseButton::Left) {
                        self.builder.target(game_state, target);
                        *submit = true;
                    }
                } else {
                    self.handle_single_target(
                        ui,
                        gui_state,
                        game_state,
                        &targeting_context,
                        submit,
                        cursor_ray_result,
                    );
                }
            }
        }
    }

    fn handle_single_target(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
        targeting_context: &TargetingContext,
        submit: &mut bool,
        cursor_ray_result: &systems::geometry::RaycastResult,
    ) {
        let Some(closest) = cursor_ray_result.closest() else {
            return;
        };

        let target = closest.target_instance(&game_state.world);
        let targeting_result = targeting_context.validate_targets(
            &game_state.world,
            &game_state.geometry,
            self.actor(),
            &[target.clone()],
        );

        match targeting_result {
            Ok(()) => {}
            Err(err) => match err {
                // We can recover from these two (hopefully) with path_to_target
                TargetingError::OutOfRange { .. } | TargetingError::NoLineOfSight { .. } => {}

                other => {
                    ui.tooltip(|| {
                        other.render(ui);
                    });
                    return;
                }
            },
        };

        let mut action = match self.builder.state().ok().unwrap() {
            ActionBuilderState::Targets { action, .. } => action.clone(),
            _ => panic!("Invalid state for single target selection"),
        };
        render_target_chance_tooltips(ui, game_state, &action, &target);
        action.targets.push(target.clone());

        match systems::movement::path_to_target(game_state, &action) {
            Ok(path_result) => {
                match path_result {
                    TargetPathFindingResult::AlreadyInRange(line_of_sight_result) => {
                        line_of_sight_result.render(ui, gui_state);
                    }

                    TargetPathFindingResult::PathFound(path_in_range_of_point_result) => {
                        let Some(goal) = path_in_range_of_point_result.path_result.taken_path.end()
                        else {
                            return;
                        };

                        self.movement_preview
                            .update(ui, gui_state, game_state, Some(*goal));
                        self.movement_preview
                            .render_with_context(ui, gui_state, game_state);

                        if let Some((shape, shape_pose_at_preview)) =
                            systems::geometry::get_shape_at_point(
                                &game_state.world,
                                &game_state.geometry,
                                action.actor.id(),
                                goal,
                            )
                            && let Some(mesh) = gui_state.mesh_cache.get(&format!("{:#?}", shape))
                        {
                            let color = Color::White.with_alpha(0.75);
                            mesh.draw(
                                gui_state.ig_renderer.gl_context(),
                                &gui_state.program,
                                &shape_pose_at_preview.to_homogeneous(),
                                color,
                                &MeshRenderMode::WireFrameOnly { color, width: 2.0 },
                            );
                        }

                        if path_in_range_of_point_result.path_result.reaches_goal() {
                            path_in_range_of_point_result
                                .line_of_sight_result
                                .render(ui, gui_state);
                        } else {
                            ui.tooltip(|| {
                                TextSegment::new("Cannot reach target".to_string(), TextKind::Red)
                                    .render(ui);
                            });
                            return;
                        }
                    }
                }

                if ui.is_mouse_clicked(MouseButton::Left) {
                    self.builder.target(game_state, target);
                    *submit = true;
                }
            }

            Err(path_error) => {
                ui.tooltip(|| {
                    TextSegment::new(format!("Cannot target: {path_error:#?}"), TextKind::Red)
                        .render(ui);
                });
            }
        }
    }

    fn render_targeting_ui(
        ui: &imgui::Ui,
        action: &ActionData,
        targeting_context: &TargetingContext,
    ) -> bool {
        if !ui.io().want_capture_mouse {
            ui.tooltip(|| {
                ui.separator_with_text(action.action_id.to_string());
            });
        }

        TextSegments::new(vec![
            ("Select targets for action:".to_string(), TextKind::Normal),
            (action.action_id.to_string(), TextKind::Action),
        ])
        .render(ui);

        ui.separator();

        targeting_context.kind.render(ui);

        ui.separator();

        return render_button_disabled_conditionally(
            ui,
            "Confirm Targets",
            [0.0, 0.0],
            action.targets.is_empty(),
            "Must select at least one target",
        );
    }

    fn render_range_preview(
        gui_state: &mut GuiState,
        game_state: &mut GameState,
        action: &ActionData,
        targeting_context: &TargetingContext,
    ) {
        let normal_range = targeting_context.range.normal().get::<meter>();
        let max_range = targeting_context.range.max().get::<meter>();
        // Take the size of the actor into account when rendering the range.
        // This is mostly relevant for melee attacks
        let Some((actor_shape, actor_pose)) =
            systems::geometry::get_shape(&game_state.world, action.actor.id())
        else {
            return;
        };
        let actor_radius = actor_shape.radius;
        let mut actor_position: [f32; 3] = actor_pose.translation.into();
        // Put the circle on the ground, but avoid z-fighting
        actor_position[1] -= actor_shape.height() + 0.1;

        gui_state.line_renderer.add_circle(
            actor_position,
            actor_radius + normal_range,
            Color::White,
        );
        if normal_range < max_range {
            gui_state.line_renderer.add_circle(
                actor_position,
                actor_radius + max_range,
                Color::Gray,
            );
        }
    }
}

impl ImguiRenderable for TargetingError {
    fn render(&self, ui: &imgui::Ui) {
        match self {
            TargetingError::NoTargetsProvided => {
                TextSegment::new("No targets provided".to_string(), TextKind::Red).render(ui)
            }

            TargetingError::ExceedsMaxTargets => TextSegment::new(
                "Exceeds maximum number of targets".to_string(),
                TextKind::Red,
            )
            .render(ui),

            TargetingError::NotSelf { .. } => {
                TextSegment::new("Must target self".to_string(), TextKind::Red).render(ui)
            }

            TargetingError::DuplicateTargetNotAllowed { target } => TextSegment::new(
                format!("Duplicate target not allowed: {target:?}"),
                TextKind::Red,
            )
            .render(ui),

            TargetingError::OutOfRange {
                distance,
                max_range,
                ..
            } => TextSegments::new(vec![(
                format!(
                    "Out of range ({:.1} m/{:.1} m)",
                    distance.get::<meter>(),
                    max_range.get::<meter>()
                ),
                TextKind::Red,
            )])
            .render(ui),

            TargetingError::NoLineOfSight { .. } => {
                TextSegment::new("No line of sight".to_string(), TextKind::Red).render(ui)
            }

            TargetingError::InvalidTarget {
                violated_filters, ..
            } => TextSegment::new(
                format!(
                    "Invalid target: {}",
                    violated_filters
                        .iter()
                        .map(|f| format!("{f:?}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                TextKind::Red,
            )
            .render(ui),
        }
    }
}

/// Renders “hit chance” or “success chance” tooltips depending on action condition.
/// Only runs when the hovered potential target is an Entity.
fn render_target_chance_tooltips(
    ui: &imgui::Ui,
    game_state: &mut GameState,
    action: &ActionData,
    target: &TargetInstance,
) {
    let TargetInstance::Entity { entity, .. } = target else {
        return;
    };

    let Some(action_def) = systems::actions::get_action(&action.action_id) else {
        return;
    };

    let ActionKind::Standard { condition, .. } = action_def.kind() else {
        return;
    };

    match condition {
        ActionCondition::AttackRoll(attack_roll) => {
            render_attack_hit_chance_tooltip(ui, game_state, action, entity.id(), attack_roll);
        }
        ActionCondition::SavingThrow(saving_throw) => {
            render_save_success_chance_tooltip(ui, game_state, action, entity.id(), saving_throw);
        }
        _ => {}
    }
}

fn render_attack_hit_chance_tooltip(
    ui: &imgui::Ui,
    game_state: &mut GameState,
    action: &ActionData,
    target: Entity,
    attack_roll_fn: &Arc<AttackRollFunction>,
) {
    let mut attack_roll = attack_roll_fn(
        &game_state.world,
        action.actor.id(),
        target,
        &action.context,
    );

    // Effects on target
    systems::effects::effects(&game_state.world, target).attacked_preview(
        &game_state.world,
        target,
        action.actor.id(),
        &mut attack_roll,
    );

    let target_ac = systems::loadout::armor_class(&game_state.world, target);

    let hit_chance = attack_roll.hit_chance(
        &game_state.world,
        action.actor.id(),
        target_ac.total() as u32,
    ) * 100.0;

    ui.tooltip(|| {
        ui.separator_with_text("Hit chance");

        render_forced_outcome_or_advantage(ui, &attack_roll.d20_check, hit_chance, false);

        ui.separator();

        ui.child_window("Attack Roll")
            .child_flags(
                ChildFlags::ALWAYS_AUTO_RESIZE
                    | ChildFlags::AUTO_RESIZE_X
                    | ChildFlags::AUTO_RESIZE_Y
                    | ChildFlags::BORDERS,
            )
            .size([0.0, 0.0])
            .build(|| {
                ui.separator_with_text("Attack Roll");
                render_d20_modifiers(ui, attack_roll.d20_check);
            });

        ui.child_window("Armor Class")
            .child_flags(
                ChildFlags::ALWAYS_AUTO_RESIZE
                    | ChildFlags::AUTO_RESIZE_X
                    | ChildFlags::AUTO_RESIZE_Y
                    | ChildFlags::BORDERS,
            )
            .size([0.0, 0.0])
            .build(|| {
                ui.separator_with_text("Armor Class");

                TextSegments::new([
                    ("Total:", TextKind::Details),
                    (&format!("{}", target_ac.total()), TextKind::Normal),
                ])
                .render(ui);

                TextSegments::new(vec![
                    (format!("{}", target_ac.base.0), TextKind::Normal),
                    (format!("({})", target_ac.base.1), TextKind::Details),
                ])
                .with_indent(1)
                .render(ui);

                target_ac
                    .modifiers
                    .render_with_context(ui, ModifierSetRenderMode::List(1));
            });
    });
}

fn render_save_success_chance_tooltip(
    ui: &imgui::Ui,
    game_state: &mut GameState,
    action: &ActionData,
    target: Entity,
    saving_throw_fn: &Arc<SavingThrowFunction>,
) {
    let saving_throw_dc = saving_throw_fn(&game_state.world, action.actor.id(), &action.context);
    let saving_throws =
        systems::helpers::get_component::<SavingThrowSet>(&game_state.world, target);
    let mut d20_check = saving_throws.get(&saving_throw_dc.key).clone();

    for effect in systems::effects::effects(&game_state.world, target).values() {
        if let Some(on_saving_throw) = effect.effect().on_saving_throw.get(&saving_throw_dc.key) {
            (on_saving_throw.check_hook)(&game_state.world, target, &mut d20_check);
        }
    }
    if let Some(ability) = saving_throws.ability(&saving_throw_dc.key) {
        let ability_scores =
            systems::helpers::get_component::<AbilityScoreMap>(&game_state.world, target);
        d20_check.add_modifier(
            ModifierSource::Ability(ability),
            ability_scores.ability_modifier(&ability).total(),
        );
    }

    let save_chance = d20_check.success_probability(
        saving_throw_dc.dc.total() as u32,
        systems::helpers::level(&game_state.world, target)
            .unwrap()
            .proficiency_bonus(),
    );

    let success_chance = (1.0 - save_chance) * 100.0;

    ui.tooltip(|| {
        ui.separator_with_text("Hit chance");

        render_forced_outcome_or_advantage(ui, &d20_check, success_chance, true);

        ui.separator();

        ui.child_window("Difficulty Class")
            .child_flags(
                ChildFlags::ALWAYS_AUTO_RESIZE
                    | ChildFlags::AUTO_RESIZE_X
                    | ChildFlags::AUTO_RESIZE_Y
                    | ChildFlags::BORDERS,
            )
            .size([0.0, 0.0])
            .build(|| {
                ui.separator_with_text("Difficulty Class");

                TextSegments::new([
                    ("Total:", TextKind::Details),
                    (&format!("{}", saving_throw_dc.dc.total()), TextKind::Normal),
                ])
                .render(ui);

                saving_throw_dc
                    .dc
                    .render_with_context(ui, ModifierSetRenderMode::List(1));
            });

        ui.child_window("Saving Throw")
            .child_flags(
                ChildFlags::ALWAYS_AUTO_RESIZE
                    | ChildFlags::AUTO_RESIZE_X
                    | ChildFlags::AUTO_RESIZE_Y
                    | ChildFlags::BORDERS,
            )
            .size([0.0, 0.0])
            .build(|| {
                ui.separator_with_text("Saving Throw");
                render_d20_modifiers(ui, d20_check);
            });
    });
}

fn render_d20_modifiers(ui: &imgui::Ui, d20_check: D20Check) {
    TextSegments::new([
        ("Total:", TextKind::Details),
        (
            &signed_value(&d20_check.modifiers().total()),
            TextKind::Normal,
        ),
    ])
    .render(ui);

    d20_check
        .modifiers()
        .render_with_context(ui, ModifierSetRenderMode::List(1));
}

fn render_forced_outcome_or_advantage(
    ui: &imgui::Ui,
    d20_check: &D20Check,
    hit_chance: f64,
    reverse_text_color: bool,
) {
    if let Some((source, outcome)) = d20_check.forced_outcome() {
        render_forced_outcome(ui, hit_chance, source, outcome, reverse_text_color);
    } else {
        render_advantage(ui, &d20_check, hit_chance, reverse_text_color);
    }
}

fn render_advantage(
    ui: &imgui::Ui,
    d20_check: &D20Check,
    hit_chance: f64,
    reverse_text_color: bool,
) {
    TextSegment::new(
        &format!("{:.0}%", hit_chance.floor()),
        d20_roll_mode_text_kind(&d20_check, reverse_text_color),
    )
    .render(ui);

    let mut advantage_text = Vec::new();
    for (source, advantage_type) in d20_check.advantage_tracker().summary() {
        let advantage_text_kind = match advantage_type {
            AdvantageType::Advantage => TextKind::Green,
            AdvantageType::Disadvantage => TextKind::Red,
        };
        if reverse_text_color {
            advantage_text.push((
                advantage_type.to_string(),
                reverse_text_kind(advantage_text_kind),
            ));
        } else {
            advantage_text.push((advantage_type.to_string(), advantage_text_kind));
        }
        advantage_text.push((source.to_string(), TextKind::Details))
    }
    TextSegments::new(advantage_text).render(ui);
}

fn d20_roll_mode_text_kind(d20_check: &D20Check, reverse_text_color: bool) -> TextKind {
    let text_kind = match d20_check.advantage_tracker().roll_mode() {
        RollMode::Normal => TextKind::Normal,
        RollMode::Advantage => TextKind::Green,
        RollMode::Disadvantage => TextKind::Red,
    };

    if reverse_text_color {
        reverse_text_kind(text_kind)
    } else {
        text_kind
    }
}

fn render_forced_outcome(
    ui: &imgui::Ui,
    hit_chance: f64,
    source: &ModifierSource,
    outcome: &D20CheckOutcome,
    reverse_text_color: bool,
) {
    let text_kind = d20_outcome_text_kind(outcome, reverse_text_color);

    TextSegment::new(&format!("{:.0}%", hit_chance.floor()), text_kind.clone()).render(ui);

    TextSegments::new(vec![
        (outcome.to_string(), text_kind),
        (source.to_string(), TextKind::Details),
    ])
    .render(ui);
}

fn d20_outcome_text_kind(outcome: &D20CheckOutcome, reverse_text_color: bool) -> TextKind {
    let text_kind = match outcome {
        D20CheckOutcome::Success | D20CheckOutcome::CriticalSuccess => TextKind::Green,
        D20CheckOutcome::Failure | D20CheckOutcome::CriticalFailure => TextKind::Red,
    };

    if reverse_text_color {
        reverse_text_kind(text_kind)
    } else {
        text_kind
    }
}

fn reverse_text_kind(text_kind: TextKind) -> TextKind {
    match text_kind {
        TextKind::Green => TextKind::Red,
        TextKind::Red => TextKind::Green,
        other => other,
    }
}
