use std::collections::HashMap;

use hecs::Entity;
use imgui::{ChildFlags, MouseButton};
use nat20_core::{
    components::{
        actions::{
            action::{ActionCondition, ActionContext, ActionKind, ActionMap},
            targeting::{AreaShape, TargetInstance, TargetingContext, TargetingKind},
        },
        d20::RollMode,
        id::{ActionId, Name, ResourceId},
        modifier::Modifiable,
        resource::{RechargeRule, ResourceAmount, ResourceAmountMap, ResourceMap},
        saving_throw::SavingThrowSet,
        speed::Speed,
    },
    engine::{
        action_prompt::{ActionData, ActionDecision, ActionDecisionKind, ActionPromptKind},
        game_state::GameState,
    },
    registry::registry::ResourcesRegistry,
    systems::{
        self,
        geometry::{RaycastHit, RaycastHitKind},
        movement::{PathResult, TargetPathFindingResult},
    },
};
use parry3d::na::Point3;
use tracing::{info, trace};
use uom::si::length::meter;

use crate::{
    render::{
        common::utils::RenderableMutWithContext,
        ui::{
            components::{LOW_HEALTH_BG_COLOR, LOW_HEALTH_COLOR, SPEED_COLOR, SPEED_COLOR_BG},
            text::{TextKind, TextSegment, TextSegments},
            utils::{
                ImguiRenderable, ImguiRenderableWithContext, ProgressBarColor,
                render_button_disabled_conditionally, render_button_with_padding,
                render_capacity_meter, render_progress_bar, roman_numeral,
            },
        },
        world::mesh::MeshRenderMode,
    },
    state::gui_state::GuiState,
    windows::anchor::{AUTO_RESIZE, BOTTOM_CENTER, WindowManager},
};

#[derive(Debug, Clone)]
pub enum ActionBarState {
    Action {
        actions: ActionMap,
    },
    Variant {
        variants: ActionMap,
    },
    Context {
        action: ActionId,
        contexts_and_costs: Vec<(ActionContext, ResourceAmountMap)>,
    },
    Targets {
        action: ActionData,
        potential_target: Option<(TargetInstance, TargetPathFindingResult)>,
    },
}

pub struct ActionBarWindow {
    pub state: ActionBarState,
    pub entity: Entity,
}

impl ActionBarWindow {
    pub fn new(game_state: &mut GameState, entity: Entity) -> Self {
        Self {
            state: ActionBarState::Action {
                actions: systems::actions::all_actions(&game_state.world, entity),
            },
            entity,
        }
    }

    pub fn is_disabled(&self, game_state: &GameState) -> bool {
        if let Some(encounter_id) = game_state.in_combat.get(&self.entity)
            && let Some(encounter) = game_state.encounters.get(encounter_id)
        {
            if encounter.current_entity() != self.entity {
                return true;
            }
            if let Some(prompt) = game_state.next_prompt_entity(self.entity)
                && matches!(prompt.kind, ActionPromptKind::Reactions { .. })
            {
                return true;
            }
        }
        false
    }
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
            format!(
                "Actions - {}",
                systems::helpers::get_component::<Name>(&game_state.world, self.entity).as_str()
            )
            .as_str(),
            &BOTTOM_CENTER,
            AUTO_RESIZE,
            &mut opened,
            || {
                let mut new_state = None;

                match &mut self.state {
                    ActionBarState::Action { actions } => {
                        render_actions(ui, game_state, self.entity, &mut new_state, actions);
                        ui.same_line();
                        render_resources(ui, game_state, self.entity);
                    }

                    ActionBarState::Variant { variants } => {
                        render_actions(ui, game_state, self.entity, &mut new_state, variants);
                        ui.separator();
                        right_click_cancel(ui, gui_state, game_state, &mut new_state, self.entity);
                    }

                    ActionBarState::Context {
                        action,
                        contexts_and_costs,
                    } => {
                        render_context_selection(
                            ui,
                            gui_state,
                            game_state,
                            &mut new_state,
                            action,
                            self.entity,
                            contexts_and_costs,
                        );
                    }

                    ActionBarState::Targets {
                        action,
                        potential_target,
                    } => {
                        render_target_selection(
                            ui,
                            gui_state,
                            game_state,
                            &mut new_state,
                            action,
                            potential_target,
                        );
                    }
                }

                if let Some(state) = new_state {
                    self.state = state;
                }
            },
        );

        disabled_token.end();

        if !opened {
            gui_state.selected_entity.take();
        }
    }
}

fn render_actions(
    ui: &imgui::Ui,
    game_state: &mut GameState,
    entity: Entity,
    new_state: &mut Option<ActionBarState>,
    actions: &mut ActionMap,
) {
    ui.child_window("Actions")
        .child_flags(
            ChildFlags::ALWAYS_AUTO_RESIZE | ChildFlags::AUTO_RESIZE_X | ChildFlags::AUTO_RESIZE_Y,
        )
        .build(|| {
            ui.separator_with_text("Actions");

            for (action_id, contexts_and_costs) in actions {
                // Don't render reactions
                if matches!(
                    systems::actions::get_action(action_id).unwrap().kind(),
                    ActionKind::Reaction { .. }
                ) {
                    continue;
                }
                // Don't render actions that either:
                // 1. Can only be used as reactions
                // 2. Cost a resource which never recharges and which is the entity
                //    currently doesn't have any of. This probably means the action
                //    is only usable under certain conditions which aren't currently
                //    met.

                // TODO: This works for now to hide stuff like Reapply Hex, but it's
                // definitely not ideal. Probably better to "mark" these actions somehow
                if contexts_and_costs.iter().all(|(_, cost)| {
                    cost.contains_key(&ResourceId::new("nat20_core", "resource.reaction"))
                        || cost.iter().any(|(res_id, amount)| {
                            let resources = systems::helpers::get_component::<ResourceMap>(
                                &game_state.world,
                                entity,
                            );
                            !resources.can_afford(res_id, amount)
                                && ResourcesRegistry::get(res_id).unwrap().recharge
                                    == RechargeRule::Never
                        })
                }) {
                    continue;
                }

                let mut action_usable = false;
                for (context, cost) in contexts_and_costs.iter_mut() {
                    for effect in systems::effects::effects(&game_state.world, entity).values() {
                        (effect.effect().on_resource_cost)(
                            &game_state.world,
                            entity,
                            action_id,
                            context,
                            cost,
                        );
                    }
                    if systems::actions::action_usable(
                        &game_state.world,
                        entity,
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
                    let action = systems::actions::get_action(action_id).unwrap();

                    match action.kind() {
                        ActionKind::Variant { variants } => {
                            *new_state = Some(ActionBarState::Variant {
                                variants: variants
                                    .iter()
                                    // Assume all variants have the same contexts and costs
                                    .map(|variant_action_id| {
                                        (variant_action_id.clone(), contexts_and_costs.clone())
                                    })
                                    .collect(),
                            });
                        }

                        _ => {
                            select_action(entity, new_state, action_id, contexts_and_costs);
                        }
                    }
                }

                disabled_token.end();

                if ui.is_item_hovered() {
                    ui.tooltip(|| {
                        let (context, cost) = &contexts_and_costs[0];
                        (action_id, context, cost)
                            .render_with_context(ui, (&game_state.world, entity));
                    });
                }
            }

            ui.separator();

            if game_state.in_combat.contains_key(&entity) {
                if ui.button("End Turn") {
                    game_state.end_turn(entity);
                }
            }
        });
}

fn select_action(
    entity: Entity,
    new_state: &mut Option<ActionBarState>,
    action_id: &ActionId,
    contexts_and_costs: &mut Vec<(ActionContext, HashMap<ResourceId, ResourceAmount>)>,
) {
    if contexts_and_costs.len() == 1 {
        *new_state = Some(ActionBarState::Targets {
            action: ActionData::new(
                entity,
                action_id.clone(),
                contexts_and_costs[0].0.clone(),
                contexts_and_costs[0].1.clone(),
                Vec::new(),
            ),
            potential_target: None,
        });
    } else {
        *new_state = Some(ActionBarState::Context {
            action: action_id.clone(),
            contexts_and_costs: contexts_and_costs.clone(),
        });
    }
}

fn render_resources(ui: &imgui::Ui, game_state: &mut GameState, entity: Entity) {
    ui.child_window("Resources")
        .child_flags(
            ChildFlags::ALWAYS_AUTO_RESIZE | ChildFlags::AUTO_RESIZE_X | ChildFlags::AUTO_RESIZE_Y,
        )
        .build(|| {
            ui.separator_with_text("Resources");
            systems::helpers::get_component::<ResourceMap>(&game_state.world, entity).render(ui);

            ui.separator_with_text("Speed");
            let speed = systems::helpers::get_component::<Speed>(&game_state.world, entity);

            let total_speed = speed.get_total_speed();
            let remaining_speed = speed.remaining_movement();
            render_progress_bar(
                ui,
                remaining_speed.value,
                total_speed.value,
                None,
                remaining_speed.value / total_speed.value,
                150.0,
                "Speed",
                Some("m"),
                Some(ProgressBarColor {
                    color_full: SPEED_COLOR,
                    color_empty: LOW_HEALTH_COLOR,
                    color_full_bg: SPEED_COLOR_BG,
                    color_empty_bg: LOW_HEALTH_BG_COLOR,
                }),
            );
        });
}

fn render_context_selection(
    ui: &imgui::Ui,
    gui_state: &mut GuiState,
    game_state: &mut GameState,
    new_state: &mut Option<ActionBarState>,
    action: &ActionId,
    actor: Entity,
    contexts_and_costs: &mut Vec<(ActionContext, ResourceAmountMap)>,
) {
    ui.text(format!("Select context for action: {}", action));

    for (i, (context, cost)) in contexts_and_costs.iter().enumerate() {
        if i > 0 {
            ui.same_line();
        }

        let disabled_token =
            ui.begin_disabled(!systems::resources::can_afford(&game_state.world, actor, cost).0);

        let clicked = match context {
            ActionContext::Weapon { slot } => {
                render_button_with_padding(ui, format!("{}", slot).as_str(), [10.0, 10.0])
            }

            ActionContext::Spell { level, .. } => {
                let style = ui.push_style_var(imgui::StyleVar::ButtonTextAlign([0.5, 0.5]));
                let clicked = ui.button_with_size(roman_numeral(*level), [30.0, 30.0]);
                style.pop();
                clicked
            }

            ActionContext::Other => render_button_with_padding(ui, "Other", [10.0, 10.0]),
        };

        if clicked {
            *new_state = Some(ActionBarState::Targets {
                action: ActionData::new(
                    actor,
                    action.clone(),
                    context.clone(),
                    cost.clone(),
                    Vec::new(),
                ),
                potential_target: None,
            });
        }

        disabled_token.end();

        if ui.is_item_hovered() {
            ui.tooltip(|| {
                (action, context, cost).render_with_context(ui, (&game_state.world, actor));
            });
        }
    }

    ui.separator();

    right_click_cancel(ui, gui_state, game_state, new_state, actor);
}

fn right_click_cancel(
    ui: &imgui::Ui,
    gui_state: &mut GuiState,
    game_state: &mut GameState,
    new_state: &mut Option<ActionBarState>,
    actor: Entity,
) {
    let right_click_cancel =
        if gui_state.cursor_ray_result.is_some() && ui.is_mouse_clicked(MouseButton::Right) {
            gui_state.cursor_ray_result.take();
            true
        } else {
            false
        };

    if ui.button("Cancel") || right_click_cancel {
        *new_state = Some(ActionBarState::Action {
            actions: systems::actions::all_actions(&game_state.world, actor),
        });
    }
}

fn render_target_selection(
    ui: &imgui::Ui,
    gui_state: &mut GuiState,
    game_state: &mut GameState,
    new_state: &mut Option<ActionBarState>,
    action: &mut ActionData,
    potential_target: &mut Option<(TargetInstance, TargetPathFindingResult)>,
) {
    ui.tooltip_text(action.action_id.to_string().as_str());

    let targeting_context = systems::actions::targeting_context(
        &game_state.world,
        action.actor,
        &action.action_id,
        &action.context,
    );

    render_range_preview(gui_state, game_state, action, &targeting_context);

    let mut submit_action = false;

    if let Some(raycast) = &gui_state.cursor_ray_result
        && let Some(closest) = raycast.closest()
    {
        update_potential_target(potential_target, game_state, action, closest);

        let potential_target_instance = if let Some((target, path_result)) = potential_target
            && should_render_target_preview(&targeting_context)
        {
            let (preview_position, path_to_target) =
                get_target_path_preview(game_state, action, path_result);

            render_target_path_preview(
                gui_state,
                game_state,
                action,
                preview_position,
                &path_to_target,
                target,
            );

            if let Some(path_to_target) = &path_to_target
                && path_to_target.reaches_goal()
                && ui.is_mouse_clicked(MouseButton::Left)
            {
                let result = game_state
                    .submit_movement(action.actor, *path_to_target.taken_path.end().unwrap());
            }

            Some(target)
        } else {
            gui_state.path_cache.remove(&action.actor);
            None
        };

        match systems::actions::get_action(&action.action_id)
            .unwrap()
            .kind()
        {
            ActionKind::Standard { condition, .. } => match condition {
                ActionCondition::AttackRoll { attack_roll, .. } => {
                    if let Some(potential_target) = &potential_target_instance
                        && let TargetInstance::Entity(target) = potential_target
                    {
                        let mut attack_roll =
                            attack_roll(&game_state.world, action.actor, *target, &action.context);
                        for effect in
                            systems::effects::effects(&game_state.world, action.actor).values()
                        {
                            (effect.effect().pre_attack_roll)(
                                &game_state.world,
                                action.actor,
                                &mut attack_roll,
                            );
                        }
                        for effect in systems::effects::effects(&game_state.world, *target).values()
                        {
                            (effect.effect().on_attacked)(
                                &game_state.world,
                                *target,
                                action.actor,
                                &mut attack_roll,
                            );
                        }

                        let target_ac = systems::loadout::armor_class(&game_state.world, *target);
                        ui.tooltip(|| {
                            ui.separator();

                            let hit_chance = attack_roll.hit_chance(
                                &game_state.world,
                                action.actor,
                                target_ac.total() as u32,
                            ) * 100.0;

                            let text_kind =
                                match attack_roll.d20_check.advantage_tracker().roll_mode() {
                                    RollMode::Normal => TextKind::Normal,
                                    RollMode::Advantage => TextKind::Green,
                                    RollMode::Disadvantage => TextKind::Red,
                                };

                            TextSegments::new(vec![
                                ("Hit chance:", TextKind::Normal),
                                (&format!("{:.0}%", hit_chance.floor()), text_kind),
                            ])
                            .render(ui);
                        });
                    }
                }

                ActionCondition::SavingThrow { saving_throw, .. } => {
                    if let Some(potential_target) = &potential_target_instance
                        && let TargetInstance::Entity(target) = potential_target
                    {
                        let dc = saving_throw(&game_state.world, action.actor, &action.context);
                        let saving_throws = systems::helpers::get_component::<SavingThrowSet>(
                            &game_state.world,
                            *target,
                        );
                        let d20_check = saving_throws.get(&dc.key);

                        // Chance for the target to succeed the save
                        let save_chance = d20_check.success_probability(
                            dc.dc.total() as u32,
                            systems::helpers::level(&game_state.world, *target)
                                .unwrap()
                                .proficiency_bonus(),
                        );
                        let success_chance = (1.0 - save_chance) * 100.0;

                        ui.tooltip(|| {
                            ui.separator();

                            let text_kind = match d20_check.advantage_tracker().roll_mode() {
                                RollMode::Normal => TextKind::Normal,
                                // It's a good thing if the *target* has disadvantage
                                RollMode::Disadvantage => TextKind::Green,
                                RollMode::Advantage => TextKind::Red,
                            };

                            TextSegment::new(
                                &format!("Success chance: {:.0}%", success_chance),
                                text_kind,
                            )
                            .render(ui);
                        });
                    }
                }

                _ => {}
            },
            _ => {}
        }

        match targeting_context.kind {
            TargetingKind::SelfTarget => {
                if ui.is_mouse_clicked(MouseButton::Left) {
                    action.targets.clear();
                    action.targets.push(TargetInstance::Entity(action.actor));
                    gui_state.cursor_ray_result.take();
                    submit_action = true;
                }
            }

            TargetingKind::Single => {
                if let Some(potential_target) = potential_target_instance {
                    if ui.is_mouse_clicked(MouseButton::Left) {
                        action.targets.clear();
                        action.targets.push(potential_target.clone());
                        gui_state.cursor_ray_result.take();
                        submit_action = true;
                    }
                }
            }

            TargetingKind::Multiple {
                max_targets,
                allow_duplicates,
            } => {
                let max_targets = max_targets as usize;
                if ui.is_mouse_clicked(MouseButton::Right) {
                    action.targets.pop();
                    if !action.targets.is_empty() {
                        gui_state.cursor_ray_result.take();
                    }
                }
                if let Some(potential_target) = potential_target_instance
                    && action.targets.len() < max_targets
                    && ui.is_mouse_clicked(MouseButton::Left)
                {
                    if !allow_duplicates && action.targets.contains(potential_target) {
                        action.targets.retain(|t| t != potential_target);
                    } else {
                        action.targets.push(potential_target.clone());
                    }
                    gui_state.cursor_ray_result.take();
                    if action.targets.len() == max_targets {
                        submit_action = true;
                    }
                }

                ui.tooltip(|| {
                    ui.separator();
                    ui.text("Targets:");
                    ui.same_line();
                    render_capacity_meter(
                        ui,
                        action.action_id.to_string().as_str(),
                        action.targets.len(),
                        max_targets,
                    );
                });
            }

            TargetingKind::Area {
                shape,
                fixed_on_actor,
            } => {
                if let Some(potential_target) = potential_target_instance {
                    // 1. Render the area shape at the potential target location
                    let point = match &potential_target {
                        TargetInstance::Entity(entity) => {
                            systems::geometry::get_foot_position(&game_state.world, *entity)
                                .unwrap()
                        }
                        TargetInstance::Point(point) => *point,
                    };
                    match &shape {
                        AreaShape::Sphere { radius } => {
                            gui_state.line_renderer.add_circle(
                                [point.x, point.y, point.z],
                                radius.get::<meter>(),
                                [1.0, 1.0, 1.0],
                            );
                        }
                        _ => { /* TODO other shapes */ }
                    }
                    // 2. Highlight entities within the area
                    let (shape, shape_pose) = shape.parry3d_shape(
                        &game_state.world,
                        action.actor,
                        fixed_on_actor,
                        &point,
                    );
                    let affected_entities =
                        systems::geometry::entities_in_shape(&game_state.world, shape, &shape_pose);
                    for entity in affected_entities {
                        gui_state.creature_render_mode.insert(
                            entity,
                            MeshRenderMode::MeshWithWireFrame {
                                color: [0.0, 1.0, 0.0, 0.5],
                                width: 3.0,
                            },
                        );
                    }
                    // 3. On left click, select all entities within the area as targets
                    if ui.is_mouse_clicked(MouseButton::Left) {
                        action.targets.clear();
                        action.targets.push(potential_target.clone());
                        gui_state.cursor_ray_result.take();
                        submit_action = true;
                    }
                }
            }
        }
    }

    if render_button_disabled_conditionally(
        ui,
        "Confirm Targets",
        [0.0, 0.0],
        action.targets.len() == 0,
        "Must select at least one target",
    ) {
        submit_action = true;
    }

    if submit_action {
        let response_to = if let Some(prompt) = game_state.next_prompt_entity(action.actor)
            && prompt.actors().contains(&action.actor)
        {
            info!("Submitting action in response to prompt: {:#?}", prompt);
            Some(prompt.id)
        } else {
            None
        };

        let action_kind = ActionDecisionKind::Action {
            action: action.clone(),
        };

        let result = if let Some(response_to) = response_to {
            game_state.submit_decision(ActionDecision {
                response_to,
                kind: action_kind,
            })
        } else {
            game_state.submit_decision(ActionDecision::without_response_to(action_kind))
        };

        info!("Submitted action decision: {:#?}", result);
    }

    // TODO: gui_state util function to handle checking for Some and taking?
    let right_click_cancel = if gui_state.cursor_ray_result.is_some()
        && ui.is_mouse_clicked(MouseButton::Right)
        && action.targets.is_empty()
    {
        gui_state.cursor_ray_result.take();
        true
    } else {
        false
    };

    if ui.button("Cancel") || right_click_cancel || submit_action {
        *new_state = Some(ActionBarState::Action {
            actions: systems::actions::all_actions(&game_state.world, action.actor),
        });
    }
}

fn should_render_target_preview(targeting_context: &TargetingContext) -> bool {
    match &targeting_context.kind {
        TargetingKind::SelfTarget => false,
        _ => true,
    }
}

fn get_target_path_preview(
    game_state: &mut GameState,
    action: &mut ActionData,
    path_result: &mut TargetPathFindingResult,
) -> (Point3<f32>, Option<PathResult>) {
    match path_result {
        TargetPathFindingResult::AlreadyInRange => (
            systems::geometry::get_eye_position(&game_state.world, action.actor).unwrap(),
            None,
        ),

        TargetPathFindingResult::PathFound(path) => {
            if let Some(end) = path.taken_path.end()
                && let Some(end_at_ground) =
                    systems::geometry::ground_position(&game_state.geometry, &end)
                && let Some(eye_pos) = systems::geometry::get_eye_position_at_point(
                    &game_state.world,
                    action.actor,
                    &end_at_ground,
                )
            {
                (eye_pos, Some(path.clone()))
            } else {
                (
                    systems::geometry::get_eye_position(&game_state.world, action.actor).unwrap(),
                    None,
                )
            }
        }
    }
}

fn render_target_path_preview(
    gui_state: &mut GuiState,
    game_state: &mut GameState,
    action: &mut ActionData,
    preview_position: Point3<f32>,
    path_to_target: &Option<PathResult>,
    target: &mut TargetInstance,
) {
    if let Some((shape, shape_pose_at_preview)) = systems::geometry::get_shape_at_point(
        &game_state.world,
        &game_state.geometry,
        action.actor,
        &preview_position,
    ) && let Some(mesh) = gui_state.mesh_cache.get(&format!("{:#?}", shape))
    {
        if let Some(path_to_target) = path_to_target {
            gui_state
                .path_cache
                .insert(action.actor, path_to_target.clone());
            mesh.draw(
                gui_state.ig_renderer.gl_context(),
                &gui_state.program,
                &shape_pose_at_preview.to_homogeneous(),
                [1.0, 1.0, 1.0, 0.75],
                &MeshRenderMode::WireFrameOnly {
                    color: [1.0, 1.0, 1.0, 0.75],
                    width: 2.0,
                },
            );
        }

        let line_end = match target {
            TargetInstance::Entity(entity) => {
                systems::geometry::get_shape(&game_state.world, *entity)
                    .map(|(_, shape_pose)| shape_pose.translation.vector.into())
                    .unwrap()
            }
            TargetInstance::Point(point) => *point,
        };
        gui_state
            .line_renderer
            .add_line(preview_position.into(), line_end.into(), [1.0, 1.0, 1.0]);
    }
}

fn render_range_preview(
    gui_state: &mut GuiState,
    game_state: &mut GameState,
    action: &mut ActionData,
    targeting_context: &nat20_core::components::actions::targeting::TargetingContext,
) {
    let normal_range = targeting_context.range.normal().get::<meter>();
    let max_range = targeting_context.range.max().get::<meter>();
    let actor_position = systems::geometry::get_foot_position(&game_state.world, action.actor)
        .map(|point| [point.x, point.y, point.z])
        .unwrap();
    gui_state
        .line_renderer
        .add_circle(actor_position, normal_range, [1.0, 1.0, 1.0]);
    if normal_range < max_range {
        gui_state
            .line_renderer
            .add_circle(actor_position, max_range, [0.5, 0.5, 0.5]);
    }
}

fn update_potential_target(
    potential_target: &mut Option<(TargetInstance, TargetPathFindingResult)>,
    game_state: &mut GameState,
    action: &ActionData,
    closest: &RaycastHit,
) {
    let closest_target = match &closest.kind {
        RaycastHitKind::Creature(entity) => TargetInstance::Entity(*entity),
        RaycastHitKind::World => TargetInstance::Point(closest.poi),
    };

    let mut potential_action = action.clone();
    potential_action.targets.clear();
    potential_action.targets.push(closest_target.clone());

    let is_new_target = if let Some((target, _)) = potential_target {
        target != &closest_target
    } else {
        true
    };

    if is_new_target {
        trace!("Finding path to new target {:?}", closest_target);
        match systems::movement::path_to_target(game_state, &potential_action, true) {
            Ok(result) => {
                trace!("Found path to target {:?}: {:?}", closest_target, result);
                *potential_target = Some((closest_target, result));
            }
            Err(err) => {
                trace!(
                    "Error finding path to target {:?}: {:?}",
                    closest_target, err
                );
                *potential_target = None;
            }
        }
    }
}
