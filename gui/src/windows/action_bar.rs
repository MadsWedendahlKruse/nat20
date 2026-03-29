use std::{collections::HashMap, path, sync::Arc};

use hecs::{Entity, World};
use imgui::{ChildFlags, MouseButton};
use nat20_core::{
    components::{
        ability::AbilityScoreMap,
        actions::{
            action::{
                ActionCondition, ActionContext, ActionKind, ActionMap, AttackRollFunction,
                SavingThrowFunction,
            },
            targeting::{AreaShape, TargetInstance, TargetingContext, TargetingKind},
        },
        activity::Activity,
        d20::{AdvantageType, D20Check, D20CheckOutcome, RollMode},
        id::{ActionId, EntityIdentifier, Name, ResourceId},
        modifier::{Modifiable, ModifierSource},
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
use tracing::{error, info, trace};
use uom::si::length::meter;

use crate::{
    render::{
        common::{colors::Color, utils::RenderableMutWithContext},
        ui::{
            components::{
                LOW_HEALTH_BG_COLOR, LOW_HEALTH_COLOR, ModifierSetRenderMode, SPEED_COLOR,
                SPEED_COLOR_BG,
            },
            text::{TextKind, TextSegment, TextSegments},
            utils::{
                ImguiRenderable, ImguiRenderableWithContext, ProgressBarColor,
                render_button_disabled_conditionally, render_button_with_padding,
                render_capacity_meter, render_progress_bar, roman_numeral, signed_value,
            },
        },
        world::mesh::MeshRenderMode,
    },
    state::gui_state::GuiState,
    windows::anchor::{AUTO_RESIZE, BOTTOM_CENTER, WindowManager},
};

/// Max number of actions (including variants) to show in the action bar before
/// it starts scrolling. This is mostly just to prevent the action bar from
/// getting too big and blocking the screen
const MAX_ACTIONS: usize = 8;
/// TODO: Just guessing the height here
const ACTION_HEIGHT: f32 = 25.0;
const ACTION_LIST_HEIGHT: f32 = MAX_ACTIONS as f32 * ACTION_HEIGHT;

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

pub struct MovementPreview {
    pub prev_target_point: Option<Point3<f32>>,
    pub prev_entity_position: Option<Point3<f32>>,
    pub path_result: Option<PathResult>,
    pub opportunity_attacks: Vec<(Entity, Point3<f32>)>,
}

impl MovementPreview {
    pub fn new() -> Self {
        Self {
            prev_target_point: None,
            prev_entity_position: None,
            path_result: None,
            opportunity_attacks: Vec::new(),
        }
    }
}

pub struct ActionBarWindow {
    pub state: ActionBarState,
    pub entity: Entity,
    pub movement_preview: MovementPreview,
}

impl ActionBarWindow {
    pub fn new(game_state: &mut GameState, entity: Entity) -> Self {
        Self {
            state: ActionBarState::Action {
                actions: systems::actions::all_actions(&game_state.world, entity),
            },
            entity,
            movement_preview: MovementPreview::new(),
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

    fn render_movement_preview(
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
            if let Some(prev_target_point) = self.movement_preview.prev_target_point {
                let entity_position =
                    systems::geometry::get_foot_position(&game_state.world, self.entity).unwrap();
                if closest.poi != prev_target_point
                    || self.movement_preview.prev_entity_position != Some(entity_position)
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
                        self.movement_preview.prev_target_point = Some(closest.poi);
                        self.movement_preview.path_result = Some(path_result.clone());
                        self.movement_preview.opportunity_attacks =
                            systems::movement::potential_opportunity_attacks(
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
                self.movement_preview.prev_target_point = Some(closest.poi);
            }

            for (entity, point) in &self.movement_preview.opportunity_attacks {
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

            if let Some(path) = &self.movement_preview.path_result {
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
                        ui.separator();
                        render_end_turn(ui, game_state, self.entity);
                        self.render_movement_preview(ui, gui_state, game_state);
                    }

                    ActionBarState::Variant { variants } => {
                        render_actions(ui, game_state, self.entity, &mut new_state, variants);
                        ui.separator();
                        right_click_cancel(ui, gui_state, game_state, &mut new_state, self.entity);
                        self.render_movement_preview(ui, gui_state, game_state);
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
                        self.render_movement_preview(ui, gui_state, game_state);
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

            render_actions_list(ui, game_state, entity, new_state, actions);
        });
}

fn render_actions_list(
    ui: &imgui::Ui,
    game_state: &mut GameState,
    entity: Entity,
    new_state: &mut Option<ActionBarState>,
    actions: &mut ActionMap,
) {
    ui.child_window("Actions")
        .child_flags(
            ChildFlags::ALWAYS_AUTO_RESIZE | ChildFlags::AUTO_RESIZE_X | ChildFlags::BORDERS,
        )
        .size([0.0, ACTION_LIST_HEIGHT])
        .build(|| {
            for (action_id, contexts_and_costs) in actions {
                // Don't render actions that either:
                // 1. Can only be used as reactions
                // 2. Cost a resource which never recharges and which is the entity
                //    currently doesn't have any of. This probably means the action
                //    is only usable under certain conditions which aren't currently
                //    met.
                if let Some(action) = systems::actions::get_action(action_id)
                    && action.kind().is_reaction()
                {
                    continue;
                }

                let mut action_usable = false;
                for (context, cost) in contexts_and_costs.iter_mut() {
                    systems::effects::effects(&game_state.world, entity).resource_cost(
                        &game_state.world,
                        entity,
                        action_id,
                        context,
                        cost,
                    );
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
                            select_action(
                                EntityIdentifier::from_world(&game_state.world, entity),
                                new_state,
                                action_id,
                                contexts_and_costs,
                            );
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
        });
}

fn render_end_turn(ui: &imgui::Ui, game_state: &mut GameState, entity: Entity) {
    if game_state.in_combat.contains_key(&entity) {
        if ui.button("End Turn") {
            game_state.end_turn(entity);
        }
    }
}

fn select_action(
    entity: EntityIdentifier,
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

            let total_speed = speed.total_speed();
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

            if ui.is_item_hovered() {
                ui.tooltip(|| {
                    TextSegments::new(vec![
                        ("Total speed:".to_string(), TextKind::Details),
                        (format!("{:.1} m", total_speed.value), TextKind::Normal),
                    ])
                    .render(ui);

                    ui.separator_with_text("Flat bonus");
                    let flat_bonuses = speed.flat_bonuses();
                    TextSegments::new(vec![
                        ("Total:".to_string(), TextKind::Details),
                        (
                            format!("{:.1} m", flat_bonuses.values().sum::<f32>()),
                            TextKind::Normal,
                        ),
                    ])
                    .render(ui);
                    for (source, flat_bonus) in flat_bonuses {
                        TextSegments::new(vec![
                            (format!("{:.1} m", flat_bonus), TextKind::Normal),
                            (source.to_string(), TextKind::Details),
                        ])
                        .with_indent(1)
                        .render(ui);
                    }

                    let multipliers = speed.multipliers();
                    if !multipliers.is_empty() {
                        ui.separator_with_text("Multipliers");
                        TextSegments::new(vec![
                            ("Total:".to_string(), TextKind::Details),
                            (
                                format!("x{:.2}", multipliers.values().product::<f32>()),
                                TextKind::Normal,
                            ),
                        ])
                        .render(ui);
                        for (source, multiplier) in speed.multipliers() {
                            TextSegments::new(vec![
                                (format!("x{:.2}", multiplier), TextKind::Normal),
                                (source.to_string(), TextKind::Details),
                            ])
                            .with_indent(1)
                            .render(ui);
                        }
                    }

                    let free_movement_multipliers = speed.free_movement_multipliers();
                    if !free_movement_multipliers.is_empty() {
                        ui.separator_with_text("Free movement");
                        TextSegments::new(vec![
                            ("Remaining:".to_string(), TextKind::Details),
                            (
                                format!("{:.1} m", speed.free_movement_remaining().get::<meter>()),
                                TextKind::Normal,
                            ),
                        ])
                        .render(ui);
                        for (source, multiplier) in speed.free_movement_multipliers() {
                            TextSegments::new(vec![
                                (format!("x{:.2}", multiplier), TextKind::Normal),
                                (source.to_string(), TextKind::Details),
                            ])
                            .with_indent(1)
                            .render(ui);
                        }
                    }
                });
            }
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
            *new_state = Some(ActionBarState::Targets {
                action: ActionData::new(
                    EntityIdentifier::from_world(&game_state.world, actor),
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
    ui.tooltip(|| {
        ui.separator_with_text(action.action_id.to_string());
    });

    let targeting_context = systems::actions::targeting_context(
        &game_state.world,
        action.actor.id(),
        &action.action_id,
        &action.context,
    );

    // Always show range rings while selecting targets.
    render_range_preview(gui_state, game_state, action, &targeting_context);

    let hovered = match hovered_target_from_cursor(gui_state) {
        Some(hovered) => hovered,
        None => {
            // Still allow cancel button even if not hovering world
            handle_target_selection_footer(ui, gui_state, game_state, new_state, action, false);
            return;
        }
    };

    update_potential_target(
        potential_target,
        game_state,
        action,
        &targeting_context,
        hovered.raycast_hit,
    );

    let hovered_target_instance =
        target_instance_from_raycast(&game_state.world, hovered.raycast_hit);

    let preview = compute_and_render_target_preview(
        ui,
        gui_state,
        game_state,
        action,
        &targeting_context,
        potential_target,
        &hovered_target_instance,
    );
    let potential_target = &preview
        .as_ref()
        .map(|preview| preview.target_instance.clone());

    let mut submit_action = apply_targeting_click_logic(
        ui,
        gui_state,
        game_state,
        action,
        &targeting_context,
        potential_target,
    );

    render_target_chance_tooltips(ui, game_state, action, potential_target);

    // Confirm button can also trigger submit.
    let confirm_clicked = render_button_disabled_conditionally(
        ui,
        "Confirm Targets",
        [0.0, 0.0],
        action.targets.is_empty(),
        "Must select at least one target",
    );
    if confirm_clicked {
        submit_action = true;
    }

    if submit_action {
        submit_action_decision(
            game_state,
            action,
            &preview.map(|preview| preview.path_to_target).flatten(),
        );
    }

    // Footer handles cancel / right-click cancel etc.
    handle_target_selection_footer(ui, gui_state, game_state, new_state, action, submit_action);
}

/// Data extracted from cursor raycast so we can avoid re-peeking everywhere.
struct HoveredWorldTarget<'a> {
    raycast_hit: &'a RaycastHit,
}

/// If we have a cursor raycast hit, return it. Does not consume.
fn hovered_target_from_cursor<'a>(gui_state: &'a GuiState) -> Option<HoveredWorldTarget<'a>> {
    let raycast = gui_state.cursor_ray_result.as_ref()?;
    let closest = raycast.closest()?;
    Some(HoveredWorldTarget {
        raycast_hit: closest,
    })
}

fn target_instance_from_raycast(world: &World, raycast_hit: &RaycastHit) -> TargetInstance {
    match &raycast_hit.kind {
        RaycastHitKind::Creature(entity) => {
            TargetInstance::Entity(EntityIdentifier::from_world(world, *entity))
        }
        RaycastHitKind::World => TargetInstance::Point(raycast_hit.poi),
    }
}

/// Result of preview computation for the current frame.
struct TargetPreviewResult {
    target_instance: TargetInstance,
    path_to_target: Option<PathResult>,
}

/// Computes + renders the targeting preview. Also includes the “move-into-range on click” behavior
/// (left click while path reaches goal triggers submit_movement).
fn compute_and_render_target_preview(
    ui: &imgui::Ui,
    gui_state: &mut GuiState,
    game_state: &mut GameState,
    action: &mut ActionData,
    targeting_context: &TargetingContext,
    potential_target: &mut Option<(TargetInstance, TargetPathFindingResult)>,
    hovered_target_instance: &TargetInstance,
) -> Option<TargetPreviewResult> {
    // If we don't render target preview for this targeting kind, then don't treat hover as selectable.
    if !should_render_target_preview(targeting_context) {
        return None;
    }

    // Require potential_target cache to exist and match the current hovered instance.
    let Some((cached_target, path_result)) = potential_target.as_mut() else {
        return None;
    };

    if cached_target != hovered_target_instance {
        // Stale cache; wait for update_potential_target to recompute next frame.
        return None;
    }

    let (preview_position, path_to_target) =
        get_target_path_preview(game_state, action, path_result);

    // Draw visuals for preview.
    render_target_path_preview(
        gui_state,
        game_state,
        action,
        preview_position,
        &path_to_target,
        cached_target,
    );

    Some(TargetPreviewResult {
        target_instance: cached_target.clone(),
        path_to_target,
    })
}

/// Renders “hit chance” or “success chance” tooltips depending on action condition.
/// Only runs when the hovered potential target is an Entity.
fn render_target_chance_tooltips(
    ui: &imgui::Ui,
    game_state: &mut GameState,
    action: &mut ActionData,
    potential_target_instance: &Option<TargetInstance>,
) {
    let Some(TargetInstance::Entity(target_entity)) = potential_target_instance else {
        return;
    };

    let Some(action_def) = systems::actions::get_action(&action.action_id) else {
        return;
    };

    let ActionKind::Standard { condition, .. } = action_def.kind() else {
        return;
    };

    match condition {
        ActionCondition::AttackRoll { attack_roll, .. } => {
            render_attack_hit_chance_tooltip(
                ui,
                game_state,
                action,
                target_entity.id(),
                attack_roll,
            );
        }
        ActionCondition::SavingThrow { saving_throw, .. } => {
            render_save_success_chance_tooltip(
                ui,
                game_state,
                action,
                target_entity.id(),
                saving_throw,
            );
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

    // Effects on attacker
    systems::effects::effects(&game_state.world, action.actor.id()).pre_attack_roll(
        &game_state.world,
        action.actor.id(),
        &mut attack_roll,
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

/// Applies click logic per TargetingKind. Mutates `action.targets`.
/// Returns whether the action should be submitted this frame.
fn apply_targeting_click_logic(
    ui: &imgui::Ui,
    gui_state: &mut GuiState,
    game_state: &mut GameState,
    action: &mut ActionData,
    targeting_context: &TargetingContext,
    potential_target_instance: &Option<TargetInstance>,
) -> bool {
    let left_clicked = ui.is_mouse_clicked(MouseButton::Left);
    let right_clicked = ui.is_mouse_clicked(MouseButton::Right);

    match &targeting_context.kind {
        TargetingKind::SelfTarget => {
            if left_clicked {
                action.targets.clear();
                action
                    .targets
                    .push(TargetInstance::Entity(EntityIdentifier::from_world(
                        &game_state.world,
                        action.actor.id(),
                    )));
                gui_state.cursor_ray_result.take();
                return true;
            }
            false
        }

        TargetingKind::Single => {
            if left_clicked {
                if let Some(target) = potential_target_instance {
                    action.targets.clear();
                    action.targets.push(target.clone());
                    gui_state.cursor_ray_result.take();
                    return true;
                }
            }
            false
        }

        TargetingKind::Multiple {
            max_targets,
            allow_duplicates,
        } => {
            let max_targets = *max_targets as usize;

            // Right click pops last target
            if right_clicked {
                action.targets.pop();
                if !action.targets.is_empty() {
                    gui_state.cursor_ray_result.take();
                }
            }

            if left_clicked {
                if let Some(target) = potential_target_instance {
                    if action.targets.len() < max_targets {
                        if !allow_duplicates && action.targets.contains(target) {
                            action.targets.retain(|t| t != target);
                        } else {
                            action.targets.push(target.clone());
                        }
                        gui_state.cursor_ray_result.take();
                    }
                }
            }

            // Status tooltip
            ui.tooltip(|| {
                ui.text("Targets:");
                ui.same_line();
                render_capacity_meter(
                    ui,
                    action.action_id.to_string().as_str(),
                    action.targets.len(),
                    max_targets,
                );
            });

            action.targets.len() == max_targets
        }

        TargetingKind::Area {
            shape,
            fixed_on_actor,
        } => {
            // Preview + highlighting is purely visual and can happen even without click
            if let Some(target) = potential_target_instance {
                let center_point = match target {
                    TargetInstance::Entity(entity) => {
                        systems::geometry::get_foot_position(&game_state.world, entity.id())
                            .unwrap()
                    }
                    TargetInstance::Point(point) => *point,
                };

                render_area_shape_preview(gui_state, shape, center_point);

                highlight_entities_in_area(
                    gui_state,
                    game_state,
                    shape,
                    action.actor.id(),
                    *fixed_on_actor,
                    center_point,
                );

                if left_clicked {
                    action.targets.clear();
                    action.targets.push(target.clone());
                    gui_state.cursor_ray_result.take();
                    return true;
                }
            }
            false
        }
    }
}

fn render_area_shape_preview(gui_state: &mut GuiState, shape: &AreaShape, center: Point3<f32>) {
    match shape {
        AreaShape::Sphere { radius } => {
            gui_state.line_renderer.add_circle(
                [center.x, center.y, center.z],
                radius.get::<meter>(),
                Color::White,
            );
        }
        _ => { /* TODO other shapes */ }
    }
}

fn highlight_entities_in_area(
    gui_state: &mut GuiState,
    game_state: &GameState,
    shape: &AreaShape,
    actor: Entity,
    fixed_on_actor: bool,
    center: Point3<f32>,
) {
    let (parry_shape, parry_pose) =
        shape.parry3d_shape(&game_state.world, actor, fixed_on_actor, &center);

    let affected_entities =
        systems::geometry::entities_in_shape(&game_state.world, parry_shape, &parry_pose);

    for entity in affected_entities {
        gui_state.creature_render_mode.insert(
            entity,
            MeshRenderMode::MeshWithWireFrame {
                color: [0.0, 1.0, 0.0, 0.5],
                width: 3.0,
            },
        );
    }
}

fn submit_action_decision(
    game_state: &mut GameState,
    action: &ActionData,
    path_to_target: &Option<PathResult>,
) {
    let response_to = if let Some(prompt) = game_state.next_prompt_entity(action.actor.id())
        && prompt.actors().contains(&action.actor.id())
    {
        info!("Submitting action in response to prompt: {:#?}", prompt);
        Some(prompt.id)
    } else {
        None
    };

    let action_kind = ActionDecisionKind::Action {
        action: action.clone(),
    };

    let decision = if let Some(response_to) = response_to {
        ActionDecision {
            response_to,
            kind: action_kind,
        }
    } else {
        ActionDecision::without_response_to(action_kind)
    };

    let activity = if let Some(path_to_target) = path_to_target {
        Activity::MoveAndAct {
            goal: *path_to_target.taken_path.end().unwrap(),
            action: decision,
        }
    } else {
        Activity::Act { action: decision }
    };
    let result = game_state.submit_activity(activity);
    info!("Submitted activity with result: {:#?}", result);
}

/// Handles Cancel button + right-click cancel rules and state transition back to Actions.
/// `submit_action` also returns to Action state (your original behavior).
fn handle_target_selection_footer(
    ui: &imgui::Ui,
    gui_state: &mut GuiState,
    game_state: &mut GameState,
    new_state: &mut Option<ActionBarState>,
    action: &ActionData,
    submit_action: bool,
) {
    let right_click_cancel = gui_state.cursor_ray_result.is_some()
        && ui.is_mouse_clicked(MouseButton::Right)
        && action.targets.is_empty();

    if right_click_cancel {
        gui_state.cursor_ray_result.take();
    }

    if ui.button("Cancel") || right_click_cancel || submit_action {
        *new_state = Some(ActionBarState::Action {
            actions: systems::actions::all_actions(&game_state.world, action.actor.id()),
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
            systems::geometry::get_eye_position(&game_state.world, action.actor.id()).unwrap(),
            None,
        ),

        TargetPathFindingResult::PathFound(path) => {
            if let Some(end) = path.taken_path.end()
                && let Some(end_at_ground) =
                    systems::geometry::ground_position(&game_state.geometry, &end)
                && let Some(eye_pos) = systems::geometry::get_eye_position_at_point(
                    &game_state.world,
                    action.actor.id(),
                    &end_at_ground,
                )
            {
                (eye_pos, Some(path.clone()))
            } else {
                (
                    systems::geometry::get_eye_position(&game_state.world, action.actor.id())
                        .unwrap(),
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
        action.actor.id(),
        &preview_position,
    ) && let Some(mesh) = gui_state.mesh_cache.get(&format!("{:#?}", shape))
    {
        if let Some(path_to_target) = path_to_target {
            gui_state
                .line_renderer
                .add_path_result(path_to_target, Color::White, Color::Red);
            let color = Color::White.with_alpha(0.75);
            mesh.draw(
                gui_state.ig_renderer.gl_context(),
                &gui_state.program,
                &shape_pose_at_preview.to_homogeneous(),
                color,
                &MeshRenderMode::WireFrameOnly { color, width: 2.0 },
            );
        }

        let line_end = match target {
            TargetInstance::Entity(entity) => {
                systems::geometry::get_shape(&game_state.world, entity.id())
                    .map(|(_, shape_pose)| shape_pose.translation.vector.into())
                    .unwrap()
            }
            TargetInstance::Point(point) => *point,
        };
        gui_state
            .line_renderer
            .add_line(preview_position.into(), line_end.into(), Color::White);
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
    let actor_position = systems::geometry::get_foot_position(&game_state.world, action.actor.id())
        .map(|point| [point.x, point.y, point.z])
        .unwrap();
    gui_state
        .line_renderer
        .add_circle(actor_position, normal_range, Color::White);
    if normal_range < max_range {
        gui_state
            .line_renderer
            .add_circle(actor_position, max_range, Color::Gray);
    }
}

fn update_potential_target(
    potential_target: &mut Option<(TargetInstance, TargetPathFindingResult)>,
    game_state: &mut GameState,
    action: &ActionData,
    targeting_context: &TargetingContext,
    closest: &RaycastHit,
) {
    if !should_render_target_preview(targeting_context) {
        // No point in computing potential target if we won't render the preview
        return;
    }

    let closest_target = match &closest.kind {
        RaycastHitKind::Creature(entity) => {
            TargetInstance::Entity(EntityIdentifier::from_world(&game_state.world, *entity))
        }
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
        // When using an action that can target multiple targets, we can't guarantee
        // that all other targets can be reached from the path to the new target, so
        // in this case we won't perform any pathfinding
        match &targeting_context.kind {
            TargetingKind::Multiple { .. } => {
                match targeting_context.validate_targets(
                    &game_state.world,
                    &game_state.geometry,
                    action.actor.id(),
                    &[closest_target.clone()],
                ) {
                    Ok(_) => {
                        *potential_target =
                            Some((closest_target, TargetPathFindingResult::AlreadyInRange))
                    }
                    Err(error) => {
                        trace!("New target {:?} is not valid: {:?}", closest_target, error);
                        *potential_target = None;
                    }
                }
            }
            _ => {
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
    }
}
