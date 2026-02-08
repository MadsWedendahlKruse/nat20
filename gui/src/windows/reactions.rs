use std::collections::HashMap;

use hecs::Entity;
use nat20_core::{
    components::id::Name,
    engine::{
        action_prompt::{ActionDecision, ActionDecisionKind, ActionPromptId, ReactionData},
        event::Event,
        game_state::GameState,
    },
    systems,
};
use tracing::{error, info};

use crate::{
    render::{
        common::utils::RenderableMutWithContext,
        ui::{
            engine::LogLevel,
            utils::{ImguiRenderableWithContext, render_button_selectable},
        },
    },
    state::gui_state::GuiState,
    windows::anchor::{AUTO_RESIZE, BOTTOM_CENTER, CENTER},
};

pub enum ReactionWindowState {
    Active {
        prompt_id: ActionPromptId,
        event: Event,
        options: HashMap<Entity, Vec<ReactionData>>,
    },
    Pending,
}

pub struct ReactionsWindow {
    state: ReactionWindowState,
}

impl ReactionsWindow {
    pub fn new() -> Self {
        Self {
            state: ReactionWindowState::Pending,
        }
    }

    pub fn is_active(&self) -> bool {
        matches!(self.state, ReactionWindowState::Active { .. })
    }

    pub fn activate(
        &mut self,
        prompt_id: ActionPromptId,
        event: &Event,
        options: &HashMap<Entity, Vec<ReactionData>>,
    ) {
        info!("Activating reactions window for prompt {:?}", prompt_id);
        self.state = ReactionWindowState::Active {
            prompt_id,
            event: event.clone(),
            options: options.clone(),
        };
    }
}

impl RenderableMutWithContext<&mut GameState> for ReactionsWindow {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let mut new_state = None;

        match &self.state {
            ReactionWindowState::Pending => return,
            ReactionWindowState::Active {
                prompt_id,
                event,
                options,
            } => gui_state.window_manager.render_window(
                ui,
                "Reactions",
                &CENTER,
                AUTO_RESIZE,
                &mut true,
                || {
                    if options.is_empty() {
                        ui.text("No reactions available.");
                    }

                    event.render_with_context(ui, &(&game_state.world, &LogLevel::Info));

                    ui.text("Choose how to react:");

                    let decisions = game_state
                        .session_for_entity(*options.keys().next().unwrap())
                        .and_then(|session| session.decisions_for_prompt(prompt_id));

                    let (mut button_clicked, mut entity, mut choice) = (false, None, None);

                    for (reactor, options) in options {
                        if !systems::ai::is_player_controlled(&game_state.world, *reactor) {
                            continue;
                        }

                        ui.separator_with_text(
                            systems::helpers::get_component_clone::<Name>(
                                &game_state.world,
                                *reactor,
                            )
                            .as_str(),
                        );

                        for option in options {
                            let option_selected = if let Some(decisions) = decisions
                                && let Some(decision) = decisions.get(reactor)
                            {
                                match &decision.kind {
                                    ActionDecisionKind::Reaction { choice, .. } => {
                                        choice.as_ref() == Some(option)
                                    }
                                    ActionDecisionKind::Action { .. } => false,
                                }
                            } else {
                                false
                            };

                            if render_button_selectable(
                                ui,
                                format!(
                                    "{}##{:?}{:?}",
                                    option.reaction_id, option.resource_cost, reactor
                                ),
                                [0., 0.],
                                option_selected,
                            ) {
                                (button_clicked, entity, choice) =
                                    (true, Some(reactor), Some(option.clone()));
                            }

                            if ui.is_item_hovered() {
                                ui.tooltip(|| {
                                    (&option.reaction_id, &option.context, &option.resource_cost)
                                        .render_with_context(ui, (&game_state.world, *reactor));
                                });
                            }
                        }

                        if options.len() > 0 {
                            ui.separator();
                            if ui.button(format!("Don't react##{:?}", reactor)) {
                                (button_clicked, entity, choice) = (true, Some(reactor), None);
                            }
                        }
                    }

                    ui.separator();

                    let mut decision_keys = decisions
                        .map(|decisions| decisions.keys().cloned().collect::<Vec<Entity>>());

                    if button_clicked && let Some(reactor) = entity {
                        info!("Submitting reaction decision for reactor {:?}...", reactor);
                        let result = game_state.submit_decision(ActionDecision {
                            response_to: *prompt_id,
                            kind: ActionDecisionKind::Reaction {
                                event: event.clone(),
                                reactor: *reactor,
                                choice,
                            },
                        });
                        match result {
                            Ok(()) => {
                                if let Some(keys) = &mut decision_keys {
                                    info!("Submitted reaction decision for reactor {:?}", reactor);
                                    keys.push(*reactor);
                                }
                            }
                            Err(action_error) => {
                                error!("Failed to submit reaction decision: {:#?}", action_error);
                            }
                        }
                    }

                    if let Some(decisions) = decision_keys
                        && options.keys().all(|entity| {
                            !systems::ai::is_player_controlled(&game_state.world, *entity)
                                || decisions.contains(entity)
                        })
                    {
                        info!("All reactions submitted, closing window.");
                        new_state = Some(ReactionWindowState::Pending);
                    }
                },
            ),
        }

        if let Some(state) = new_state {
            self.state = state;
        }
    }
}
