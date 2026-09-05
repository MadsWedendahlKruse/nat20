use imgui::TreeNodeFlags;
use nat20_core::{
    components::{
        actions::execution::{ActionExecution, ExecutionMailbox},
        activity::ActivityState,
    },
    engine::game_state::GameState,
    systems::{self, entities::EntityKind},
};

use crate::{
    render::common::utils::RenderableWithContext,
    state::{self, gui_state::GuiState},
    windows::anchor,
};

pub struct GameStateDebugWindow {}

impl GameStateDebugWindow {
    pub fn new() -> Self {
        Self {}
    }
}

impl RenderableWithContext<&mut GameState> for GameStateDebugWindow {
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let mut game_state_debug_open = *gui_state
            .settings
            .get::<bool>(state::parameters::RENDER_GAME_STATE_DEBUG);

        if !game_state_debug_open {
            return;
        }

        gui_state.window_manager.render_window(
            ui,
            "Game State Debug",
            &anchor::TOP_RIGHT,
            [0.0, 500.0],
            &mut game_state_debug_open,
            || {
                if ui.collapsing_header("Entities", TreeNodeFlags::empty()) {
                    let entities = game_state
                        .world
                        .query::<&EntityKind>()
                        .iter()
                        .filter_map(|(entity, kind)| {
                            if kind.is_creature() {
                                Some(entity)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    for entity in entities {
                        ui.text(format!("Entity {:?}", entity));

                        let activity_state = systems::helpers::get_component_mut::<ActivityState>(
                            &mut game_state.world,
                            entity,
                        );

                        ui.text(format!("Activity State: {:#?}", activity_state));

                        ui.separator();
                    }
                }

                if ui.collapsing_header("Interaction Engine", TreeNodeFlags::empty()) {
                    ui.text(format!("{:#?}", game_state.interaction_engine));
                }

                if ui.collapsing_header("Event Log", TreeNodeFlags::empty()) {
                    ui.indent();
                    if ui.collapsing_header("Events", TreeNodeFlags::empty()) {
                        for (i, event) in game_state.event_log.events.iter().enumerate() {
                            ui.indent();
                            if ui.collapsing_header(
                                format!("Event {}: {:?}", i, event.id),
                                TreeNodeFlags::empty(),
                            ) {
                                ui.text(format!("{:#?}", event));
                            }
                            ui.unindent();
                        }
                    }
                    ui.unindent();

                    ui.indent();
                    if ui.collapsing_header("Reactors", TreeNodeFlags::empty()) {
                        for (event_id, reactors) in game_state.event_log.reactors.iter() {
                            ui.indent();
                            if ui.collapsing_header(
                                format!("Event {:?} Reactors", event_id),
                                TreeNodeFlags::empty(),
                            ) {
                                for reactor in reactors {
                                    ui.text(format!("{:?}", reactor));
                                }
                            }
                            ui.unindent();
                        }
                    }
                    ui.unindent();

                    ui.indent();
                    if ui.collapsing_header("Action Events", TreeNodeFlags::empty()) {
                        for (action_instance_id, event_id) in
                            game_state.event_log.action_events.iter()
                        {
                            ui.text(format!(
                                "Action Instance {:?} -> Event {:?}",
                                action_instance_id, event_id
                            ));
                        }
                    }
                    ui.unindent();
                }

                // TODO: Since these are no longer stored on the GameState, should
                // they live somewhere else?
                if ui.collapsing_header("Action Executions", TreeNodeFlags::empty()) {
                    for (actor, (execution,)) in
                        game_state.world.query::<(&ActionExecution,)>().iter()
                    {
                        ui.indent();
                        if ui.collapsing_header(
                            format!("Action Instance {:?}", actor),
                            TreeNodeFlags::empty(),
                        ) {
                            ui.text(format!("{:#?}", execution));
                        }
                        ui.unindent();
                    }
                }

                if ui.collapsing_header("Execution Mailbox", TreeNodeFlags::empty()) {
                    for (actor, (mailbox,)) in
                        game_state.world.query::<(&ExecutionMailbox,)>().iter()
                    {
                        ui.indent();
                        if ui.collapsing_header(
                            format!("Mailbox for {:?}", actor),
                            TreeNodeFlags::empty(),
                        ) {
                            ui.text(format!("{:#?}", mailbox));
                        }
                        ui.unindent();
                    }
                }
            },
        );
    }
}
