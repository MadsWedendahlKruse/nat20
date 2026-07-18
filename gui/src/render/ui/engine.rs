use hecs::World;
use imgui::{MouseButton, TreeNodeFlags};
use nat20_core::{
    components::{
        actions::targeting::TargetInstance, spells::spell::ConcentrationInstance,
        time::TurnBoundary,
    },
    engine::{
        action_prompt::ActionData,
        event::{EncounterEvent, Event, EventKind, EventLog},
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20ResultKind},
    },
};
use strum::{Display, EnumIter};

use crate::render::ui::{
    components::new_life_state_text,
    text::{TextKind, TextSegment, TextSegments},
    utils::{ImguiRenderable, ImguiRenderableWithContext},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, EnumIter, Display)]
pub enum LogLevel {
    Info,
    Debug,
}

impl From<usize> for LogLevel {
    fn from(value: usize) -> Self {
        match value {
            0 => LogLevel::Info,
            1 => LogLevel::Debug,
            _ => LogLevel::Info,
        }
    }
}

pub fn event_log_level(event: &Event) -> LogLevel {
    match &event.kind {
        EventKind::Encounter(encounter_event) => LogLevel::Info,
        EventKind::MovingOutOfReach { .. } => LogLevel::Debug,
        EventKind::ActionRequested { .. } => LogLevel::Info,
        EventKind::LifeStateChanged { .. } => LogLevel::Info,
        EventKind::D20CheckPerformed { result, .. }
        | EventKind::D20CheckResolved { result, .. } => match result {
            D20ResultKind::SavingThrow { .. } | D20ResultKind::Skill { .. } => LogLevel::Info,
            systems::d20::D20ResultKind::AttackRoll { .. } => LogLevel::Debug,
        },
        EventKind::DamageRollPerformed { .. } => LogLevel::Debug,
        EventKind::DamageRollResolved { .. } => LogLevel::Debug,
        EventKind::TurnBoundary { .. } => LogLevel::Info,
        EventKind::RestStarted { .. } => LogLevel::Info,
        EventKind::RestFinished { .. } => LogLevel::Info,
        EventKind::LostConcentration { .. } => LogLevel::Info,
        EventKind::ActionResult { .. } => LogLevel::Info,
    }
}

pub fn render_action_description(ui: &imgui::Ui, action: &ActionData) {
    TextSegments::new(vec![
        (
            format!("{}'s", action.actor.name().as_str()),
            TextKind::Actor,
        ),
        (format!("{}", action.action_id), TextKind::Action),
    ])
    .render(ui);
}

pub fn render_event_description(ui: &imgui::Ui, event: &Event) {
    match &event.kind {
        EventKind::ActionRequested { action } => {
            render_action_description(ui, action);
        }
        EventKind::ActionResult { result, actor } => {
            if let Some(actor) = actor {
                TextSegments::new(vec![
                    (format!("{}'s", actor.name().as_str()), TextKind::Actor),
                    ("action targeting".to_string(), TextKind::Normal),
                    (
                        format!("{}", result.target.name().as_str()),
                        TextKind::Target,
                    ),
                ])
                .render(ui);
            } else {
                TextSegments::new(vec![("TODO: Action result".to_string(), TextKind::Details)])
                    .render(ui);
            }
        }
        EventKind::D20CheckPerformed { actor, dc, .. } => {
            let label = get_dc_description(dc);
            let mut segments = vec![(format!("{}'s", actor.name().as_str()), TextKind::Actor)];
            segments.extend(label);
            TextSegments::new(segments).render(ui);
        }
        EventKind::MovingOutOfReach { mover, entity, .. } => {
            TextSegments::new(vec![
                (mover.name().as_str(), TextKind::Actor),
                ("moving out of reach of", TextKind::Normal),
                (entity.name().as_str(), TextKind::Target),
            ])
            .render(ui);
        }
        _ => TextSegments::new(vec![(format!("{:?}", event.kind), TextKind::Details)]).render(ui),
    };
}

pub fn events_match(event1: &Event, event2: &Event) -> bool {
    match (&event1.kind, &event2.kind) {
        (
            EventKind::D20CheckPerformed { actor: e1, .. },
            EventKind::D20CheckResolved { actor: e2, .. },
        ) => e1 == e2,

        (
            EventKind::DamageRollPerformed { actor: e1, .. },
            EventKind::DamageRollResolved { actor: e2, .. },
        ) => e1 == e2,

        _ => false,
    }
}

pub fn filter_matching_events(events: Vec<&Event>) -> Vec<&Event> {
    let mut filtered_events = Vec::new();

    for (i, event) in events.iter().enumerate() {
        if i < events.len() - 1 {
            let next_event = events[i + 1];
            if events_match(event, next_event) {
                continue;
            }
        }

        filtered_events.push(*event);
    }

    filtered_events
}

fn should_render_as_top_level(log: &EventLog, event: &Event) -> bool {
    if event.parent.is_none() {
        return true;
    }

    // If the parent event triggered a reaction, it's going to look weird regarding
    // the "chronology" of the events if we render the child event as a child of the
    // event which triggerd the reaction. Suppose you have something like:
    // Event A (triggers reaction)
    // Event B (reaction to A)
    //
    // If we render a new Event C as a child of Event A, it will look like:
    // Event A
    //   Event C
    // Event B
    //
    // This makes it look like Event C happened after Event A but before Event B,
    // which is not true. So, if the parent event triggered a reaction, we will render
    // the child event as a top-level event instead of a child of the parent event.
    if let Some(parent_event) = event.parent
        && log.triggered_reactions(&parent_event)
    {
        return true;
    }

    false
}

impl ImguiRenderableWithContext<&(&World, &LogLevel)> for EventLog {
    fn render_with_context(&self, ui: &imgui::Ui, context: &(&World, &LogLevel)) {
        let (world, log_level) = context;

        let mut log_level_events = self
            .events
            .iter()
            .filter(|event| should_render_as_top_level(self, event))
            .filter(|event| event_log_level(event) <= **log_level)
            .collect::<Vec<_>>();

        if **log_level == LogLevel::Info {
            log_level_events = filter_matching_events(log_level_events);
        }

        for event in log_level_events {
            event.render_with_context(ui, &(world, log_level, Some(self)));
        }
    }
}

impl ImguiRenderableWithContext<&(&World, &LogLevel, Option<&EventLog>)> for Event {
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        context: &(&World, &LogLevel, Option<&EventLog>),
    ) {
        let (world, log_level, event_log) = context;

        if event_log_level(self) > **log_level {
            return;
        }

        let group_token = ui.begin_group();

        match &self.kind {
            EventKind::Encounter(encounter_event) => match encounter_event {
                EncounterEvent::EncounterStarted(encounter_id) => {
                    ui.separator_with_text(&format!("Encounter {}", encounter_id));
                }
                EncounterEvent::EncounterEnded(encounter_id, combat_log) => {
                    if ui.collapsing_header(format!("Log##{}", encounter_id), TreeNodeFlags::FRAMED)
                    {
                        combat_log.render_with_context(ui, &(world, log_level));
                    }
                    ui.separator();
                }
                EncounterEvent::NewRound(encounter_id, round) => {
                    ui.separator_with_text(format!("Round {}", round));
                }
            },
            EventKind::MovingOutOfReach { mover, entity } => {
                TextSegments::new(vec![
                    (mover.name().as_str(), TextKind::Actor),
                    ("is moving out of reach of", TextKind::Normal),
                    (entity.name().as_str(), TextKind::Target),
                ])
                .render(ui);
            }
            EventKind::ActionRequested { action } => {
                action.render_with_context(ui, world);

                if let Some(trigger_event) = action.trigger_event.as_ref() {
                    TextSegment::new("as a response to".to_string(), TextKind::Normal).render(ui);
                    ui.same_line();
                    render_event_description(ui, trigger_event);
                }
            }
            EventKind::LifeStateChanged {
                entity,
                new_state,
                actor,
            } => {
                let segments = new_life_state_text(
                    entity.name().as_str(),
                    new_state,
                    actor.as_ref().map(|a| a.name().as_str()),
                );
                TextSegments::new(segments).render(ui);
            }
            EventKind::D20CheckResolved { actor, result, dc }
            | EventKind::D20CheckPerformed { actor, result, dc } => {
                let dc_text_segments = get_dc_description(dc);

                let mut segments = vec![
                    (actor.name().to_string(), TextKind::Actor),
                    (
                        if result.is_success(dc) {
                            "succeeded a".to_string()
                        } else {
                            "failed a".to_string()
                        },
                        TextKind::Normal,
                    ),
                ];

                segments.extend(dc_text_segments);

                TextSegments::new(segments).render(ui);

                if ui.is_item_hovered() {
                    ui.tooltip(|| {
                        ui.text("DC:");
                        ui.same_line();
                        dc.render(ui);
                        ui.text("");
                        ui.text("D20 Check:");
                        ui.same_line();
                        result.render(ui);
                        ui.same_line();
                        if result.is_success(dc) {
                            TextSegment::new("Success", TextKind::Green).render(ui);
                        } else {
                            TextSegment::new("Failure", TextKind::Red).render(ui);
                        }
                    });
                }
            }
            EventKind::DamageRollPerformed { actor, result }
            | EventKind::DamageRollResolved { actor, result } => {
                TextSegments::new(vec![
                    (actor.name().as_str(), TextKind::Details),
                    ("rolled", TextKind::Details),
                    (&format!("{}", result.total), TextKind::Details),
                    ("damage", TextKind::Details),
                ])
                .render(ui);

                if ui.is_item_hovered() {
                    ui.tooltip(|| {
                        result.render(ui);
                    });
                }
            }
            // TODO: Not sure how to render this?
            EventKind::TurnBoundary { entity, boundary } => {
                let boundary_text = match boundary {
                    TurnBoundary::Start => "Starting",
                    TurnBoundary::End => "Ending",
                };
                TextSegments::new(vec![(
                    format!("{} the turn for {}", boundary_text, entity.name().as_str()),
                    TextKind::Details,
                )])
                .render(ui);
            }
            // TODO: Improve rest event rendering
            EventKind::RestStarted { kind, participants } => {
                TextSegments::new(vec![
                    ("Started".to_string(), TextKind::Normal),
                    (format!("{:?} rest for", kind), TextKind::Normal),
                ])
                .render(ui);

                ui.same_line();

                participants
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
                    .render_with_context(ui, &world);
            }
            EventKind::RestFinished { kind, participants } => {
                TextSegments::new(vec![
                    ("Finished".to_string(), TextKind::Normal),
                    (format!("{:?} rest for", kind), TextKind::Normal),
                ])
                .render(ui);

                ui.same_line();

                participants
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
                    .render_with_context(ui, &world);
            }
            EventKind::LostConcentration { entity, instances } => {
                let instance_descriptions = instances
                    .iter()
                    .map(|instance| match instance {
                        ConcentrationInstance::Effect { effect, .. } => {
                            (effect.to_string(), TextKind::Effect)
                        }
                    })
                    .collect::<Vec<_>>();

                TextSegments::new(vec![
                    (entity.name().as_str(), TextKind::Actor),
                    ("lost concentration on", TextKind::Normal),
                ])
                .render(ui);

                // TODO: Just render the first for now
                ui.same_line();
                TextSegment::new(
                    instance_descriptions[0].0.clone(),
                    instance_descriptions[0].1.clone(),
                )
                .render(ui);

                // if instance_descriptions.len() == 1 {
                //     ui.same_line();
                //     TextSegment::new(
                //         instance_descriptions[0].0.clone(),
                //         instance_descriptions[0].1.clone(),
                //     )
                //     .render(ui);
                // } else {
                //     TextSegments::new(instance_descriptions)
                //         .with_indent(1)
                //         .render(ui);
                // }
            }

            EventKind::ActionResult { result, actor } => {
                for component in result.components.iter() {
                    component
                        .render_with_context(ui, (&actor.as_ref(), result.target.name().as_str()));
                }
            }
        }

        group_token.end();

        if ui.is_item_hovered() && ui.is_mouse_clicked(MouseButton::Right) {
            ui.open_popup(self.id.to_string());
        }

        // TODO: Event debug doesn't work for EncounterEnded (it just renders everything)
        ui.popup(self.id.to_string(), || {
            let debug_text = format!("{:#?}", self);
            let size = ui.calc_text_size(&debug_text);
            ui.child_window("Event Debug Info")
                .size([size[0] + 10.0, f32::min(size[1], 400.0)])
                .build(|| {
                    ui.text(debug_text);
                });
        });

        if let Some(event_log) = event_log {
            let mut child_events = event_log
                .child_events(&self.id)
                .iter()
                .filter(|child_event| !should_render_as_top_level(event_log, child_event))
                .filter_map(|child_event| {
                    if event_log_level(*child_event) <= **log_level {
                        Some(*child_event)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if **log_level == LogLevel::Info {
                child_events = filter_matching_events(child_events);
            }

            for child_event in child_events {
                ui.indent();
                child_event.render_with_context(ui, context);
                ui.unindent();
            }
        }
    }
}

fn get_dc_description(dc_kind: &D20CheckDCKind) -> Vec<(String, TextKind)> {
    match dc_kind {
        D20CheckDCKind::SavingThrow(dc) => vec![
            (dc.key.to_string(), TextKind::Ability),
            ("saving throw".to_string(), TextKind::Normal),
        ],
        D20CheckDCKind::Skill(dc) => vec![
            (dc.key.to_string(), TextKind::Ability),
            ("check".to_string(), TextKind::Normal),
        ],
        D20CheckDCKind::AttackRoll(target, _, _) => {
            vec![
                ("attack roll against".to_string(), TextKind::Normal),
                (target.name().to_string(), TextKind::Actor),
            ]
        }
    }
}

impl ImguiRenderableWithContext<&World> for ActionData {
    fn render_with_context(&self, ui: &imgui::Ui, world: &World) {
        TextSegments::new(vec![
            (self.actor.name().as_str(), TextKind::Actor),
            ("is using", TextKind::Normal),
            (&self.action_id.to_string(), TextKind::Action),
        ])
        .render(ui);

        if !self.is_self_target() && !self.targets.is_empty() {
            let targets = self
                .targets
                .iter()
                .filter_map(|t| match t {
                    TargetInstance::Entity { entity, .. } => Some(entity.clone()),
                    TargetInstance::Point(point) => None,
                })
                .collect::<Vec<_>>();

            ui.same_line();
            TextSegment::new("on", TextKind::Normal).render(ui);
            targets.render_with_context(ui, &world);
        }
    }
}
