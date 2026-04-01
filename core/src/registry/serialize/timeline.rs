use serde::{Deserialize, Serialize};

use crate::components::actions::action::{ActionTimeline, ActionTimelineEvent};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionTimelineDefinition {
    pub total_duration: f32,
    #[serde(default)]
    pub step_spacing: f32,
    pub events: Vec<(f32, ActionTimelineEventDefinition)>,
}

impl From<ActionTimelineDefinition> for ActionTimeline {
    fn from(def: ActionTimelineDefinition) -> Self {
        ActionTimeline {
            total_duration: def.total_duration,
            step_spacing: def.step_spacing,
            events: def
                .events
                .into_iter()
                .map(|(time, event_def)| (time, event_def.into()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ActionTimelineEventDefinition {
    SubmitAction,
    SpawnProjectile {
        // TODO: Populate
    },
}

impl From<ActionTimelineEventDefinition> for ActionTimelineEvent {
    fn from(def: ActionTimelineEventDefinition) -> Self {
        match def {
            ActionTimelineEventDefinition::SubmitAction => ActionTimelineEvent::SubmitAction,
            ActionTimelineEventDefinition::SpawnProjectile { .. } => {
                ActionTimelineEvent::SpawnProjectile { /* TODO */ }
            }
        }
    }
}
