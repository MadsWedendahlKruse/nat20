use std::{
    collections::HashMap,
    str::FromStr,
    sync::{Arc, LazyLock},
};

use hecs::{Entity, World};
use parry3d::na::Point3;
use serde::{Deserialize, Serialize};
use tracing::trace;

use crate::{
    components::{
        actions::{action::ReactionTriggerFunction, targeting::TargetingRange},
        id::ScriptId,
    },
    engine::event::{Event, EventKind},
    scripts::script_api::{ScriptEntity, ScriptEventView, ScriptReactionTriggerContext},
    systems,
};

static REACTION_TRIGGER_DEFAULTS: LazyLock<HashMap<String, Arc<ReactionTriggerFunction>>> =
    LazyLock::new(|| {
        HashMap::from([(
            "opportunity_attack".to_string(),
            Arc::new(|world: &World, reactor: &Entity, event: &Event| {
                if let Some(actor) = event.actor()
                    && *reactor == actor
                {
                    // Can't opportunity attack yourself
                    return false;
                }

                let EventKind::MovementRequested {
                    entity,
                    path,
                    free_movement_distance,
                } = &event.kind
                else {
                    return false;
                };

                let loadout = systems::loadout::loadout(world, *reactor);
                let reactor_reach = loadout.melee_range();
                let Some(reactor_position) = systems::geometry::get_foot_position(world, *reactor)
                else {
                    return false;
                };

                let path_intersections = systems::geometry::path_intersections_within_radius(
                    path,
                    reactor_position,
                    reactor_reach.max(),
                );

                trace!(
                    "Opportunity attack trigger: entity {:?} moving along path {:?} with free movement distance {:?} has {:?} intersections ({:?}) with reactor {:?} at position {:?} with reach {:?}",
                    entity,
                    path,
                    free_movement_distance,
                    path_intersections.len(),
                    path_intersections,
                    reactor,
                    reactor_position,
                    reactor_reach
                );

                if evaluate_opportunity_attacks(
                    world,
                    reactor,
                    entity,
                    reactor_reach,
                    &path_intersections,
                ) {
                    for intersection in &path_intersections {
                        if let Some(distance_along_path) = path.distance_along_path(intersection)
                            && distance_along_path > *free_movement_distance
                        {
                            return true;
                        }
                    }
                }

                return false;
            }) as Arc<ReactionTriggerFunction>,
        )])
    });

fn evaluate_opportunity_attacks(
    world: &World,
    reactor: &Entity,
    entity: &Entity,
    reactor_reach: &TargetingRange,
    path_intersections: &Vec<Point3<f32>>,
) -> bool {
    // Scenario 1: No intersections
    // Entity either doesn't come within reach or doesn't leave reach, so no opportunity attack.
    if path_intersections.is_empty() {
        return false;
    }

    // Scenario 2: One intersection
    // 2a: Entity starts outside of reach and enters reach: no opportunity attack
    // 2b: Entity starts inside of reach and leaves reach: opportunity attack
    if path_intersections.len() == 1 {
        let Some(distance_to_entity_start_position) =
            systems::geometry::distance_between_entities(world, *entity, *reactor)
        else {
            return false;
        };

        if distance_to_entity_start_position > reactor_reach.max() {
            // Scenario 2a
            return false;
        } else {
            // Scenario 2b
            return true;
        }
    }

    // Scenario 3: More than one intersection
    // Entity enters and leaves reach at least once, so opportunity attack.
    true
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ReactionTrigger {
    pub raw: String,
    pub function: Arc<ReactionTriggerFunction>,
    pub script: Option<ScriptId>,
}

impl FromStr for ReactionTrigger {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(function) = REACTION_TRIGGER_DEFAULTS.get(s) {
            return Ok(ReactionTrigger {
                raw: s.to_string(),
                function: function.clone(),
                script: None,
            });
        }

        let script_id: ScriptId = s.parse().map_err(|_| {
            format!(
                "Failed to parse reaction trigger: '{}'. Not found in defaults and not a valid script ID.",
                s
            )
        })?;
        let script = Some(script_id.clone());

        Ok(ReactionTrigger {
            raw: s.to_string(),
            function: Arc::new(move |_world, reactor, event| {
                let Some(script_event) = ScriptEventView::from_event(event) else {
                    return false;
                };
                let context = ScriptReactionTriggerContext {
                    reactor: ScriptEntity::from(*reactor),
                    event: script_event,
                };
                systems::scripts::evaluate_reaction_trigger(&script_id, &context)
            }),
            script,
        })
    }
}

impl TryFrom<String> for ReactionTrigger {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<ReactionTrigger> for String {
    fn from(trigger: ReactionTrigger) -> Self {
        trigger.raw
    }
}
