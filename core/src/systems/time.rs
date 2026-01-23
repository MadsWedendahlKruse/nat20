use std::collections::HashMap;

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};
use tracing::{debug, error, info};
use tracing_subscriber::field::debug;

use crate::{
    components::{
        health::hit_points::HitPoints,
        resource::RechargeRule,
        time::{EntityClock, TimeMode, TimeStep},
    },
    engine::{
        event::{ActionError, Event, EventKind},
        game_state::GameState,
    },
    systems,
};

pub fn set_time_mode(world: &mut World, entity: Entity, mode: TimeMode) {
    let mut clock = systems::helpers::get_component_mut::<EntityClock>(world, entity);
    clock.set_mode(mode);
}

pub fn advance_time(world: &mut World, entity: Entity, time_step: TimeStep) {
    // TODO: Recharge resources on time advance?
    {
        let mut clock = systems::helpers::get_component_mut::<EntityClock>(world, entity);

        if clock.mode() == TimeMode::Paused {
            return;
        }

        match (clock.mode(), time_step) {
            (TimeMode::RealTime, TimeStep::TurnBoundary { .. })
            | (TimeMode::TurnBased { .. }, TimeStep::RealTime { .. }) => {
                return;
            }
            _ => { /* valid combination, continue */ }
        }

        clock.update(time_step);
    }

    match time_step {
        TimeStep::TurnBoundary {
            entity: turn_entity,
            boundary,
        } => {
            debug!(
                "Advancing turn-based time for entity {:?} at turn boundary {:?} of entity {:?}",
                entity, boundary, turn_entity
            );
        }
        _ => { /* no special logging for other time steps */ }
    }

    let mut expired_effects = Vec::new();
    for (id, effect) in systems::effects::effects_mut(world, entity).iter_mut() {
        effect.advance_time(time_step);
        if effect.is_expired() {
            expired_effects.push(id.clone());
        }
    }

    systems::effects::remove_effects(world, entity, &expired_effects);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RestKind {
    Short,
    Long,
}

#[derive(Debug, Clone)]
pub enum RestError {
    InCombat { entities: Vec<Entity> },
    NotResting { entities: Vec<Entity> },
    DifferentRestKinds { entities: HashMap<Entity, RestKind> },
    ActionError(ActionError),
}

pub fn on_turn_start(world: &mut World, entity: Entity) {
    debug!("Starting turn for entity {:?}", entity);
    systems::resources::recharge(world, entity, &RechargeRule::Turn);
    systems::movement::recharge_movement(world, entity);
}

pub fn on_turn_end(_world: &mut World, _entity: Entity) {
    debug!("Ending turn for entity {:?}", _entity);
    // Currently nothing happens at turn end
}

pub fn start_rest(
    game_state: &mut GameState,
    participants: Vec<Entity>,
    kind: &RestKind,
) -> Result<(), RestError> {
    info!("Starting {:?} rest for entities {:?}", kind, participants);

    let entities_in_combat = entities_in_combat(game_state, &participants);
    if !entities_in_combat.is_empty() {
        error!("Entities in combat cannot rest: {:?}", entities_in_combat);
        return Err(RestError::InCombat {
            entities: entities_in_combat,
        });
    }

    let event = Event::new(EventKind::RestStarted {
        kind: *kind,
        participants: participants.clone(),
    });
    let result = game_state
        .process_event(event)
        .map_err(RestError::ActionError);

    participants.iter().for_each(|&entity| {
        game_state.resting.insert(entity, *kind);
    });

    result
}

pub fn finish_rest(game_state: &mut GameState, participants: Vec<Entity>) -> Result<(), RestError> {
    info!("Finishing rest for entities {:?}", participants);

    // Check that all participants are actually resting and of the same kind
    let mut not_resting_entities: Vec<Entity> = Vec::new();
    let mut rest_kinds = HashMap::new();
    for &entity in &participants {
        if let Some(kind) = game_state.resting.remove(&entity) {
            rest_kinds.insert(entity, kind);
        } else {
            not_resting_entities.push(entity);
        }
    }

    if !not_resting_entities.is_empty() {
        error!("Entities not resting: {:?}", not_resting_entities);
        return Err(RestError::NotResting {
            entities: not_resting_entities,
        });
    }

    let first_kind = rest_kinds
        .values()
        .next()
        .expect("At least one participant should be present");

    if rest_kinds.values().any(|kind| kind != first_kind) {
        error!("Entities with different rest kinds: {:#?}", rest_kinds);
        return Err(RestError::DifferentRestKinds {
            entities: rest_kinds,
        });
    }

    let event = Event::new(EventKind::RestFinished {
        kind: *first_kind,
        participants: participants.clone(),
    });
    game_state
        .process_event(event)
        .map_err(RestError::ActionError)?;

    on_rest_end(&mut game_state.world, &participants, first_kind);

    Ok(())
}

fn entities_in_combat(game_state: &GameState, participants: &[Entity]) -> Vec<Entity> {
    // Can only rest if no one is in combat
    participants
        .iter()
        .cloned()
        .filter(|entity| game_state.in_combat.contains_key(entity))
        .collect()
}

pub fn on_rest_end(world: &mut World, participants: &[Entity], kind: &RestKind) {
    for &entity in participants {
        match kind {
            RestKind::Short => {
                systems::resources::recharge(world, entity, &RechargeRule::Rest(RestKind::Short));
                // SRD says we should spend Hit Dice here, but for now it's easier
                // to just heal half our max HP
                let half_max_hp =
                    systems::helpers::get_component::<HitPoints>(world, entity).max() / 2;
                systems::health::heal(world, entity, half_max_hp);
            }

            RestKind::Long => {
                systems::resources::recharge(world, entity, &RechargeRule::Rest(RestKind::Long));
                systems::health::heal_full(world, entity);
                // TODO: Remove non-permanent effects?
            }
        }
    }
}
