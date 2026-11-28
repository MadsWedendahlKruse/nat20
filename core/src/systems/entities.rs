use hecs::Entity;
use serde::{Deserialize, Serialize};

use crate::{
    engine::engine_state::EngineState,
    entities::{
        common::EntityUpdater,
        creature::{CharacterUpdater, MonsterUpdater},
        projectile::ProjectileUpdater,
    },
};

/// The different kinds of entities that can exist in the world.
///
/// Primarily used to differentiate between them, as well as defining how they should
/// be updated each tick (see [`self::update`]). Note that the order of the variants
/// is important, as it defines the order in which they will be updated, e.g. projectiles
/// are updated before creatures, so that they can hit them in the same tick.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EntityKind {
    Projectile,
    Character,
    Monster,
}

impl EntityKind {
    pub fn is_creature(&self) -> bool {
        matches!(self, EntityKind::Character | EntityKind::Monster)
    }

    pub fn updater(&self) -> &'static dyn EntityUpdater {
        match self {
            EntityKind::Projectile => &ProjectileUpdater,
            EntityKind::Character => &CharacterUpdater,
            EntityKind::Monster => &MonsterUpdater,
        }
    }
}

pub fn update(engine_state: &mut EngineState, delta_time: f32) {
    for (entity, kind) in get_entities_to_update(engine_state) {
        kind.updater().update(engine_state, delta_time, entity);
    }
}

fn get_entities_to_update(engine_state: &EngineState) -> Vec<(Entity, EntityKind)> {
    let mut entities = engine_state
        .world
        .query::<&EntityKind>()
        .iter()
        .filter_map(|(entity, kind)| {
            if kind.updater().should_update(engine_state, entity) {
                Some((entity, *kind))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    entities.sort_by_key(|(_, kind)| *kind);

    entities
}

pub fn should_update(engine_state: &EngineState, entity: Entity) -> bool {
    if let Ok(kind) = engine_state.world.get::<&EntityKind>(entity) {
        return kind.updater().should_update(engine_state, entity);
    }
    false
}
