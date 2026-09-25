use hecs::Entity;

use crate::{
    engine::engine_state::EngineState,
    entities::{self},
};

/// The different kinds of entities that can exist in the world.
///
/// Primarily used to differentiate between them, as well as defining how they should
/// be updated each tick (see [`self::update`]). Note that the order of the variants
/// is important, as it defines the order in which they will be updated, e.g. projectiles
/// are updated before creatures, so that they can hit them in the same tick.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EntityKind {
    Projectile,
    Character,
    Monster,
}

impl EntityKind {
    pub fn is_creature(&self) -> bool {
        matches!(self, EntityKind::Character | EntityKind::Monster)
    }
}

pub fn update(engine_state: &mut EngineState, delta_time: f32) {
    for (entity, kind) in get_entities_to_update(engine_state) {
        match kind {
            EntityKind::Projectile => {
                entities::projectile::update(engine_state, delta_time, entity);
            }

            EntityKind::Character | EntityKind::Monster => {
                entities::creature::update(engine_state, delta_time, entity);
            }
        }
    }
}

fn get_entities_to_update(engine_state: &EngineState) -> Vec<(Entity, EntityKind)> {
    let mut entities = engine_state
        .world
        .query::<&EntityKind>()
        .iter()
        .filter_map(|(entity, kind)| {
            if should_update(engine_state, entity, kind) {
                Some((entity, *kind))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    entities.sort_by_key(|(_, kind)| *kind);

    entities
}

fn should_update(engine_state: &EngineState, entity: Entity, kind: &EntityKind) -> bool {
    match kind {
        EntityKind::Projectile => entities::projectile::should_update(engine_state, entity),
        EntityKind::Character | EntityKind::Monster => {
            entities::creature::should_update(engine_state, entity)
        }
    }
}
