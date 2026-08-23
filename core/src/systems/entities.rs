use crate::{
    engine::game_state::GameState,
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

pub fn update(game_state: &mut GameState, delta_time: f32) {
    let mut entities = game_state
        .world
        .query::<&EntityKind>()
        .iter()
        .map(|(entity, kind)| (entity, *kind))
        .collect::<Vec<_>>();
    entities.sort_by_key(|(_, kind)| *kind);

    for (entity, kind) in entities {
        match kind {
            EntityKind::Projectile => {
                entities::projectile::update(game_state, delta_time, entity);
            }

            EntityKind::Character | EntityKind::Monster => {
                entities::creature::update(game_state, delta_time, entity);
            }
        }
    }
}
