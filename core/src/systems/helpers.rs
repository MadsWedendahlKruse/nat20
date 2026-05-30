use std::{any::type_name, ops::Deref};

use hecs::{Entity, Ref, World};
use tracing::error;

use crate::components::level::{ChallengeRating, CharacterLevels, Level};

#[track_caller]
pub fn get_component<'a, T: hecs::Component + 'static>(
    world: &'a World,
    entity: Entity,
) -> Ref<'a, T> {
    world
        .get::<&T>(entity)
        .unwrap_or_else(|_| missing_component_panic::<T>(entity))
}

#[track_caller]
pub fn get_component_mut<'a, T: hecs::Component + 'static>(
    world: &'a mut World,
    entity: Entity,
) -> hecs::RefMut<'a, T> {
    world
        .get::<&mut T>(entity)
        .unwrap_or_else(|_| missing_component_panic::<T>(entity))
}

#[track_caller]
pub fn get_component_clone<T: hecs::Component + Clone>(world: &World, entity: Entity) -> T {
    get_component::<T>(world, entity).deref().clone()
}

#[track_caller]
pub fn set_component<T: hecs::Component + Clone>(world: &mut World, entity: Entity, value: T) {
    world
        .insert_one(entity, value)
        .unwrap_or_else(|_| missing_component_panic::<T>(entity));
}

#[track_caller]
fn missing_component_panic<T: 'static>(entity: Entity) -> ! {
    let type_name = type_name::<T>();

    if type_name.starts_with('&') {
        error!(
            "❗️ You likely passed a reference type to a helper expecting a component.\n\
            `get_component::<{}>()` is incorrect — try `get_component::<{}>()` instead.",
            type_name,
            &type_name[1..].trim()
        );
    }

    panic!(
        "Entity {:?} is missing component of type `{}`",
        entity, type_name
    );
}

pub fn level(world: &World, entity: Entity) -> Option<Ref<'_, dyn Level>> {
    if let Ok(level) = world.get::<&CharacterLevels>(entity) {
        return Some(Ref::map(level, |l| l as &dyn Level));
    }

    if let Ok(level) = world.get::<&ChallengeRating>(entity) {
        return Some(Ref::map(level, |l| l as &dyn Level));
    }

    None
}
