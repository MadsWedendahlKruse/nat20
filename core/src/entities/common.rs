use hecs::{DynamicBundle, Entity};

use crate::engine::engine_state::EngineState;

#[macro_export]
macro_rules! from_world {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $(
                $(#[$field_meta:meta])*
                $field_vis:vis $field:ident : $ty:ty,
            )*
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $(
                $(#[$field_meta])*
                $field_vis $field : $ty,
            )*
        }

        impl $name {
            pub fn from_world(world: &hecs::World, entity: hecs::Entity) -> Self {
                Self {
                    $(
                        $field: $crate::systems::helpers::get_component_clone(world, entity),
                    )*
                }
            }
        }
    }
}

pub trait EntityUpdater {
    fn should_update(&self, engine_state: &EngineState, entity: Entity) -> bool;
    fn update(&self, engine_state: &mut EngineState, delta_time: f32, entity: Entity);
}

pub trait EntityTemplate {
    type TemplateError;

    fn spawn(
        &self,
        engine_state: &mut EngineState,
        id: Option<Entity>,
    ) -> Result<Entity, Self::TemplateError>;
}

pub fn spawn_bundle(
    engine_state: &mut EngineState,
    bundle: impl DynamicBundle,
    entity: Option<Entity>,
) -> Entity {
    if let Some(entity) = entity {
        engine_state.world.spawn_at(entity, bundle);
        entity
    } else {
        engine_state.world.spawn(bundle)
    }
}
