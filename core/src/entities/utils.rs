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
                        $field: crate::systems::helpers::get_component_clone(world, entity),
                    )*
                }
            }
        }
    }
}
