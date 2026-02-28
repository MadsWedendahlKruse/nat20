use std::{
    collections::HashMap,
    sync::{Arc, LazyLock},
};

use hecs::{Entity, World};

use crate::{
    components::{actions::action::ActionContext, level::CharacterLevels},
    registry::registry::ClassesRegistry,
    systems,
};

pub type VariableFunction = dyn Fn(&World, Entity, &ActionContext) -> i32 + Send + Sync;

pub type VariableMap = HashMap<String, Arc<VariableFunction>>;

pub static PARSER_VARIABLES: LazyLock<VariableMap> = LazyLock::new(|| {
    let mut map = HashMap::from([
        (
            "spell_level".to_string(),
            Arc::new(|_world: &World, _entity: Entity, context: &ActionContext| {
                context.spell.as_ref().map_or(0, |spell| spell.level as i32)
            }) as Arc<VariableFunction>,
        ),
        (
            "caster_level".to_string(),
            Arc::new(|world: &World, entity: Entity, _context: &ActionContext| {
                systems::spells::spellcaster_levels(world, entity) as i32
            }) as Arc<VariableFunction>,
        ),
        (
            "character_level".to_string(),
            Arc::new(|world: &World, entity: Entity, _context: &ActionContext| {
                systems::helpers::get_component::<CharacterLevels>(world, entity).total_level()
                    as i32
            }) as Arc<VariableFunction>,
        ),
    ]);

    let classes = ClassesRegistry::keys();

    for class_id in classes {
        // TODO: Consider if namespace should be included in variable name
        let variable_name = format!("{}.level", class_id.id);
        map.insert(
            variable_name,
            Arc::new(
                move |world: &World, entity: Entity, _context: &ActionContext| {
                    systems::class::class_level(world, entity, &class_id) as i32
                },
            ) as Arc<VariableFunction>,
        );
    }

    map
});
