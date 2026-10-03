use hecs::{DynamicBundle, TypeInfo};
use nat20_core::{
    entities::creature::{Character, Monster},
    persistence::world::should_save,
    test_utils::scenario::Scenario,
};

/// A little sanity check to make sure we didn't forget to save any creature components
#[test]
fn save_all_creature_components() {
    let mut scenario = Scenario::new();
    scenario.spawn("hero", "hero.wizard").spawn();
    scenario.spawn("goblin", "monster.goblin_warrior").spawn();

    let world = &scenario.engine_state.world;
    let bundles = [
        Character::from_world(world, scenario.entity("hero")).type_info(),
        Monster::from_world(world, scenario.entity("goblin")).type_info(),
    ];

    let unsaved: Vec<TypeInfo> = bundles
        .into_iter()
        .flatten()
        .filter(|type_info| !should_save(type_info.id()))
        .collect();
    assert!(
        unsaved.is_empty(),
        "Components missing from `saved_components!`: {unsaved:#?}"
    );
}
