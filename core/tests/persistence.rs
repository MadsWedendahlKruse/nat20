use hecs::{DynamicBundle, TypeInfo};
use nat20_core::{
    entities::creature::{Character, Monster},
    persistence::{
        self,
        engine::EngineSaveBlocker,
        world::{WorldSaveBlocker, should_save},
    },
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

#[test]
fn save_blocked_by_action() {
    let mut scenario = Scenario::new();
    scenario.spawn("fighter", "hero.fighter").spawn();
    scenario.spawn("goblin", "monster.goblin_warrior").spawn();

    scenario
        .probe("fighter")
        .act("action.melee_attack")
        .target_entity("goblin")
        // Disable auto updating after perform so it's still in progress
        .auto_update(false)
        .perform();

    assert_eq!(
        Err(EngineSaveBlocker::World(
            WorldSaveBlocker::ActionExecutionInProgress {
                actor: scenario.entity_identifier("fighter")
            }
        )),
        persistence::engine::save_engine_state(&scenario.engine_state)
    );
}
