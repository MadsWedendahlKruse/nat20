use hecs::{DynamicBundle, Entity, TypeInfo};
use nat20_core::{
    components::{
        ability::Ability,
        d20::{D20CheckKind, D20CheckOutcome},
        id::{EffectId, ResourceId, SpeciesId},
        modifier::ModifierSource,
        resource::{ResourceBudget, ResourceBudgetKind, ResourceMap, ResourceMergePolicy},
        saving_throw::SavingThrowKind,
    },
    engine::event::{EventKindTag, ListenerSource},
    entities::creature::{Character, Monster},
    persistence::{
        engine::{EngineLoadBlocker, SaveFile, SaveFileSource, SaveFileTarget},
        registry::LoadPolicy,
        world::should_save,
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

    assert!(scenario.save().is_err());
}

/// Effect end conditions live in the EventDispatcher as EventListener, which can't
/// be serialized, so they need to be rebuilt when loading the engine state
#[test]
fn load_rebuilds_effect_end_condition() {
    let mut scenario = Scenario::new();
    // Spawn a Warlock with Hold Person, which uses repeat save end condition
    scenario.spawn("warlock", "hero.warlock").level(5).spawn();
    // Spawn a target for Hold Person
    scenario
        .spawn("fighter", "hero.fighter")
        .position([1.0, 0.0, 0.0], true)
        .spawn();
    // Spawn a Barbarian with Rage, which ends on equipping heavy armor
    scenario
        .spawn("barbarian", "hero.barbarian")
        .level(5)
        .position([0.0, 0.0, 1.0], true)
        .spawn();

    scenario.probe("fighter").d20_force_outcome(
        D20CheckKind::SavingThrow(SavingThrowKind::Ability(Ability::Wisdom)),
        D20CheckOutcome::Failure,
    );

    scenario
        .act("warlock", "action.hold_person")
        .target_entity("fighter")
        .perform();

    scenario.act("barbarian", "action.barbarian.rage").perform();

    assert_event_listeners(&scenario);

    scenario.save().unwrap();
    scenario.load(true).unwrap();

    assert_event_listeners(&scenario);
}

// TODO: Not the nicest solution, but might also not be needed anywhere else
fn assert_event_listeners(scenario: &Scenario) {
    let listeners = scenario.engine_state.event_dispatcher.listeners();

    let filtered_listeners = listeners
        .filter(|(_, listener)| {
            match listener.source {
                ListenerSource::EffectInstance { entity, .. } => {
                    // Rage
                    if entity == scenario.entity("barbarian") {
                        return listener.kinds.as_ref().is_some_and(|kinds| {
                            kinds.contains(&EventKindTag::EquipmentChanged)
                                && kinds.contains(&EventKindTag::ActionResult)
                        });
                    }

                    // Hold person
                    if entity == scenario.entity("fighter") {
                        return listener
                            .kinds
                            .as_ref()
                            .is_some_and(|kinds| kinds.contains(&EventKindTag::TurnBoundary));
                    }

                    false
                }
                _ => false,
            }
        })
        .collect::<Vec<_>>();

    assert_eq!(filtered_listeners.len(), 2);
}

#[test]
fn load_blocked_by_fatal_missing_references() {
    let mut scenario = Scenario::new();

    let (mut save_file, entity) = spawn_entity_and_save(&mut scenario);

    // Modify the save file to introduce a fatal missing reference
    {
        *save_file
            .world
            .query_one_mut::<&mut SpeciesId>(entity)
            .unwrap() = SpeciesId::new("nat20_core", "species.species_that_does_not_exist");
    }

    let load_result = serialize_and_load(&mut scenario, &save_file, LoadPolicy::Strict);

    println!("{:#?}", load_result);
    assert!(load_result.is_err());
}

// TODO: Come up with more scenarios to cover all droppable components?
#[test]
fn load_droppable() {
    let mut scenario = Scenario::new();

    let (mut save_file, entity) = spawn_entity_and_save(&mut scenario);

    // Resources are considered droppable, so introducing a missing resource should
    // still allow loading with the DropDroppable policy
    {
        save_file
            .world
            .query_one_mut::<&mut ResourceMap>(entity)
            .unwrap()
            .add(
                ResourceId::new("nat20_core", "resource.that_does_not_exist"),
                ResourceBudgetKind::Flat(
                    ResourceBudget::with_max_uses(
                        1,
                        ModifierSource::Effect(EffectId::new(
                            "nat20_core",
                            "effect.that_does_not_exist",
                        )),
                    )
                    .unwrap(),
                ),
                &ResourceMergePolicy::add_all(),
            )
            .unwrap();
    }

    // Strict load should fail due to the missing resource
    let load_result = serialize_and_load(&mut scenario, &save_file, LoadPolicy::Strict);
    println!("{:#?}", load_result);
    assert!(load_result.is_err());

    // Droppable load should succeed despite the missing resource
    let load_result = serialize_and_load(&mut scenario, &save_file, LoadPolicy::DropDroppable);
    println!("{:#?}", load_result);
    assert!(load_result.is_ok());

    // The missing resource should be dropped in the loaded world when using DropDroppable policy
    scenario
        .probe("fighter")
        .assert_no_resource("resource.that_does_not_exist");
}

fn spawn_entity_and_save(scenario: &mut Scenario) -> (SaveFile, Entity) {
    scenario.spawn("fighter", "hero.fighter").level(5).spawn();
    let entity = scenario.entity("fighter");

    let save_file = scenario.save_to_target(SaveFileTarget::Struct).unwrap();

    let SaveFileSource::Struct(save_file) = save_file else {
        panic!("Expected save file to be in memory");
    };

    (save_file, entity)
}

fn serialize_and_load(
    scenario: &mut Scenario,
    save_file: &SaveFile,
    policy: LoadPolicy,
) -> Result<(), EngineLoadBlocker> {
    // TODO: Little bit hacky?
    // IDs are recorded during deserialization, so we need to serialize the modified
    // save file to a string so that the recorded IDs are updated
    let save_file_string = serde_json::to_string_pretty(save_file).unwrap();

    let load_result =
        scenario.load_from_source(SaveFileSource::String(save_file_string), policy, true);
    load_result
}
