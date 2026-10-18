use hecs::{DynamicBundle, TypeInfo};
use nat20_core::{
    components::{
        ability::Ability,
        d20::{D20CheckKind, D20CheckOutcome},
        saving_throw::SavingThrowKind,
    },
    engine::event::{EventKindTag, ListenerSource},
    entities::creature::{Character, Monster},
    persistence::{
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
        scenario.save(),
    );
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
