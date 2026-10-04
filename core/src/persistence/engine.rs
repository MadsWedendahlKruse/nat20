use std::{collections::HashMap, fs::File};

use hecs::World;
use serde::{Deserialize, Serialize};

use crate::{
    components::effects::effect_manager::EffectManager,
    engine::{
        encounter::{Encounter, EncounterId},
        engine_state::EngineState,
        event::{EventDispatcher, EventLog},
        prompt::PromptManager,
    },
    persistence, systems,
};

#[derive(Serialize)]
struct SaveFileRef<'engine> {
    #[serde(with = "persistence::world")]
    world: &'engine World,
    prompts: &'engine PromptManager,
    encounters: &'engine HashMap<EncounterId, Encounter>,
    event_log: &'engine EventLog,
}

pub fn save_engine_state<'engine>(engine_state: &'engine EngineState) -> Result<(), String> {
    let save_file = SaveFileRef {
        world: &engine_state.world,
        event_log: &engine_state.event_log,
        encounters: &engine_state.encounters,
        prompts: &engine_state.prompts,
    };

    let file = File::create("save_file.json").unwrap();
    serde_json::to_writer_pretty(file, &save_file).unwrap();

    Ok(())
}

#[derive(Deserialize)]
struct SaveFile {
    #[serde(with = "persistence::world")]
    world: World,
    prompts: PromptManager,
    encounters: HashMap<EncounterId, Encounter>,
    event_log: EventLog,
}

pub fn load_engine_state() -> Result<EngineState, String> {
    let file = File::open("save_file.json").unwrap();
    let save_file: SaveFile = serde_json::from_reader(file).unwrap();

    let mut engine_state = EngineState {
        world: save_file.world,
        geometry: serde_json::from_reader(
            File::open("assets/models/geometry/cache/test_terrain_2.json").unwrap(),
        )
        .unwrap(),
        encounters: save_file.encounters,
        prompts: save_file.prompts,
        event_log: save_file.event_log,
        event_dispatcher: EventDispatcher::default(),
    };

    let mut parent_effects = HashMap::new();

    for (entity, effect_manager) in engine_state.world.query::<&EffectManager>().iter() {
        for (id, instance) in &effect_manager.effects {
            if instance.is_parent()
                && let Some(applier) = instance.applier
            {
                parent_effects
                    .entry(entity)
                    .or_insert_with(Vec::new)
                    .push((*id, applier));
            }
        }
    }

    for (entity, instances) in parent_effects {
        for (instance, applier) in instances {
            systems::effects::register_end_conditions(
                &mut engine_state,
                applier,
                entity,
                &instance,
            );
        }
    }

    Ok(engine_state)
}
