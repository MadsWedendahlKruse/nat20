use std::{
    collections::HashMap,
    fs::File,
    path::{Path, PathBuf},
};

use hecs::World;
use serde::{Deserialize, Serialize};

use crate::{
    components::effects::effect_manager::EffectManager,
    engine::{
        encounter::{Encounter, EncounterId},
        engine_state::EngineState,
        event::{EventDispatcher, EventLog},
        geometry::{GeometrySourceError, WorldGeometry, WorldGeometrySource},
        prompt::PromptManager,
    },
    persistence::{
        self,
        registry::{LoadPolicy, RegistryLoadBlocker},
        world::WorldSaveBlocker,
    },
    registry::registry_validation::{self, RegistryReference},
    systems,
};

/// Saves the current engine state to the specified [`SaveFileTarget`].
/// Returns a [`SaveFileSource`] representing where the engine state was saved,
/// which can then be loaded using [`load_engine_state`].
pub fn save_engine_state<'engine>(
    engine_state: &'engine EngineState,
    target: impl Into<SaveFileTarget>,
) -> Result<SaveFileSource, EngineSaveBlocker> {
    can_save(engine_state)?;

    let save_file = SaveFileRef {
        world: &engine_state.world,
        event_log: &engine_state.event_log,
        encounters: &engine_state.encounters,
        prompts: &engine_state.prompts,
        geometry: engine_state.geometry.identifier.clone(),
    };

    match target.into() {
        SaveFileTarget::Path(path_buf) => {
            let file = File::create(&path_buf)?;
            serde_json::to_writer_pretty(file, &save_file)?;
            return Ok(SaveFileSource::Path(path_buf));
        }

        SaveFileTarget::Struct => {
            // Several fields on the EngineState don't implement Clone, so we serialize
            // and then deserialize it to create an owned copy of the save file.
            // We can skip the registry validation since we know it was already valid
            // when we serialized it.
            let serde_json_string = serde_json::to_string_pretty(&save_file)?;
            let save_file = serde_json::from_str(&serde_json_string)?;
            return Ok(SaveFileSource::Struct(save_file));
        }

        SaveFileTarget::String => {
            let serde_json_string = serde_json::to_string_pretty(&save_file)?;
            return Ok(SaveFileSource::String(serde_json_string));
        }
    }
}

/// Loads the engine state from the specified [`SaveFileSource`].
/// The `policy` parameter determines how registry references are validated.
/// Returns the reconstructed [`EngineState`] if successful.
pub fn load_engine_state(
    save_file_source: impl Into<SaveFileSource>,
    policy: LoadPolicy,
) -> Result<EngineState, EngineLoadBlocker> {
    let save_file = persistence::registry::validate_registry_references(
        SaveFile::try_from(save_file_source.into())?,
        policy,
    )?;

    let mut engine_state = EngineState {
        world: save_file.world,
        geometry: WorldGeometry::try_from(WorldGeometrySource::Cached {
            identifier: save_file.geometry.clone(),
        })?,
        encounters: save_file.encounters,
        prompts: save_file.prompts,
        event_log: save_file.event_log,
        event_dispatcher: EventDispatcher::default(),
    };

    rebuild_effect_end_conditions(&mut engine_state);

    Ok(engine_state)
}

/// A reference to the current engine state for serialization purposes.
/// We don't want to clone the entire engine state, so we use references with a
/// lifetime tied to the engine state.
#[derive(Serialize)]
struct SaveFileRef<'engine> {
    #[serde(with = "persistence::world")]
    world: &'engine World,
    prompts: &'engine PromptManager,
    encounters: &'engine HashMap<EncounterId, Encounter>,
    event_log: &'engine EventLog,
    geometry: String,
}

pub enum SaveFileTarget {
    Path(PathBuf),
    Struct,
    String,
}

impl<T> From<T> for SaveFileTarget
where
    T: AsRef<Path>,
{
    fn from(path: T) -> Self {
        SaveFileTarget::Path(path.as_ref().to_path_buf())
    }
}

#[derive(Debug)]
pub enum EngineSaveBlocker {
    World(WorldSaveBlocker),
    Io(std::io::Error),
    SerdeJson(serde_json::Error),
}

impl From<WorldSaveBlocker> for EngineSaveBlocker {
    fn from(blocker: WorldSaveBlocker) -> Self {
        EngineSaveBlocker::World(blocker)
    }
}

impl From<std::io::Error> for EngineSaveBlocker {
    fn from(blocker: std::io::Error) -> Self {
        EngineSaveBlocker::Io(blocker)
    }
}

impl From<serde_json::Error> for EngineSaveBlocker {
    fn from(blocker: serde_json::Error) -> Self {
        EngineSaveBlocker::SerdeJson(blocker)
    }
}

pub fn can_save(engine_state: &EngineState) -> Result<(), EngineSaveBlocker> {
    Ok(persistence::world::can_save(&engine_state.world)?)
}

/// The actual save file structure that is deserialized when loading the engine state.
/// The structure mirrors [`SaveFileRef`], except it actually owns the data.
#[derive(Serialize, Deserialize)]
pub struct SaveFile {
    #[serde(with = "persistence::world")]
    pub world: World,
    pub prompts: PromptManager,
    pub encounters: HashMap<EncounterId, Encounter>,
    pub event_log: EventLog,
    pub geometry: String,

    #[serde(skip)]
    pub path: PathBuf,
    #[serde(skip)]
    pub references: Vec<RegistryReference>,
}

pub enum SaveFileSource {
    Path(PathBuf),
    // TODO: It is only the registry references stored in the `references` field
    // on the SaveFile that are actually validated, so if the World is modified
    // such that it refers to some different registry entries, those aren't
    // actually validated during the load process.
    Struct(SaveFile),
    String(String),
}

impl TryFrom<SaveFileSource> for SaveFile {
    type Error = EngineLoadBlocker;

    fn try_from(source: SaveFileSource) -> Result<SaveFile, EngineLoadBlocker> {
        match source {
            SaveFileSource::Path(path) => {
                let file = File::open(&path)?;
                let (result, ids) = registry_validation::record_ids_scoped(|| {
                    serde_json::from_reader::<_, SaveFile>(file)
                });
                let mut save_file = result?;
                save_file.path = path;
                save_file.references = ids
                    .iter()
                    .filter_map(|id| RegistryReference::try_from(id.clone()).ok())
                    .collect();
                Ok(save_file)
            }

            SaveFileSource::Struct(save_file) => Ok(save_file),

            SaveFileSource::String(string) => {
                let (result, ids) = registry_validation::record_ids_scoped(|| {
                    serde_json::from_str::<SaveFile>(&string)
                });
                let mut save_file = result?;
                save_file.references = ids
                    .iter()
                    .filter_map(|id| RegistryReference::try_from(id.clone()).ok())
                    .collect();
                Ok(save_file)
            }
        }
    }
}

impl<T> From<T> for SaveFileSource
where
    T: AsRef<Path>,
{
    fn from(path: T) -> Self {
        SaveFileSource::Path(path.as_ref().to_path_buf())
    }
}

impl From<SaveFile> for SaveFileSource {
    fn from(save_file: SaveFile) -> Self {
        SaveFileSource::Struct(save_file)
    }
}

#[derive(Debug)]
pub enum EngineLoadBlocker {
    Registry(RegistryLoadBlocker),
    Io(std::io::Error),
    SerdeJson(serde_json::Error),
    Geometry(GeometrySourceError),
}

impl From<RegistryLoadBlocker> for EngineLoadBlocker {
    fn from(blocker: RegistryLoadBlocker) -> Self {
        EngineLoadBlocker::Registry(blocker)
    }
}

impl From<std::io::Error> for EngineLoadBlocker {
    fn from(blocker: std::io::Error) -> Self {
        EngineLoadBlocker::Io(blocker)
    }
}

impl From<serde_json::Error> for EngineLoadBlocker {
    fn from(blocker: serde_json::Error) -> Self {
        EngineLoadBlocker::SerdeJson(blocker)
    }
}

impl From<GeometrySourceError> for EngineLoadBlocker {
    fn from(blocker: GeometrySourceError) -> Self {
        EngineLoadBlocker::Geometry(blocker)
    }
}

fn rebuild_effect_end_conditions(engine_state: &mut EngineState) {
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
            systems::effects::register_end_conditions(engine_state, applier, entity, &instance);
        }
    }
}
