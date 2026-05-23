use std::{
    collections::HashMap,
    fmt::{self, Debug, Display},
    fs,
    hash::Hash,
    path::{Path, PathBuf},
    sync::LazyLock,
};

use serde::de::DeserializeOwned;

use tracing::{error, info};

use crate::{
    components::{
        actions::action::Action,
        background::Background,
        class::{Class, Subclass},
        effects::effect::Effect,
        faction::Faction,
        feat::Feat,
        id::{
            ActionId, BackgroundId, ClassId, EffectId, FactionId, FeatId, IdProvider, ItemId,
            ResourceId, ScriptId, SpeciesId, SpellId, SubclassId, SubspeciesId,
        },
        items::inventory::ItemInstance,
        resource::Resource,
        species::{Species, Subspecies},
        spells::spell::Spell,
    },
    registry::{
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
        serialize::{
            action::ActionDefinition,
            class::ClassDefinition,
            effect::EffectDefinition,
            species::{SpeciesDefinition, SubspeciesDefinition},
            spell::SpellDefinition,
        },
    },
    scripts::script::{LUA_FILE_EXTENSION, Script, ScriptError},
};

pub static REGISTRIES_FOLDER: &str = "registries";

// TODO: Make this configurable?
pub static REGISTRY_ROOT: LazyLock<PathBuf> = LazyLock::new(|| {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(format!("../assets/{}", REGISTRIES_FOLDER))
});

static REGISTRIES: LazyLock<RegistrySet> =
    LazyLock::new(
        || match RegistrySet::load_from_root_directory(&*REGISTRY_ROOT) {
            Ok(set) => set,
            Err(error) => {
                error!(path = ?&*REGISTRY_ROOT, %error, "Failed to load registries");
                eprintln!(
                    "Failed to load registries from {:?}: {}",
                    &*REGISTRY_ROOT, error
                );
                panic!("Failed to load registries");
            }
        },
    );

#[derive(Debug, Clone)]
pub struct RegistryEntry<V, D> {
    pub value: V,
    pub definition: D,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct Registry<K, V, D> {
    pub entries: HashMap<K, RegistryEntry<V, D>>,
}

#[derive(Debug)]
pub enum RegistryError {
    ReadDirectory {
        directory: PathBuf,
        message: String,
    },
    ReadDirectoryEntry {
        directory: PathBuf,
        message: String,
    },
    ReadFile {
        path: PathBuf,
        message: String,
    },
    DeserializeJson {
        path: PathBuf,
        message: String,
    },
    DuplicateId {
        id_debug: String,
        first_path: PathBuf,
        second_path: PathBuf,
    },
    MissingRegistryEntry {
        path: PathBuf,
        reference: RegistryReference,
        suggestion: Option<String>,
    },
    ScriptError(ScriptError),
    Many(Vec<RegistryError>),
}

impl RegistryError {
    pub fn push_into(self, errors: &mut Vec<RegistryError>) {
        match self {
            RegistryError::Many(mut inner) => errors.append(&mut inner),
            other => errors.push(other),
        }
    }
}

impl fmt::Display for RegistryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegistryError::ReadDirectory { directory, message } => {
                write!(f, "Failed to read directory {:?}: {}", directory, message)
            }
            RegistryError::ReadDirectoryEntry { directory, message } => {
                write!(
                    f,
                    "Failed to read directory entry in {:?}: {}",
                    directory, message
                )
            }
            RegistryError::ReadFile { path, message } => {
                write!(f, "Failed to read file {:?}: {}", path, message)
            }
            RegistryError::DeserializeJson { path, message } => {
                write!(f, "Failed to deserialize JSON {:?}: {}", path, message)
            }
            RegistryError::DuplicateId {
                id_debug,
                first_path,
                second_path,
            } => {
                write!(
                    f,
                    "Duplicate id {}:\n  first:  {:?}\n  second: {:?}",
                    id_debug, first_path, second_path
                )
            }
            RegistryError::MissingRegistryEntry {
                path,
                reference,
                suggestion,
            } => {
                write!(
                    f,
                    "Missing registry entry for reference {} in definition at {:?}{}",
                    reference,
                    path,
                    match suggestion {
                        Some(s) => format!(". Did you mean '{}'?", s),
                        None => String::new(),
                    }
                )
            }
            RegistryError::ScriptError(script_error) => {
                write!(f, "Script error: {}", script_error)
            }
            RegistryError::Many(errors) => {
                writeln!(f, "{} registry error(s):", errors.len())?;
                for (index, error) in errors.iter().enumerate() {
                    writeln!(f, "  {}. {}", index + 1, error)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for RegistryError {}

impl<K, V, D> Registry<K, V, D>
where
    K: Eq + Hash + Clone + Debug + Display,
    V: IdProvider<Id = K> + From<D>,
    D: DeserializeOwned + RegistryReferenceCollector + Clone,
{
    pub fn load_from_directory(directory: impl AsRef<Path>) -> Result<Self, RegistryError> {
        let directory = directory.as_ref();

        let mut entries: HashMap<K, RegistryEntry<V, D>> = HashMap::new();
        let mut id_to_path: HashMap<K, PathBuf> = HashMap::new();
        let mut errors: Vec<RegistryError> = Vec::new();

        Self::load_directory_recursive(directory, &mut entries, &mut id_to_path, &mut errors);

        if errors.is_empty() {
            Ok(Self { entries })
        } else {
            Err(RegistryError::Many(errors))
        }
    }

    fn load_directory_recursive(
        directory: &Path,
        entries: &mut HashMap<K, RegistryEntry<V, D>>,
        id_to_path: &mut HashMap<K, PathBuf>,
        errors: &mut Vec<RegistryError>,
    ) {
        let read_dir_iter = match fs::read_dir(directory) {
            Ok(iter) => iter,
            Err(error) => {
                errors.push(RegistryError::ReadDirectory {
                    directory: directory.to_path_buf(),
                    message: error.to_string(),
                });
                return;
            }
        };

        for entry_result in read_dir_iter {
            let entry = match entry_result {
                Ok(entry) => entry,
                Err(error) => {
                    errors.push(RegistryError::ReadDirectoryEntry {
                        directory: directory.to_path_buf(),
                        message: error.to_string(),
                    });
                    continue;
                }
            };

            let path = entry.path();

            if path.is_dir() {
                Self::load_directory_recursive(&path, entries, id_to_path, errors);
                continue;
            }

            if path.extension().is_none()
                || path.extension().and_then(|ext| ext.to_str()) != Some("json")
            {
                info!("Skipping non-json file in registry: {:?}", path);
                continue;
            }

            let definition = match Self::load_file(&path) {
                Ok(definition) => definition,
                Err(error) => {
                    error!(%error, "Failed to load registry entry");
                    error.push_into(errors);
                    continue;
                }
            };

            let value = V::from(definition.clone());

            let id = value.id().clone();

            if let Some(first_path) = id_to_path.get(&id).cloned() {
                errors.push(RegistryError::DuplicateId {
                    id_debug: format!("{:?}", id),
                    first_path,
                    second_path: path.clone(),
                });
                continue;
            }

            id_to_path.insert(id.clone(), path.clone());

            let entry = RegistryEntry {
                value,
                definition,
                path: path.clone(),
            };

            entries.insert(id, entry);
        }
    }

    fn load_file(path: &Path) -> Result<D, RegistryError> {
        let file_contents = fs::read_to_string(path).map_err(|error| RegistryError::ReadFile {
            path: path.to_path_buf(),
            message: error.to_string(),
        })?;

        serde_json::from_str::<D>(&file_contents).map_err(|error| RegistryError::DeserializeJson {
            path: path.to_path_buf(),
            message: error.to_string(),
        })
    }

    fn load_registry(
        directory: &Path,
        errors: &mut Vec<RegistryError>,
    ) -> Option<Registry<K, V, D>> {
        if !directory.exists() {
            // Decide policy: missing directory might be okay.
            // If not okay, push an error here.
            return Some(Registry {
                entries: HashMap::new(),
            });
        }

        match Registry::<K, V, D>::load_from_directory(directory) {
            Ok(registry) => Some(registry),
            Err(error) => {
                error.push_into(errors);
                None
            }
        }
    }

    pub fn all_keys_strings(&self) -> Vec<String> {
        self.entries.keys().map(|key| format!("{}", key)).collect()
    }
}

pub struct RegistrySet {
    pub actions: Registry<ActionId, Action, ActionDefinition>,
    pub backgrounds: Registry<BackgroundId, Background, Background>,
    pub classes: Registry<ClassId, Class, ClassDefinition>,
    pub effects: Registry<EffectId, Effect, EffectDefinition>,
    pub factions: Registry<FactionId, Faction, Faction>,
    pub feats: Registry<FeatId, Feat, Feat>,
    pub items: Registry<ItemId, ItemInstance, ItemInstance>,
    pub resources: Registry<ResourceId, Resource, Resource>,
    pub scripts: Registry<ScriptId, Script, Script>,
    pub species: Registry<SpeciesId, Species, SpeciesDefinition>,
    pub spells: Registry<SpellId, Spell, SpellDefinition>,
    pub subclasses: Registry<SubclassId, Subclass, Subclass>,
    pub subspecies: Registry<SubspeciesId, Subspecies, SubspeciesDefinition>,
}

impl RegistrySet {
    pub fn load_from_root_directory(
        root_directory: impl AsRef<Path>,
    ) -> Result<Self, RegistryError> {
        let root_directory = root_directory.as_ref();

        let actions_directory = root_directory.join("actions");
        let backgrounds_directory = root_directory.join("backgrounds");
        let classes_directory = root_directory.join("classes");
        let effects_directory = root_directory.join("effects");
        let factions_directory = root_directory.join("factions");
        let feats_directory = root_directory.join("feats");
        let items_directory = root_directory.join("items");
        let resources_directory = root_directory.join("resources");
        let species_directory = root_directory.join("species");
        let spells_directory = root_directory.join("spells");
        let subclasses_directory = root_directory.join("subclasses");
        let subspecies_directory = root_directory.join("subspecies");

        let all_directories: Vec<&Path> = vec![
            actions_directory.as_path(),
            backgrounds_directory.as_path(),
            classes_directory.as_path(),
            effects_directory.as_path(),
            factions_directory.as_path(),
            feats_directory.as_path(),
            items_directory.as_path(),
            resources_directory.as_path(),
            species_directory.as_path(),
            spells_directory.as_path(),
            subclasses_directory.as_path(),
            subspecies_directory.as_path(),
        ];

        let mut errors: Vec<RegistryError> = Vec::new();

        // Load scripts first (but do not fail early).
        let scripts_map = Self::load_scripts_from_directories(&all_directories, &mut errors);

        let actions = Registry::load_registry(&actions_directory, &mut errors);
        let backgrounds = Registry::load_registry(&backgrounds_directory, &mut errors);
        let classes = Registry::load_registry(&classes_directory, &mut errors);
        let effects = Registry::load_registry(&effects_directory, &mut errors);
        let factions = Registry::load_registry(&factions_directory, &mut errors);
        let feats = Registry::load_registry(&feats_directory, &mut errors);
        let items = Registry::load_registry(&items_directory, &mut errors);
        let resources = Registry::load_registry(&resources_directory, &mut errors);
        let species = Registry::load_registry(&species_directory, &mut errors);
        let spells = Registry::load_registry(&spells_directory, &mut errors);
        let subclasses = Registry::load_registry(&subclasses_directory, &mut errors);
        let subspecies = Registry::load_registry(&subspecies_directory, &mut errors);

        // If anything failed, report all collected diagnostics once.
        if !errors.is_empty() {
            return Err(RegistryError::Many(errors));
        }

        let set = Self {
            actions: actions.expect("validated"),
            backgrounds: backgrounds.expect("validated"),
            classes: classes.expect("validated"),
            effects: effects.expect("validated"),
            factions: factions.expect("validated"),
            feats: feats.expect("validated"),
            items: items.expect("validated"),
            resources: resources.expect("validated"),
            scripts: Registry {
                entries: scripts_map,
            },
            species: species.expect("validated"),
            spells: spells.expect("validated"),
            subclasses: subclasses.expect("validated"),
            subspecies: subspecies.expect("validated"),
        };

        // Validate references now that all registries are loaded.
        Self::validate_registry_references(&mut errors, &set.actions, &set);
        Self::validate_registry_references(&mut errors, &set.backgrounds, &set);
        Self::validate_registry_references(&mut errors, &set.classes, &set);
        Self::validate_registry_references(&mut errors, &set.effects, &set);
        Self::validate_registry_references(&mut errors, &set.factions, &set);
        Self::validate_registry_references(&mut errors, &set.feats, &set);
        Self::validate_registry_references(&mut errors, &set.items, &set);
        Self::validate_registry_references(&mut errors, &set.resources, &set);
        Self::validate_registry_references(&mut errors, &set.species, &set);
        Self::validate_registry_references(&mut errors, &set.spells, &set);
        Self::validate_registry_references(&mut errors, &set.subclasses, &set);
        Self::validate_registry_references(&mut errors, &set.subspecies, &set);

        if !errors.is_empty() {
            return Err(RegistryError::Many(errors));
        }

        Ok(set)
    }

    // assuming Script has: id: ScriptId, and Script::try_from(entry) -> Result<Script, ScriptError>
    fn load_scripts_from_directories(
        directories: &[&Path],
        errors: &mut Vec<RegistryError>,
    ) -> HashMap<ScriptId, RegistryEntry<Script, Script>> {
        let mut scripts = HashMap::new();
        let mut script_id_to_path = HashMap::new();

        for directory in directories {
            if !directory.exists() {
                continue;
            }

            Self::load_scripts_from_directory_recursive(
                directory,
                &mut scripts,
                &mut script_id_to_path,
                errors,
            );
        }

        scripts
    }

    fn load_scripts_from_directory_recursive(
        directory: &Path,
        scripts: &mut HashMap<ScriptId, RegistryEntry<Script, Script>>,
        script_id_to_path: &mut HashMap<ScriptId, PathBuf>,
        errors: &mut Vec<RegistryError>,
    ) {
        let read_dir_iter = match fs::read_dir(directory) {
            Ok(iter) => iter,
            Err(error) => {
                errors.push(RegistryError::ReadDirectory {
                    directory: directory.to_path_buf(),
                    message: error.to_string(),
                });
                return;
            }
        };

        for entry_result in read_dir_iter {
            let entry = match entry_result {
                Ok(entry) => entry,
                Err(error) => {
                    errors.push(RegistryError::ReadDirectoryEntry {
                        directory: directory.to_path_buf(),
                        message: error.to_string(),
                    });
                    continue;
                }
            };

            let path = entry.path();

            if path.is_dir() {
                Self::load_scripts_from_directory_recursive(
                    &path,
                    scripts,
                    script_id_to_path,
                    errors,
                );
                continue;
            }

            // Skip non-lua files (assuming scripts are lua files).
            let ext = path.extension().and_then(|e| e.to_str());
            if ext.is_none() || ext != Some(LUA_FILE_EXTENSION) {
                continue;
            }

            match Script::try_from(entry) {
                Ok(script) => {
                    let id = script.id.clone();

                    if let Some(first_path) = script_id_to_path.get(&id).cloned() {
                        errors.push(RegistryError::DuplicateId {
                            id_debug: format!("{:?}", id),
                            first_path,
                            second_path: path.clone(),
                        });
                        continue;
                    }

                    script_id_to_path.insert(id.clone(), path.clone());
                    scripts.insert(
                        id,
                        RegistryEntry {
                            value: script.clone(),
                            definition: script,
                            path: path.clone(),
                        },
                    );
                }
                Err(script_error) => {
                    errors.push(RegistryError::ReadFile {
                        path: path.clone(),
                        message: format!("Failed to load script: {:?}", script_error),
                    });
                }
            }
        }
    }

    fn validate_registry_references<K, V, D>(
        errors: &mut Vec<RegistryError>,
        registry: &Registry<K, V, D>,
        registries: &RegistrySet,
    ) where
        K: Eq + Hash + Clone + Debug,
        V: IdProvider<Id = K> + From<D>,
        D: DeserializeOwned + RegistryReferenceCollector + Clone,
    {
        for entry in registry.entries.values() {
            let mut collector = ReferenceCollector::new();
            entry.definition.collect_registry_references(&mut collector);
            for reference in collector.into_references() {
                let found = match &reference {
                    RegistryReference::Action(id) => registries.actions.entries.contains_key(id),
                    RegistryReference::Background(id) => {
                        registries.backgrounds.entries.contains_key(id)
                    }
                    RegistryReference::Class(id) => registries.classes.entries.contains_key(id),
                    RegistryReference::Effect(id) => registries.effects.entries.contains_key(id),
                    RegistryReference::Faction(id) => registries.factions.entries.contains_key(id),
                    RegistryReference::Feat(id) => registries.feats.entries.contains_key(id),
                    RegistryReference::Item(id) => registries.items.entries.contains_key(id),
                    RegistryReference::Resource(id) => {
                        registries.resources.entries.contains_key(id)
                    }
                    RegistryReference::Species(id) => registries.species.entries.contains_key(id),
                    RegistryReference::Spell(id) => registries.spells.entries.contains_key(id),
                    RegistryReference::Subclass(id) => {
                        registries.subclasses.entries.contains_key(id)
                    }
                    RegistryReference::Subspecies(id) => {
                        registries.subspecies.entries.contains_key(id)
                    }
                    RegistryReference::Script(id, function) => {
                        let found = registries.scripts.entries.contains_key(id);

                        if found {
                            let script_entry = &registries.scripts.entries[id].value;
                            if !function.defined_in_script(script_entry) {
                                errors.push(RegistryError::ScriptError(
                                    ScriptError::MissingFunction {
                                        function_name: function.fn_name().to_string(),
                                        script_id: id.clone(),
                                    },
                                ));
                            }
                        }

                        found
                    }
                };

                if !found {
                    let suggestion = Self::find_nearest_match(registries, &reference);
                    errors.push(RegistryError::MissingRegistryEntry {
                        path: entry.path.clone(),
                        reference,
                        suggestion: Some(suggestion),
                    });
                }
            }
        }
    }

    fn find_nearest_match(registries: &RegistrySet, reference: &RegistryReference) -> String {
        let (id_str, candidates) = match reference {
            RegistryReference::Action(id) => {
                (id.to_string(), registries.actions.all_keys_strings())
            }
            RegistryReference::Background(id) => {
                (id.to_string(), registries.backgrounds.all_keys_strings())
            }
            RegistryReference::Class(id) => (id.to_string(), registries.classes.all_keys_strings()),
            RegistryReference::Effect(id) => {
                (id.to_string(), registries.effects.all_keys_strings())
            }
            RegistryReference::Faction(id) => {
                (id.to_string(), registries.factions.all_keys_strings())
            }
            RegistryReference::Feat(id) => (id.to_string(), registries.feats.all_keys_strings()),
            RegistryReference::Item(id) => (id.to_string(), registries.items.all_keys_strings()),
            RegistryReference::Resource(id) => {
                (id.to_string(), registries.resources.all_keys_strings())
            }
            RegistryReference::Species(id) => {
                (id.to_string(), registries.species.all_keys_strings())
            }
            RegistryReference::Spell(id) => (id.to_string(), registries.spells.all_keys_strings()),
            RegistryReference::Subclass(id) => {
                (id.to_string(), registries.subclasses.all_keys_strings())
            }
            RegistryReference::Subspecies(id) => {
                (id.to_string(), registries.subspecies.all_keys_strings())
            }
            RegistryReference::Script(id, _) => (
                id.to_string(),
                registries
                    .scripts
                    .entries
                    .keys()
                    .map(|k| format!("{}", k))
                    .collect(),
            ),
        };

        let mut best_match = String::new();
        let mut best_distance = usize::MAX;

        for candidate in candidates {
            let distance = strsim::levenshtein(&id_str, &candidate);
            if distance < best_distance {
                best_distance = distance;
                best_match = candidate;
            }
        }

        best_match
    }
}

macro_rules! define_registry {
    ($registry_name:ident, $key_type:ty, $value_type:ty, $field:ident) => {
        pub struct $registry_name;

        impl $registry_name {
            pub fn get(key: &$key_type) -> Option<&'static $value_type> {
                REGISTRIES.$field.entries.get(key).map(|entry| &entry.value)
            }

            pub fn keys() -> impl Iterator<Item = &'static $key_type> + 'static {
                REGISTRIES.$field.entries.keys()
            }

            pub fn values() -> impl Iterator<Item = &'static $value_type> + 'static {
                REGISTRIES.$field.entries.values().map(|entry| &entry.value)
            }
        }
    };
}

define_registry!(ActionsRegistry, ActionId, Action, actions);
define_registry!(BackgroundsRegistry, BackgroundId, Background, backgrounds);
define_registry!(ClassesRegistry, ClassId, Class, classes);
define_registry!(EffectsRegistry, EffectId, Effect, effects);
define_registry!(FactionsRegistry, FactionId, Faction, factions);
define_registry!(FeatsRegistry, FeatId, Feat, feats);
define_registry!(ItemsRegistry, ItemId, ItemInstance, items);
define_registry!(ResourcesRegistry, ResourceId, Resource, resources);
define_registry!(ScriptsRegistry, ScriptId, Script, scripts);
define_registry!(SpeciesRegistry, SpeciesId, Species, species);
define_registry!(SpellsRegistry, SpellId, Spell, spells);
define_registry!(SubclassesRegistry, SubclassId, Subclass, subclasses);
define_registry!(SubspeciesRegistry, SubspeciesId, Subspecies, subspecies);
