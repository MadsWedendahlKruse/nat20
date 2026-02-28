use std::{
    fmt::Display,
    fs::{self, DirEntry},
    str::FromStr,
};

use strum::EnumIter;

use crate::{
    components::id::{IdProvider, ScriptId},
    registry::registry::REGISTRIES_FOLDER,
};

#[derive(Debug)]
pub enum ScriptError {
    MissingFileExtension,
    UnknownLanguage {
        full_path: String,
        extension: String,
    },
    MissingFunction {
        function_name: String,
        script_id: ScriptId,
    },
    LoadError(String),
    RuntimeError(String),
}

impl Display for ScriptError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScriptError::MissingFileExtension => {
                write!(f, "Script file is missing a file extension")
            }
            ScriptError::UnknownLanguage {
                full_path,
                extension,
            } => {
                write!(
                    f,
                    "Unknown script language '{}' for script file '{}'",
                    extension, full_path
                )
            }
            ScriptError::MissingFunction {
                function_name,
                script_id,
            } => {
                write!(
                    f,
                    "Missing function '{}' in script '{}'",
                    function_name, script_id
                )
            }
            ScriptError::LoadError(message) => write!(f, "Script load error: {}", message),
            ScriptError::RuntimeError(message) => write!(f, "Script runtime error: {}", message),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumIter)]
pub enum ScriptLanguage {
    // Lua,
    Rhai,
}

impl ScriptLanguage {
    pub fn file_extension(&self) -> &str {
        match self {
            // ScriptLanguage::Lua => "lua",
            ScriptLanguage::Rhai => "rhai",
        }
    }
}

impl FromStr for ScriptLanguage {
    type Err = ScriptError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            // "lua" => Ok(ScriptLanguage::Lua),
            "rhai" => Ok(ScriptLanguage::Rhai),
            _ => Err(ScriptError::LoadError(format!(
                "Unknown script language: {}",
                s
            ))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Script {
    pub id: ScriptId,
    pub file_path: String,
    pub content: String,
    pub language: ScriptLanguage,
}

impl TryFrom<DirEntry> for Script {
    type Error = ScriptError;

    fn try_from(value: DirEntry) -> Result<Self, Self::Error> {
        let full_file_path = value.path();
        let file_name = full_file_path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| ScriptError::LoadError("Invalid file name".to_string()))?;
        let content = fs::read_to_string(&full_file_path).map_err(|e| {
            ScriptError::LoadError(format!(
                "Failed to read script file {:?}: {}",
                full_file_path, e
            ))
        })?;

        let file_extension = full_file_path
            .extension()
            .and_then(|s| s.to_str())
            .ok_or_else(|| ScriptError::MissingFileExtension)?;
        let language =
            ScriptLanguage::from_str(file_extension).map_err(|_| ScriptError::UnknownLanguage {
                full_path: full_file_path.to_string_lossy().to_string(),
                extension: file_extension.to_string(),
            })?;

        // Keep visiting parent folders until we reach the registry root
        let mut script_id = file_name.to_string();
        let mut file_path = full_file_path.clone();
        while let Some(parent) = file_path.parent() {
            if let Some(folder_name) = parent.file_name().and_then(|s| s.to_str()) {
                if folder_name == REGISTRIES_FOLDER {
                    break;
                }
                // Convert plural folder names to singular for script IDs
                let folder_name = folder_name.trim_end_matches('s');
                script_id = format!("{}.{}", folder_name, script_id);
            }
            file_path = parent.to_path_buf();
        }
        let id = ScriptId::new("nat20_core", format!("script.{}", script_id));

        Ok(Script {
            id,
            file_path: full_file_path.to_string_lossy().to_string(),
            content,
            language,
        })
    }
}

impl IdProvider for Script {
    type Id = ScriptId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumIter)]
pub enum ScriptFunction {
    ActionHook,
    ActionResultHook,
    ArmorClassHook,
    AttackRollHook,
    DamageRollResultHook,
    PreDamageMitigationHook,
    PostDamageMitigationHook,
    ReactionBody,
    ReactionTrigger,
    ResourceCostHook,
    DeathHook,
}

impl ScriptFunction {
    pub fn fn_name(&self) -> &str {
        match self {
            ScriptFunction::ActionHook => "action_hook",
            ScriptFunction::ActionResultHook => "action_result_hook",
            ScriptFunction::ArmorClassHook => "armor_class_hook",
            ScriptFunction::AttackRollHook => "attack_roll_hook",
            ScriptFunction::DamageRollResultHook => "damage_roll_result_hook",
            ScriptFunction::PreDamageMitigationHook => "pre_damage_mitigation_hook",
            ScriptFunction::PostDamageMitigationHook => "post_damage_mitigation_hook",
            ScriptFunction::ReactionBody => "reaction_body",
            ScriptFunction::ReactionTrigger => "reaction_trigger",
            ScriptFunction::ResourceCostHook => "resource_cost_hook",
            ScriptFunction::DeathHook => "death_hook",
        }
    }

    pub fn defined_in_script(&self, script: &Script) -> bool {
        match script.language {
            ScriptLanguage::Rhai => script
                .content
                .contains(format!("fn {}", self.fn_name()).as_str()),
        }
    }
}
