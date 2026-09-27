use std::path::Path;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        id::{ActionId, FactionId},
        resource::ResourceAmountMap,
    },
    registry::{
        registry::{self, RegistryError},
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
    },
};

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Rules {
    /// The actions which are available to all creatures by default, and which
    /// don't require any special conditions to be used. Usually something like
    /// Dash or Disengage
    pub default_actions: Vec<ActionId>,
    /// The underlying actions which are actually performed when making an attack,
    /// i.e. melee_attack, ranged_attack, unarmed_attack, etc.
    /// These are granted to all creatures with a `Loadout`.
    pub loadout_attack_actions: Vec<ActionId>,
    /// The default faction assigned to all (player) characters.
    pub default_character_faction: FactionId,
    /// The default resources available to all creatures.
    pub default_resources: ResourceAmountMap,
}

impl Rules {
    pub fn load(path: &Path, errors: &mut Vec<RegistryError>) -> Option<Self> {
        match registry::load_file(path) {
            Ok(rules) => Some(rules),
            Err(error) => {
                error.clone().push_into(errors);
                None
            }
        }
    }
}

impl RegistryReferenceCollector for Rules {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for action_id in &self.default_actions {
            collector.add(RegistryReference::Action(action_id.clone()));
        }
    }
}
