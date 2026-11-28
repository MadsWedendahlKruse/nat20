use std::{collections::HashMap, path::Path};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        id::{ActionId, FactionId},
        level::ChallengeRating,
        resource::ResourceAmountMap,
    },
    registry::registry::{self, RegistryError},
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
    /// The amount of experience required to reach each character level.
    pub experience_per_level: HashMap<u8, u32>,
    /// The amount of experience awarded for defeating a creature of a given challenge
    /// rating.
    pub experience_by_challenge_rating: HashMap<ChallengeRating, u32>,
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

    pub fn max_level(&self) -> u8 {
        self.experience_per_level.keys().cloned().max().unwrap_or(0)
    }

    pub fn max_challenge_rating(&self) -> ChallengeRating {
        self.experience_by_challenge_rating
            .keys()
            .cloned()
            .max()
            .unwrap_or(ChallengeRating::Full(0))
    }
}
