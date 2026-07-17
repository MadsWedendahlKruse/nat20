use std::collections::{HashMap, HashSet};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::Display;

use crate::{
    components::{
        id::{ActionId, EffectId, IdProvider, SpeciesId, SubspeciesId},
        speed::Speed,
    },
    registry::serialize::species::{SpeciesDefinition, SubspeciesDefinition},
};

// TODO: Mutliple creature types? e.g. Undead Dragon
#[derive(
    Debug, Clone, Display, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, JsonSchema,
)]
#[serde(rename_all = "snake_case")]
pub enum CreatureType {
    Aberration,
    Beast,
    Celestial,
    Construct,
    Dragon,
    Elemental,
    Fey,
    Fiend,
    Giant,
    Humanoid,
    Monstrosity,
    Ooze,
    Plant,
    Undead,
}

#[derive(
    Debug, Clone, Display, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, JsonSchema,
)]
#[serde(rename_all = "snake_case")]
pub enum CreatureSize {
    Tiny,
    Small,
    Medium,
    Large,
    Huge,
    Gargantuan,
}

// TODO: Do we need all these modes?
// pub struct Speed {
//     pub walk: u8,
//     pub burrow: Option<u8>,
//     pub climb: Option<u8>,
//     pub fly: Option<u8>,
//     pub swim: Option<u8>,
// }

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SpeciesBase {
    pub effects_by_level: HashMap<u8, Vec<EffectId>>,
    pub actions_by_level: HashMap<u8, Vec<ActionId>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(from = "SpeciesDefinition")]
pub struct Species {
    pub id: SpeciesId,
    pub base: SpeciesBase,
    pub subspecies: HashSet<SubspeciesId>,
    pub creature_type: CreatureType,
    pub size: CreatureSize,
    // TODO: Subspeciess can modify the speed using an effect?
    pub speed: Speed,
}

impl IdProvider for Species {
    type Id = SpeciesId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(from = "SubspeciesDefinition")]
pub struct Subspecies {
    pub id: SubspeciesId,
    pub base: SpeciesBase,
}

impl IdProvider for Subspecies {
    type Id = SubspeciesId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}
