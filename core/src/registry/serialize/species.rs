use std::collections::{HashMap, HashSet};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        id::{ActionId, EffectId, SpeciesId, SubspeciesId},
        species::{CreatureSize, CreatureType, Species, SpeciesBase, Subspecies},
        speed::Speed,
    },
    registry::{
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
        serialize::quantity::LengthExpressionDefinition,
    },
};

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SpeciesDefinition {
    pub id: SpeciesId,
    pub creature_type: CreatureType,
    pub size: CreatureSize,
    #[serde(default)]
    pub effects_by_level: HashMap<u8, Vec<EffectId>>,
    #[serde(default)]
    pub actions_by_level: HashMap<u8, Vec<ActionId>>,
    pub subspecies: HashSet<SubspeciesId>,
    pub speed: LengthExpressionDefinition,
}

impl From<SpeciesDefinition> for Species {
    fn from(value: SpeciesDefinition) -> Self {
        Species {
            id: value.id,
            base: SpeciesBase {
                effects_by_level: value.effects_by_level,
                actions_by_level: value.actions_by_level,
            },
            subspecies: value.subspecies,
            creature_type: value.creature_type,
            size: value.size,
            speed: Speed::new(value.speed.evaluate_without_variables().unwrap()),
        }
    }
}

impl RegistryReferenceCollector for SpeciesDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for effects in self.effects_by_level.values() {
            for effect in effects {
                collector.add(RegistryReference::Effect(effect.clone()));
            }
        }
        for actions in self.actions_by_level.values() {
            for action in actions {
                collector.add(RegistryReference::Action(action.clone()));
            }
        }
        for subspecies in &self.subspecies {
            collector.add(RegistryReference::Subspecies(subspecies.clone()));
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SubspeciesDefinition {
    pub id: SubspeciesId,
    #[serde(default)]
    pub effects_by_level: HashMap<u8, Vec<EffectId>>,
    #[serde(default)]
    pub actions_by_level: HashMap<u8, Vec<ActionId>>,
}

impl From<SubspeciesDefinition> for Subspecies {
    fn from(value: SubspeciesDefinition) -> Self {
        Subspecies {
            id: value.id,
            base: SpeciesBase {
                effects_by_level: value.effects_by_level,
                actions_by_level: value.actions_by_level,
            },
        }
    }
}

impl RegistryReferenceCollector for SubspeciesDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for effects in self.effects_by_level.values() {
            for effect in effects {
                collector.add(RegistryReference::Effect(effect.clone()));
            }
        }
        for actions in self.actions_by_level.values() {
            for action in actions {
                collector.add(RegistryReference::Action(action.clone()));
            }
        }
    }
}
