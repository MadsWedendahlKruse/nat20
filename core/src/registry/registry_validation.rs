use std::{collections::HashMap, fmt::Display};

use crate::{
    components::{
        background::Background,
        faction::Faction,
        feat::Feat,
        id::{
            ActionId, BackgroundId, ClassId, EffectId, FactionId, FeatId, ItemId, ResourceId,
            ScriptId, SpeciesId, SpellId, SubclassId, SubspeciesId,
        },
        resource::Resource,
    },
    scripts::script::ScriptFunction,
};

#[derive(Debug, Clone)]
pub enum RegistryReference {
    Action(ActionId),
    Background(BackgroundId),
    Class(ClassId),
    Effect(EffectId),
    Faction(FactionId),
    Feat(FeatId),
    Item(ItemId),
    Resource(ResourceId),
    Script(ScriptId, ScriptFunction),
    ScriptAnyOf(ScriptId, Vec<ScriptFunction>),
    Species(SpeciesId),
    Spell(SpellId),
    Subclass(SubclassId),
    Subspecies(SubspeciesId),
}

impl Display for RegistryReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegistryReference::Action(id) => write!(f, "Action '{}'", id),
            RegistryReference::Background(id) => write!(f, "Background '{}'", id),
            RegistryReference::Class(id) => write!(f, "Class '{}'", id),
            RegistryReference::Effect(id) => write!(f, "Effect '{}'", id),
            RegistryReference::Faction(id) => write!(f, "Faction '{}'", id),
            RegistryReference::Feat(id) => write!(f, "Feat '{}'", id),
            RegistryReference::Item(id) => write!(f, "Item '{}'", id),
            RegistryReference::Resource(id) => write!(f, "Resource '{}'", id),
            RegistryReference::Script(id, function) => {
                write!(f, "Script '{}' (function: {:?})", id, function)
            }
            RegistryReference::ScriptAnyOf(id, functions) => {
                write!(f, "Script '{}' (any function of: {:?})", id, functions)
            }
            RegistryReference::Species(id) => write!(f, "Species '{}'", id),
            RegistryReference::Spell(id) => write!(f, "Spell '{}'", id),
            RegistryReference::Subclass(id) => write!(f, "Subclass '{}'", id),
            RegistryReference::Subspecies(id) => write!(f, "Subspecies '{}'", id),
        }
    }
}

#[derive(Debug, Default)]
pub struct ReferenceCollector {
    pub references: Vec<RegistryReference>,
}

impl ReferenceCollector {
    pub fn new() -> Self {
        Self {
            references: Vec::new(),
        }
    }

    pub fn into_references(self) -> Vec<RegistryReference> {
        self.references
    }
}

impl ReferenceCollector {
    pub fn add(&mut self, reference: RegistryReference) {
        self.references.push(reference);
    }
}

pub trait RegistryReferenceCollector {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector);
}

impl<T: RegistryReferenceCollector> RegistryReferenceCollector for Vec<T> {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for item in self {
            item.collect_registry_references(collector);
        }
    }
}

impl<T: RegistryReferenceCollector> RegistryReferenceCollector for Option<T> {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        if let Some(value) = self {
            value.collect_registry_references(collector);
        }
    }
}

impl<K, V> RegistryReferenceCollector for HashMap<K, V>
where
    V: RegistryReferenceCollector,
{
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for value in self.values() {
            value.collect_registry_references(collector);
        }
    }
}

// I don't know where to put these, so I'm hiding them in here :^)

impl RegistryReferenceCollector for Background {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        collector.add(RegistryReference::Feat(self.feat.clone()));
        self.equipment.collect_registry_references(collector);
    }
}

impl RegistryReferenceCollector for Faction {
    fn collect_registry_references(&self, _collector: &mut ReferenceCollector) {
        // Factions currently have no registry references
    }
}

impl RegistryReferenceCollector for Feat {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for effect in self.effects() {
            collector.add(RegistryReference::Effect(effect.clone()));
        }
        for prompt in self.prompts() {
            prompt.collect_registry_references(collector);
        }
    }
}

impl RegistryReferenceCollector for Resource {
    fn collect_registry_references(&self, _collector: &mut ReferenceCollector) {
        // Resources currently have no registry references
    }
}
