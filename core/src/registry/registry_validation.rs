use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::{
    components::{
        background::Background,
        faction::Faction,
        feat::Feat,
        id::{
            ActionId, ActionVariantId, BackgroundId, ClassId, EffectId, FactionId, FeatId, Id,
            ItemId, ResourceId, ScriptId, SpeciesId, SpellId, SubclassId, SubspeciesId,
        },
        modifier::ModifierSource,
        resource::Resource,
    },
    scripts::script::ScriptFunction,
};

/// Medium cooked solution for recording IDs during deserialization.
///
/// Instead of digging through every component to find IDs, we store a global set of
/// IDs and then whenever an ID is deserialized we add it to the set. Afterwards
/// we can then check if the recorded IDs exist in the registry.
#[derive(Debug, Default)]
struct IdRecorder {
    ids: HashSet<Id>,
}

thread_local! {
    static ID_RECORDER: RefCell<Option<IdRecorder>> = RefCell::new(Option::None);
}

pub fn record_id(id: impl FnOnce() -> Id) {
    ID_RECORDER.with_borrow_mut(|borrow| {
        if let Some(ref mut recorder) = *borrow {
            recorder.ids.insert(id());
        };
    });
}

/// Temporarily replace the global `IdRecorder` with a new one, so we only record
/// IDs that were parsed within the scope of the provided closure.
/// Returns a tuple containing the result of the closure and the set of recorded IDs.
pub fn record_ids_scoped<T>(f: impl FnOnce() -> T) -> (T, HashSet<Id>) {
    let previous = ID_RECORDER.replace(Some(IdRecorder::default()));
    let result = f();
    let recorded_ids = ID_RECORDER.replace(previous).unwrap().ids;
    (result, recorded_ids)
}

#[derive(Debug, Clone)]
pub enum RegistryReference {
    Action(ActionId),
    ActionVariant(ActionVariantId),
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

impl TryFrom<Id> for RegistryReference {
    type Error = ();

    fn try_from(value: Id) -> Result<Self, Self::Error> {
        match value {
            Id::ActionId(action_id) => Ok(RegistryReference::Action(action_id)),
            Id::ActionVariantId(action_variant_id) => {
                Ok(RegistryReference::ActionVariant(action_variant_id))
            }
            Id::AIControllerId(_) => Err(()),
            Id::BackgroundId(background_id) => Ok(RegistryReference::Background(background_id)),
            Id::ClassId(class_id) => Ok(RegistryReference::Class(class_id)),
            Id::EffectId(effect_id) => Ok(RegistryReference::Effect(effect_id)),
            Id::FactionId(faction_id) => Ok(RegistryReference::Faction(faction_id)),
            Id::FeatId(feat_id) => Ok(RegistryReference::Feat(feat_id)),
            Id::ItemId(item_id) => Ok(RegistryReference::Item(item_id)),
            Id::ResourceId(resource_id) => Ok(RegistryReference::Resource(resource_id)),
            Id::ScriptId(_) => Err(()),
            Id::SpeciesId(species_id) => Ok(RegistryReference::Species(species_id)),
            Id::SpellId(spell_id) => Ok(RegistryReference::Spell(spell_id)),
            Id::SubclassId(subclass_id) => Ok(RegistryReference::Subclass(subclass_id)),
            Id::SubspeciesId(subspecies_id) => Ok(RegistryReference::Subspecies(subspecies_id)),
        }
    }
}

impl TryInto<ModifierSource> for RegistryReference {
    type Error = ();

    fn try_into(self) -> Result<ModifierSource, Self::Error> {
        match self {
            RegistryReference::Action(id) => Ok(ModifierSource::Action(id)),
            RegistryReference::Background(id) => Ok(ModifierSource::Background(id)),
            RegistryReference::Class(id) => Ok(ModifierSource::Class(id)),
            RegistryReference::Effect(id) => Ok(ModifierSource::Effect(id)),
            RegistryReference::Faction(_) => Err(()),
            // TODO: Repeatable feats should be handled here as well?
            RegistryReference::Feat(id) => Ok(ModifierSource::Feat(id)),
            RegistryReference::Item(id) => Ok(ModifierSource::Item(id)),
            RegistryReference::Resource(id) => Err(()),
            RegistryReference::Script(_, _) => Err(()),
            RegistryReference::ScriptAnyOf(_, _) => Err(()),
            RegistryReference::Species(id) => Ok(ModifierSource::Species(id)),
            RegistryReference::Spell(id) => Err(()),
            RegistryReference::Subclass(id) => Ok(ModifierSource::Subclass(id)),
            RegistryReference::Subspecies(id) => Ok(ModifierSource::Subspecies(id)),
            RegistryReference::ActionVariant(_) => Err(()),
        }
    }
}

impl Display for RegistryReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegistryReference::Action(id) => write!(f, "Action '{}'", id),
            RegistryReference::ActionVariant(id) => write!(f, "Action variant '{}'", id),
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
