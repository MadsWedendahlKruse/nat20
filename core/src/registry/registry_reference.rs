use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    path::PathBuf,
};

use tracing::debug;

use crate::{
    components::{
        id::{
            ActionId, ActionVariantId, BackgroundId, ClassId, EffectId, FactionId, FeatId, Id,
            ItemId, ResourceId, ScriptId, SpeciesId, SpellId, SubclassId, SubspeciesId,
        },
        modifier::ModifierSource,
    },
    scripts::script::ScriptFunction,
};

pub type ReferenceMap = BTreeMap<RegistryReference, BTreeSet<ReferenceLabel>>;

/// Medium cooked solution for recording IDs during deserialization.
///
/// Instead of digging through every component to find IDs, we store a global set of
/// IDs and then whenever an ID is deserialized we add it to the set. Afterwards
/// we can then check if the recorded IDs exist in the registry.
#[derive(Debug, Default)]
struct ReferenceRecorder {
    label: ReferenceLabel,
    references: ReferenceMap,
}

impl ReferenceRecorder {
    pub fn record(&mut self, reference: RegistryReference) {
        self.record_with_label(reference, self.label.clone());
    }

    pub fn record_with_label(&mut self, reference: RegistryReference, label: ReferenceLabel) {
        if !self.references.contains_key(&reference) {
            debug!(
                "Recording new reference: {:?} with label: {:?}",
                reference, label
            );
        }

        self.references
            .entry(reference)
            .or_insert_with(BTreeSet::new)
            .insert(label);
    }
}

/// Label to associate with a registry reference. This is used to track the context
/// in which the reference was encountered, such as a file path or a set of string
/// labels.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum ReferenceLabel {
    #[default]
    None,
    /// Arbitrary string labels associated with the reference.
    Labels(Vec<String>),
    /// A file path and associated string labels for the reference.
    Path {
        /// A file path pointing to e.g. a save file.
        path: PathBuf,
        /// Labels for extra context within the file, e.g. for a save file this
        /// could be a JSON path to the specific entity component which contains
        /// the registry reference.
        labels: Vec<String>,
    },
}

impl ReferenceLabel {
    pub fn push(&mut self, label: impl Into<ReferenceLabel>) {
        match (&mut *self, label.into()) {
            (ReferenceLabel::None, new_label) => {
                *self = new_label;
            }
            (ReferenceLabel::Labels(labels), ReferenceLabel::Labels(mut new_labels)) => {
                labels.append(&mut new_labels);
            }
            (
                ReferenceLabel::Path { labels, .. },
                ReferenceLabel::Path {
                    labels: mut new_labels,
                    ..
                },
            ) => {
                labels.append(&mut new_labels);
            }
            (ReferenceLabel::Path { labels, .. }, ReferenceLabel::Labels(mut new_labels)) => {
                labels.append(&mut new_labels);
            }
            _ => {}
        }
    }
}

impl From<PathBuf> for ReferenceLabel {
    fn from(path: PathBuf) -> Self {
        ReferenceLabel::Path {
            path,
            labels: Vec::new(),
        }
    }
}

impl From<Vec<String>> for ReferenceLabel {
    fn from(labels: Vec<String>) -> Self {
        ReferenceLabel::Labels(labels)
    }
}

impl From<&str> for ReferenceLabel {
    fn from(label: &str) -> Self {
        ReferenceLabel::Labels(vec![label.to_string()])
    }
}

thread_local! {
    static REFERENCE_RECORDER: RefCell<Option<ReferenceRecorder>> = RefCell::new(Option::None);
}

fn with_recorder<T>(f: impl FnOnce(&mut ReferenceRecorder) -> T) -> Option<T> {
    REFERENCE_RECORDER.with_borrow_mut(|borrow| {
        if let Some(ref mut recorder) = *borrow {
            Some(f(recorder))
        } else {
            None
        }
    })
}

pub fn record_id(id: impl FnOnce() -> Id) {
    with_recorder(|recorder| {
        if let Ok(reference) = RegistryReference::try_from(id()) {
            recorder.record(reference);
        }
    });
}

pub fn record_reference(reference: impl FnOnce() -> RegistryReference) {
    with_recorder(|recorder| {
        recorder.record(reference());
    });
}

/// Temporarily replace the global `ReferenceRecorder` with a new one, so we only
/// record references that were parsed within the scope of the provided closure.
/// Returns a tuple containing the result of the closure and the recorded references.
pub fn record_references_scoped<T>(f: impl FnOnce() -> T) -> (T, ReferenceMap) {
    let previous = REFERENCE_RECORDER.replace(Some(ReferenceRecorder::default()));
    let result = f();
    let recorded = REFERENCE_RECORDER.replace(previous).unwrap().references;
    (result, recorded)
}

pub fn with_label<T>(label: impl Into<ReferenceLabel>, f: impl FnOnce() -> T) -> T {
    let previous_labels = with_recorder(|recorder| {
        let previous_labels = recorder.label.clone();
        recorder.label.push(label);
        previous_labels
    });
    let result = f();
    if let Some(previous_labels) = previous_labels {
        with_recorder(|recorder| {
            recorder.label = previous_labels;
        });
    }
    result
}

/// Merge references recorded in a nested scope into the current one, with `label`
/// in front of the labels they were recorded under.
pub fn merge_references(label: impl Into<ReferenceLabel>, references: ReferenceMap) {
    let label = label.into();
    with_recorder(|recorder| {
        for (reference, inner_labels) in references {
            for inner_label in inner_labels {
                let mut full_label = recorder.label.clone();
                full_label.push(label.clone());
                full_label.push(inner_label);
                recorder.record_with_label(reference.clone(), full_label);
            }
        }
    });
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    Script(ScriptId, Vec<ScriptFunction>),
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
                write!(f, "Script '{}' (functions: {:?})", id, function)
            }
            RegistryReference::Species(id) => write!(f, "Species '{}'", id),
            RegistryReference::Spell(id) => write!(f, "Spell '{}'", id),
            RegistryReference::Subclass(id) => write!(f, "Subclass '{}'", id),
            RegistryReference::Subspecies(id) => write!(f, "Subspecies '{}'", id),
        }
    }
}
