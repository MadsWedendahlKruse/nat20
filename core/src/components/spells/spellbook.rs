//! spellbook.rs
//!
//! A class-agnostic Spellbook that supports:
//! - Learned casters (e.g., Sorcerer/Bard/Warlock) with optional “prepared” step
//! - Prepared casters that know “entire class list up to max level” (e.g., Cleric/Paladin)
//! - Always-prepared/granted spells that do not count against preparation limits
//! - Clean “known vs castable” queries
//! - ActionProvider implementation that generates spell actions with upcasting and slot costs
//!
//! Design principles:
//! 1) Rules are per-class (ability, spell list, access model, readiness model).
//! 2) Selections are per-class (chosen cantrips / learned / prepared / always-prepared).
//! 3) “Known” is computed on demand for EntireClassList casters (not stored).

use std::{
    cmp::max,
    collections::{HashMap, HashSet},
};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        actions::action::{
            ActionContext, ActionMap, ActionProvider, AttackRollProvider, SavingThrowProvider,
        },
        class::{CastingReadinessModel, ClassAndSubclass, SpellAccessModel, SpellcastingRules},
        d20::{D20Check, D20CheckDC},
        damage::{AttackRange, AttackRoll, AttackSource},
        id::{EffectId, FeatId, ItemId, ResourceId, SpeciesId, SpellId},
        modifier::{Modifiable, ModifierSet, ModifierSource},
        proficiency::{Proficiency, ProficiencyLevel},
        resource::{ResourceAmount, ResourceAmountMap, ResourceBudgetKind, ResourceMap},
        saving_throw::{SavingThrowDC, SavingThrowKind},
        spells::spell::{ConcentrationTracker, SPELL_CASTING_ABILITIES},
    },
    registry::registry::{ClassesRegistry, SpellsRegistry},
    systems,
};

/// A deterministic bounded set:
/// - stable iteration order (insertion order)
/// - stable truncation behavior when shrinking caps
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BoundedSpellSet {
    max_size: usize,
    spell_ids: Vec<SpellId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundedSpellSetError {
    MaxSizeReached,
    SpellAlreadyPresent,
    SpellNotFound,
}

impl BoundedSpellSet {
    pub fn new(max_size: usize) -> Self {
        Self {
            max_size,
            spell_ids: Vec::new(),
        }
    }

    pub fn max_size(&self) -> usize {
        self.max_size
    }

    pub fn len(&self) -> usize {
        self.spell_ids.len()
    }

    pub fn is_empty(&self) -> bool {
        self.spell_ids.is_empty()
    }

    pub fn as_slice(&self) -> &[SpellId] {
        &self.spell_ids
    }

    pub fn iter(&self) -> impl Iterator<Item = &SpellId> {
        self.spell_ids.iter()
    }

    pub fn contains(&self, spell_id: &SpellId) -> bool {
        self.spell_ids.iter().any(|id| id == spell_id)
    }

    pub fn try_add(&mut self, spell_id: SpellId) -> Result<(), BoundedSpellSetError> {
        if self.contains(&spell_id) {
            return Err(BoundedSpellSetError::SpellAlreadyPresent);
        }
        if self.spell_ids.len() >= self.max_size {
            return Err(BoundedSpellSetError::MaxSizeReached);
        }
        self.spell_ids.push(spell_id);
        Ok(())
    }

    pub fn remove(&mut self, spell_id: &SpellId) -> Result<(), BoundedSpellSetError> {
        let index = self
            .spell_ids
            .iter()
            .position(|id| id == spell_id)
            .ok_or(BoundedSpellSetError::SpellNotFound)?;
        self.spell_ids.remove(index);
        Ok(())
    }

    pub fn set_max_size(&mut self, new_max_size: usize) {
        self.max_size = new_max_size;
        if self.spell_ids.len() > self.max_size {
            // Deterministic: remove newest picks first
            self.spell_ids.truncate(self.max_size);
        }
    }
}

/// The per-class *mutable choices* the player (or progression) makes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassSpellSelections {
    pub cantrips: BoundedSpellSet,

    /// Used when access_model == Learned
    pub learned_spells: BoundedSpellSet,

    /// Used when readiness_model == PreparedCaster
    pub prepared_spells: BoundedSpellSet,

    /// Always available, does not count against prepared cap.
    pub always_prepared: HashSet<SpellId>,
}

impl ClassSpellSelections {
    pub fn new(max_cantrips: usize, max_learned_spells: usize, max_prepared_spells: usize) -> Self {
        Self {
            cantrips: BoundedSpellSet::new(max_cantrips),
            learned_spells: BoundedSpellSet::new(max_learned_spells),
            prepared_spells: BoundedSpellSet::new(max_prepared_spells),
            always_prepared: HashSet::new(),
        }
    }
}

/// The per-class caster state (rules + caps + level-derived info).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassSpellcastingState {
    pub selections: ClassSpellSelections,
}

impl ClassSpellcastingState {
    pub fn new(max_cantrips: usize, max_learned_spells: usize, max_prepared_spells: usize) -> Self {
        Self {
            selections: ClassSpellSelections::new(
                max_cantrips,
                max_learned_spells,
                max_prepared_spells,
            ),
        }
    }

    pub fn set_caps(
        &mut self,
        max_cantrips: usize,
        max_learned_spells: usize,
        max_prepared_spells: usize,
    ) {
        self.selections.cantrips.set_max_size(max_cantrips);
        self.selections
            .learned_spells
            .set_max_size(max_learned_spells);
        self.selections
            .prepared_spells
            .set_max_size(max_prepared_spells);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum GrantedSpellSource {
    Item(ItemId),
    Feat(FeatId),
    Species(SpeciesId),
    Effect(EffectId),
    ParentSpell(SpellId),
}

/// A class-independent spell source (items/feats/race/boons).
/// Keep this small at first; you can grow it later into “charges”, “once per rest”, etc.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GrantedSpellMap {
    /// Spells that are castable without being prepared/known by any class state.
    /// These do not consume spell slots. The value is the level at which they are
    /// castable
    pub spells: HashMap<SpellId, u8>,
}

impl GrantedSpellMap {
    pub fn new() -> Self {
        Self {
            spells: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SpellSource {
    Class(ClassAndSubclass),
    Granted {
        source: GrantedSpellSource,
        level: u8,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpellbookError {
    ClassNotFound,
    SpellNotOnClassList,
    SpellTooHighLevel,
    CannotLearnForThisClass,
    CannotPrepareForThisClass,
    NotKnownSoCannotPrepare,
    NotACantrip,
    NotALevelledSpell,
    CapacityReached,
    AlreadyPresent,
    NotFound,
}

#[derive(Debug, Clone)]
pub struct Spellbook {
    /// Per-class spellcasting state.
    class_states: HashMap<ClassAndSubclass, ClassSpellcastingState>,
    /// External sources (items/feats/race).
    granted: HashMap<GrantedSpellSource, GrantedSpellMap>,
    concentration: ConcentrationTracker,

    saving_throw_modifiers: ModifierSet,
    attack_roll_modifiers: ModifierSet,
}

impl Spellbook {
    pub fn new() -> Self {
        Self {
            class_states: HashMap::new(),
            granted: HashMap::new(),
            concentration: ConcentrationTracker::default(),
            saving_throw_modifiers: ModifierSet::new(),
            attack_roll_modifiers: ModifierSet::new(),
        }
    }

    pub fn insert_granted_spell_source(
        &mut self,
        source: GrantedSpellSource,
        spell_set: GrantedSpellMap,
    ) {
        self.granted.insert(source, spell_set);
    }

    pub fn granted_spell_set(&self, source: &GrantedSpellSource) -> Option<&GrantedSpellMap> {
        self.granted.get(source)
    }

    pub fn granted_spell_set_mut(
        &mut self,
        source: &GrantedSpellSource,
    ) -> Option<&mut GrantedSpellMap> {
        self.granted.get_mut(source)
    }

    pub fn insert_class_state(
        &mut self,
        class_and_subclass: ClassAndSubclass,
        state: ClassSpellcastingState,
    ) {
        self.class_states.insert(class_and_subclass, state);
    }

    pub fn class_state(
        &self,
        class_and_subclass: &ClassAndSubclass,
    ) -> Option<&ClassSpellcastingState> {
        self.class_states.get(class_and_subclass)
    }

    pub fn class_state_mut(
        &mut self,
        class_and_subclass: &ClassAndSubclass,
    ) -> Option<&mut ClassSpellcastingState> {
        self.class_states.get_mut(class_and_subclass)
    }

    /// Computed known spells for a class:
    /// - Learned: learned_spells (+ cantrips, always_prepared)
    /// - EntireClassList: all spells from class list up to max_spell_level (+ cantrips, always_prepared)
    pub fn known_spells_for_class(
        &self,
        class_and_subclass: &ClassAndSubclass,
        resources: &ResourceMap,
    ) -> Result<HashSet<SpellId>, SpellbookError> {
        let state = self
            .class_states
            .get(class_and_subclass)
            .ok_or(SpellbookError::ClassNotFound)?;

        let mut known = HashSet::<SpellId>::new();

        // Cantrips are always "known" in practice.
        for cantrip_id in state.selections.cantrips.iter() {
            known.insert(cantrip_id.clone());
        }

        // Always prepared is at least known.
        known.extend(state.selections.always_prepared.iter().cloned());

        if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
            && let Some(spellcasting_rules) = class.spellcasting_rules(&class_and_subclass.subclass)
        {
            match spellcasting_rules.access_model {
                SpellAccessModel::Learned => {
                    for spell_id in state.selections.learned_spells.iter() {
                        known.insert(spell_id.clone());
                    }
                }
                SpellAccessModel::EntireClassList => {
                    // Compute: all spells on the class list that are within max spell level.
                    for spell_id in spellcasting_rules.spell_list.iter() {
                        let spell = SpellsRegistry::get(spell_id)
                            .unwrap_or_else(|| panic!("Missing spell in registry: {}", spell_id));

                        if spell.base_level()
                            <= Self::max_spell_level(
                                &spellcasting_rules.spellcasting_resource,
                                resources,
                            )
                        {
                            known.insert(spell_id.clone());
                        }
                    }
                }
            }
        }

        Ok(known)
    }

    /// Computed castable spells for a class:
    /// - PreparedCaster: prepared_spells + always_prepared + cantrips
    /// - KnownCaster: known spells directly
    pub fn castable_spells_for_class(
        &self,
        class_and_subclass: &ClassAndSubclass,
        resources: &ResourceMap,
    ) -> Result<HashSet<SpellId>, SpellbookError> {
        let state = self
            .class_states
            .get(class_and_subclass)
            .ok_or(SpellbookError::ClassNotFound)?;

        let mut castable = HashSet::<SpellId>::new();

        // Cantrips are castable once known/chosen.
        for cantrip_id in state.selections.cantrips.iter() {
            castable.insert(cantrip_id.clone());
        }

        // Always prepared spells are castable.
        castable.extend(state.selections.always_prepared.iter().cloned());

        if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
            && let Some(spellcasting_rules) = class.spellcasting_rules(&class_and_subclass.subclass)
        {
            match spellcasting_rules.readiness_model {
                CastingReadinessModel::Prepared => {
                    for prepared_id in state.selections.prepared_spells.iter() {
                        castable.insert(prepared_id.clone());
                    }
                }
                CastingReadinessModel::Known => {
                    let known = self.known_spells_for_class(class_and_subclass, resources)?;
                    castable.extend(known);
                }
            }
        }

        Ok(castable)
    }

    /// Choose a cantrip for a class.
    pub fn try_choose_cantrip(
        &mut self,
        class_and_subclass: &ClassAndSubclass,
        spell_id: &SpellId,
    ) -> Result<(), SpellbookError> {
        let state = self
            .class_states
            .get_mut(class_and_subclass)
            .ok_or(SpellbookError::ClassNotFound)?;

        if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
            && let Some(spellcasting_rules) = class.spellcasting_rules(&class_and_subclass.subclass)
        {
            if !spellcasting_rules.spell_list.contains(spell_id) {
                return Err(SpellbookError::SpellNotOnClassList);
            }
        }

        let spell = SpellsRegistry::get(spell_id)
            .unwrap_or_else(|| panic!("Missing spell in registry: {}", spell_id));
        if !spell.is_cantrip() {
            return Err(SpellbookError::NotACantrip);
        }

        match state.selections.cantrips.try_add(spell_id.clone()) {
            Ok(()) => Ok(()),
            Err(BoundedSpellSetError::MaxSizeReached) => Err(SpellbookError::CapacityReached),
            Err(BoundedSpellSetError::SpellAlreadyPresent) => Err(SpellbookError::AlreadyPresent),
            Err(_) => Err(SpellbookError::NotFound),
        }
    }

    /// Learn a levelled spell (only meaningful for Learned access model).
    pub fn try_learn_spell(
        &mut self,
        class_and_subclass: &ClassAndSubclass,
        spell_id: &SpellId,
        resources: &ResourceMap,
    ) -> Result<(), SpellbookError> {
        let state = self
            .class_states
            .get_mut(class_and_subclass)
            .ok_or(SpellbookError::ClassNotFound)?;

        if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
            && let Some(spellcasting_rules) = class.spellcasting_rules(&class_and_subclass.subclass)
        {
            if spellcasting_rules.access_model != SpellAccessModel::Learned {
                return Err(SpellbookError::CannotLearnForThisClass);
            }
            if !spellcasting_rules.spell_list.contains(spell_id) {
                return Err(SpellbookError::SpellNotOnClassList);
            }

            let spell = SpellsRegistry::get(spell_id)
                .unwrap_or_else(|| panic!("Missing spell in registry: {}", spell_id));
            if spell.is_cantrip() {
                return Err(SpellbookError::NotALevelledSpell);
            }
            if spell.base_level()
                > Self::max_spell_level(&spellcasting_rules.spellcasting_resource, resources)
            {
                return Err(SpellbookError::SpellTooHighLevel);
            }
        }

        match state.selections.learned_spells.try_add(spell_id.clone()) {
            Ok(()) => Ok(()),
            Err(BoundedSpellSetError::MaxSizeReached) => Err(SpellbookError::CapacityReached),
            Err(BoundedSpellSetError::SpellAlreadyPresent) => Err(SpellbookError::AlreadyPresent),
            Err(_) => Err(SpellbookError::NotFound),
        }
    }

    /// Prepare a spell (only meaningful for PreparedCaster readiness model).
    pub fn try_prepare_spell(
        &mut self,
        class_and_subclass: &ClassAndSubclass,
        spell_id: &SpellId,
        resources: &ResourceMap,
    ) -> Result<(), SpellbookError> {
        let state = self
            .class_states
            .get_mut(class_and_subclass)
            .ok_or(SpellbookError::ClassNotFound)?;
        if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
            && let Some(spellcasting_rules) = class.spellcasting_rules(&class_and_subclass.subclass)
        {
            if spellcasting_rules.readiness_model != CastingReadinessModel::Prepared {
                return Err(SpellbookError::CannotPrepareForThisClass);
            }
            if !spellcasting_rules.spell_list.contains(spell_id) {
                return Err(SpellbookError::SpellNotOnClassList);
            }
            // Must be "known" in the sense of your access model:
            // - EntireClassList: anything on list within max level is known
            // - Learned: must have been learned
            let is_known_for_class = match spellcasting_rules.access_model {
                SpellAccessModel::EntireClassList => true,
                SpellAccessModel::Learned => state.selections.learned_spells.contains(spell_id),
            };
            if !is_known_for_class {
                return Err(SpellbookError::NotKnownSoCannotPrepare);
            }

            let spell = SpellsRegistry::get(spell_id)
                .unwrap_or_else(|| panic!("Missing spell in registry: {}", spell_id));
            if spell.is_cantrip() {
                // Cantrips are chosen, not prepared
                return Err(SpellbookError::NotALevelledSpell);
            }
            if spell.base_level()
                > Self::max_spell_level(&spellcasting_rules.spellcasting_resource, resources)
            {
                return Err(SpellbookError::SpellTooHighLevel);
            }
        }

        // Always-prepared spells don't need to be in prepared_spells set.
        if state.selections.always_prepared.contains(spell_id) {
            return Ok(());
        }

        match state.selections.prepared_spells.try_add(spell_id.clone()) {
            Ok(()) => Ok(()),
            Err(BoundedSpellSetError::MaxSizeReached) => Err(SpellbookError::CapacityReached),
            Err(BoundedSpellSetError::SpellAlreadyPresent) => Err(SpellbookError::AlreadyPresent),
            Err(_) => Err(SpellbookError::NotFound),
        }
    }

    pub fn add_spell(
        &mut self,
        spell_id: &SpellId,
        source: &SpellSource,
        resources: &ResourceMap,
    ) -> Result<(), SpellbookError> {
        match source {
            SpellSource::Class(class_and_subclass) => {
                if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
                    && let Some(spellcasting_rules) =
                        class.spellcasting_rules(&class_and_subclass.subclass)
                {
                    if SpellsRegistry::get(spell_id).unwrap().is_cantrip() {
                        self.try_choose_cantrip(class_and_subclass, spell_id)?;
                        return Ok(());
                    }

                    match spellcasting_rules.access_model {
                        SpellAccessModel::Learned => {
                            self.try_learn_spell(class_and_subclass, spell_id, resources)?;
                        }
                        SpellAccessModel::EntireClassList => {
                            // No action needed; all spells on class list are known.
                        }
                    }

                    match spellcasting_rules.readiness_model {
                        CastingReadinessModel::Prepared => {
                            // TODO: Not sure if it's correct to (potentially) error here?
                            self.try_prepare_spell(class_and_subclass, spell_id, resources)?;
                        }
                        CastingReadinessModel::Known => {
                            // No action needed; all known spells are castable.
                        }
                    }
                }
            }

            SpellSource::Granted { source, level } => {
                // TODO: Is this all we need here?
                self.granted
                    .entry(source.clone())
                    .or_insert_with(GrantedSpellMap::new)
                    .spells
                    .insert(spell_id.clone(), *level);
            }
        }

        // TODO: Little bit funky
        if let Some(spell) = SpellsRegistry::get(spell_id) {
            for (spell_id, level) in spell.granted_spells() {
                self.add_spell(
                    spell_id,
                    &SpellSource::Granted {
                        source: GrantedSpellSource::ParentSpell(spell.id().clone()),
                        level: *level,
                    },
                    resources,
                )?;
            }
        }

        Ok(())
    }

    pub fn unprepare_spell(
        &mut self,
        class_and_subclass: &ClassAndSubclass,
        spell_id: &SpellId,
    ) -> Result<(), SpellbookError> {
        let state = self
            .class_states
            .get_mut(class_and_subclass)
            .ok_or(SpellbookError::ClassNotFound)?;
        if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
            && let Some(spellcasting_rules) = class.spellcasting_rules(&class_and_subclass.subclass)
        {
            if spellcasting_rules.readiness_model != CastingReadinessModel::Prepared {
                return Err(SpellbookError::CannotPrepareForThisClass);
            }
        }
        if state.selections.always_prepared.contains(spell_id) {
            // Always-prepared isn't removed here
            return Ok(());
        }
        match state.selections.prepared_spells.remove(spell_id) {
            Ok(()) => Ok(()),
            Err(BoundedSpellSetError::SpellNotFound) => Err(SpellbookError::NotFound),
            _ => Err(SpellbookError::NotFound),
        }
    }

    pub fn remove_spell(
        &mut self,
        spell_id: &SpellId,
        source: &SpellSource,
    ) -> Result<(), SpellbookError> {
        match source {
            SpellSource::Class(class_and_subclass) => {
                let state = self
                    .class_states
                    .get_mut(class_and_subclass)
                    .ok_or(SpellbookError::ClassNotFound)?;

                if state.selections.cantrips.contains(spell_id) {
                    match state.selections.cantrips.remove(spell_id) {
                        Ok(()) => return Ok(()),
                        Err(BoundedSpellSetError::SpellNotFound) => {
                            return Err(SpellbookError::NotFound);
                        }
                        _ => return Err(SpellbookError::NotFound),
                    }
                }

                if state.selections.learned_spells.contains(spell_id) {
                    match state.selections.learned_spells.remove(spell_id) {
                        Ok(()) => return Ok(()),
                        Err(BoundedSpellSetError::SpellNotFound) => {
                            return Err(SpellbookError::NotFound);
                        }
                        _ => return Err(SpellbookError::NotFound),
                    }
                }

                if state.selections.prepared_spells.contains(spell_id) {
                    match state.selections.prepared_spells.remove(spell_id) {
                        Ok(()) => return Ok(()),
                        Err(BoundedSpellSetError::SpellNotFound) => {
                            return Err(SpellbookError::NotFound);
                        }
                        _ => return Err(SpellbookError::NotFound),
                    }
                }

                Ok(())
            }

            SpellSource::Granted { source, level } => {
                if let Some(granted_set) = self.granted.get_mut(source) {
                    if let Some(_) = granted_set.spells.remove(spell_id) {
                        return Ok(());
                    } else {
                        return Err(SpellbookError::NotFound);
                    }
                }
                Err(SpellbookError::NotFound)
            }
        }
    }

    /// Global list of castable spells across:
    /// - all class states
    /// - granted sources (items/feats)
    pub fn all_castable_spells(&self, resources: &ResourceMap) -> HashSet<(SpellId, SpellSource)> {
        let mut castable = HashSet::new();

        // Granted cantrips/spells
        for (source, granted_set) in self.granted.iter() {
            for (spell_id, level) in granted_set.spells.iter() {
                castable.insert((
                    spell_id.clone(),
                    SpellSource::Granted {
                        source: source.clone(),
                        level: *level,
                    },
                ));
            }
        }

        // Per-class castables
        for (class_and_subclass, state) in self.class_states.iter() {
            if let Ok(class_castable) =
                self.castable_spells_for_class(class_and_subclass, resources)
            {
                for spell_id in class_castable.iter() {
                    castable.insert((
                        spell_id.clone(),
                        SpellSource::Class(class_and_subclass.clone()),
                    ));
                }
            }
        }

        castable
    }

    pub fn max_spell_level(spellcasting_resource: &ResourceId, resources: &ResourceMap) -> u8 {
        resources
            .get(spellcasting_resource)
            .and_then(|budget| match budget {
                ResourceBudgetKind::Tiered(tiers) => tiers.keys().cloned().max(),
                _ => None,
            })
            .unwrap_or(0)
    }

    pub fn concentration_tracker(&self) -> &ConcentrationTracker {
        &self.concentration
    }

    pub fn concentration_tracker_mut(&mut self) -> &mut ConcentrationTracker {
        &mut self.concentration
    }

    pub fn spellcasting_ability(
        &self,
        world: &World,
        entity: Entity,
        source: &SpellSource,
    ) -> Ability {
        match source {
            SpellSource::Class(class_and_subclass) => {
                if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
                    && let Some(spellcasting_rules) =
                        class.spellcasting_rules(&class_and_subclass.subclass)
                {
                    spellcasting_rules.spellcasting_ability
                } else {
                    panic!(
                        "Class {:?} does not have spellcasting capabilities",
                        class_and_subclass
                    );
                }
            }
            SpellSource::Granted { .. } => {
                // Use the highest spellcasting ability
                systems::helpers::get_component::<AbilityScoreMap>(world, entity)
                    .get_max_score(SPELL_CASTING_ABILITIES)
                    .0
            }
        }
    }
}

impl AttackRollProvider for Spellbook {
    fn attack_roll(
        &self,
        world: &World,
        entity: Entity,
        target: Entity,
        context: &ActionContext,
    ) -> AttackRoll {
        if let ActionContext::Spell {
            id: spell_id,
            source,
            ..
        } = context
        {
            let ability_scores = systems::helpers::get_component::<AbilityScoreMap>(world, entity);
            let spellcasting_ability = self.spellcasting_ability(world, entity, source);

            let mut roll = D20Check::new(Proficiency::new(
                ProficiencyLevel::Proficient,
                ModifierSource::Base,
            ));
            let spellcasting_modifier = ability_scores
                .ability_modifier(&spellcasting_ability)
                .total();
            roll.add_modifier(
                ModifierSource::Ability(spellcasting_ability),
                spellcasting_modifier,
            );

            let spell = SpellsRegistry::get(spell_id).expect("Spell must exist in registry");
            let spell_range = (spell.action().targeting)(world, entity, context).range;
            let attack_range = if spell_range.is_melee() {
                AttackRange::Melee
            } else {
                AttackRange::Ranged
            };

            return AttackRoll::new(roll, AttackSource::Spell, attack_range);
        }
        panic!("Action context must be Spell");
    }
}

const BASE_SAVE_DC: i32 = 8;

impl SavingThrowProvider for Spellbook {
    fn saving_throw(
        &self,
        world: &World,
        entity: Entity,
        context: &ActionContext,
        kind: SavingThrowKind,
    ) -> SavingThrowDC {
        let source = if let ActionContext::Spell { source, .. } = context {
            source
        } else {
            panic!("Action context must be Spell");
        };

        let ability_scores = systems::helpers::get_component::<AbilityScoreMap>(world, entity);
        let spellcasting_ability = self.spellcasting_ability(world, entity, source);
        let proficiency_bonus = systems::helpers::level(world, entity)
            .unwrap()
            .proficiency_bonus();

        let mut spell_save_dc = ModifierSet::new();
        spell_save_dc.add_modifier(ModifierSource::Base, BASE_SAVE_DC);
        let spellcasting_modifier = ability_scores
            .ability_modifier(&spellcasting_ability)
            .total();
        spell_save_dc.add_modifier(
            ModifierSource::Ability(spellcasting_ability),
            spellcasting_modifier,
        );
        // TODO: Not sure if Proficiency is the correct modifier source here, since I don't think
        // you can have e.g. Expertise in spell save DCs.
        spell_save_dc.add_modifier(
            ModifierSource::Proficiency(ProficiencyLevel::Proficient),
            proficiency_bonus as i32,
        );

        D20CheckDC {
            key: kind,
            dc: spell_save_dc,
        }
    }
}

impl ActionProvider for Spellbook {
    fn actions(&self, world: &World, entity: Entity) -> ActionMap {
        let mut actions = ActionMap::new();

        // 1) Build a set of all castable spells (class + granted).
        let resources = systems::helpers::get_component::<ResourceMap>(world, entity);
        let castable_spell_ids = self.all_castable_spells(&resources);

        // 2) Emit actions for each spell.
        for (spell_id, source) in castable_spell_ids.iter() {
            let spell = SpellsRegistry::get(spell_id)
                .unwrap_or_else(|| panic!("Missing spell in registry: {}", spell_id));

            // Cantrips: single context, level 0.
            if spell.is_cantrip() {
                let context = ActionContext::Spell {
                    id: spell_id.clone(),
                    source: source.clone(),
                    level: 0,
                };

                actions.insert(
                    spell.action().id().clone(),
                    vec![(context, spell.action().resource_cost().clone())],
                );
                continue;
            }

            match source {
                SpellSource::Granted { level, .. } => {
                    let context = ActionContext::Spell {
                        id: spell_id.clone(),
                        source: source.clone(),
                        level: *level,
                    };

                    actions.insert(
                        spell.action().id().clone(),
                        vec![(context, spell.action().resource_cost().clone())],
                    );
                    continue;
                }

                SpellSource::Class(class_and_subclass) => {
                    let class = ClassesRegistry::get(&class_and_subclass.class).unwrap();
                    let spellcasting_rules = class
                        .spellcasting_rules(&class_and_subclass.subclass)
                        .unwrap();
                    let spellcasting_resource = &spellcasting_rules.spellcasting_resource;

                    let Some(spellcasting_resource_budget) = resources.get(spellcasting_resource)
                    else {
                        continue;
                    };

                    let available_spell_levels = match spellcasting_resource_budget {
                        ResourceBudgetKind::Tiered(tiers) => {
                            tiers.keys().cloned().collect::<Vec<_>>()
                        }
                        _ => continue,
                    };

                    let min_spell_level = max(
                        *available_spell_levels.iter().min().unwrap_or(&0),
                        spell.base_level(),
                    );
                    let max_spell_level = available_spell_levels.iter().max().unwrap_or(&0);

                    for cast_level in min_spell_level..=*max_spell_level {
                        let context = ActionContext::Spell {
                            id: spell_id.clone(),
                            source: source.clone(),
                            level: cast_level,
                        };

                        let mut resource_cost: ResourceAmountMap =
                            spell.action().resource_cost().clone();

                        resource_cost.insert(
                            spellcasting_rules.spellcasting_resource.clone(),
                            ResourceAmount::Tiered {
                                tier: cast_level,
                                amount: 1,
                            },
                        );

                        actions
                            .entry(spell.action().id().clone())
                            .or_insert_with(Vec::new)
                            .push((context, resource_cost));
                    }
                }
            }
        }

        actions
    }
}
