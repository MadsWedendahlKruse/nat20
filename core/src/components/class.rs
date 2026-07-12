use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        ability::{Ability, AbilityScoreDistribution},
        dice::DieSize,
        id::{ActionId, ClassId, EffectId, IdProvider, ResourceId, SpellId, SubclassId},
        items::equipment::{armor::ArmorType, weapon::WeaponCategory},
        level_up::{ChoiceItem, ChoiceSpec, LevelUpPrompt},
        modifier::ModifierSource,
        resource::ResourceBudgetKind,
        skill::Skill,
    },
    registry::{registry::SubclassesRegistry, serialize::class::ClassDefinition},
};

/// Classes and subclasses share a lot of common properties, so we define a base struct
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ClassBase {
    /// Skills that can be chosen from when gaining the (sub)class
    #[serde(default)]
    pub skill_proficiencies: HashSet<Skill>,
    /// The number of skill proficiencies the character can choose
    #[serde(default)]
    pub skill_prompts: u8,

    #[serde(default)]
    pub armor_proficiencies: HashSet<ArmorType>,
    #[serde(default)]
    pub weapon_proficiencies: HashSet<WeaponCategory>,

    /// Spellcasting progression, defining how many spells slots are available per level, if any.
    /// This is usually defined by the class, but certain subclasses might want to override it.
    /// For example, a subclass of a Fighter might gain some spellcasting abilities.
    #[serde(default)]
    pub spellcasting: Option<SpellcastingRules>,
    /// Passive effects that are always active for the class or subclass.
    #[serde(default)]
    pub effects_by_level: HashMap<u8, Vec<EffectId>>,
    #[serde(default)]
    pub resources_by_level: HashMap<u8, Vec<(ResourceId, ResourceBudgetKind, bool)>>,
    /// Class specific prompts that can be made at each level.
    /// For example, a Fighter might choose a fighting style at level 1.
    /// TODO: Include subclass prompts?
    #[serde(default)]
    pub prompts_by_level: HashMap<u8, Vec<LevelUpPrompt>>,
    /// Actions that are available at each level.
    #[serde(default)]
    pub actions_by_level: HashMap<u8, Vec<ActionId>>,
}

/// How a class gets access to spells (i.e., what the "known pool" means).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SpellAccessModel {
    /// Known spells are explicitly selected over time (e.g. Sorcerer).
    Learned,
    /// Known spells are all spells on the class list, filtered by max spell level (e.g. Cleric/Paladin).
    EntireClassList,
}

/// Whether a class must "prepare" a subset before casting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum CastingReadinessModel {
    /// Must prepare a subset before casting.
    Prepared,
    /// Can cast from known spells directly.
    Known,
}

/// Defines how a class gains spellcasting abilities.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SpellcastingProgression {
    /// Full spellcasting progression, e.g. Wizard.
    Full,
    /// Half spellcasting progression, e.g. Cleric.
    Half,
    /// Third spellcasting progression, e.g. Bard.
    Third,
    /// No spellcasting progression.
    None,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SpellReplacementModel {
    LevelUp,
    LongRest,
}

/// Rules that define a class’ spellcasting *mechanics*.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SpellcastingRules {
    pub progression: SpellcastingProgression,
    pub spellcasting_ability: Ability,
    pub spellcasting_resource: ResourceId,
    pub access_model: SpellAccessModel,
    pub readiness_model: CastingReadinessModel,
    pub cantrips_per_level: HashMap<u8, usize>,
    pub prepared_spells_per_level: HashMap<u8, usize>,
    pub spell_replacement_model: SpellReplacementModel,
    /// The universe of spells this class can ever touch.
    pub spell_list: HashSet<SpellId>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(from = "ClassDefinition")]
pub struct Class {
    pub id: ClassId,
    pub hit_die: DieSize,
    pub hp_per_level: u8,

    pub default_abilities: AbilityScoreDistribution,

    /// Saving throw proficiencies granted at level 1 (e.g. STR + CON for Fighter)
    pub saving_throw_proficiencies: [Ability; 2],

    pub subclasses: HashSet<SubclassId>,

    /// The levels at which the class can pick a new feat.
    pub feat_levels: HashSet<u8>,

    pub base: ClassBase,
}

impl Class {
    pub fn new(
        id: ClassId,
        hit_die: DieSize,
        hp_per_level: u8,
        default_abilities: AbilityScoreDistribution,
        saving_throw_proficiencies: [Ability; 2],
        subclass_level: u8,
        subclasses: HashSet<SubclassId>,
        feat_levels: HashSet<u8>,
        skill_proficiencies: HashSet<Skill>,
        skill_prompts: u8,
        armor_proficiencies: HashSet<ArmorType>,
        weapon_proficiencies: HashSet<WeaponCategory>,
        spellcasting: Option<SpellcastingRules>,
        effects_by_level: HashMap<u8, Vec<EffectId>>,
        resources_by_level: HashMap<u8, Vec<(ResourceId, ResourceBudgetKind, bool)>>,
        mut prompts_by_level: HashMap<u8, Vec<LevelUpPrompt>>,
        actions_by_level: HashMap<u8, Vec<ActionId>>,
    ) -> Self {
        // Add skill proficiencies
        prompts_by_level
            .entry(1)
            .or_default()
            .push(LevelUpPrompt::SkillProficiency(
                skill_proficiencies.clone(),
                skill_prompts,
                ModifierSource::ClassFeature(id.clone()),
            ));

        // Add subclass prompt
        // NOTE: *DON'T* make a helper method in LevelUpPrompt for subclass prompts.
        // you've done it twice, and every time it creates a lookup in the class
        // registry while it's being initialized, so it just creates an infinite loop.
        prompts_by_level
            .entry(subclass_level)
            .or_default()
            .push(LevelUpPrompt::Choice(ChoiceSpec::single(
                "Subclass",
                subclasses
                    .clone()
                    .into_iter()
                    .map(ChoiceItem::Subclass)
                    .collect(),
            )));

        // TODO: What if the subclass triggers its own prompts?

        Self {
            id,
            hit_die,
            hp_per_level,
            default_abilities,
            saving_throw_proficiencies,
            subclasses,
            feat_levels,
            base: ClassBase {
                skill_proficiencies,
                skill_prompts,
                armor_proficiencies,
                weapon_proficiencies,
                spellcasting,
                effects_by_level,
                resources_by_level,
                prompts_by_level,
                actions_by_level,
            },
        }
    }

    pub fn subclass(&self, subclass_id: &SubclassId) -> Option<&Subclass> {
        if !self.subclasses.contains(subclass_id) {
            return None;
        }
        SubclassesRegistry::get(subclass_id)
    }

    pub fn spellcasting_rules(
        &self,
        subclass_id: &Option<SubclassId>,
    ) -> Option<&SpellcastingRules> {
        // If the subclass has its own spellcasting rules, use those.
        // Otherwise, fall back to the base class rules.
        if let Some(subclass_id) = subclass_id
            && let Some(subclass) = self.subclass(subclass_id)
            && let Some(rules) = &subclass.base.spellcasting
        {
            return Some(rules);
        }
        self.base.spellcasting.as_ref()
    }

    pub fn base(&self) -> &ClassBase {
        &self.base
    }
}

impl IdProvider for Class {
    type Id = ClassId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Subclass {
    pub id: SubclassId,
    pub base: ClassBase,
}

impl Subclass {
    pub fn base(&self) -> &ClassBase {
        &self.base
    }
}

impl IdProvider for Subclass {
    type Id = SubclassId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

// TODO: Not the most elegant solution to just ignore the subclass in equality
// and hashing, but it creates some problems in the spellbook where we use
// ClassAndSubclass as a key but only really care about the class. When leveling
// up and choosing a subclass, it breaks the subsequent lookups (e.g. determining
// how many spells to select in a level up prompt) because the subclass (and thus
// the key) is now different.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ClassAndSubclass {
    pub class: ClassId,
    pub subclass: Option<SubclassId>,
}

impl PartialEq for ClassAndSubclass {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class
    }
}

impl Eq for ClassAndSubclass {}

impl Hash for ClassAndSubclass {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.class.hash(state);
    }
}
