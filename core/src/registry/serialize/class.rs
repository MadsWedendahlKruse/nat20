use std::collections::{HashMap, HashSet};

use schemars::JsonSchema;
use serde::Deserialize;

use crate::{
    components::{
        ability::{Ability, AbilityScoreDistribution},
        class::{Class, SpellcastingRules},
        dice::DieSize,
        id::{ActionId, ClassId, EffectId, ResourceId, SubclassId},
        items::equipment::{armor::ArmorType, weapon::WeaponCategory},
        level_up::LevelUpPrompt,
        modifier::ModifierSource,
        resource::{ResourceMergeOperation, ResourceMergePolicy},
        skill::Skill,
    },
    registry::serialize::resource::ResourceBudgetKindDefinition,
};

#[derive(Clone, Deserialize, JsonSchema)]
pub struct ClassDefinition {
    pub id: ClassId,
    pub hit_die: DieSize,
    pub hp_per_level: u8,
    pub default_abilities: AbilityScoreDistribution,
    pub saving_throw_proficiencies: [Ability; 2],
    pub subclass_level: u8,
    pub subclasses: HashSet<SubclassId>,
    pub feat_levels: HashSet<u8>,
    pub skill_proficiencies: HashSet<Skill>,
    pub skill_prompts: u8,
    pub armor_proficiencies: HashSet<ArmorType>,
    pub weapon_proficiencies: HashSet<WeaponCategory>,
    #[serde(default)]
    pub spellcasting: Option<SpellcastingRules>,
    pub effects_by_level: HashMap<u8, Vec<EffectId>>,
    pub resources_by_level: HashMap<u8, Vec<ClassResourceDefinition>>,
    pub prompts_by_level: HashMap<u8, Vec<LevelUpPrompt>>,
    pub actions_by_level: HashMap<u8, Vec<ActionId>>,
}

impl From<ClassDefinition> for Class {
    fn from(def: ClassDefinition) -> Self {
        let resources_by_level = def
            .resources_by_level
            .into_iter()
            .map(|(level, resources)| {
                (
                    level,
                    resources
                        .into_iter()
                        .map(|res_def| {
                            (
                                res_def.id,
                                res_def
                                    .budget
                                    .try_into_budget_kind(ModifierSource::Class(def.id.clone()))
                                    .unwrap(),
                                res_def.policy.into(),
                            )
                        })
                        .collect(),
                )
            })
            .collect();

        Class::new(
            def.id,
            def.hit_die,
            def.hp_per_level,
            def.default_abilities,
            def.saving_throw_proficiencies,
            def.subclass_level,
            def.subclasses,
            def.feat_levels,
            def.skill_proficiencies,
            def.skill_prompts,
            def.armor_proficiencies,
            def.weapon_proficiencies,
            def.spellcasting,
            def.effects_by_level,
            resources_by_level,
            def.prompts_by_level,
            def.actions_by_level,
        )
    }
}

#[derive(Clone, Deserialize, JsonSchema)]
pub struct ClassResourceDefinition {
    pub id: ResourceId,
    pub budget: ResourceBudgetKindDefinition,
    #[serde(default = "default_policy")]
    pub policy: ClassResourceMergePolicy,
}

#[derive(Clone, Deserialize, JsonSchema)]
pub struct ClassResourceMergePolicy {
    #[serde(default = "default_current_uses")]
    pub current_uses: ResourceMergeOperation,
    #[serde(default = "default_max_uses")]
    pub max_uses: ResourceMergeOperation,
}

impl Into<ResourceMergePolicy> for ClassResourceMergePolicy {
    fn into(self) -> ResourceMergePolicy {
        ResourceMergePolicy {
            current_uses: self.current_uses,
            max_uses: self.max_uses,
        }
    }
}

fn default_current_uses() -> ResourceMergeOperation {
    ResourceMergeOperation::Skip
}

fn default_max_uses() -> ResourceMergeOperation {
    ResourceMergeOperation::Replace
}

fn default_policy() -> ClassResourceMergePolicy {
    ClassResourceMergePolicy {
        current_uses: default_current_uses(),
        max_uses: default_max_uses(),
    }
}
