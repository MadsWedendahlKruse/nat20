use std::collections::{BTreeMap, HashMap, HashSet};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        ability::{Ability, AbilityScoreDistribution},
        class::{Class, SpellcastingRules, Subclass},
        dice::DieSize,
        id::{ActionId, ClassId, EffectId, ResourceId, SubclassId},
        items::equipment::{armor::ArmorType, weapon::WeaponCategory},
        level_up::{ChoiceItem, ChoiceSpec, LevelUpPrompt},
        resource::{ResourceBudget, ResourceBudgetKind},
        skill::Skill,
    },
    registry::registry_validation::{
        ReferenceCollector, RegistryReference, RegistryReferenceCollector,
    },
};

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ResourceBudgetKindDefinition {
    Flat(ResourceBudget),
    // Tier keys should be the level (u8), but serde only supports string keys in maps
    // so we need this intermediate representation
    Tiered(BTreeMap<String, ResourceBudget>),
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
pub struct ClassResourceDefinition {
    pub id: ResourceId,
    pub budget: ResourceBudgetKindDefinition,
    #[serde(default, rename(deserialize = "override"))]
    pub override_existing: bool,
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
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
                                match res_def.budget {
                                    ResourceBudgetKindDefinition::Flat(resource_budget) => {
                                        ResourceBudgetKind::Flat(resource_budget)
                                    }
                                    ResourceBudgetKindDefinition::Tiered(btree_map) => {
                                        let mut tiered_map = BTreeMap::new();
                                        for (tier_str, budget) in btree_map {
                                            let tier: u8 = tier_str.parse().unwrap_or(0);
                                            tiered_map.insert(tier, budget);
                                        }
                                        ResourceBudgetKind::Tiered(tiered_map)
                                    }
                                },
                                res_def.override_existing,
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

impl RegistryReferenceCollector for ClassDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for effect_list in self.effects_by_level.values() {
            for effect in effect_list {
                collector.add(RegistryReference::Effect(effect.clone()));
            }
        }
        for action_list in self.actions_by_level.values() {
            for action in action_list {
                collector.add(RegistryReference::Action(action.clone()));
            }
        }
        for prompts in self.prompts_by_level.values() {
            for prompt in prompts {
                prompt.collect_registry_references(collector);
            }
        }
    }
}

impl RegistryReferenceCollector for LevelUpPrompt {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        match self {
            LevelUpPrompt::Choice(choice_spec) => {
                choice_spec.collect_registry_references(collector);
            }
            LevelUpPrompt::AbilityScoreImprovement { feat, .. } => {
                collector.add(RegistryReference::Feat(feat.clone()));
            }
            _ => { /* No references to collect */ }
        }
    }
}

impl RegistryReferenceCollector for ChoiceSpec {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for option in &self.options {
            match option {
                ChoiceItem::Action(action_id) => {
                    collector.add(RegistryReference::Action(action_id.clone()));
                }
                ChoiceItem::Spell(spell_id, _) => {
                    collector.add(RegistryReference::Spell(spell_id.clone()));
                }
                ChoiceItem::Background(background_id) => {
                    collector.add(RegistryReference::Background(background_id.clone()));
                }
                ChoiceItem::Class(class_id) => {
                    collector.add(RegistryReference::Class(class_id.clone()));
                }
                ChoiceItem::Subclass(subclass_id) => {
                    collector.add(RegistryReference::Subclass(subclass_id.clone()));
                }
                ChoiceItem::Effect(effect_id) => {
                    collector.add(RegistryReference::Effect(effect_id.clone()));
                }
                ChoiceItem::Feat(feat_id) => {
                    collector.add(RegistryReference::Feat(feat_id.clone()));
                }
                ChoiceItem::Species(species_id) => {
                    collector.add(RegistryReference::Species(species_id.clone()));
                }
                ChoiceItem::Subspecies(subspecies_id) => {
                    collector.add(RegistryReference::Subspecies(subspecies_id.clone()));
                }
                ChoiceItem::Equipment { items, .. } => {
                    for (_, item_id) in items {
                        collector.add(RegistryReference::Item(item_id.clone()));
                    }
                }
            }
        }
    }
}

impl RegistryReferenceCollector for Subclass {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        for effect_list in self.base.effects_by_level.values() {
            for effect in effect_list {
                collector.add(RegistryReference::Effect(effect.clone()));
            }
        }
        for action_list in self.base.actions_by_level.values() {
            for action in action_list {
                collector.add(RegistryReference::Action(action.clone()));
            }
        }
    }
}
