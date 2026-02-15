use core::panic;
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use hecs::{Entity, World};
use strum::IntoEnumIterator;
use tracing::error;
use uuid::Uuid;

use crate::{
    components::{
        ability::{Ability, AbilityScore, AbilityScoreDistribution, AbilityScoreMap},
        class::ClassAndSubclass,
        health::hit_points::HitPoints,
        id::{ActionId, ClassId, EffectId, Name, ResourceId, SpellId, SubclassId},
        items::{equipment::loadout::EquipmentInstance, money::MonetaryValue},
        level::CharacterLevels,
        level_up::{ChoiceItem, LevelUpPrompt},
        modifier::{KeyedModifiable, ModifierSource},
        proficiency::{Proficiency, ProficiencyLevel},
        resource::{ResourceBudgetKind, ResourceMap},
        skill::{Skill, SkillSet},
        spells::spellbook::{SpellSource, Spellbook},
    },
    engine::game_state::{self, GameState},
    registry::registry::{ClassesRegistry, ItemsRegistry},
    systems,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LevelUpDecision {
    Choice {
        id: String,
        selected: Vec<ChoiceItem>,
    },
    AbilityScores(AbilityScoreDistribution),
    AbilityScoreImprovement(HashMap<Ability, u8>),
    SkillProficiency(HashSet<Skill>),
    ReplaceSpells {
        // Old spell, new spell
        spells: Vec<(SpellId, SpellId)>,
    },
}

impl LevelUpDecision {
    pub fn matches(&self, prompt: &LevelUpPrompt) -> bool {
        match (self, prompt) {
            (LevelUpDecision::Choice { id, .. }, LevelUpPrompt::Choice(spec)) => id == &spec.id,
            (LevelUpDecision::AbilityScores(_), LevelUpPrompt::AbilityScores(_, _)) => true,
            (
                LevelUpDecision::AbilityScoreImprovement(_),
                LevelUpPrompt::AbilityScoreImprovement { .. },
            ) => true,
            (LevelUpDecision::SkillProficiency(_), LevelUpPrompt::SkillProficiency(_, _, _)) => {
                true
            }
            (LevelUpDecision::ReplaceSpells { .. }, LevelUpPrompt::ReplaceSpells { .. }) => true,
            _ => false,
        }
    }

    pub fn from_choice(id: impl Into<String>, selected: Vec<ChoiceItem>) -> Self {
        LevelUpDecision::Choice {
            id: id.into(),
            selected,
        }
    }

    pub fn single_choice_with_id(id: impl Into<String>, selected: ChoiceItem) -> Self {
        LevelUpDecision::from_choice(id, vec![selected])
    }

    pub fn single_choice(selected: ChoiceItem) -> Self {
        LevelUpDecision::single_choice_with_id(selected.id(), selected)
    }

    pub fn spells(
        id: &str,
        class: &ClassId,
        subclass: &Option<SubclassId>,
        selected: Vec<SpellId>,
    ) -> Self {
        let source = SpellSource::Class(ClassAndSubclass {
            class: class.clone(),
            subclass: subclass.clone(),
        });
        LevelUpDecision::from_choice(
            id.to_string(),
            selected
                .into_iter()
                .map(|spell_id| ChoiceItem::Spell(spell_id, source.clone()))
                .collect(),
        )
    }
}

#[derive(Debug, Clone)]
pub enum LevelUpError {
    InvalidDecision {
        prompt: LevelUpPrompt,
        decision: LevelUpDecision,
        message: Option<String>,
    },
    PrompDecisionMismatch {
        prompt: LevelUpPrompt,
        decision: LevelUpDecision,
    },
    MissingChoiceForDecision {
        decision: LevelUpDecision,
    },
    RegistryMissing(String),
    // TODO: Add more error variants as needed
}

pub struct LevelUpSession {
    character: Entity,
    pending_prompts: Vec<LevelUpPrompt>,
    decisions: Vec<LevelUpDecision>,
}

impl LevelUpSession {
    pub fn new(world: &World, character: Entity) -> Self {
        let mut pending_prompts = Vec::new();

        let levels = systems::helpers::get_component::<CharacterLevels>(world, character);
        if levels.total_level() == 0 {
            [LevelUpPrompt::species(), LevelUpPrompt::background()]
                .iter()
                .for_each(|prompt| {
                    pending_prompts.push(prompt.clone());
                });
        }

        pending_prompts.push(LevelUpPrompt::class());

        LevelUpSession {
            character,
            pending_prompts,
            decisions: Vec::new(),
        }
    }

    pub fn pending_prompts(&self) -> &Vec<LevelUpPrompt> {
        &self.pending_prompts
    }

    pub fn decisions(&self) -> &Vec<LevelUpDecision> {
        &self.decisions
    }

    pub fn is_complete(&self) -> bool {
        self.pending_prompts.is_empty()
    }

    pub fn advance(
        &mut self,
        game_state: &mut GameState,
        decision: &LevelUpDecision,
    ) -> Result<(), LevelUpError> {
        let mut new_prompts = Vec::new();

        let mut resolved_prompt = None;

        for prompt in self.pending_prompts.iter() {
            if !decision.matches(prompt) {
                continue;
            }

            let next_prompts = resolve_level_up_prompt(
                game_state,
                self.character,
                prompt.clone(),
                decision.clone(),
            )?;
            new_prompts.extend(next_prompts);
            resolved_prompt = Some(prompt.clone());
            break;
        }

        if resolved_prompt.is_none() {
            return Err(LevelUpError::MissingChoiceForDecision {
                decision: decision.clone(),
            });
        }

        self.pending_prompts
            .retain(|c| c != resolved_prompt.as_ref().unwrap());

        self.decisions.push(decision.clone());

        self.pending_prompts.extend(new_prompts);

        self.pending_prompts.sort_by_key(|p| p.priority());

        Ok(())
    }

    pub fn chosen_class(&self) -> Option<ClassId> {
        self.decisions.iter().find_map(|d| match d {
            LevelUpDecision::Choice { selected, .. } => {
                selected.iter().find_map(|item| match item {
                    ChoiceItem::Class(class_id) => Some(class_id.clone()),
                    _ => None,
                })
            }
            _ => None,
        })
    }
}

fn resolve_level_up_prompt(
    game_state: &mut GameState,
    entity: Entity,
    prompt: LevelUpPrompt,
    decision: LevelUpDecision,
) -> Result<Vec<LevelUpPrompt>, LevelUpError> {
    let mut prompts = Vec::new();

    match (&prompt, &decision) {
        (LevelUpPrompt::Choice(spec), LevelUpDecision::Choice { id, selected }) => {
            if &spec.id != id {
                return Err(LevelUpError::PrompDecisionMismatch { prompt, decision });
            }

            if selected.len() as u8 != spec.picks {
                return Err(LevelUpError::InvalidDecision {
                    prompt: prompt.clone(),
                    decision: decision.clone(),
                    message: Some(format!(
                        "Invalid number of choices selected: expected {}, got {}",
                        spec.picks,
                        selected.len()
                    )),
                });
            }

            let mut seen = HashMap::new();
            for item in selected {
                if !spec.options.contains(item) {
                    return Err(LevelUpError::InvalidDecision {
                        prompt,
                        decision: decision.clone(),
                        message: Some(format!(
                            "Choice item {:?} does not exist in the options",
                            item
                        )),
                    });
                }
                let count = seen.entry(item).or_insert(0);
                *count += 1;
                if !spec.allow_duplicates && *count > 1 {
                    return Err(LevelUpError::InvalidDecision {
                        prompt,
                        decision: decision.clone(),
                        message: Some(format!(
                            "Duplicate choice item {:?} selected but duplicates are not allowed",
                            item
                        )),
                    });
                }
            }

            for item in selected {
                match item {
                    ChoiceItem::Effect(effect_id) => {
                        systems::effects::add_permanent_effect(
                            game_state,
                            entity,
                            effect_id.clone(),
                            // TODO: Determine proper source
                            &ModifierSource::Base,
                            None,
                        );
                    }
                    ChoiceItem::Feat(feat_id) => {
                        let result = systems::feats::add_feat(game_state, entity, feat_id);
                        if let Ok(new_prompts) = result {
                            prompts.extend(new_prompts);
                        } else {
                            return Err(LevelUpError::InvalidDecision {
                                prompt,
                                decision,
                                message: None,
                            });
                        }
                    }
                    ChoiceItem::Action(action_id) => {
                        systems::actions::add_actions(
                            &mut game_state.world,
                            entity,
                            &[action_id.clone()],
                        );
                    }
                    ChoiceItem::Background(background_id) => {
                        prompts.extend(systems::backgrounds::set_background(
                            game_state,
                            entity,
                            background_id,
                        ));
                    }
                    ChoiceItem::Class(class_id) => {
                        // Special prompt when creating a new character
                        if systems::helpers::get_component::<CharacterLevels>(
                            &mut game_state.world,
                            entity,
                        )
                        .total_level()
                            == 0
                        {
                            prompts.push(LevelUpPrompt::ability_scores());
                        }

                        prompts.extend(systems::class::increment_class_level(
                            game_state, entity, class_id,
                        ));
                    }
                    ChoiceItem::Subclass(subclass_id) => {
                        systems::class::set_subclass(game_state, entity, subclass_id);
                    }
                    ChoiceItem::Species(species_id) => {
                        prompts.extend(systems::species::set_species(
                            game_state, entity, species_id,
                        ));
                    }
                    ChoiceItem::Subspecies(subspecies_id) => {
                        systems::species::set_subspecies(game_state, entity, subspecies_id);
                    }
                    ChoiceItem::Equipment { items, money } => {
                        for (count, item_id) in items {
                            // TODO: Not the most elegant solution
                            for _ in 0..*count {
                                let item = ItemsRegistry::get(item_id).unwrap().clone();
                                if item.equipable() {
                                    let equipment: EquipmentInstance = item.clone().into();
                                    if systems::loadout::can_equip(
                                        &game_state.world,
                                        entity,
                                        &equipment,
                                    ) {
                                        let result =
                                            systems::loadout::equip(game_state, entity, equipment);
                                        if let Err(e) = result {
                                            error!("Failed to equip item {}: {:?}", item_id, e);
                                        } else {
                                            // If the item is successfully equipped,
                                            // we don't need to add it to inventory
                                            continue;
                                        }
                                    }
                                }
                                systems::inventory::add_item(&mut game_state.world, entity, item);
                            }
                        }
                        if !money.is_empty() {
                            let money = MonetaryValue::from_str(money).unwrap();
                            systems::inventory::add_money(&mut game_state.world, entity, money);
                        }
                    }
                    ChoiceItem::Spell(spell_id, source) => {
                        if let Ok((spellbook, resources)) =
                            game_state
                                .world
                                .query_one_mut::<(&mut Spellbook, &ResourceMap)>(entity)
                        {
                            match spellbook.add_spell(spell_id, source, &resources) {
                                Ok(_) => {}
                                Err(e) => {
                                    let error_message = format!(
                                        "Failed to add spell {} to spellbook: {:?}",
                                        spell_id, e
                                    );
                                    error!("{}", error_message);
                                    return Err(LevelUpError::InvalidDecision {
                                        prompt,
                                        decision: decision.clone(),
                                        message: Some(error_message),
                                    });
                                }
                            }
                        } else {
                            panic!(
                                "Failed to get spellbook and resources for entity {:?}",
                                entity
                            );
                        }
                    }
                }
            }
        }

        (
            LevelUpPrompt::SkillProficiency(skills, num_prompts, source),
            LevelUpDecision::SkillProficiency(selected_skills),
        ) => {
            if selected_skills.len() != *num_prompts as usize {
                return Err(LevelUpError::InvalidDecision {
                    prompt,
                    decision,
                    message: None,
                });
            }

            for skill in selected_skills {
                if !skills.contains(&skill) {
                    return Err(LevelUpError::InvalidDecision {
                        prompt,
                        decision,
                        message: None,
                    });
                }
                // TODO: Expertise handling
                systems::helpers::get_component_mut::<SkillSet>(&mut game_state.world, entity)
                    .set_proficiency(
                        skill,
                        Proficiency::new(ProficiencyLevel::Proficient, source.clone()),
                    );
            }
        }

        (
            LevelUpPrompt::AbilityScores(score_point_cost, num_points),
            LevelUpDecision::AbilityScores(distribution),
        ) => {
            if distribution.scores.len() != Ability::iter().count() {
                return Err(LevelUpError::InvalidDecision {
                    prompt,
                    decision,
                    message: None,
                });
            }

            if distribution
                .scores
                .values()
                .any(|&score| !score_point_cost.contains_key(&score))
            {
                return Err(LevelUpError::InvalidDecision {
                    prompt,
                    decision,
                    message: None,
                });
            }

            let total_cost = distribution
                .scores
                .iter()
                .map(|(_, score)| {
                    score_point_cost
                        .get(score)
                        .expect("Invalid ability score")
                        .clone()
                })
                .sum::<u8>();
            if total_cost != *num_points {
                return Err(LevelUpError::InvalidDecision {
                    prompt,
                    decision,
                    message: None,
                });
            }

            let mut ability_score_set = systems::helpers::get_component_mut::<AbilityScoreMap>(
                &mut game_state.world,
                entity,
            );
            for (ability, score) in &distribution.scores {
                let mut final_score = *score as i32;
                if *ability == distribution.plus_2_bonus {
                    final_score += 2;
                } else if *ability == distribution.plus_1_bonus {
                    final_score += 1;
                }
                ability_score_set.set(*ability, AbilityScore::new(*ability, final_score));
            }
        }

        (
            LevelUpPrompt::AbilityScoreImprovement {
                feat,
                budget,
                abilities,
                max_score,
            },
            LevelUpDecision::AbilityScoreImprovement(decision_points),
        ) => {
            if decision_points.values().sum::<u8>() != *budget {
                return Err(LevelUpError::InvalidDecision {
                    prompt,
                    decision,
                    message: None,
                });
            }

            let mut ability_score_set = systems::helpers::get_component_mut::<AbilityScoreMap>(
                &mut game_state.world,
                entity,
            );

            for (ability, bonus) in decision_points {
                if !abilities.contains(ability) {
                    return Err(LevelUpError::InvalidDecision {
                        prompt,
                        decision,
                        message: None,
                    });
                }
                let current_score = ability_score_set.get(ability).total() as u8;
                if current_score + bonus > *max_score {
                    return Err(LevelUpError::InvalidDecision {
                        prompt,
                        decision,
                        message: None,
                    });
                }

                // TODO: Not sure what the best way to apply the points is
                ability_score_set.add_modifier(
                    ability,
                    // Since some feats are repeatable, we can't use the same source
                    // every time, so we'll have to make it unique
                    ModifierSource::FeatRepeatable(feat.clone(), Uuid::new_v4()),
                    *bonus as i32,
                );
            }
        }

        (
            LevelUpPrompt::ReplaceSpells {
                spells,
                source,
                replacements: num_replacements,
            },
            LevelUpDecision::ReplaceSpells {
                spells: spell_replacements,
            },
        ) => {
            if spell_replacements.len() > *num_replacements as usize {
                return Err(LevelUpError::InvalidDecision {
                    prompt: prompt.clone(),
                    decision: decision.clone(),
                    message: Some(format!(
                        "Expected {} spell replacements, but got {}",
                        num_replacements,
                        spell_replacements.len()
                    )),
                });
            }

            for (old_spell, new_spell) in spell_replacements {
                if !spells.contains(old_spell) {
                    return Err(LevelUpError::InvalidDecision {
                        prompt: prompt.clone(),
                        decision: decision.clone(),
                        message: Some(format!(
                            "Unexpected spell to replace: {}. Expected one of: {:#?}",
                            old_spell, spells
                        )),
                    });
                }

                if !spells.contains(new_spell) {
                    return Err(LevelUpError::InvalidDecision {
                        prompt: prompt.clone(),
                        decision: decision.clone(),
                        message: Some(format!(
                            "Unexpected spell to add: {}. Expected one of: {:#?}",
                            old_spell, spells
                        )),
                    });
                }

                if let Ok((spellbook, resources)) = game_state
                    .world
                    .query_one_mut::<(&mut Spellbook, &ResourceMap)>(entity)
                {
                    match spellbook.remove_spell(old_spell, source) {
                        Ok(_) => {}
                        Err(e) => {
                            return Err(LevelUpError::InvalidDecision {
                                prompt: prompt.clone(),
                                decision: decision.clone(),
                                message: Some(format!(
                                    "Failed to remove spell {}: {:?}",
                                    old_spell, e
                                )),
                            });
                        }
                    }
                    match spellbook.add_spell(new_spell, source, &resources) {
                        Ok(_) => {}
                        Err(e) => {
                            return Err(LevelUpError::InvalidDecision {
                                prompt: prompt.clone(),
                                decision: decision.clone(),
                                message: Some(format!(
                                    "Failed to add spell {}: {:?}",
                                    new_spell, e
                                )),
                            });
                        }
                    }
                } else {
                    panic!(
                        "Failed to get spellbook and resources for entity {:?}",
                        entity
                    );
                }
            }
        }

        _ => {
            // If the prompt and decision are called the same, and we made it here,
            // it's probably just because it hasn't been implemented yet
            if decision.matches(&prompt) {
                todo!(
                    "Implement prompt: {:?} with decision: {:?}",
                    prompt,
                    decision
                );
            }

            return Err(LevelUpError::PrompDecisionMismatch { prompt, decision });
        }
    }

    Ok(prompts)
}

pub fn apply_level_up_decision(
    game_state: &mut GameState,
    entity: Entity,
    levels: u8,
    decisions: Vec<LevelUpDecision>,
) {
    let mut decisions = decisions;

    for level in 1..=levels {
        let name = systems::helpers::get_component_clone::<Name>(&game_state.world, entity);
        let mut level_up_session = LevelUpSession::new(&game_state.world, entity);

        // Some of the responses are identical, e.g. selecting the same class
        // multiple times. Using retain would therefore remove all of them,
        // so we need to track the indices of the used responses.
        let mut used_indices = Vec::new();
        for (i, decision) in decisions.iter().enumerate() {
            let result = level_up_session.advance(game_state, decision);
            match result {
                Ok(_) | Err(LevelUpError::MissingChoiceForDecision { .. }) => {
                    // This is expected to happen since the responses cover all
                    // levels, but the session only advances one level at a time.
                    used_indices.push(i);
                    if level_up_session.is_complete() {
                        break;
                    }
                }
                _ => {
                    panic!(
                        "Failed to apply level up response for {} at level {}: {:#?}",
                        name.as_str(),
                        level,
                        result
                    );
                }
            }
        }

        // Remove the used responses from the list
        for index in used_indices.iter().rev() {
            decisions.remove(*index);
        }

        if !level_up_session.is_complete() {
            panic!(
                "Level up session for {} at level {} did not complete. Pending prompts: {:#?}",
                name.as_str(),
                level,
                level_up_session.pending_prompts()
            );
        }
    }
}

pub struct LevelUpGains {
    pub hit_points: HitPoints,
    pub actions: Vec<ActionId>,
    pub effects: Vec<EffectId>,
    pub resources: Vec<(ResourceId, ResourceBudgetKind, bool)>,
}

pub fn level_up_gains(
    world: &World,
    entity: Entity,
    class_id: &ClassId,
    level: u8,
) -> LevelUpGains {
    let class = ClassesRegistry::get(class_id).expect("Class should exist in the registry");

    let hit_points = systems::helpers::get_component_clone::<HitPoints>(world, entity);
    let mut effects = class
        .base
        .effects_by_level
        .get(&level)
        .cloned()
        .unwrap_or_default();
    let mut resources = class
        .base
        .resources_by_level
        .get(&level)
        .cloned()
        .unwrap_or_default();
    let mut actions = class
        .base
        .actions_by_level
        .get(&level)
        .cloned()
        .unwrap_or_default();

    if let Some(subclass) =
        systems::helpers::get_component::<CharacterLevels>(world, entity).subclass(class_id)
    {
        if let Some(subclass) = class.subclass(&subclass) {
            if let Some(subclass_effects) = subclass.base.effects_by_level.get(&level) {
                effects.extend(subclass_effects.clone());
            }
            if let Some(subclass_resources) = subclass.base.resources_by_level.get(&level) {
                resources.extend(subclass_resources.clone());
            }
            if let Some(subclass_actions) = subclass.base.actions_by_level.get(&level) {
                actions.extend(subclass_actions.clone());
            }
        }
    }

    LevelUpGains {
        hit_points,
        actions,
        effects,
        resources,
    }
}
