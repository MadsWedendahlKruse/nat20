use std::{collections::HashSet, fmt};

use hecs::World;
use strum::IntoEnumIterator;
use tracing::{debug, error, info};

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        actions::action::ActionMap,
        d20::{AdvantageAware, D20Check},
        damage::DamageResistances,
        effects::effect_manager::EffectManager,
        faction::FactionSet,
        health::hit_points::HitPoints,
        id::FeatId,
        items::{
            equipment::{
                armor::{ArmorTrainingMap, ArmorType},
                loadout::Loadout,
                weapon::{WeaponCategory, WeaponKind, WeaponProficiencyMap},
            },
            inventory::Inventory,
        },
        modifier::{KeyedFlatModifiable, Modifiable, ModifierSource},
        resource::ResourceMap,
        saving_throw::{SavingThrowKind, SavingThrowSet},
        skill::{Skill, SkillSet},
        speed::Speed,
        spells::spellbook::{GrantedSpellSource, SpellSource, Spellbook},
    },
    persistence::engine::{SaveFile, SaveFileSource},
    registry::{
        registry::{REGISTRIES, RegistryError},
        registry_validation::RegistryReference,
    },
};

pub enum RegistryLoadBlocker {
    /// The save file cannot be loaded due to missing fatal registry references.
    /// If this save file was loaded, it would likely result in runtime errors
    /// or undefined behavior.
    MissingFatalReferences { references: Vec<RegistryReference> },
    /// The save file contains missing registry references, but can still be loaded
    /// since the missing references are considered droppable, i.e. they can be
    /// removed from the save file, and it will still function correctly.
    MissingDroppableReferences {
        /// To avoid parsing the save file again, the original save file is stored
        /// here so it can be reused if needed.
        /// It's stored as an `Option` to allow `.take()` to take ownership of the
        /// save file when it is actually loaded. TODO: Currently the "taking part"
        /// is only used by the GUI, so maybe this could be refactored in the future.
        save_file: Option<SaveFile>,
        references: Vec<RegistryReference>,
    },
}

impl TryInto<SaveFileSource> for RegistryLoadBlocker {
    type Error = ();

    fn try_into(self) -> Result<SaveFileSource, Self::Error> {
        match self {
            RegistryLoadBlocker::MissingFatalReferences { .. } => Err(()),
            RegistryLoadBlocker::MissingDroppableReferences { save_file, .. } => {
                if let Some(save_file) = save_file {
                    Ok(SaveFileSource::Struct(save_file))
                } else {
                    Err(())
                }
            }
        }
    }
}

impl fmt::Debug for RegistryLoadBlocker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegistryLoadBlocker::MissingFatalReferences { references } => f
                .debug_struct("MissingFatalReferences")
                .field("references", references)
                .finish(),
            RegistryLoadBlocker::MissingDroppableReferences {
                save_file,
                references,
            } => f
                .debug_struct("MissingDroppableReferences")
                .field("save_file", &save_file.as_ref().map(|sf| &sf.path))
                .field("references", references)
                .finish(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LoadPolicy {
    Strict,
    DropDroppable,
}

pub fn validate_registry_references(
    mut save_file: SaveFile,
    policy: LoadPolicy,
) -> Result<SaveFile, RegistryLoadBlocker> {
    let mut errors = Vec::new();
    REGISTRIES.validate_registry_references(&mut errors, &save_file.references, &save_file.path);

    if errors.is_empty() {
        return Ok(save_file);
    }

    let missing_references = errors
        .iter()
        .map(|error| match error {
            RegistryError::MissingRegistryEntry { reference, .. } => reference.clone(),
            _ => panic!("Unexpected registry error: {:?}", error),
        })
        .collect::<Vec<_>>();

    error!(
        "Registry validation errors: {}",
        RegistryError::Many(errors)
    );

    let reference_policies = missing_references
        .iter()
        .map(|reference| (reference, missing_policy(reference)))
        .collect::<Vec<_>>();

    let (fatal, droppable): (Vec<_>, Vec<_>) = reference_policies
        .into_iter()
        .partition(|(_, policy)| matches!(policy, MissingPolicy::Fatal));

    if !fatal.is_empty() {
        return Err(RegistryLoadBlocker::MissingFatalReferences {
            references: fatal
                .into_iter()
                .map(|(reference, _)| reference.clone())
                .collect(),
        });
    }

    match policy {
        LoadPolicy::Strict => {
            if !droppable.is_empty() {
                return Err(RegistryLoadBlocker::MissingDroppableReferences {
                    save_file: Some(save_file),
                    references: droppable
                        .into_iter()
                        .map(|(reference, _)| reference.clone())
                        .collect(),
                });
            }
        }

        LoadPolicy::DropDroppable => {
            for (reference, policy) in droppable {
                if let MissingPolicy::Droppable(drop_fn) = policy {
                    info!("Dropping missing reference: {}", reference);
                    drop_fn(&mut save_file);

                    if let Ok(modifier) = reference.clone().try_into() {
                        remove_modifier(&mut save_file.world, &modifier);
                    }
                } else {
                    panic!("Unexpected missing policy: {}", reference);
                }
            }
        }
    }

    Ok(save_file)
}

enum MissingPolicy {
    Fatal,
    Droppable(Box<dyn Fn(&mut SaveFile)>),
}

fn missing_policy(reference: &RegistryReference) -> MissingPolicy {
    match reference {
        RegistryReference::Background(_)
        | RegistryReference::Class(_)
        | RegistryReference::Species(_)
        | RegistryReference::Subclass(_)
        | RegistryReference::Subspecies(_) => MissingPolicy::Fatal,

        RegistryReference::Action(action_id) => MissingPolicy::Droppable(Box::new({
            let action_id = action_id.clone();
            move |save_file| {
                for (_entity, action_manager) in save_file.world.query_mut::<&mut ActionMap>() {
                    action_manager.remove(&action_id);
                }
            }
        })),

        RegistryReference::ActionVariant(action_variant_id) => {
            panic!(
                "Action variant should not be stored in a save file: {}",
                action_variant_id
            );
        }

        RegistryReference::Effect(effect_id) => MissingPolicy::Droppable(Box::new({
            let effect_id = effect_id.clone();
            move |save_file| {
                for (_entity, effect_manager) in save_file.world.query_mut::<&mut EffectManager>() {
                    let mut all_instances_to_remove = HashSet::new();

                    for (id, _) in effect_manager
                        .effects
                        .iter()
                        .filter(|(_, instance)| instance.effect_id == effect_id)
                    {
                        all_instances_to_remove.extend(effect_manager.relatives(id));
                    }

                    if all_instances_to_remove.is_empty() {
                        continue;
                    }

                    debug!(
                        "Dropping instances for effect_id {} from {:?}: {:?}",
                        effect_id, _entity, all_instances_to_remove
                    );

                    effect_manager
                        .effects
                        .retain(|id, _| !all_instances_to_remove.contains(id));
                }
            }
        })),

        RegistryReference::Faction(faction_id) => MissingPolicy::Droppable(Box::new({
            let faction_id = faction_id.clone();
            move |save_file| {
                for (_entity, factions) in save_file.world.query_mut::<&mut FactionSet>() {
                    factions.remove(&faction_id);
                }
            }
        })),

        RegistryReference::Feat(feat_id) => MissingPolicy::Droppable(Box::new({
            let feat_id = feat_id.clone();
            move |save_file| {
                for (_entity, feats) in save_file.world.query_mut::<&mut Vec<FeatId>>() {
                    feats.retain(|id| id != &feat_id);
                }
            }
        })),

        RegistryReference::Item(item_id) => MissingPolicy::Droppable(Box::new({
            let item_id = item_id.clone();
            move |save_file| {
                for (_entity, inventory) in save_file.world.query_mut::<&mut Inventory>() {
                    while let Some(index) = inventory.find_item(&item_id) {
                        inventory.remove_item(index);
                    }
                }
            }
        })),

        RegistryReference::Resource(resource_id) => MissingPolicy::Droppable(Box::new({
            let resource_id = resource_id.clone();
            move |save_file| {
                for (_entity, resource_map) in save_file.world.query_mut::<&mut ResourceMap>() {
                    resource_map.remove(&resource_id);
                }
            }
        })),

        RegistryReference::Script(script_id, _) | RegistryReference::ScriptAnyOf(script_id, _) => {
            panic!(
                "Missing script found in save file: {}. Scripts should not be stored in a save file, so I'm not sure how this happened",
                script_id
            );
        }

        RegistryReference::Spell(spell_id) => MissingPolicy::Droppable(Box::new({
            let spell_id = spell_id.clone();
            move |save_file| {
                for (_entity, spellbook) in save_file.world.query_mut::<&mut Spellbook>() {
                    let classes = spellbook.class_states().keys().cloned().collect::<Vec<_>>();
                    for class in classes {
                        spellbook
                            .remove_spell(&spell_id, &SpellSource::Class(class.clone()))
                            .expect(
                                format!(
                                    "Failed to remove spell {} from class {:?}",
                                    spell_id, class
                                )
                                .as_str(),
                            );
                    }

                    for (_source, spells) in spellbook.granted_spells_mut() {
                        spells.spells.remove(&spell_id);
                    }
                }
            }
        })),
    }
}

fn remove_modifier(world: &mut World, source: &ModifierSource) {
    // TODO: More performant to combine the queries?
    for (_entity, speed) in world.query_mut::<&mut Speed>() {
        speed.remove_flat_modifier(source);
        speed.remove_free_movement_multiplier(source);
        speed.remove_multiplier(source);
    }
    for (_entity, hit_points) in world.query_mut::<&mut HitPoints>() {
        hit_points.clear_temp(source);
    }
    for (_entity, ability_scores) in world.query_mut::<&mut AbilityScoreMap>() {
        for ability in Ability::iter() {
            ability_scores.remove_modifier(&ability, source);
        }
    }
    for (_entity, skills) in world.query_mut::<&mut SkillSet>() {
        for skill in Skill::iter() {
            remove_d20_check_modifiers(skills.get_mut(&skill), source);
        }
    }
    for (_entity, saving_throws) in world.query_mut::<&mut SavingThrowSet>() {
        for saving_throw in SavingThrowKind::iter() {
            remove_d20_check_modifiers(saving_throws.get_mut(&saving_throw), source);
        }
    }
    for (_entity, resistances) in world.query_mut::<&mut DamageResistances>() {
        resistances.effects.retain(|_damage_type, effects| {
            effects.retain(|effect| &effect.source != source);
            !effects.is_empty()
        });
    }
    for (_entity, armor_training) in world.query_mut::<&mut ArmorTrainingMap>() {
        for armor_type in ArmorType::iter() {
            armor_training.remove_proficient(&armor_type, source);
        }
    }
    for (_entity, weapon_proficiency) in world.query_mut::<&mut WeaponProficiencyMap>() {
        for weapon_category in WeaponCategory::iter() {
            weapon_proficiency.remove_proficient(&weapon_category, source);
        }
    }
    for (_entity, loadout) in world.query_mut::<&mut Loadout>() {
        for weapon_kind in WeaponKind::iter() {
            remove_d20_check_modifiers(loadout.attack_roll_template_mut(&weapon_kind), source);
            loadout
                .saving_throw_modifiers_mut(&weapon_kind)
                .remove_modifier(source);
        }
    }
    for (_entity, spellbook) in world.query_mut::<&mut Spellbook>() {
        spellbook
            .saving_throw_modifiers_mut()
            .remove_modifier(source);
        remove_d20_check_modifiers(spellbook.attack_roll_template_mut(), source);

        spellbook
            .granted_spells_mut()
            .retain(|granted_source, _granted_spells| {
                let modifier_source = match granted_source.clone() {
                    GrantedSpellSource::Item(item_id) => ModifierSource::Item(item_id),
                    GrantedSpellSource::Feat(feat_id) => ModifierSource::Feat(feat_id),
                    GrantedSpellSource::Species(species_id) => ModifierSource::Species(species_id),
                    GrantedSpellSource::Effect(effect_id) => ModifierSource::Effect(effect_id),
                };
                source != &modifier_source
            });
    }
    for (_entity, resources) in world.query_mut::<&mut ResourceMap>() {
        for (_resource, budget) in resources.iter_mut() {
            budget.remove_uses(source);
        }
    }
}

fn remove_d20_check_modifiers(d20_check: &mut D20Check, source: &ModifierSource) {
    d20_check.remove_modifier(source);
    d20_check.remove_crit_threshold_reduction(source);
    d20_check.remove_advantage(source);
    if let Some((outcome_source, _forced_outcome)) = d20_check.forced_outcome() {
        if outcome_source == source {
            d20_check.clear_forced_outcome();
        }
    }
}
