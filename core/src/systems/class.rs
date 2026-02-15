use crate::{
    components::{
        class::{ClassAndSubclass, ClassBase},
        id::{ClassId, SubclassId},
        items::equipment::{armor::ArmorTrainingSet, weapon::WeaponProficiencyMap},
        proficiency::ProficiencyLevel,
        resource::ResourceMap,
        saving_throw::SavingThrowKind,
    },
    engine::game_state::GameState,
    registry::registry::ClassesRegistry,
};
use hecs::{Entity, World};

use crate::{
    components::{
        level::CharacterLevels, level_up::LevelUpPrompt, modifier::ModifierSource,
        proficiency::Proficiency, saving_throw::SavingThrowSet,
    },
    systems,
};

enum ClassIdentifier {
    Class(ClassId),
    Subclass(SubclassId),
}

impl ClassIdentifier {
    pub fn modifier_source(&self) -> ModifierSource {
        match self {
            ClassIdentifier::Class(class_id) => ModifierSource::ClassFeature(class_id.clone()),
            ClassIdentifier::Subclass(subclass_id) => {
                ModifierSource::SubclassFeature(subclass_id.clone())
            }
        }
    }
}

pub fn class_level(world: &World, entity: Entity, class_id: &ClassId) -> u8 {
    if let Ok(character_levels) = world.get::<&CharacterLevels>(entity) {
        if let Some(class_level) = character_levels.class_level(class_id) {
            return class_level.level();
        }
    }
    0
}

pub fn increment_class_level(
    game_state: &mut GameState,
    entity: Entity,
    class_id: &ClassId,
) -> Vec<LevelUpPrompt> {
    let class = ClassesRegistry::get(class_id).expect(&format!(
        "Class with name `{}` not found in the registry",
        class_id
    ));

    let (new_level, subclass) = {
        let mut character_levels =
            systems::helpers::get_component_mut::<CharacterLevels>(&mut game_state.world, entity);
        let new_level = character_levels.level_up(class_id.clone());
        let subclass = if let Some(subclass_id) = character_levels.subclass(&class_id) {
            class.subclass(&subclass_id)
        } else {
            None
        };
        (new_level, subclass)
    };

    // TODO: Do we need to do this every time?
    for ability in class.saving_throw_proficiencies.iter() {
        systems::helpers::get_component_mut::<SavingThrowSet>(&mut game_state.world, entity)
            .set_proficiency(
                &SavingThrowKind::Ability(*ability),
                Proficiency::new(
                    ProficiencyLevel::Proficient,
                    ModifierSource::ClassFeature(class_id.clone()),
                ),
            );
    }

    // TODO: If it's a level that triggers a feat prompt, and ability score improvement
    // is selected, then the Constitution modifier might increase, in which case we need to
    // recalculate hit points.
    systems::health::update_hit_points(&mut game_state.world, entity);

    let mut prompts = apply_class_base(
        game_state,
        entity,
        &class.base,
        ClassIdentifier::Class(class_id.clone()),
        new_level,
    );
    if let Some(subclass) = subclass {
        prompts.extend(apply_class_base(
            game_state,
            entity,
            subclass.base(),
            ClassIdentifier::Subclass(subclass.id.clone()),
            new_level,
        ));
    }

    prompts.extend(systems::spells::update_spellbook(
        &mut game_state.world,
        entity,
        ClassAndSubclass {
            class: class_id.clone(),
            subclass: subclass.as_ref().map(|s| s.id.clone()),
        },
        new_level,
    ));

    // Feats need special handling since they can have prerequisites and
    // can (or can't) be repeatable.
    if class.feat_levels.contains(&new_level) {
        prompts.push(LevelUpPrompt::feats(&game_state.world, entity));
    }

    prompts
}

pub fn set_subclass(
    game_state: &mut GameState,
    entity: Entity,
    subclass_id: &SubclassId,
) -> Vec<LevelUpPrompt> {
    let class_name = subclass_id.id.split(".").collect::<Vec<_>>()[1];
    let class_id = &ClassId::new("nat20_core", format!("class.{}", class_name));

    let class = ClassesRegistry::get(class_id).expect(&format!(
        "Class with name `{}` not found in the registry",
        class_id
    ));

    let (subclass, level) = {
        let mut character_levels =
            systems::helpers::get_component_mut::<CharacterLevels>(&mut game_state.world, entity);
        character_levels.set_subclass(class_id, &subclass_id);

        let subclass = class
            .subclass(&subclass_id)
            .expect("Subclass should exist in the class registry");
        let level = character_levels.class_level(class_id).unwrap().level();

        (subclass, level)
    };

    apply_class_base(
        game_state,
        entity,
        subclass.base(),
        ClassIdentifier::Subclass(subclass_id.clone()),
        level,
    )
}

fn apply_class_base(
    game_state: &mut GameState,
    entity: Entity,
    class_base: &ClassBase,
    id: ClassIdentifier,
    level: u8,
) -> Vec<LevelUpPrompt> {
    // Effect
    if let Some(effects_for_level) = class_base.effects_by_level.get(&level) {
        systems::effects::add_permanent_effects(
            game_state,
            entity,
            effects_for_level.clone(),
            &id.modifier_source(),
            None,
        );
    }

    // Resources
    {
        let mut resources =
            systems::helpers::get_component_mut::<ResourceMap>(&mut game_state.world, entity);
        if let Some(resources_for_level) = class_base.resources_by_level.get(&level) {
            for (resource, amount, override_existing) in resources_for_level {
                if *override_existing {
                    resources.remove(resource);
                }
                resources.add(resource.clone(), amount.clone().into(), false);
            }
        }
    }

    // Actions
    {
        if let Some(actions_for_level) = class_base.actions_by_level.get(&level) {
            systems::actions::add_actions(&mut game_state.world, entity, actions_for_level);
        }
    }

    // Weapons proficiencies
    {
        let mut weapon_proficiencies = systems::helpers::get_component_mut::<WeaponProficiencyMap>(
            &mut game_state.world,
            entity,
        );
        for proficiency in class_base.weapon_proficiencies.iter() {
            weapon_proficiencies.set_proficiency(
                proficiency.clone(),
                Proficiency::new(ProficiencyLevel::Proficient, id.modifier_source()),
            );
        }
    }

    // Armor training
    {
        let mut armor_training =
            systems::helpers::get_component_mut::<ArmorTrainingSet>(&mut game_state.world, entity);
        for armor_type in class_base.armor_proficiencies.iter() {
            armor_training.insert(armor_type.clone());
        }
    }

    // Return any additional prompts that should be presented to the player
    let mut new_prompts = class_base
        .prompts_by_level
        .get(&level)
        .cloned()
        .unwrap_or_default();

    new_prompts
}
