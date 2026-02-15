use std::{cmp::max, collections::HashMap, sync::LazyLock};

use hecs::{Entity, World};
use tracing::debug;

use crate::{
    components::{
        class::{
            ClassAndSubclass, SpellAccessModel, SpellReplacementModel, SpellcastingProgression,
        },
        id::ResourceId,
        level::CharacterLevels,
        level_up::LevelUpPrompt,
        resource::{ResourceAmount, ResourceBudgetKind, ResourceMap},
        spells::{
            spell::ConcentrationInstance,
            spellbook::{ClassSpellcastingState, Spellbook},
        },
    },
    engine::{
        action_prompt::ActionExecutionInstanceId,
        event::{Event, EventKind},
        game_state::GameState,
    },
    registry::registry::ClassesRegistry,
    systems,
};

pub fn spellcaster_levels(world: &World, entity: Entity) -> u8 {
    let mut spellcaster_levels = 0.0;
    if let Ok(class_levels) = world.get::<&CharacterLevels>(entity) {
        for (class_id, level_progression) in class_levels.all_classes() {
            if let Some(class) = ClassesRegistry::get(&class_id)
                && let Some(spellcasting_rules) =
                    class.spellcasting_rules(&level_progression.subclass().cloned())
            {
                let level = level_progression.level() as f32;

                spellcaster_levels += match spellcasting_rules.progression {
                    SpellcastingProgression::Full => level,
                    SpellcastingProgression::Half => level / 2.0,
                    SpellcastingProgression::Third => level / 3.0,
                    SpellcastingProgression::None => 0.0,
                };
            }
        }
    }
    spellcaster_levels as u8
}

pub static MAX_SPELL_LEVEL: u8 = 9;

static SPELL_SLOTS_PER_LEVEL: LazyLock<HashMap<u8, Vec<u8>>> = LazyLock::new(|| {
    HashMap::from([
        (0, vec![]),
        (1, vec![2]),
        (2, vec![3]),
        (3, vec![4, 2]),
        (4, vec![4, 3]),
        (5, vec![4, 3, 2]),
        (6, vec![4, 3, 3]),
        (7, vec![4, 3, 3, 1]),
        (8, vec![4, 3, 3, 2]),
        (9, vec![4, 3, 3, 3, 1]),
        (10, vec![4, 3, 3, 3, 2]),
        (11, vec![4, 3, 3, 3, 2, 1]),
        (12, vec![4, 3, 3, 3, 2, 1]),
        (13, vec![4, 3, 3, 3, 2, 1, 1]),
        (14, vec![4, 3, 3, 3, 2, 1, 1]),
        (15, vec![4, 3, 3, 3, 2, 1, 1, 1]),
        (16, vec![4, 3, 3, 3, 2, 1, 1, 1]),
        (17, vec![4, 3, 3, 3, 2, 1, 1, 1, 1]),
        (18, vec![4, 3, 3, 3, 2, 1, 1, 1, 1]),
        (19, vec![4, 3, 3, 3, 3, 2, 1, 1, 1]),
        (20, vec![4, 3, 3, 3, 3, 2, 2, 1, 1]),
    ])
});

pub fn update_spellbook(
    world: &mut World,
    entity: Entity,
    class_and_subclass: ClassAndSubclass,
    level: u8,
) -> Vec<LevelUpPrompt> {
    let mut prompts = Vec::new();

    let spellcaster_levels = spellcaster_levels(world, entity);
    let slots_per_level = SPELL_SLOTS_PER_LEVEL.get(&spellcaster_levels).unwrap();

    {
        let mut resources = systems::helpers::get_component_mut::<ResourceMap>(world, entity);
        for (level, &num_slots) in slots_per_level.iter().enumerate() {
            let spellslot_level = level as u8 + 1;
            resources.add(
                ResourceId::new("nat20_core", "resource.spell_slot"),
                ResourceBudgetKind::from(ResourceAmount::Tiered {
                    tier: spellslot_level,
                    amount: num_slots,
                }),
                false,
            );
        }
    }

    {
        let (new_cantrips, new_spells, replacement_model, spellcasting_resource) =
            if let Some(spellcasting_rules) = ClassesRegistry::get(&class_and_subclass.class)
                .unwrap()
                .spellcasting_rules(&class_and_subclass.subclass)
            {
                let mut spellbook = systems::helpers::get_component_mut::<Spellbook>(world, entity);

                let max_cantrips = spellcasting_rules.cantrips_per_level.get(&level).unwrap();
                let max_prepared_spells = spellcasting_rules
                    .prepared_spells_per_level
                    .get(&level)
                    .unwrap();
                let max_learned_spells =
                    if spellcasting_rules.access_model == SpellAccessModel::Learned {
                        max_prepared_spells
                    } else {
                        &0
                    };

                let (new_cantrips, new_spells) = if spellbook
                    .class_state_mut(&class_and_subclass)
                    .is_none()
                {
                    spellbook.insert_class_state(
                        class_and_subclass.clone(),
                        ClassSpellcastingState::new(
                            *max_cantrips,
                            *max_learned_spells,
                            *max_prepared_spells,
                        ),
                    );

                    (
                        *max_cantrips,
                        max(*max_prepared_spells, *max_learned_spells),
                    )
                } else {
                    let class_state = spellbook.class_state_mut(&class_and_subclass).unwrap();
                    let new_cantrips = *max_cantrips - class_state.selections.cantrips.max_size();
                    let new_spells = match spellcasting_rules.access_model {
                        SpellAccessModel::EntireClassList => {
                            *max_prepared_spells - class_state.selections.prepared_spells.max_size()
                        }
                        SpellAccessModel::Learned => {
                            *max_learned_spells - class_state.selections.learned_spells.max_size()
                        }
                    };
                    spellbook
                        .class_state_mut(&class_and_subclass)
                        .unwrap()
                        .set_caps(*max_cantrips, *max_learned_spells, *max_prepared_spells);

                    (new_cantrips, new_spells)
                };

                (
                    new_cantrips,
                    new_spells,
                    Some(spellcasting_rules.spell_replacement_model.clone()),
                    Some(spellcasting_rules.spellcasting_resource.clone()),
                )
            } else {
                (0, 0, None, None)
            };

        let max_spell_level = if let Some(spellcasting_resource) = spellcasting_resource {
            Spellbook::max_spell_level(
                &spellcasting_resource,
                &systems::helpers::get_component::<ResourceMap>(world, entity),
            )
        } else {
            0
        };

        if new_cantrips > 0 {
            prompts.push(LevelUpPrompt::spells(
                world,
                entity,
                &class_and_subclass,
                true,
                new_cantrips as u8,
                max_spell_level,
            ));
        }
        if new_spells > 0 {
            prompts.push(LevelUpPrompt::spells(
                world,
                entity,
                &class_and_subclass,
                false,
                new_spells as u8,
                max_spell_level,
            ));
        }
        if let Some(replacement_model) = replacement_model
            && matches!(replacement_model, SpellReplacementModel::LevelUp)
        {
            prompts.push(LevelUpPrompt::spell_replacement(
                world,
                entity,
                &class_and_subclass,
                1,
            ));
        }
        prompts
    }
}

pub fn add_concentration_instance(
    game_state: &mut GameState,
    caster: Entity,
    instance: ConcentrationInstance,
    action_instance: &ActionExecutionInstanceId,
) {
    debug!(
        "Adding concentration instance for entity {:?}: {:?} ({:?})",
        caster, instance, action_instance
    );

    let current_action = {
        let mut spellbook =
            systems::helpers::get_component_mut::<Spellbook>(&mut game_state.world, caster);
        let tracker = spellbook.concentration_tracker_mut();
        tracker.action_instance().cloned()
    };

    if let Some(existing_action_instance) = current_action
        && existing_action_instance != *action_instance
    {
        break_concentration(game_state, caster);
    }

    {
        let mut spellbook =
            systems::helpers::get_component_mut::<Spellbook>(&mut game_state.world, caster);
        spellbook
            .concentration_tracker_mut()
            .add_instance(instance, action_instance);
    }
}

pub fn break_concentration(game_state: &mut GameState, target: Entity) {
    debug!("Breaking concentration for entity {:?}", target);

    let instances_to_break: Vec<ConcentrationInstance> = {
        let mut spellbook =
            systems::helpers::get_component_mut::<Spellbook>(&mut game_state.world, target);
        spellbook.concentration_tracker_mut().take_instances()
    };

    if instances_to_break.is_empty() {
        return;
    }

    game_state.process_event(Event::new(EventKind::LostConcentration {
        entity: target,
        instances: instances_to_break.clone(),
    }));

    for instance in &instances_to_break {
        instance.break_concentration(game_state);
    }
}
