use std::{cmp::max, ops::Deref, sync::Arc};

use hecs::{Entity, World};
use tracing::debug;

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        d20::{D20CheckDC, D20CheckOutcome},
        damage::{AttackRollResult, DamageMitigationResult, DamageResistances, DamageRollResult},
        effects::{
            effect::{EffectInstance, EffectInstanceId, EffectLifetime},
            hooks::DeathHook,
        },
        health::{hit_points::HitPoints, life_state::LifeState},
        level::CharacterLevels,
        modifier::{Modifiable, ModifierSet, ModifierSource},
        saving_throw::SavingThrowKind,
        spells::{spell::CONCENTRATION_SAVING_THROW_DC_DEFAULT, spellbook::Spellbook},
    },
    engine::{
        event::{CallbackResult, EventCallback, EventKind},
        game_state::GameState,
    },
    entities::{character::CharacterTag, monster::MonsterTag},
    registry::registry::ClassesRegistry,
    systems::{self, d20::D20CheckDCKind},
};

pub fn heal(world: &mut World, target: Entity, amount: u32) -> Option<LifeState> {
    if let Ok(mut hit_points) = world.get::<&mut HitPoints>(target) {
        let hit_points_before = hit_points.current();
        hit_points.heal(amount);
        if let Ok(mut life_state) = world.get::<&mut LifeState>(target) {
            if hit_points.current() > 0 && hit_points_before == 0 {
                *life_state = LifeState::Normal;
                return Some(LifeState::Normal);
            }
        }
    }
    None
}

pub fn heal_full(world: &mut World, target: Entity) -> Option<LifeState> {
    // TODO: Bit of a convoluted way to get avoid repeating the life state logic
    let hit_point_max = if let Ok(hit_points) = world.get::<&HitPoints>(target) {
        Some(hit_points.max())
    } else {
        None
    };
    if let Some(max) = hit_point_max {
        return heal(world, target, max);
    }
    None
}

pub fn damage(
    game_state: &mut GameState,
    target: Entity,
    damage_roll_result: &DamageRollResult,
    attack_roll: Option<&AttackRollResult>,
) -> (Option<DamageMitigationResult>, Option<LifeState>) {
    let resistances =
        if let Ok(resistances) = game_state.world.get::<&mut DamageResistances>(target) {
            resistances.deref().clone()
        } else {
            DamageResistances::new()
        };

    let mut damage_roll_result = damage_roll_result.clone();
    for effect in systems::effects::effects(&game_state.world, target).values() {
        (effect.effect().pre_damage_mitigation)(
            &game_state.world,
            target,
            effect,
            &mut damage_roll_result,
        );
    }

    let mut mitigation_result = resistances.apply(&damage_roll_result);

    for effect in systems::effects::effects(&game_state.world, target).values() {
        (effect.effect().post_damage_mitigation)(&game_state.world, target, &mut mitigation_result);
    }

    let (damage_taken, killed_by_damage, mut new_life_state, removed_temp_hp_source) =
        if let Ok((hit_points, life_state)) = game_state
            .world
            .query_one_mut::<(&mut HitPoints, &mut LifeState)>(target)
        {
            // Track any changes to the life state of the target
            let mut new_life_state = None;
            // Check if the target is already at 0 HP
            let hp_before_damage = hit_points.current();
            if hit_points.current() == 0 {
                match life_state {
                    LifeState::Stable => {
                        new_life_state = Some(LifeState::unconscious());
                    }

                    LifeState::Unconscious(death_saving_throws) => {
                        if let Some(attack_roll) = attack_roll {
                            if matches!(
                                attack_roll.roll_result.outcome,
                                Some(D20CheckOutcome::CriticalSuccess)
                            ) {
                                death_saving_throws.record_failure(2);
                            } else {
                                death_saving_throws.record_failure(1);
                            }
                        } else {
                            death_saving_throws.record_failure(1);
                        }

                        let next_state = death_saving_throws.next_state();
                        if !matches!(next_state, LifeState::Unconscious(_)) {
                            // If the next state is not still unconscious, we need to update it
                            new_life_state = Some(next_state);
                        }
                    }

                    _ => {
                        // Other valid states where HP would be zero are some form of
                        // dead, so no-op
                        // TODO: Validate that this is the case?
                    }
                }
            }

            let damage_taken = mitigation_result.total.max(0) as u32;

            let removed_temp_hp = hit_points.damage(damage_taken);
            debug!(
                "Entity {:?} took {} damage (HP: {} -> {})",
                target,
                damage_taken,
                hp_before_damage,
                hit_points.current()
            );

            (
                damage_taken,
                hp_before_damage > 0 && hit_points.current() == 0,
                new_life_state,
                removed_temp_hp,
            )
        } else {
            return (None, None);
        };

    if killed_by_damage {
        // Monsters and Characters 'die' differently
        if let Ok(_) = game_state.world.get::<&MonsterTag>(target) {
            new_life_state = Some(LifeState::Dead);
        }

        if let Ok(_) = game_state.world.get::<&CharacterTag>(target) {
            new_life_state = Some(LifeState::unconscious());
        }

        // Trigger death hooks and remove effects that are not permanent
        let hooks = systems::effects::effects(&game_state.world, target)
            .iter()
            .map(|(id, effect)| (id.clone(), effect.clone(), effect.effect().on_death.clone()))
            .collect::<Vec<(EffectInstanceId, EffectInstance, DeathHook)>>();

        let killer = damage_roll_result
            .action
            .as_ref()
            .and_then(|(actor, _)| Some(actor));

        for (id, effect, hook) in hooks {
            hook(
                &mut game_state.world,
                target,
                killer.cloned(),
                effect.applier,
            );

            if !matches!(effect.lifetime, EffectLifetime::Permanent) {
                if let Some(applier) = effect.applier
                    && applier != target
                {
                    // Check if the applier is concentrating on the target and break concentration if so
                    let mut spellbook = systems::helpers::get_component_mut::<Spellbook>(
                        &mut game_state.world,
                        applier,
                    );
                    spellbook
                        .concentration_tracker_mut()
                        .remove_instances_by_entity(target);
                }

                systems::effects::remove_effect(&mut game_state.world, target, &id);
            }
        }
    }

    if let Some(new_life_state) = new_life_state {
        if let Ok(mut life_state) = game_state.world.get::<&mut LifeState>(target) {
            *life_state = new_life_state;
        }
    }

    if let Some(source) = &removed_temp_hp_source {
        debug!(
            "Entity {:?} lost temporary hit points from source {:?}",
            target, source
        );
        match source {
            ModifierSource::Effect(effect_id) => {
                // TODO: Source should be EffectInstanceId?
                systems::effects::remove_effects_by_id(&mut game_state.world, target, effect_id);
            }
            _ => { /* Other sources don't need to be removed? */ }
        }
    }

    let is_concentrating = {
        let spellbook = systems::helpers::get_component::<Spellbook>(&game_state.world, target);
        spellbook.concentration_tracker().is_concentrating()
    };

    if is_concentrating {
        debug!(
            "Entity {:?} is concentrating; checking for concentration break due to damage",
            target
        );

        let dc = max(
            CONCENTRATION_SAVING_THROW_DC_DEFAULT,
            damage_taken as i32 / 2,
        );
        let saving_throw_dc = D20CheckDC {
            key: SavingThrowKind::Concentration,
            dc: ModifierSet::from(ModifierSource::Base, dc),
        };
        let saving_throw_event = systems::d20::check(
            game_state,
            target,
            &D20CheckDCKind::SavingThrow(saving_throw_dc),
        );
        let callback = EventCallback::new({
            move |game_state, event, _| {
                match &event.kind {
                    EventKind::D20CheckResolved(_, check_result, dc) => {
                        if !check_result.is_success(dc) {
                            systems::spells::break_concentration(&mut game_state.world, target);
                        }
                    }
                    _ => {}
                }
                CallbackResult::None
            }
        });
        game_state.process_event_with_callback(saving_throw_event, callback);
    }

    (Some(mitigation_result), new_life_state)
}

pub fn is_alive(world: &World, entity: Entity) -> bool {
    if let Ok(hit_points) = world.get::<&HitPoints>(entity) {
        hit_points.current() > 0
    } else {
        false
    }
}

pub fn update_hit_points(world: &mut World, entity: Entity) {
    if let Ok(mut hit_points) = world.get::<&mut HitPoints>(entity) {
        if let Ok(class_levels) = world.get::<&CharacterLevels>(entity) {
            let mut new_hp = 0;

            let total_level = class_levels.total_level();

            if total_level == 0 {
                // TODO: Not sure if this ever happens
                return;
            }

            // Calculate the hit points based on the class levels and the
            // Constitution modifier. Hit points are calculated on a per-level
            // basis. Hit points are calculated as follows:
            // - For level 1, the hit points are the hit die of the first class
            //   + Constitution modifier
            // - For subsequent levels, the hit points are increased by a fixed
            //   amount based on the class + Constitution modifier.

            let constitution_modifier =
                systems::helpers::get_component::<AbilityScoreMap>(world, entity)
                    .get(&Ability::Constitution)
                    .ability_modifier()
                    .total();

            for (class_id, class_level) in class_levels.all_classes() {
                if let Some(class) = ClassesRegistry::get(class_id) {
                    for level in 1..=class_level.level() {
                        let hp_increase =
                            if class_id == class_levels.first_class().unwrap() && level == 1 {
                                class.hit_die as u32
                            } else {
                                class.hp_per_level as u32
                            };

                        new_hp += (hp_increase + (constitution_modifier as u32)).max(1);
                    }
                }
            }

            hit_points.update_max(new_hp);
        }
    }
}
