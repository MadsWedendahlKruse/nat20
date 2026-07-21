use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::{
            ActionConditionResolution, ActionContext, AttackRollFunction, DamageFunction,
        },
        damage::{AttackRoll, AttackRollResult, DamageRoll, DamageRollResult},
        effects::{effect::EffectInstance, hooks::AttackedHook},
    },
    engine::{action_prompt::ActionData, game_state::GameState},
    systems,
};

pub fn damage_roll(
    mut damage_roll: DamageRoll,
    game_state: &GameState,
    action: &ActionData,
    resolution: &ActionConditionResolution,
) -> DamageRollResult {
    let entity = action.actor.id();

    systems::effects::effects(&game_state.world, entity).pre_damage_roll(
        &game_state,
        entity,
        &mut damage_roll,
        action,
        resolution,
    );

    let mut result = damage_roll.roll(resolution.is_crit());

    systems::effects::effects(&game_state.world, entity).post_damage_roll(
        game_state,
        entity,
        &mut result,
        action,
        resolution,
    );

    result
}

pub fn damage_roll_fn(
    damage_roll_fn: &DamageFunction,
    game_state: &GameState,
    action: &ActionData,
    resolution: &ActionConditionResolution,
) -> DamageRollResult {
    let roll = damage_roll_fn(&game_state.world, action.actor.id(), &action.context);
    damage_roll(roll, game_state, action, resolution)
}

pub fn attack_roll(
    mut attack_roll: AttackRoll,
    game_state: &mut GameState,
    attacker: Entity,
    target: Entity,
) -> AttackRollResult {
    let attacked_hooks: Vec<(AttackedHook, EffectInstance)> =
        systems::effects::effects_mut(&mut game_state.world, target)
            .collect_one_shot_hooks_with_instance(|effect| effect.on_attacked.as_ref());
    for (hook, instance) in &attacked_hooks {
        hook(
            &game_state.world,
            instance,
            target,
            attacker,
            &mut attack_roll,
        );
    }

    systems::effects::effects(&game_state.world, attacker).pre_attack_roll(
        game_state,
        attacker,
        &mut attack_roll,
    );

    let mut result = {
        let level = systems::helpers::level(&game_state.world, attacker)
            .expect("Entity must have a level component");
        attack_roll.roll_raw(level.proficiency_bonus())
    };

    systems::effects::effects(&game_state.world, attacker).post_attack_roll(
        &game_state.world,
        attacker,
        &mut result,
    );

    result
}

pub fn attack_roll_fn(
    attack_roll_fn: &AttackRollFunction,
    game_state: &mut GameState,
    entity: Entity,
    target: Entity,
    context: &ActionContext,
) -> AttackRollResult {
    let roll = attack_roll_fn(&game_state.world, entity, target, context);
    attack_roll(roll, game_state, entity, target)
}
