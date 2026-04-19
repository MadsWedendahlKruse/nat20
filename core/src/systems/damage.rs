use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::{ActionContext, AttackRollFunction, DamageFunction},
        damage::{AttackRoll, AttackRollResult, DamageRoll, DamageRollResult},
        effects::{effect::EffectInstance, hooks::AttackedHook},
    },
    systems,
};

pub fn damage_roll(
    mut damage_roll: DamageRoll,
    world: &World,
    entity: Entity,
    crit: bool,
) -> DamageRollResult {
    systems::effects::effects(world, entity).pre_damage_roll(world, entity, &mut damage_roll);

    let mut result = damage_roll.roll(crit);

    systems::effects::effects(world, entity).post_damage_roll(world, entity, &mut result);

    result
}

pub fn damage_roll_fn(
    damage_roll_fn: &DamageFunction,
    world: &World,
    entity: Entity,
    context: &ActionContext,
    crit: bool,
) -> DamageRollResult {
    let roll = damage_roll_fn(world, entity, context);
    damage_roll(roll, world, entity, crit)
}

pub fn attack_roll(
    mut attack_roll: AttackRoll,
    world: &mut World,
    attacker: Entity,
    target: Entity,
) -> AttackRollResult {
    systems::effects::effects(world, attacker).pre_attack_roll(world, attacker, &mut attack_roll);

    let attacked_hooks: Vec<(AttackedHook, EffectInstance)> =
        systems::effects::effects_mut(world, target)
            .collect_one_shot_hooks_with_instance(|effect| effect.on_attacked.as_ref());
    for (hook, instance) in &attacked_hooks {
        hook(world, instance, target, attacker, &mut attack_roll);
    }

    let mut result = {
        let level =
            systems::helpers::level(world, attacker).expect("Entity must have a level component");
        attack_roll.roll_raw(level.proficiency_bonus())
    };

    systems::effects::effects(world, attacker).post_attack_roll(world, attacker, &mut result);

    result
}

pub fn attack_roll_fn(
    attack_roll_fn: &AttackRollFunction,
    world: &mut World,
    entity: Entity,
    target: Entity,
    context: &ActionContext,
) -> AttackRollResult {
    let roll = attack_roll_fn(world, entity, target, context);
    attack_roll(roll, world, entity, target)
}
