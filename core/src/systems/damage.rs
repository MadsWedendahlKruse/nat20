use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::{ActionContext, AttackRollFunction, DamageFunction},
        damage::{AttackRoll, AttackRollResult, DamageRoll, DamageRollResult},
    },
    systems,
};

pub fn damage_roll(
    mut damage_roll: DamageRoll,
    world: &World,
    entity: Entity,
    crit: bool,
) -> DamageRollResult {
    for effect in systems::effects::effects(world, entity).values() {
        (effect.effect().pre_damage_roll)(world, entity, &mut damage_roll);
    }

    let mut result = damage_roll.roll(crit);

    for effect in systems::effects::effects(world, entity).values() {
        (effect.effect().post_damage_roll)(world, entity, &mut result);
    }

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
    world: &World,
    attacker: Entity,
    target: Entity,
) -> AttackRollResult {
    for effect in systems::effects::effects(world, attacker).values() {
        (effect.effect().pre_attack_roll)(world, attacker, &mut attack_roll);
    }

    for effect in systems::effects::effects(world, target).values() {
        (effect.effect().on_attacked)(world, target, attacker, &mut attack_roll);
    }

    let mut result = {
        let level =
            systems::helpers::level(world, attacker).expect("Entity must have a level component");
        attack_roll.roll_raw(level.proficiency_bonus())
    };

    for effect in systems::effects::effects(world, attacker).values() {
        (effect.effect().post_attack_roll)(world, attacker, &mut result);
    }

    result
}

pub fn attack_roll_fn(
    attack_roll_fn: &AttackRollFunction,
    world: &World,
    entity: Entity,
    target: Entity,
    context: &ActionContext,
) -> AttackRollResult {
    let roll = attack_roll_fn(world, entity, target, context);
    attack_roll(roll, world, entity, target)
}
