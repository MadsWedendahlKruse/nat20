use crate::{
    components::{
        actions::action::{ActionConditionResolution, DamageFunction},
        damage::{DamageRoll, DamageRollResult},
    },
    engine::{action_prompt::ActionData, engine_state::EngineState},
    systems,
};

// TODO: Maybe a bit overkill with an entire file for two functions?

pub fn damage_roll(
    mut damage_roll: DamageRoll,
    engine_state: &EngineState,
    action: &ActionData,
    resolution: &ActionConditionResolution,
) -> DamageRollResult {
    let entity = action.actor.id();

    systems::effects::effects(&engine_state.world, entity).pre_damage_roll(
        engine_state,
        entity,
        &mut damage_roll,
        action,
        resolution,
    );

    let mut result = damage_roll.roll(resolution.is_crit());

    systems::effects::effects(&engine_state.world, entity).post_damage_roll(
        engine_state,
        entity,
        &mut result,
        action,
        resolution,
    );

    result
}

pub fn damage_roll_fn(
    damage_roll_fn: &DamageFunction,
    engine_state: &EngineState,
    action: &ActionData,
    resolution: &ActionConditionResolution,
) -> DamageRollResult {
    let roll = damage_roll_fn(&engine_state.world, action.actor.id(), &action.context);
    damage_roll(roll, engine_state, action, resolution)
}
