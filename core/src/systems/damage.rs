use crate::{
    components::{
        actions::action::{ActionConditionResolution, DamageFunction},
        damage::{DamageRoll, DamageRollResult},
    },
    engine::{action_prompt::ActionData, game_state::GameState},
    systems,
};

// TODO: Maybe a bit overkill with an entire file for two functions?

pub fn damage_roll(
    mut damage_roll: DamageRoll,
    game_state: &GameState,
    action: &ActionData,
    resolution: &ActionConditionResolution,
) -> DamageRollResult {
    let entity = action.actor.id();

    systems::effects::effects(&game_state.world, entity).pre_damage_roll(
        game_state,
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
