---@type ActionResultHookFn
local function action_result_hook(game_state, action, result)
    if action.action_id == "nat20_core::action.fighter.second_wind" then
        game_state:apply_effect_for_turns(
            action.actor,
            action.actor,
            "nat20_core::effect.fighter.tactical_shift_disengage",
            1,
            false,
            "nat20_core::effect.fighter.tactical_shift",
            action.action_context,
            result:resolution()
        )
    end
end

return {
    action_result_hook = action_result_hook,
}
