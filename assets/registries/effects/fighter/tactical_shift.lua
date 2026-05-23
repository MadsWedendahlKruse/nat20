---@type ActionResultHookFn
local function action_result_hook(game_state, entity, action_performed_view)
    if action_performed_view.action.action_id == "nat20_core::action.fighter.second_wind" then
        local actor = action_performed_view.action.actor
        game_state:apply_effect_for_turns(
            actor,
            actor,
            "nat20_core::effect.fighter.tactical_shift_disengage",
            1,
            false,
            action_performed_view
        )
    end
end

return {
    action_result_hook = action_result_hook,
}
