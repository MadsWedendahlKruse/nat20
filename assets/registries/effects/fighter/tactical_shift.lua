---@type ActionResultHookFn
local function action_result_hook(game_state, entity, action, results)
    if action.action_id == "nat20_core::action.fighter.second_wind" then
        local result = results[0] or results[1]
        if not result then
            return
        end

        local standard_kind = result.kind:as_standard()
        if not standard_kind then
            return
        end

        game_state:apply_effect_for_turns(
            action.actor,
            action.actor,
            "nat20_core::effect.fighter.tactical_shift_disengage",
            1,
            false,
            "nat20_core::effect.fighter.tactical_shift",
            action.action_context,
            standard_kind:resolution()
        )
    end
end

return {
    action_result_hook = action_result_hook,
}
