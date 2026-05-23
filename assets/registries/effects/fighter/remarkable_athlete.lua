---@type ActionResultHookFn
local function action_result_hook(game_state, entity, action_performed_view)
    local actor = action_performed_view.action.actor
    for _, result in ipairs(action_performed_view:results()) do
        local kind = result.kind
        if kind:is_standard() and kind:as_standard():has_attack_critical_hit() then
            game_state:apply_effect_for_turns(
                actor,
                actor,
                "nat20_core::effect.fighter.champion.remarkable_athlete_disengage",
                1,
                false,
                action_performed_view
            )
            return
        end
    end
end

return {
    action_result_hook = action_result_hook,
}
