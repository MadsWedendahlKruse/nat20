---@type ActionResultHookFn
local function action_result_hook(game_state, action, result)
    local resolution = result:resolution()
    if resolution and resolution:is_attack_roll() and not resolution:is_success() then
        game_state:apply_effect_for_turns(
            action.actor,
            result.target,
            "nat20_core::effect.fighter.studied_attacks_advantage",
            1,
            true,
            "nat20_core::effect.fighter.studied_attacks",
            action.action_context,
            resolution
        )
    end
end

return {
    action_result_hook = action_result_hook,
}
