---@type ActionResultHookFn
local function action_result_hook(game_state, action, result)
    local resolution = result:resolution()
    if resolution and resolution:is_attack_roll() and resolution:is_crit() then
        game_state:apply_effect_for_turns(
            action.actor,
            action.actor,
            "nat20_core::effect.fighter.champion.remarkable_athlete_disengage",
            1,
            true,
            "nat20_core::effect.fighter.champion.remarkable_athlete",
            action.action_context,
            resolution
        )
        return
    end
end

return {
    action_result_hook = action_result_hook,
}
