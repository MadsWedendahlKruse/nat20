---@type ActionResultHookFn
local function action_result_hook(game_state, action, result)
    if action.action_id == "nat20_core::action.barbarian.rage" then
        game_state:apply_effect_for_turns(
            action.actor,
            action.actor,
            "nat20_core::effect.barbarian.instinctive_pounce_active",
            1,
            false,
            "nat20_core::effect.barbarian.instinctive_pounce",
            nil,
            nil
        )
    end
end

return {
    action_result_hook = action_result_hook,
}
