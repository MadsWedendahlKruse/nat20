---@type ActionHookFn
local function action_hook(game_state, action)
    print("action_hook", action.action_id)
    if string.find(action.action_id, "nat20_core::action.hex") then
        if game_state:has_effect(action.actor, "nat20_core::effect.spell.hex_reapply") then
            game_state:remove_effect(action.actor, "nat20_core::effect.spell.hex_reapply")
        end
    end
end

return {
    action_hook = action_hook,
}
