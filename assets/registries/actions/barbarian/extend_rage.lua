---@type ActionUsabilityFn
local function action_usability(game_state, entity, context)
    local remaining_duration = game_state:effect_remaining_duration(entity, "nat20_core::effect.barbarian.rage")
    if remaining_duration and remaining_duration.turns > 1 then
        return "Cannot extend Rage further than next turn"
    end

    return nil
end

return {
    action_usability = action_usability,
}
