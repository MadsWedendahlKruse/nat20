---@type TargetUsabilityFn
local function target_usability(game_state, entity, target, action_id, context)
    local check = game_state:preview_attack_roll(entity, target, context, action_id)
    if check.roll_mode == "disadvantage" then
        return "The chosen attack roll mustn't have Disadvantage"
    end

    if not check:get_advantage("nat20_core::effect.barbarian.reckless_attack_advantage") then
        return "Brutal Strike cannot be used without Reckless Attack"
    end
end

return {
    target_usability = target_usability,
}
