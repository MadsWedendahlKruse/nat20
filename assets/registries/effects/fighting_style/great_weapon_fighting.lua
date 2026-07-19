---@type DamageRollResultHookFn
local function damage_roll_result_hook(game_state, entity, damage_roll_result, action, resolution)
    -- Only apply for melee weapon attacks wielded with both hands
    local context = action.action_context
    if not (context:is_weapon_attack() and context:is_melee_attack()) then
        return
    end

    if not game_state:wielding_with_both_hands(entity, "Melee") then
        return
    end

    -- Treat any damage dice that rolled a 1 or 2 as a 3
    damage_roll_result:clamp_damage_dice_min(3)
end

return {
    damage_roll_result_hook = damage_roll_result_hook,
}
