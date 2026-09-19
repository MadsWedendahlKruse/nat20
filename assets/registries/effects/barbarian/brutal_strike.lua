-- The strikes are variants of these two, so the action id is always one of them
local BRUTAL_STRIKE_ACTIONS = {
    ["nat20_core::action.barbarian.brutal_strike"] = true,
    ["nat20_core::action.barbarian.improved_brutal_strike"] = true,
}

---@type D20CheckHookFn
local function d20_check_hook(game_state, entity, check)
    if BRUTAL_STRIKE_ACTIONS[check.action_id] then
        check:forgo_advantage("nat20_core::effect.barbarian.brutal_strike")
    end
end

---@type DamageRollHookFn
local function damage_roll_hook(game_state, entity, damage_roll, action, resolution)
    if not resolution:is_attack_roll() or not BRUTAL_STRIKE_ACTIONS[action.action_id] then
        return
    end

    local component = damage_roll.components[1]
    if not component then
        return
    end

    local dice = "1d10"
    if game_state:class_level(entity, "nat20_core::class.barbarian") >= 17 then
        dice = "2d10"
    end

    damage_roll:add_damage(dice, component.damage_type, "nat20_core::effect.barbarian.brutal_strike")
end

return {
    d20_check_hook = d20_check_hook,
    damage_roll_hook = damage_roll_hook,
    BRUTAL_STRIKE_ACTIONS = BRUTAL_STRIKE_ACTIONS,
}
