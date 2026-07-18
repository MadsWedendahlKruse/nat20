---@type ActionUsabilityFn
local function action_usability(game_state, entity, context)
    if game_state:armor_type(entity) == "Heavy" then
        return "Rage cannot be used while wearing Heavy armor"
    end

    return nil
end

local rage_damage_table = {
    [1] = "2",
    [2] = "2",
    [3] = "2",
    [4] = "2",
    [5] = "2",
    [6] = "2",
    [7] = "2",
    [8] = "2",
    [9] = "3",
    [10] = "3",
    [11] = "3",
    [12] = "3",
    [13] = "3",
    [14] = "3",
    [15] = "3",
    [16] = "4",
    [17] = "4",
    [18] = "4",
    [19] = "4",
    [20] = "4"
}

---@param game_state GameState
---@param entity ScriptEntity
---@return string
local function rage_damage(game_state, entity)
    local barbarian_level = game_state:class_level(entity, "nat20_core::class.barbarian");
    return rage_damage_table[barbarian_level] or "4"
end

---@type DamageRollHookFn
local function damage_roll_hook(game_state, entity, damage_roll, action, resolution)
    if not resolution:is_attack_roll() then
        return
    end

    for _, component in ipairs(damage_roll.components) do
        local strength_modifier = component.damage:get_modifier("strength")
        if strength_modifier then
            damage_roll:add_damage(
                rage_damage(game_state, entity),
                component.damage_type,
                "nat20_core::effect.barbarian.rage"
            )
        end
    end
end

return {
    action_usability = action_usability,
    damage_roll_hook = damage_roll_hook,
}
