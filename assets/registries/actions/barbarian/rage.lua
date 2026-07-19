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

---@type ActionUsabilityFn
local function action_usability(game_state, entity, context)
    if game_state:armor_type(entity) == "Heavy" then
        return "Rage cannot be used while wearing Heavy armor"
    end

    if game_state:has_effect(entity, "nat20_core::effect.condition.incapacitated") then
        return "Rage cannot be used while Incapacitated"
    end

    if game_state:has_effect(entity, "nat20_core::effect.barbarian.rage") then
        return "Rage is already active"
    end

    return nil
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

---@type EventFilterFn
local function event_filter(event, applier, target)
    -- Rage ends early if you equip Heavy armor
    local entity, item, armor_type, equipped = event:as_equipment_changed()
    if entity and armor_type and equipped then
        return entity == target and equipped and armor_type == "Heavy"
    end

    -- or have the Incapacitated condition
    local result = event:as_action_result()
    if result and result.target == target then
        return result:has_applied_effect("nat20_core::effect.condition.incapacitated")
    end

    return false
end

---@type ActionHookFn
local function action_hook(game_state, action)
    --- Cannot extend further than next turn
    local effect_remaining_duration = game_state:effect_remaining_duration(action.actor,
        "nat20_core::effect.barbarian.rage")
    if effect_remaining_duration and effect_remaining_duration.turns > 1 then
        return
    end

    for _, condition in ipairs(action.conditions) do
        if condition:is_attack_roll() or condition:is_saving_throw() then
            game_state:extend_effect_duration(action.actor, "nat20_core::effect.barbarian.rage", 1)
            return
        end
    end
end

return {
    action_usability = action_usability,
    damage_roll_hook = damage_roll_hook,
    event_filter = event_filter,
    action_hook = action_hook,
}
