local primal_knowledge_skills = {
    ["acrobatics"] = true,
    ["intimidation"] = true,
    ["perception"] = true,
    ["stealth"] = true,
    ["survival"] = true
}

---@type D20AbilityHookFn
local function d20_ability_hook(game_state, entity, d20_check)
    if not game_state:has_effect(entity, "nat20_core::effect.barbarian.rage") then
        return
    end

    local skill = d20_check.kind:skill();
    if not skill then
        return
    end

    if not primal_knowledge_skills[skill] then
        return
    end

    local ability = d20_check.ability;
    if not ability then
        return
    end

    local ability_modifier = game_state:ability_modifier(entity, ability)
    local strength_modifier = game_state:ability_modifier(entity, "strength")
    if ability_modifier.total > strength_modifier.total then
        return
    end

    return "strength"
end

return {
    d20_ability_hook = d20_ability_hook,
}
