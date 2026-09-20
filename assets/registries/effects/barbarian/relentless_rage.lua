---@type PreDeathHookFn
local function pre_death_hook(game_state, victim, killer, applier)
    if not game_state:has_effect(victim, "nat20_core::effect.barbarian.rage") then
        return
    end

    local dc = game_state:scratchpad_get(
        victim, "nat20_core::effect.barbarian.relentless_rage.save_dc") or 10

    dc_table = {
        ["nat20_core::effect.barbarian.relentless_rage"] = dc
    }

    game_state:saving_throw(victim, "constitution", dc_table, function(game_state, success, result)
        if not success then
            return
        end

        local hit_points = 2 * game_state:class_level(victim, "nat20_core::class.barbarian")
        game_state:heal(
            victim,
            { ["nat20_core::effect.barbarian.relentless_rage"] = tostring(hit_points) }
        )

        game_state:scratchpad_set(
            victim,
            "nat20_core::effect.barbarian.relentless_rage.save_dc",
            dc + 5
        )
    end)
end

---@type RestHookFn
local function rest_hook(game_state, entity, kind)
    game_state:scratchpad_clear(entity, "nat20_core::effect.barbarian.relentless_rage.save_dc")
end

return {
    pre_death_hook = pre_death_hook,
    rest_hook = rest_hook,
}
