---@type PreDeathHookFn
local function pre_death_hook(engine_state, victim, killer, applier)
    if not engine_state:has_effect(victim, "nat20_core::effect.barbarian.rage") then
        return
    end

    local dc = engine_state:scratchpad_get(
        victim, "nat20_core::effect.barbarian.relentless_rage.save_dc") or 10

    dc_table = {
        ["nat20_core::effect.barbarian.relentless_rage"] = dc
    }

    engine_state:saving_throw(victim, "constitution", dc_table, function(engine_state, success, result)
        if not success then
            return
        end

        local hit_points = 2 * engine_state:class_level(victim, "nat20_core::class.barbarian")
        engine_state:heal(
            victim,
            { ["nat20_core::effect.barbarian.relentless_rage"] = tostring(hit_points) }
        )

        engine_state:scratchpad_set(
            victim,
            "nat20_core::effect.barbarian.relentless_rage.save_dc",
            dc + 5
        )
    end)
end

---@type RestHookFn
local function rest_hook(engine_state, entity, kind)
    engine_state:scratchpad_clear(entity, "nat20_core::effect.barbarian.relentless_rage.save_dc")
end

return {
    pre_death_hook = pre_death_hook,
    rest_hook = rest_hook,
}
