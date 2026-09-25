---@type TurnStartHookFn
local function turn_start_hook(engine_state, entity)
    local current_hp = engine_state:hp_current(entity)
    local max_hp = engine_state:hp_max(entity)
    if current_hp > 0 and current_hp <= max_hp / 2 then
        engine_state:heal(
            entity,
            {
                ["base"] = "5",
                ["constitution"] = engine_state:ability_modifier(entity, "constitution").total,
            }
        )
    end
end

return {
    turn_start_hook = turn_start_hook,
}
