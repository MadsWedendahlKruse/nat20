---@type AttackedHookFn
local function attacked_hook(game_state, effect, victim, attacker, check)
    if not check.kind:attack_roll() then
        return
    end

    if effect.applier ~= attacker then
        check:add_modifier("5", "nat20_core::effect.barbarian.brutal_strike.sundering_blow")
    end
end

---@type EventFilterFn
local function event_filter(event, applier, target)
    local actor, result, dc = event:as_d20_check_performed()
    if actor and result and dc and result.kind:attack_roll() then
        return actor ~= applier
    end

    return false
end

return {
    attacked_hook = attacked_hook,
    event_filter = event_filter
}
