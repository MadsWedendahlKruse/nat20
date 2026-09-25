---@type ReactionTriggerFn
local function reaction_trigger(engine_state, reactor, event)
    local actor, d20_result, d20_dc = event:as_d20_check_performed()
    if actor and d20_result and d20_dc then
        if actor ~= reactor then
            return false
        end

        --- No point in adding advantage if the attack already succeeded
        if d20_result:is_success(d20_dc) then
            return false
        end

        --- Cannot use Reckless Attack if it's already active
        if engine_state:has_effect(reactor, "nat20_core::effect.barbarian.reckless_attack_advantage") then
            return false
        end

        if d20_result.kind:attack_roll() and d20_result.modifiers:get_modifier("strength") then
            return true
        end
    end

    return false
end

---@type ReactionBodyFn
local function reaction_body(engine_state, reaction, event)
    event:with_d20_check(function(result, dc)
        result:add_advantage("advantage", "nat20_core::effect.barbarian.reckless_attack_advantage")
    end)

    engine_state:apply_effect_for_turns(
        reaction.actor,
        reaction.actor,
        "nat20_core::effect.barbarian.reckless_attack_advantage",
        1,
        "nat20_core::effect.barbarian.reckless_attack",
        nil,
        nil
    )
end

return {
    reaction_trigger = reaction_trigger,
    reaction_body = reaction_body,
}
