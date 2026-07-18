---@type ReactionTriggerFn
local function reaction_trigger(game_state, reactor, event)
    local actor, d20_result, d20_dc = event:as_d20_check_performed()
    if actor and d20_result and d20_dc then
        if actor ~= reactor then
            return false
        end

        if not d20_result.kind:skill() then
            return false
        end

        return not d20_result:is_success(d20_dc)
    end

    return false
end

---@type ReactionBodyFn
local function reaction_body(game_state, reaction, event)
    event:with_d20_check(function(result, dc)
        result:modify_result("1d10", "nat20_core::action.fighter.tactical_mind")
    end)
end

return {
    reaction_trigger = reaction_trigger,
    reaction_body = reaction_body,
}
