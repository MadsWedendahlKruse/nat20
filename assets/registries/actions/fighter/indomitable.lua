---@type ReactionTriggerFn
local function reaction_trigger(game_state, reactor, event)
    if not event:is_d20_check_performed() then
        return false
    end

    local actor, d20_result, d20_dc = event:as_d20_check_performed()
    if actor and d20_result and d20_dc then
        if not d20_result.kind:saving_throw() then
            return false
        end

        return actor == reactor and
            not d20_result:is_success(d20_dc)
    end

    return false
end

---@type ReactionBodyFn
local function reaction_body(game_state, reaction, event)
    if not event:is_d20_check_performed() then
        return
    end

    event:with_d20_check(function(result, dc)
        result:reroll_bonus(
            game_state:class_level(reaction.reactor, "nat20_core::class.fighter"),
            "nat20_core::action.fighter.indomitable",
            true
        )
    end)
end

return {
    reaction_trigger = reaction_trigger,
    reaction_body = reaction_body,
}
