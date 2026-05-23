---@type ReactionTriggerFn
local function reaction_trigger(context)
    return context:is_own_failed_d20_check("SavingThrow")
end

---@type ReactionBodyFn
local function reaction_body(context)
    local event = context.event
    if not event:is_d20_check_performed() then
        return ReactionPlan.none()
    end

    local d20_check = event:as_d20_check_performed()
    d20_check:reroll_result("class.fighter.level", true)
    return event
end

return {
    reaction_trigger = reaction_trigger,
    reaction_body = reaction_body,
}
