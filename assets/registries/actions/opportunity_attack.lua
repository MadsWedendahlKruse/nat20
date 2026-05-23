---@type ReactionTriggerFn
local function reaction_trigger(context)
    local event = context.event
    if not event:is_moving_out_of_reach() then
        return false
    end
    local moving_out_of_reach = event:as_moving_out_of_reach()
    return moving_out_of_reach.entity == context.reactor.id
end

---@type ReactionBodyFn
local function reaction_body(context)
    local event = context.event
    if not event:is_moving_out_of_reach() then
        return ReactionPlan.none()
    end
    local moving_out_of_reach = event:as_moving_out_of_reach()
    moving_out_of_reach.continue_movement = false
    return event
end

return {
    reaction_trigger = reaction_trigger,
    reaction_body = reaction_body,
}
