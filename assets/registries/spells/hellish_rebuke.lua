---@type ReactionTriggerFn
local function reaction_trigger(context)
    local event = context.event
    if not event:is_action_performed() then
        return false
    end

    local action, results = event:as_action_performed()
    if not action or not results then
        return false
    end

    -- Cannot target yourself
    if action.actor == context.reactor then
        return false
    end

    for _, result in ipairs(results) do
        local standard_kind = result.kind:as_standard()
        if result.target:entity() == context.reactor and standard_kind then
            local damage = standard_kind:damage()
            if damage and damage:damage_taken_total() > 0 then
                return true
            end
        end
    end

    return false
end

return {
    reaction_trigger = reaction_trigger,
}
