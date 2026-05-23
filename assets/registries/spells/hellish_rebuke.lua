---@type ReactionTriggerFn
local function reaction_trigger(context)
    local event = context.event
    if not event:is_action_performed() then
        return false
    end

    local performed = event:as_action_performed()
    -- Cannot target yourself
    if performed.action.actor == context.reactor then
        return false
    end

    for _, result in ipairs(performed:results()) do
        if result.target == context.reactor and result.kind:is_standard() then
            local standard_kind = result.kind:as_standard()
            if standard_kind:has_damage() then
                local damage = standard_kind:get_damage()
                if damage:damage_taken_total() > 0 then
                    return true
                end
            end
        end
    end

    return false
end

return {
    reaction_trigger = reaction_trigger,
}
