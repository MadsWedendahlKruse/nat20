---@type ReactionTriggerFn
local function reaction_trigger(game_state, reactor, event)
    if not event:is_action_result() then
        return false
    end

    local result, actor = event:as_action_result()
    if not result then
        return false
    end

    -- Only trigger if the reactor is the target of the action
    if result.target ~= reactor then
        return false
    end

    -- Cannot target yourself
    if actor and actor == reactor then
        return false
    end

    for _, damage in ipairs(result:damage()) do
        if damage:damage_taken_total() > 0 then
            return true
        end
    end

    return false
end

return {
    reaction_trigger = reaction_trigger,
}
