---@type ReactionTriggerFn
local function reaction_trigger(game_state, reactor, event)
    -- Only care about action-like events (ActionRequested / ReactionRequested).
    if not event:is_action_requested() then
        return false
    end

    local action = event:as_action_requested()
    -- Cannot counterspell yourself.
    if action.actor == reactor then
        return false
    end
    -- Only react to spells.
    if not action.action_context:is_spell() then
        return false
    end

    return true
end

return {
    reaction_trigger = reaction_trigger,
}
