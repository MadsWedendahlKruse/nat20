---@type ReactionTriggerFn
local function reaction_trigger(game_state, reactor, event)
    local action = event:as_action_requested()
    if not action then
        return false
    end

    -- Cannot counterspell yourself.
    if action.actor == reactor then
        return false
    end

    -- Only react to spells.
    return action.action_context:is_spell()
end

return {
    reaction_trigger = reaction_trigger,
}
