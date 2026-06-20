---@type ReactionTriggerFn
local function reaction_trigger(game_state, reactor, event)
    if not event:is_moving_out_of_reach() then
        return false
    end

    local mover, entity = event:as_moving_out_of_reach()
    if mover and entity then
        return entity == reactor
    end

    return false
end

return {
    reaction_trigger = reaction_trigger,
}
