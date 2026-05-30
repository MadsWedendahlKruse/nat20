---@type ReactionTriggerFn
local function reaction_trigger(game_state, reactor, event)
    if not event:is_moving_out_of_reach() then
        return false
    end

    local mover, entity, continue = event:as_moving_out_of_reach()
    if mover and entity and continue ~= nil then
        return entity == reactor
    end

    return false
end

---@type ReactionBodyFn
local function reaction_body(game_state, reaction, event)
    if not event:is_moving_out_of_reach() then
        return
    end

    event:with_moving_out_of_reach(function(mover, entity, continue)
        -- return false to prevent the movement, true to allow it
        return false
    end)
end

return {
    reaction_trigger = reaction_trigger,
    reaction_body = reaction_body,
}
