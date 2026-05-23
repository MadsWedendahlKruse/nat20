---@type ReactionTriggerFn
local function reaction_trigger(context)
    local event = context.event
    -- Only care about action-like events (ActionRequested / ReactionRequested).
    if not event:is_action_requested() then
        return false
    end

    local action = event:as_action_requested()
    -- Cannot counterspell yourself.
    if action.actor == context.reactor then
        return false
    end
    -- Only react to spells.
    if not action.action_context:is_spell() then
        return false
    end

    return true
end

---@type ReactionBodyFn
local function reaction_body(context)
    -- The actor that triggered the reaction makes a Constitution saving throw
    -- against the reactor's spell save DC. On failure, the spell is canceled
    -- and the spell slot is refunded.
    local dc = SavingThrow.dc("reactor", "spell_save_dc;constitution")
    local on_success = ReactionPlan.none()
    local on_failure = ReactionPlan.cancel_trigger_event("nat20_core::resource.spell_slot")
    return ReactionPlan.require_saving_throw("actor", dc, on_success, on_failure)
end

return {
    reaction_trigger = reaction_trigger,
    reaction_body = reaction_body,
}
