---@type ReactionTriggerFn
local function reaction_trigger(context)
    local event = context.event

    if event:is_action_requested() then
        local action = event:as_action_requested()
        -- Cannot use Shield as a reaction to your own spell
        if action.actor == context.reactor then
            return false
        end
        -- Note: checking the *action* ID, not the spell ID
        return action.action_id == "nat20_core::action.magic_missile"
    end

    if event:is_d20_check_performed() then
        local d20_check = event:as_d20_check_performed()
        local result = d20_check.result
        return result.dc_kind.label == "AttackRoll"
            and result.dc_kind.target == context.reactor.id
            and result.is_success == true
    end

    return false
end

---@type PostDamageMitigationHookFn
local function post_damage_mitigation_hook(game_state, entity, damage_mitigation_result)
    if damage_mitigation_result.source == "nat20_core::spell.magic_missile" then
        damage_mitigation_result:add_immunity()
    end
end

return {
    reaction_trigger = reaction_trigger,
    post_damage_mitigation_hook = post_damage_mitigation_hook,
}
