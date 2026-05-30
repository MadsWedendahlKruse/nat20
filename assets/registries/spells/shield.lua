---@type ReactionTriggerFn
local function reaction_trigger(game_state, reactor, event)
    if event:is_action_requested() then
        local action = event:as_action_requested()
        -- Cannot use Shield as a reaction to your own spell
        if action.actor == reactor then
            return false
        end
        -- Note: checking the *action* ID, not the spell ID
        return action.action_id == "nat20_core::action.magic_missile"
    end

    if event:is_d20_check_performed() then
        local actor, d20_result, d20_dc = event:as_d20_check_performed()
        if actor and d20_result and d20_dc then
            if not d20_result.kind:attack_roll() or not d20_dc.target then
                return false
            end

            return d20_dc.target == reactor and
                d20_result:is_success(d20_dc)
        end
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
