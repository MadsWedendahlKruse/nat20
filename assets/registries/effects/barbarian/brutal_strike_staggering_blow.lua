--- The hook is registered without a `kind` filter, so it covers every saving throw.
--- The effect's end condition removes it as soon as one is made.
---@type D20CheckHookFn
local function d20_check_hook(game_state, entity, check)
    check:add_advantage("disadvantage", "nat20_core::effect.barbarian.brutal_strike.staggering_blow")
end

--- Only Opportunity Attacks are blocked, not reactions in general
---@type ActionUsabilityHookFn
local function action_usability_hook(game_state, entity, action_id, context)
    if action_id == "nat20_core::action.opportunity_attack" then
        return "Staggered: cannot make Opportunity Attacks"
    end
end

return {
    d20_check_hook = d20_check_hook,
    action_usability_hook = action_usability_hook,
}
