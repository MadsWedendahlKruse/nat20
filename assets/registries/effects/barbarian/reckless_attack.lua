local BRUTAL_STRIKE_ACTIONS = require("effects.barbarian.brutal_strike").BRUTAL_STRIKE_ACTIONS

--- Add the Advantage which Reckless Attack is about to grant, so we can see it in
--- the GUI and Brutal Strike's usability gate can check for it. If the Barbarian
--- already has the "real" Reckless Attack effect active, this hook does nothing
---@type D20CheckHookFn
local function d20_check_hook(game_state, entity, d20_check)
    if d20_check.action_id ~= "nat20_core::action.barbarian.reckless_attack" and
        not BRUTAL_STRIKE_ACTIONS[d20_check.action_id]
    then
        return
    end

    if game_state:has_effect(entity, "nat20_core::effect.barbarian.reckless_attack_advantage") then
        return
    end

    if d20_check.kind:attack_roll() and d20_check.modifiers:get_modifier("strength") then
        d20_check:add_advantage("advantage", "nat20_core::effect.barbarian.reckless_attack_advantage")
    end
end

return {
    d20_check_hook = d20_check_hook
}
