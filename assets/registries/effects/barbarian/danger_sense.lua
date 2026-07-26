---@type D20CheckHookFn
local function d20_check_hook(game_state, entity, d20_check)
    if d20_check.kind:saving_throw() and not game_state:has_effect(entity, "nat20_core::effect.condition.incapacitated") then
        d20_check:add_advantage("advantage", "nat20_core::effect.barbarian.danger_sense")
    end
end

return {
    d20_check_hook = d20_check_hook,
}
