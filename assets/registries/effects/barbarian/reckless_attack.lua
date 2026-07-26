---@type D20CheckHookFn
local function d20_check_hook(game_state, entity, d20_check)
    if d20_check.kind:attack_roll() and d20_check.modifiers:get_modifier("strength") then
        d20_check:add_advantage("advantage", "nat20_core::effect.barbarian.reckless_attack")
    end
end

return {
    d20_check_hook = d20_check_hook
}
