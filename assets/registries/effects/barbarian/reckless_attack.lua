---@type AttackRollHookFn
local function attack_roll_hook(game_state, entity, attack_roll)
    if attack_roll.d20_check.modifiers:get_modifier("strength") then
        attack_roll:add_advantage("advantage", "nat20_core::effect.barbarian.reckless_attack")
    end
end

return {
    attack_roll_hook = attack_roll_hook
}
