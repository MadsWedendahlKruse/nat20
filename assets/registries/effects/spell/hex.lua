---@type PreDamageMitigationHookFn
local function pre_damage_mitigation_hook(game_state, victim, effect, damage_roll_result)
    local actor = damage_roll_result.actor
    local applier = effect.applier
    if damage_roll_result:is_action_attack_roll() and actor and applier then
        if actor == applier then
            damage_roll_result:add_damage("1d6", "necrotic")
        end
    end
end

---@type DeathHookFn
local function death_hook(game_state, victim, killer, applier)
    if applier and not game_state:has_effect(applier, "nat20_core::effect.spell.hex_reapply") then
        game_state:apply_effect_for_turns(
            applier,
            applier,
            "nat20_core::effect.spell.hex_reapply",
            600, -- TODO: Not realy sure about the duration here? 1 hour for now
            false,
            "nat20_core::effect.hex",
            nil,
            nil
        )
    end
end

return {
    pre_damage_mitigation_hook = pre_damage_mitigation_hook,
    death_hook = death_hook,
}
