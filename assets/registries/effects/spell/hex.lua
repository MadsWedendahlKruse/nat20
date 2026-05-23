local HEX_REAPPLY_RESOURCE_ID = "nat20_core::resource.warlock.hex_reapply"

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
    if applier and not game_state:can_afford_resource(applier, HEX_REAPPLY_RESOURCE_ID, "1") then
        game_state:add_resource(applier, HEX_REAPPLY_RESOURCE_ID, "1")
    end
end

return {
    pre_damage_mitigation_hook = pre_damage_mitigation_hook,
    death_hook = death_hook,
}
