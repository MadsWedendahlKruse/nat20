---@type EffectLifetimeHookFn
local function effect_lifetime_hook(engine_state, applier, target, effect_id, lifetime)
    if effect_id == "nat20_core::effect.barbarian.rage" then
        lifetime:set_duration("10 minutes")
    end
end

return {
    effect_lifetime_hook = effect_lifetime_hook,
}
