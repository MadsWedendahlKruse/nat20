---@type SpeedHookFn
local function speed_hook(engine_state, entity, speed)
    if engine_state:armor_type(entity) ~= "Heavy" then
        speed:add_modifier("nat20_core::effect.barbarian.fast_movement", "10 feet")
    end
end

return {
    speed_hook = speed_hook,
}
