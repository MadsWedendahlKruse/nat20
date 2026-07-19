---@type ArmorClassHookFn
local function armor_class_hook(game_state, entity, modifiers)
    local armor_type = game_state:armor_type(entity)
    if armor_type ~= "None" and armor_type ~= "Clothing" then
        modifiers:add_modifier("nat20_core::effect.fighting_style.defense", 1)
    end
end

return {
    armor_class_hook = armor_class_hook,
}
