---@type ArmorClassHookFn
local function armor_class_hook(game_state, entity)
    local armor_type = game_state:armor_type(entity)
    if armor_type ~= "None" and armor_type ~= "Clothing" then
        return 1
    end
    return 0
end

return {
    armor_class_hook = armor_class_hook,
}
