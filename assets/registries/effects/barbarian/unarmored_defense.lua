---@type ArmorClassHookFn
local function armor_class_hook(game_state, entity, modifiers)
    local armor_type = game_state:armor_type(entity)
    if armor_type == "None" or armor_type == "Clothing" then
        modifiers:add_modifier("dexterity", game_state:ability_modifier(entity, "dexterity").total)
        modifiers:add_modifier("constitution", game_state:ability_modifier(entity, "constitution").total)
    end
end

return {
    armor_class_hook = armor_class_hook,
}
