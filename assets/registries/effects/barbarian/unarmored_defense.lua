---@type ArmorClassHookFn
local function armor_class_hook(engine_state, entity, modifiers)
    local armor_type = engine_state:armor_type(entity)
    if armor_type == "None" or armor_type == "Clothing" then
        modifiers:add_modifier("dexterity", engine_state:ability_modifier(entity, "dexterity").total)
        modifiers:add_modifier("constitution", engine_state:ability_modifier(entity, "constitution").total)
    end
end

return {
    armor_class_hook = armor_class_hook,
}
