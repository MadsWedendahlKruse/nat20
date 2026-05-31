---@type TurnStartHookFn
local function turn_start_hook(game_state, entity)
    local current_hp = game_state:hp_current(entity)
    local max_hp = game_state:hp_max(entity)
    if current_hp > 0 and current_hp <= max_hp / 2 then
        game_state:heal(
            entity,
            "5",
            game_state:ability_modifier(entity, "constitution"),
            "nat20_core::effect.fighter.champion.survivor.heroic_rally"
        )
    end
end

return {
    turn_start_hook = turn_start_hook,
}
