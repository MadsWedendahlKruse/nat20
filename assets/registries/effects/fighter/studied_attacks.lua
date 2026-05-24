---@type ActionResultHookFn
local function action_result_hook(game_state, entity, action, results)
    for _, result in ipairs(results) do
        local standard_kind = result.kind:as_standard()
        local target_entity = result.target:entity()
        if standard_kind and target_entity then
            local resolution = standard_kind:resolution()
            if resolution and resolution:is_attack_roll() and not resolution:is_success() then
                game_state:apply_effect_for_turns(
                    action.actor,
                    target_entity,
                    "nat20_core::effect.fighter.studied_attacks_advantage",
                    1,
                    true,
                    "nat20_core::effect.fighter.studied_attacks",
                    action.action_context,
                    resolution
                )
            end
        end
    end
end

return {
    action_result_hook = action_result_hook,
}
