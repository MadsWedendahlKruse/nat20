---@type ResourceCostHookFn
local function resource_cost_hook(game_state, entity, action, cost)
    if action.action_id ~= "nat20_core::action.dash" then
        return
    end

    if cost:costs_resource("nat20_core::resource.action") then
        cost:replace_resource(
            "nat20_core::resource.action",
            "nat20_core::resource.bonus_action",
            "1"
        )
    end
end

return {
    resource_cost_hook = resource_cost_hook,
}
