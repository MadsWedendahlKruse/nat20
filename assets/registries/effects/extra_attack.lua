local common = require("effects.extra_attack_common")

local extra_attack_action_hook = common.action_hook(1)

---@type ActionHookFn
local function action_hook(game_state, entity, action)
    extra_attack_action_hook(game_state, entity, action)
end

---@type ResourceCostHookFn
local function resource_cost_hook(game_state, entity, action, cost)
    common.resource_cost_hook(game_state, entity, action, cost)
end

return {
    action_hook = action_hook,
    resource_cost_hook = resource_cost_hook,
}
