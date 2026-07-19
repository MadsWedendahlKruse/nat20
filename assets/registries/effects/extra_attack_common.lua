-- Shared helpers for the Extra Attack family (extra_attack, two_extra_attacks,
-- three_extra_attacks). Consumed via `require("effects.extra_attack_common")`.

local M = {}

M.EXTRA_ATTACK_RESOURCE_ID = "nat20_core::resource.extra_attack"
M.ACTION_RESOURCE_ID = "nat20_core::resource.action"

---@param action ActionData
---@return boolean
local function should_apply(action)
    -- 1) Only weapon attacks
    -- 2) Only when the action's cost is still an `action` resource — i.e. not
    --    already substituted to extra-attack by a previous resource_cost_hook.
    --    Without this guard, `action_hook` (which fires AFTER the cost has
    --    been spent in `validate_action`) would re-grant a charge on every
    --    extra attack and the charge stack would never deplete.
    return action.action_context:is_attack_action() and action:costs_resource(M.ACTION_RESOURCE_ID)
end

---@param max_extra_attacks string|integer
---@return ActionHookFn
function M.action_hook(max_extra_attacks)
    return function(game_state, action)
        if not should_apply(action) then
            return
        end
        -- First trigger of Extra Attack consumes the action; subsequent
        -- attacks come from the extra-attack resource. If we don't already
        -- have extra-attack charges, hand some out.
        if not game_state:can_afford_resource(action.actor, M.EXTRA_ATTACK_RESOURCE_ID, "1") then
            game_state:add_resource(action.actor, M.EXTRA_ATTACK_RESOURCE_ID, max_extra_attacks)
        end
    end
end

---@type ResourceCostHookFn
function M.resource_cost_hook(game_state, entity, action, cost)
    if not should_apply(action) then
        return
    end
    -- If the entity has any extra-attack charges, spend one instead of the action.
    if game_state:can_afford_resource(entity, M.EXTRA_ATTACK_RESOURCE_ID, "1") then
        cost:replace_resource(
            M.ACTION_RESOURCE_ID,
            M.EXTRA_ATTACK_RESOURCE_ID,
            "1"
        )
    end
end

return M
