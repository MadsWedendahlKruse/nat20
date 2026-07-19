---@meta
-- Type stubs for the nat20-rs Lua scripting API.
-- This file is consumed by lua-language-server (sumneko Lua extension) to
-- provide autocomplete, hover, and diagnostics for scripts under
-- assets/registries/. It is never executed at runtime — keep declarations in
-- sync with the UserData impls in core/src/scripts/lua/lua_types.rs.

------------------------------------------------------------
-- Core handles
------------------------------------------------------------

---@class ScriptEntity
---@field id integer

------------------------------------------------------------
-- Damage
------------------------------------------------------------

---@class DamageComponent
---@field damage_type string
---@field damage ModifierMap
local DamageComponent = {}

---@class DamageRoll
---@field components DamageComponent[]
local DamageRoll = {}

---@param dice string         -- e.g. "1d6"
---@param damage_type string  -- e.g. "necrotic", "fire"
---@param source string       -- e.g. "nat20_core::action.spell.hex"
function DamageRoll:add_damage(dice, damage_type, source) end

---@class DamageComponentResult
---@field damage_type string
---@field result ModifierResult
local DamageComponentResult = {}

---@class DamageRollResult
---@field components DamageComponentResult[]
local DamageRollResult = {}
---@param min integer
function DamageRollResult:clamp_damage_dice_min(min) end

---@param dice string         -- e.g. "1d6"
---@param damage_type string  -- e.g. "necrotic", "fire"
---@param source string       -- e.g. "nat20_core::action.spell.hex"
function DamageRollResult:add_damage(dice, damage_type, source) end

---@class DamageMitigationResult
---@field total integer
local DamageMitigationResult = {}
---@param source string
function DamageMitigationResult:add_immunity(source) end

---@class DamageResult
local DamageResult = {}
---@return DamageRollResult?
function DamageResult:damage_roll() end

---@return DamageMitigationResult?
function DamageResult:damage_taken() end

---@return integer
function DamageResult:damage_roll_total() end

---@return integer
function DamageResult:damage_taken_total() end

------------------------------------------------------------
-- Effects
------------------------------------------------------------

---@class EffectInstance
---@field effect_id string
---@field source string
---@field applier ScriptEntity?

------------------------------------------------------------
-- Action context / views
------------------------------------------------------------

---@class ActionContext
local ActionContext = {}
---@return boolean
function ActionContext:is_spell() end

---@return boolean
function ActionContext:is_attack_action() end

---@return boolean
function ActionContext:is_weapon_attack() end

---@return boolean
function ActionContext:is_unarmed_attack() end

---@return boolean
function ActionContext:is_melee_attack() end

---@return boolean
function ActionContext:is_ranged_attack() end

---@class ActionData
---@field action_id string
---@field actor ScriptEntity
---@field action_context ActionContext
local ActionData = {}
---@param resource_id string
---@return boolean
function ActionData:costs_resource(resource_id) end

---@class ResourceAmountMap
local ResourceAmountMap = {}
---@param resource_id string
---@return boolean
function ResourceAmountMap:costs_resource(resource_id) end

---@param from string
---@param to string
---@param new_amount string|integer
function ResourceAmountMap:replace_resource(from, to, new_amount) end

---@class ActionConditionResolution
local ActionConditionResolution = {}
---@return boolean
function ActionConditionResolution:is_unconditional() end

---@return boolean
function ActionConditionResolution:is_attack_roll() end

---@return boolean
function ActionConditionResolution:is_saving_throw() end

---@return boolean
function ActionConditionResolution:is_success() end

---@return boolean
function ActionConditionResolution:is_crit() end

---@class ActionResult
---@field target ScriptEntity
local ActionResult = {}
---@return DamageResult[]
function ActionResult:damage() end

---@return ActionConditionResolution
function ActionResult:resolution() end

---@param effect_id string
---@return boolean
function ActionResult:has_applied_effect(effect_id) end

---@class TargetInstance
local TargetInstance = {}
---@return ScriptEntity?
function TargetInstance:entity() end

------------------------------------------------------------
-- D20 / event views
------------------------------------------------------------

---@class D20CheckKind
local D20CheckKind = {}
---@return string?
function D20CheckKind:saving_throw() end

---@return string?
function D20CheckKind:skill() end

---@return string?
function D20CheckKind:attack_roll() end

---@class D20ResultKind
---@field kind D20CheckKind
---@field total integer
local D20ResultKind = {}
---@param dc D20CheckDCKind
---@return boolean
function D20ResultKind:is_success(dc) end

---@param bonus string
---@param source string
---@param force_use_new boolean
function D20ResultKind:reroll_bonus(bonus, source, force_use_new) end

---@param bonus string
---@param source string
function D20ResultKind:modify_result(bonus, source) end

---@class D20CheckDCKind
---@field kind D20CheckKind
---@field target ScriptEntity?
local D20CheckDCKind = {}

---@class Event
local Event = {}
---@return ScriptEntity?, D20ResultKind?, D20CheckDCKind?
function Event:as_d20_check_performed() end

---@param callback fun(result: D20ResultKind, dc: D20CheckDCKind)
function Event:with_d20_check(callback) end

---@return ActionData?
function Event:as_action_requested() end

---@return ActionResult?, ScriptEntity?
function Event:as_action_result() end

---Returns the entity that is moving out of reach, and the entity whose reach this
---relates to
---@return ScriptEntity?, ScriptEntity?
function Event:as_moving_out_of_reach() end

---Returns entity, item id, armor type (e.g. "Heavy", nil for non-armor) and
---whether the item was equipped or unequipped
---@return ScriptEntity?, string?, string?, boolean?
function Event:as_equipment_changed() end

------------------------------------------------------------
-- GameState — the main script-facing world handle
------------------------------------------------------------

---@class GameState
local GameState = {}

---@param entity ScriptEntity
---@param class_id string
---@return integer
function GameState:class_level(entity, class_id) end

---@param entity ScriptEntity
---@param resource_id string
---@param amount string|integer
---@return boolean
function GameState:can_afford_resource(entity, resource_id, amount) end

---@param entity ScriptEntity
---@param resource_id string
---@param amount string|integer
function GameState:add_resource(entity, resource_id, amount) end

---@param entity ScriptEntity
---@return integer
function GameState:hp_current(entity) end

---@param entity ScriptEntity
---@return integer
function GameState:hp_max(entity) end

---@param entity ScriptEntity
---@param ability_name string  -- "strength" | "dexterity" | ...
---@return FlatModifierMap
function GameState:ability_modifier(entity, ability_name) end

---@param entity ScriptEntity
---@return string  -- "None" | "Clothing" | "Light" | "Medium" | "Heavy" | "Shield"
function GameState:armor_type(entity) end

---@param entity ScriptEntity
---@param weapon_kind string  -- "Melee" | "Ranged"
---@return boolean
function GameState:wielding_with_both_hands(entity, weapon_kind) end

---@param applier ScriptEntity
---@param target ScriptEntity
---@param effect_id string
---@param source_effect string
---@param context ActionContext?
function GameState:apply_effect(applier, target, effect_id, source_effect, context) end

---@param applier ScriptEntity
---@param target ScriptEntity
---@param effect_id string
---@param turns integer
---@param one_shot boolean
---@param source_effect string
---@param context ActionContext?
---@param resolution ActionConditionResolution?
function GameState:apply_effect_for_turns(
    applier, target, effect_id, turns, one_shot, source_effect, context, resolution
)
end

--- @param target ScriptEntity
--- @param effect_id string
--- @return boolean
function GameState:has_effect(target, effect_id) end

---@param target ScriptEntity
---@param effect_id string
function GameState:remove_effect(target, effect_id) end

---@param target ScriptEntity
---@param amount table
function GameState:heal(target, amount) end

---@class ModifierMap
local ModifierMap = {}
---@param source string
---@param value string
function ModifierMap:add_modifier(source, value) end

---@param source string
---@return string?
function ModifierMap:get_modifier(source) end

---@class FlatModifierMap
---@field total integer
local FlatModifierMap = {}

---@class ModifierResult
---@field total integer
local ModifierResult = {}
------------------------------------------------------------
-- Hook function signatures. Annotate your script-side functions with
--   ---@type <HookName>Fn
-- to get parameter type inference and nil checks.
------------------------------------------------------------

---@alias ArmorClassHookFn fun(game_state: GameState, entity: ScriptEntity): integer
---@alias ActionHookFn fun(game_state: GameState, entity: ScriptEntity, action: ActionData)
---@alias ActionResultHookFn fun(game_state: GameState, action: ActionData, result: ActionResult)
---@alias ResourceCostHookFn fun(game_state: GameState, entity: ScriptEntity, action: ActionData, cost: ResourceAmountMap)
---@alias DamageRollHookFn fun(game_state: GameState, entity: ScriptEntity, damage_roll: DamageRoll, action: ActionData, resolution: ActionConditionResolution)
---@alias DamageRollResultHookFn fun(game_state: GameState, entity: ScriptEntity, damage_roll: DamageRollResult, action: ActionData, resolution: ActionConditionResolution)
---@alias PreDamageMitigationHookFn fun(game_state: GameState, victim: ScriptEntity, effect: EffectInstance, damage_roll: DamageRollResult, action: ActionData?, resolution: ActionConditionResolution?)
---@alias PostDamageMitigationHookFn fun(game_state: GameState, entity: ScriptEntity, damage_taken: DamageMitigationResult, action: ActionData?, resolution: ActionConditionResolution?)
---@alias ReactionTriggerFn fun(game_state: GameState, reactor: ScriptEntity, event: Event): boolean
---@alias EventFilterFn fun(event: Event, applier: ScriptEntity, target: ScriptEntity): boolean
---@alias ReactionBodyFn fun(game_state: GameState, reaction: ActionData, event: Event)
---@alias DeathHookFn fun(game_state: GameState, victim: ScriptEntity, killer: ScriptEntity?, applier: ScriptEntity?)
---@alias TurnStartHookFn fun(game_state: GameState, entity: ScriptEntity)
---@alias ActionUsabilityFn fun(game_state: GameState, entity: ScriptEntity, context: ActionContext): string?
