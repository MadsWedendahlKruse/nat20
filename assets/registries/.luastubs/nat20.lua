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

---@class DamageRollResult
---@field source string
---@field actor ScriptEntity?
local DamageRollResult = {}
---@param min integer
function DamageRollResult:clamp_damage_dice_min(min) end
---@return boolean
function DamageRollResult:is_action_attack_roll() end
---@return boolean
function DamageRollResult:is_action_saving_throw() end
---@return boolean
function DamageRollResult:is_action_unconditional() end
---@param dice string         -- e.g. "1d6"
---@param damage_type string  -- e.g. "necrotic", "fire"
function DamageRollResult:add_damage(dice, damage_type) end

---@class DamageMitigationResult
---@field source string
local DamageMitigationResult = {}
function DamageMitigationResult:add_immunity() end

---@class ScriptDamageOutcomeView
local ScriptDamageOutcomeView = {}
---@return boolean
function ScriptDamageOutcomeView:has_damage_roll() end
---@return DamageRollResult
function ScriptDamageOutcomeView:get_damage_roll() end
---@return boolean
function ScriptDamageOutcomeView:has_damage_taken() end
---@return DamageMitigationResult
function ScriptDamageOutcomeView:get_damage_taken() end
---@return integer
function ScriptDamageOutcomeView:damage_roll_total() end
---@return integer
function ScriptDamageOutcomeView:damage_taken_total() end

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

---@class ScriptActionContext
local ScriptActionContext = {}
---@return boolean
function ScriptActionContext:is_spell() end
---@return boolean
function ScriptActionContext:is_attack_action() end
---@return boolean
function ScriptActionContext:is_weapon_attack() end
---@return boolean
function ScriptActionContext:is_unarmed_attack() end
---@return boolean
function ScriptActionContext:is_melee_attack() end
---@return boolean
function ScriptActionContext:is_ranged_attack() end

---@class ScriptActionView
---@field action_id string
---@field actor ScriptEntity
---@field action_context ScriptActionContext
local ScriptActionView = {}
---@param entity_id integer
---@return boolean
function ScriptActionView:is_targetting_entity(entity_id) end
---@return integer[]
function ScriptActionView:targets() end
---@param resource_id string
---@return boolean
function ScriptActionView:costs_resource(resource_id) end

---@class ResourceCost
local ResourceCost = {}
---@param resource_id string
---@return boolean
function ResourceCost:costs_resource(resource_id) end
---@param from string
---@param to string
---@param new_amount string|integer
function ResourceCost:replace_resource(from, to, new_amount) end

---@class ScriptActionConditionResolution
local ScriptActionConditionResolution = {}
---@return boolean
function ScriptActionConditionResolution:is_unconditional() end
---@return boolean
function ScriptActionConditionResolution:is_attack_roll() end
---@return boolean
function ScriptActionConditionResolution:is_saving_throw() end
---@return boolean
function ScriptActionConditionResolution:is_attack_roll_hit() end
---@return boolean
function ScriptActionConditionResolution:is_attack_roll_critical_hit() end
---@return boolean
function ScriptActionConditionResolution:is_saving_throw_success() end

---@class ScriptActionOutcomeBundleView
local ScriptActionOutcomeBundleView = {}
---@return boolean
function ScriptActionOutcomeBundleView:has_damage() end
---@return ScriptDamageOutcomeView
function ScriptActionOutcomeBundleView:get_damage() end
---@return boolean
function ScriptActionOutcomeBundleView:has_attack_hit() end
---@return boolean
function ScriptActionOutcomeBundleView:has_attack_critical_hit() end
---@return boolean
function ScriptActionOutcomeBundleView:has_attack_miss() end

---@class ScriptActionKindResultView
local ScriptActionKindResultView = {}
---@return boolean
function ScriptActionKindResultView:is_standard() end
---@return ScriptActionOutcomeBundleView
function ScriptActionKindResultView:as_standard() end

---@class ScriptActionResultView
---@field performer ScriptEntity
---@field target ScriptEntity
---@field kind ScriptActionKindResultView

---@class ScriptActionPerformedView
---@field action ScriptActionView
local ScriptActionPerformedView = {}
---@return ScriptActionResultView[]
function ScriptActionPerformedView:results() end

------------------------------------------------------------
-- D20 / event views
------------------------------------------------------------

---@class ScriptD20CheckDCKind
---@field label string  -- "AttackRoll" | "SavingThrow" | "Skill"
---@field dc integer
---@field target integer

---@class ScriptD20Result
---@field total integer
---@field dc_kind ScriptD20CheckDCKind
---@field is_success boolean

---@class ScriptD20CheckView
---@field performer ScriptEntity
---@field result ScriptD20Result
local ScriptD20CheckView = {}
---@param bonus string
function ScriptD20CheckView:modify_result(bonus) end
---@param modifier string
function ScriptD20CheckView:modify_dc(modifier) end
---@param bonus string
---@param force_use_new boolean
function ScriptD20CheckView:reroll_result(bonus, force_use_new) end

---@class ScriptMovingOutOfReachView
---@field mover integer
---@field entity integer
---@field continue_movement boolean

---@class ScriptEventView
local ScriptEventView = {}
---@return boolean
function ScriptEventView:is_d20_check_performed() end
---@return ScriptD20CheckView
function ScriptEventView:as_d20_check_performed() end
---@return boolean
function ScriptEventView:is_action_requested() end
---@return ScriptActionView
function ScriptEventView:as_action_requested() end
---@return boolean
function ScriptEventView:is_action_performed() end
---@return ScriptActionPerformedView
function ScriptEventView:as_action_performed() end
---@return boolean
function ScriptEventView:is_moving_out_of_reach() end
---@return ScriptMovingOutOfReachView
function ScriptEventView:as_moving_out_of_reach() end

---@class ScriptReactionTriggerContext
---@field reactor ScriptEntity
---@field event ScriptEventView
local ScriptReactionTriggerContext = {}
---@param dc_kind string
---@return boolean
function ScriptReactionTriggerContext:is_own_failed_d20_check(dc_kind) end

---@class ScriptReactionBodyContext
---@field reactor ScriptEntity
---@field event ScriptEventView
---@field reaction_id string
---@field context ScriptActionContext

------------------------------------------------------------
-- GameState — the main script-facing world handle
------------------------------------------------------------

---@class GameState
local GameState = {}

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
---@return integer
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
---@param action ScriptActionPerformedView
function GameState:apply_effect(applier, target, effect_id, action) end

---@param applier ScriptEntity
---@param target ScriptEntity
---@param effect_id string
---@param turns integer
---@param one_shot boolean
---@param action ScriptActionPerformedView
function GameState:apply_effect_for_turns(
    applier, target, effect_id, turns, one_shot, action
) end

---@param target ScriptEntity
---@param effect_id string
function GameState:remove_effect(target, effect_id) end

---@param target ScriptEntity
---@param dice string
---@param bonus ModifierSet
---@param source ModifierSource
function GameState:heal(target, dice, bonus, source) end

------------------------------------------------------------
-- Opaque tag types (used as parameters / returns)
------------------------------------------------------------

---@class ScriptSavingThrow
---@class ScriptReactionPlan
---@class ScriptReactionBodyResult

---@class ModifierSet
local ModifierSet_instance = {}
---@param source ModifierSource
---@param value integer
function ModifierSet_instance:add_modifier(source, value) end

---@class ModifierSource

------------------------------------------------------------
-- Global module tables (registered by lua_types::register_globals)
------------------------------------------------------------

ReactionPlan = {}
---@return ScriptReactionPlan
function ReactionPlan.none() end
---@vararg ScriptReactionPlan
---@return ScriptReactionPlan
function ReactionPlan.sequence(...) end
---@param target_role string  -- "actor" | "reactor" | "target"
---@param dc ScriptSavingThrow
---@param on_success ScriptReactionPlan
---@param on_failure ScriptReactionPlan
---@return ScriptReactionPlan
function ReactionPlan.require_saving_throw(target_role, dc, on_success, on_failure) end
---@vararg string  -- resource IDs to refund
---@return ScriptReactionPlan
function ReactionPlan.cancel_trigger_event(...) end

SavingThrow = {}
---@param entity_role string         -- "actor" | "reactor" | "target"
---@param saving_throw string        -- e.g. "spell_save_dc;constitution"
---@return ScriptSavingThrow
function SavingThrow.dc(entity_role, saving_throw) end

ModifierSet = {}
---@return ModifierSet
function ModifierSet.empty() end
---@param source ModifierSource
---@param value integer
---@return ModifierSet
function ModifierSet.from(source, value) end

ModifierSource = {}
---@param ability_name string
---@return ModifierSource
function ModifierSource.ability(ability_name) end
---@return ModifierSource
function ModifierSource.base() end
---@param effect_id string
---@return ModifierSource
function ModifierSource.effect(effect_id) end

------------------------------------------------------------
-- Hook function signatures. Annotate your script-side functions with
--   ---@type <HookName>Fn
-- to get parameter type inference and nil checks.
------------------------------------------------------------

---@alias ArmorClassHookFn fun(game_state: GameState, entity: ScriptEntity): integer
---@alias ActionHookFn fun(game_state: GameState, entity: ScriptEntity, action: ScriptActionView)
---@alias ActionResultHookFn fun(game_state: GameState, entity: ScriptEntity, action: ScriptActionPerformedView)
---@alias ResourceCostHookFn fun(game_state: GameState, entity: ScriptEntity, action: ScriptActionView, cost: ResourceCost)
---@alias DamageRollResultHookFn fun(game_state: GameState, entity: ScriptEntity, damage_roll: DamageRollResult)
---@alias PreDamageMitigationHookFn fun(game_state: GameState, victim: ScriptEntity, effect: EffectInstance, damage_roll: DamageRollResult)
---@alias PostDamageMitigationHookFn fun(game_state: GameState, entity: ScriptEntity, damage_taken: DamageMitigationResult)
---@alias ReactionTriggerFn fun(context: ScriptReactionTriggerContext): boolean
---@alias ReactionBodyFn fun(context: ScriptReactionBodyContext): ScriptReactionPlan|ScriptEventView|nil
---@alias DeathHookFn fun(game_state: GameState, victim: ScriptEntity, killer: ScriptEntity?, applier: ScriptEntity?)
---@alias TurnStartHookFn fun(game_state: GameState, entity: ScriptEntity)
