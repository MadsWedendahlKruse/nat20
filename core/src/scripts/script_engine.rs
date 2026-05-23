//! mlua-backed script engine. One Lua state per `ScriptEngine` instance,
//! shared via the [`SCRIPT_ENGINE`] static.
//!
//! Each hook closure runs inside [`mlua::Lua::scope`] so any `UserData` we
//! pass in (especially `&mut GameState` and `&mut DamageRollResult`) is
//! destroyed when the scope exits, releasing all script-side borrows before
//! the caller reclaims ownership.

use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use hecs::Entity;
use mlua::{Function, Lua, RegistryKey, Table, Value};

// mlua's `Lua` is `Send + Sync` with the `send` feature, but its internal
// state lock is *not* reentrant: a thread that's already inside Lua and then
// re-enters via a UserData method (e.g. `game_state:apply_effect(...)` calls
// back into Rust, which calls into another script for a reaction hook) will
// deadlock on itself. Same-thread reentry is exactly what our hooks do, so we
// can't put an outer `Mutex<ScriptEngine>` either — that lock would also
// deadlock the same way.
//
// We rely instead on mlua's own serialization and expose `&ScriptEngine`.
// Only the `module_cache` needs explicit sync; it's behind a short-lived
// `Mutex` and isn't held across script execution.
//
// **Important**: this means **only one OS thread may use `SCRIPT_ENGINE` at a
// time**. For `cargo test`, that means running with `--test-threads=1`. If
// you ever need parallel scripted tests, switch to a thread-local Lua state
// (each thread gets its own engine; no sharing, no contention).

use crate::{
    components::{
        actions::{
            action::{ActionContext, ActionResult},
            targeting::TargetInstance,
        },
        damage::{DamageMitigationResult, DamageRollResult},
        effects::effect::EffectInstance,
        id::{ActionId, ScriptId},
        resource::ResourceAmountMap,
    },
    engine::{action_prompt::ActionData, game_state::GameState},
    registry::registry::REGISTRY_ROOT,
    scripts::{
        lua::lua_types::{self},
        script::{Script, ScriptError, ScriptFunction},
        script_api::{
            ScriptActionPerformedView, ScriptActionView, ScriptEntity, ScriptEventView,
            ScriptReactionBodyContext, ScriptReactionBodyResult, ScriptReactionPlan,
            ScriptReactionTriggerContext,
        },
    },
};

pub static SCRIPT_ENGINE: LazyLock<ScriptEngine> = LazyLock::new(ScriptEngine::new);

pub struct ScriptEngine {
    lua: Lua,
    module_cache: Mutex<HashMap<ScriptId, RegistryKey>>,
}

impl ScriptEngine {
    pub fn new() -> Self {
        let lua = Lua::new();
        lua_types::register_globals(&lua).expect("failed to register Lua globals");

        // Configure package.path so scripts can `require("effects.foo")` and
        // resolve to `<registry_root>/effects/foo.lua`.
        let registry_root = REGISTRY_ROOT.to_string_lossy().replace('\\', "/");
        let path = format!("{0}/?.lua;{0}/?/init.lua", registry_root);
        let cmd = format!("package.path = '{};' .. package.path", path);
        lua.load(cmd)
            .exec()
            .expect("failed to configure package.path");

        ScriptEngine {
            lua,
            module_cache: Mutex::new(HashMap::new()),
        }
    }

    fn load_module(&self, script: &Script) -> Result<Table, ScriptError> {
        // Brief lock to check the cache; release before potentially compiling
        // a new script (which may `require(...)` and re-enter `load_module`).
        {
            let cache = self.module_cache.lock().unwrap();
            if let Some(key) = cache.get(&script.id) {
                return self
                    .lua
                    .registry_value(key)
                    .map_err(|e| ScriptError::RuntimeError(format!("Lua registry: {e}")));
            }
        }

        let chunk = self
            .lua
            .load(&script.content)
            .set_name(script.id.to_string());
        let value: Value = chunk
            .call(())
            .map_err(|e| ScriptError::LoadError(format!("Lua load error: {e}")))?;

        let table = match value {
            Value::Table(t) => t,
            other => {
                return Err(ScriptError::LoadError(format!(
                    "Lua script '{}' did not return a table (got {})",
                    script.id,
                    other.type_name(),
                )));
            }
        };

        let key = self
            .lua
            .create_registry_value(table.clone())
            .map_err(|e| ScriptError::LoadError(format!("Lua registry: {e}")))?;
        self.module_cache
            .lock()
            .unwrap()
            .insert(script.id.clone(), key);
        Ok(table)
    }

    fn get_function(
        &self,
        script: &Script,
        function: ScriptFunction,
    ) -> Result<Function, ScriptError> {
        let module = self.load_module(script)?;
        let func = module
            .get(function.fn_name())
            .map_err(|_| ScriptError::MissingFunction {
                function_name: function.fn_name().to_string(),
                script_id: script.id.clone(),
            })?;
        Ok(func)
    }

    fn runtime_error(e: mlua::Error) -> ScriptError {
        ScriptError::RuntimeError(format!("Lua error: {e}"))
    }

    pub fn evaluate_reaction_trigger(
        &self,
        script: &Script,
        context: &ScriptReactionTriggerContext,
    ) -> Result<bool, ScriptError> {
        let func = self.get_function(script, ScriptFunction::ReactionTrigger)?;
        let ctx = self
            .lua
            .create_userdata(context.clone())
            .map_err(Self::runtime_error)?;
        func.call::<bool>(ctx).map_err(Self::runtime_error)
    }

    pub fn evaluate_reaction_body(
        &self,
        script: &Script,
        context: &ScriptReactionBodyContext,
    ) -> Result<ScriptReactionBodyResult, ScriptError> {
        let func = self.get_function(script, ScriptFunction::ReactionBody)?;
        let ctx = self
            .lua
            .create_userdata(context.clone())
            .map_err(Self::runtime_error)?;
        let value: Value = func.call(ctx).map_err(Self::runtime_error)?;
        match value {
            Value::Nil => Ok(ScriptReactionBodyResult::none()),
            Value::UserData(ud) => {
                if let Ok(plan) = ud.borrow::<ScriptReactionPlan>() {
                    Ok(ScriptReactionBodyResult::Plan(plan.clone()))
                } else if let Ok(body) = ud.borrow::<ScriptReactionBodyResult>() {
                    Ok(body.clone())
                } else if let Ok(event) = ud.borrow::<ScriptEventView>() {
                    Ok(ScriptReactionBodyResult::TriggerEvent(event.clone()))
                } else {
                    Err(Self::runtime_error(mlua::Error::RuntimeError(
                        "Unexpected userdata return type from reaction_body".to_string(),
                    )))
                }
            }
            other => Err(Self::runtime_error(mlua::Error::RuntimeError(format!(
                "Unexpected return type from reaction_body: {}",
                other.type_name()
            )))),
        }
    }

    pub fn evaluate_resource_cost_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        action_id: &ActionId,
        action_context: &ActionContext,
        resource_cost: &mut ResourceAmountMap,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::ResourceCostHook)?;
        let action_view = ScriptActionView::new(
            action_id,
            entity,
            action_context,
            Vec::new(),
            resource_cost.clone(),
        );
        // TODO: What if everything is passed as a reference?
        // Owned-and-static args go through `lua.create_userdata` so their
        // metatable carries the correct TypeId — required for `FromLua` when
        // scripts pass them into other UserData methods. Scope is only needed
        // for borrowed (`game_state`) values
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        let action_ud = self
            .lua
            .create_userdata(action_view)
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                let gs = scope.create_userdata_ref(game_state)?;
                let cost = scope.create_userdata_ref_mut(resource_cost)?;
                func.call::<()>((gs, ent, action_ud, cost))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_action_hook(
        &self,
        script: &Script,
        game_state: &mut GameState,
        action: &ActionData,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::ActionHook)?;
        let action_view = ScriptActionView::from(action);
        let entity = action.actor.id();
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        let av = self
            .lua
            .create_userdata(action_view)
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                let gs = scope.create_userdata_ref_mut(game_state)?;
                func.call::<()>((gs, ent, av))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_action_result_hook(
        &self,
        script: &Script,
        game_state: &mut GameState,
        action: &ActionData,
        results: &[ActionResult],
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::ActionResultHook)?;

        let action_view = ScriptActionView::from(action);
        let mut script_results = Vec::new();
        for result in results {
            if let TargetInstance::Entity { entity, .. } = &result.target {
                script_results.push(
                    crate::scripts::script_api::ScriptActionResultView::from_action_result(
                        action.actor.id(),
                        entity.id(),
                        &result.kind,
                    ),
                );
            }
        }
        let performed = ScriptActionPerformedView::new(action_view, script_results);
        let entity = action.actor.id();

        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        let p = self
            .lua
            .create_userdata(performed)
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                let gs = scope.create_userdata_ref_mut(game_state)?;
                func.call::<()>((gs, ent, p))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_armor_class_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
    ) -> Result<i32, ScriptError> {
        let func = self.get_function(script, ScriptFunction::ArmorClassHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        let modifier: i64 = self
            .lua
            .scope(|scope| {
                let gs_const = scope.create_userdata_ref(game_state)?;
                func.call::<i64>((gs_const, ent))
            })
            .map_err(Self::runtime_error)?;
        Ok(modifier as i32)
    }

    pub fn evaluate_damage_roll_result_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        damage_roll_result: &mut DamageRollResult,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::DamageRollResultHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                let gs = scope.create_userdata_ref(game_state)?;
                let d = scope.create_userdata_ref_mut(damage_roll_result)?;
                func.call::<()>((gs, ent, d))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_pre_damage_mitigation_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        effect: &EffectInstance,
        damage_roll_result: &mut DamageRollResult,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::PreDamageMitigationHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        let ef = self
            .lua
            .create_userdata(effect.clone())
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                let gs = scope.create_userdata_ref(game_state)?;
                let d = scope.create_userdata_ref_mut(damage_roll_result)?;
                func.call::<()>((gs, ent, ef, d))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_post_damage_mitigation_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        damage_mitigation_result: &mut DamageMitigationResult,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::PostDamageMitigationHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                let gs = scope.create_userdata_ref(game_state)?;
                let d = scope.create_userdata_ref_mut(damage_mitigation_result)?;
                func.call::<()>((gs, ent, d))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_death_hook(
        &self,
        script: &Script,
        game_state: &mut GameState,
        victim: Entity,
        killer: Option<Entity>,
        applier: Option<Entity>,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::DeathHook)?;
        let victim = self
            .lua
            .create_userdata(ScriptEntity::from(victim))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref_mut(game_state)?,
                    victim,
                    killer.map(ScriptEntity::from),
                    applier.map(ScriptEntity::from),
                ))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_turn_start_hook(
        &self,
        script: &Script,
        game_state: &mut GameState,
        entity: Entity,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::TurnStartHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                let gs = scope.create_userdata_ref_mut(game_state)?;
                func.call::<()>((gs, ent))
            })
            .map_err(Self::runtime_error)
    }
}
