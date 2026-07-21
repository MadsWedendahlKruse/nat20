//! mlua-backed script engine. One Lua state per `ScriptEngine` instance,
//! shared via the [`SCRIPT_ENGINE`] static.
//!
//! Each hook closure runs inside [`mlua::Lua::scope`] so any `UserData` we
//! pass in (especially `&mut GameState` and other mutable data) is
//! destroyed when the scope exits, releasing all script-side borrows before
//! the caller reclaims ownership.

use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use hecs::Entity;
use mlua::{Function, Lua, RegistryKey, Table, Value};

// `SCRIPT_ENGINE` exposes `&ScriptEngine` directly — no outer `Mutex`.
// mlua's `Lua` is internally `Send + Sync` (with the `send` feature) and
// uses a `parking_lot::ReentrantMutex` internally, so:
//
// - Same-thread reentry works. Our hooks reliably do this:
//   `apply_effect` → `process_event(GainedEffect)` →
//   `available_reactions_to_event` → another script's `resource_cost_hook`.
//   An outer `std::sync::Mutex<ScriptEngine>` would deadlock the same thread
//   here; mlua's reentrant lock handles it.
// - Cross-thread access is *documented* to serialize via that same reentrant
//   lock. Empirically, however, multiple OS threads pounding on the same
//   `Lua` state from `cargo test` reliably hang once you cross ~4 concurrent
//   scripted tests. The cause is somewhere in mlua-with-vendored-Lua-under-
//   contention rather than in our locking; we can't reproduce it as a clean
//   two-thread deadlock.
//
// **Workaround**: `.cargo/config.toml` sets `RUST_TEST_THREADS=1`, so
// `cargo test` is single-threaded by default. Tests run in ~0.5s total
// either way, so serializing is essentially free.
//
// **Long-term fix** (if you ever care about parallel scripted tests): make
// the engine thread-local. Each thread gets its own `Lua` state, no shared
// `module_cache`, no contention. Costs a per-thread init pass through the
// scripts; for tests that's microseconds.
//
// Only the `module_cache` itself needs explicit sync within `ScriptEngine`;
// it lives behind a short-lived `Mutex` that's never held across script
// execution.

use crate::{
    components::{
        actions::action::{ActionConditionResolution, ActionContext, ActionResult},
        damage::{AttackRoll, DamageMitigationResult, DamageRoll, DamageRollResult},
        effects::effect::EffectInstance,
        id::{ActionId, EntityIdentifier, ScriptId},
        items::equipment::armor::ArmorClass,
        modifier::FlatModifiable,
        resource::ResourceAmountMap,
    },
    engine::{action_prompt::ActionData, event::Event, game_state::GameState},
    registry::registry::REGISTRY_ROOT,
    scripts::{
        script::{Script, ScriptError, ScriptFunction},
        script_api::ScriptEntity,
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
        game_state: &GameState,
        reactor: Entity,
        event: &Event,
    ) -> Result<bool, ScriptError> {
        let func = self.get_function(script, ScriptFunction::ReactionTrigger)?;
        let reactor = self
            .lua
            .create_userdata(ScriptEntity::from(reactor))
            .map_err(Self::runtime_error)?;
        let result: bool = self
            .lua
            .scope(|scope| {
                func.call::<bool>((
                    scope.create_userdata_ref(game_state)?,
                    reactor,
                    scope.create_userdata_ref(event)?,
                ))
            })
            .map_err(Self::runtime_error)?;
        Ok(result)
    }

    pub fn evaluate_reaction_body(
        &self,
        script: &Script,
        game_state: &mut GameState,
        reaction: &ActionData,
        event: &mut Event,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::ReactionBody)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref_mut(game_state)?,
                    scope.create_userdata_ref(reaction)?,
                    scope.create_userdata_ref_mut(event)?,
                ))
            })
            .map_err(Self::runtime_error)
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
        let action_view = ActionData::new(
            EntityIdentifier::from_world(&game_state.world, entity),
            action_id.clone(),
            action_context.clone(),
            resource_cost.clone(),
            Vec::new(),
        );
        // For some reason the entity has to be passed in like this, otherwise the
        // unit tests fail
        let entity = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref(game_state)?,
                    entity,
                    scope.create_userdata_ref(&action_view)?,
                    scope.create_userdata_ref_mut(resource_cost)?,
                ))
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
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref_mut(game_state)?,
                    scope.create_userdata_ref(action)?,
                ))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_action_result_hook(
        &self,
        script: &Script,
        game_state: &mut GameState,
        action: &ActionData,
        results: &ActionResult,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::ActionResultHook)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref_mut(game_state)?,
                    scope.create_userdata_ref(action)?,
                    results.clone(),
                ))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_armor_class_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        armor_class: &mut ArmorClass,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::ArmorClassHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref(game_state)?,
                    ent,
                    scope.create_userdata_ref_mut(armor_class.modifiers_mut())?,
                ))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_attack_roll_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        attack_roll: &mut AttackRoll,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::AttackRollHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref(game_state)?,
                    ent,
                    scope.create_userdata_ref_mut(attack_roll)?,
                ))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_damage_roll_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        damage_roll: &mut DamageRoll,
        action: &ActionData,
        resolution: &ActionConditionResolution,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::DamageRollHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref(game_state)?,
                    ent,
                    scope.create_userdata_ref_mut(damage_roll)?,
                    scope.create_userdata_ref(action)?,
                    scope.create_userdata_ref(resolution)?,
                ))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_damage_roll_result_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        damage_roll_result: &mut DamageRollResult,
        action: &ActionData,
        resolution: &ActionConditionResolution,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::DamageRollResultHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref(game_state)?,
                    ent,
                    scope.create_userdata_ref_mut(damage_roll_result)?,
                    scope.create_userdata_ref(action)?,
                    scope.create_userdata_ref(resolution)?,
                ))
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
        action: Option<&ActionData>,
        resolution: Option<&ActionConditionResolution>,
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
                func.call::<()>((
                    scope.create_userdata_ref(game_state)?,
                    ent,
                    ef,
                    scope.create_userdata_ref_mut(damage_roll_result)?,
                    action.map(|a| scope.create_userdata_ref(a)).transpose()?,
                    resolution
                        .map(|r| scope.create_userdata_ref(r))
                        .transpose()?,
                ))
            })
            .map_err(Self::runtime_error)
    }

    pub fn evaluate_post_damage_mitigation_hook(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        damage_mitigation_result: &mut DamageMitigationResult,
        action: Option<&ActionData>,
        resolution: Option<&ActionConditionResolution>,
    ) -> Result<(), ScriptError> {
        let func = self.get_function(script, ScriptFunction::PostDamageMitigationHook)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        self.lua
            .scope(|scope| {
                func.call::<()>((
                    scope.create_userdata_ref(game_state)?,
                    ent,
                    scope.create_userdata_ref_mut(damage_mitigation_result)?,
                    action.map(|a| scope.create_userdata_ref(a)).transpose()?,
                    resolution
                        .map(|r| scope.create_userdata_ref(r))
                        .transpose()?,
                ))
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

    pub fn evaluate_event_filter(
        &self,
        script: &Script,
        event: &Event,
        applier: Entity,
        target: Entity,
    ) -> Result<bool, ScriptError> {
        let func = self.get_function(script, ScriptFunction::EventFilter)?;
        let applier = self
            .lua
            .create_userdata(ScriptEntity::from(applier))
            .map_err(Self::runtime_error)?;
        let target = self
            .lua
            .create_userdata(ScriptEntity::from(target))
            .map_err(Self::runtime_error)?;
        let result: bool = self
            .lua
            .scope(|scope| func.call::<bool>((scope.create_userdata_ref(event)?, applier, target)))
            .map_err(Self::runtime_error)?;
        Ok(result)
    }

    pub fn evaluate_action_usability(
        &self,
        script: &Script,
        game_state: &GameState,
        entity: Entity,
        context: &ActionContext,
    ) -> Result<Option<String>, ScriptError> {
        let func = self.get_function(script, ScriptFunction::ActionUsability)?;
        let ent = self
            .lua
            .create_userdata(ScriptEntity::from(entity))
            .map_err(Self::runtime_error)?;
        let result = self
            .lua
            .scope(|scope| {
                func.call::<Value>((
                    scope.create_userdata_ref(game_state)?,
                    ent,
                    scope.create_userdata_ref(context)?,
                ))
            })
            .map_err(Self::runtime_error)?;

        match result {
            Value::Nil => Ok(None),
            Value::String(s) => Ok(Some(
                s.to_str()
                    .map_err(|e| {
                        ScriptError::RuntimeError(format!("Lua string conversion error: {e}"))
                    })?
                    .to_string(),
            )),
            other => Err(ScriptError::RuntimeError(format!(
                "Action usability script returned unexpected value: {}",
                other.type_name()
            ))),
        }
    }
}
