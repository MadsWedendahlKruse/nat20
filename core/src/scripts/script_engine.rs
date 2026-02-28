use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use strum::IntoEnumIterator;

use crate::scripts::{
    rhai::rhai_engine::RhaiScriptEngine,
    script::{Script, ScriptError, ScriptLanguage},
    script_api::{
        ScriptActionView, ScriptDamageMitigationResult, ScriptDamageRollResult, ScriptEffectView,
        ScriptEntityView, ScriptOptionalEntityView, ScriptReactionBodyContext,
        ScriptReactionBodyResult, ScriptReactionTriggerContext,
    },
};

pub static SCRIPT_ENGINES: LazyLock<
    Mutex<HashMap<ScriptLanguage, Box<dyn ScriptEngine + Send + Sync>>>,
> = LazyLock::new(|| {
    let mut engines = HashMap::new();
    for language in ScriptLanguage::iter() {
        match language {
            // ScriptLanguage::Lua => {
            //     engines.insert(language, Box::new(LuaScriptEngine::new()));
            // }
            ScriptLanguage::Rhai => {
                engines.insert(
                    language,
                    Box::new(RhaiScriptEngine::new()) as Box<dyn ScriptEngine + Send + Sync>,
                );
            }
        }
    }
    Mutex::new(engines)
});

pub trait ScriptEngine {
    /// Pure predicate: should the reaction trigger?
    fn evaluate_reaction_trigger(
        &mut self,
        script: &Script,
        context: &ScriptReactionTriggerContext,
    ) -> Result<bool, ScriptError>;

    /// Compute the reaction body result to execute.
    fn evaluate_reaction_body(
        &mut self,
        script: &Script,
        context: &ScriptReactionBodyContext,
    ) -> Result<ScriptReactionBodyResult, ScriptError>;

    /// Compute the resource cost for an action.
    fn evaluate_resource_cost_hook(
        &mut self,
        script: &Script,
        action: &ScriptActionView,
        entity: &ScriptEntityView,
    ) -> Result<(), ScriptError>;

    /// Execute an action hook, returning the modified entity view.
    fn evaluate_action_hook(
        &mut self,
        script: &Script,
        action: &ScriptActionView,
        entity: &ScriptEntityView,
    ) -> Result<(), ScriptError>;

    /// Execute an armor class hook, returning the modifier to apply.
    fn evaluate_armor_class_hook(
        &mut self,
        script: &Script,
        entity: &ScriptEntityView,
    ) -> Result<i32, ScriptError>;

    /// Execute a damage roll result hook, returning the modified damage roll result.
    fn evaluate_damage_roll_result_hook(
        &mut self,
        script: &Script,
        entity: &ScriptEntityView,
        damage_roll_result: &ScriptDamageRollResult,
    ) -> Result<(), ScriptError>;

    fn evaluate_pre_damage_mitigation_hook(
        &mut self,
        script: &Script,
        entity: &ScriptEntityView,
        effect: &ScriptEffectView,
        damage_roll_result: &ScriptDamageRollResult,
    ) -> Result<(), ScriptError>;

    fn evaluate_post_damage_mitigation_hook(
        &mut self,
        script: &Script,
        entity: &ScriptEntityView,
        damage_mitigation_result: &ScriptDamageMitigationResult,
    ) -> Result<(), ScriptError>;

    fn evaluate_death_hook(
        &mut self,
        script: &Script,
        victim_entity_view: &ScriptEntityView,
        killer_entity_view: &ScriptOptionalEntityView,
        applier_entity_view: &ScriptOptionalEntityView,
    ) -> Result<(), ScriptError>;
}
