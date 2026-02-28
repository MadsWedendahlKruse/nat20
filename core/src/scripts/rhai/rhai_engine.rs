use std::collections::HashMap;

use rhai::{AST, Dynamic, Engine, Scope, exported_module, module_resolvers::FileModuleResolver};

use crate::{
    components::id::ScriptId,
    registry::registry::REGISTRY_ROOT,
    scripts::{
        rhai::rhai_types,
        script::{Script, ScriptError, ScriptFunction},
        script_api::{
            ScriptActionContext, ScriptActionKindResultView, ScriptActionOutcomeBundleView,
            ScriptActionPerformedView, ScriptActionResultView, ScriptActionView,
            ScriptD20CheckDCKind, ScriptD20CheckView, ScriptD20Result,
            ScriptDamageMitigationResult, ScriptDamageOutcomeView, ScriptDamageResolutionKindView,
            ScriptDamageRollResult, ScriptEffectView, ScriptEntity, ScriptEntityView,
            ScriptEventView, ScriptLoadoutView, ScriptMovingOutOfReachView,
            ScriptOptionalEntityView, ScriptReactionBodyContext, ScriptReactionBodyResult,
            ScriptReactionPlan, ScriptReactionTriggerContext, ScriptResourceCost,
            ScriptResourceView, ScriptSavingThrow,
        },
        script_engine::ScriptEngine,
    },
};

pub struct RhaiScriptEngine {
    pub engine: Engine,
    pub ast_cache: HashMap<ScriptId, AST>,
}

impl RhaiScriptEngine {
    pub fn new() -> Self {
        let mut engine = Engine::new();

        engine
            .build_type::<ScriptActionContext>()
            .build_type::<ScriptActionView>()
            .build_type::<ScriptActionResultView>()
            .build_type::<ScriptActionKindResultView>()
            .build_type::<ScriptActionOutcomeBundleView>()
            .build_type::<ScriptActionPerformedView>()
            .build_type::<ScriptD20CheckDCKind>()
            .build_type::<ScriptD20CheckView>()
            .build_type::<ScriptD20Result>()
            .build_type::<ScriptDamageMitigationResult>()
            .build_type::<ScriptDamageOutcomeView>()
            .build_type::<ScriptDamageRollResult>()
            .build_type::<ScriptDamageResolutionKindView>()
            .build_type::<ScriptEffectView>()
            .build_type::<ScriptEntity>()
            .build_type::<ScriptEntityView>()
            .build_type::<ScriptEventView>()
            .build_type::<ScriptLoadoutView>()
            .build_type::<ScriptMovingOutOfReachView>()
            .build_type::<ScriptOptionalEntityView>()
            .build_type::<ScriptReactionBodyContext>()
            .build_type::<ScriptReactionBodyResult>()
            .build_type::<ScriptReactionPlan>()
            .build_type::<ScriptReactionTriggerContext>()
            .build_type::<ScriptResourceCost>()
            .build_type::<ScriptResourceView>()
            .build_type::<ScriptSavingThrow>();

        engine.register_static_module(
            "ReactionPlan",
            exported_module!(rhai_types::reaction_plan_module).into(),
        );
        engine.register_static_module(
            "SavingThrow",
            exported_module!(rhai_types::saving_throw_module).into(),
        );

        let resolver = FileModuleResolver::new_with_path(&*REGISTRY_ROOT);
        engine.set_module_resolver(resolver);

        RhaiScriptEngine {
            engine,
            ast_cache: HashMap::new(),
        }
    }

    fn compile_script(&self, script: &Script) -> Result<AST, ScriptError> {
        self.engine
            .compile(&script.content)
            .map_err(|e| ScriptError::LoadError(format!("Failed to compile Rhai script: {}", e)))
    }

    fn cache_script(&mut self, script: &Script) -> Result<(), ScriptError> {
        let ast = self.compile_script(script)?;
        self.ast_cache.insert(script.id.clone(), ast);
        Ok(())
    }

    fn get_ast(&mut self, script: &Script) -> Result<&AST, ScriptError> {
        if !self.ast_cache.contains_key(&script.id) {
            self.cache_script(script)?;
        }
        Ok(self
            .ast_cache
            .get(&script.id)
            .expect("AST must exist after caching"))
    }
}

impl ScriptEngine for RhaiScriptEngine {
    fn evaluate_reaction_trigger(
        &mut self,
        script: &Script,
        // TODO: Maybe not references for the contexts if they're getting cloned anyways?
        context: &ScriptReactionTriggerContext,
    ) -> Result<bool, ScriptError> {
        // TODO: Don't clone AST every time (if it's actually a performance issue)
        let ast = self.get_ast(script).cloned()?;

        let mut scope = Scope::new();
        self.engine
            .call_fn::<bool>(
                &mut scope,
                &ast,
                ScriptFunction::ReactionTrigger.fn_name(),
                (context.clone(),),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))
    }

    fn evaluate_reaction_body(
        &mut self,
        script: &Script,
        context: &ScriptReactionBodyContext,
    ) -> Result<ScriptReactionBodyResult, ScriptError> {
        // TODO: Don't clone AST every time (if it's actually a performance issue)
        let ast = self.get_ast(script).cloned()?;

        let mut scope = Scope::new();
        let result = self
            .engine
            .call_fn::<Dynamic>(
                &mut scope,
                &ast,
                ScriptFunction::ReactionBody.fn_name(),
                (context.clone(),),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))?;

        if result.is::<ScriptReactionBodyResult>() {
            Ok(result.cast::<ScriptReactionBodyResult>())
        } else if result.is::<ScriptReactionPlan>() {
            Ok(ScriptReactionBodyResult::Plan(
                result.cast::<ScriptReactionPlan>(),
            ))
        } else if result.is::<ScriptEventView>() {
            Ok(ScriptReactionBodyResult::TriggerEvent(
                result.cast::<ScriptEventView>(),
            ))
        } else if result.is::<()>() {
            Ok(ScriptReactionBodyResult::none())
        } else {
            Err(ScriptError::RuntimeError(format!(
                "Unexpected return type from reaction_body: {}",
                result.type_name()
            )))
        }
    }

    fn evaluate_resource_cost_hook(
        &mut self,
        script: &Script,
        action: &ScriptActionView,
        entity: &ScriptEntityView,
    ) -> Result<(), ScriptError> {
        let ast = self.get_ast(script).cloned()?;
        let mut scope = Scope::new();
        self.engine
            .call_fn::<()>(
                &mut scope,
                &ast,
                ScriptFunction::ResourceCostHook.fn_name(),
                (action.clone(), entity.clone()),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))?;

        Ok(())
    }

    fn evaluate_action_hook(
        &mut self,
        script: &Script,
        context: &ScriptActionView,
        entity: &ScriptEntityView,
    ) -> Result<(), ScriptError> {
        let ast = self.get_ast(script).cloned()?;
        let mut scope = Scope::new();
        self.engine
            .call_fn::<()>(
                &mut scope,
                &ast,
                ScriptFunction::ActionHook.fn_name(),
                (context.clone(), entity.clone()),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))?;

        Ok(())
    }

    fn evaluate_armor_class_hook(
        &mut self,
        script: &Script,
        entity: &ScriptEntityView,
    ) -> Result<i32, ScriptError> {
        let ast = self.get_ast(script).cloned()?;
        let mut scope = Scope::new();
        let modifier = self
            .engine
            .call_fn::<i64>(
                &mut scope,
                &ast,
                ScriptFunction::ArmorClassHook.fn_name(),
                (entity.clone(),),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))?;

        // Rhai’s default integer type is i64. Apparently there's a "significant
        // runtime performance hit", so might be worth investigating this later?
        // https://rhai.rs/book/language/values-and-types.html
        Ok(modifier as i32)
    }

    fn evaluate_damage_roll_result_hook(
        &mut self,
        script: &Script,
        entity: &ScriptEntityView,
        damage_roll_result: &ScriptDamageRollResult,
    ) -> Result<(), ScriptError> {
        let ast = self.get_ast(script).cloned()?;
        let mut scope = Scope::new();
        self.engine
            .call_fn::<()>(
                &mut scope,
                &ast,
                ScriptFunction::DamageRollResultHook.fn_name(),
                (entity.clone(), damage_roll_result.clone()),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))?;

        Ok(())
    }

    fn evaluate_pre_damage_mitigation_hook(
        &mut self,
        script: &Script,
        entity: &ScriptEntityView,
        effect: &ScriptEffectView,
        damage_roll_result: &ScriptDamageRollResult,
    ) -> Result<(), ScriptError> {
        let ast = self.get_ast(script).cloned()?;
        let mut scope = Scope::new();
        self.engine
            .call_fn::<()>(
                &mut scope,
                &ast,
                ScriptFunction::PreDamageMitigationHook.fn_name(),
                (entity.clone(), effect.clone(), damage_roll_result.clone()),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))?;
        Ok(())
    }

    fn evaluate_post_damage_mitigation_hook(
        &mut self,
        script: &Script,
        entity: &ScriptEntityView,
        damage_mitigation_result: &ScriptDamageMitigationResult,
    ) -> Result<(), ScriptError> {
        let ast = self.get_ast(script).cloned()?;
        let mut scope = Scope::new();
        self.engine
            .call_fn::<()>(
                &mut scope,
                &ast,
                ScriptFunction::PostDamageMitigationHook.fn_name(),
                (entity.clone(), damage_mitigation_result.clone()),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))?;
        Ok(())
    }

    fn evaluate_death_hook(
        &mut self,
        script: &Script,
        victim_entity_view: &ScriptEntityView,
        killer_entity_view: &ScriptOptionalEntityView,
        applier_entity_view: &ScriptOptionalEntityView,
    ) -> Result<(), ScriptError> {
        let ast = self.get_ast(script).cloned()?;
        let mut scope = Scope::new();
        self.engine
            .call_fn::<()>(
                &mut scope,
                &ast,
                ScriptFunction::DeathHook.fn_name(),
                (
                    victim_entity_view.clone(),
                    killer_entity_view.clone(),
                    applier_entity_view.clone(),
                ),
            )
            .map_err(|e| ScriptError::RuntimeError(format!("Rhai error: {}", e)))?;
        Ok(())
    }
}
