use hecs::{Entity, World};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use std::{collections::HashMap, hash::Hash, sync::Arc};
use strum::IntoEnumIterator;
use tracing::debug;

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::action::{ActionConditionResolution, ActionContext, ActionResult},
        d20::{D20Check, D20CheckDC, D20CheckKey, D20CheckMap},
        damage::{
            AttackSource, DamageMitigationEffect, DamageMitigationResult, DamageResistances,
            DamageRoll, DamageRollResult,
        },
        effects::{
            effect::{
                Effect, EffectEndConditionTemplate, EffectEntiyReference, EffectEventFilter,
                EffectGrantedAction, EffectInstance, EffectInstanceTemplate, EffectKind,
                EffectLifetimeTemplate, EffectStackingPolicy,
            },
            hooks::{
                ActionHook, ActionResultHook, ArmorClassHook, AttackedHook, D20CheckHooks,
                DamageRollHook, DamageRollResultHook, DeathHook, PostDamageMitigationHook,
                PreDamageMitigationHook, ResourceCostHook, SpeedHook, TurnStartHook,
            },
        },
        health::hit_points::{HitPoints, TemporaryHitPoints},
        id::{ActionId, EffectId, ResourceId, ScriptId},
        items::equipment::{armor::ArmorClass, loadout::Loadout},
        modifier::{FlatModifiable, KeyedFlatModifiable, ModifierSource},
        resource::{ResourceAmount, ResourceAmountMap, ResourceMap},
        saving_throw::{SavingThrowKind, SavingThrowSet},
        skill::{Skill, SkillSet},
        speed::Speed,
        spells::spellbook::Spellbook,
        time::{TimeDuration, TurnBoundary},
    },
    engine::{
        action_prompt::ActionData,
        event::{CallbackResult, EventCallback, EventKind, EventKindTag, ListenerSource},
        game_state::GameState,
    },
    registry::{
        registry::ScriptsRegistry,
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
        serialize::{
            dice::HealEquation,
            modifier::{
                AbilityModifierProvider, ArmorClassModifierProvider, AttackSourceDefinition,
                D20CheckModifierProvider, D20Modifier, DamageResistanceProvider,
                SavingThrowModifierProvider, SkillModifierProvider, SpeedModifier,
                SpeedModifierProvider,
            },
            quantity::{LengthExpressionDefinition, TimeExpressionDefinition},
        },
    },
    scripts::script::ScriptFunction,
    systems::{self},
};

// TODO: Should this be it's own time module?
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum TimeDurationDefinition {
    RealTime { time: TimeExpressionDefinition },
    Turns { turns: u32 },
}

impl From<TimeDurationDefinition> for TimeDuration {
    fn from(definition: TimeDurationDefinition) -> Self {
        match definition {
            TimeDurationDefinition::RealTime { time } => {
                TimeDuration::from_seconds(time.evaluate_without_variables().unwrap().value)
            }
            TimeDurationDefinition::Turns { turns } => TimeDuration::from_turns(turns),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct EffectDefinition {
    pub id: EffectId,
    pub kind: EffectKind,
    pub description: String,

    /// If present, this effect replaces another effect with the given id
    #[serde(default)]
    pub replaces: Option<EffectId>,

    #[serde(default)]
    pub children: Vec<EffectId>,

    #[serde(default)]
    pub stacking_policy: EffectStackingPolicy,

    #[serde(default)]
    pub actions: Vec<EffectGrantedAction>,

    /// End conditions intrinsic to the effect, instantiated for every
    /// application (per-cast conditions live on the applying action's
    /// effect instance instead).
    #[serde(default)]
    pub end_conditions: Vec<EffectEndConditionDefinition>,

    /// Simple effect modifiers like:
    /// - Ability score changes
    /// - Skill modifiers
    /// - Saving throw modifiers
    /// - Damage resistances
    /// - Resource changes
    #[serde(default)]
    pub modifiers: Vec<EffectModifier>,

    /// Scripted d20 check hooks (`check_hook` pre-roll, `result_hook`
    /// post-roll), one field per check kind just like the runtime maps
    #[serde(default)]
    pub on_attack_roll: Vec<AttackRollHookDefinition>,
    #[serde(default)]
    pub on_saving_throw: Vec<SavingThrowHookDefinition>,
    #[serde(default)]
    pub on_skill_check: Vec<SkillCheckHookDefinition>,
    #[serde(default)]
    pub on_attacked: Vec<AttackedHookDefinition>,
    #[serde(default)]
    pub on_armor_class: Vec<ArmorClassHookDefinition>,
    #[serde(default)]
    pub pre_damage_roll: Vec<DamageRollHookDefinition>,
    #[serde(default)]
    pub post_damage_roll: Vec<DamageRollResultHookDefinition>,
    #[serde(default)]
    pub pre_damage_mitigation: Vec<PreDamageMitigationHookDefinition>,
    #[serde(default)]
    pub post_damage_mitigation: Vec<PostDamageMitigationHookDefinition>,
    #[serde(default)]
    pub on_action: Vec<ActionHookDefinition>,
    #[serde(default)]
    pub on_action_result: Vec<ActionResultHookDefinition>,
    #[serde(default)]
    pub on_resource_cost: Vec<ResourceCostHookDefinition>,
    #[serde(default)]
    pub on_death: Vec<DeathHookDefinition>,
    #[serde(default)]
    pub on_turn_start: Vec<TurnStartHookDefinition>,
    #[serde(default)]
    pub on_speed: Vec<SpeedHookDefinition>,
}

impl From<EffectDefinition> for Effect {
    fn from(definition: EffectDefinition) -> Self {
        let effect_id = definition.id.clone();

        let mut effect = Effect::new(effect_id.clone(), definition.kind, definition.description);
        effect.replaces = definition.replaces;
        effect.children = definition.children;
        effect.stacking_policy = definition.stacking_policy;

        effect.actions = definition.actions;
        effect.end_conditions = definition
            .end_conditions
            .into_iter()
            .map(Into::into)
            .collect();

        // 1. Simple persistent modifiers
        // Build on_apply from all modifiers
        {
            let effect_id = effect_id.clone();
            let modifiers = definition.modifiers.clone();
            if !modifiers.is_empty() {
                effect.on_apply = Some(Arc::new(
                    move |game_state: &mut GameState,
                          entity: Entity,
                          context: Option<&ActionContext>| {
                        for modifier in &modifiers {
                            modifier.evaluate(
                                game_state,
                                entity,
                                &effect_id,
                                EffectPhase::Apply,
                                context,
                            );
                        }
                    },
                ));
            }
        }

        // Build on_unapply from the *same* modifiers, but different phase
        {
            let effect_id = effect_id.clone();
            let modifiers_for_unapply = definition.modifiers;
            if !modifiers_for_unapply.is_empty() {
                effect.on_unapply = Some(Arc::new(
                    move |game_state: &mut GameState, entity: Entity| {
                        for modifier in &modifiers_for_unapply {
                            modifier.evaluate(
                                game_state,
                                entity,
                                &effect_id,
                                EffectPhase::Unapply,
                                None,
                            );
                        }
                    },
                ));
            }
        }

        // 2. Hook-based modifiers
        // Build the scripted d20 check hooks, fanning out over all keys when
        // no filter is given
        {
            for def in &definition.on_attack_roll {
                let sources = match &def.source {
                    Some(source) => vec![source.source],
                    None => AttackSource::all().to_vec(),
                };
                let hooks = build_d20_check_hooks(&def.script);
                for source in sources {
                    insert_d20_check_hooks(&mut effect.on_attack_roll, source, hooks.clone());
                }
            }

            for def in &definition.on_saving_throw {
                let kinds = match &def.kind {
                    Some(kind) => vec![*kind],
                    None => SavingThrowKind::iter().collect(),
                };
                let hooks = build_d20_check_hooks(&def.script);
                for kind in kinds {
                    insert_d20_check_hooks(&mut effect.on_saving_throw, kind, hooks.clone());
                }
            }

            for def in &definition.on_skill_check {
                let skills = match &def.skill {
                    Some(skill) => vec![*skill],
                    None => Skill::iter().collect(),
                };
                let hooks = build_d20_check_hooks(&def.script);
                for skill in skills {
                    insert_d20_check_hooks(&mut effect.on_skill_check, skill, hooks.clone());
                }
            }
        }

        // Build on_attacked hooks
        {
            if !definition.on_attacked.is_empty() {
                let hooks = collect_effect_hooks(&definition.on_attacked, &effect_id);
                effect.on_attacked = Some(AttackedHookDefinition::combine_hooks(hooks));
            }
        }

        // Build pre_damage_roll hooks
        {
            if !definition.pre_damage_roll.is_empty() {
                let hooks = collect_effect_hooks(&definition.pre_damage_roll, &effect_id);
                effect.pre_damage_roll = Some(DamageRollHookDefinition::combine_hooks(hooks));
            }
        }

        // Build post_damage_roll hooks
        {
            if !definition.post_damage_roll.is_empty() {
                let hooks = collect_effect_hooks(&definition.post_damage_roll, &effect_id);
                effect.post_damage_roll =
                    Some(DamageRollResultHookDefinition::combine_hooks(hooks));
            }
        }

        // Build pre_damage_taken hooks
        {
            if !definition.pre_damage_mitigation.is_empty() {
                let hooks = collect_effect_hooks(&definition.pre_damage_mitigation, &effect_id);
                effect.pre_damage_mitigation =
                    Some(PreDamageMitigationHookDefinition::combine_hooks(hooks));
            }
        }
        // Build post_damage_mitigation hooks
        {
            if !definition.post_damage_mitigation.is_empty() {
                let hooks = collect_effect_hooks(&definition.post_damage_mitigation, &effect_id);
                effect.post_damage_mitigation =
                    Some(PostDamageMitigationHookDefinition::combine_hooks(hooks));
            }
        }

        // Build armor class hooks
        {
            if !definition.on_armor_class.is_empty() {
                let hooks = collect_effect_hooks(&definition.on_armor_class, &effect_id);
                effect.on_armor_class = Some(ArmorClassHookDefinition::combine_hooks(hooks));
            }
        }

        // Build resource cost hooks
        {
            if !definition.on_resource_cost.is_empty() {
                let hooks = collect_effect_hooks(&definition.on_resource_cost, &effect_id);
                effect.on_resource_cost = Some(ResourceCostHookDefinition::combine_hooks(hooks));
            }
        }

        // Build on_action hooks
        {
            if !definition.on_action.is_empty() {
                let hooks = collect_effect_hooks(&definition.on_action, &effect_id);
                effect.on_action = Some(ActionHookDefinition::combine_hooks(hooks));
            }
        }

        // Build on_action_result hooks
        {
            if !definition.on_action_result.is_empty() {
                let hooks = collect_effect_hooks(&definition.on_action_result, &effect_id);
                effect.on_action_result = Some(ActionResultHookDefinition::combine_hooks(hooks));
            }
        }

        // Build on_death hooks
        {
            if !definition.on_death.is_empty() {
                let hooks = collect_effect_hooks(&definition.on_death, &effect_id);
                effect.on_death = Some(DeathHookDefinition::combine_hooks(hooks));
            }
        }

        // Build on_turn_start hooks
        {
            if !definition.on_turn_start.is_empty() {
                let hooks = collect_effect_hooks(&definition.on_turn_start, &effect_id);
                effect.on_turn_start = Some(TurnStartHookDefinition::combine_hooks(hooks));
            }
        }

        // Build speed hooks
        {
            if !definition.on_speed.is_empty() {
                let hooks = collect_effect_hooks(&definition.on_speed, &effect_id);
                effect.on_speed = Some(SpeedHookDefinition::combine_hooks(hooks));
            }
        }

        effect
    }
}

impl RegistryReferenceCollector for EffectDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        if let Some(replaces) = &self.replaces {
            collector.add(RegistryReference::Effect(replaces.clone()));
        }
        for child in &self.children {
            collector.add(RegistryReference::Effect(child.clone()));
        }
        for modifier in &self.modifiers {
            match modifier {
                EffectModifier::Resource { resource, .. } => {
                    collector.add(RegistryReference::Resource(resource.clone()));
                }
                _ => { /* No references to collect */ }
            }
        }
        for end_condition in &self.end_conditions {
            if let EffectEventFilterDefinition::Script { script, .. } = &end_condition.event {
                collector.add(RegistryReference::Script(
                    script.clone(),
                    ScriptFunction::EventFilter,
                ));
            }
        }
        let d20_hook_scripts = self
            .on_attack_roll
            .iter()
            .map(|hook| &hook.script)
            .chain(self.on_saving_throw.iter().map(|hook| &hook.script))
            .chain(self.on_skill_check.iter().map(|hook| &hook.script));
        for script in d20_hook_scripts {
            collector.add(RegistryReference::ScriptAnyOf(
                script.clone(),
                vec![
                    ScriptFunction::D20AbilityHook,
                    ScriptFunction::D20CheckHook,
                    ScriptFunction::D20CheckResultHook,
                ],
            ));
        }
        for hook in &self.post_damage_roll {
            match hook {
                DamageRollResultHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::DamageRollResultHook,
                    ));
                }
            }
        }
        for hook in &self.on_armor_class {
            match hook {
                ArmorClassHookDefinition::Modifier { .. } => { /* No references to collect */ }
                ArmorClassHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::ArmorClassHook,
                    ));
                }
            }
        }
        for hook in &self.on_action {
            match hook {
                ActionHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::ActionHook,
                    ));
                }
            }
        }
        for hook in &self.on_action_result {
            match hook {
                ActionResultHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::ActionResultHook,
                    ));
                }
            }
        }
        for hook in &self.on_resource_cost {
            match hook {
                ResourceCostHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::ResourceCostHook,
                    ));
                }
            }
        }
        for hook in &self.on_death {
            match hook {
                DeathHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::DeathHook,
                    ));
                }
            }
        }
        for hook in &self.on_turn_start {
            match hook {
                TurnStartHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::TurnStartHook,
                    ));
                }
            }
        }
        for hook in &self.on_speed {
            match hook {
                SpeedHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::SpeedHook,
                    ));
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum EffectModifier {
    Ability {
        ability: AbilityModifierProvider,
    },
    Skill {
        skill: SkillModifierProvider,
    },
    SavingThrow {
        saving_throw: SavingThrowModifierProvider,
    },
    AttackRoll {
        #[serde(default)]
        attack_roll_source: Option<AttackSourceDefinition>,
        attack_roll_modifier: D20Modifier,
    },
    DamageResistance {
        resistance: DamageResistanceProvider,
    },
    Resource {
        resource: ResourceId,
        #[serde(default)]
        amount: Option<ResourceAmount>,
        #[serde(default)]
        disable: bool,
    },
    Speed {
        speed: SpeedModifierProvider,
    },
    FreeMovement {
        free_movement: f32,
    },
    TemporaryHitPoints {
        temporary_hit_points: HealEquation,
    },
    Concentration {
        #[serde(default)]
        break_concentration: bool,
        #[serde(default)]
        block_concentration: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EffectPhase {
    Apply,
    Unapply,
}

impl EffectModifier {
    pub fn evaluate(
        &self,
        game_state: &mut GameState,
        entity: Entity,
        effect_id: &EffectId,
        phase: EffectPhase,
        context: Option<&ActionContext>,
    ) {
        let source = ModifierSource::Effect(effect_id.clone());
        match self {
            EffectModifier::Ability { ability: modifier } => {
                let mut abilities = systems::helpers::get_component_mut::<AbilityScoreMap>(
                    &mut game_state.world,
                    entity,
                );
                match phase {
                    EffectPhase::Apply => {
                        abilities.add_modifier(&modifier.ability, source, modifier.delta);
                    }
                    EffectPhase::Unapply => {
                        abilities.remove_modifier(&modifier.ability, &source);
                    }
                }
            }

            EffectModifier::Skill { skill: modifier } => {
                let mut skills =
                    systems::helpers::get_component_mut::<SkillSet>(&mut game_state.world, entity);
                Self::apply_d20_check_modifier(&mut *skills, modifier, source, phase);
            }

            EffectModifier::SavingThrow {
                saving_throw: modifier,
            } => {
                let mut saves = systems::helpers::get_component_mut::<SavingThrowSet>(
                    &mut game_state.world,
                    entity,
                );
                Self::apply_d20_check_modifier(&mut *saves, modifier, source, phase);
            }

            EffectModifier::AttackRoll {
                attack_roll_source: attack_source,
                attack_roll_modifier: modifier,
            } => {
                let sources = match attack_source {
                    // Specific source means only apply to that source's attack rolls
                    Some(attack_source) => vec![attack_source.source],
                    // No source means all attack rolls
                    None => AttackSource::all().to_vec(),
                };

                for attack_source in sources {
                    match attack_source {
                        AttackSource::Weapon(weapon_kind) => {
                            let mut loadout = systems::helpers::get_component_mut::<Loadout>(
                                &mut game_state.world,
                                entity,
                            );
                            Self::apply_attack_roll_modifier(
                                loadout.attack_roll_template_mut(&weapon_kind),
                                modifier,
                                &source,
                                phase,
                            );
                        }

                        AttackSource::Spell => {
                            let mut spellbook = systems::helpers::get_component_mut::<Spellbook>(
                                &mut game_state.world,
                                entity,
                            );
                            Self::apply_attack_roll_modifier(
                                spellbook.attack_roll_template_mut(),
                                modifier,
                                &source,
                                phase,
                            );
                        }
                    }
                }
            }

            EffectModifier::DamageResistance {
                resistance: modifier,
            } => {
                let mut res = systems::helpers::get_component_mut::<DamageResistances>(
                    &mut game_state.world,
                    entity,
                );
                let mitigation_effect = DamageMitigationEffect {
                    source: source.clone(),
                    operation: modifier.operation.clone(),
                };
                match phase {
                    EffectPhase::Apply => {
                        res.add_effect(modifier.damage_type, mitigation_effect);
                    }
                    EffectPhase::Unapply => {
                        res.remove_effect(modifier.damage_type, &mitigation_effect);
                    }
                }
            }

            EffectModifier::Resource {
                resource,
                amount,
                disable,
            } => {
                let mut resources = systems::helpers::get_component_mut::<ResourceMap>(
                    &mut game_state.world,
                    entity,
                );
                match phase {
                    EffectPhase::Apply => {
                        if let Some(amount) = amount {
                            resources.add_uses(resource, amount);
                        }
                        if *disable {
                            resources.disable_resource(resource, source.clone());
                        }
                    }
                    EffectPhase::Unapply => {
                        if let Some(amount) = amount {
                            resources.remove_uses(resource, amount);
                        }
                        if *disable {
                            resources.enable_resource(resource);
                        }
                    }
                }
            }

            EffectModifier::Speed { speed: modifier } => {
                let mut speed =
                    systems::helpers::get_component_mut::<Speed>(&mut game_state.world, entity);
                match phase {
                    EffectPhase::Apply => match &modifier.modifier {
                        SpeedModifier::Flat(bonus) => {
                            speed.add_flat_modifier(
                                source,
                                bonus.evaluate_without_variables().unwrap().value,
                            );
                        }
                        SpeedModifier::Multiplier(multiplier) => {
                            speed.add_multiplier(source, *multiplier);
                        }
                    },
                    EffectPhase::Unapply => match modifier.modifier {
                        SpeedModifier::Flat(_) => {
                            speed.remove_flat_modifier(&source);
                        }
                        SpeedModifier::Multiplier(_) => {
                            speed.remove_multiplier(&source);
                        }
                    },
                }
            }

            EffectModifier::FreeMovement { free_movement } => {
                let mut speed =
                    systems::helpers::get_component_mut::<Speed>(&mut game_state.world, entity);
                match phase {
                    EffectPhase::Apply => {
                        speed.add_free_movement_multiplier(source, *free_movement);
                    }
                    EffectPhase::Unapply => {
                        speed.remove_free_movement_multiplier(&source);
                    }
                }
            }

            EffectModifier::TemporaryHitPoints {
                temporary_hit_points,
            } => {
                if let Some(context) = context {
                    let amount =
                        (temporary_hit_points.function)(&mut game_state.world, entity, context)
                            .evaluate()
                            .total() as u32;
                    let mut hit_points = systems::helpers::get_component_mut::<HitPoints>(
                        &mut game_state.world,
                        entity,
                    );
                    let source = ModifierSource::Effect(effect_id.clone());
                    match phase {
                        EffectPhase::Apply => {
                            hit_points.set_temp(TemporaryHitPoints::new(amount, &source));
                        }
                        EffectPhase::Unapply => {
                            hit_points.clear_temp(&source);
                        }
                    }
                }
            }

            EffectModifier::Concentration {
                break_concentration,
                block_concentration,
            } => {
                if *break_concentration && phase == EffectPhase::Apply {
                    systems::spells::break_concentration(game_state, entity);
                }
                if *block_concentration {
                    let mut spellbook = systems::helpers::get_component_mut::<Spellbook>(
                        &mut game_state.world,
                        entity,
                    );
                    match phase {
                        EffectPhase::Apply => {
                            spellbook
                                .concentration_tracker_mut()
                                .block_concentration(source);
                        }
                        EffectPhase::Unapply => {
                            spellbook
                                .concentration_tracker_mut()
                                .unblock_concentration(&source);
                        }
                    }
                }
            }
        }
    }

    fn apply_d20_check_modifier<K>(
        modifiable: &mut D20CheckMap<K>,
        modifier: &D20CheckModifierProvider<K>,
        source: ModifierSource,
        phase: EffectPhase,
    ) where
        K: D20CheckKey + DeserializeOwned,
    {
        for kind in &modifier.kind {
            match phase {
                EffectPhase::Apply => {
                    modifier
                        .modifier
                        .apply(modifiable.get_mut(kind), source.clone());
                }
                EffectPhase::Unapply => {
                    modifier.modifier.remove(modifiable.get_mut(kind), &source);
                }
            }
        }
    }

    fn apply_attack_roll_modifier(
        check: &mut D20Check,
        modifier: &D20Modifier,
        source: &ModifierSource,
        phase: EffectPhase,
    ) {
        match phase {
            EffectPhase::Apply => modifier.apply(check, source.clone()),
            EffectPhase::Unapply => modifier.remove(check, source),
        }
    }
}

/// Trait for effects that rely on hooks rather than simple modifiers
pub trait HookEffect<HookFn> {
    fn build_hook(&self, effect_id: &EffectId) -> HookFn;

    fn combine_hooks(hooks: Vec<HookFn>) -> HookFn;
}

fn collect_effect_hooks<HookFn, HookDefinition>(
    definitions: &Vec<HookDefinition>,
    effect_id: &EffectId,
) -> Vec<HookFn>
where
    HookDefinition: HookEffect<HookFn>,
{
    definitions
        .iter()
        .map(|def| def.build_hook(effect_id))
        .collect::<Vec<HookFn>>()
}

/// A scripted d20 check hook: the script defines `check_hook` (pre-roll)
/// and/or `result_hook` (post-roll). Registry validation requires at least
/// one of the two.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct AttackRollHookDefinition {
    pub script: ScriptId,
    /// Restrict to attacks from one source; all attacks when omitted
    #[serde(default)]
    pub source: Option<AttackSourceDefinition>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SavingThrowHookDefinition {
    pub script: ScriptId,
    /// Restrict to one saving throw kind; all of them when omitted
    #[serde(default)]
    pub kind: Option<SavingThrowKind>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SkillCheckHookDefinition {
    pub script: ScriptId,
    /// Restrict to one skill; all skills when omitted
    #[serde(default)]
    pub skill: Option<Skill>,
}

/// Wires a script into a `D20CheckHooks` pair. Each side only fires if the
/// script actually defines the corresponding function.
fn build_d20_check_hooks(script: &ScriptId) -> D20CheckHooks {
    D20CheckHooks {
        ability_hook: Arc::new({
            let script = script.clone();
            move |game_state, entity, check| {
                if script_defines(&script, ScriptFunction::D20AbilityHook) {
                    systems::scripts::evaluate_d20_ability_hook(&script, game_state, entity, check)
                } else {
                    None
                }
            }
        }),
        check_hook: Arc::new({
            let script = script.clone();
            move |game_state, entity, check| {
                if script_defines(&script, ScriptFunction::D20CheckHook) {
                    systems::scripts::evaluate_d20_check_hook(&script, game_state, entity, check);
                }
            }
        }),
        result_hook: Arc::new({
            let script = script.clone();
            move |game_state, entity, result| {
                if script_defines(&script, ScriptFunction::D20CheckResultHook) {
                    systems::scripts::evaluate_d20_result_hook(&script, game_state, entity, result);
                }
            }
        }),
    }
}

fn script_defines(script: &ScriptId, function: ScriptFunction) -> bool {
    ScriptsRegistry::get(script).is_some_and(|s| function.defined_in_script(s))
}

fn insert_d20_check_hooks<K>(map: &mut HashMap<K, D20CheckHooks>, key: K, hooks: D20CheckHooks)
where
    K: Eq + Hash + Clone,
{
    match map.remove(&key) {
        Some(existing) => {
            map.insert(key, D20CheckHooks::combined(vec![existing, hooks]));
        }
        None => {
            map.insert(key, hooks);
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum AttackedHookDefinition {
    Modifier {
        modifier: D20Modifier,
        #[serde(default)]
        attacked_by: Option<EffectEntiyReference>,
        distance: Option<LengthExpressionDefinition>,
    },
}

impl HookEffect<AttackedHook> for AttackedHookDefinition {
    fn build_hook(&self, effect: &EffectId) -> AttackedHook {
        match self {
            AttackedHookDefinition::Modifier {
                modifier,
                attacked_by,
                distance,
            } => {
                let modifier_source = ModifierSource::Effect(effect.clone());
                Arc::new({
                    let modifier = modifier.clone();
                    let attacked_by = attacked_by.clone();
                    let distance = distance.clone();

                    move |world: &World,
                          effect: &EffectInstance,
                          victim: Entity,
                          attacker: Entity,
                          check: &mut D20Check| {
                        if let Some(distance_expression) = &distance {
                            let distance_between = systems::geometry::distance_between_entities(
                                world, victim, attacker,
                            )
                            .unwrap();

                            let max_distance =
                                distance_expression.evaluate_without_variables().unwrap();

                            if distance_between > max_distance {
                                return;
                            }
                        }

                        if let Some(attacked_by) = attacked_by {
                            // Only apply if the attacker matches the reference
                            let attacker_matches = match attacked_by {
                                EffectEntiyReference::Applier => effect.applier == Some(attacker),
                                // Target reference doesn't make sense here?
                                EffectEntiyReference::Target => false,
                            };
                            if !attacker_matches {
                                return;
                            }
                        }

                        modifier.apply(check, modifier_source.clone());
                    }
                })
            }
        }
    }

    fn combine_hooks(hooks: Vec<AttackedHook>) -> AttackedHook {
        Arc::new(move |world, victim, attacker, effect, attack_roll| {
            for hook in &hooks {
                hook(world, victim, attacker, effect, attack_roll);
            }
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum DamageRollHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<DamageRollHook> for DamageRollHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> DamageRollHook {
        match self {
            DamageRollHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &GameState,
                          entity: Entity,
                          damage_roll: &mut DamageRoll,
                          action: &ActionData,
                          resolution: &ActionConditionResolution| {
                        systems::scripts::evaluate_damage_roll_hook(
                            &script_id,
                            game_state,
                            entity,
                            damage_roll,
                            action,
                            resolution,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<DamageRollHook>) -> DamageRollHook {
        Arc::new(
            move |game_state: &GameState,
                  entity: Entity,
                  damage_roll: &mut DamageRoll,
                  action,
                  resolution| {
                for hook in &hooks {
                    hook(game_state, entity, damage_roll, action, resolution);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum DamageRollResultHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<DamageRollResultHook> for DamageRollResultHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> DamageRollResultHook {
        match self {
            DamageRollResultHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &GameState,
                          entity: Entity,
                          damage_roll_result: &mut DamageRollResult,
                          action: &ActionData,
                          resolution: &ActionConditionResolution| {
                        systems::scripts::evaluate_damage_roll_result_hook(
                            &script_id,
                            game_state,
                            entity,
                            damage_roll_result,
                            action,
                            resolution,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<DamageRollResultHook>) -> DamageRollResultHook {
        Arc::new(
            move |game_state: &GameState,
                  entity: Entity,
                  damage_roll_result: &mut DamageRollResult,
                  action: &ActionData,
                  resolution: &ActionConditionResolution| {
                for hook in &hooks {
                    hook(game_state, entity, damage_roll_result, action, resolution);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ArmorClassHookDefinition {
    Modifier {
        modifier: ArmorClassModifierProvider,
    },
    Script {
        script: ScriptId,
    },
}

impl HookEffect<ArmorClassHook> for ArmorClassHookDefinition {
    fn build_hook(&self, effect: &EffectId) -> ArmorClassHook {
        match self {
            ArmorClassHookDefinition::Modifier { modifier } => Arc::new({
                let modifier = modifier.clone();
                let effect = effect.clone();
                move |_game_state, _entity, armor_class| {
                    armor_class
                        .add_modifier(ModifierSource::Effect(effect.clone()), modifier.delta);
                }
            }),

            ArmorClassHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &GameState, entity: Entity, armor_class: &mut ArmorClass| {
                        systems::scripts::evaluate_armor_class_hook(
                            &script_id,
                            game_state,
                            entity,
                            armor_class,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<ArmorClassHook>) -> ArmorClassHook {
        Arc::new(move |game_state, entity, armor_class| {
            for hook in &hooks {
                hook(game_state, entity, armor_class);
            }
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ActionHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<ActionHook> for ActionHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> ActionHook {
        match self {
            ActionHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &mut GameState, action_data: &ActionData| {
                        systems::scripts::evaluate_action_hook(&script_id, game_state, action_data);
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<ActionHook>) -> ActionHook {
        Arc::new(
            move |game_state: &mut GameState, action_data: &ActionData| {
                for hook in &hooks {
                    hook(game_state, action_data);
                }
            },
        )
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ActionResultHookDefinition {
    Script { script: ScriptId },
}

impl std::fmt::Debug for ActionResultHookDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActionResultHookDefinition::Script { script } => {
                f.debug_struct("Script").field("script", script).finish()
            }
        }
    }
}

impl HookEffect<ActionResultHook> for ActionResultHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> ActionResultHook {
        match self {
            ActionResultHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &mut GameState,
                          action_data: &ActionData,
                          results: &ActionResult| {
                        systems::scripts::evaluate_action_result_hook(
                            &script_id,
                            game_state,
                            action_data,
                            results,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<ActionResultHook>) -> ActionResultHook {
        Arc::new(
            move |game_state: &mut GameState, action_data: &ActionData, results: &ActionResult| {
                for hook in &hooks {
                    hook(game_state, action_data, results);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ResourceCostHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<ResourceCostHook> for ResourceCostHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> ResourceCostHook {
        match self {
            ResourceCostHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &GameState,
                          entity: Entity,
                          action: &ActionId,
                          context: &ActionContext,
                          resource_costs: &mut ResourceAmountMap| {
                        systems::scripts::evaluate_resource_cost_hook(
                            &script_id,
                            game_state,
                            entity,
                            action,
                            context,
                            resource_costs,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<ResourceCostHook>) -> ResourceCostHook {
        Arc::new(
            move |game_state: &GameState,
                  entity: Entity,
                  action: &ActionId,
                  context: &ActionContext,
                  resource_costs: &mut ResourceAmountMap| {
                for hook in &hooks {
                    hook(game_state, entity, action, context, resource_costs);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum PreDamageMitigationHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<PreDamageMitigationHook> for PreDamageMitigationHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> PreDamageMitigationHook {
        match self {
            PreDamageMitigationHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &GameState,
                          effect: &EffectInstance,
                          entity: Entity,
                          damage_roll_result: &mut DamageRollResult,
                          action: Option<&ActionData>,
                          resolution: Option<&ActionConditionResolution>| {
                        systems::scripts::evaluate_pre_damage_mitigation_hook(
                            &script_id,
                            game_state,
                            entity,
                            effect,
                            damage_roll_result,
                            action,
                            resolution,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<PreDamageMitigationHook>) -> PreDamageMitigationHook {
        Arc::new(
            move |game_state: &GameState,
                  effect: &EffectInstance,
                  entity: Entity,
                  damage_roll_result: &mut DamageRollResult,
                  action: Option<&ActionData>,
                  resolution: Option<&ActionConditionResolution>| {
                for hook in &hooks {
                    hook(
                        game_state,
                        effect,
                        entity,
                        damage_roll_result,
                        action,
                        resolution,
                    );
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum PostDamageMitigationHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<PostDamageMitigationHook> for PostDamageMitigationHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> PostDamageMitigationHook {
        match self {
            PostDamageMitigationHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &GameState,
                          entity: Entity,
                          damage_mitigation_result: &mut DamageMitigationResult,
                          action: Option<&ActionData>,
                          resolution: Option<&ActionConditionResolution>| {
                        systems::scripts::evaluate_post_damage_mitigation_hook(
                            &script_id,
                            game_state,
                            entity,
                            damage_mitigation_result,
                            action,
                            resolution,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<PostDamageMitigationHook>) -> PostDamageMitigationHook {
        Arc::new(
            move |game_state: &GameState,
                  entity: Entity,
                  damage_mitigation_result: &mut DamageMitigationResult,
                  action: Option<&ActionData>,
                  resolution: Option<&ActionConditionResolution>| {
                for hook in &hooks {
                    hook(
                        game_state,
                        entity,
                        damage_mitigation_result,
                        action,
                        resolution,
                    );
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum DeathHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<DeathHook> for DeathHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> DeathHook {
        match self {
            DeathHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &mut GameState,
                          victim: Entity,
                          killer: Option<Entity>,
                          applier: Option<Entity>| {
                        systems::scripts::evaluate_death_hook(
                            &script_id, game_state, victim, killer, applier,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<DeathHook>) -> DeathHook {
        Arc::new(
            move |game_state: &mut GameState,
                  victim: Entity,
                  killer: Option<Entity>,
                  applier: Option<Entity>| {
                for hook in &hooks {
                    hook(game_state, victim, killer, applier);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum TurnStartHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<TurnStartHook> for TurnStartHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> TurnStartHook {
        match self {
            TurnStartHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(move |game_state: &mut GameState, entity: Entity| {
                    systems::scripts::evaluate_turn_start_hook(&script_id, game_state, entity);
                })
            }
        }
    }

    fn combine_hooks(hooks: Vec<TurnStartHook>) -> TurnStartHook {
        Arc::new(move |game_state, entity| {
            for hook in &hooks {
                hook(game_state, entity);
            }
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum SpeedHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<SpeedHook> for SpeedHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> SpeedHook {
        match self {
            SpeedHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(
                    move |game_state: &GameState, entity: Entity, speed: &mut Speed| {
                        systems::scripts::evaluate_speed_hook(
                            &script_id, game_state, entity, speed,
                        );
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<SpeedHook>) -> SpeedHook {
        Arc::new(move |game_state, entity, speed| {
            for hook in &hooks {
                hook(game_state, entity, speed);
            }
        })
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
pub struct EffectInstanceDefinition {
    pub effect_id: EffectId,
    pub lifetime: EffectLifetimeTemplate,
    #[serde(default)]
    pub end_condition: Option<EffectEndConditionDefinition>,
    #[serde(default)]
    pub one_shot: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum EffectEventFilterDefinition {
    TurnBoundary {
        entity: EffectEntiyReference,
        boundary: TurnBoundary,
    },
    Script {
        /// Event kinds this filter can match
        events: Vec<EventKindTag>,
        script: ScriptId,
    },
}

impl From<EffectEventFilterDefinition> for EffectEventFilter {
    fn from(def: EffectEventFilterDefinition) -> Self {
        match def {
            EffectEventFilterDefinition::TurnBoundary { entity, boundary } => {
                EffectEventFilter::TurnBoundary { entity, boundary }
            }
            EffectEventFilterDefinition::Script { events, script } => {
                EffectEventFilter::Script { events, script }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct EffectEndConditionDefinition {
    pub event: EffectEventFilterDefinition,
    pub on_trigger: EffectEndVerb,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum EffectEndVerb {
    /// Remove the effect instance when the event fires
    Remove,
    /// Re-roll the applying action's condition and remove the effect on a
    /// success (e.g. Hold Person's repeat save)
    RepeatApplyCondition,
}

impl EffectEndVerb {
    fn callback(&self) -> EventCallback {
        match self {
            EffectEndVerb::Remove => remove_effect_callback(),
            EffectEndVerb::RepeatApplyCondition => repeat_apply_condition_callback(),
        }
    }
}

impl From<EffectEndConditionDefinition> for EffectEndConditionTemplate {
    fn from(def: EffectEndConditionDefinition) -> Self {
        EffectEndConditionTemplate {
            event_filter: def.event.into(),
            callback: def.on_trigger.callback(),
        }
    }
}

fn remove_effect_callback() -> EventCallback {
    EventCallback::new(|game_state, _event, source| {
        let ListenerSource::EffectInstance { id, entity } = source else {
            return CallbackResult::None;
        };
        systems::effects::remove_effect(game_state, *entity, id);
        CallbackResult::None
    })
}

fn repeat_apply_condition_callback() -> EventCallback {
    EventCallback::new(move |game_state, event, source| {
        let ListenerSource::EffectInstance { id, entity } = source.clone() else {
            return CallbackResult::None;
        };

        debug!(
            "Checking end condition for effect instance {:?} on entity {:?} in response to event {:?}",
            id, entity, event
        );

        let instance = systems::effects::effects(&game_state.world, entity)
            .get(&id)
            .unwrap()
            .clone();
        let instance_id = id.clone();

        match &instance.action_resolution {
            ActionConditionResolution::Unconditional => { /* No check to repeat */ }

            ActionConditionResolution::Conditional {
                dc: dc @ (D20CheckDC::AttackRoll { .. } | D20CheckDC::Skill { .. }),
                ..
            } => {
                // TODO: Is this used anywhere?
                todo!(
                    "Repeat apply condition for attack roll or skill check DCs is not yet implemented"
                );
            }

            ActionConditionResolution::Conditional {
                dc: dc @ D20CheckDC::SavingThrow { .. },
                ..
            } => {
                let event = systems::d20::check(game_state, entity, dc);
                game_state.process_event_with_response_callback(
                    event,
                    EventCallback::new(move |game_state, event, _| {
                        if let EventKind::D20CheckResolved { result, dc, .. } = &event.kind {
                            if result.is_success(dc) {
                                systems::effects::remove_effect(game_state, entity, &instance_id);
                            }
                        }
                        CallbackResult::None
                    }),
                );
            }
        }

        CallbackResult::None
    })
}

impl From<EffectInstanceDefinition> for EffectInstanceTemplate {
    fn from(def: EffectInstanceDefinition) -> Self {
        let EffectInstanceDefinition {
            effect_id,
            lifetime,
            end_condition,
            one_shot,
        } = def;

        EffectInstanceTemplate {
            effect_id,
            lifetime,
            end_condition: end_condition.map(Into::into),
            one_shot,
        }
    }
}
