use hecs::{Entity, World};
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use std::{collections::HashMap, sync::Arc};
use tracing::debug;

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::{
            action::{ActionConditionResolution, ActionContext, ActionKindResult, ActionResult},
            targeting::TargetInstance,
        },
        d20::{D20CheckKey, D20CheckMap, D20CheckOutcome},
        damage::{
            AttackRange, AttackRollTemplate, AttackSource, DamageMitigationEffect,
            DamageMitigationResult, DamageResistances, DamageRollResult,
        },
        effects::{
            effect::{
                Effect, EffectEndConditionTemplate, EffectEntiyReference, EffectEventFilter,
                EffectInstance, EffectInstanceTemplate, EffectKind, EffectLifetimeTemplate,
            },
            hooks::{
                ActionHook, ActionResultHook, ArmorClassHook, AttackRollHook, AttackedHook,
                DamageRollResultHook, DeathHook, PostDamageMitigationHook, PreDamageMitigationHook,
                ResourceCostHook,
            },
        },
        health::hit_points::{HitPoints, TemporaryHitPoints},
        id::{ActionId, EffectId, ResourceId, ScriptId},
        items::equipment::{armor::ArmorClass, loadout::Loadout, weapon::WeaponKind},
        modifier::{KeyedModifiable, Modifiable, ModifierSource},
        resource::{ResourceAmount, ResourceAmountMap, ResourceMap},
        saving_throw::SavingThrowSet,
        skill::SkillSet,
        speed::Speed,
        spells::spellbook::Spellbook,
        time::{TimeDuration, TurnBoundary},
    },
    engine::{
        action_prompt::ActionData,
        event::{CallbackResult, EventCallback, EventKind, ListenerSource},
        game_state::GameState,
    },
    registry::{
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
        serialize::{
            dice::HealEquation,
            modifier::{
                AbilityModifierProvider, ArmorClassModifierProvider, AttackRollModifier,
                AttackSourceDefinition, D20CheckModifierProvider, D20Modifier,
                DamageResistanceProvider, SavingThrowModifierProvider, SkillModifierProvider,
                SpeedModifier, SpeedModifierProvider,
            },
            quantity::{LengthExpressionDefinition, TimeExpressionDefinition},
        },
    },
    scripts::{
        script::ScriptFunction,
        script_api::{
            ScriptActionPerformedView, ScriptActionResultView, ScriptActionView,
            ScriptDamageMitigationResult, ScriptDamageRollResult, ScriptEffectView,
            ScriptEntityView, ScriptOptionalEntityView, ScriptResourceCost,
        },
    },
    systems::{self, d20::D20CheckDCKind},
};

// TODO: Should this be it's own time module?
#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EffectDefinition {
    pub id: EffectId,
    pub kind: EffectKind,
    pub description: String,

    /// If present, this effect replaces another effect with the given id
    #[serde(default)]
    pub replaces: Option<EffectId>,

    #[serde(default)]
    pub children: Vec<EffectId>,

    /// Simple effect modifiers like:
    /// - Ability score changes
    /// - Skill modifiers
    /// - Saving throw modifiers
    /// - Damage resistances
    /// - Resource changes
    #[serde(default)]
    pub modifiers: Vec<EffectModifier>,

    /// Other hooks can be either pattern-based or script-based
    #[serde(default)]
    pub pre_attack_roll: Vec<AttackRollHookDefinition>,
    // #[serde(default)]
    // pub post_attack_roll: Vec<AttackRollResultHookDef>,
    #[serde(default)]
    pub on_attacked: Vec<AttackedHookDefinition>,
    #[serde(default)]
    pub on_armor_class: Vec<ArmorClassHookDefinition>,

    // #[serde(default)]
    // pub pre_damage_roll: Vec<DamageRollHookDef>,
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
}

impl From<EffectDefinition> for Effect {
    fn from(definition: EffectDefinition) -> Self {
        let effect_id = definition.id.clone();

        let mut effect = Effect::new(effect_id.clone(), definition.kind, definition.description);
        effect.replaces = definition.replaces;
        effect.children = definition.children;

        // 1. Simple persistent modifiers
        // Build on_apply from all modifiers
        {
            let effect_id = effect_id.clone();
            let modifiers = definition.modifiers.clone();
            effect.on_apply = Arc::new(
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
            );
        }

        // Build on_unapply from the *same* modifiers, but different phase
        {
            let effect_id = effect_id.clone();
            let modifiers_for_unapply = definition.modifiers;
            effect.on_unapply = Arc::new(move |game_state: &mut GameState, entity: Entity| {
                for modifier in &modifiers_for_unapply {
                    modifier.evaluate(game_state, entity, &effect_id, EffectPhase::Unapply, None);
                }
            });
        }

        // 2. Hook-based modifiers
        // Build pre_attack_roll hooks
        {
            let hooks = collect_effect_hooks(&definition.pre_attack_roll, &effect_id);
            effect.pre_attack_roll = AttackRollHookDefinition::combine_hooks(hooks);
        }
        // Build on_attacked hooks
        {
            let hooks = collect_effect_hooks(&definition.on_attacked, &effect_id);
            effect.on_attacked = AttackedHookDefinition::combine_hooks(hooks);
        }

        // Build post_damage_roll hooks
        {
            let hooks = collect_effect_hooks(&definition.post_damage_roll, &effect_id);
            effect.post_damage_roll = DamageRollResultHookDefinition::combine_hooks(hooks);
        }

        // Build pre_damage_taken hooks
        {
            let hooks = collect_effect_hooks(&definition.pre_damage_mitigation, &effect_id);
            effect.pre_damage_mitigation = PreDamageMitigationHookDefinition::combine_hooks(hooks);
        }
        // Build post_damage_mitigation hooks
        {
            let hooks = collect_effect_hooks(&definition.post_damage_mitigation, &effect_id);
            effect.post_damage_mitigation =
                PostDamageMitigationHookDefinition::combine_hooks(hooks);
        }

        // Build armor class hooks
        {
            let hooks = collect_effect_hooks(&definition.on_armor_class, &effect_id);
            effect.on_armor_class = ArmorClassHookDefinition::combine_hooks(hooks);
        }

        // Build resource cost hooks
        {
            let hooks = collect_effect_hooks(&definition.on_resource_cost, &effect_id);
            effect.on_resource_cost = ResourceCostHookDefinition::combine_hooks(hooks);
        }

        // Build on_action hooks
        {
            let hooks = collect_effect_hooks(&definition.on_action, &effect_id);
            effect.on_action = ActionHookDefinition::combine_hooks(hooks);
        }

        // Build on_action_result hooks
        {
            let hooks = collect_effect_hooks(&definition.on_action_result, &effect_id);
            effect.on_action_result = ActionResultHookDefinition::combine_hooks(hooks);
        }

        // Build on_death hooks
        {
            let hooks = collect_effect_hooks(&definition.on_death, &effect_id);
            effect.on_death = DeathHookDefinition::combine_hooks(hooks);
        }

        effect
    }
}

impl RegistryReferenceCollector for EffectDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        if let Some(replaces) = &self.replaces {
            collector.add(RegistryReference::Effect(replaces.clone()));
        }
        for modifier in &self.modifiers {
            match modifier {
                EffectModifier::Resource { resource, .. } => {
                    collector.add(RegistryReference::Resource(resource.clone()));
                }
                _ => { /* No references to collect */ }
            }
        }
        for hook in &self.pre_attack_roll {
            match hook {
                AttackRollHookDefinition::Script { script } => {
                    collector.add(RegistryReference::Script(
                        script.clone(),
                        ScriptFunction::AttackRollHook,
                    ));
                }
                _ => { /* No references to collect */ }
            }
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
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
        attack_roll_modifier: AttackRollModifier,
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
        break_concentration: bool,
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
                // TODO: Not the prettiest solution, but needed to avoid double mutable borrows
                match attack_source {
                    Some(attack_source) => {
                        // Specific source means only apply to that source's attack rolls
                        match attack_source.source {
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

                    None => {
                        // No source means all attack rolls
                        {
                            let mut loadout = systems::helpers::get_component_mut::<Loadout>(
                                &mut game_state.world,
                                entity,
                            );
                            for weapon_kind in
                                &[WeaponKind::Melee, WeaponKind::Ranged, WeaponKind::Unarmed]
                            {
                                Self::apply_attack_roll_modifier(
                                    loadout.attack_roll_template_mut(weapon_kind),
                                    modifier,
                                    &source,
                                    phase,
                                );
                            }
                        }
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
                            .roll()
                            .subtotal as u32;
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
            } => {
                if *break_concentration && phase == EffectPhase::Apply {
                    systems::spells::break_concentration(game_state, entity);
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
        match phase {
            EffectPhase::Apply => match modifier.modifier {
                D20Modifier::Flat(delta) => {
                    for kind in &modifier.kind {
                        modifiable.add_modifier(kind, source.clone(), delta);
                    }
                }
                D20Modifier::Advantage(advantage_type) => {
                    for kind in &modifier.kind {
                        modifiable.add_advantage(kind, advantage_type, source.clone());
                    }
                }
                D20Modifier::ForceOutcome(force_outcome) => {
                    for kind in &modifier.kind {
                        modifiable.set_forced_outcome(kind, source.clone(), force_outcome);
                    }
                }
            },
            EffectPhase::Unapply => {
                for kind in &modifier.kind {
                    modifiable.remove_modifier(kind, &source);
                    modifiable.remove_advantage(kind, &source);
                    modifiable.clear_forced_outcome(kind);
                }
            }
        }
    }

    fn apply_attack_roll_modifier(
        template: &mut AttackRollTemplate,
        modifier: &AttackRollModifier,
        source: &ModifierSource,
        phase: EffectPhase,
    ) {
        match (modifier, phase) {
            (AttackRollModifier::FlatBonus(bonus), EffectPhase::Apply) => {
                template.d20_check.add_modifier(source.clone(), *bonus);
            }
            (AttackRollModifier::FlatBonus(_), EffectPhase::Unapply) => {
                template.d20_check.remove_modifier(source);
            }

            (AttackRollModifier::Advantage(advantage), EffectPhase::Apply) => {
                template
                    .d20_check
                    .advantage_tracker_mut()
                    .add(*advantage, source.clone());
            }
            (AttackRollModifier::Advantage(_), EffectPhase::Unapply) => {
                template.d20_check.advantage_tracker_mut().remove(source);
            }

            (AttackRollModifier::CritThreshold(threshold), EffectPhase::Apply) => {
                template.add_crit_threshold_reduction(source.clone(), *threshold);
            }
            (AttackRollModifier::CritThreshold(_), EffectPhase::Unapply) => {
                template.remove_crit_threshold_reduction(source);
            }
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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AttackRollHookDefinition {
    Modifier {
        #[serde(default)]
        source: Option<AttackSource>,
        #[serde(default)]
        range: Option<AttackRange>,
        modifier: AttackRollModifier,
    },
    Script {
        script: ScriptId,
    },
}

impl HookEffect<AttackRollHook> for AttackRollHookDefinition {
    fn build_hook(&self, effect: &EffectId) -> AttackRollHook {
        match self {
            AttackRollHookDefinition::Modifier {
                source,
                range,
                modifier,
            } => {
                let modifier_source = ModifierSource::Effect(effect.clone());
                Arc::new({
                    let source = source.clone();
                    let range = range.clone();
                    let modifier = modifier.clone();
                    move |_world, _entity, attack_roll| {
                        if let Some(source) = source
                            && attack_roll.source != source
                        {
                            // Only apply if the attack source matches
                            return;
                        }
                        if let Some(range) = range
                            && attack_roll.range != range
                        {
                            // Only apply if the damage source matches
                            return;
                        }

                        match modifier {
                            AttackRollModifier::FlatBonus(bonus) => {
                                attack_roll
                                    .d20_check
                                    .add_modifier(modifier_source.clone(), bonus);
                            }
                            AttackRollModifier::Advantage(advantage) => {
                                attack_roll
                                    .d20_check
                                    .advantage_tracker_mut()
                                    .add(advantage, modifier_source.clone());
                            }
                            AttackRollModifier::CritThreshold(threshold) => {
                                attack_roll.reduce_crit_threshold(threshold);
                            }
                        }
                    }
                })
            }

            AttackRollHookDefinition::Script { script } => {
                todo!("Implement script-based AttackRollHook")
            }
        }
    }

    fn combine_hooks(hooks: Vec<AttackRollHook>) -> AttackRollHook {
        Arc::new(move |world, entity, attack_roll| {
            for hook in &hooks {
                hook(world, entity, attack_roll);
            }
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AttackedHookDefinition {
    Modifier {
        modifier: AttackRollModifier,
    },
    AutoCrit {
        auto_crit_distance: LengthExpressionDefinition,
    },
}

impl HookEffect<AttackedHook> for AttackedHookDefinition {
    fn build_hook(&self, effect: &EffectId) -> AttackedHook {
        match self {
            AttackedHookDefinition::Modifier { modifier } => {
                let modifier_source = ModifierSource::Effect(effect.clone());
                Arc::new({
                    let modifier = modifier.clone();
                    move |_world, _victim, _attacker, attack_roll| match modifier {
                        AttackRollModifier::FlatBonus(bonus) => {
                            attack_roll
                                .d20_check
                                .add_modifier(modifier_source.clone(), bonus);
                        }
                        AttackRollModifier::Advantage(advantage) => {
                            attack_roll
                                .d20_check
                                .advantage_tracker_mut()
                                .add(advantage, modifier_source.clone());
                        }
                        AttackRollModifier::CritThreshold(threshold) => {
                            attack_roll.reduce_crit_threshold(threshold);
                        }
                    }
                })
            }

            AttackedHookDefinition::AutoCrit { auto_crit_distance } => {
                let effect_id = effect.clone();
                let distance_expression = auto_crit_distance.clone();
                Arc::new(
                    move |world: &World,
                          victim: Entity,
                          attacker: Entity,
                          attack_roll: &mut crate::components::damage::AttackRoll| {
                        let distance = distance_expression
                            .evaluate_without_variables().unwrap();

                        let distance_between = systems::geometry::distance_between_entities(world, victim, attacker).unwrap();

                        if distance_between <= distance {
                            attack_roll.d20_check.set_forced_outcome(ModifierSource::Effect(effect_id.clone()), D20CheckOutcome::CriticalSuccess);
                        }
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<AttackedHook>) -> AttackedHook {
        Arc::new(move |world, victim, attacker, attack_roll| {
            for hook in &hooks {
                hook(world, victim, attacker, attack_roll);
            }
        })
    }
}

pub enum DamageRollHookDefinition {
    // …
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
                    move |world: &World,
                          entity: Entity,
                          damage_roll_result: &mut DamageRollResult| {
                        let entity_view = ScriptEntityView::new_from_world(world, entity);
                        let script_damage_roll_result =
                            ScriptDamageRollResult::take_from(damage_roll_result);

                        systems::scripts::evaluate_damage_roll_result_hook(
                            &script_id,
                            &entity_view,
                            &script_damage_roll_result,
                        );

                        *damage_roll_result = script_damage_roll_result.into_inner();
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<DamageRollResultHook>) -> DamageRollResultHook {
        Arc::new(
            move |world: &World, entity: Entity, damage_roll_result: &mut DamageRollResult| {
                for hook in &hooks {
                    hook(world, entity, damage_roll_result);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
                move |_world, _entity, armor_class| {
                    armor_class
                        .add_modifier(ModifierSource::Effect(effect.clone()), modifier.delta);
                }
            }),

            ArmorClassHookDefinition::Script { script } => {
                let effect_id = effect.clone();
                let script_id = script.clone();
                Arc::new(
                    move |world: &World, entity: Entity, armor_class: &mut ArmorClass| {
                        let entity_view = ScriptEntityView::new_from_world(world, entity);

                        let modifier =
                            systems::scripts::evaluate_armor_class_hook(&script_id, &entity_view);
                        armor_class
                            .add_modifier(ModifierSource::Effect(effect_id.clone()), modifier);
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<ArmorClassHook>) -> ArmorClassHook {
        Arc::new(move |world, entity, armor_class| {
            for hook in &hooks {
                hook(world, entity, armor_class);
            }
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ActionHookDefinition {
    Script { script: ScriptId },
}

impl HookEffect<ActionHook> for ActionHookDefinition {
    fn build_hook(&self, _effect: &EffectId) -> ActionHook {
        match self {
            ActionHookDefinition::Script { script } => {
                let script_id = script.clone();
                Arc::new(move |world: &mut World, action_data: &ActionData| {
                    let action_view = ScriptActionView::from(action_data);

                    let entity_view = ScriptEntityView::take_from_world(world, action_data.actor);

                    systems::scripts::evalute_action_hook(&script_id, &action_view, &entity_view);

                    // Replace the entity in the world with the modified one
                    entity_view.replace_in_world(world);
                })
            }
        }
    }

    fn combine_hooks(hooks: Vec<ActionHook>) -> ActionHook {
        Arc::new(move |world: &mut World, action_data: &ActionData| {
            for hook in &hooks {
                hook(world, action_data);
            }
        })
    }
}

#[derive(Clone, Serialize, Deserialize)]
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
                          results: &[ActionResult]| {
                        let script_results: Vec<ScriptActionResultView> = results
                            .iter()
                            .filter_map(|result| {
                                if let TargetInstance::Entity(target_entity) = &result.target {
                                    Some(ScriptActionResultView::from_action_result(
                                        action_data.actor,
                                        *target_entity,
                                        &result.kind,
                                    ))
                                } else {
                                    None
                                }
                            })
                            .collect();

                        if script_results.is_empty() {
                            // If there are no results, don't run the script
                            return;
                        }

                        let action_performed_view = ScriptActionPerformedView::new(
                            ScriptActionView::from(action_data),
                            script_results,
                        );

                        let mut entity_view = ScriptEntityView::take_from_world(
                            &mut game_state.world,
                            action_data.actor,
                        );

                        systems::scripts::evalute_action_result_hook(
                            &script_id,
                            &action_performed_view,
                            &entity_view,
                        );

                        let effect_applications = entity_view.take_effect_applications();

                        let action_resolution = if let Some(result) = results.first()
                            && let ActionKindResult::Standard(outcome) = &result.kind
                            && let Some(resolution) = outcome.resolution()
                        {
                            resolution.clone()
                        } else {
                            ActionConditionResolution::Unconditional
                        };

                        for application in effect_applications {
                            // TODO: Do we need to check this?
                            // let already_has_effect =
                            //     systems::effects::effects(&game_state.world, action_data.actor)
                            //         .values()
                            //         .any(|instance| instance.effect_id == application.effect_id);

                            // if already_has_effect {
                            //     continue;
                            // }

                            systems::effects::add_effect_template(
                                game_state,
                                action_data.actor,
                                action_data.actor,
                                ModifierSource::Action(action_data.action_id.clone()),
                                &EffectInstanceTemplate {
                                    effect_id: application.effect_id,
                                    lifetime: application.lifetime,
                                    end_condition: None,
                                },
                                Some(&action_data.context),
                                action_resolution.clone(),
                            );
                        }

                        entity_view.replace_in_world(&mut game_state.world);
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<ActionResultHook>) -> ActionResultHook {
        Arc::new(
            move |game_state: &mut GameState,
                  action_data: &ActionData,
                  results: &[ActionResult]| {
                for hook in &hooks {
                    hook(game_state, action_data, results);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
                    move |world: &World,
                          entity: Entity,
                          action: &ActionId,
                          context: &ActionContext,
                          resource_costs: &mut ResourceAmountMap| {
                        // Evaluate the script, which can modify the shared resource costs.
                        let action_view = ScriptActionView::new(
                            action,
                            entity,
                            context,
                            // Move out (no clone), leaving an empty map behind temporarily
                            ScriptResourceCost::take_from(resource_costs),
                            // TODO: Not sure what to do about targets here
                            Vec::new(),
                        );

                        let entity_view = ScriptEntityView::new_from_world(world, entity);

                        systems::scripts::evaluate_resource_cost_hook(
                            &script_id,
                            &action_view,
                            &entity_view,
                        );

                        // Move back out (no clone)
                        *resource_costs = action_view.resource_cost.into_inner();
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<ResourceCostHook>) -> ResourceCostHook {
        Arc::new(
            move |world: &World,
                  entity: Entity,
                  action: &ActionId,
                  context: &ActionContext,
                  resource_costs: &mut ResourceAmountMap| {
                for hook in &hooks {
                    hook(world, entity, action, context, resource_costs);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
                    move |world: &World,
                          entity: Entity,
                          effect: &EffectInstance,
                          damage_roll_result: &mut DamageRollResult| {
                        let entity_view = ScriptEntityView::new_from_world(world, entity);
                        let effect_view = ScriptEffectView::from(effect);
                        let script_damage_roll_result =
                            ScriptDamageRollResult::take_from(damage_roll_result);

                        systems::scripts::evaluate_pre_damage_mitigation_hook(
                            &script_id,
                            &entity_view,
                            &effect_view,
                            &script_damage_roll_result,
                        );

                        *damage_roll_result = script_damage_roll_result.into_inner();
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<PreDamageMitigationHook>) -> PreDamageMitigationHook {
        Arc::new(
            move |world: &World,
                  entity: Entity,
                  effect: &EffectInstance,
                  damage_roll_result: &mut DamageRollResult| {
                for hook in &hooks {
                    hook(world, entity, effect, damage_roll_result);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
                    move |world: &World,
                          entity: Entity,
                          damage_mitigation_result: &mut DamageMitigationResult| {
                        let entity_view = ScriptEntityView::new_from_world(world, entity);
                        let script_damage_mitigation_result =
                            ScriptDamageMitigationResult::take_from(damage_mitigation_result);

                        systems::scripts::evaluate_post_damage_mitigation_hook(
                            &script_id,
                            &entity_view,
                            &script_damage_mitigation_result,
                        );

                        *damage_mitigation_result = script_damage_mitigation_result.into_inner();
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<PostDamageMitigationHook>) -> PostDamageMitigationHook {
        Arc::new(
            move |world: &World,
                  entity: Entity,
                  damage_mitigation_result: &mut DamageMitigationResult| {
                for hook in &hooks {
                    hook(world, entity, damage_mitigation_result);
                }
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
                    move |world: &mut World,
                          victim: Entity,
                          killer: Option<Entity>,
                          applier: Option<Entity>| {
                        // 1) Take each distinct entity exactly once.
                        let mut taken: HashMap<Entity, ScriptEntityView> = HashMap::new();

                        take_entity_view_once(world, &mut taken, victim);
                        if let Some(killer_entity) = killer {
                            take_entity_view_once(world, &mut taken, killer_entity);
                        }
                        if let Some(applier_entity) = applier {
                            take_entity_view_once(world, &mut taken, applier_entity);
                        }

                        // 2) Build script inputs as temporary views (clones are OK here),
                        //    but ensure they drop before we replace back into the world.
                        {
                            let victim_view: &ScriptEntityView =
                                taken.get(&victim).expect("Victim must be taken");

                            let killer_view =
                                ScriptOptionalEntityView::from(killer.and_then(|e| taken.get(&e)));

                            let applier_view =
                                ScriptOptionalEntityView::from(applier.and_then(|e| taken.get(&e)));

                            systems::scripts::evaluate_death_hook(
                                &script_id,
                                victim_view,
                                &killer_view,
                                &applier_view,
                            );

                            // killer_view and applier_view drop at the end of this scope.
                        }

                        // 3) Replace back exactly once per entity (no aliasing ambiguity).
                        //    Drain ensures we move the owned ScriptEntityView out.
                        for (_entity, view) in taken.drain() {
                            view.replace_in_world(world);
                        }
                    },
                )
            }
        }
    }

    fn combine_hooks(hooks: Vec<DeathHook>) -> DeathHook {
        Arc::new(
            move |world: &mut World,
                  victim: Entity,
                  killer: Option<Entity>,
                  applier: Option<Entity>| {
                for hook in &hooks {
                    hook(world, victim, killer, applier);
                }
            },
        )
    }
}

fn take_entity_view_once(
    world: &mut World,
    taken: &mut HashMap<Entity, ScriptEntityView>,
    entity: Entity,
) {
    taken
        .entry(entity)
        .or_insert_with(|| ScriptEntityView::take_from_world(world, entity));
}

#[derive(Clone, Serialize, Deserialize)]
pub struct EffectInstanceDefinition {
    pub effect_id: EffectId,
    pub lifetime: EffectLifetimeTemplate,
    #[serde(default)]
    pub end_condition: Option<EffectEndConditionDefinition>,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EffectEventFilterDefinition {
    TurnBoundary {
        entity: EffectEntiyReference,
        boundary: TurnBoundary,
    },
    Script {
        script: ScriptId,
    },
}

impl From<EffectEventFilterDefinition> for EffectEventFilter {
    fn from(def: EffectEventFilterDefinition) -> Self {
        match def {
            EffectEventFilterDefinition::TurnBoundary { entity, boundary } => {
                EffectEventFilter::TurnBoundary { entity, boundary }
            }

            EffectEventFilterDefinition::Script { script } => {
                todo!("Implement script-based EffectEventFilter")
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum EffectEndConditionDefinition {
    Script {
        script: ScriptId,
    },
    Event {
        event: EffectEventFilterDefinition,
        callback: EventCallbackDefinition,
    },
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EventCallbackDefinition {
    RepeatApplyCondition,
}

impl From<EffectInstanceDefinition> for EffectInstanceTemplate {
    fn from(def: EffectInstanceDefinition) -> Self {
        let EffectInstanceDefinition {
            effect_id,
            lifetime,
            end_condition,
        } = def;

        let end_condition = match end_condition {
            Some(EffectEndConditionDefinition::Script { script }) => {
                todo!("Implement script-based EffectEndCondition")
            }

            // TODO: Indentation is going out of control here
            Some(EffectEndConditionDefinition::Event { event, callback }) => {
                Some(EffectEndConditionTemplate {
                    event_filter: event.into(),
                    callback: match callback {
                        EventCallbackDefinition::RepeatApplyCondition => {
                            EventCallback::new(move |game_state, event, source| {
                                if let ListenerSource::EffectInstance { id, entity } =
                                    source.clone()
                                {
                                    debug!(
                                        "Checking end condition for effect instance {:?} on entity {:?} in response to event {:?}",
                                        id, entity, event
                                    );

                                    let instance =
                                        systems::effects::effects(&game_state.world, entity)
                                            .get(&id)
                                            .unwrap()
                                            .clone();
                                    let instance_id = id.clone();

                                    match &instance.action_resolution {
                                        ActionConditionResolution::Unconditional => { /* No check to repeat */
                                        }

                                        ActionConditionResolution::AttackRoll {
                                            attack_roll,
                                            armor_class,
                                        } => todo!(),

                                        ActionConditionResolution::SavingThrow {
                                            saving_throw_dc,
                                            saving_throw_result,
                                        } => {
                                            let event = systems::d20::check(
                                                game_state,
                                                entity,
                                                &D20CheckDCKind::SavingThrow(
                                                    saving_throw_dc.clone(),
                                                ),
                                            );
                                            game_state.process_event_with_response_callback(
                                                event,
                                                EventCallback::new({
                                                    move |game_state, event, _| {
                                                        if let EventKind::D20CheckResolved(
                                                            _,
                                                            result,
                                                            dc,
                                                        ) = &event.kind
                                                        {
                                                            if result.is_success(dc) {
                                                                systems::effects::remove_effect(
                                                                    game_state,
                                                                    entity,
                                                                    &instance_id,
                                                                );
                                                            }
                                                        }

                                                        CallbackResult::None
                                                    }
                                                }),
                                            );
                                        }
                                    }
                                }

                                CallbackResult::None
                            })
                        }
                    },
                })
            }
            None => None,
        };

        EffectInstanceTemplate {
            effect_id,
            lifetime,
            end_condition,
        }
    }
}
