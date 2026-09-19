use std::{fmt::Display, sync::Arc};

use hecs::{Entity, World};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{
            Action, ActionCondition, ActionContext, ActionKind, ActionPayload,
            ActionPayloadComponent, ActionPhaseSpec, ActionTimeline, ActionUsabilityFunction,
            ActionVariant, DamageOnFailure, PayloadDelivery, PhaseRequirement, PhaseTargets,
            TargetUsabilityFunction,
        },
        id::{ActionId, ActionVariantId, ScriptId},
        resource::{RechargeRule, ResourceAmountMap},
    },
    entities::projectile::ProjectileTemplate,
    registry::{
        registry::Registry,
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
        serialize::{
            d20::{AttackRollDefinition, SavingThrowDefinition},
            dice::{DamageEquation, HealEquation},
            effect::EffectInstanceDefinition,
            parser::{Evaluable, EvaluationError},
            quantity::{LengthExpressionDefinition, VelocityExpressionDefinition},
            reaction::{ReactionBodyDefinition, ReactionTriggerDefinition},
            targeting::{AreaShapeDefinition, TargetingDefinition, TargetingKindDefinition},
            variables::{PARSER_VARIABLES, VariableMap},
        },
    },
    scripts::script::ScriptFunction,
    systems::{self, geometry::DisplacementTemplate},
};

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
pub struct ActionDefinition {
    pub id: ActionId,
    pub description: String,
    pub kind: ActionKindDefinition,
    /// The contexts in which this action can be performed. For example, a weapon
    /// attack can be performed in the context of a melee or ranged attack. If left
    /// empty, the default context is used, which is appropriate for actions like
    /// `Dash` or `Action Surge`, which don't require a specific context
    #[serde(default)]
    pub contexts: Vec<ActionContext>,
    pub targeting: TargetingDefinition,
    /// e.g. Action, Bonus Action, Reaction
    pub resource_cost: ResourceAmountMap,
    /// Optional cooldown for the action
    #[serde(default)]
    pub cooldown: Option<RechargeRule>,
    #[serde(default)]
    pub reaction_trigger: Option<ReactionTriggerDefinition>,
    pub timeline: ActionTimeline,
    #[serde(default)]
    pub usability: Option<ActionUsabilityDefinition>,
    /// Checked once a target has been chosen, for conditions that depend on who is
    /// being targeted (e.g. Brutal Strike needs Advantage against *this* target)
    #[serde(default)]
    pub target_usability: Option<TargetUsabilityDefinition>,
}

impl RegistryReferenceCollector for ActionDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        self.kind.collect_registry_references(collector);
        for resource in self.resource_cost.map.keys() {
            collector.add(RegistryReference::Resource(resource.clone()));
        }
        if let Some(reaction_trigger) = &self.reaction_trigger
            && let Some(script_id) = &reaction_trigger.script.script
        {
            collector.add(RegistryReference::Script(
                script_id.clone(),
                ScriptFunction::ReactionTrigger,
            ));
        }
        if let Some(usability) = &self.usability
            && let ActionUsabilityDefinition::Script(script_id) = usability
        {
            collector.add(RegistryReference::Script(
                script_id.clone(),
                ScriptFunction::ActionUsability,
            ));
        }
        if let Some(target_usability) = &self.target_usability
            && let TargetUsabilityDefinition::Script(script_id) = target_usability
        {
            collector.add(RegistryReference::Script(
                script_id.clone(),
                ScriptFunction::TargetUsability,
            ));
        }
    }
}

impl ActionKindDefinition {
    /// A phase gate reads the previous phase's single outcome, which is
    /// ambiguous when that phase hit multiple entities from one chosen target.
    /// Per-entity riders belong in that phase's payload components instead,
    /// where they share its per-entity condition resolution.
    ///
    /// Runs once every registry is loaded, since a variant's phases live in the
    /// variant registry but only make sense appended to the parent's.
    pub fn validate_phase_requirements(
        &self,
        id: &dyn Display,
        targeting: &TargetingDefinition,
        variants: &Registry<ActionVariantId, ActionVariant, ActionVariantDefinition>,
    ) {
        match self {
            ActionKindDefinition::Standard { phases } => {
                validate_phase_sequence(id, targeting, phases.iter())
            }

            ActionKindDefinition::Variant {
                phases,
                variants: variant_ids,
            } => {
                assert!(
                    !variant_ids.is_empty(),
                    "{}: a variant action must offer at least one variant",
                    id
                );

                for (index, variant_id) in variant_ids.iter().enumerate() {
                    assert!(
                        !variant_ids[..index].contains(variant_id),
                        "{}: duplicate variant `{}`",
                        id,
                        variant_id
                    );

                    let variant = &variants
                        .entries
                        .get(variant_id)
                        .expect("variant references are validated before phase requirements")
                        .definition;
                    validate_phase_sequence(id, targeting, phases.iter().chain(&variant.phases));
                }
            }

            ActionKindDefinition::Reaction { .. } => {}
        }
    }
}

fn validate_phase_sequence<'a>(
    id: &dyn Display,
    targeting: &TargetingDefinition,
    phases: impl Iterator<Item = &'a ActionPhaseDefinition>,
) {
    let mut previous: Option<&ActionPhaseDefinition> = None;
    for (index, phase) in phases.enumerate() {
        if phase.requires != PhaseRequirement::None {
            let Some(previous) = previous else {
                panic!(
                    "{}: the first phase cannot have `requires` if there is no previous phase",
                    id
                );
            };
            let previous_is_area = match &previous.targets {
                PhaseTargetsDefinition::Shape { .. } => true,
                PhaseTargetsDefinition::Inherited => matches!(
                    targeting,
                    TargetingDefinition::Custom(custom)
                        if matches!(custom.kind, TargetingKindDefinition::Area { .. })
                ),
                PhaseTargetsDefinition::Actor => false,
            };
            assert!(
                !previous_is_area,
                "{}: phase {} has `requires` but phase {} targets an area; gate outcomes \
                 are per chosen target, not per entity. Put per-entity riders in the \
                 previous phase's payload components",
                id,
                index,
                index - 1
            );
        }
        previous = Some(phase);
    }
}

impl From<ActionDefinition> for Action {
    fn from(value: ActionDefinition) -> Self {
        let contexts = if value.contexts.is_empty() {
            vec![ActionContext::default()]
        } else {
            value.contexts
        };

        Action {
            id: value.id,
            description: value.description,
            kind: value.kind.into(),
            resource_cost: value.resource_cost,
            targeting: value.targeting.function(),
            cooldown: value.cooldown,
            contexts,
            reaction_trigger: value.reaction_trigger.map(Into::into),
            timeline: value.timeline,
            usability: value.usability.map(|usability| usability.function()),
            target_usability: value
                .target_usability
                .map(|target_usability| target_usability.function()),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum DamageOnFailureDefinition {
    Half,
    Custom(DamageEquation),
}

impl From<DamageOnFailureDefinition> for DamageOnFailure {
    fn from(value: DamageOnFailureDefinition) -> Self {
        match value {
            DamageOnFailureDefinition::Half => DamageOnFailure::Half,
            DamageOnFailureDefinition::Custom(damage_equation) => {
                DamageOnFailure::Custom(damage_equation.function)
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ActionConditionDefinition {
    AttackRoll { attack_roll: AttackRollDefinition },
    SavingThrow { saving_throw: SavingThrowDefinition },
}

impl From<ActionConditionDefinition> for ActionCondition {
    fn from(value: ActionConditionDefinition) -> Self {
        match value {
            ActionConditionDefinition::AttackRoll { attack_roll } => {
                ActionCondition::AttackRoll(attack_roll.function)
            }
            ActionConditionDefinition::SavingThrow { saving_throw } => {
                ActionCondition::SavingThrow(saving_throw.function)
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum TrajectoryTemplateDefinition {
    Ray,
    Parabola,
}

#[derive(Clone, Serialize, Deserialize, Default, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum PayloadDeliveryDefinition {
    #[default]
    Immediate,
    Projectile {
        trajectory: TrajectoryTemplateDefinition,
        velocity: VelocityExpressionDefinition,
    },
}

impl From<PayloadDeliveryDefinition> for PayloadDelivery {
    fn from(value: PayloadDeliveryDefinition) -> Self {
        match value {
            PayloadDeliveryDefinition::Immediate => PayloadDelivery::Immediate,
            PayloadDeliveryDefinition::Projectile {
                trajectory,
                velocity,
            } => PayloadDelivery::Projectile {
                template: match trajectory {
                    TrajectoryTemplateDefinition::Ray => ProjectileTemplate::Ray {
                        velocity: velocity.evaluate_without_variables().unwrap(),
                    },
                    TrajectoryTemplateDefinition::Parabola => ProjectileTemplate::Parabola {
                        launch_velocity: velocity.evaluate_without_variables().unwrap(),
                    },
                },
            },
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum DisplacementTemplateDefinition {
    Teleport,
    Push {
        distance: LengthExpressionDefinition,
    },
    Pull {
        distance: LengthExpressionDefinition,
    },
}

impl Evaluable for DisplacementTemplateDefinition {
    type Output = DisplacementTemplate;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
        variables: &VariableMap,
    ) -> Result<Self::Output, EvaluationError> {
        match self {
            DisplacementTemplateDefinition::Teleport => Ok(DisplacementTemplate::Teleport),

            DisplacementTemplateDefinition::Push { distance } => Ok(DisplacementTemplate::Push {
                distance: distance.evaluate(world, entity, action_context, variables)?,
            }),

            DisplacementTemplateDefinition::Pull { distance } => Ok(DisplacementTemplate::Pull {
                distance: distance.evaluate(world, entity, action_context, variables)?,
            }),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ActionPayloadComponentDefinition {
    DamageFailure {
        damage: DamageEquation,
        damage_on_failure: Option<DamageOnFailureDefinition>,
    },
    Damage {
        damage: DamageEquation,
    },
    Effect {
        effect: EffectInstanceDefinition,
    },
    Healing {
        healing: HealEquation,
    },
    Reaction {
        reaction: ReactionBodyDefinition,
    },
    Displacement {
        displacement: DisplacementTemplateDefinition,
    },
}

impl From<ActionPayloadComponentDefinition> for ActionPayloadComponent {
    fn from(value: ActionPayloadComponentDefinition) -> Self {
        match value {
            ActionPayloadComponentDefinition::Damage { damage } => ActionPayloadComponent::Damage {
                damage: damage.function,
                damage_on_failure: None,
            },
            ActionPayloadComponentDefinition::DamageFailure {
                damage,
                damage_on_failure,
            } => ActionPayloadComponent::Damage {
                damage: damage.function,
                damage_on_failure: damage_on_failure.map(Into::into),
            },
            ActionPayloadComponentDefinition::Effect { effect } => {
                ActionPayloadComponent::Effect(effect.into())
            }
            ActionPayloadComponentDefinition::Healing { healing } => {
                ActionPayloadComponent::Healing(healing.function)
            }
            ActionPayloadComponentDefinition::Reaction { reaction } => {
                ActionPayloadComponent::Reaction(reaction.into())
            }
            ActionPayloadComponentDefinition::Displacement { displacement } => {
                ActionPayloadComponent::Displacement(Arc::new(
                    move |world, entity, action_context| {
                        displacement
                            .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                            .unwrap()
                    },
                ))
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
pub struct ActionPayloadDefinition {
    #[serde(default)]
    pub components: Vec<ActionPayloadComponentDefinition>,
    pub delivery: PayloadDeliveryDefinition,
}

/// How a phase derives its targets. Follow-up phases never re-select targets;
/// they either inherit the action's chosen targets or expand each of them into
/// the entities inside a shape centered on it (e.g. Ice Knife's 5 ft AoE damage).
#[derive(Clone, Serialize, Deserialize, Default, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum PhaseTargetsDefinition {
    #[default]
    Inherited,
    Shape(AreaShapeDefinition),
    Actor,
}

impl From<PhaseTargetsDefinition> for PhaseTargets {
    fn from(value: PhaseTargetsDefinition) -> Self {
        match value {
            PhaseTargetsDefinition::Inherited => PhaseTargets::Inherited,
            PhaseTargetsDefinition::Shape(shape) => {
                PhaseTargets::Shape(Arc::new(move |world, entity, action_context| {
                    shape
                        .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                        .unwrap()
                }))
            }
            PhaseTargetsDefinition::Actor => PhaseTargets::Actor,
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
pub struct ActionPhaseDefinition {
    /// Gate on the previous phase's condition outcome (per target)
    #[serde(default)]
    pub requires: PhaseRequirement,
    #[serde(default)]
    pub condition: Option<ActionConditionDefinition>,
    pub payload: ActionPayloadDefinition,
    #[serde(default)]
    pub targets: PhaseTargetsDefinition,
}

impl From<ActionPhaseDefinition> for ActionPhaseSpec {
    fn from(value: ActionPhaseDefinition) -> Self {
        ActionPhaseSpec {
            requires: value.requires,
            condition: value.condition.map_or(ActionCondition::None, Into::into),
            payload: ActionPayload::new(
                value
                    .payload
                    .components
                    .into_iter()
                    .map(Into::into)
                    .collect(),
                value.payload.delivery.into(),
            )
            .unwrap(),
            targets: value.targets.into(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
pub struct ActionVariantDefinition {
    pub id: ActionVariantId,
    pub description: String,
    /// Appended to the parent's shared `phases`
    pub phases: Vec<ActionPhaseDefinition>,
}

impl RegistryReferenceCollector for ActionVariantDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        collect_phase_references(&self.phases, collector);
    }
}

impl From<ActionVariantDefinition> for ActionVariant {
    fn from(value: ActionVariantDefinition) -> Self {
        Self {
            id: value.id,
            description: value.description,
            phases: value.phases.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ActionKindDefinition {
    Standard {
        phases: Vec<ActionPhaseDefinition>,
    },
    Variant {
        /// The phases every variant shares, run before the chosen variant's own
        #[serde(default)]
        phases: Vec<ActionPhaseDefinition>,
        variants: Vec<ActionVariantId>,
    },
    Reaction {
        body: ReactionBodyDefinition,
    },
}

impl From<ActionKindDefinition> for ActionKind {
    fn from(spec: ActionKindDefinition) -> Self {
        match spec {
            ActionKindDefinition::Standard { phases } => ActionKind::Standard {
                phases: phases.into_iter().map(Into::into).collect(),
            },

            ActionKindDefinition::Variant { phases, variants } => ActionKind::Variant {
                phases: phases.into_iter().map(Into::into).collect(),
                variants,
            },

            ActionKindDefinition::Reaction { body } => ActionKind::Reaction { body: body.into() },
        }
    }
}

fn collect_phase_references(phases: &[ActionPhaseDefinition], collector: &mut ReferenceCollector) {
    for component in phases.iter().flat_map(|phase| &phase.payload.components) {
        match component {
            ActionPayloadComponentDefinition::Effect { effect } => {
                collector.add(RegistryReference::Effect(effect.effect_id.clone()));
            }
            ActionPayloadComponentDefinition::Reaction { reaction } => {
                if let Some(script_id) = &reaction.script {
                    collector.add(RegistryReference::Script(
                        script_id.clone(),
                        ScriptFunction::ReactionBody,
                    ));
                }
            }
            _ => {}
        }
    }
}

impl RegistryReferenceCollector for ActionKindDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        match self {
            ActionKindDefinition::Standard { phases } => {
                collect_phase_references(phases, collector);
            }

            ActionKindDefinition::Variant { phases, variants } => {
                collect_phase_references(phases, collector);
                for variant in variants {
                    collector.add(RegistryReference::ActionVariant(variant.clone()));
                }
            }

            ActionKindDefinition::Reaction { body } => {
                if let Some(script_id) = &body.script {
                    collector.add(RegistryReference::Script(
                        script_id.clone(),
                        ScriptFunction::ReactionBody,
                    ));
                }
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ActionUsabilityDefinition {
    Script(ScriptId),
}

impl ActionUsabilityDefinition {
    pub fn function(&self) -> Arc<ActionUsabilityFunction> {
        match self {
            ActionUsabilityDefinition::Script(script_id) => Arc::new({
                let script_id = script_id.clone();
                move |game_state, entity, action_id, action_context| {
                    systems::scripts::evaluate_action_usability(
                        &script_id,
                        game_state,
                        entity,
                        action_id,
                        action_context,
                    )
                }
            }),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum TargetUsabilityDefinition {
    Script(ScriptId),
}

impl TargetUsabilityDefinition {
    pub fn function(&self) -> Arc<TargetUsabilityFunction> {
        match self {
            TargetUsabilityDefinition::Script(script_id) => Arc::new({
                let script_id = script_id.clone();
                move |game_state, entity, target, action_id, action_context| {
                    systems::scripts::evaluate_target_usability(
                        &script_id,
                        game_state,
                        entity,
                        target,
                        action_id,
                        action_context,
                    )
                }
            }),
        }
    }
}
