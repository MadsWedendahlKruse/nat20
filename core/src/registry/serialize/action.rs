use std::sync::Arc;

use hecs::{Entity, World};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{
            Action, ActionCondition, ActionContext, ActionKind, ActionPayload,
            ActionPayloadComponent, ActionPhaseSpec, ActionTimeline, ActionUsabilityFunction,
            DamageOnFailure, PayloadDelivery, PhaseRequirement, PhaseTargets,
        },
        id::{ActionId, ScriptId},
        resource::{RechargeRule, ResourceAmountMap},
    },
    entities::projectile::ProjectileTemplate,
    registry::{
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
    }
}

impl ActionDefinition {
    /// A phase gate reads the previous phase's single outcome, which is
    /// ambiguous when that phase hit multiple entities from one chosen target.
    /// Per-entity riders belong in that phase's payload components instead,
    /// where they share its per-entity condition resolution.
    fn validate_phase_requirements(&self) {
        let ActionKindDefinition::Standard { phases } = &self.kind else {
            return;
        };
        for (index, phase) in phases.iter().enumerate() {
            if phase.requires == PhaseRequirement::None {
                continue;
            }
            assert!(
                index > 0,
                "{}: the first phase cannot have `requires` — there is no previous phase",
                self.id
            );
            let previous_is_area = match &phases[index - 1].targets {
                PhaseTargetsDefinition::Shape { .. } => true,
                PhaseTargetsDefinition::Inherited => matches!(
                    &self.targeting,
                    TargetingDefinition::Custom(custom)
                        if matches!(custom.kind, TargetingKindDefinition::Area { .. })
                ),
            };
            assert!(
                !previous_is_area,
                "{}: phase {} has `requires` but phase {} targets an area; gate outcomes \
                 are per chosen target, not per entity. Put per-entity riders in the \
                 previous phase's payload components",
                self.id,
                index,
                index - 1
            );
        }
    }
}

impl From<ActionDefinition> for Action {
    fn from(value: ActionDefinition) -> Self {
        value.validate_phase_requirements();
        Action {
            id: value.id,
            description: value.description,
            kind: value.kind.into(),
            resource_cost: value.resource_cost,
            targeting: value.targeting.function(),
            cooldown: value.cooldown,
            reaction_trigger: value.reaction_trigger.map(Into::into),
            timeline: value.timeline,
            usability: value.usability.map(|usability| usability.function()),
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
#[serde(rename_all = "snake_case")]
pub enum ActionKindDefinition {
    Standard { phases: Vec<ActionPhaseDefinition> },
    Variants { variants: Vec<ActionId> },
    Reaction { body: ReactionBodyDefinition },
}

impl From<ActionKindDefinition> for ActionKind {
    fn from(spec: ActionKindDefinition) -> Self {
        match spec {
            ActionKindDefinition::Standard { phases } => ActionKind::Standard {
                phases: phases.into_iter().map(Into::into).collect(),
            },

            ActionKindDefinition::Variants { variants } => ActionKind::Variant { variants },

            ActionKindDefinition::Reaction { body } => ActionKind::Reaction { body: body.into() },
        }
    }
}

impl RegistryReferenceCollector for ActionKindDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        match self {
            ActionKindDefinition::Standard { phases } => {
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

            ActionKindDefinition::Variants { .. } => {
                // TODO: Although the variant references an ActionId, that might
                // refer to the action associated with a spell, so the action ID
                // would not be present in the action registry directly. So how do
                // we validate that?
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
                move |game_state, entity, action_context| {
                    systems::scripts::evaluate_action_usability(
                        &script_id,
                        game_state,
                        entity,
                        action_context,
                    )
                }
            }),
        }
    }
}
