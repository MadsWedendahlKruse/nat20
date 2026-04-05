use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{
            Action, ActionCondition, ActionKind, ActionPayload, ActionTimeline, DamageOnFailure,
            PayloadDelivery,
        },
        id::ActionId,
        resource::{RechargeRule, ResourceAmountMap},
    },
    entities::projectile::ProjectileTemplate,
    registry::{
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
        serialize::{
            d20::{AttackRollDefinition, SavingThrowDefinition},
            dice::{DamageEquation, HealEquation},
            effect::EffectInstanceDefinition,
            quantity::VelocityExpressionDefinition,
            reaction::{ReactionBody, ReactionTrigger},
            targeting::TargetingDefinition,
        },
    },
    scripts::script::ScriptFunction,
};

#[derive(Clone, Serialize, Deserialize)]
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
    pub reaction_trigger: Option<ReactionTrigger>,
    #[serde(default)]
    pub timeline: Option<ActionTimeline>,
}

impl RegistryReferenceCollector for ActionDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        self.kind.collect_registry_references(collector);
        for resource in self.resource_cost.keys() {
            collector.add(RegistryReference::Resource(resource.clone()));
        }
        if let Some(reaction_trigger) = &self.reaction_trigger
            && let Some(script_id) = &reaction_trigger.script
        {
            collector.add(RegistryReference::Script(
                script_id.clone(),
                ScriptFunction::ReactionTrigger,
            ));
        }
    }
}

impl From<ActionDefinition> for Action {
    fn from(value: ActionDefinition) -> Self {
        Action {
            id: value.id,
            description: value.description,
            kind: value.kind.into(),
            resource_cost: value.resource_cost,
            targeting: value.targeting.function(),
            cooldown: value.cooldown,
            reaction_trigger: value.reaction_trigger.map(|trigger| trigger.function),
            timeline: value.timeline.map(|timeline_def| timeline_def.into()),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
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

#[derive(Clone, Serialize, Deserialize)]
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

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TrajectoryTemplateDefinition {
    Ray,
    Parabola,
}

#[derive(Clone, Serialize, Deserialize, Default)]
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

#[derive(Clone, Serialize, Deserialize)]
pub struct ActionPayloadDefinition {
    #[serde(default)]
    pub damage: Option<DamageEquation>,
    #[serde(default)]
    pub damage_on_failure: Option<DamageOnFailureDefinition>,
    #[serde(default)]
    pub healing: Option<HealEquation>,
    #[serde(default)]
    pub effect: Option<EffectInstanceDefinition>,
    #[serde(default)]
    pub delivery: PayloadDeliveryDefinition,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ActionKindDefinition {
    Standard {
        #[serde(default)]
        condition: Option<ActionConditionDefinition>,
        payload: ActionPayloadDefinition,
    },
    Composite {
        actions: Vec<ActionKindDefinition>,
    },
    Variants {
        variants: Vec<ActionId>,
    },
    Reaction {
        reaction: ReactionBody,
    },
}

impl From<ActionKindDefinition> for ActionKind {
    fn from(spec: ActionKindDefinition) -> Self {
        match spec {
            ActionKindDefinition::Standard { condition, payload } => ActionKind::Standard {
                condition: if let Some(condition) = condition {
                    condition.into()
                } else {
                    ActionCondition::None
                },
                payload: ActionPayload::new(
                    payload.damage.map(|eq| eq.function),
                    payload.damage_on_failure.map(|d| d.into()),
                    payload
                        .effect
                        .map(|effect_instance_definition| effect_instance_definition.into()),
                    payload.healing.map(|eq| eq.function),
                    payload.delivery.into(),
                )
                .unwrap(),
            },

            ActionKindDefinition::Composite { actions } => ActionKind::Composite {
                actions: actions.into_iter().map(ActionKind::from).collect(),
            },

            ActionKindDefinition::Variants { variants } => ActionKind::Variant { variants },

            ActionKindDefinition::Reaction { reaction } => ActionKind::Reaction {
                reaction: reaction.function,
            },
        }
    }
}

impl RegistryReferenceCollector for ActionKindDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        match self {
            ActionKindDefinition::Standard { payload, .. } => {
                if let Some(effect) = &payload.effect {
                    collector.add(RegistryReference::Effect(effect.effect_id.clone()));
                }
            }
            ActionKindDefinition::Composite { actions } => {
                for action in actions {
                    action.collect_registry_references(collector);
                }
            }
            ActionKindDefinition::Variants { .. } => {
                // TODO: Although the variant references an ActionId, that might
                // refer to the action associated with a spell, so the action ID
                // would not be present in the action registry directly. So how do
                // we validate that?
            }
            ActionKindDefinition::Reaction { reaction } => {
                if let Some(script_id) = &reaction.script {
                    collector.add(RegistryReference::Script(
                        script_id.clone(),
                        ScriptFunction::ReactionBody,
                    ));
                }
            }
        }
    }
}
