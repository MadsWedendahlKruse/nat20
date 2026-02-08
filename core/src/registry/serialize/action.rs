use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{Action, ActionCondition, ActionKind, ActionPayload, DamageOnFailure},
        effects::effect::EffectInstanceTemplate,
        id::{ActionId, ScriptId},
        resource::{RechargeRule, ResourceAmountMap},
    },
    registry::{
        registry_validation::{ReferenceCollector, RegistryReference, RegistryReferenceCollector},
        serialize::{
            d20::{AttackRollDefinition, SavingThrowDefinition},
            dice::{DamageEquation, HealEquation},
            effect::EffectInstanceDefinition,
            targeting::TargetingDefinition,
        },
    },
    scripts::script::ScriptFunction,
};

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DamageOnFailureDefinition {
    Half,
    Custom(DamageEquation),
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ActionConditionDefinition {
    AttackRoll {
        attack_roll: AttackRollDefinition,
        #[serde(default)]
        damage_on_miss: Option<DamageOnFailureDefinition>,
    },
    SavingThrow {
        saving_throw: SavingThrowDefinition,
        #[serde(default)]
        damage_on_save: Option<DamageOnFailureDefinition>,
    },
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ActionPayloadDefinition {
    #[serde(default)]
    pub damage: Option<DamageEquation>,
    #[serde(default)]
    pub healing: Option<HealEquation>,
    #[serde(default)]
    pub effect: Option<EffectInstanceDefinition>,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ActionKindDefinition {
    Standard {
        #[serde(default)]
        condition: Option<ActionConditionDefinition>,
        payload: ActionPayloadDefinition,
    },

    // Utility {
    //     // most likely non-data / hand-written only,
    //     // you can keep this variant out of the serializable enum if you want
    // },
    Composite {
        actions: Vec<ActionKindDefinition>,
    },
    Variants {
        variants: Vec<ActionId>,
    },
    // Same story here: you probably will not serialize these directly
    // for now, so they can live only on the runtime enum.
    Reaction {
        script: ScriptId,
    }, // Custom(...)
}

impl From<ActionKindDefinition> for ActionKind {
    fn from(spec: ActionKindDefinition) -> Self {
        match spec {
            ActionKindDefinition::Standard { condition, payload } => ActionKind::Standard {
                condition: if let Some(condition) = condition {
                    match condition {
                        ActionConditionDefinition::AttackRoll {
                            attack_roll,
                            damage_on_miss,
                        } => ActionCondition::AttackRoll {
                            attack_roll: attack_roll.function,
                            damage_on_miss: damage_on_miss.map(|damage_on_failure| {
                                match damage_on_failure {
                                    DamageOnFailureDefinition::Half => DamageOnFailure::Half,
                                    DamageOnFailureDefinition::Custom(damage_equation) => {
                                        DamageOnFailure::Custom(damage_equation.function)
                                    }
                                }
                            }),
                        },
                        ActionConditionDefinition::SavingThrow {
                            saving_throw,
                            damage_on_save,
                        } => ActionCondition::SavingThrow {
                            saving_throw: saving_throw.function,
                            damage_on_save: damage_on_save.map(|damage_on_failure| {
                                match damage_on_failure {
                                    DamageOnFailureDefinition::Half => DamageOnFailure::Half,
                                    DamageOnFailureDefinition::Custom(damage_equation) => {
                                        DamageOnFailure::Custom(damage_equation.function)
                                    }
                                }
                            }),
                        },
                    }
                } else {
                    ActionCondition::None
                },
                payload: ActionPayload::new(
                    payload.damage.map(|eq| eq.function),
                    payload
                        .effect
                        .map(|effect_instance_definition| effect_instance_definition.into()),
                    payload.healing.map(|eq| eq.function),
                )
                .unwrap(),
            },

            ActionKindDefinition::Composite { actions } => ActionKind::Composite {
                actions: actions.into_iter().map(ActionKind::from).collect(),
            },

            ActionKindDefinition::Variants { variants } => ActionKind::Variant { variants },

            ActionKindDefinition::Reaction { script } => ActionKind::Reaction { reaction: script },
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
            ActionKindDefinition::Reaction { script } => {
                collector.add(RegistryReference::Script(
                    script.clone(),
                    ScriptFunction::ReactionBody,
                ));
            }
        }
    }
}

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
    // TODO: How to handle reaction triggers in serialization?
    #[serde(default)]
    pub reaction_trigger: Option<ScriptId>,
}

impl RegistryReferenceCollector for ActionDefinition {
    fn collect_registry_references(&self, collector: &mut ReferenceCollector) {
        self.kind.collect_registry_references(collector);
        for resource in self.resource_cost.keys() {
            collector.add(RegistryReference::Resource(resource.clone()));
        }
        if let Some(reaction_trigger) = &self.reaction_trigger {
            collector.add(RegistryReference::Script(
                reaction_trigger.clone(),
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
            reaction_trigger: value.reaction_trigger,
        }
    }
}
