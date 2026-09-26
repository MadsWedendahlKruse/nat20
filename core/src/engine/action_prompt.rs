use std::collections::HashMap;

use hecs::Entity;
use uuid::Uuid;

use crate::{
    components::{
        actions::{action::ActionContext, targeting::TargetInstance},
        id::{ActionId, ActionVariantId, EntityIdentifier},
        resource::{ResourceAmountMap, ResourceError},
    },
    engine::event::{Event, EventId},
    systems::{self, actions::ActionUsabilityError},
};

pub type ActionPromptId = Uuid;

#[derive(Debug, Clone, PartialEq)]
pub enum ActionPromptKind {
    /// Prompt an entity to perform an action
    Action {
        /// The entity that should perform the action
        actor: Entity,
    },
    /// Prompt all entities that can react to an event to make a reaction decision.
    /// While actions are prompted one at a time, an action can trigger multiple
    /// reactions, and we need to give everyone a fair chance to react before we
    /// can proceede
    Reactions {
        /// The event that triggered the reactions
        event: Event,
        /// The options available for those reacting. The key is the entity which
        /// is reaction, and the value is their options
        options: HashMap<Entity, Vec<ActionData>>,
    },
}

impl ActionPromptKind {
    pub fn actors(&self) -> Vec<Entity> {
        match self {
            ActionPromptKind::Action { actor } => vec![*actor],
            ActionPromptKind::Reactions { options, .. } => options.keys().cloned().collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionPrompt {
    pub id: ActionPromptId,
    pub kind: ActionPromptKind,
}

impl ActionPrompt {
    pub fn new(kind: ActionPromptKind) -> Self {
        Self {
            id: Uuid::new_v4(),
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionDecisionKind {
    Action {
        action: ActionData,
    },
    Reaction {
        /// The event that triggered the reaction
        event: Event,
        reactor: Entity,
        /// The chosen reaction. None if the entity chooses not to react
        choice: Option<ActionData>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionDecision {
    pub response_to: ActionPromptId,
    pub kind: ActionDecisionKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionError {
    PromptDecisionMismatch {
        prompt: ActionPrompt,
        decision: ActionDecision,
    },
    FieldMismatch {
        field: &'static str,
        expected: String,
        actual: String,
        prompt: ActionPrompt,
        decision: ActionDecision,
    },
    MissingPrompt {
        decision: ActionDecision,
        prompts: Vec<ActionPrompt>,
    },
    NotYourTurn {
        decision: ActionDecision,
    },
    Usability(ActionUsabilityError),
    Resource(Vec<ResourceError>),
}

fn compare_fields<T>(
    a: T,
    b: T,
    field: &'static str,
    prompt: &ActionPrompt,
    decision: &ActionDecision,
) -> Result<(), ActionError>
where
    T: PartialEq + std::fmt::Debug,
{
    if a != b {
        return Err(ActionError::FieldMismatch {
            field,
            expected: format!("{:?}", a),
            actual: format!("{:?}", b),
            prompt: prompt.clone(),
            decision: decision.clone(),
        });
    }
    Ok(())
}

impl ActionPrompt {
    pub fn actors(&self) -> Vec<Entity> {
        match &self.kind {
            ActionPromptKind::Action { actor, .. } => vec![*actor],
            ActionPromptKind::Reactions { options, .. } => options.keys().cloned().collect(),
        }
    }

    pub fn is_reaction(&self) -> bool {
        matches!(self.kind, ActionPromptKind::Reactions { .. })
    }

    pub fn is_valid_decision(&self, decision: &ActionDecision) -> Result<(), ActionError> {
        compare_fields(
            self.is_reaction(),
            decision.kind.is_reaction(),
            "decision.is_reaction",
            self,
            decision,
        )?;

        if let Some(action) = decision.action()
            && let Some(action) = systems::actions::get_action(&action.action_id)
        {
            compare_fields(
                self.is_reaction(),
                action.is_reaction(),
                "action.is_reaction",
                self,
                decision,
            )?;
        }

        compare_fields(self.id, decision.response_to, "response_to", self, decision)?;

        match (&self.kind, &decision.kind) {
            (
                ActionPromptKind::Action {
                    actor: prompt_actor,
                },
                ActionDecisionKind::Action { action },
            ) => {
                compare_fields(prompt_actor, &action.actor.id(), "actor", self, decision)?;
            }

            (
                ActionPromptKind::Reactions {
                    event: prompt_event,
                    options,
                },
                ActionDecisionKind::Reaction {
                    event: decision_event,
                    reactor,
                    choice,
                },
            ) => {
                compare_fields(
                    prompt_event.id,
                    decision_event.id,
                    "event_id",
                    self,
                    decision,
                )?;

                if let Some(options) = options.get(reactor) {
                    if let Some(choice) = choice
                        && !options.contains(choice)
                    {
                        return Err(ActionError::FieldMismatch {
                            field: "choices",
                            expected: format!("one of {:?}", options),
                            actual: format!("{:?}", choice),
                            prompt: self.clone(),
                            decision: decision.clone(),
                        });
                    }
                } else {
                    return Err(ActionError::FieldMismatch {
                        field: "reactor",
                        expected: format!("one of {:?}", options.keys()),
                        actual: format!("{:?}", reactor),
                        prompt: self.clone(),
                        decision: decision.clone(),
                    });
                }
            }

            _ => {
                return Err(ActionError::PromptDecisionMismatch {
                    prompt: self.clone(),
                    decision: decision.clone(),
                });
            }
        }
        Ok(())
    }
}

impl ActionDecisionKind {
    pub fn actor(&self) -> Entity {
        match self {
            ActionDecisionKind::Action { action, .. } => action.actor.id(),
            ActionDecisionKind::Reaction { reactor, .. } => *reactor,
        }
    }

    pub fn is_reaction(&self) -> bool {
        matches!(self, ActionDecisionKind::Reaction { .. })
    }
}

impl ActionDecision {
    pub fn without_response_to(kind: ActionDecisionKind) -> Self {
        Self {
            response_to: Uuid::nil(),
            kind,
        }
    }

    pub fn actor(&self) -> Entity {
        self.kind.actor()
    }

    pub fn action(&self) -> Option<&ActionData> {
        match &self.kind {
            ActionDecisionKind::Action { action } => Some(action),
            ActionDecisionKind::Reaction { choice, .. } => choice.as_ref(),
        }
    }

    pub fn action_id(&self) -> Option<ActionId> {
        match &self.kind {
            ActionDecisionKind::Action { action } => Some(action.action_id.clone()),
            ActionDecisionKind::Reaction { choice, .. } => {
                choice.as_ref().map(|c| c.action_id.clone())
            }
        }
    }
}

pub type ActionExecutionInstanceId = Uuid;

// TODO: struct name?
#[derive(Debug, Clone, PartialEq)]
pub struct ActionData {
    pub instance_id: ActionExecutionInstanceId,
    pub actor: EntityIdentifier,
    pub action_id: ActionId,
    pub context: ActionContext,
    pub resource_cost: ResourceAmountMap,
    pub targets: Vec<TargetInstance>,
    /// In the case of a reaction, this will be the event that triggered the reaction
    pub trigger_event: Option<EventId>,
    // In the case of a variant action, this will be set to the chosen variant's ID
    pub variant: Option<ActionVariantId>,
}

impl ActionData {
    pub fn new(
        actor: EntityIdentifier,
        action_id: ActionId,
        context: ActionContext,
        resource_cost: ResourceAmountMap,
        targets: Vec<TargetInstance>,
    ) -> Self {
        Self {
            instance_id: Uuid::new_v4(),
            actor,
            action_id,
            context,
            resource_cost,
            targets,
            trigger_event: None,
            variant: None,
        }
    }

    pub fn with_trigger_event(mut self, event: EventId) -> Self {
        self.trigger_event = Some(event);
        self
    }

    pub fn with_variant(mut self, variant: impl Into<ActionVariantId>) -> Self {
        self.variant = Some(variant.into());
        self
    }

    pub fn is_self_target(&self) -> bool {
        self.targets.len() == 1
            && matches!(&self.targets[0], TargetInstance::Entity { entity, ..} if entity.id() == self.actor.id())
    }

    pub fn is_reaction(&self) -> bool {
        self.trigger_event.is_some()
    }
}
