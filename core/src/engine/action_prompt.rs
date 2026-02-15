use std::collections::HashMap;

use hecs::Entity;
use uuid::Uuid;

use crate::{
    components::{
        actions::{action::ActionContext, targeting::TargetInstance},
        id::ActionId,
        resource::{ResourceAmountMap, ResourceError},
    },
    engine::event::Event,
    systems::actions::ActionUsabilityError,
};

pub type ActionPromptId = Uuid;

#[derive(Debug, Clone)]
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
        options: HashMap<Entity, Vec<ReactionData>>,
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

#[derive(Debug, Clone)]
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
        choice: Option<ReactionData>,
    },
}

#[derive(Debug, Clone)]
pub struct ActionDecision {
    pub response_to: ActionPromptId,
    pub kind: ActionDecisionKind,
}

#[derive(Debug, Clone)]
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
    Resource(ResourceError),
}

macro_rules! ensure_equal {
    ($a:expr, $b:expr, $label:literal, $err_variant:ident, $self:expr, $decision:expr) => {
        if $a != $b {
            return Err(ActionError::$err_variant {
                field: $label,
                expected: format!("{:?}", $a),
                actual: format!("{:?}", $b),
                prompt: $self.clone(),
                decision: $decision.clone(),
            });
        }
    };
}

impl ActionPrompt {
    pub fn actors(&self) -> Vec<Entity> {
        match &self.kind {
            ActionPromptKind::Action { actor, .. } => vec![*actor],
            ActionPromptKind::Reactions { options, .. } => options.keys().cloned().collect(),
        }
    }

    pub fn is_valid_decision(&self, decision: &ActionDecision) -> Result<(), ActionError> {
        ensure_equal!(
            self.id,
            decision.response_to,
            "response_to",
            FieldMismatch,
            self,
            decision
        );

        match (&self.kind, &decision.kind) {
            (
                ActionPromptKind::Action {
                    actor: prompt_actor,
                },
                ActionDecisionKind::Action { action },
            ) => {
                ensure_equal!(
                    prompt_actor,
                    &action.actor,
                    "actor",
                    FieldMismatch,
                    self,
                    decision
                );
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
                ensure_equal!(
                    prompt_event.id,
                    decision_event.id,
                    "event_id",
                    FieldMismatch,
                    self,
                    decision
                );

                if let Some(options) = options.get(&reactor) {
                    if let Some(choice) = choice
                        && !options.contains(&choice)
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
            ActionDecisionKind::Action { action, .. } => action.actor,
            ActionDecisionKind::Reaction { reactor, .. } => *reactor,
        }
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
}

pub type ActionExecutionInstanceId = Uuid;

// TODO: struct name?
#[derive(Debug, Clone, PartialEq)]
pub struct ActionData {
    pub instance_id: ActionExecutionInstanceId,
    pub actor: Entity,
    pub action_id: ActionId,
    pub context: ActionContext,
    pub resource_cost: ResourceAmountMap,
    pub targets: Vec<TargetInstance>,
}

impl ActionData {
    pub fn new(
        actor: Entity,
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
        }
    }

    pub fn entity_targets(&self) -> Vec<Entity> {
        self.targets
            .iter()
            .filter_map(|t| {
                if let TargetInstance::Entity(e) = t {
                    Some(*e)
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReactionData {
    pub instance_id: ActionExecutionInstanceId,
    pub reactor: Entity,
    // The event that triggered this reaction
    pub event: Box<Event>,
    pub reaction_id: ActionId,
    pub context: ActionContext,
    pub resource_cost: ResourceAmountMap,
    pub target: TargetInstance,
}

impl ReactionData {
    pub fn new(
        reactor: Entity,
        event: Event,
        reaction_id: ActionId,
        context: ActionContext,
        resource_cost: ResourceAmountMap,
        target: TargetInstance,
    ) -> Self {
        Self {
            instance_id: Uuid::new_v4(),
            reactor,
            event: Box::new(event),
            reaction_id,
            context,
            resource_cost,
            target,
        }
    }
}

impl From<&ReactionData> for ActionData {
    fn from(reaction: &ReactionData) -> Self {
        ActionData {
            instance_id: reaction.instance_id,
            actor: reaction.reactor,
            action_id: reaction.reaction_id.clone(),
            context: reaction.context.clone(),
            resource_cost: reaction.resource_cost.clone(),
            targets: vec![reaction.target.clone()],
        }
    }
}
