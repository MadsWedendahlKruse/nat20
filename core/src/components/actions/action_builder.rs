use hecs::{Entity, World};

use crate::{
    components::{
        actions::{
            action::{ActionContext, ActionKind, ActionMap},
            targeting::{TargetInstance, TargetingKind},
        },
        activity::{Activity, ActivityError},
        id::{ActionId, EntityIdentifier},
        resource::ResourceAmountMap,
    },
    engine::{
        action_prompt::{ActionData, ActionDecision, ActionDecisionKind},
        game_state::GameState,
    },
    systems::{
        self,
        movement::{TargetPathFindingError, TargetPathFindingResult},
    },
};

pub struct ActionBuilder {
    actor: EntityIdentifier,
    state: Result<ActionBuilderState, ActionBuilderError>,
}

impl ActionBuilder {
    pub fn all(world: &World, entity: Entity) -> Self {
        Self {
            actor: EntityIdentifier::from_world(world, entity),
            state: Ok(ActionBuilderState::Action {
                actions: systems::actions::all_actions(world, entity),
            }),
        }
    }

    pub fn available(world: &World, entity: Entity) -> Self {
        Self {
            actor: EntityIdentifier::from_world(world, entity),
            state: Ok(ActionBuilderState::Action {
                actions: systems::actions::available_actions(world, entity),
            }),
        }
    }

    pub fn actor(&self) -> &EntityIdentifier {
        &self.actor
    }

    pub fn state(&self) -> Result<&ActionBuilderState, &ActionBuilderError> {
        self.state.as_ref()
    }

    pub fn action(mut self, world: &World, action_id: &ActionId) -> Self {
        let actor = self.actor.clone();
        self.state = self.state.and_then(|state| match state {
            ActionBuilderState::Action { mut actions } => {
                Self::pick_action(&actor, world, &mut actions, action_id)
            }
            other => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Action",
                actual: other.kind_name(),
            }),
        });
        self
    }

    pub fn variant(mut self, world: &World, variant_id: &ActionId) -> Self {
        let actor = self.actor.clone();
        self.state = self.state.and_then(|state| match state {
            ActionBuilderState::Variant { mut variants } => {
                Self::pick_action(&actor, world, &mut variants, variant_id)
            }
            other => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Variant",
                actual: other.kind_name(),
            }),
        });
        self
    }

    pub fn context_index(mut self, world: &World, context_index: usize) -> Self {
        let actor = self.actor.clone();
        self.state = self.state.and_then(|state| match state {
            ActionBuilderState::Context {
                action,
                contexts_and_costs,
            } => Self::pick_context_and_cost(
                &actor,
                world,
                &action,
                &contexts_and_costs,
                context_index,
            ),
            other => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Context",
                actual: other.kind_name(),
            }),
        });
        self
    }

    pub fn context_filter(
        mut self,
        world: &World,
        filter_fn: impl Fn(&ActionContext, &ResourceAmountMap) -> bool,
    ) -> Self {
        let actor = self.actor.clone();
        self.state = self.state.and_then(|state| match state {
            ActionBuilderState::Context {
                action,
                contexts_and_costs,
            } => match contexts_and_costs
                .iter()
                .position(|(context, cost)| filter_fn(context, cost))
            {
                Some(index) => {
                    Self::pick_context_and_cost(&actor, world, &action, &contexts_and_costs, index)
                }
                None => Err(ActionBuilderError::NoMatchingContext {
                    options: contexts_and_costs,
                }),
            },
            other => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Context",
                actual: other.kind_name(),
            }),
        });
        self
    }

    pub fn target(mut self, game_state: &mut GameState, target: TargetInstance) -> Self {
        self.state = match self.state {
            Ok(ActionBuilderState::Targets { mut action, .. }) => {
                action.targets.push(target.clone());
                match systems::movement::path_to_target(game_state, &action) {
                    Err(reason) => Err(ActionBuilderError::InvalidTarget { target, reason }),
                    Ok(path_to_target) => Ok(ActionBuilderState::Targets {
                        action,
                        path_to_target: Some(path_to_target),
                    }),
                }
            }
            Ok(other) => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Targets",
                actual: other.kind_name(),
            }),
            Err(e) => Err(e),
        };
        self
    }

    pub fn perform(self, game_state: &mut GameState) -> Result<(), ActionBuilderError> {
        let state = self.state?;
        match state {
            ActionBuilderState::Targets {
                action,
                path_to_target,
            } => {
                let decision_kind = ActionDecisionKind::Action { action };
                let decision = if let Some(prompt_id) = game_state
                    .next_prompt_entity(self.actor.id())
                    .map(|prompt| prompt.id)
                {
                    ActionDecision {
                        response_to: prompt_id,
                        kind: decision_kind,
                    }
                } else {
                    ActionDecision::without_response_to(decision_kind)
                };

                let activity = if let Some(path_result) = path_to_target {
                    match path_result {
                        TargetPathFindingResult::AlreadyInRange(_) => {
                            Activity::Act { action: decision }
                        }
                        TargetPathFindingResult::PathFound(path_result) => Activity::MoveAndAct {
                            goal: path_result.path_result.taken_path.end().unwrap().clone(),
                            action: decision,
                        },
                    }
                } else {
                    Activity::Act { action: decision }
                };

                game_state
                    .submit_activity(activity)
                    .map_err(ActionBuilderError::Activity)
            }
            other => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Targets",
                actual: other.kind_name(),
            }),
        }
    }

    fn pick_action(
        actor: &EntityIdentifier,
        world: &World,
        actions: &mut ActionMap,
        action_id: &ActionId,
    ) -> Result<ActionBuilderState, ActionBuilderError> {
        let Some(action) = systems::actions::get_action(action_id) else {
            return Err(ActionBuilderError::ActionNotFound(action_id.clone()));
        };

        if action.is_reaction() {
            return Err(ActionBuilderError::ActionIsReaction(action_id.clone()));
        }

        let Some(contexts_and_costs) = actions.get_mut(action_id) else {
            return Err(ActionBuilderError::ActionNotAvailable {
                action: action_id.clone(),
                actor: actor.clone(),
                available: actions.keys().cloned().collect(),
            });
        };

        // TODO: Could it ever cause problems to apply this twice?
        for (context, cost) in contexts_and_costs.iter_mut() {
            systems::effects::effects(world, actor.id()).resource_cost(
                world,
                actor.id(),
                &action_id,
                context,
                cost,
            );
        }

        match action.kind() {
            ActionKind::Variant { variants } => {
                if variants.len() == 1 {
                    // If there's only one variant, skip the variant selection step and go straight to context selection
                    return Self::pick_action(actor, world, actions, &variants[0]);
                }

                // Assume all variants have the same contexts and costs
                let variants = variants
                    .iter()
                    .map(|variant_id| (variant_id.clone(), contexts_and_costs.clone()))
                    .collect();

                Ok(ActionBuilderState::Variant { variants })
            }
            _ => {
                if contexts_and_costs.len() == 1 {
                    // If there's only one context, skip the context selection step
                    return Self::pick_context_and_cost(
                        actor,
                        world,
                        action_id,
                        contexts_and_costs,
                        0,
                    );
                }

                Ok(ActionBuilderState::Context {
                    action: action_id.clone(),
                    contexts_and_costs: contexts_and_costs.clone(),
                })
            }
        }
    }

    fn pick_context_and_cost(
        actor: &EntityIdentifier,
        world: &World,
        action_id: &ActionId,
        contexts_and_costs: &Vec<(ActionContext, ResourceAmountMap)>,
        index: usize,
    ) -> Result<ActionBuilderState, ActionBuilderError> {
        let (context, resource_cost) =
            contexts_and_costs
                .get(index)
                .ok_or(ActionBuilderError::InvalidContextIndex {
                    index,
                    len: contexts_and_costs.len(),
                })?;

        let targeting = systems::actions::targeting_context(world, actor.id(), action_id, context);

        let targets = if TargetingKind::SelfTarget == targeting.kind {
            vec![TargetInstance::Entity(actor.clone())]
        } else {
            Vec::new()
        };

        Ok(ActionBuilderState::Targets {
            action: ActionData::new(
                actor.clone(),
                action_id.clone(),
                context.clone(),
                resource_cost.clone(),
                targets,
            ),
            path_to_target: None,
        })
    }
}

pub enum ActionBuilderState {
    Action {
        actions: ActionMap,
    },
    Variant {
        variants: ActionMap,
    },
    Context {
        action: ActionId,
        contexts_and_costs: Vec<(ActionContext, ResourceAmountMap)>,
    },
    Targets {
        action: ActionData,
        path_to_target: Option<TargetPathFindingResult>,
    },
}

impl ActionBuilderState {
    fn kind_name(&self) -> &'static str {
        match self {
            ActionBuilderState::Action { .. } => "Action",
            ActionBuilderState::Variant { .. } => "Variant",
            ActionBuilderState::Context { .. } => "Context",
            ActionBuilderState::Targets { .. } => "Targets",
        }
    }
}

#[derive(Debug)]
pub enum ActionBuilderError {
    ActionNotFound(ActionId),
    ActionIsReaction(ActionId),
    ActionNotAvailable {
        actor: EntityIdentifier,
        action: ActionId,
        available: Vec<ActionId>,
    },
    InvalidContextIndex {
        index: usize,
        len: usize,
    },
    NoMatchingContext {
        options: Vec<(ActionContext, ResourceAmountMap)>,
    },
    InvalidTarget {
        target: TargetInstance,
        reason: TargetPathFindingError,
    },
    InvalidStateTransition {
        expected: &'static str,
        actual: &'static str,
    },
    Activity(ActivityError),
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_utils::{creature_builder::CreatureBuilder, fixtures};

    #[test]
    fn known_action_with_available_context_succeeds() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .spawn(&mut game_state);

        let action_id = ActionId::new("nat20_core", "action.fighter.action_surge");
        let result = ActionBuilder::available(&game_state.world, fighter.id())
            .action(&game_state.world, &action_id)
            .perform(&mut game_state);

        assert!(result.is_ok(), "expected Ok(()), got {:?}", result.err());
    }

    #[test]
    fn unknown_action_returns_error() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .spawn(&mut game_state);

        let bogus = ActionId::new("nat20_core", "action.does_not_exist");
        let result = ActionBuilder::available(&game_state.world, fighter.id())
            .action(&game_state.world, &bogus)
            .perform(&mut game_state);

        assert!(
            matches!(result, Err(ActionBuilderError::ActionNotFound(ref id)) if id == &bogus),
            "expected ActionNotFound, got {:?}",
            result
        );
    }

    #[test]
    fn unavailable_action_returns_error() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .spawn(&mut game_state);

        // Fighter doesn't know any spells, so this should return an ActionNotAvailable error, not ActionNotFound
        let action_id = ActionId::new("nat20_core", "spell.magic_missile");
        let result = ActionBuilder::available(&game_state.world, fighter.id())
            .action(&game_state.world, &action_id)
            .perform(&mut game_state);

        assert!(
            matches!(
                result,
                Err(ActionBuilderError::ActionNotAvailable { ref action, ref actor, .. })
                if action == &action_id && actor.id() == fighter.id()
            ),
            "expected ActionNotAvailable for action {:?} and actor {:?}, got {:?}",
            action_id,
            fighter,
            result
        );
    }

    #[test]
    fn wrong_state_returns_error() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .spawn(&mut game_state);

        // Calling target() before action() should poison the builder with
        // an InvalidStateTransition error that surfaces at perform().
        let result = ActionBuilder::available(&game_state.world, fighter.id())
            .target(&mut game_state, TargetInstance::Entity(fighter.clone()))
            .perform(&mut game_state);

        assert!(
            matches!(
                result,
                Err(ActionBuilderError::InvalidStateTransition {
                    expected: "Targets",
                    actual: "Action",
                }),
            ),
            "expected InvalidStateTransition (expected: Targets, actual: Action), got {:?}",
            result
        );
    }

    #[test]
    fn valid_context_index_succeeds() {
        let mut game_state = fixtures::engine::game_state();
        let wizard = CreatureBuilder::new("hero.wizard")
            .level(5)
            .spawn(&mut game_state);

        let action_id = ActionId::new("nat20_core", "action.magic_missile");
        let result = ActionBuilder::available(&game_state.world, wizard.id())
            .action(&game_state.world, &action_id)
            .context_index(&game_state.world, 0)
            .target(&mut game_state, TargetInstance::Entity(wizard.clone()))
            .perform(&mut game_state);

        assert!(result.is_ok(), "expected Ok(()), got {:?}", result.err());
    }

    #[test]
    fn invalid_context_index_returns_error() {
        let mut game_state = fixtures::engine::game_state();
        let wizard = CreatureBuilder::new("hero.wizard")
            .level(5)
            .spawn(&mut game_state);

        let action_id = ActionId::new("nat20_core", "action.magic_missile");
        let result = ActionBuilder::available(&game_state.world, wizard.id())
            .action(&game_state.world, &action_id)
            .context_index(&game_state.world, 999) // Invalid index
            .target(&mut game_state, TargetInstance::Entity(wizard.clone()))
            .perform(&mut game_state);

        assert!(
            matches!(
                result,
                Err(ActionBuilderError::InvalidContextIndex { index: 999, len: _ })
            ),
            "expected InvalidContextIndex with index 999, got {:?}",
            result
        );
    }

    #[test]
    fn valid_context_filter_succeeds() {
        let mut game_state = fixtures::engine::game_state();
        let wizard = CreatureBuilder::new("hero.wizard")
            .level(5)
            .spawn(&mut game_state);

        let action_id = ActionId::new("nat20_core", "action.magic_missile");
        let result = ActionBuilder::available(&game_state.world, wizard.id())
            .action(&game_state.world, &action_id)
            .context_filter(&game_state.world, |context, _cost| {
                context
                    .spell
                    .as_ref()
                    // Level 5 wizard should have a level 3 spell slot available
                    .map(|spell| spell.level == 3)
                    .unwrap_or(false)
            })
            .target(&mut game_state, TargetInstance::Entity(wizard.clone()))
            .perform(&mut game_state);

        assert!(result.is_ok(), "expected Ok(()), got {:?}", result.err());
    }

    #[test]
    fn invalid_context_filter_returns_error() {
        let mut game_state = fixtures::engine::game_state();
        let wizard = CreatureBuilder::new("hero.wizard")
            .level(5)
            .spawn(&mut game_state);

        let action_id = ActionId::new("nat20_core", "action.magic_missile");
        let result = ActionBuilder::available(&game_state.world, wizard.id())
            .action(&game_state.world, &action_id)
            .context_filter(&game_state.world, |context, _cost| {
                context
                    .spell
                    .as_ref()
                    // Level 5 wizard should NOT have a level 9 spell slot available
                    .map(|spell| spell.level == 9)
                    .unwrap_or(false)
            })
            .target(&mut game_state, TargetInstance::Entity(wizard.clone()))
            .perform(&mut game_state);

        assert!(
            matches!(result, Err(ActionBuilderError::NoMatchingContext { ref options }) if options.len() > 0),
            "expected NoMatchingContext, got {:?}",
            result
        );
    }

    #[test]
    fn target_entity_in_range_succeeds() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .position([0.0, 0.0, 0.0].into(), false)
            .spawn(&mut game_state);
        let goblin = CreatureBuilder::new("monster.goblin_warrior")
            .level(1)
            .position([1.0, 0.0, 0.0].into(), false)
            .spawn(&mut game_state);

        let action_id = ActionId::new("nat20_core", "action.melee_attack");
        let result = ActionBuilder::available(&game_state.world, fighter.id())
            .action(&game_state.world, &action_id)
            .target(&mut game_state, TargetInstance::Entity(goblin.clone()))
            .perform(&mut game_state);

        assert!(
            result.is_ok(),
            "expected Ok(()) when targeting another entity, got {:?}",
            result.err()
        );
    }

    #[test]
    fn target_entity_out_of_range_but_within_movement_succeeds() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .position([0.0, 0.0, 0.0].into(), false)
            .spawn(&mut game_state);
        let goblin = CreatureBuilder::new("monster.goblin_warrior")
            .level(1)
            .position([3.0, 0.0, 0.0].into(), false)
            .spawn(&mut game_state);

        let action_id = ActionId::new("nat20_core", "action.melee_attack");
        let result = ActionBuilder::available(&game_state.world, fighter.id())
            .action(&game_state.world, &action_id)
            .target(&mut game_state, TargetInstance::Entity(goblin.clone()))
            .perform(&mut game_state);

        assert!(
            result.is_ok(),
            "expected Ok(()) when targeting an entity within movement range, got {:?}",
            result.err()
        );
    }

    #[test]
    fn target_entity_out_of_range_returns_error() {
        let mut game_state = fixtures::engine::game_state();
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .position([0.0, 0.0, 0.0].into(), false)
            .spawn(&mut game_state);
        let goblin = CreatureBuilder::new("monster.goblin_warrior")
            .level(1)
            .position([100.0, 0.0, 0.0].into(), false)
            .spawn(&mut game_state);

        let action_id = ActionId::new("nat20_core", "action.melee_attack");
        let result = ActionBuilder::available(&game_state.world, fighter.id())
            .action(&game_state.world, &action_id)
            .target(&mut game_state, TargetInstance::Entity(goblin.clone()))
            .perform(&mut game_state);

        assert!(
            matches!(result, Err(ActionBuilderError::InvalidTarget { ref target, ref reason })
            if target == &TargetInstance::Entity(EntityIdentifier::from_world(&game_state.world, goblin.id()))),
            // && matches!(reason, TargetingError::OutOfRange { .. })),
            "expected InvalidTarget with reason OutOfRange, got {:?}",
            result
        );
    }
}
