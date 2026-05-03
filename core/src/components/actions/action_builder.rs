use std::collections::HashMap;

use hecs::{Entity, World};
use parry3d::na::Point3;

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
        action_prompt::{
            ActionData, ActionDecision, ActionDecisionKind, ActionPromptKind, ReactionData,
        },
        event::Event,
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

    pub fn action(&mut self, world: &World, action_id: &ActionId) -> &mut Self {
        let actor = self.actor.clone();
        self.state = match &mut self.state {
            Ok(ActionBuilderState::Action { actions }) => {
                Self::pick_action(&actor, world, actions, action_id)
            }
            Ok(other) => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Action",
                actual: other.kind_name(),
            }),
            Err(_) => return self,
        };
        self
    }

    pub fn variant(&mut self, world: &World, variant_id: &ActionId) -> &mut Self {
        let actor = self.actor.clone();
        self.state = match &mut self.state {
            Ok(ActionBuilderState::Variant { variants }) => {
                Self::pick_action(&actor, world, variants, variant_id)
            }
            Ok(other) => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Variant",
                actual: other.kind_name(),
            }),
            Err(_) => return self,
        };
        self
    }

    pub fn context_index(&mut self, world: &World, context_index: usize) -> &mut Self {
        let actor = self.actor.clone();
        self.state = match &mut self.state {
            Ok(ActionBuilderState::Context {
                action,
                contexts_and_costs,
            }) => Self::pick_context_and_cost(
                &actor,
                world,
                action,
                contexts_and_costs,
                context_index,
            ),
            Ok(other) => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Context",
                actual: other.kind_name(),
            }),
            Err(_) => return self,
        };
        self
    }

    pub fn context_filter(
        &mut self,
        world: &World,
        filter_fn: impl Fn(&ActionContext, &ResourceAmountMap) -> bool,
    ) -> &mut Self {
        let actor = self.actor.clone();
        self.state = match &mut self.state {
            Ok(ActionBuilderState::Context {
                action,
                contexts_and_costs,
            }) => match contexts_and_costs
                .iter()
                .position(|(context, cost)| filter_fn(context, cost))
            {
                Some(index) => {
                    Self::pick_context_and_cost(&actor, world, &action, &contexts_and_costs, index)
                }
                None => Err(ActionBuilderError::NoMatchingContext {
                    options: contexts_and_costs.clone(),
                }),
            },
            Ok(other) => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Context",
                actual: other.kind_name(),
            }),
            Err(_) => return self,
        };
        self
    }

    pub fn target(&mut self, game_state: &mut GameState, target: TargetInstance) -> &mut Self {
        self.state = match &mut self.state {
            Ok(ActionBuilderState::Targets { action, .. }) => {
                action.targets.push(target.clone());
                match systems::movement::path_to_target(game_state, &action) {
                    Err(reason) => Err(ActionBuilderError::InvalidTarget { target, reason }),
                    Ok(path_to_target) => Ok(ActionBuilderState::Targets {
                        action: action.clone(),
                        path_to_target: Some(path_to_target),
                    }),
                }
            }
            Ok(other) => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Targets",
                actual: other.kind_name(),
            }),
            Err(_) => return self,
        };
        self
    }

    pub fn target_point(
        &mut self,
        game_state: &mut GameState,
        point: impl Into<Point3<f32>>,
    ) -> &mut Self {
        self.target(game_state, TargetInstance::Point(point.into()))
    }

    pub fn target_entity(&mut self, game_state: &mut GameState, entity: Entity) -> &mut Self {
        let target =
            TargetInstance::Entity(EntityIdentifier::from_world(&game_state.world, entity));
        self.target(game_state, target)
    }

    pub fn build(&self, game_state: &mut GameState) -> Result<Activity, ActionBuilderError> {
        match &self.state {
            Ok(ActionBuilderState::Targets {
                action,
                path_to_target,
            }) => {
                let decision_kind = ActionDecisionKind::Action {
                    action: action.clone(),
                };
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

                return Ok(activity);
            }
            Ok(other) => Err(ActionBuilderError::InvalidStateTransition {
                expected: "Targets",
                actual: other.kind_name(),
            }),
            Err(e) => Err(e.clone()),
        }
    }

    pub fn perform(&self, game_state: &mut GameState) -> Result<(), ActionBuilderError> {
        let activity = self.build(game_state)?;
        game_state
            .submit_activity(activity)
            .map_err(ActionBuilderError::Activity)
    }

    /// Convenience method for performing an action and panicking if it returns an
    /// error, to reduce boilerplate in tests where the action is expected to succeed.
    pub fn perform_ok(&self, game_state: &mut GameState) {
        match self.perform(game_state) {
            Ok(()) => (),
            Err(e) => panic!(
                "Expected perform to succeed, but it returned error: {:?}",
                e
            ),
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

#[derive(Debug, Clone)]
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

pub struct ReactionBuilder {
    actor: EntityIdentifier,
    state: Result<ReactionBuilderState, ReactionBuilderError>,
}

impl ReactionBuilder {
    pub fn new(game_state: &GameState, entity: Entity) -> Self {
        let actor = EntityIdentifier::from_world(&game_state.world, entity);

        let Some(prompt) = game_state.next_prompt_entity(entity) else {
            return Self {
                actor,
                state: Err(ReactionBuilderError::NoPrompt),
            };
        };

        match &prompt.kind {
            ActionPromptKind::Action { .. } => {
                return Self {
                    actor,
                    state: Err(ReactionBuilderError::NoReactionPrompt),
                };
            }

            ActionPromptKind::Reactions { event, options } => {
                let Some(actor_options) = options.get(&actor.id()) else {
                    return Self {
                        actor,
                        state: Err(ReactionBuilderError::NoOptionsForEntity {
                            options: options.clone(),
                        }),
                    };
                };

                Self {
                    actor,
                    state: Ok(ReactionBuilderState::Options {
                        event: event.clone(),
                        options: actor_options.clone(),
                    }),
                }
            }
        }
    }

    pub fn actor(&self) -> &EntityIdentifier {
        &self.actor
    }

    pub fn state(&self) -> Result<&ReactionBuilderState, &ReactionBuilderError> {
        self.state.as_ref()
    }

    pub fn option_none(&mut self) -> &mut Self {
        self.state = match &mut self.state {
            Ok(ReactionBuilderState::Options { event, options: _ }) => {
                Ok(ReactionBuilderState::Decision {
                    event: event.clone(),
                    decision: None,
                })
            }
            Ok(other) => Err(ReactionBuilderError::InvalidStateTransition {
                expected: "Options",
                actual: other.kind_name(),
            }),
            Err(_) => return self,
        };
        self
    }

    pub fn option_index(&mut self, option_index: usize) -> &mut Self {
        self.state = match &mut self.state {
            Ok(ReactionBuilderState::Options { event, options }) => {
                if let Some(option) = options.get(option_index) {
                    Ok(ReactionBuilderState::Decision {
                        event: event.clone(),
                        decision: Some(option.clone()),
                    })
                } else {
                    Err(ReactionBuilderError::InvalidOptionIndex {
                        index: option_index,
                        len: options.len(),
                    })
                }
            }
            Ok(other) => Err(ReactionBuilderError::InvalidStateTransition {
                expected: "Options",
                actual: other.kind_name(),
            }),
            Err(_) => return self,
        };
        self
    }

    pub fn option_filter(&mut self, filter_fn: impl Fn(&ReactionData) -> bool) -> &mut Self {
        self.state = match &mut self.state {
            Ok(ReactionBuilderState::Options { event, options }) => {
                if let Some(option) = options.iter().find(|option| filter_fn(option)) {
                    Ok(ReactionBuilderState::Decision {
                        event: event.clone(),
                        decision: Some(option.clone()),
                    })
                } else {
                    Err(ReactionBuilderError::NoMatchingOption {
                        options: options.clone(),
                    })
                }
            }
            Ok(other) => Err(ReactionBuilderError::InvalidStateTransition {
                expected: "Options",
                actual: other.kind_name(),
            }),
            Err(_) => return self,
        };
        self
    }

    pub fn build(&self) -> Result<Activity, ReactionBuilderError> {
        match &self.state {
            Ok(ReactionBuilderState::Decision { event, decision }) => Ok(Activity::Act {
                action: ActionDecision {
                    response_to: event.id,
                    kind: ActionDecisionKind::Reaction {
                        event: event.clone(),
                        reactor: self.actor.id(),
                        choice: decision.clone(),
                    },
                },
            }),
            Ok(other) => Err(ReactionBuilderError::InvalidStateTransition {
                expected: "Decision",
                actual: other.kind_name(),
            }),
            Err(e) => Err(e.clone()),
        }
    }

    pub fn perform(&self, game_state: &mut GameState) -> Result<(), ReactionBuilderError> {
        let activity = self.build()?;
        game_state
            .submit_activity(activity)
            .map_err(ReactionBuilderError::Activity)
    }

    /// See `ActionBuilder::perform_ok`
    pub fn perform_ok(&self, game_state: &mut GameState) {
        match self.perform(game_state) {
            Ok(()) => (),
            Err(e) => panic!(
                "Expected perform to succeed, but it returned error: {:?}",
                e
            ),
        }
    }
}

pub enum ReactionBuilderState {
    Options {
        event: Event,
        options: Vec<ReactionData>,
    },
    Decision {
        event: Event,
        decision: Option<ReactionData>,
    },
}

impl ReactionBuilderState {
    fn kind_name(&self) -> &'static str {
        match self {
            ReactionBuilderState::Options { .. } => "Options",
            ReactionBuilderState::Decision { .. } => "Decision",
        }
    }
}

#[derive(Debug, Clone)]
pub enum ReactionBuilderError {
    NoPrompt,
    NoReactionPrompt,
    NoOptionsForEntity {
        options: HashMap<Entity, Vec<ReactionData>>,
    },
    InvalidOptionIndex {
        index: usize,
        len: usize,
    },
    NoMatchingOption {
        options: Vec<ReactionData>,
    },
    InvalidStateTransition {
        expected: &'static str,
        actual: &'static str,
    },
    Activity(ActivityError),
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use rstest::{fixture, rstest};

    use super::*;

    use crate::{
        components::{
            d20::D20CheckOutcome, items::equipment::weapon::WeaponKind, modifier::ModifierSource,
        },
        test_utils::{creature_builder::CreatureBuilder, fixtures},
    };

    #[fixture]
    fn game_state() -> GameState {
        fixtures::engine::game_state()
    }

    #[fixture]
    fn game_state_fighter(mut game_state: GameState) -> (GameState, EntityIdentifier) {
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .spawn(&mut game_state);
        (game_state, fighter)
    }

    #[rstest]
    fn action_builder_known_action_with_available_context_succeeds(
        game_state_fighter: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, fighter) = game_state_fighter;

        let action_id = ActionId::new("nat20_core", "action.fighter.action_surge");
        let result = ActionBuilder::available(&game_state.world, fighter.id())
            .action(&game_state.world, &action_id)
            .perform(&mut game_state);

        assert!(result.is_ok(), "expected Ok(()), got {:?}", result.err());
    }

    #[rstest]
    fn action_builder_unknown_action_returns_error(
        game_state_fighter: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, fighter) = game_state_fighter;

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

    #[rstest]
    fn action_builder_unavailable_action_returns_error(
        game_state_fighter: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, fighter) = game_state_fighter;

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

    #[rstest]
    fn action_builder_wrong_state_returns_error(game_state_fighter: (GameState, EntityIdentifier)) {
        let (mut game_state, fighter) = game_state_fighter;

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

    #[fixture]
    fn game_state_wizard(mut game_state: GameState) -> (GameState, EntityIdentifier) {
        let wizard = CreatureBuilder::new("hero.wizard")
            .level(5)
            .spawn(&mut game_state);
        (game_state, wizard)
    }

    #[rstest]
    fn action_builder_valid_context_index_succeeds(
        game_state_wizard: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, wizard) = game_state_wizard;

        let action_id = ActionId::new("nat20_core", "action.magic_missile");
        let result = ActionBuilder::available(&game_state.world, wizard.id())
            .action(&game_state.world, &action_id)
            .context_index(&game_state.world, 0)
            .target(&mut game_state, TargetInstance::Entity(wizard.clone()))
            .perform(&mut game_state);

        assert!(result.is_ok(), "expected Ok(()), got {:?}", result.err());
    }

    #[rstest]
    fn action_builder_invalid_context_index_returns_error(
        game_state_wizard: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, wizard) = game_state_wizard;

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

    #[rstest]
    fn action_builder_valid_context_filter_succeeds(
        game_state_wizard: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, wizard) = game_state_wizard;

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

    #[rstest]
    fn action_builder_invalid_context_filter_returns_error(
        game_state_wizard: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, wizard) = game_state_wizard;

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

    #[rstest]
    fn action_builder_target_entity_in_range_succeeds(
        game_state_fighter: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, fighter) = game_state_fighter;

        let goblin = CreatureBuilder::new("monster.goblin_warrior")
            .level(1)
            .position([1.0, 0.0, 0.0], false)
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

    #[rstest]
    fn action_builder_target_entity_out_of_range_but_within_movement_succeeds(
        game_state_fighter: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, fighter) = game_state_fighter;

        let goblin = CreatureBuilder::new("monster.goblin_warrior")
            .level(1)
            .position([3.0, 0.0, 0.0], false)
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

    #[rstest]
    fn action_builder_target_entity_out_of_range_returns_error(
        game_state_fighter: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, fighter) = game_state_fighter;

        let goblin = CreatureBuilder::new("monster.goblin_warrior")
            .level(1)
            .position([100.0, 0.0, 0.0], false)
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

    #[rstest]
    fn reaction_builder_no_prompt_returns_error(game_state_fighter: (GameState, EntityIdentifier)) {
        let (game_state, fighter) = game_state_fighter;

        let result = ReactionBuilder::new(&game_state, fighter.id()).build();

        assert!(
            matches!(result, Err(ReactionBuilderError::NoPrompt)),
            "expected NoPrompt error, got {:?}",
            result
        );
    }

    #[rstest]
    fn reaction_builder_wrong_prompt_type_returns_error(
        game_state_fighter: (GameState, EntityIdentifier),
    ) {
        let (mut game_state, fighter) = game_state_fighter;

        // Start encounter to queue an action prompt
        game_state.start_encounter(HashSet::from([fighter.id()]));

        let result = ReactionBuilder::new(&game_state, fighter.id()).build();

        assert!(
            matches!(result, Err(ReactionBuilderError::NoReactionPrompt)),
            "expected WrongPromptType error, got {:?}",
            result
        );
    }

    #[fixture]
    fn game_state_reaction(
        mut game_state: GameState,
    ) -> (GameState, EntityIdentifier, EntityIdentifier) {
        let fighter = CreatureBuilder::new("hero.fighter")
            .level(5)
            .position([0.0, 0.0, 0.0], false)
            .spawn(&mut game_state);
        let wizard = CreatureBuilder::new("hero.wizard")
            .level(5)
            .position([1.0, 0.0, 0.0], false)
            .spawn(&mut game_state);
        // Force the fighter to hit the attack so the wizard has a reason to react
        systems::loadout::loadout_mut(&mut game_state.world, fighter.id())
            .attack_roll_template_mut(&WeaponKind::Melee)
            .d20_check
            .set_forced_outcome(
                ModifierSource::Custom("Testing".to_string()),
                D20CheckOutcome::CriticalSuccess,
            );

        // Fighter attacking wizard triggers shield reaction
        ActionBuilder::available(&game_state.world, fighter.id())
            .action(
                &game_state.world,
                &ActionId::new("nat20_core", "action.melee_attack"),
            )
            .target(&mut game_state, TargetInstance::Entity(wizard.clone()))
            .perform(&mut game_state)
            .expect("Failed to perform action");
        game_state.update(10.0);

        (game_state, fighter, wizard)
    }

    #[rstest]
    fn reaction_builder_no_options_for_entity_returns_error(
        game_state_reaction: (GameState, EntityIdentifier, EntityIdentifier),
    ) {
        let (game_state, fighter, _wizard) = game_state_reaction;

        let result = ReactionBuilder::new(&game_state, fighter.id()).build();

        assert!(
            matches!(result, Err(ReactionBuilderError::NoOptionsForEntity { .. })),
            "expected NoOptionsForEntity error, got {:?}",
            result
        );
    }

    #[rstest]
    fn reaction_builder_valid_option_index_succeeds(
        game_state_reaction: (GameState, EntityIdentifier, EntityIdentifier),
    ) {
        let (game_state, _fighter, wizard) = game_state_reaction;

        let result = ReactionBuilder::new(&game_state, wizard.id())
            .option_index(0) // Choose the first reaction option (Shield spell)
            .build();

        assert!(
            result.is_ok(),
            "expected Ok(()) when choosing valid reaction option, got {:?}",
            result.err()
        );
    }

    #[rstest]
    fn reaction_builder_invalid_option_index_returns_error(
        game_state_reaction: (GameState, EntityIdentifier, EntityIdentifier),
    ) {
        let (game_state, _fighter, wizard) = game_state_reaction;

        let result = ReactionBuilder::new(&game_state, wizard.id())
            .option_index(999) // Invalid index
            .build();

        assert!(
            matches!(
                result,
                Err(ReactionBuilderError::InvalidOptionIndex { index: 999, len: _ })
            ),
            "expected InvalidOptionIndex error with index 999, got {:?}",
            result
        );
    }

    #[rstest]
    fn reaction_builder_option_none_succeeds(
        game_state_reaction: (GameState, EntityIdentifier, EntityIdentifier),
    ) {
        let (game_state, _fighter, wizard) = game_state_reaction;

        let result = ReactionBuilder::new(&game_state, wizard.id())
            .option_none() // Choose to not react
            .build();

        assert!(
            result.is_ok(),
            "expected Ok(()) when choosing no reaction option, got {:?}",
            result.err()
        );
    }

    #[rstest]
    fn reaction_builder_option_filter_succeeds(
        game_state_reaction: (GameState, EntityIdentifier, EntityIdentifier),
    ) {
        let (game_state, _fighter, wizard) = game_state_reaction;

        let result = ReactionBuilder::new(&game_state, wizard.id())
            .option_filter(|option| {
                // Choose the level 3 Shield reaction option
                option
                    .context
                    .spell
                    .as_ref()
                    .map(|spell| spell.level == 3)
                    .unwrap_or(false)
                    && option.reaction_id == ActionId::new("nat20_core", "action.shield")
            })
            .build();

        assert!(
            result.is_ok(),
            "expected Ok(()) when choosing Shield reaction option, got {:?}",
            result.err()
        );
    }

    #[rstest]
    fn reaction_builder_option_filter_no_match_returns_error(
        game_state_reaction: (GameState, EntityIdentifier, EntityIdentifier),
    ) {
        let (game_state, _fighter, wizard) = game_state_reaction;

        let result = ReactionBuilder::new(&game_state, wizard.id())
            // Try to choose a non-existent reaction option
            .option_filter(|option| {
                option
                    .context
                    .spell
                    .as_ref()
                    .map(|spell| spell.level == 9)
                    .unwrap_or(false)
            })
            .build();

        assert!(
            matches!(&result, Err(ReactionBuilderError::NoMatchingOption { options }) if options.len() > 0),
            "expected NoMatchingOption error, got {:?}",
            result
        );
    }
}
