use std::{collections::HashMap, sync::LazyLock};

use hecs::Entity;
use rand::{
    Rng,
    seq::{IndexedRandom, IteratorRandom},
};
use tracing::{debug, error};

use crate::{
    components::{
        actions::{
            action_builder::{ActionBuilder, ActionBuilderState, ReactionBuilder},
            targeting::TargetingKind,
        },
        activity::Activity,
        ai::AIController,
        id::AIControllerId,
    },
    engine::{
        action_prompt::{ActionPrompt, ActionPromptKind},
        game_state::GameState,
    },
    systems::{self},
};

pub static AI_CONTROLLER_REGISTRY: LazyLock<HashMap<AIControllerId, Box<dyn AIController>>> =
    LazyLock::new(|| {
        HashMap::from([(
            RANDOM_CONTROLLER_ID.clone(),
            Box::new(RandomController) as Box<dyn AIController>,
        )])
    });

pub static RANDOM_CONTROLLER_ID: LazyLock<AIControllerId> =
    LazyLock::new(|| AIControllerId::new("nat20_core", "ai_controller.random"));

pub struct RandomController;

impl AIController for RandomController {
    fn decide(
        &self,
        game_state: &mut GameState,
        prompt: &ActionPrompt,
        actor: Entity,
    ) -> Option<Activity> {
        let rng = &mut rand::rng();

        match &prompt.kind {
            ActionPromptKind::Action { actor } => {
                let mut action_builder = ActionBuilder::available(game_state, *actor);

                loop {
                    match action_builder.state() {
                        Ok(state) => match state {
                            ActionBuilderState::Action { actions }
                            | ActionBuilderState::Variant { variants: actions } => {
                                action_builder
                                    .action(game_state, &actions.keys().choose(rng).cloned()?);
                            }

                            ActionBuilderState::Context {
                                contexts_and_costs, ..
                            } => {
                                action_builder.context_index(
                                    &game_state.world,
                                    rng.random_range(0..contexts_and_costs.len()),
                                );
                            }

                            ActionBuilderState::Targets { action, .. } => {
                                // This means it was populated on the previous iteration
                                if !action.targets.is_empty() {
                                    match action_builder.build(game_state) {
                                        Ok(activity) => return Some(activity),
                                        Err(error) => {
                                            error!(
                                                "AI actor {:?} failed to build action: {:?}",
                                                actor, error
                                            );
                                            return None;
                                        }
                                    }
                                }

                                let possible_targets =
                                    systems::ai::possible_targets(game_state, action);

                                if possible_targets.is_empty() {
                                    debug!(
                                        "AI actor {:?} has no possible targets for action: {:?}",
                                        actor, action.action_id
                                    );
                                    return None;
                                }

                                let targeting = systems::actions::targeting_context_data(
                                    &game_state.world,
                                    action,
                                );

                                match targeting.kind {
                                    TargetingKind::SelfTarget => {
                                        action_builder.target_entity(game_state, *actor);
                                    }

                                    TargetingKind::Single => {
                                        action_builder.target_entity(
                                            game_state,
                                            *possible_targets.choose(rng)?,
                                        );
                                    }

                                    TargetingKind::Multiple {
                                        max_targets,
                                        allow_duplicates,
                                    } => {
                                        let chosen_targets = if allow_duplicates {
                                            possible_targets
                                                .iter()
                                                .choose_multiple(rng, max_targets)
                                        } else {
                                            let max_unique_targets =
                                                max_targets.min(possible_targets.len());
                                            possible_targets[0..max_unique_targets].iter().collect()
                                        };

                                        for target in chosen_targets {
                                            action_builder.target_entity(game_state, *target);
                                        }
                                    }

                                    TargetingKind::Area {
                                        shape,
                                        fixed_on_actor,
                                        filters,
                                    } => todo!(),
                                }
                            }
                        },

                        Err(error) => {
                            error!("Failed to build action: {:?}", error);
                            return None;
                        }
                    }
                }
            }

            ActionPromptKind::Reactions { options, .. } => {
                let mut reaction_builder = ReactionBuilder::new(game_state, actor);

                reaction_builder.option_index(rng.random_range(0..options.get(&actor)?.len()));

                match reaction_builder.build() {
                    Ok(reaction) => Some(reaction),
                    Err(error) => {
                        error!("AI actor {:?} failed to build reaction: {:?}", actor, error);
                        None
                    }
                }
            }
        }
    }
}
