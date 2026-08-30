use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::{ActionKind, ActionPayloadComponent},
        activity::Activity,
        ai::PlayerControlledTag,
        effects::effect::EffectKind,
        faction::Attitude,
        id::AIControllerId,
    },
    engine::{
        action_prompt::{ActionData, ActionPrompt},
        game_state::GameState,
    },
    registry::{self},
    systems,
};

pub fn is_player_controlled(world: &World, entity: Entity) -> bool {
    world.get::<&PlayerControlledTag>(entity).is_ok()
}

pub fn decide_activity(
    game_state: &mut GameState,
    prompt: &ActionPrompt,
    actor: Entity,
) -> Option<Activity> {
    let controller_id =
        systems::helpers::get_component_clone::<AIControllerId>(&game_state.world, actor);

    registry::ai::AI_CONTROLLER_REGISTRY
        .get(&controller_id)
        .unwrap()
        .decide(game_state, prompt, actor)
}

pub fn recommeneded_target_attitude(
    _world: &World,
    _actor: Entity,
    action_kind: &ActionKind,
) -> Attitude {
    match action_kind {
        ActionKind::Standard { phases } => {
            for component in phases.iter().flat_map(|phase| phase.payload.components()) {
                match component {
                    ActionPayloadComponent::Damage { .. } => return Attitude::Hostile,
                    ActionPayloadComponent::Effect(effect_instance_template) => {
                        return match effect_instance_template.effect().kind {
                            EffectKind::Buff => Attitude::Friendly,
                            EffectKind::Debuff => Attitude::Hostile,
                        };
                    }
                    ActionPayloadComponent::Healing(_) => {
                        return Attitude::Friendly;
                    }
                    ActionPayloadComponent::Reaction(_) => {
                        // TODO: Reactions can be hostile or friendly
                    }
                    ActionPayloadComponent::Displacement(_) => todo!(),
                }
            }
            Attitude::Neutral
        }

        ActionKind::Variant { variants: _ } => {
            todo!()
        }

        ActionKind::Reaction { .. } => {
            todo!()
        }
    }
}

pub fn possible_targets(game_state: &GameState, action_data: &ActionData) -> Vec<Entity> {
    let targeting = systems::actions::targeting_context_data(&game_state.world, action_data);

    if let Some(encounter_id) = &game_state.in_combat.get(&action_data.actor.id())
        && let Some(encounter) = game_state.encounters.get(encounter_id)
        && let Some(action) = systems::actions::get_action(&action_data.action_id)
    {
        encounter
            .participants(&game_state.world, &targeting.allowed_entities)
            .into_iter()
            .filter(|target| {
                let target_attitude = systems::factions::mutual_attitude(
                    &game_state.world,
                    action_data.actor.id(),
                    *target,
                );
                target_attitude
                    == recommeneded_target_attitude(
                        &game_state.world,
                        action_data.actor.id(),
                        &action.kind,
                    )
            })
            .collect::<Vec<Entity>>()
    } else {
        Vec::new()
    }
}
