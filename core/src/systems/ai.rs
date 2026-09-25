use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::{ActionKind, ActionPayloadComponent},
        activity::Activity,
        ai::PlayerControlledTag,
        effects::effect::EffectKind,
        faction::Attitude,
        id::{AIControllerId, ActionVariantId},
    },
    engine::{
        action_prompt::{ActionData, ActionPrompt},
        engine_state::EngineState,
    },
    registry::{self},
    systems,
};

pub fn is_player_controlled(world: &World, entity: Entity) -> bool {
    world.get::<&PlayerControlledTag>(entity).is_ok()
}

pub fn decide_activity(
    engine_state: &mut EngineState,
    prompt: &ActionPrompt,
    actor: Entity,
) -> Option<Activity> {
    let controller_id =
        systems::helpers::get_component_clone::<AIControllerId>(&engine_state.world, actor);

    registry::ai::AI_CONTROLLER_REGISTRY
        .get(&controller_id)
        .unwrap()
        .decide(engine_state, prompt, actor)
}

pub fn recommeneded_target_attitude(
    _world: &World,
    _actor: Entity,
    action_kind: &ActionKind,
    variant: Option<&ActionVariantId>,
) -> Attitude {
    match action_kind {
        ActionKind::Standard { .. } | ActionKind::Variant { .. } => {
            for component in action_kind
                .phases(variant)
                .iter()
                .flat_map(|phase| phase.payload.components())
            {
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

        ActionKind::Reaction { .. } => {
            todo!()
        }
    }
}

pub fn possible_targets(engine_state: &EngineState, action_data: &ActionData) -> Vec<Entity> {
    let targeting = systems::actions::targeting_context_data(&engine_state.world, action_data);

    if let Some(encounter) = engine_state.encounter_for_entity(action_data.actor.id())
        && let Some(action) = systems::actions::get_action(&action_data.action_id)
    {
        encounter
            .participants(&engine_state.world, &targeting.allowed_entities)
            .into_iter()
            .filter(|target| {
                let target_attitude = systems::factions::mutual_attitude(
                    &engine_state.world,
                    action_data.actor.id(),
                    *target,
                );
                target_attitude
                    == recommeneded_target_attitude(
                        &engine_state.world,
                        action_data.actor.id(),
                        &action.kind,
                        action_data.variant.as_ref(),
                    )
            })
            .collect::<Vec<Entity>>()
    } else {
        Vec::new()
    }
}
