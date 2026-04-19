use hecs::{Entity, Ref, RefMut, World};
use tracing::debug;

use crate::{
    components::{
        actions::action::{ActionConditionResolution, ActionContext},
        effects::{
            effect::{
                EffectInstance, EffectInstanceId, EffectInstanceTemplate, EffectLifetimeTemplate,
                EffectsMap,
            },
            effect_manager::EffectManager,
        },
        id::{EffectId, EntityIdentifier},
        modifier::ModifierSource,
    },
    engine::{
        event::{Event, EventKind, EventListener, ListenerSource},
        game_state::GameState,
    },
    systems,
};

/// This gets used so often that it deserves its own function
pub fn effects(world: &World, entity: Entity) -> Ref<'_, EffectManager> {
    systems::helpers::get_component::<EffectManager>(world, entity)
}

pub fn effects_mut(world: &mut World, entity: Entity) -> RefMut<'_, EffectManager> {
    systems::helpers::get_component_mut::<EffectManager>(world, entity)
}

pub fn add_effect_template(
    game_state: &mut GameState,
    applier: Entity,
    target: Entity,
    source: ModifierSource,
    template: &EffectInstanceTemplate,
    context: Option<&ActionContext>,
    action_resolution: ActionConditionResolution,
) -> EffectInstanceId {
    let (parent_id, mut effect_instances) =
        template.instantiate(applier, target, source, action_resolution);

    debug!(
        "Instantiated effect instances from template {:?} -> {:?}",
        template, effect_instances
    );

    if let Some(parent_instance) = effect_instances.get(&parent_id)
        && let Some(end_condition) = &parent_instance.end_condition
    {
        game_state
            .event_dispatcher
            // TODO: A bit verbose EventListener construction
            .register_listener(EventListener::new(
                end_condition.event_filter.clone(),
                end_condition.callback.clone(),
                ListenerSource::EffectInstance {
                    id: parent_id.clone(),
                    entity: target,
                },
                false,
            ));
    }

    add_effect_instance(
        game_state,
        target,
        parent_id,
        &mut effect_instances,
        context,
    );

    parent_id
}

pub fn add_permanent_effect(
    game_state: &mut GameState,
    entity: Entity,
    effect_id: EffectId,
    source: &ModifierSource,
    context: Option<&ActionContext>,
) {
    add_effect_template(
        game_state,
        entity,
        entity,
        source.clone(),
        &EffectInstanceTemplate {
            effect_id,
            lifetime: EffectLifetimeTemplate::Permanent,
            end_condition: None,
            one_shot: false,
        },
        context,
        ActionConditionResolution::Unconditional,
    );
}

pub fn add_permanent_effects(
    game_state: &mut GameState,
    entity: Entity,
    effects: Vec<EffectId>,
    source: &ModifierSource,
    context: Option<&ActionContext>,
) {
    for effect_id in effects {
        add_permanent_effect(game_state, entity, effect_id, source, context);
    }
}

fn add_effect_instance(
    game_state: &mut GameState,
    entity: Entity,
    instance_id: EffectInstanceId,
    effect_instances: &mut EffectsMap,
    context: Option<&ActionContext>,
) -> Vec<EffectInstance> {
    let mut replaced_effects = Vec::new();

    if let Some(instance) = effect_instances.remove(&instance_id) {
        debug!(
            "Adding effect instance to entity {:?}: {:?} ",
            entity, instance,
        );
        replaced_effects.extend(apply_and_replace(game_state, entity, &instance, context));
        for child_instance in &instance.children {
            replaced_effects.extend(add_effect_instance(
                game_state,
                entity,
                child_instance.clone(),
                effect_instances,
                context,
            ));
        }
        effects_mut(&mut game_state.world, entity).insert(instance);
    }

    replaced_effects
}

fn apply_and_replace(
    game_state: &mut GameState,
    entity: Entity,
    effect_instance: &EffectInstance,
    context: Option<&ActionContext>,
) -> Vec<EffectInstance> {
    let effect = effect_instance.effect();
    if let Some(on_apply) = &effect.on_apply {
        on_apply(game_state, entity, context);
    }

    if let Some(replaces) = &effect.replaces {
        // TODO: Not sure how best to find the instance that should be replaced.
        // It's probably always just one?
        remove_effects_by_id(game_state, entity, replaces)
    } else {
        Vec::new()
    }
}

pub fn remove_effect(
    game_state: &mut GameState,
    entity: Entity,
    instance_id: &EffectInstanceId,
) -> Vec<EffectInstance> {
    debug!("Removing effect {:?} from entity {:?}", instance_id, entity);

    let mut removed_effects = Vec::new();

    if let Ok(effects) = game_state.world.query_one_mut::<&mut EffectManager>(entity) {
        if let Some(effect_instance) = effects.remove(instance_id) {
            let effect = effect_instance.effect();
            if let Some(on_unapply) = &effect.on_unapply {
                on_unapply(game_state, entity);
            }

            if !effect_instance.is_permanent() {
                game_state.event_dispatcher.remove_listeners_by_source(
                    &ListenerSource::EffectInstance {
                        id: *instance_id,
                        entity,
                    },
                );

                if effect_instance.is_parent() {
                    game_state.process_event(Event::new(EventKind::LostEffect {
                        entity: EntityIdentifier::from_world(&game_state.world, entity),
                        effect: effect.id.clone(),
                    }));
                }
            }

            for child_id in effect_instance.children.iter() {
                removed_effects.extend(remove_effect(game_state, entity, child_id));
            }

            removed_effects.push(effect_instance);
        }
    }

    removed_effects
}

pub fn remove_effects(
    game_state: &mut GameState,
    entity: Entity,
    effects: &[EffectInstanceId],
) -> Vec<EffectInstance> {
    let mut removed_effects = Vec::new();
    for effect_id in effects {
        removed_effects.extend(remove_effect(game_state, entity, effect_id));
    }
    removed_effects
}

pub fn remove_effects_by_source(
    game_state: &mut GameState,
    entity: Entity,
    source: &ModifierSource,
) -> Vec<EffectInstance> {
    remove_effects_by_filter(game_state, entity, |effect_instance| {
        effect_instance.source == *source
    })
}

pub fn remove_effects_by_id(
    game_state: &mut GameState,
    entity: Entity,
    effect_id: &EffectId,
) -> Vec<EffectInstance> {
    remove_effects_by_filter(game_state, entity, |effect_instance| {
        effect_instance.effect_id == *effect_id
    })
}

pub fn remove_temporary_effects(game_state: &mut GameState, entity: Entity) -> Vec<EffectInstance> {
    remove_effects_by_filter(game_state, entity, |effect_instance| {
        !effect_instance.is_permanent()
    })
}

fn remove_effects_by_filter(
    game_state: &mut GameState,
    entity: Entity,
    filter: impl Fn(&EffectInstance) -> bool,
) -> Vec<EffectInstance> {
    let instance_ids: Vec<EffectInstanceId> = effects(&game_state.world, entity)
        .iter()
        .filter_map(|(id, effect_instance)| {
            if filter(effect_instance) {
                Some(id.clone())
            } else {
                None
            }
        })
        .collect();

    let mut removed_effects = Vec::new();

    for instance_id in instance_ids {
        removed_effects.extend(remove_effect(game_state, entity, &instance_id));
    }

    removed_effects
}
