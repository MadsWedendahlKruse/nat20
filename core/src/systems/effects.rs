use hecs::{Entity, Ref, RefMut, World};
use tracing::debug;

use crate::{
    components::{
        actions::action::{
            ActionConditionResolution, ActionContext, ActionResultComponent, EffectResult,
            EffectResultKind,
        },
        effects::{
            effect::{
                EffectGrantedAction, EffectInstance, EffectInstanceId, EffectInstanceTemplate,
                EffectLifetime, EffectLifetimeTemplate, EffectStackingPolicy, EffectsMap,
            },
            effect_manager::EffectManager,
        },
        id::{EffectId, EntityIdentifier},
        modifier::ModifierSource,
        spells::spellbook::{GrantedSpellSource, SpellSource},
        time::TimeDuration,
    },
    engine::{
        event::{Event, EventListener, ListenerSource},
        game_state::GameState,
    },
    registry::registry::EffectsRegistry,
    systems,
};

/// This gets used so often that it deserves its own function
pub fn effects(world: &World, entity: Entity) -> Ref<'_, EffectManager> {
    systems::helpers::get_component::<EffectManager>(world, entity)
}

pub fn effects_mut(world: &mut World, entity: Entity) -> RefMut<'_, EffectManager> {
    systems::helpers::get_component_mut::<EffectManager>(world, entity)
}

#[derive(Debug, Clone, PartialEq)]
pub enum EffectApplicationResult {
    Added(EffectInstanceId),
    RefreshedDuration(EffectInstanceId),
    // TODO: Does anyone care about the replaced effects?
    // Replaced(Vec<EffectInstance>),
}

impl Into<EffectResultKind> for EffectApplicationResult {
    fn into(self) -> EffectResultKind {
        match self {
            EffectApplicationResult::Added(_) => EffectResultKind::Applied,
            EffectApplicationResult::RefreshedDuration(_) => EffectResultKind::RefreshedDuration,
        }
    }
}

pub fn add_effect_template(
    game_state: &mut GameState,
    applier: Entity,
    target: Entity,
    source: ModifierSource,
    template: &EffectInstanceTemplate,
    context: Option<&ActionContext>,
    action_resolution: ActionConditionResolution,
) -> EffectApplicationResult {
    let (parent_id, mut effect_instances) =
        template.instantiate(applier, target, source, action_resolution);

    debug!(
        "Instantiated effect instances from template {:?} -> {:?}",
        template, effect_instances
    );

    let result = add_effect_instance(
        game_state,
        target,
        parent_id,
        &mut effect_instances,
        context,
    );

    debug!(
        "Result of adding effect instance {:?} to entity {:?}: {:?}",
        parent_id, target, result
    );

    match result {
        EffectApplicationResult::Added(parent_id) => {
            register_end_conditions(game_state, applier, target, &parent_id);
        }
        _ => {}
    }

    result
}

fn register_end_conditions(
    game_state: &mut GameState,
    applier: Entity,
    target: Entity,
    parent_instance: &EffectInstanceId,
) {
    debug!(
        "Registering end conditions for effect instance {:?} on entity {:?}",
        parent_instance, target
    );

    if let Some(parent_instance) = effects(&game_state.world, target).get(parent_instance) {
        // End conditions can come from the applying action (e.g. Hold Person's
        // save DC) or from the effect definition itself (e.g. Rage)
        let end_conditions = parent_instance
            .end_condition
            .iter()
            .cloned()
            .chain(
                parent_instance
                    .effect()
                    .end_conditions
                    .iter()
                    .map(|template| template.instantiate(applier, target)),
            )
            .collect::<Vec<_>>();

        for end_condition in end_conditions {
            game_state.event_dispatcher.register_listener(
                EventListener::new(
                    end_condition.event_filter,
                    end_condition.callback,
                    ListenerSource::EffectInstance {
                        id: parent_instance.instance_id.clone(),
                        entity: target,
                    },
                    false,
                )
                .with_kinds(end_condition.kinds),
            );
        }
    }
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
) -> EffectApplicationResult {
    if let Some(instance) = effect_instances.remove(&instance_id) {
        let effect = instance.effect();

        match instance.effect().stacking_policy {
            EffectStackingPolicy::Stack => { /* Don't need to do anything here? */ }

            EffectStackingPolicy::Replace => {
                if has_effect(game_state, entity, effect.id()) {
                    debug!("Replacing effect {:?} on entity {:?}", effect.id(), entity);
                    remove_effects_by_id(game_state, entity, effect.id());
                }
            }

            EffectStackingPolicy::RefreshDuration => {
                if has_effect(game_state, entity, effect.id()) {
                    debug!(
                        "Refreshing duration of effect {:?} on entity {:?}",
                        effect.id(),
                        entity
                    );
                    refresh_effect_duration(game_state, entity, effect.id());
                    return EffectApplicationResult::RefreshedDuration(instance_id);
                }
            }
        }

        debug!(
            "Adding effect instance to entity {:?}: {:?} ",
            entity, instance,
        );

        apply_and_replace(game_state, entity, &instance, context);

        for child_instance in &instance.children {
            add_effect_instance(
                game_state,
                entity,
                child_instance.clone(),
                effect_instances,
                context,
            );
        }

        effects_mut(&mut game_state.world, entity).insert(instance);
    }

    EffectApplicationResult::Added(instance_id)
}

fn apply_and_replace(
    game_state: &mut GameState,
    entity: Entity,
    effect_instance: &EffectInstance,
    context: Option<&ActionContext>,
) -> Vec<EffectInstance> {
    let effect = effect_instance.effect();

    let replaced = if let Some(replaces) = &effect.replaces {
        // TODO: Not sure how best to find the instance that should be replaced.
        // It's probably always just one?
        remove_effects_by_id(game_state, entity, replaces)
    } else {
        Vec::new()
    };

    if let Some(on_apply) = &effect.on_apply {
        on_apply(game_state, entity, context);
    }

    for action in &effect.actions {
        debug!(
            "Applying action granted by effect {:?} to entity {:?}: {:?}",
            effect.id, entity, action
        );

        match action {
            EffectGrantedAction::Action { id } => {
                // TODO: Might need some work under the hood
                systems::actions::add_actions(&mut game_state.world, entity, &[id.clone()]);
            }
            EffectGrantedAction::Spell { id, level } => {
                let _ = systems::spells::add_spell(
                    game_state,
                    entity,
                    id,
                    &SpellSource::Granted {
                        source: GrantedSpellSource::Effect(effect.id.clone()),
                        level: *level,
                    },
                );
            }
        }
    }

    replaced
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

            for action in &effect.actions {
                debug!(
                    "Removing action granted by effect {:?} from entity {:?}: {:?}",
                    effect.id, entity, action
                );

                match action {
                    EffectGrantedAction::Action { id } => {
                        systems::actions::remove_action(&mut game_state.world, entity, id);
                    }
                    EffectGrantedAction::Spell { id, level } => {
                        let _ = systems::spells::remove_spell(
                            game_state,
                            entity,
                            id,
                            &SpellSource::Granted {
                                source: GrantedSpellSource::Effect(effect.id.clone()),
                                level: *level,
                            },
                        );
                    }
                }
            }

            game_state.event_dispatcher.remove_listeners_by_source(
                &ListenerSource::EffectInstance {
                    id: *instance_id,
                    entity,
                },
            );

            if !effect_instance.is_permanent() && effect_instance.is_parent() {
                game_state.process_event(Event::action_result_event(
                    EntityIdentifier::from_world(&game_state.world, entity),
                    ActionResultComponent::Effect(EffectResult {
                        resolution: ActionConditionResolution::Unconditional,
                        effects: systems::effects::effect_id_and_children(
                            &effect_instance.effect_id,
                        ),
                        result: EffectResultKind::Removed,
                    }),
                ));
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

pub fn has_effect(game_state: &GameState, entity: Entity, effect_id: &EffectId) -> bool {
    effects(&game_state.world, entity)
        .iter()
        .any(|(_, effect_instance)| effect_instance.effect_id == *effect_id)
}

pub fn effect_remaining_duration(
    game_state: &GameState,
    entity: Entity,
    effect_id: &EffectId,
) -> Option<TimeDuration> {
    effects(&game_state.world, entity)
        .iter()
        .find_map(|(_, effect_instance)| {
            if effect_instance.effect_id == *effect_id {
                match effect_instance.lifetime {
                    EffectLifetime::Permanent => Some(TimeDuration::permanent()),
                    EffectLifetime::TurnBoundary { remaining, .. } => Some(remaining),
                }
            } else {
                None
            }
        })
}

fn instances_by_id(
    game_state: &GameState,
    entity: Entity,
    effect_id: &EffectId,
) -> Vec<EffectInstanceId> {
    effects(&game_state.world, entity)
        .iter()
        .filter_map(|(id, effect_instance)| {
            if effect_instance.effect_id == *effect_id {
                Some(id.clone())
            } else {
                None
            }
        })
        .collect()
}

fn children_by_parent_id(
    game_state: &GameState,
    entity: Entity,
    parent_id: &EffectInstanceId,
) -> Vec<EffectInstanceId> {
    effects(&game_state.world, entity)
        .iter()
        .filter_map(|(id, effect_instance)| {
            if effect_instance.parent == Some(parent_id.clone()) {
                Some(id.clone())
            } else {
                None
            }
        })
        .collect()
}

fn instances_and_children_by_id(
    game_state: &GameState,
    entity: Entity,
    effect_id: &EffectId,
) -> Vec<EffectInstanceId> {
    let mut instances = instances_by_id(game_state, entity, effect_id);
    let mut children = Vec::new();

    for instance_id in &instances {
        children.extend(children_by_parent_id(game_state, entity, instance_id));
    }

    instances.extend(children);
    instances
}

pub fn extend_effect_duration(
    game_state: &mut GameState,
    entity: Entity,
    effect_id: &EffectId,
    duration: &TimeDuration,
) {
    let instance_ids = instances_and_children_by_id(game_state, entity, effect_id);

    let mut effects = effects_mut(&mut game_state.world, entity);
    for instance_id in instance_ids {
        if let Some(effect_instance) = effects.effects.get_mut(&instance_id) {
            effect_instance.extend_remaing_duration(duration);
        }
    }
}

pub fn refresh_effect_duration(game_state: &mut GameState, entity: Entity, effect_id: &EffectId) {
    let instance_ids = instances_and_children_by_id(game_state, entity, effect_id);

    let mut effects = effects_mut(&mut game_state.world, entity);
    for instance_id in instance_ids {
        if let Some(effect_instance) = effects.effects.get_mut(&instance_id) {
            effect_instance.refresh_remaining_duration();
        }
    }
}

pub fn effect_id_and_children(effect_id: &EffectId) -> Vec<EffectId> {
    // Root first, then children recursively
    let mut ids = vec![effect_id.clone()];

    let effect = EffectsRegistry::get(effect_id)
        .expect(format!("Effect definition not found for {}", effect_id).as_str());

    for child in &effect.children {
        ids.extend(effect_id_and_children(child));
    }

    ids
}
