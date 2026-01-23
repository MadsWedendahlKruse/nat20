use hecs::{Entity, Ref, World};
use tracing::debug;
use uuid::Uuid;

use crate::{
    components::{
        actions::action::ActionContext,
        effects::effect::{EffectInstance, EffectInstanceId, EffectInstanceTemplate, EffectsMap},
        id::EffectId,
        modifier::ModifierSource,
    },
    systems,
};

/// This gets used so often that it deserves its own function
pub fn effects(world: &World, entity: Entity) -> Ref<'_, EffectsMap> {
    systems::helpers::get_component::<EffectsMap>(world, entity)
}

pub fn effects_mut(world: &mut World, entity: Entity) -> hecs::RefMut<'_, EffectsMap> {
    systems::helpers::get_component_mut::<EffectsMap>(world, entity)
}

pub fn add_effect_template(
    world: &mut World,
    applier: Entity,
    target: Entity,
    source: ModifierSource,
    template: &EffectInstanceTemplate,
    context: Option<&ActionContext>,
) -> EffectInstanceId {
    let (parent_id, mut effect_instances) = template.instantiate(applier, target, source);
    debug!(
        "Instantiated effect instances from template\n{:#?} ->\n{:#?}",
        template, effect_instances
    );
    add_effect_instance(world, target, parent_id, &mut effect_instances, context);
    parent_id
}

pub fn add_permanent_effect(
    world: &mut World,
    entity: Entity,
    effect_id: EffectId,
    source: &ModifierSource,
    context: Option<&ActionContext>,
) {
    let effect_instance = EffectInstance::permanent(effect_id.clone(), source.clone());
    let instance_id = Uuid::new_v4();
    apply_and_replace(world, entity, &effect_instance, context);
    effects_mut(world, entity).insert(instance_id, effect_instance);
}

pub fn add_permanent_effects(
    world: &mut World,
    entity: Entity,
    effects: Vec<EffectId>,
    source: &ModifierSource,
    context: Option<&ActionContext>,
) {
    for effect_id in effects {
        add_permanent_effect(world, entity, effect_id, source, context);
    }
}

fn add_effect_instance(
    world: &mut World,
    entity: Entity,
    instance_id: EffectInstanceId,
    effect_instances: &mut EffectsMap,
    context: Option<&ActionContext>,
) {
    if let Some(instance) = effect_instances.remove(&instance_id) {
        debug!(
            "Adding effect instance (id: {:?}) {:?} to entity {:?}",
            instance_id, instance, entity
        );
        apply_and_replace(world, entity, &instance, context);
        for child_instance in &instance.children {
            add_effect_instance(
                world,
                entity,
                child_instance.clone(),
                effect_instances,
                context,
            );
        }
        effects_mut(world, entity).insert(instance_id, instance);
    }
}

fn apply_and_replace(
    world: &mut World,
    entity: Entity,
    effect_instance: &EffectInstance,
    context: Option<&ActionContext>,
) {
    let effect = effect_instance.effect();
    (effect.on_apply)(world, entity, context);
    if let Some(replaces) = &effect.replaces {
        // TODO: Not sure how best to find the instance that shouold be replaced.
        // It's probably always just one?
        remove_effects_by_id(world, entity, replaces);
    }
}

pub fn remove_effect(world: &mut World, entity: Entity, instance_id: &EffectInstanceId) {
    debug!("Removing effect {:?} from entity {:?}", instance_id, entity);
    if let Ok(effects) = world.query_one_mut::<&mut EffectsMap>(entity) {
        if let Some(effect_instance) = effects.remove(instance_id) {
            let effect = effect_instance.effect();
            (effect.on_unapply)(world, entity);

            for child_id in effect_instance.children.iter() {
                remove_effect(world, entity, child_id);
            }
        }
    }
}

pub fn remove_effects(world: &mut World, entity: Entity, effects: &[EffectInstanceId]) {
    for effect_id in effects {
        remove_effect(world, entity, effect_id);
    }
}

pub fn remove_effects_by_source(world: &mut World, entity: Entity, source: &ModifierSource) {
    let effect_ids: Vec<EffectInstanceId> = effects(world, entity)
        .iter()
        .filter_map(|(id, effect_instance)| {
            if &effect_instance.source == source {
                Some(id.clone())
            } else {
                None
            }
        })
        .collect();

    for effect_id in effect_ids {
        remove_effect(world, entity, &effect_id);
    }
}

pub fn remove_effects_by_id(world: &mut World, entity: Entity, effect_id: &EffectId) {
    let instance_ids: Vec<EffectInstanceId> = effects(world, entity)
        .iter()
        .filter_map(|(id, effect_instance)| {
            if &effect_instance.effect_id == effect_id {
                Some(id.clone())
            } else {
                None
            }
        })
        .collect();

    for instance_id in instance_ids {
        remove_effect(world, entity, &instance_id);
    }
}
