use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::ActionCooldownMap,
        id::ResourceId,
        resource::{
            DEFAULT_RESOURCES, RechargeRule, ResourceAmount, ResourceAmountMap, ResourceError,
            ResourceMap,
        },
    },
    registry::registry::ResourcesRegistry,
    systems,
};

// TODO: No idea where to put this
pub fn recharge_rule(resource: &ResourceId) -> RechargeRule {
    ResourcesRegistry::get(resource)
        .map(|res_def| res_def.recharge.clone())
        .expect(format!("Missing resource definition for resource ID `{}`", resource).as_str())
}

pub fn recharge(world: &mut World, entity: Entity, rest_type: &RechargeRule) {
    for (resource_id, resource) in
        systems::helpers::get_component_mut::<ResourceMap>(world, entity).iter_mut()
    {
        if let Some(resource_definition) = ResourcesRegistry::get(&resource_id) {
            if resource_definition.recharge.is_recharged_by(rest_type) {
                resource.recharge_full();
            }
        }
    }

    systems::helpers::get_component_mut::<ActionCooldownMap>(world, entity)
        .retain(|_, recharge_rule| !recharge_rule.is_recharged_by(rest_type));
}

pub fn can_afford(
    world: &World,
    entity: Entity,
    cost: &ResourceAmountMap,
) -> (bool, Option<ResourceId>) {
    systems::helpers::get_component::<ResourceMap>(world, entity).can_afford_all(cost)
}

pub fn spend(
    world: &mut World,
    entity: Entity,
    cost: &ResourceAmountMap,
) -> Result<(), ResourceError> {
    systems::helpers::get_component_mut::<ResourceMap>(world, entity).spend_all(cost)
}

pub fn restore(
    world: &mut World,
    entity: Entity,
    restoration: &ResourceAmountMap,
) -> Result<(), ResourceError> {
    systems::helpers::get_component_mut::<ResourceMap>(world, entity).restore_all(restoration)
}

pub fn can_act(world: &World, entity: Entity) -> (bool, Option<ResourceId>) {
    // Presumably the entity can't act if they don't have any action, bonus action,
    // or reaction resources left
    can_afford(
        world,
        entity,
        &ResourceAmountMap::from_iter(
            DEFAULT_RESOURCES
                .iter()
                .map(|res_id| (res_id.clone(), ResourceAmount::Flat(1))),
        ),
    )
}
