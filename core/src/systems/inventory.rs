use hecs::{Entity, World};

use crate::{
    components::{
        id::ItemId,
        items::{
            equipment::{loadout::TryEquipError, slots::EquipmentSlot},
            inventory::Inventory,
            money::{MonetaryValue, MonetaryValueError},
        },
    },
    engine::engine_state::EngineState,
    systems,
};

pub fn equip(
    engine_state: &mut EngineState,
    entity: Entity,
    item_id: &ItemId,
) -> Result<Vec<ItemId>, TryEquipError> {
    systems::loadout::equip(engine_state, entity, item_id)
}

pub fn unequip(
    engine_state: &mut EngineState,
    entity: Entity,
    slot: &EquipmentSlot,
) -> Option<ItemId> {
    systems::loadout::unequip(engine_state, entity, slot)
}

pub fn add_item(world: &mut World, entity: Entity, item: ItemId) {
    systems::helpers::get_component_mut::<Inventory>(world, entity).add_item(item);
}

pub fn remove_item(world: &mut World, entity: Entity, index: usize) -> Option<ItemId> {
    systems::helpers::get_component_mut::<Inventory>(world, entity).remove_item(index)
}

pub fn add_money(world: &mut World, entity: Entity, amount: MonetaryValue) {
    systems::helpers::get_component_mut::<Inventory>(world, entity).add_money(amount);
}

pub fn remove_money(
    world: &mut World,
    entity: Entity,
    amount: MonetaryValue,
) -> Result<(), MonetaryValueError> {
    systems::helpers::get_component_mut::<Inventory>(world, entity).remove_money(amount)
}
