use hecs::{Entity, World};

use crate::{
    components::items::{
        equipment::{
            loadout::{EquipmentInstance, TryEquipError},
            slots::EquipmentSlot,
        },
        inventory::{Inventory, ItemInstance},
        money::{MonetaryValue, MonetaryValueError},
    },
    engine::engine_state::EngineState,
    systems,
};

pub fn equip<T>(
    engine_state: &mut EngineState,
    entity: Entity,
    item: T,
) -> Result<Vec<ItemInstance>, TryEquipError>
where
    T: Into<ItemInstance>,
{
    let item: ItemInstance = item.into();
    let equipment: EquipmentInstance = item.into();

    let unequippped_items = systems::loadout::equip(engine_state, entity, equipment)?;

    Ok(unequippped_items
        .iter()
        .map(|item| <EquipmentInstance as Into<ItemInstance>>::into(item.clone()))
        .collect::<Vec<ItemInstance>>())
}

pub fn unequip(
    engine_state: &mut EngineState,
    entity: Entity,
    slot: &EquipmentSlot,
) -> Option<ItemInstance> {
    let unequipped_item = systems::loadout::unequip(engine_state, entity, slot);
    unequipped_item.map(|item| item.into())
}

pub fn add_item<T>(world: &mut World, entity: Entity, item: T)
where
    T: Into<ItemInstance>,
{
    systems::helpers::get_component_mut::<Inventory>(world, entity).add_item(item.into());
}

pub fn remove_item(world: &mut World, entity: Entity, index: usize) -> Option<ItemInstance> {
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
