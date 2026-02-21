use hecs::{Entity, Ref, World};

use crate::{
    components::{
        actions::targeting::TargetingRange,
        damage::DamageRoll,
        items::{
            equipment::{
                armor::ArmorClass,
                loadout::{EquipmentInstance, Loadout, TryEquipError},
                slots::EquipmentSlot,
            },
            inventory::ItemContainer,
        },
        modifier::ModifierSource,
    },
    engine::game_state::GameState,
    systems,
};

pub fn loadout(world: &World, entity: Entity) -> Ref<'_, Loadout> {
    systems::helpers::get_component::<Loadout>(world, entity)
}

pub fn loadout_mut(world: &mut World, entity: Entity) -> hecs::RefMut<'_, Loadout> {
    systems::helpers::get_component_mut::<Loadout>(world, entity)
}

pub fn equip_in_slot<T>(
    game_state: &mut GameState,
    entity: Entity,
    slot: &EquipmentSlot,
    equipment: T,
) -> Result<Vec<EquipmentInstance>, TryEquipError>
where
    T: Into<EquipmentInstance>,
{
    let equipment = equipment.into();
    let item_id = equipment.item().id.clone();

    let unequipped_items =
        loadout_mut(&mut game_state.world, entity).equip_in_slot(slot, equipment)?;
    for unequipped_item in &unequipped_items {
        systems::effects::remove_effects_by_source(
            game_state,
            entity,
            &ModifierSource::Item(unequipped_item.item().id.clone()),
        );
    }

    let effects = loadout(&game_state.world, entity)
        .item_in_slot(slot)
        .unwrap()
        .effects()
        .clone();
    systems::effects::add_permanent_effects(
        game_state,
        entity,
        effects,
        &ModifierSource::Item(item_id),
        None,
    );

    Ok(unequipped_items)
}

pub fn equip<T>(
    game_state: &mut GameState,
    entity: Entity,
    equipment: T,
) -> Result<Vec<EquipmentInstance>, TryEquipError>
where
    T: Into<EquipmentInstance>,
{
    let equipment = equipment.into();
    let item_id = equipment.item().id.clone();
    // TODO: Slightly less performant than calling `equip_in_slot` directly
    let effects = equipment.effects().clone();

    let unequipped_items = loadout_mut(&mut game_state.world, entity).equip(equipment)?;
    for unequipped_item in &unequipped_items {
        systems::effects::remove_effects_by_source(
            game_state,
            entity,
            &ModifierSource::Item(unequipped_item.item().id.clone()),
        );
    }

    systems::effects::add_permanent_effects(
        game_state,
        entity,
        effects,
        &ModifierSource::Item(item_id),
        None,
    );

    Ok(unequipped_items)
}

pub fn unequip(
    game_state: &mut GameState,
    entity: Entity,
    slot: &EquipmentSlot,
) -> Option<EquipmentInstance> {
    let unequipped_item = loadout_mut(&mut game_state.world, entity).unequip(slot);
    if let Some(item) = &unequipped_item {
        systems::effects::remove_effects_by_source(
            game_state,
            entity,
            &ModifierSource::Item(item.item().id.clone()),
        );
    }
    unequipped_item
}

pub fn armor_class(world: &World, entity: Entity) -> ArmorClass {
    loadout(world, entity).armor_class(world, entity)
}

pub fn can_equip(world: &World, entity: Entity, equipment: &EquipmentInstance) -> bool {
    loadout(world, entity).can_equip(equipment)
}

pub fn weapon_damage_roll(world: &World, entity: Entity, slot: &EquipmentSlot) -> DamageRoll {
    loadout(world, entity).damage_roll(world, entity, slot)
}
