use hecs::{Entity, Ref, World};

use crate::{
    components::{
        actions::{
            action::{ActionAttackKind, ActionContext},
            targeting::TargetingRange,
        },
        damage::DamageRoll,
        id::{EntityIdentifier, ItemId},
        items::equipment::{
            armor::ArmorClass,
            loadout::{Loadout, TryEquipError},
            slots::EquipmentSlot,
            weapon::MELEE_RANGE_DEFAULT,
        },
        modifier::ModifierSource,
    },
    engine::{
        engine_state::EngineState,
        event::{Event, EventKind},
    },
    registry::registry::ItemsRegistry,
    systems,
};

pub fn loadout(world: &World, entity: Entity) -> Ref<'_, Loadout> {
    systems::helpers::get_component::<Loadout>(world, entity)
}

pub fn loadout_mut(world: &mut World, entity: Entity) -> &mut Loadout {
    systems::helpers::get_component_mut::<Loadout>(world, entity)
}

pub fn equip_in_slot(
    engine_state: &mut EngineState,
    entity: Entity,
    slot: &EquipmentSlot,
    item_id: &ItemId,
) -> Result<Vec<ItemId>, TryEquipError> {
    let unequipped_items =
        loadout_mut(&mut engine_state.world, entity).equip_in_slot(slot, item_id)?;

    update_equipment_effects(engine_state, entity, item_id, unequipped_items)
}

pub fn equip(
    engine_state: &mut EngineState,
    entity: Entity,
    item_id: &ItemId,
) -> Result<Vec<ItemId>, TryEquipError> {
    let unequipped_items = loadout_mut(&mut engine_state.world, entity).equip(item_id)?;

    update_equipment_effects(engine_state, entity, item_id, unequipped_items)
}

fn update_equipment_effects(
    engine_state: &mut EngineState,
    entity: Entity,
    equipped_item_id: &ItemId,
    unequipped_items: Vec<ItemId>,
) -> Result<Vec<ItemId>, TryEquipError> {
    for unequipped_item in &unequipped_items {
        systems::effects::remove_effects_by_source(
            engine_state,
            entity,
            &ModifierSource::Item(unequipped_item.clone()),
        );
        equipment_changed_event(engine_state, entity, unequipped_item.clone(), false);
    }

    if let Some(equipped_item) = ItemsRegistry::get(&equipped_item_id) {
        systems::effects::add_permanent_effects(
            engine_state,
            entity,
            equipped_item.effects(),
            &ModifierSource::Item(equipped_item_id.clone()),
            None,
        );
        equipment_changed_event(engine_state, entity, equipped_item_id.clone(), true);
    }

    Ok(unequipped_items)
}

pub fn unequip(
    engine_state: &mut EngineState,
    entity: Entity,
    slot: &EquipmentSlot,
) -> Option<ItemId> {
    let unequipped_item = loadout_mut(&mut engine_state.world, entity).unequip(slot);
    if let Some(item) = &unequipped_item {
        systems::effects::remove_effects_by_source(
            engine_state,
            entity,
            &ModifierSource::Item(item.clone()),
        );
        equipment_changed_event(engine_state, entity, item.clone(), false);
    }
    unequipped_item
}

fn equipment_changed_event(
    engine_state: &mut EngineState,
    entity: Entity,
    item: ItemId,
    equipped: bool,
) {
    engine_state.process_event(Event::new(EventKind::EquipmentChanged {
        entity: EntityIdentifier::from_world(&engine_state.world, entity),
        item,
        equipped,
    }));
}

pub fn armor_class(engine_state: &EngineState, entity: Entity) -> ArmorClass {
    loadout(&engine_state.world, entity).armor_class(engine_state, entity)
}

pub fn can_equip(world: &World, entity: Entity, item: &ItemId) -> bool {
    loadout(world, entity).can_equip(item)
}

pub fn weapon_damage_roll(world: &World, entity: Entity, slot: &EquipmentSlot) -> DamageRoll {
    loadout(world, entity).weapon_damage_roll(world, entity, slot)
}

pub fn attack_damage_roll(world: &World, entity: Entity, context: &ActionContext) -> DamageRoll {
    loadout(world, entity).damage_roll_from_context(world, entity, context)
}

pub fn attack_targeting_range(
    world: &World,
    entity: Entity,
    context: &ActionContext,
) -> TargetingRange {
    let loadout = loadout(world, entity);
    let attack = context
        .attack
        .as_ref()
        .expect("Attack targeting requires an attack context");

    match attack.kind {
        ActionAttackKind::MeleeWeapon | ActionAttackKind::RangedWeapon => loadout
            .weapon_in_hand(
                attack
                    .slot
                    .as_ref()
                    .expect("Weapon attacks require an equipment slot"),
            )
            .expect("No weapon equipped in the specified slot")
            .range()
            .clone(),
        ActionAttackKind::Unarmed => MELEE_RANGE_DEFAULT.clone(),
    }
}
