use hecs::{Entity, World};
use nat20_core::{
    components::items::{
        equipment::slots::EquipmentSlot,
        inventory::{Inventory, ItemContainer, ItemInstance},
        item::Item,
    },
    engine::game_state::GameState,
    registry::registry::ItemsRegistry,
    systems,
};
use strum::IntoEnumIterator;
use tracing::{debug, error, info};

use crate::{
    render::ui::{
        text::item_rarity_color,
        utils::{ImguiRenderable, ImguiRenderableWithContext, render_uniform_buttons},
    },
    table_with_columns,
};

#[derive(Debug, Clone)]
pub enum ContainerSlot {
    Inventory(usize),
    Loadout(EquipmentSlot),
}

#[derive(Debug, Clone)]
pub enum InteractMode {
    RightClick,
    DoubleClick,
    Drag,
}

#[derive(Debug, Clone)]
pub struct InteractEvent {
    pub entity: Entity,
    pub slot: ContainerSlot,
    pub mode: InteractMode,
}

impl InteractEvent {
    pub fn from_ui(ui: &imgui::Ui, entity: Entity, slot: ContainerSlot) -> Option<Self> {
        if ui.is_item_hovered() {
            if ui.is_mouse_released(imgui::MouseButton::Right) {
                return Some(Self {
                    entity,
                    slot,
                    mode: InteractMode::RightClick,
                });
            }

            if ui.is_mouse_double_clicked(imgui::MouseButton::Left) {
                return Some(Self {
                    entity,
                    slot,
                    mode: InteractMode::DoubleClick,
                });
            }

            if ui.is_mouse_dragging(imgui::MouseButton::Left) {
                return Some(Self {
                    entity,
                    slot,
                    mode: InteractMode::Drag,
                });
            }
        }
        None
    }
}

fn render_item_button(ui: &imgui::Ui, item: &Item, index: usize) -> bool {
    let words = item.name.split_whitespace();
    // Render first three lettes of each word
    let short_name = words
        .map(|word| word.chars().take(3).collect::<String>())
        .collect::<Vec<_>>()
        .join("\n");

    let background_color = item_rarity_color(&item.rarity);

    let style = ui.push_style_var(imgui::StyleVar::FrameBorderSize(1.0));
    let color = ui.push_style_color(imgui::StyleColor::Border, background_color);

    let clicked = ui.button_with_size(format!("{}##{}", short_name, index), [30.0, 30.0]);

    style.pop();
    color.pop();

    clicked
}

static INVENTORY_ITEMS_PER_ROW: usize = 8;

pub fn render_inventory(
    ui: &imgui::Ui,
    world: &mut World,
    entity: Entity,
) -> Option<InteractEvent> {
    ui.separator_with_text("Inventory");

    let inventory = systems::helpers::get_component::<Inventory>(world, entity);
    inventory.money().render(ui);

    let mut event = None;
    let items = inventory.items();
    let rows = (items.len() + INVENTORY_ITEMS_PER_ROW) / INVENTORY_ITEMS_PER_ROW;
    let total_items = rows * INVENTORY_ITEMS_PER_ROW;
    for i in 0..total_items {
        let slot = ContainerSlot::Inventory(i);

        if i < items.len() {
            let item_name = items[i].item().name.clone();
            if render_item_button(ui, items[i].item(), i) {
                // Handle item click (don't think we need to do anything here)
                debug!("Clicked on item: {}", item_name);
            }

            if ui.is_item_hovered() {
                ui.tooltip(|| {
                    items[i].render_with_context(ui, (world, entity));
                });
            }
        } else {
            // Render empty button for unused slots
            ui.button_with_size(format!("##{}", i), [30.0, 30.0]);
        }

        if event.is_none() {
            event = InteractEvent::from_ui(ui, entity, slot);
        }

        if (i + 1) % INVENTORY_ITEMS_PER_ROW != 0 && i + 1 < total_items {
            ui.same_line();
        }
    }

    event
}

pub fn render_loadout(ui: &imgui::Ui, world: &World, entity: Entity) -> Option<InteractEvent> {
    let loadout = systems::loadout::loadout(world, entity);
    let mut event = None;

    if let Some(table) = table_with_columns!(ui, "Loadout", "Slot", "Item") {
        for (i, slot) in EquipmentSlot::iter().enumerate() {
            // Slot column
            ui.table_next_column();
            ui.text(slot.to_string());
            // Item column
            ui.table_next_column();
            let item = loadout.item_in_slot(&slot);
            if let Some(item) = item {
                if render_item_button(ui, item.item(), i) {
                    // Handle item click (don't think we need to do anything here)
                    debug!("Clicked on loadout item: {}", item.item().name);
                }

                if ui.is_item_hovered() {
                    ui.tooltip(|| {
                        // TODO: Consider implementing a dedicated render method for EquipmentInstance
                        let item_instance: ItemInstance = item.clone().into();
                        item_instance.render_with_context(ui, (world, entity));
                    });
                }

                if event.is_none() {
                    event = InteractEvent::from_ui(ui, entity, ContainerSlot::Loadout(slot));
                }
            } else {
                // Render empty button for unused slots
                ui.button_with_size(format!("##{}", slot), [30.0, 30.0]);
            }
        }

        table.end();
    } else {
        ui.text("No loadout available.");
    }

    event
}

pub fn render_loadout_inventory(ui: &imgui::Ui, game_state: &mut GameState, entity: Entity) {
    if let Some(event) = render_loadout(ui, &game_state.world, entity) {
        // Handle loadout interaction event
        debug!("Loadout interaction: {:?}", event);

        let ContainerSlot::Loadout(slot) = event.slot else {
            return;
        };

        match event.mode {
            InteractMode::RightClick => {
                // Handle right-click on loadout item
                debug!("Right-clicked on loadout item: {:?}", event.slot);
            }

            InteractMode::DoubleClick => {
                let result = systems::inventory::unequip(game_state, entity, &slot);
                if let Some(item) = result {
                    debug!("Unequipped item: {:?}", item);
                    systems::inventory::add_item(&mut game_state.world, entity, item);
                }
            }

            InteractMode::Drag => {
                // Handle drag on loadout item
                debug!("Dragging loadout item: {:?}", event.slot);
            }
        }
    }

    if let Some(event) = render_inventory(ui, &mut game_state.world, entity) {
        // Handle inventory interaction event
        debug!("Inventory interaction: {:?}", event);

        let ContainerSlot::Inventory(index) = event.slot else {
            return;
        };

        let item = systems::helpers::get_component::<Inventory>(&game_state.world, entity)
            .items()
            .get(index)
            .cloned();

        if let Some(item) = item {
            match event.mode {
                InteractMode::RightClick => {
                    // Handle right-click on inventory item
                    debug!("Right-clicked on inventory item: {:?}", item.item().name);
                }

                InteractMode::DoubleClick => {
                    // Try to equip the item
                    let item_name = item.item().name.clone();
                    debug!("Double-clicked on inventory item: {:?}", item_name);
                    let result = systems::inventory::equip(game_state, entity, item);
                    match result {
                        Ok(unequipped_items) => {
                            info!("Equipped item: {:?}", item_name);
                            // Remove the item that was equipped from the inventory
                            systems::inventory::remove_item(&mut game_state.world, entity, index);
                            for unequipped_item in unequipped_items {
                                info!("Unequipped item: {:?}", unequipped_item);
                                // Add unequipped items back to inventory
                                systems::inventory::add_item(
                                    &mut game_state.world,
                                    entity,
                                    unequipped_item,
                                );
                            }
                        }
                        Err(err) => {
                            error!("Failed to equip item: {:?}", err);
                        }
                    }
                }

                InteractMode::Drag => {
                    // Handle drag on inventory item
                    debug!("Dragging inventory item: {:?}", item.item().name);
                }
            }
        } else {
            match InteractMode::RightClick {
                InteractMode::RightClick => {
                    debug!("Right-clicked on empty inventory slot: {:?}", index);
                    ui.open_popup("spawn_item_popup");
                }
                InteractMode::DoubleClick => {
                    debug!("Double-clicked on empty inventory slot: {:?}", index);
                }
                InteractMode::Drag => {
                    debug!("Dragging empty inventory slot: {:?}", index);
                }
            }
        }
    }

    ui.popup("spawn_item_popup", || {
        let items = ItemsRegistry::keys().collect::<Vec<_>>();

        let strings = items
            .iter()
            .map(|item| item.to_string())
            .collect::<Vec<_>>();

        if let Some(selected_item) = render_uniform_buttons(ui, strings) {
            let item =
                ItemsRegistry::get(items[selected_item]).expect("Item should exist in registry");
            systems::inventory::add_item(&mut game_state.world, entity, item.clone());
            ui.close_current_popup();
        }
    });
}

impl ImguiRenderableWithContext<(&World, Entity)> for ItemInstance {
    fn render_with_context(&self, ui: &imgui::Ui, context: (&World, Entity)) {
        match self {
            ItemInstance::Weapon(weapon) => {
                weapon.render_with_context(ui, context);
            }
            ItemInstance::Armor(armor) => {
                armor.render_with_context(ui, context);
            }
            _ => {
                ui.text("Placeholder tooltip :^)");
            }
        }
    }
}
