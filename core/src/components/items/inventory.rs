use schemars::JsonSchema;
use serde::Deserialize;

use crate::components::{
    id::{EffectId, IdProvider, ItemId},
    items::{
        equipment::{
            armor::Armor,
            equipment::EquipmentItem,
            slots::{EquipmentSlot, SlotProvider},
            weapon::Weapon,
        },
        item::Item,
        money::{MonetaryValue, MonetaryValueError},
    },
};

#[derive(Debug, Clone, PartialEq, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ItemInstance {
    Item(Item),
    Armor(Armor),
    Weapon(Weapon),
    Equipment(EquipmentItem),
}

impl ItemInstance {
    pub fn is_equippable(&self) -> bool {
        matches!(
            self,
            ItemInstance::Armor(_) | ItemInstance::Weapon(_) | ItemInstance::Equipment(_)
        )
    }

    pub fn effects(&self) -> &[EffectId] {
        match self {
            ItemInstance::Armor(armor) => armor.effects(),
            ItemInstance::Weapon(weapon) => weapon.effects(),
            ItemInstance::Equipment(equipment) => &equipment.effects,
            _ => &[],
        }
    }
}

impl SlotProvider for ItemInstance {
    fn valid_slots(&self) -> &'static [EquipmentSlot] {
        match self {
            ItemInstance::Armor(armor) => armor.valid_slots(),
            ItemInstance::Weapon(weapon) => weapon.valid_slots(),
            ItemInstance::Equipment(equipment) => equipment.valid_slots(),
            _ => &[],
        }
    }

    fn required_slots(&self) -> &'static [EquipmentSlot] {
        match self {
            ItemInstance::Armor(armor) => armor.required_slots(),
            ItemInstance::Weapon(weapon) => weapon.required_slots(),
            ItemInstance::Equipment(equipment) => equipment.required_slots(),
            _ => &[],
        }
    }
}

impl IdProvider for ItemInstance {
    type Id = ItemId;

    fn id(&self) -> &Self::Id {
        match self {
            ItemInstance::Item(item) => &item.id,
            ItemInstance::Armor(armor) => &armor.item.id,
            ItemInstance::Weapon(weapon) => &weapon.item().id,
            ItemInstance::Equipment(equipment) => &equipment.item.id,
        }
    }
}

pub trait ItemContainer {
    fn item(&self) -> &Item;
}

impl ItemContainer for ItemInstance {
    fn item(&self) -> &Item {
        match self {
            ItemInstance::Item(item) => item,
            ItemInstance::Armor(armor) => &armor.item,
            ItemInstance::Weapon(weapon) => weapon.item(),
            ItemInstance::Equipment(equipment) => &equipment.item,
        }
    }
}

macro_rules! impl_into_item_instance {
    ($($ty:ty => $variant:ident),* $(,)?) => {
        $(
            impl From<$ty> for ItemInstance {
                fn from(value: $ty) -> Self {
                    ItemInstance::$variant(value)
                }
            }
        )*
    };
}

impl_into_item_instance! {
    Item => Item,
    Armor => Armor,
    Weapon => Weapon,
    EquipmentItem => Equipment,
}

#[derive(Debug, Clone)]
pub struct Inventory {
    items: Vec<ItemId>,
    money: MonetaryValue,
}

impl Default for Inventory {
    fn default() -> Self {
        Self::new()
    }
}

impl Inventory {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            money: MonetaryValue::new(),
        }
    }

    pub fn add_item(&mut self, item: ItemId) {
        self.items.push(item);
    }

    pub fn remove_item(&mut self, index: usize) -> Option<ItemId> {
        if index < self.items.len() {
            Some(self.items.remove(index))
        } else {
            None
        }
    }

    pub fn items(&self) -> &[ItemId] {
        &self.items
    }

    /// Optional: find by name
    // pub fn find_by_name(&self, name: &str) -> Option<&ItemInstance> {
    //     self.items.iter().find(|i| i.item().name == name)
    // }

    pub fn money(&self) -> &MonetaryValue {
        &self.money
    }

    pub fn add_money(&mut self, amount: MonetaryValue) {
        for (currency, value) in amount.values.into_iter() {
            self.money.add(currency, value);
        }
    }

    pub fn remove_money(&mut self, amount: MonetaryValue) -> Result<(), MonetaryValueError> {
        for (currency, value) in amount.values.into_iter() {
            self.money.remove(currency, value)?;
        }
        Ok(())
    }
}
