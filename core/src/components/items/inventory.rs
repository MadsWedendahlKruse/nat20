use schemars::JsonSchema;
use serde::Deserialize;

use crate::components::{
    id::{IdProvider, ItemId},
    items::{
        equipment::{
            armor::Armor, equipment::EquipmentItem, loadout::EquipmentInstance, weapon::Weapon,
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
    pub fn equipable(&self) -> bool {
        matches!(
            self,
            ItemInstance::Armor(_) | ItemInstance::Weapon(_) | ItemInstance::Equipment(_)
        )
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
            impl Into<ItemInstance> for $ty {
                fn into(self) -> ItemInstance {
                    ItemInstance::$variant(self)
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

impl From<ItemInstance> for EquipmentInstance {
    fn from(item: ItemInstance) -> EquipmentInstance {
        match item {
            ItemInstance::Armor(armor) => EquipmentInstance::Armor(armor),
            ItemInstance::Weapon(weapon) => EquipmentInstance::Weapon(weapon),
            ItemInstance::Equipment(equipment) => EquipmentInstance::Equipment(equipment),
            _ => panic!("Cannot convert ItemInstance::Item to EquipmentInstance"),
        }
    }
}

impl Into<ItemInstance> for EquipmentInstance {
    fn into(self) -> ItemInstance {
        match self {
            EquipmentInstance::Armor(armor) => ItemInstance::Armor(armor),
            EquipmentInstance::Weapon(weapon) => ItemInstance::Weapon(weapon),
            EquipmentInstance::Equipment(equipment) => ItemInstance::Equipment(equipment),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Inventory {
    items: Vec<ItemInstance>,
    money: MonetaryValue,
}

impl Inventory {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            money: MonetaryValue::new(),
        }
    }

    pub fn add_item(&mut self, item: ItemInstance) {
        self.items.push(item);
    }

    pub fn remove_item(&mut self, index: usize) -> Option<ItemInstance> {
        if index < self.items.len() {
            Some(self.items.remove(index))
        } else {
            None
        }
    }

    pub fn items(&self) -> &[ItemInstance] {
        &self.items
    }

    /// Optional: find by name
    pub fn find_by_name(&self, name: &str) -> Option<&ItemInstance> {
        self.items.iter().find(|i| i.item().name == name)
    }

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
