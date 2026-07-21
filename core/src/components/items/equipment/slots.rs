use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, Display, EnumIter, Serialize, Deserialize, JsonSchema,
)]
#[serde(rename_all = "snake_case")]
pub enum EquipmentSlot {
    Headwear,
    Cloak,
    Gloves,
    Boots,
    Amulet,
    Ring1,
    Ring2,
    Armor,
    MeleeMainHand,
    MeleeOffHand,
    RangedMainHand,
    RangedOffHand,
}

impl EquipmentSlot {
    pub fn weapon_slots() -> &'static [EquipmentSlot] {
        &[
            EquipmentSlot::MeleeMainHand,
            EquipmentSlot::MeleeOffHand,
            EquipmentSlot::RangedMainHand,
            EquipmentSlot::RangedOffHand,
        ]
    }

    pub fn is_weapon_slot(&self) -> bool {
        Self::weapon_slots().contains(self)
    }

    pub fn other_hand(&self) -> Option<EquipmentSlot> {
        match self {
            EquipmentSlot::MeleeMainHand => Some(EquipmentSlot::MeleeOffHand),
            EquipmentSlot::MeleeOffHand => Some(EquipmentSlot::MeleeMainHand),
            EquipmentSlot::RangedMainHand => Some(EquipmentSlot::RangedOffHand),
            EquipmentSlot::RangedOffHand => Some(EquipmentSlot::RangedMainHand),
            _ => None,
        }
    }
}

pub trait SlotProvider {
    /// Returns the valid slots for this equipment item. Valid slots are the slots
    /// where the item *can* be equipped. Rings can be equipped in either of the
    /// `Ring1` or `Ring2` slots, but it makes no difference which one is used.
    fn valid_slots(&self) -> &'static [EquipmentSlot];

    /// Returns the required slots for this equipment item. Required slots are the
    /// slots that the item *must* be equipped in to function properly.
    ///
    /// This is usually empty, indication no specific requirements, but in some
    /// cases (probably only two-handed weapons) it can differ. For a two-handed
    /// weapon, it *must* by definition occupy both the main hand and off hand slots.
    fn required_slots(&self) -> &'static [EquipmentSlot] {
        &[]
    }
}
