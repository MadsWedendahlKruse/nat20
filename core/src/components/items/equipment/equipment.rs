use std::fmt::Debug;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::components::{
    id::EffectId,
    items::{
        equipment::slots::{EquipmentSlot, SlotProvider},
        item::Item,
    },
};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum EquipmentKind {
    Headwear,
    Cloak,
    Gloves,
    Boots,
    Amulet,
    Ring,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema)]
pub struct EquipmentItem {
    pub item: Item,
    pub kind: EquipmentKind,
    pub effects: Vec<EffectId>,
}

impl SlotProvider for EquipmentItem {
    fn valid_slots(&self) -> &'static [EquipmentSlot] {
        match self.kind {
            EquipmentKind::Headwear => &[EquipmentSlot::Headwear],
            EquipmentKind::Cloak => &[EquipmentSlot::Cloak],
            EquipmentKind::Gloves => &[EquipmentSlot::Gloves],
            EquipmentKind::Boots => &[EquipmentSlot::Boots],
            EquipmentKind::Amulet => &[EquipmentSlot::Amulet],
            EquipmentKind::Ring => &[EquipmentSlot::Ring1, EquipmentSlot::Ring2],
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn valid_slots() {
        let helmet = EquipmentItem {
            item: Item::default(),
            kind: EquipmentKind::Headwear,
            effects: vec![],
        };
        assert_eq!(helmet.valid_slots(), &[EquipmentSlot::Headwear]);

        let ring = EquipmentItem {
            item: Item::default(),
            kind: EquipmentKind::Ring,
            effects: vec![],
        };
        assert_eq!(
            ring.valid_slots(),
            &[EquipmentSlot::Ring1, EquipmentSlot::Ring2]
        );
    }

    #[test]
    fn invalid_slots() {
        let boots = EquipmentItem {
            item: Item::default(),
            kind: EquipmentKind::Boots,
            effects: vec![],
        };
        assert_ne!(boots.valid_slots(), &[EquipmentSlot::Headwear]);
    }
}
