use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    str::FromStr,
    sync::LazyLock,
};

use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};
use uom::si::length::foot;

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        actions::targeting::TargetingRange,
        d20::D20Check,
        damage::{AttackRange, AttackRoll, AttackSource, DamageRoll, DamageSource, DamageType},
        dice::DiceSet,
        id::{ActionId, EffectId},
        items::{
            equipment::slots::{EquipmentSlot, SlotProvider},
            item::Item,
        },
        modifier::{KeyedModifiable, Modifiable, ModifierSet, ModifierSource},
        proficiency::{Proficiency, ProficiencyLevel},
    },
    registry::serialize::item::WeaponDefinition,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq, Display, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WeaponCategory {
    Simple,
    Martial,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, EnumIter, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WeaponKind {
    Melee,
    Ranged,
    Unarmed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum WeaponProperties {
    // TODO: Ammunition,
    Finesse,
    // TODO: Actually implement Heavy and Light
    Heavy,
    Light,
    // TODO: Loading,
    Range(TargetingRange),
    Reach,
    Thrown(TargetingRange),
    TwoHanded,
    /// Damage if wielded with two hands
    Versatile(DiceSet),
    Enchantment(u32),
}

impl Display for WeaponProperties {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WeaponProperties::Enchantment(level) => write!(f, "Enchantment +{}", level),
            WeaponProperties::Versatile(dice) => write!(f, "Versatile ({})", dice),
            WeaponProperties::Range(range) => write!(f, "Range ({})", range),
            WeaponProperties::TwoHanded => write!(f, "Two-Handed"),
            WeaponProperties::Thrown(range) => write!(f, "Thrown ({})", range),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl FromStr for WeaponProperties {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let parts = s.split_whitespace().collect::<Vec<&str>>();
        if parts.len() == 1 {
            match s.to_lowercase().as_str() {
                "finesse" => Ok(WeaponProperties::Finesse),
                "heavy" => Ok(WeaponProperties::Heavy),
                "light" => Ok(WeaponProperties::Light),
                "reach" => Ok(WeaponProperties::Reach),
                "twohanded" | "two-handed" => Ok(WeaponProperties::TwoHanded),
                _ => Err(format!("Invalid weapon property: {}", s)),
            }
        } else if parts.len() > 1 {
            match parts[0].to_lowercase().as_str() {
                "enchantment" => {
                    let level = parts[1]
                        .trim()
                        .trim_start_matches('+')
                        .parse::<u32>()
                        .map_err(|_| format!("Invalid enchantment level: {}", parts[1]))?;
                    Ok(WeaponProperties::Enchantment(level))
                }
                "versatile" => {
                    let dice = DiceSet::from_str(
                        parts[1]
                            .trim()
                            .trim_start_matches('(')
                            .trim_end_matches(')'),
                    )?;
                    Ok(WeaponProperties::Versatile(dice))
                }
                "range" => {
                    let rest = parts[1..].join(" ");
                    let range = TargetingRange::from_str(
                        rest.trim()
                            .trim_start_matches('(')
                            .trim_end_matches(')')
                            // TODO: Support other units
                            .trim_end_matches("m"),
                    )?;
                    Ok(WeaponProperties::Range(range))
                }
                "thrown" => {
                    let rest = parts[1..].join(" ");
                    let range = TargetingRange::from_str(
                        rest.trim()
                            .trim_start_matches('(')
                            .trim_end_matches(')')
                            .trim_end_matches("m"),
                    )?;
                    Ok(WeaponProperties::Thrown(range))
                }
                _ => Err(format!("Invalid weapon property: {}", s)),
            }
        } else {
            Err(format!("Invalid weapon property: {}", s))
        }
    }
}

impl TryFrom<String> for WeaponProperties {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        WeaponProperties::from_str(&value)
    }
}

impl Into<String> for WeaponProperties {
    fn into(self) -> String {
        self.to_string()
    }
}

// These are really extra abilities, so might have to handle them differently
// TODO: Handle these as weapon_actions
// pub enum MasteryProperty {
//     Cleave,
//     Graze,
//     Nick,
//     Push,
//     Sap,
//     Slow,
//     Topple,
//     Vex,
// }

#[derive(Debug, Clone)]
pub struct WeaponProficiencyMap {
    map: HashMap<WeaponCategory, Proficiency>,
}

impl WeaponProficiencyMap {
    pub fn new() -> Self {
        WeaponProficiencyMap {
            map: HashMap::new(),
        }
    }

    pub fn set_proficiency(&mut self, category: WeaponCategory, proficiency: Proficiency) {
        self.map.insert(category, proficiency);
    }

    pub fn proficiency(&self, category: &WeaponCategory) -> Proficiency {
        self.map.get(category).cloned().unwrap_or(Proficiency::new(
            ProficiencyLevel::None,
            ModifierSource::None,
        ))
    }
}

pub static MELEE_RANGE_DEFAULT: LazyLock<TargetingRange> =
    LazyLock::new(|| TargetingRange::new::<foot>(5.0));
pub static MELEE_RANGE_REACH: LazyLock<TargetingRange> =
    LazyLock::new(|| TargetingRange::new::<foot>(10.0));

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(from = "WeaponDefinition")]
pub struct Weapon {
    item: Item,
    category: WeaponCategory,
    kind: WeaponKind,
    properties: HashSet<WeaponProperties>,
    damage_roll: DamageRoll,
    ability: Ability,
    extra_actions: Vec<ActionId>,
    effects: Vec<EffectId>,
}

impl Weapon {
    pub fn new(
        item: Item,
        kind: WeaponKind,
        category: WeaponCategory,
        properties: HashSet<WeaponProperties>,
        damage: Vec<(DiceSet, DamageType)>,
        extra_actions: Vec<ActionId>,
        effects: Vec<EffectId>,
    ) -> Self {
        if matches!(kind, WeaponKind::Unarmed) {
            panic!("Unarmed is not an equippable weapon kind");
        }

        if matches!(kind, WeaponKind::Ranged)
            && !properties
                .iter()
                .any(|p| matches!(p, WeaponProperties::Range(_)))
        {
            panic!("Ranged weapons must have a range property");
        }

        let ability = match kind {
            WeaponKind::Melee => Ability::Strength,
            WeaponKind::Ranged => Ability::Dexterity,
            WeaponKind::Unarmed => Ability::Strength,
        };

        if damage.is_empty() {
            panic!("Weapon must have at least one damage type");
        }
        let (dice, damage_type) = damage[0];
        let source = DamageSource::Weapon(kind.clone());
        let mut damage_roll = DamageRoll::new(dice, damage_type, source.clone());
        for i in 1..damage.len() {
            let (dice, damage_type) = damage[i];
            damage_roll.add_bonus(dice, damage_type);
        }

        Self {
            item,
            category,
            kind,
            properties,
            damage_roll,
            ability,
            extra_actions,
            effects,
        }
    }

    pub fn item(&self) -> &Item {
        &self.item
    }

    pub fn category(&self) -> &WeaponCategory {
        &self.category
    }

    pub fn kind(&self) -> &WeaponKind {
        &self.kind
    }

    pub fn properties(&self) -> &HashSet<WeaponProperties> {
        &self.properties
    }

    pub fn has_property(&self, property: &WeaponProperties) -> bool {
        self.properties.contains(property)
    }

    pub fn attack_roll(
        &self,
        ability_scores: &AbilityScoreMap,
        weapon_proficiency: &Proficiency,
    ) -> AttackRoll {
        let mut attack_roll = D20Check::new(weapon_proficiency.clone());

        self.add_ability_modifier(ability_scores, &mut attack_roll.modifiers_mut());

        let enchantment = self.enchantment();
        attack_roll.add_modifier(
            ModifierSource::Custom("Enchantment".to_string()),
            enchantment as i32,
        );

        let range = match self.kind {
            WeaponKind::Melee => AttackRange::Melee,
            WeaponKind::Ranged => AttackRange::Ranged,
            WeaponKind::Unarmed => AttackRange::Melee,
        };

        AttackRoll::new(attack_roll, AttackSource::Weapon(*self.kind()), range)
    }

    pub fn damage_roll(
        &self,
        ability_scores: &AbilityScoreMap,
        wielding_both_hands: bool,
    ) -> DamageRoll {
        let mut damage_roll = self.damage_roll.clone();

        // Check if the weapon is versatile and the character is wielding it in two hands
        let versatile_dice = self.properties.iter().find_map(|prop| {
            if let WeaponProperties::Versatile(dice) = prop {
                Some(dice)
            } else {
                None
            }
        });
        if versatile_dice.is_some() && wielding_both_hands {
            damage_roll.primary.dice_roll.dice = versatile_dice.unwrap().clone();
        }

        self.add_ability_modifier(ability_scores, &mut damage_roll.primary.dice_roll.modifiers);

        let enchantment = self.enchantment();
        if enchantment > 0 {
            damage_roll.primary.dice_roll.modifiers.add_modifier(
                ModifierSource::Custom("Enchantment".to_string()),
                enchantment as i32,
            );
        }

        damage_roll
    }

    fn add_ability_modifier(&self, ability_scores: &AbilityScoreMap, modifiers: &mut ModifierSet) {
        let ability = self.determine_ability(ability_scores);
        modifiers.add_modifier(
            ModifierSource::Ability(ability),
            ability_scores.ability_modifier(&ability).total(),
        );
    }

    fn enchantment(&self) -> u32 {
        self.properties
            .iter()
            .find_map(|prop| {
                if let WeaponProperties::Enchantment(enchantment) = prop {
                    Some(*enchantment)
                } else {
                    None
                }
            })
            .unwrap_or(0)
    }

    pub fn determine_ability(&self, ability_scores: &AbilityScoreMap) -> Ability {
        if self.has_property(&WeaponProperties::Finesse) {
            // Return the higher of the two abilities
            let str = ability_scores.total(&Ability::Strength);
            let dex = ability_scores.total(&Ability::Dexterity);
            if str > dex {
                Ability::Strength
            } else {
                Ability::Dexterity
            }
        } else {
            self.ability
        }
    }

    // TODO: Can this be done in a nicer way?
    /// Returns the normal range and the long range of the weapon.
    /// When attacking a target beyond normal range, you have Disadvantage on the
    /// attack roll. You can’t attack a target beyond the long range.
    /// Note that for melee weapons the normal and long range is the same.
    pub fn range(&self) -> &TargetingRange {
        for property in &self.properties {
            match property {
                WeaponProperties::Range(range) => {
                    return range;
                }
                WeaponProperties::Reach => {
                    return &MELEE_RANGE_REACH;
                }
                _ => {}
            }
        }
        return &MELEE_RANGE_DEFAULT;
    }

    pub fn effects(&self) -> &Vec<EffectId> {
        &self.effects
    }

    pub fn extra_actions(&self) -> &Vec<ActionId> {
        &self.extra_actions
    }
}

impl SlotProvider for Weapon {
    fn valid_slots(&self) -> &'static [EquipmentSlot] {
        match self.kind {
            WeaponKind::Melee => &[EquipmentSlot::MeleeMainHand, EquipmentSlot::MeleeOffHand],
            WeaponKind::Ranged => &[EquipmentSlot::RangedMainHand, EquipmentSlot::RangedOffHand],
            WeaponKind::Unarmed => &[],
        }
    }

    fn required_slots(&self) -> &'static [EquipmentSlot] {
        if self.has_property(&WeaponProperties::TwoHanded) {
            match self.kind {
                WeaponKind::Melee => &[EquipmentSlot::MeleeMainHand, EquipmentSlot::MeleeOffHand],
                WeaponKind::Ranged => {
                    &[EquipmentSlot::RangedMainHand, EquipmentSlot::RangedOffHand]
                }
                WeaponKind::Unarmed => &[],
            }
        } else {
            &[]
        }
    }
}

#[cfg(test)]
mod tests {

    use std::str::FromStr;

    use uom::si::{f32::Mass, mass::pound};

    use crate::components::{
        ability::AbilityScore,
        dice::DieSize,
        id::ItemId,
        items::{item::ItemRarity, money::MonetaryValue},
    };

    use super::*;

    #[test]
    fn create_weapon() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.longsword"),
            name: "Longsword".to_string(),
            description: "A longsword".to_string(),
            weight: Mass::new::<pound>(5.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Common,
        };
        let weapon = Weapon::new(
            item.clone(),
            WeaponKind::Melee,
            WeaponCategory::Martial,
            HashSet::from([WeaponProperties::Finesse, WeaponProperties::Enchantment(1)]),
            vec![("1d8".parse().unwrap(), DamageType::Slashing)],
            vec![],
            vec![],
        );

        assert_eq!(weapon.category(), &WeaponCategory::Martial);
        assert_eq!(weapon.kind(), &WeaponKind::Melee);
        assert_eq!(weapon.properties().len(), 2);
        assert_eq!(weapon.damage_roll.primary.dice_roll.dice.num_dice, 1);
        assert_eq!(
            weapon.damage_roll.primary.dice_roll.dice.die_size,
            DieSize::D8
        );
        assert_eq!(weapon.item.name, "Longsword");
        println!("{:?}", weapon);
    }

    #[test]
    #[should_panic(expected = "Ranged weapons must have a range property")]
    fn ranged_weapon_without_range_panics() {
        let item = Item {
            id: ItemId::new("nat20_core", "Shortbow"),
            name: "Shortbow".to_string(),
            description: "A ranged weapon".to_string(),
            weight: Mass::new::<pound>(2.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Common,
        };
        Weapon::new(
            item,
            WeaponKind::Ranged,
            WeaponCategory::Simple,
            HashSet::new(),
            vec![("1d6".parse().unwrap(), DamageType::Piercing)],
            vec![],
            vec![],
        );
    }

    #[test]
    fn weapon_has_property() {
        let item = Item {
            id: ItemId::new("nat20_core", "Dagger"),
            name: "Dagger".to_string(),
            description: "A small dagger".to_string(),
            weight: Mass::new::<pound>(1.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Common,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Melee,
            WeaponCategory::Simple,
            HashSet::from([WeaponProperties::Finesse, WeaponProperties::Light]),
            vec![("1d4".parse().unwrap(), DamageType::Piercing)],
            vec![],
            vec![],
        );
        assert!(weapon.has_property(&WeaponProperties::Finesse));
        assert!(weapon.has_property(&WeaponProperties::Light));
        assert!(!weapon.has_property(&WeaponProperties::Heavy));
    }

    #[test]
    fn weapon_enchantment_property() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.magic_sword"),
            name: "Magic Sword".to_string(),
            description: "A sword with enchantment".to_string(),
            weight: Mass::new::<pound>(3.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Uncommon,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Melee,
            WeaponCategory::Martial,
            HashSet::from([WeaponProperties::Enchantment(2)]),
            vec![("1d6".parse().unwrap(), DamageType::Slashing)],
            vec![],
            vec![],
        );
        assert_eq!(weapon.enchantment(), 2);
    }

    #[test]
    fn weapon_determine_ability_finesse() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.rapier"),
            name: "Rapier".to_string(),
            description: "A finesse weapon".to_string(),
            weight: Mass::new::<pound>(2.5),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Common,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Melee,
            WeaponCategory::Martial,
            HashSet::from([WeaponProperties::Finesse]),
            vec![("1d8".parse().unwrap(), DamageType::Piercing)],
            vec![],
            vec![],
        );
        let mut ability_scores = AbilityScoreMap::new();
        ability_scores.set(Ability::Strength, AbilityScore::new(Ability::Strength, 10));
        ability_scores.set(
            Ability::Dexterity,
            AbilityScore::new(Ability::Dexterity, 18),
        );
        assert_eq!(
            weapon.determine_ability(&ability_scores),
            Ability::Dexterity
        );
    }

    #[test]
    fn weapon_name_and_effects() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.warhammer"),
            name: "Warhammer".to_string(),
            description: "A heavy warhammer".to_string(),
            weight: Mass::new::<pound>(8.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Rare,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Melee,
            WeaponCategory::Martial,
            HashSet::new(),
            vec![("1d10".parse().unwrap(), DamageType::Bludgeoning)],
            vec![],
            vec![],
        );
        assert_eq!(weapon.item.name, "Warhammer");
        assert!(weapon.effects().is_empty());
    }

    #[test]
    fn no_extra_weapon_actions() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.shortbow"),
            name: "Shortbow".to_string(),
            description: "A ranged weapon".to_string(),
            weight: Mass::new::<pound>(2.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Common,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Ranged,
            WeaponCategory::Simple,
            HashSet::from([WeaponProperties::Range(TargetingRange::with_max::<foot>(
                80.0, 320.0,
            ))]),
            vec![("1d6".parse().unwrap(), DamageType::Piercing)],
            vec![],
            vec![],
        );
        assert!(weapon.extra_actions().is_empty());
    }

    #[test]
    fn weapon_range_melee_default() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.club"),
            name: "Club".to_string(),
            description: "A simple club".to_string(),
            weight: Mass::new::<pound>(2.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Common,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Melee,
            WeaponCategory::Simple,
            HashSet::new(),
            vec![("1d4".parse().unwrap(), DamageType::Bludgeoning)],
            vec![],
            vec![],
        );
        assert_eq!(weapon.range(), &TargetingRange::new::<foot>(5.0));
    }

    #[test]
    fn weapon_range_melee_reach() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.whip"),
            name: "Whip".to_string(),
            description: "A whip with reach".to_string(),
            weight: Mass::new::<pound>(3.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Uncommon,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Melee,
            WeaponCategory::Martial,
            HashSet::from([WeaponProperties::Reach]),
            vec![("1d4".parse().unwrap(), DamageType::Slashing)],
            vec![],
            vec![],
        );
        assert_eq!(weapon.range(), &TargetingRange::new::<foot>(10.0));
    }

    #[test]
    fn weapon_range_ranged() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.longbow"),
            name: "Longbow".to_string(),
            description: "A long ranged bow".to_string(),
            weight: Mass::new::<pound>(2.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Rare,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Ranged,
            WeaponCategory::Martial,
            HashSet::from([WeaponProperties::Range(TargetingRange::with_max::<foot>(
                150.0, 600.0,
            ))]),
            vec![("1d8".parse().unwrap(), DamageType::Piercing)],
            vec![],
            vec![],
        );
        assert_eq!(
            weapon.range(),
            &TargetingRange::with_max::<foot>(150.0, 600.0)
        );
    }

    #[test]
    fn slot_provider_valid_slots_melee() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.axe"),
            name: "Axe".to_string(),
            description: "A hand axe".to_string(),
            weight: Mass::new::<pound>(2.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Common,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Melee,
            WeaponCategory::Simple,
            HashSet::new(),
            vec![("1d6".parse().unwrap(), DamageType::Slashing)],
            vec![],
            vec![],
        );
        let slots = <Weapon as SlotProvider>::valid_slots(&weapon);
        assert!(slots.contains(&EquipmentSlot::MeleeMainHand));
        assert!(slots.contains(&EquipmentSlot::MeleeOffHand));
    }

    #[test]
    fn slot_provider_valid_slots_ranged() {
        let item = Item {
            id: ItemId::new("nat20_core", "item.crossbow"),
            name: "Crossbow".to_string(),
            description: "A light crossbow".to_string(),
            weight: Mass::new::<pound>(3.0),
            value: MonetaryValue::from_str("1 GP").unwrap(),
            rarity: ItemRarity::Uncommon,
        };
        let weapon = Weapon::new(
            item,
            WeaponKind::Ranged,
            WeaponCategory::Simple,
            HashSet::from([WeaponProperties::Range(TargetingRange::with_max::<foot>(
                80.0, 320.0,
            ))]),
            vec![("1d8".parse().unwrap(), DamageType::Piercing)],
            vec![],
            vec![],
        );
        let slots = <Weapon as SlotProvider>::valid_slots(&weapon);
        assert!(slots.contains(&EquipmentSlot::RangedMainHand));
        assert!(slots.contains(&EquipmentSlot::RangedOffHand));
    }

    // #[test]
    // fn serialize() {
    //     let item = Item {
    //         id: ItemId::new("nat20_core", "item.spear"),
    //         name: "Spear".to_string(),
    //         description: "A simple spear".to_string(),
    //         weight: Mass::new::<pound>(3.0),
    //         value: MonetaryValue::from_str("1 GP").unwrap(),
    //         rarity: ItemRarity::Common,
    //     };
    //     let weapon = Weapon::new(
    //         item,
    //         WeaponKind::Melee,
    //         WeaponCategory::Simple,
    //         HashSet::from([
    //             WeaponProperties::Thrown,
    //             WeaponProperties::Range(TargetingRange::with_max::<foot>(20.0, 60.0)),
    //         ]),
    //         vec![("1d6".parse().unwrap(), DamageType::Piercing)],
    //         vec![],
    //         vec![],
    //     );
    //     let serialized = serde_json::to_string_pretty(&weapon).unwrap();
    //     println!("Weapon:\n{}\n", serialized);
    //     let deserialized: Weapon = serde_json::from_str(&serialized).unwrap();
    //     assert_eq!(weapon, deserialized);

    //     let item_instance = ItemInstance::Weapon(weapon);
    //     let serialized_instance = serde_json::to_string_pretty(&item_instance).unwrap();
    //     println!("ItemInstance::Weapon\n{}\n", serialized_instance);
    //     let deserialized_instance: ItemInstance =
    //         serde_json::from_str(&serialized_instance).unwrap();
    //     assert_eq!(item_instance, deserialized_instance);

    //     assert_eq!(serialized_instance, serialized);
    // }
}
