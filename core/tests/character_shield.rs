use nat20_core::{
    components::{
        ability::Ability,
        id::ItemId,
        items::equipment::{loadout::Loadout, slots::EquipmentSlot, weapon::WeaponKind},
        modifier::{FlatModifierMap, ModifierSource},
    },
    test_utils::scenario::Scenario,
};

fn barbarian_scenario() -> Scenario {
    let mut scenario = Scenario::new();
    scenario
        .spawn("barbarian", "hero.barbarian")
        .level(3)
        .spawn();
    scenario
}

#[test]
fn shield_armor_class() {
    let mut scenario = barbarian_scenario();

    scenario
        .probe("barbarian")
        // Unarmored Defense still applies with a Shield
        .equip("item.shield")
        .assert_armor_class(&FlatModifierMap::from_iter(vec![
            (ModifierSource::Base, 10),
            (ModifierSource::Ability(Ability::Dexterity), 2),
            (ModifierSource::Ability(Ability::Constitution), 3),
            (
                ModifierSource::Item(ItemId::new("nat20_core", "item.shield")),
                2,
            ),
        ]))
        .equip("item.chainmail")
        .assert_armor_class(&FlatModifierMap::from_iter(vec![
            (
                ModifierSource::Item(ItemId::new("nat20_core", "item.chainmail")),
                16,
            ),
            (
                ModifierSource::Item(ItemId::new("nat20_core", "item.shield")),
                2,
            ),
        ]))
        .unequip(&EquipmentSlot::MeleeOffHand)
        .assert_armor_class(&FlatModifierMap::from_iter(vec![(
            ModifierSource::Item(ItemId::new("nat20_core", "item.chainmail")),
            16,
        )]));
}

#[test]
fn shield_and_two_handed_weapon_displace_each_other() {
    let mut scenario = barbarian_scenario();
    let mut barbarian = scenario.probe("barbarian");

    barbarian.component::<Loadout>(|loadout| {
        assert_eq!(
            loadout.item_in_slot(&EquipmentSlot::MeleeMainHand),
            Some(&ItemId::new("nat20_core", "item.greataxe"))
        );
    });

    barbarian
        .equip("item.shield")
        .component::<Loadout>(|loadout| {
            assert_eq!(loadout.item_in_slot(&EquipmentSlot::MeleeMainHand), None);
            assert_eq!(
                loadout.item_in_slot(&EquipmentSlot::MeleeOffHand),
                Some(&ItemId::new("nat20_core", "item.shield"))
            );
        });

    barbarian
        .equip("item.greataxe")
        .component::<Loadout>(|loadout| {
            assert_eq!(
                loadout.item_in_slot(&EquipmentSlot::MeleeMainHand),
                Some(&ItemId::new("nat20_core", "item.greataxe"))
            );
            assert_eq!(loadout.item_in_slot(&EquipmentSlot::MeleeOffHand), None);
        });
}

#[test]
fn versatile_weapon_with_shield_is_one_handed() {
    let mut scenario = barbarian_scenario();

    scenario
        .probe("barbarian")
        .equip("item.longsword")
        .component::<Loadout>(|loadout| {
            assert!(loadout.is_wielding_weapon_with_both_hands(&WeaponKind::Melee));
        });

    scenario
        .probe("barbarian")
        .equip("item.shield")
        .component::<Loadout>(|loadout| {
            assert!(!loadout.is_wielding_weapon_with_both_hands(&WeaponKind::Melee));
        });
}
