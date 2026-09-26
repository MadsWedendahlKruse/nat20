extern crate nat20_core;

mod tests {

    use nat20_core::{
        components::{
            ability::Ability,
            actions::action::{ActionContext, AttackRollProvider},
            d20::RollMode,
            id::ItemId,
            items::equipment::slots::EquipmentSlot,
            saving_throw::{SavingThrowKind, SavingThrowSet},
            skill::{Skill, SkillSet},
        },
        entities::creature::Character,
        systems,
        test_utils::fixtures,
    };

    #[test]
    fn character_pre_attack_roll_effect() {
        let mut engine_state = fixtures::engine::engine_state();
        let entity = engine_state.world.spawn(Character::default());

        let _ = systems::loadout::equip(
            &mut engine_state,
            entity,
            &ItemId::new("nat20_core", "item.dagger"),
        );

        // Before equipping the ring
        let context = &ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand);

        let (_, mut roll) = systems::loadout::loadout(&engine_state.world, entity).attack_roll(
            &engine_state.world,
            entity,
            entity,
            context,
        );
        systems::d20::preview_attack_roll(&engine_state, entity, entity, &mut roll);
        assert_eq!(roll.advantage_tracker().roll_mode(), RollMode::Normal);

        // Equip the ring
        let _ = systems::loadout::equip_in_slot(
            &mut engine_state,
            entity,
            &EquipmentSlot::Ring1,
            &ItemId::new("nat20_core", "item.ring_of_attacking"),
        );

        let (_, mut roll) = systems::loadout::loadout(&engine_state.world, entity).attack_roll(
            &engine_state.world,
            entity,
            entity,
            context,
        );
        systems::d20::preview_attack_roll(&engine_state, entity, entity, &mut roll);
        assert_eq!(roll.advantage_tracker().roll_mode(), RollMode::Advantage);

        // Unequip the ring
        systems::loadout::unequip(&mut engine_state, entity, &EquipmentSlot::Ring1);
        let (_, roll) = systems::loadout::loadout(&engine_state.world, entity).attack_roll(
            &engine_state.world,
            entity,
            entity,
            context,
        );
        assert_eq!(roll.advantage_tracker().roll_mode(), RollMode::Normal);
    }

    #[test]
    fn character_skill_bonus_effect() {
        let mut engine_state = fixtures::engine::engine_state();
        let entity = engine_state.world.spawn(Character::default());

        let _ = systems::loadout::equip(
            &mut engine_state,
            entity,
            &ItemId::new("nat20_core", "item.armor_of_sneaking"),
        );

        let check = systems::helpers::get_component::<SkillSet>(&engine_state.world, entity).check(
            &Skill::Stealth,
            &engine_state,
            entity,
        );
        assert_eq!(check.total_modifier(), 2);

        let _ = systems::loadout::unequip(&mut engine_state, entity, &EquipmentSlot::Armor)
            .expect("Failed to unequip armor");

        let check = systems::helpers::get_component::<SkillSet>(&engine_state.world, entity).check(
            &Skill::Stealth,
            &engine_state,
            entity,
        );
        assert_eq!(check.total_modifier(), 0);
    }

    #[test]
    fn character_saving_throw_effect() {
        let mut engine_state = fixtures::engine::engine_state();
        let entity = engine_state.world.spawn(Character::default());

        let _ = systems::loadout::equip(
            &mut engine_state,
            entity,
            &ItemId::new("nat20_core", "item.armor_of_constitution_saving_throws"),
        );

        let throw = systems::helpers::get_component::<SavingThrowSet>(&engine_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Constitution),
                &engine_state,
                entity,
            );
        assert_eq!(throw.advantage_tracker().roll_mode(), RollMode::Advantage);

        systems::loadout::unequip(&mut engine_state, entity, &EquipmentSlot::Armor)
            .expect("Failed to unequip armor");

        let throw = systems::helpers::get_component::<SavingThrowSet>(&engine_state.world, entity)
            .check(
                &SavingThrowKind::Ability(Ability::Constitution),
                &engine_state,
                entity,
            );
        assert_eq!(throw.advantage_tracker().roll_mode(), RollMode::Normal);
    }
}
