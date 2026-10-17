extern crate nat20_core;

mod tests {
    use std::collections::HashSet;

    use hecs::Entity;
    use nat20_core::{
        components::{
            d20::{D20Check, D20CheckKind},
            dice::{DiceSet, DieSize},
            modifier::ModifierSource,
            proficiency::{Proficiency, ProficiencyLevel},
            rng,
            skill::Skill,
        },
        test_utils::fixtures,
    };

    fn roll_sequence() -> (Vec<Vec<u32>>, Vec<Vec<u8>>) {
        let dice = DiceSet::new(4, DieSize::D6);
        let check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::None, ModifierSource::None),
        );

        let dice_rolls = (0..20).map(|_| dice.roll().rolls().to_vec()).collect();
        let d20_rolls = (0..20).map(|_| check.roll().rolls).collect();
        (dice_rolls, d20_rolls)
    }

    #[test]
    fn same_seed_gives_same_rolls() {
        rng::seed(42);
        let first = roll_sequence();
        rng::seed(42);
        let second = roll_sequence();
        assert_eq!(first, second);
    }

    #[test]
    fn different_seeds_give_different_rolls() {
        rng::seed(1);
        let first = roll_sequence();
        rng::seed(2);
        let second = roll_sequence();
        assert_ne!(first, second);
    }

    #[test]
    fn seeded_rolls_stay_in_range() {
        rng::seed(7);
        let (dice_rolls, d20_rolls) = roll_sequence();
        assert!(
            dice_rolls
                .iter()
                .flatten()
                .all(|roll| (1..=6).contains(roll))
        );
        assert!(
            d20_rolls
                .iter()
                .flatten()
                .all(|roll| (1..=20).contains(roll))
        );
    }

    fn seeded_initiative(seed: u64) -> Vec<(Entity, Vec<u8>)> {
        let mut engine_state = fixtures::engine::engine_state();
        let mut participants = HashSet::new();
        participants.insert(fixtures::creatures::heroes::fighter(&mut engine_state, 1, None).id());
        for _ in 0..4 {
            participants.insert(
                fixtures::creatures::monsters::goblin_warrior(&mut engine_state, 1, None).id(),
            );
        }

        rng::seed(seed);
        let encounter_id = engine_state.start_encounter(participants);
        engine_state
            .encounter(&encounter_id)
            .unwrap()
            .initiative_order()
            .iter()
            .map(|(entity, roll)| (*entity, roll.rolls.clone()))
            .collect()
    }

    #[test]
    fn same_seed_gives_same_initiative() {
        // The participants are a HashSet, so this also checks that initiative
        // doesn't depend on its iteration order.
        assert_eq!(seeded_initiative(42), seeded_initiative(42));
    }
}
