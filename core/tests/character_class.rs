extern crate nat20_core;

mod tests {

    use std::collections::HashSet;

    use hecs::World;
    use nat20_core::{
        components::{
            ability::Ability,
            id::{
                BackgroundId, ClassId, EffectId, FeatId, ItemId, SpeciesId, SubclassId,
                SubspeciesId,
            },
            level::CharacterLevels,
            level_up::ChoiceItem,
            proficiency::ProficiencyLevel,
            saving_throw::{SavingThrowKind, SavingThrowSet},
            skill::{Skill, SkillSet},
        },
        entities::character::Character,
        registry::registry::ClassesRegistry,
        systems::{self, level_up::LevelUpDecision},
        test_utils::fixtures,
    };

    #[test]
    fn character_level_up_fighter() {
        let mut game_state = fixtures::engine::game_state();
        let character = game_state.world.spawn(Character::default());

        systems::level_up::apply_level_up_decision(
            &mut game_state,
            character,
            3,
            vec![
                // Level 1
                // TODO: Everyone is dragonborn for now
                LevelUpDecision::single_choice(ChoiceItem::Species(SpeciesId::new(
                    "nat20_core",
                    "species.dragonborn",
                ))),
                LevelUpDecision::single_choice(ChoiceItem::Subspecies(SubspeciesId::new(
                    "nat20_core",
                    "subspecies.dragonborn.white",
                ))),
                LevelUpDecision::single_choice(ChoiceItem::Background(BackgroundId::new(
                    "nat20_core",
                    "background.soldier",
                ))),
                LevelUpDecision::single_choice(ChoiceItem::Class(ClassId::new(
                    "nat20_core",
                    "class.fighter",
                ))),
                LevelUpDecision::AbilityScores(
                    ClassesRegistry::get(&ClassId::new("nat20_core", "class.fighter"))
                        .unwrap()
                        .default_abilities
                        .clone(),
                ),
                LevelUpDecision::single_choice_with_id(
                    "choice.fighting_style",
                    ChoiceItem::Feat(FeatId::new(
                        "nat20_core",
                        "feat.fighting_style.great_weapon_fighting",
                    )),
                ),
                LevelUpDecision::SkillProficiency(HashSet::from([
                    Skill::Acrobatics,
                    Skill::Perception,
                ])),
                LevelUpDecision::single_choice_with_id(
                    "choice.starting_equipment.fighter",
                    ChoiceItem::Equipment {
                        items: vec![
                            (1, ItemId::new("nat20_core", "item.chainmail")),
                            (1, ItemId::new("nat20_core", "item.greatsword")),
                            (1, ItemId::new("nat20_core", "item.flail")),
                            (8, ItemId::new("nat20_core", "item.javelin")),
                        ],
                        money: "4 GP".to_string(),
                    },
                ),
                LevelUpDecision::single_choice_with_id(
                    "choice.starting_equipment.soldier",
                    ChoiceItem::Equipment {
                        items: Vec::new(),
                        money: "50 GP".to_string(),
                    },
                ),
                // Level 2
                LevelUpDecision::single_choice(ChoiceItem::Class(ClassId::new(
                    "nat20_core",
                    "class.fighter",
                ))),
                // Level 3
                LevelUpDecision::single_choice(ChoiceItem::Class(ClassId::new(
                    "nat20_core",
                    "class.fighter",
                ))),
                LevelUpDecision::single_choice(ChoiceItem::Subclass(SubclassId::new(
                    "nat20_core",
                    "subclass.fighter.champion",
                ))),
            ],
        );

        {
            let levels =
                systems::helpers::get_component::<CharacterLevels>(&game_state.world, character);
            assert_eq!(levels.total_level(), 3);
            assert_eq!(
                levels
                    .class_level(&ClassId::new("nat20_core", "class.fighter"))
                    .unwrap()
                    .level(),
                3
            );
            assert_eq!(
                levels
                    .class_level(&ClassId::new("nat20_core", "class.fighter"))
                    .unwrap()
                    .subclass(),
                Some(&SubclassId::new("nat20_core", "subclass.fighter.champion"))
            );
        }

        {
            let effects = systems::effects::effects(&game_state.world, character);
            let effect_ids: HashSet<&EffectId> =
                effects.values().into_iter().map(|e| &e.effect_id).collect();
            for effect_id in [
                EffectId::new("nat20_core", "effect.fighting_style.great_weapon_fighting"),
                EffectId::new("nat20_core", "effect.fighter.champion.improved_critical"),
            ] {
                assert!(
                    effect_ids.contains(&effect_id),
                    "Effect {:?} not found in character effects: {:?}",
                    effect_id,
                    effect_ids
                );
            }
        }

        {
            let skills = systems::helpers::get_component::<SkillSet>(&game_state.world, character);
            for skill in [Skill::Athletics, Skill::Perception] {
                assert_eq!(
                    skills.get(&skill).proficiency().level(),
                    &ProficiencyLevel::Proficient
                );
            }
        }

        {
            let saving_throws =
                systems::helpers::get_component::<SavingThrowSet>(&game_state.world, character);
            for ability in [Ability::Strength, Ability::Constitution] {
                assert_eq!(
                    saving_throws
                        .get(&SavingThrowKind::Ability(ability))
                        .proficiency()
                        .level(),
                    &ProficiencyLevel::Proficient
                );
            }
        }
    }
}
