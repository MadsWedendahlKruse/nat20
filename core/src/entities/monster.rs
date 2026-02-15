use hecs::Bundle;

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::action::{ActionCooldownMap, ActionMap, default_actions},
        damage::DamageResistances,
        effects::effect::EffectsMap,
        faction::FactionSet,
        health::{hit_points::HitPoints, life_state::LifeState},
        id::{AIControllerId, Name},
        items::equipment::{
            armor::ArmorTrainingSet, loadout::Loadout, weapon::WeaponProficiencyMap,
        },
        level::ChallengeRating,
        resource::ResourceMap,
        saving_throw::SavingThrowSet,
        skill::SkillSet,
        species::{CreatureSize, CreatureType},
        speed::Speed,
        spells::spellbook::Spellbook,
        time::EntityClock,
    },
    from_world,
    systems::geometry::CreaturePose,
};

#[derive(Debug, Clone)]
pub struct MonsterTag;

from_world!(
    #[derive(Bundle, Clone)]
    pub struct Monster {
        pub tag: MonsterTag,
        pub name: Name,
        // TODO: Can monsters be player controlled?
        pub brain: AIControllerId,
        pub pose: CreaturePose,
        pub time: EntityClock,
        pub challenge_rating: ChallengeRating,
        pub hit_points: HitPoints,
        pub life_state: LifeState,
        pub size: CreatureSize,
        pub creature_type: CreatureType,
        pub speed: Speed,
        pub abilities: AbilityScoreMap,
        pub skills: SkillSet,
        pub saving_throws: SavingThrowSet,
        pub resistances: DamageResistances,
        // TODO: alignment?
        // TODO: ArmorClass or just Loadout?
        pub loadout: Loadout,
        pub spellbook: Spellbook,
        pub resources: ResourceMap,
        pub effects: EffectsMap,
        pub actions: ActionMap,
        pub cooldowns: ActionCooldownMap,
        pub weapon_proficiencies: WeaponProficiencyMap,
        pub armor_training: ArmorTrainingSet,
        pub factions: FactionSet,
    }
);

impl Monster {
    pub fn new(
        name: Name,
        brain: AIControllerId,
        challenge_rating: ChallengeRating,
        hit_points: HitPoints,
        size: CreatureSize,
        creature_type: CreatureType,
        speed: Speed,
        abilities: AbilityScoreMap,
        factions: FactionSet,
    ) -> Self {
        Self {
            tag: MonsterTag,
            name,
            brain,
            pose: CreaturePose::default(),
            time: EntityClock::new(),
            challenge_rating,
            hit_points,
            life_state: LifeState::Normal,
            size,
            creature_type,
            speed,
            abilities,
            skills: SkillSet::default(),
            saving_throws: SavingThrowSet::default(),
            resistances: DamageResistances::default(),
            loadout: Loadout::default(),
            spellbook: Spellbook::new(),
            resources: ResourceMap::default(),
            effects: EffectsMap::new(),
            actions: default_actions(),
            cooldowns: ActionCooldownMap::default(),
            weapon_proficiencies: WeaponProficiencyMap::new(),
            armor_training: ArmorTrainingSet::default(),
            factions,
        }
    }
}
