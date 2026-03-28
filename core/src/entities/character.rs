use std::collections::HashMap;

use hecs::Bundle;

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::action::{ActionCooldownMap, ActionMap, default_actions},
        activity::ActivityState,
        ai::PlayerControlledTag,
        damage::DamageResistances,
        effects::effect_manager::EffectManager,
        faction::FactionSet,
        health::{hit_points::HitPoints, life_state::LifeState},
        id::{AIControllerId, BackgroundId, FactionId, FeatId, Name, SpeciesId, SubspeciesId},
        items::{
            equipment::{armor::ArmorTrainingSet, loadout::Loadout, weapon::WeaponProficiencyMap},
            inventory::Inventory,
        },
        level::CharacterLevels,
        resource::ResourceMap,
        saving_throw::SavingThrowSet,
        skill::SkillSet,
        species::{CreatureSize, CreatureType},
        speed::Speed,
        spells::spellbook::Spellbook,
        time::EntityClock,
    },
    from_world, registry,
    systems::geometry::Pose,
};

#[derive(Debug, Clone)]
pub struct CharacterTag;

from_world!(
    #[derive(Bundle, Clone)]
    pub struct Character {
        pub character_tag: CharacterTag,
        /// By default, characters are player controlled. In case the player gets
        /// possessed or mind controlled, this component can be removed from the
        /// entity to make it AI controlled.
        pub player_controlled: PlayerControlledTag,
        /// AI controller for this character. Ignored if `player_controlled` is present.
        pub brain: AIControllerId,
        pub pose: Pose,
        pub time: EntityClock,
        pub activity_state: ActivityState,
        pub name: Name,
        pub species: SpeciesId,
        pub subspecies: Option<SubspeciesId>,
        pub size: CreatureSize,
        pub creature_type: CreatureType,
        pub speed: Speed,
        pub background: BackgroundId,
        pub levels: CharacterLevels,
        pub hit_points: HitPoints,
        pub life_state: LifeState,
        pub ability_scores: AbilityScoreMap,
        pub skills: SkillSet,
        pub saving_throws: SavingThrowSet,
        pub resistances: DamageResistances,
        pub weapon_proficiencies: WeaponProficiencyMap,
        pub armor_training: ArmorTrainingSet,
        pub inventory: Inventory,
        pub loadout: Loadout,
        pub spellbook: Spellbook,
        pub resources: ResourceMap,
        pub effects: EffectManager,
        pub feats: Vec<FeatId>,
        pub actions: ActionMap,
        pub cooldowns: ActionCooldownMap,
        pub factions: FactionSet,
    }
);

impl Character {
    pub fn new(name: Name) -> Self {
        Self {
            character_tag: CharacterTag,
            player_controlled: PlayerControlledTag,
            // TODO: Update to an actual ID
            brain: registry::ai::RANDOM_CONTROLLER_ID.clone(),
            pose: Pose::identity(),
            time: EntityClock::new(),
            activity_state: ActivityState::default(),
            name,
            species: SpeciesId::new("nat20_core", ""),
            subspecies: None,
            background: BackgroundId::new("nat20_core", ""),
            size: CreatureSize::Medium,
            creature_type: CreatureType::Humanoid,
            speed: Speed::default(),
            levels: CharacterLevels::new(),
            hit_points: HitPoints::new(1),
            life_state: LifeState::Normal,
            ability_scores: AbilityScoreMap::new(),
            skills: SkillSet::default(),
            saving_throws: SavingThrowSet::default(),
            resistances: DamageResistances::new(),
            armor_training: ArmorTrainingSet::new(),
            weapon_proficiencies: WeaponProficiencyMap::new(),
            loadout: Loadout::new(),
            inventory: Inventory::new(),
            spellbook: Spellbook::new(),
            resources: ResourceMap::default(),
            effects: EffectManager::new(),
            feats: Vec::new(),
            actions: default_actions(),
            cooldowns: HashMap::new(),
            factions: FactionSet::from([FactionId::new("nat20_core", "faction.players")]),
        }
    }
}

impl Default for Character {
    fn default() -> Self {
        Character::new(Name::new("John Doe"))
    }
}
