use std::{fs::File, path::Path};

use hecs::serialize::row::{DeserializeContext, SerializeContext, try_serialize};
use serde_json::{Deserializer, Serializer};
use tracing::warn;

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::{
            action::{ActionCooldownMap, ActionMap},
            execution::{ActionExecution, ExecutionMailbox},
        },
        activity::ActivityState,
        ai::PlayerControlledTag,
        damage::DamageResistances,
        effects::effect_manager::EffectManager,
        faction::FactionSet,
        health::{
            hit_points::HitPoints,
            life_state::{DeathPolicy, LifeState},
        },
        id::{AIControllerId, BackgroundId, FeatId, Name, SpeciesId, SubspeciesId},
        items::{
            equipment::{armor::ArmorTrainingSet, loadout::Loadout, weapon::WeaponProficiencyMap},
            inventory::Inventory,
        },
        level::CharacterLevels,
        resource::ResourceMap,
        saving_throw::SavingThrowSet,
        scratchpad::Scratchpad,
        skill::SkillSet,
        species::{CreatureSize, CreatureType},
        speed::Speed,
        spells::spellbook::Spellbook,
        time::EntityClock,
    },
    engine::engine_state::EngineState,
    systems::{combat::CombatState, entities::EntityKind, geometry::Pose},
};

struct WorldSaveContext;

impl SerializeContext for WorldSaveContext {
    fn serialize_entity<S>(
        &mut self,
        entity: hecs::EntityRef<'_>,
        mut map: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::SerializeMap,
    {
        try_serialize::<EntityKind, _, _>(&entity, "entity_kind", &mut map)?;
        try_serialize::<PlayerControlledTag, _, _>(&entity, "player_controlled", &mut map)?;
        try_serialize::<AIControllerId, _, _>(&entity, "brain", &mut map)?;
        try_serialize::<Pose, _, _>(&entity, "pose", &mut map)?;
        try_serialize::<EntityClock, _, _>(&entity, "time", &mut map)?;
        try_serialize::<ActivityState, _, _>(&entity, "activity_state", &mut map)?;
        try_serialize::<CombatState, _, _>(&entity, "combat_state", &mut map)?;
        try_serialize::<Name, _, _>(&entity, "name", &mut map)?;
        try_serialize::<SpeciesId, _, _>(&entity, "species", &mut map)?;
        try_serialize::<Option<SubspeciesId>, _, _>(&entity, "subspecies", &mut map)?;
        try_serialize::<CreatureSize, _, _>(&entity, "size", &mut map)?;
        try_serialize::<CreatureType, _, _>(&entity, "creature_type", &mut map)?;
        try_serialize::<Speed, _, _>(&entity, "speed", &mut map)?;
        try_serialize::<BackgroundId, _, _>(&entity, "background", &mut map)?;
        try_serialize::<CharacterLevels, _, _>(&entity, "levels", &mut map)?;
        try_serialize::<HitPoints, _, _>(&entity, "hit_points", &mut map)?;
        try_serialize::<LifeState, _, _>(&entity, "life_state", &mut map)?;
        try_serialize::<DeathPolicy, _, _>(&entity, "death_policy", &mut map)?;
        try_serialize::<AbilityScoreMap, _, _>(&entity, "ability_scores", &mut map)?;
        try_serialize::<SkillSet, _, _>(&entity, "skills", &mut map)?;
        try_serialize::<SavingThrowSet, _, _>(&entity, "saving_throws", &mut map)?;
        try_serialize::<DamageResistances, _, _>(&entity, "resistances", &mut map)?;
        try_serialize::<WeaponProficiencyMap, _, _>(&entity, "weapon_proficiencies", &mut map)?;
        try_serialize::<ArmorTrainingSet, _, _>(&entity, "armor_training", &mut map)?;
        try_serialize::<Inventory, _, _>(&entity, "inventory", &mut map)?;
        try_serialize::<Loadout, _, _>(&entity, "loadout", &mut map)?;
        try_serialize::<Spellbook, _, _>(&entity, "spellbook", &mut map)?;
        try_serialize::<ResourceMap, _, _>(&entity, "resources", &mut map)?;
        // TODO: Remember to re-register end conditions when deserializing
        try_serialize::<EffectManager, _, _>(&entity, "effects", &mut map)?;
        try_serialize::<Vec<FeatId>, _, _>(&entity, "feats", &mut map)?;
        try_serialize::<ActionMap, _, _>(&entity, "actions", &mut map)?;
        // TODO: Can't serialize ActionExecution, so I guess we can't save if it's Some
        // try_serialize::<Option<ActionExecution>, _, _>(&entity, "action_execution", &mut map)?;
        try_serialize::<ExecutionMailbox, _, _>(&entity, "execution_mailbox", &mut map)?;
        try_serialize::<ActionCooldownMap, _, _>(&entity, "cooldowns", &mut map)?;
        try_serialize::<FactionSet, _, _>(&entity, "factions", &mut map)?;
        try_serialize::<Scratchpad, _, _>(&entity, "scratchpad", &mut map)?;

        map.end()
    }
}

pub fn save_world(engine_state: &EngineState) {
    let mut context = WorldSaveContext;
    let writer = File::create("world.json").unwrap();
    let mut serializer = Serializer::pretty(writer);
    let result =
        hecs::serialize::row::serialize(&engine_state.world, &mut context, &mut serializer);
    print!("{:?}", result);
}

impl DeserializeContext for WorldSaveContext {
    fn deserialize_entity<'de, M>(
        &mut self,
        mut map: M,
        entity: &mut hecs::EntityBuilder,
    ) -> Result<(), M::Error>
    where
        M: serde::de::MapAccess<'de>,
    {
        while let Some(key) = map.next_key::<String>()? {
            match key.as_str() {
                "entity_kind" => {
                    let entity_kind = map.next_value::<EntityKind>()?;
                    if entity_kind.is_creature() {
                        entity.add::<Option<ActionExecution>>(None);
                    }
                    entity.add::<EntityKind>(entity_kind);
                }
                "player_controlled" => {
                    entity.add::<PlayerControlledTag>(map.next_value()?);
                }
                "brain" => {
                    entity.add::<AIControllerId>(map.next_value()?);
                }
                "pose" => {
                    entity.add::<Pose>(map.next_value()?);
                }
                "time" => {
                    entity.add::<EntityClock>(map.next_value()?);
                }
                "activity_state" => {
                    entity.add::<ActivityState>(map.next_value()?);
                }
                "combat_state" => {
                    entity.add::<CombatState>(map.next_value()?);
                }
                "name" => {
                    entity.add::<Name>(map.next_value()?);
                }
                "species" => {
                    entity.add::<SpeciesId>(map.next_value()?);
                }
                "subspecies" => {
                    entity.add::<Option<SubspeciesId>>(map.next_value()?);
                }
                "size" => {
                    entity.add::<CreatureSize>(map.next_value()?);
                }
                "creature_type" => {
                    entity.add::<CreatureType>(map.next_value()?);
                }
                "speed" => {
                    entity.add::<Speed>(map.next_value()?);
                }
                "background" => {
                    entity.add::<BackgroundId>(map.next_value()?);
                }
                "levels" => {
                    entity.add::<CharacterLevels>(map.next_value()?);
                }
                "hit_points" => {
                    entity.add::<HitPoints>(map.next_value()?);
                }
                "life_state" => {
                    entity.add::<LifeState>(map.next_value()?);
                }
                "death_policy" => {
                    entity.add::<DeathPolicy>(map.next_value()?);
                }
                "ability_scores" => {
                    entity.add::<AbilityScoreMap>(map.next_value()?);
                }
                "skills" => {
                    entity.add::<SkillSet>(map.next_value()?);
                }
                "saving_throws" => {
                    entity.add::<SavingThrowSet>(map.next_value()?);
                }
                "resistances" => {
                    entity.add::<DamageResistances>(map.next_value()?);
                }
                "weapon_proficiencies" => {
                    entity.add::<WeaponProficiencyMap>(map.next_value()?);
                }
                "armor_training" => {
                    entity.add::<ArmorTrainingSet>(map.next_value()?);
                }
                "inventory" => {
                    entity.add::<Inventory>(map.next_value()?);
                }
                "loadout" => {
                    entity.add::<Loadout>(map.next_value()?);
                }
                "spellbook" => {
                    entity.add::<Spellbook>(map.next_value()?);
                }
                "resources" => {
                    entity.add::<ResourceMap>(map.next_value()?);
                }
                "effects" => {
                    entity.add::<EffectManager>(map.next_value()?);
                }
                "feats" => {
                    entity.add::<Vec<FeatId>>(map.next_value()?);
                }
                "actions" => {
                    entity.add::<ActionMap>(map.next_value()?);
                }
                "execution_mailbox" => {
                    entity.add::<ExecutionMailbox>(map.next_value()?);
                }
                "cooldowns" => {
                    entity.add::<ActionCooldownMap>(map.next_value()?);
                }
                "factions" => {
                    entity.add::<FactionSet>(map.next_value()?);
                }
                "scratchpad" => {
                    entity.add::<Scratchpad>(map.next_value()?);
                }

                other => {
                    warn!("Entity has unknown field: {}", other);
                    let _: serde::de::IgnoredAny = map.next_value()?;
                }
            }
        }
        Ok(())
    }
}

pub fn load_world<P>(engine_state: &mut EngineState, file_path: &P)
where
    P: AsRef<Path>,
{
    let mut context = WorldSaveContext;
    let file = File::open(file_path).expect("Failed to open world file");
    let mut deserializer = Deserializer::from_reader(file);
    let world = hecs::serialize::row::deserialize(&mut context, &mut deserializer)
        .expect("Failed to deserialize world");
    engine_state.world = world;
}
