use std::any::TypeId;

use hecs::{
    EntityBuilder, EntityRef, World,
    serialize::row::{self, DeserializeContext, SerializeContext, try_serialize},
};
use serde::{
    Deserializer, Serializer,
    de::{IgnoredAny, MapAccess},
    ser::SerializeMap,
};
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
        level::{ChallengeRating, CharacterLevels},
        resource::ResourceMap,
        saving_throw::SavingThrowSet,
        scratchpad::Scratchpad,
        skill::SkillSet,
        species::{CreatureSize, CreatureType},
        speed::Speed,
        spells::spellbook::Spellbook,
        time::EntityClock,
    },
    systems::{combat::CombatState, entities::EntityKind, geometry::Pose, time::RestKind},
};

struct WorldSaveContext;

/// Generate both the serialization and deserialization implementations at the same
/// time so we don't forget one of them
macro_rules! saved_components {
    ($($key:literal => $ty:ty,)*) => {
        impl SerializeContext for WorldSaveContext {
            fn serialize_entity<S>(
                &mut self,
                entity: EntityRef<'_>,
                mut map: S,
            ) -> Result<S::Ok, S::Error>
            where
                S: SerializeMap,
            {
                $(try_serialize::<$ty, _, _>(&entity, $key, &mut map)?;)*
                map.end()
            }
        }

        impl DeserializeContext for WorldSaveContext {
            fn deserialize_entity<'de, M>(
                &mut self,
                mut map: M,
                entity: &mut EntityBuilder,
            ) -> Result<(), M::Error>
            where
                M: MapAccess<'de>,
            {
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        $($key => {
                            entity.add::<$ty>(map.next_value()?);
                        })*
                        other => {
                            warn!("Entity has unknown field: {}", other);
                            let _: IgnoredAny = map.next_value()?;
                        }
                    }
                }
                rebuild_transient(entity);
                Ok(())
            }
        }

        /// Keep track of which components are saved by this macro
        fn is_saved(type_id: TypeId) -> bool {
            [$(TypeId::of::<$ty>()),*].contains(&type_id)
        }
    };
}

saved_components! {
    "entity_kind" => EntityKind,
    "player_controlled" => PlayerControlledTag,
    "brain" => AIControllerId,
    "pose" => Pose,
    "time" => EntityClock,
    "activity_state" => ActivityState,
    "combat_state" => CombatState,
    "name" => Name,
    "species" => SpeciesId,
    "subspecies" => Option<SubspeciesId>,
    "challenge_rating" => ChallengeRating,
    "size" => CreatureSize,
    "creature_type" => CreatureType,
    "speed" => Speed,
    "background" => BackgroundId,
    "levels" => CharacterLevels,
    "hit_points" => HitPoints,
    "life_state" => LifeState,
    "death_policy" => DeathPolicy,
    "ability_scores" => AbilityScoreMap,
    "skills" => SkillSet,
    "saving_throws" => SavingThrowSet,
    "resistances" => DamageResistances,
    "weapon_proficiencies" => WeaponProficiencyMap,
    "armor_training" => ArmorTrainingSet,
    "inventory" => Inventory,
    "loadout" => Loadout,
    "spellbook" => Spellbook,
    "resources" => ResourceMap,
    // TODO: Remember to re-register end conditions when deserializing
    "effects" => EffectManager,
    "feats" => Vec<FeatId>,
    "actions" => ActionMap,
    "execution_mailbox" => ExecutionMailbox,
    "cooldowns" => ActionCooldownMap,
    "factions" => FactionSet,
    "scratchpad" => Scratchpad,
    "resting" => Option<RestKind>,
}

/// Some components can't be saved, so we have to rebuild them on load
fn rebuild_transient(entity: &mut EntityBuilder) {
    if entity
        .get::<&EntityKind>()
        .is_some_and(|kind| kind.is_creature())
        && entity.get::<&Option<ActionExecution>>().is_none()
    {
        entity.add::<Option<ActionExecution>>(None);
    }
}

/// Whether a component of the given type is expected to be saved
pub fn should_save(type_id: TypeId) -> bool {
    is_saved(type_id) || type_id == TypeId::of::<Option<ActionExecution>>()
}

/// These two are shaped for `#[serde(with = "world")]`

pub(crate) fn serialize<S: Serializer>(world: &World, serializer: S) -> Result<S::Ok, S::Error> {
    row::serialize(world, &mut WorldSaveContext, serializer)
}

pub(crate) fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<World, D::Error> {
    row::deserialize(&mut WorldSaveContext, deserializer)
}
