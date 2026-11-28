use std::collections::HashMap;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        faction::FactionSet,
        health::hit_points::HitPoints,
        id::{AIControllerId, IdProvider, ItemId, MonsterId, Name},
        level::ChallengeRating,
        species::{CreatureSize, CreatureType},
        speed::Speed,
    },
    entities::creature::Monster,
    registry::serialize::quantity::LengthExpressionDefinition,
};

/// Defines the ability scores for a creature.
/// Each ability is defined as an explicit field, so you're forced to fill it out
/// in the JSON file.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct AbilityScoreDefinition {
    pub strength: u8,
    pub dexterity: u8,
    pub constitution: u8,
    pub intelligence: u8,
    pub wisdom: u8,
    pub charisma: u8,
}

impl From<AbilityScoreDefinition> for AbilityScoreMap {
    fn from(def: AbilityScoreDefinition) -> Self {
        AbilityScoreMap::from(HashMap::from([
            (Ability::Strength, def.strength),
            (Ability::Dexterity, def.dexterity),
            (Ability::Constitution, def.constitution),
            (Ability::Intelligence, def.intelligence),
            (Ability::Wisdom, def.wisdom),
            (Ability::Charisma, def.charisma),
        ]))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MonsterDefinition {
    pub id: MonsterId,
    // TODO: Should the name actually be derived from the ID?
    pub name: String,
    pub brain: AIControllerId,
    pub challenge_rating: ChallengeRating,
    pub hit_points: u32,
    pub size: CreatureSize,
    pub creature_type: CreatureType,
    pub speed: LengthExpressionDefinition,
    pub abilities: AbilityScoreDefinition,
    pub factions: FactionSet,
    pub gear: Vec<ItemId>,
}

impl IdProvider for MonsterDefinition {
    type Id = MonsterId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

impl From<MonsterDefinition> for Monster {
    fn from(def: MonsterDefinition) -> Self {
        Self::new(
            Name::new(def.name),
            def.brain,
            def.challenge_rating,
            HitPoints::new(def.hit_points),
            def.size,
            def.creature_type,
            Speed::new(def.speed.evaluate_without_variables().expect(&format!(
                "Failed to evaluate speed expression for monster {}",
                def.id
            ))),
            def.abilities.into(),
            def.factions,
        )
    }
}
