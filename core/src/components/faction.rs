use std::collections::{HashMap, HashSet};

use hecs::Entity;
use serde::{Deserialize, Serialize};

use crate::components::id::{FactionId, IdProvider};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Attitude {
    Friendly,
    Neutral,
    Hostile,
}

pub type FactionSet = HashSet<FactionId>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Faction {
    id: FactionId,
    name: String,
    attitudes: HashMap<FactionId, Attitude>,
    /// Default attitude towards other factions not explicitly listed in `attitudes`
    default_cross_attitude: Attitude,
    /// Default attitude towards members of the same faction
    default_intra_attitude: Attitude,
}

/// Optional per-entity attitude overrides
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttitudeOverride {
    /// Overrides for specific entities, e.g. due to Charm effects
    pub entities: HashMap<Entity, Attitude>,
    /// Overrides for entire factions, e.g. due to diplomatic events
    pub factions: HashMap<FactionId, Attitude>,
}

impl Faction {
    pub fn new(
        id: FactionId,
        name: String,
        attitudes: HashMap<FactionId, Attitude>,
        default_cross_attitude: Attitude,
        default_intra_attitude: Attitude,
    ) -> Self {
        Self {
            id,
            name,
            attitudes,
            default_cross_attitude,
            default_intra_attitude,
        }
    }

    pub fn attitude_towards(&self, other: &Faction) -> Attitude {
        if self.id == other.id {
            self.default_intra_attitude
        } else {
            self.attitudes
                .get(&other.id)
                .cloned()
                .unwrap_or(self.default_cross_attitude)
        }
    }

    pub fn id(&self) -> &FactionId {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn set_attitude(&mut self, other: &FactionId, attitude: Attitude) {
        self.attitudes.insert(other.clone(), attitude);
    }

    pub fn remove_attitude(&mut self, other: &FactionId) {
        self.attitudes.remove(other);
    }

    pub fn attitudes(&self) -> &HashMap<FactionId, Attitude> {
        &self.attitudes
    }

    pub fn default_cross_attitude(&self) -> Attitude {
        self.default_cross_attitude
    }

    pub fn default_intra_attitude(&self) -> Attitude {
        self.default_intra_attitude
    }
}

impl IdProvider for Faction {
    type Id = FactionId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

impl AttitudeOverride {
    pub fn new() -> Self {
        Self {
            entities: HashMap::new(),
            factions: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use rstest::{fixture, rstest};

    use super::*;

    #[fixture]
    fn faction_knights() -> Faction {
        Faction::new(
            FactionId::new("nat20_core", "faction.knights"),
            "Knights".to_string(),
            HashMap::from([]),
            Attitude::Neutral,
            Attitude::Friendly,
        )
    }

    #[fixture]
    fn faction_orcs() -> Faction {
        Faction::new(
            FactionId::new("nat20_core", "faction.orcs"),
            "Orcs".to_string(),
            HashMap::from([]),
            Attitude::Neutral,
            Attitude::Friendly,
        )
    }

    #[rstest]
    fn attitude_towards_self(faction_knights: Faction) {
        assert_eq!(
            faction_knights.attitude_towards(&faction_knights),
            Attitude::Friendly
        );
    }

    #[rstest]
    fn attitude_towards_other_with_explicit_attitude(
        faction_knights: Faction,
        faction_orcs: Faction,
    ) {
        let mut faction = faction_knights;
        faction.set_attitude(&faction_orcs.id, Attitude::Hostile);
        assert_eq!(faction.attitude_towards(&faction_orcs), Attitude::Hostile);
    }

    #[rstest]
    fn attitude_towards_other_with_default_cross_attitude(
        faction_knights: Faction,
        faction_orcs: Faction,
    ) {
        assert_eq!(
            faction_knights.attitude_towards(&faction_orcs),
            Attitude::Neutral
        );
    }

    #[rstest]
    fn set_and_remove_attitude(faction_knights: Faction) {
        let mut faction = faction_knights;
        let orc_id = FactionId::new("nat20_core", "faction.orcs");
        faction.set_attitude(&orc_id, Attitude::Hostile);
        assert_eq!(faction.attitudes().get(&orc_id), Some(&Attitude::Hostile));
        faction.remove_attitude(&orc_id);
        assert_eq!(faction.attitudes().get(&orc_id), None);
    }

    #[test]
    fn attitude_override_new() {
        let override_obj = AttitudeOverride::new();
        assert!(override_obj.entities.is_empty());
        assert!(override_obj.factions.is_empty());
    }
}
