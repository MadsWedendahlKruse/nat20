use std::{fmt::Display, str::FromStr};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

use crate::{
    components::{
        ability::Ability,
        d20::{D20CheckDC, D20CheckMap},
        effects::hooks::D20CheckHooks,
    },
    systems::{self},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum SavingThrowKind {
    Ability(Ability),
    Death, // sounds a bit edgy lol
    Concentration,
}

impl Default for SavingThrowKind {
    fn default() -> Self {
        SavingThrowKind::Ability(Ability::Strength)
    }
}

impl IntoEnumIterator for SavingThrowKind {
    type Iterator = std::vec::IntoIter<SavingThrowKind>;

    fn iter() -> Self::Iterator {
        let mut v = Vec::with_capacity(2 + Ability::iter().len());
        v.push(SavingThrowKind::Death);
        v.push(SavingThrowKind::Concentration);
        v.extend(Ability::iter().map(SavingThrowKind::Ability));
        v.into_iter()
    }
}

impl Display for SavingThrowKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SavingThrowKind::Ability(ability) => write!(f, "{}", ability),
            SavingThrowKind::Death => write!(f, "Death"),
            SavingThrowKind::Concentration => write!(f, "Concentration"),
        }
    }
}

impl FromStr for SavingThrowKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("death") {
            return Ok(SavingThrowKind::Death);
        }
        if s.eq_ignore_ascii_case("concentration") {
            return Ok(SavingThrowKind::Concentration);
        }
        for ability in Ability::iter() {
            if let Ok(parsed_ability) = s.parse::<Ability>() {
                if parsed_ability == ability {
                    return Ok(SavingThrowKind::Ability(ability));
                }
            }
        }
        Err(format!("Unknown saving throw kind: {}", s))
    }
}

impl TryFrom<String> for SavingThrowKind {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<SavingThrowKind> for String {
    fn from(kind: SavingThrowKind) -> Self {
        kind.to_string()
    }
}

pub type SavingThrowSet = D20CheckMap<SavingThrowKind>;

pub type SavingThrowDC = D20CheckDC<SavingThrowKind>;

pub fn get_saving_throw_hooks(
    kind: &SavingThrowKind,
    world: &World,
    entity: Entity,
) -> Vec<D20CheckHooks> {
    systems::effects::effects(world, entity)
        .values()
        .filter_map(|e| e.effect().on_saving_throw.get(&kind))
        .cloned()
        .collect()
}

impl Default for SavingThrowSet {
    fn default() -> Self {
        Self::new(
            |kind| match kind {
                SavingThrowKind::Ability(ability) => Some(*ability),
                SavingThrowKind::Death => None,
                SavingThrowKind::Concentration => Some(Ability::Constitution),
            },
            get_saving_throw_hooks,
        )
    }
}
