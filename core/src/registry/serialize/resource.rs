use std::{collections::BTreeMap, str::FromStr};

use schemars::JsonSchema;
use serde::Deserialize;
use serde_with::{DisplayFromStr, serde_as};

use crate::{
    components::{
        modifier::ModifierSource,
        resource::{ResourceBudget, ResourceBudgetError, ResourceBudgetKind},
    },
    registry::serialize::schema::impl_string_schema,
};

#[derive(Clone, Deserialize)]
#[serde(try_from = "String")]
pub struct ResourceBudgetDefinition {
    current_uses: u8,
    max_uses: u8,
}

impl FromStr for ResourceBudgetDefinition {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split('/').collect();
        if parts.len() == 1 {
            let max_uses = parts[0]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceBudget format: {}", s))?;
            Ok(ResourceBudgetDefinition {
                current_uses: max_uses,
                max_uses,
            })
        } else if parts.len() == 2 {
            let current_uses = parts[0]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceBudget format: {}", s))?;
            let max_uses = parts[1]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceBudget format: {}", s))?;
            Ok(ResourceBudgetDefinition {
                current_uses,
                max_uses,
            })
        } else {
            Err(format!("Invalid ResourceBudget format: {}", s))
        }
    }
}

impl TryFrom<String> for ResourceBudgetDefinition {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl_string_schema!(
    ResourceBudgetDefinition,
    "ResourceBudget",
    "description": "A resource budget: `<max uses>` (spawns full) or `<current>/<max>`.",
    "examples": ["2", "1/4"]
);

impl ResourceBudgetDefinition {
    pub fn try_into_budget(
        self,
        source: ModifierSource,
    ) -> Result<ResourceBudget, ResourceBudgetError> {
        ResourceBudget::new(self.current_uses, self.max_uses, source)
    }
}

#[serde_as]
#[derive(Clone, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ResourceBudgetKindDefinition {
    Flat(ResourceBudgetDefinition),
    // An untagged enum is buffered before a variant is picked, and the buffer
    // keeps the map keys as strings, so the tier has to be parsed explicitly
    Tiered(
        #[serde_as(as = "BTreeMap<DisplayFromStr, _>")]
        #[schemars(with = "BTreeMap<u8, ResourceBudgetDefinition>")]
        BTreeMap<u8, ResourceBudgetDefinition>,
    ),
}

impl ResourceBudgetKindDefinition {
    pub fn try_into_budget_kind(
        self,
        source: ModifierSource,
    ) -> Result<ResourceBudgetKind, ResourceBudgetError> {
        match self {
            ResourceBudgetKindDefinition::Flat(flat) => {
                Ok(ResourceBudgetKind::Flat(flat.try_into_budget(source)?))
            }
            ResourceBudgetKindDefinition::Tiered(tiered) => {
                let mut converted = BTreeMap::new();
                for (k, v) in tiered {
                    converted.insert(k, v.try_into_budget(source.clone())?);
                }
                Ok(ResourceBudgetKind::Tiered(converted))
            }
        }
    }
}
