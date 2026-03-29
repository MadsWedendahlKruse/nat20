use std::ops::Deref;

use hecs::Entity;
use serde::{Deserialize, Serialize};
use std::{fmt, fmt::Debug, hash::Hash, str::FromStr};
use strum::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display, Serialize, Deserialize)]
pub enum IdError {
    MissingNamespace,
    InvalidPrefix { expected: String, found: String },
    EmptyId,
}

macro_rules! id_newtypes {
    ($($name:ident),+) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
            #[serde(try_from = "String", into = "String")]
            pub struct $name {
                pub(crate) namespace: String,
                pub(crate) id: String,
            }

            impl $name {
                pub fn new(namespace: impl Into<String>, id: impl Into<String>) -> Self {
                    Self {
                        namespace: namespace.into(),
                        id: id.into(),
                    }
                }

                pub fn namespace(&self) -> &str {
                    &self.namespace
                }

                pub fn id(&self) -> &str {
                    &self.id
                }
            }

            impl FromStr for $name {
                type Err = IdError;

                fn from_str(s: &str) -> Result<Self, IdError> {
                    let parts: Vec<&str> = s.splitn(2, "::").collect();
                    if parts.len() != 2 {
                        return Err(IdError::MissingNamespace);
                    }
                    let prefix = stringify!($name).to_lowercase().replace("id", "");
                    let id = parts[1];
                    if !id.starts_with(&prefix) {
                        return Err(IdError::InvalidPrefix {
                            expected: prefix,
                            found: id.to_string(),
                        });
                    }
                    if id.trim().is_empty() {
                        return Err(IdError::EmptyId);
                    }

                    Ok(Self::new(parts[0].to_string(), parts[1].to_string()))
                }
            }

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "{}::{}", self.namespace, self.id)
                }
            }

            impl TryFrom<String> for $name {
                type Error = IdError;

                fn try_from(value: String) -> Result<Self, Self::Error> {
                    Self::from_str(&value)
                }
            }

            impl From<$name> for String {
                fn from(value: $name) -> Self {
                    value.to_string()
                }
            }
        )+
    };
}

id_newtypes!(
    ClassId,
    SubclassId,
    ItemId,
    EffectId,
    ResourceId,
    ActionId,
    SpellId,
    FeatId,
    BackgroundId,
    SpeciesId,
    SubspeciesId,
    AIControllerId,
    FactionId,
    ScriptId
);

impl Into<ActionId> for SpellId {
    fn into(self) -> ActionId {
        let id = self.id.replacen("spell", "action", 1);
        ActionId::new(self.namespace, id)
    }
}

impl Into<ActionId> for &SpellId {
    fn into(self) -> ActionId {
        let id = self.id.replacen("spell", "action", 1);
        ActionId::new(self.namespace.clone(), id)
    }
}

impl Into<SpellId> for ActionId {
    fn into(self) -> SpellId {
        let id = self.id.replacen("action", "spell", 1);
        SpellId::new(self.namespace, id)
    }
}

impl Into<SpellId> for &ActionId {
    fn into(self) -> SpellId {
        let id = self.id.replacen("action", "spell", 1);
        SpellId::new(self.namespace.clone(), id)
    }
}

pub trait IdProvider {
    type Id: Eq + Hash + Clone + Debug;

    fn id(&self) -> &Self::Id;
}

// TODO: Not sure if this is the best place for this
/// Name is a simple wrapper around a String to provide a type-safe way to
/// handle names when querying entities in the game world. The alternative is to
/// use a String directly, but a String can be ambiguous in terms of what it
/// represents
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(String);

impl Name {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn to_string(&self) -> String {
        self.0.clone()
    }

    pub fn to_string_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

// TODO: Not sure if this just causes more problems than it solves
/// Identifier for an entity in the game world.
/// This is used to uniquely identify entities, such as characters or creatures.
/// In most cases the id (`Entity`) is meaningless outside the context of the
/// world, so for convenience we also store the name of the entity.
/// When despawning an entity from the world, the `Entity` ID becomes invalid, which
/// makes it impossible to fetch the name of the entity when rendering events related
/// to the entity after it has been despawned.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityIdentifier {
    id: Entity,
    name: Name,
}

impl EntityIdentifier {
    pub fn new(id: Entity, name: Name) -> Self {
        Self { id, name }
    }

    pub fn id(&self) -> Entity {
        self.id
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn from_world(world: &hecs::World, entity: Entity) -> Self {
        let name = world
            .get::<&Name>(entity)
            .map(|name| name.deref().clone())
            .unwrap_or_else(|_| Name::new("Unnamed Entity"));
        Self::new(entity, name)
    }
}
