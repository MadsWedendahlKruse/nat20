use std::ops::Deref;

use hecs::Entity;
use serde::{Deserialize, Serialize};
use std::{fmt, fmt::Debug, hash::Hash, str::FromStr};
use strum::Display;

pub const DEFAULT_NAMESPACE: &str = "nat20_core";
pub const NAMESPACE_SEPARATOR: &str = "::";

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display, Serialize, Deserialize)]
pub enum IdError {
    InvalidPrefix { expected: String, found: String },
    EmptyId,
}

macro_rules! id_newtypes {
    ($($name:ident => $prefix:literal),+) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
            #[serde(try_from = "String", into = "String")]
            pub struct $name {
                pub(crate) namespace: String,
                pub(crate) id: String,
            }

            impl $name {
                 /// The required id prefix for this type, e.g. `ClassId` -> "class".
                pub const PREFIX: &'static str = $prefix;

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
                    let parts: Vec<&str> = s.splitn(2, NAMESPACE_SEPARATOR).collect();
                    let (namespace, id) = if parts.len() != 2 {
                        (DEFAULT_NAMESPACE, parts[0])
                    } else {
                        (parts[0], parts[1])
                    };
                    if !id.starts_with(Self::PREFIX) {
                        return Err(IdError::InvalidPrefix {
                            expected: Self::PREFIX.into(),
                            found: id.to_string(),
                        });
                    }
                    if id.trim().is_empty() {
                        return Err(IdError::EmptyId);
                    }

                    Ok(Self::new(namespace.to_string(), id.to_string()))
                }
            }

            impl From<&str> for $name {
                fn from(value: &str) -> Self {
                    value.parse().unwrap_or_else(|err| panic!("Failed to parse {} from string '{}': {:?}", stringify!($name), value, err))
                }
            }

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "{}{}{}", self.namespace, NAMESPACE_SEPARATOR, self.id)
                }
            }

            impl TryFrom<String> for $name {
                type Error = IdError;

                fn try_from(value: String) -> Result<Self, Self::Error> {
                    value.parse()
                }
            }

            impl From<$name> for String {
                fn from(value: $name) -> Self {
                    value.to_string()
                }
            }

            impl schemars::JsonSchema for $name {
                fn schema_name() -> std::borrow::Cow<'static, str> {
                    std::borrow::Cow::Borrowed(stringify!($name))
                }

                fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
                    schemars::json_schema!({
                        "type": "string",
                        "description": concat!(
                            "Registry id `[namespace::]", $prefix, ".<name>`, e.g. `",
                            "nat20_core::", $prefix, ".example`. The namespace defaults to `nat20_core`."),
                        "pattern": concat!("^([A-Za-z0-9_]+::)?", $prefix, "[A-Za-z0-9_.]*$")
                    })
                }
            }
        )+

        pub enum Id {
            $($name($name),)+
        }

        impl FromStr for Id {
            type Err = IdError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                $(
                    if let Ok(id) = s.parse::<$name>() {
                        return Ok(Id::$name(id));
                    }
                )+
                Err(IdError::InvalidPrefix {
                    expected: format!("one of: {}", vec![$($name::PREFIX),+].join(", ")),
                    found: s.to_string(),
                })
            }
        }

        impl fmt::Display for Id {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(Id::$name(id) => write!(f, "{}", id),)+
                }
            }
        }
    };
}

id_newtypes!(
    ClassId => "class",
    SubclassId => "subclass",
    ItemId => "item",
    EffectId => "effect",
    ResourceId => "resource",
    ActionId => "action",
    SpellId => "spell",
    FeatId => "feat",
    BackgroundId => "background",
    SpeciesId => "species",
    SubspeciesId => "subspecies",
    AIControllerId => "aicontroller",
    FactionId => "faction",
    ScriptId => "script"
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
