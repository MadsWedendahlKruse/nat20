//! Generates JSON Schemas for the registry content files, consumed by the
//! editor (`.vscode/settings.json` maps each `assets/registries/<kind>/`
//! directory to its schema). Regenerate with
//! `cargo run -p nat20_core --example generate_schemas`; a test asserts the
//! checked-in schemas are up to date.

use schemars::{JsonSchema, generate::SchemaSettings};
use serde::Serialize;
use serde_json::ser::{PrettyFormatter, Serializer};

use crate::{
    components::{
        background::Background, class::Subclass, faction::Faction, items::inventory::ItemInstance,
        resource::Resource,
    },
    registry::serialize::{
        action::ActionDefinition,
        class::ClassDefinition,
        effect::EffectDefinition,
        feat::FeatDefinition,
        species::{SpeciesDefinition, SubspeciesDefinition},
        spell::SpellDefinition,
    },
};

/// One generated schema: the registry directory it applies to and the schema
/// JSON text.
pub struct RegistrySchema {
    pub registry: &'static str,
    pub json: String,
}

fn schema_for<T: JsonSchema>(registry: &'static str) -> RegistrySchema {
    // Draft-07 for maximum compatibility with VS Code's JSON language server.
    let schema = SchemaSettings::draft07()
        .into_generator()
        .into_root_schema_for::<T>();

    let mut buffer = Vec::new();
    let formatter = PrettyFormatter::with_indent(b"    ");
    let mut serializer = Serializer::with_formatter(&mut buffer, formatter);
    schema
        .serialize(&mut serializer)
        .expect("schema serialization cannot fail");
    let mut json = String::from_utf8(buffer).expect("schema JSON is valid UTF-8");
    json.push('\n');

    RegistrySchema { registry, json }
}

/// Schemas for every registry kind, keyed by the directory name under
/// `assets/registries/`.
pub fn registry_schemas() -> Vec<RegistrySchema> {
    vec![
        schema_for::<ActionDefinition>("actions"),
        schema_for::<Background>("backgrounds"),
        schema_for::<ClassDefinition>("classes"),
        schema_for::<EffectDefinition>("effects"),
        schema_for::<Faction>("factions"),
        schema_for::<FeatDefinition>("feats"),
        schema_for::<ItemInstance>("items"),
        schema_for::<Resource>("resources"),
        schema_for::<SpeciesDefinition>("species"),
        schema_for::<SpellDefinition>("spells"),
        schema_for::<Subclass>("subclasses"),
        schema_for::<SubspeciesDefinition>("subspecies"),
    ]
}

/// File name of a registry schema inside the `schemas/` directory, singular by
/// convention (`actions` -> `action.schema.json`).
pub fn schema_file_name(registry: &str) -> String {
    let singular = match registry {
        "species" => "species",
        "subspecies" => "subspecies",
        "classes" => "class",
        "subclasses" => "subclass",
        other => other.trim_end_matches('s'),
    };
    format!("{}.schema.json", singular)
}
