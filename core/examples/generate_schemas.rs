//! Writes the registry JSON Schemas to `schemas/` at the repository root.
//! Run after changing any `*Definition` type:
//! `cargo run -p nat20_core --example generate_schemas`

use std::{fs, path::PathBuf};

use nat20_core::registry::schema::{registry_schemas, schema_file_name};

fn main() {
    let schemas_dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../assets/registries/.schemas");
    fs::create_dir_all(&schemas_dir).expect("failed to create schemas directory");

    for schema in registry_schemas() {
        let path = schemas_dir.join(schema_file_name(schema.registry));
        fs::write(&path, &schema.json)
            .unwrap_or_else(|error| panic!("failed to write {:?}: {}", path, error));
        println!("wrote {:?}", path);
    }
}
