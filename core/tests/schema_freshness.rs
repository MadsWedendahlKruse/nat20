//! Asserts the checked-in `assets/registries/.schemas/*.schema.json` files match
//! the current `*Definition` types, so schema-affecting changes fail loudly
//! instead of silently drifting from the editor tooling.

use std::path::PathBuf;

use nat20_core::registry::schema::{registry_schemas, schema_file_name};

#[test]
fn checked_in_schemas_are_up_to_date() {
    let schemas_dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../assets/registries/.schemas");

    for schema in registry_schemas() {
        let path = schemas_dir.join(schema_file_name(schema.registry));
        let on_disk = std::fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("failed to read {:?}: {}", path, error))
            .replace("\r\n", "\n");

        assert_eq!(
            on_disk, schema.json,
            "{:?} is stale; regenerate with `cargo run -p nat20_core --example generate_schemas`",
            path
        );
    }
}
