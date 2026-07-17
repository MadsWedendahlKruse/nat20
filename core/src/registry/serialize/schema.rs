/// Shared documentation snippet for schemas of expression-language strings.
pub const EXPRESSION_VARIABLES_DOC: &str = "Expressions may use the variables `spell_level`, \
     `caster_level`, `character_level` and `<class>.level` (e.g. `class.wizard.level`).";

/// Implements `JsonSchema` for string-backed spec types
/// (`#[serde(try_from = "String", into = "String")]`), whose derived schema
/// would otherwise wrongly describe the Rust struct/enum instead of the
/// string the JSON actually contains. The schema is inlined as a plain
/// string schema carrying editor-facing documentation.
///
/// Extra schema properties (`"description"`, `"examples"`, `"enum"`,
/// `"pattern"`, ...) are passed through to `schemars::json_schema!`.
macro_rules! impl_string_schema {
    ($type:ty, $name:literal, $($schema:tt)+) => {
        impl schemars::JsonSchema for $type {
            fn schema_name() -> std::borrow::Cow<'static, str> {
                std::borrow::Cow::Borrowed($name)
            }

            fn inline_schema() -> bool {
                true
            }

            fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
                schemars::json_schema!({
                    "type": "string",
                    $($schema)+
                })
            }
        }
    };
}
pub(crate) use impl_string_schema;

/// Implements `JsonSchema` for a type deserialized via another type
/// (`#[serde(from = "$via")]`) by delegating to `$via`'s schema.
macro_rules! impl_schema_via {
    ($type:ty, $via:ty) => {
        impl schemars::JsonSchema for $type {
            fn schema_name() -> std::borrow::Cow<'static, str> {
                <$via as schemars::JsonSchema>::schema_name()
            }

            fn schema_id() -> std::borrow::Cow<'static, str> {
                <$via as schemars::JsonSchema>::schema_id()
            }

            fn inline_schema() -> bool {
                <$via as schemars::JsonSchema>::inline_schema()
            }

            fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
                <$via as schemars::JsonSchema>::json_schema(generator)
            }
        }
    };
}
pub(crate) use impl_schema_via;
