use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize, de::DeserializeOwned};
use strum::IntoEnumIterator;

use crate::{
    components::{
        ability::Ability,
        d20::{AdvantageType, D20CheckOutcome},
        damage::{AttackSource, DamageType, MitigationOperation},
        items::equipment::weapon::WeaponKind,
        saving_throw::SavingThrowKind,
        skill::Skill,
    },
    registry::serialize::{quantity::LengthExpressionDefinition, schema::impl_string_schema},
};

/// For spec types that:
/// - implement `FromStr<Err = String>`
/// - have a `pub raw: String` field
macro_rules! impl_string_backed_spec {
    ($type:ty) => {
        impl TryFrom<String> for $type {
            type Error = String;

            fn try_from(value: String) -> Result<Self, Self::Error> {
                value.parse()
            }
        }

        impl From<$type> for String {
            fn from(spec: $type) -> Self {
                spec.raw
            }
        }
    };
}

/// For types that:
/// - implement `FromStr<Err = String>`
/// - implement `Display`
/// - and you want Serde round-trip via `String`
macro_rules! impl_display_roundtrip_spec {
    ($type:ty) => {
        impl TryFrom<String> for $type {
            type Error = String;

            fn try_from(value: String) -> Result<Self, Self::Error> {
                value.parse()
            }
        }

        impl From<$type> for String {
            fn from(value: $type) -> Self {
                value.to_string()
            }
        }
    };
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum D20Modifier {
    Advantage(AdvantageType),
    Flat(i32),
    ForceOutcome(D20CheckOutcome),
    CritThreshold(u8),
}

impl Display for D20Modifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            D20Modifier::Advantage(a) => write!(f, "{}", serde_plain::to_string(a).unwrap()),
            D20Modifier::Flat(n) => {
                if *n >= 0 {
                    write!(f, "+{}", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            D20Modifier::ForceOutcome(o) => {
                write!(f, "{}", serde_plain::to_string(o).unwrap())
            }
            D20Modifier::CritThreshold(n) => write!(f, "crit(-{})", n),
        }
    }
}

impl FromStr for D20Modifier {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let s = input.trim().to_lowercase();

        if s.starts_with("crit(-") && s.ends_with(')') {
            let inner = &s[6..s.len() - 1];
            let n: u8 = inner
                .parse()
                .map_err(|_| format!("Invalid crit threshold in '{}'", input))?;
            return Ok(D20Modifier::CritThreshold(n));
        }

        parse_d20_modifier(&s, input)
    }
}

impl_display_roundtrip_spec!(D20Modifier);

impl_string_schema!(
    D20Modifier,
    "D20Modifier",
    "description": "A d20 roll modifier: `advantage`, `disadvantage`, a flat bonus/penalty \
         (`+2`, `-1`), a forced outcome (`success`, `failure`, `critical_success`, \
         `critical_failure`) or a lowered crit threshold (`crit(-N)`).",
    "examples": ["advantage", "+2", "crit(-1)"]
);

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct D20CheckModifierProvider<T: Clone + DeserializeOwned + IntoEnumIterator> {
    #[serde(skip)]
    pub kind: Vec<T>,
    #[serde(skip)]
    pub modifier: D20Modifier,
    pub raw: String,
}

impl<T> FromStr for D20CheckModifierProvider<T>
where
    T: Clone + DeserializeOwned + IntoEnumIterator,
{
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        // Examples:
        // "perception advantage"
        // "stealth-1"
        // "investigation+2"
        // "strength disadvantage"
        // "all disadvantage"
        // "dexterity critical_success"

        let normalized = normalize_spec_string(input);

        let (check_str, modifier_str) =
            split_first_delimiter(&normalized, &[' ', '+', '-'], "D20CheckModifierProvider")?;

        let kind = if check_str.to_lowercase().eq("all") {
            T::iter().collect()
        } else {
            vec![parse_plain_enum(check_str, "check kind", &normalized)?]
        };

        let modifier = D20Modifier::from_str(modifier_str)?;

        Ok(D20CheckModifierProvider {
            raw: normalized,
            kind,
            modifier,
        })
    }
}

impl<T> TryFrom<String> for D20CheckModifierProvider<T>
where
    T: Clone + DeserializeOwned + IntoEnumIterator,
{
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl<T> From<D20CheckModifierProvider<T>> for String
where
    T: Clone + DeserializeOwned + IntoEnumIterator,
{
    fn from(spec: D20CheckModifierProvider<T>) -> Self {
        spec.raw
    }
}

impl<T> schemars::JsonSchema for D20CheckModifierProvider<T>
where
    T: Clone + DeserializeOwned + IntoEnumIterator,
{
    fn schema_name() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Owned(format!(
            "D20CheckModifierProvider<{}>",
            std::any::type_name::<T>().rsplit("::").next().unwrap()
        ))
    }

    fn inline_schema() -> bool {
        true
    }

    fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "type": "string",
            "description": "`<check kind or `all`> <modifier>` where the modifier is \
                 `advantage`, `disadvantage`, a flat bonus/penalty (`+2`, `-1`), a forced \
                 outcome (`success`, ..., `critical_failure`) or `crit(-N)`.",
            "examples": ["perception advantage", "stealth+2", "all disadvantage"]
        })
    }
}

pub type SkillModifierProvider = D20CheckModifierProvider<Skill>;
pub type SavingThrowModifierProvider = D20CheckModifierProvider<SavingThrowKind>;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct AbilityModifierProvider {
    #[serde(skip)]
    pub ability: Ability,
    #[serde(skip)]
    pub delta: i32,
    pub raw: String,
}

impl FromStr for AbilityModifierProvider {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        // Examples:
        // "strength+2"
        // "intelligence-1"

        let (name, delta_part) =
            split_first_delimiter(input, &['+', '-'], "AbilityModifierProvider")?;

        let ability: Ability = parse_plain_enum(name, "ability", input)?;

        let delta: i32 = delta_part
            .parse()
            .map_err(|_| format!("Invalid modifier in '{}'", input))?;

        Ok(AbilityModifierProvider {
            raw: input.to_string(),
            ability,
            delta,
        })
    }
}

impl_string_backed_spec!(AbilityModifierProvider);

impl_string_schema!(
    AbilityModifierProvider,
    "AbilityModifierProvider",
    "description": "`<ability><+/-delta>`, e.g. `strength+2`, `intelligence-1`.",
    "examples": ["strength+2", "intelligence-1"]
);

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct DamageResistanceProvider {
    #[serde(skip)]
    pub damage_type: DamageType,
    #[serde(skip)]
    pub operation: MitigationOperation,
    pub raw: String,
}

impl FromStr for DamageResistanceProvider {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        // Examples:
        // "fire resistance"
        // "cold immunity"
        // "force -2"
        let normalized = normalize_spec_string(input);
        let parts: Vec<&str> = normalized.split_whitespace().collect();

        if parts.len() != 2 {
            return Err(format!("Invalid DamageResistanceProvider: {}", input));
        }

        let damage_type: DamageType = parse_plain_enum(parts[0].trim(), "damage type", input)?;

        let operation: MitigationOperation =
            if let Ok(operation) = serde_plain::from_str(parts[1].trim()) {
                operation
            } else if parts[1].trim().starts_with('-') {
                let amount_str = parts[1].trim().trim_start_matches('-');
                let amount: i32 = amount_str
                    .parse()
                    .map_err(|_| format!("Invalid flat reduction amount in '{}'", input))?;
                MitigationOperation::FlatReduction(amount)
            } else {
                return Err(format!("Unknown mitigation operation in '{}'", input));
            };

        Ok(DamageResistanceProvider {
            raw: input.to_string(),
            damage_type,
            operation,
        })
    }
}

impl_string_backed_spec!(DamageResistanceProvider);

impl_string_schema!(
    DamageResistanceProvider,
    "DamageResistanceProvider",
    "description": "`<damage type> <mitigation>` where the mitigation is `resistance`, \
         `vulnerability`, `immunity` or a flat reduction (`-2`).",
    "examples": ["fire resistance", "cold immunity", "force -2"]
);

// TODO: Not sure if this is the correct place
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct AttackSourceDefinition {
    pub source: AttackSource,
    pub raw: String,
}

impl FromStr for AttackSourceDefinition {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let normalized = normalize_spec_string(input);
        let source = match normalized.as_str() {
            "melee_weapon" => AttackSource::Weapon(WeaponKind::Melee),
            "ranged_weapon" => AttackSource::Weapon(WeaponKind::Ranged),
            "unarmed" => AttackSource::Weapon(WeaponKind::Unarmed),
            "spell" => AttackSource::Spell,
            _ => return Err(format!("Unknown attack source in '{}'", input)),
        };

        Ok(AttackSourceDefinition {
            raw: input.to_string(),
            source,
        })
    }
}

impl_string_backed_spec!(AttackSourceDefinition);

impl_string_schema!(
    AttackSourceDefinition,
    "AttackSourceDefinition",
    "description": "The source of an attack.",
    "enum": ["melee_weapon", "ranged_weapon", "unarmed", "spell"]
);

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ArmorClassModifierProvider {
    #[serde(skip)]
    pub delta: i32,
    pub raw: String,
}

impl FromStr for ArmorClassModifierProvider {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let normalized = normalize_spec_string(input);

        let delta: i32 = normalized
            .parse()
            .map_err(|_| format!("Invalid modifier in '{}'", input))?;

        Ok(ArmorClassModifierProvider {
            raw: normalized,
            delta,
        })
    }
}

impl_string_backed_spec!(ArmorClassModifierProvider);

impl_string_schema!(
    ArmorClassModifierProvider,
    "ArmorClassModifierProvider",
    "description": "Flat armor class delta, e.g. `2` or `-1`.",
    "examples": ["2", "-1"]
);

fn find_index_of_first(s: &str, chars: &[char]) -> Option<usize> {
    s.char_indices()
        .find(|(_, c)| chars.contains(c))
        .map(|(i, _)| i)
}

fn normalize_spec_string(input: &str) -> String {
    input.trim().to_lowercase()
}

fn split_first_delimiter<'a>(
    s: &'a str,
    delimiters: &[char],
    kind_name: &str,
) -> Result<(&'a str, &'a str), String> {
    if let Some(index) = find_index_of_first(s, delimiters) {
        let (left, right) = s.split_at(index);
        Ok((left.trim(), right.trim()))
    } else {
        Err(format!("Invalid {} spec: {}", kind_name, s))
    }
}

fn parse_plain_enum<T>(name: &str, field_name: &str, whole: &str) -> Result<T, String>
where
    T: DeserializeOwned,
{
    serde_plain::from_str(name).map_err(|_| format!("Unknown {} in '{}'", field_name, whole))
}

fn parse_d20_modifier(modifier_str: &str, full_input: &str) -> Result<D20Modifier, String> {
    let modifier_str = modifier_str.trim();
    match modifier_str {
        "" => Err(format!("Missing modifier in '{}'", full_input)),
        "advantage" => Ok(D20Modifier::Advantage(AdvantageType::Advantage)),
        "disadvantage" => Ok(D20Modifier::Advantage(AdvantageType::Disadvantage)),
        "success" => Ok(D20Modifier::ForceOutcome(D20CheckOutcome::Success)),
        "failure" => Ok(D20Modifier::ForceOutcome(D20CheckOutcome::Failure)),
        "critical_success" => Ok(D20Modifier::ForceOutcome(D20CheckOutcome::CriticalSuccess)),
        "critical_failure" => Ok(D20Modifier::ForceOutcome(D20CheckOutcome::CriticalFailure)),
        _ => {
            let delta: i32 = modifier_str
                .parse()
                .map_err(|_| format!("Invalid modifier in '{}'", full_input))?;
            Ok(D20Modifier::Flat(delta))
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum SpeedModifier {
    Flat(LengthExpressionDefinition),
    Multiplier(f32),
}

impl Display for SpeedModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpeedModifier::Flat(value) => {
                write!(f, "{}", value)
            }
            SpeedModifier::Multiplier(value) => {
                write!(f, "x{}", value)
            }
        }
    }
}

impl FromStr for SpeedModifier {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let normalized = normalize_spec_string(input);

        if normalized.starts_with('x') {
            let multiplier_str = &normalized[1..];
            let multiplier: f32 = multiplier_str
                .parse()
                .map_err(|_| format!("Invalid speed multiplier in '{}'", input))?;
            Ok(SpeedModifier::Multiplier(multiplier))
        } else {
            let length_expr: LengthExpressionDefinition = normalized
                .parse()
                .map_err(|e| format!("Invalid length expression in '{}': {}", input, e))?;
            Ok(SpeedModifier::Flat(length_expr))
        }
    }
}

impl_display_roundtrip_spec!(SpeedModifier);

impl_string_schema!(
    SpeedModifier,
    "SpeedModifier",
    "description": "Flat speed bonus as a length expression (`10 feet`) or a multiplier (`x2`).",
    "examples": ["10 feet", "x2"]
);

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct SpeedModifierProvider {
    pub modifier: SpeedModifier,
    pub raw: String,
}

impl FromStr for SpeedModifierProvider {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let modifier: SpeedModifier = input.parse()?;

        Ok(SpeedModifierProvider {
            raw: input.to_string(),
            modifier,
        })
    }
}

impl_string_backed_spec!(SpeedModifierProvider);

impl_string_schema!(
    SpeedModifierProvider,
    "SpeedModifierProvider",
    "description": "Flat speed bonus as a length expression (`10 feet`) or a multiplier (`x2`).",
    "examples": ["10 feet", "x2"]
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn skill_modifier_provider_parsing() {
        let spec: SkillModifierProvider = "stealth+2".parse().unwrap();
        assert_eq!(spec.kind[0], Skill::Stealth);
        assert!(matches!(spec.modifier, D20Modifier::Flat(2)));

        let spec: SkillModifierProvider = "athletics-1".parse().unwrap();
        assert_eq!(spec.kind[0], Skill::Athletics);
        assert!(matches!(spec.modifier, D20Modifier::Flat(-1)));

        let spec: SkillModifierProvider = "perception advantage".parse().unwrap();
        assert_eq!(spec.kind[0], Skill::Perception);
        assert!(matches!(
            spec.modifier,
            D20Modifier::Advantage(AdvantageType::Advantage)
        ));

        let spec: SkillModifierProvider = "all disadvantage".parse().unwrap();
        assert_eq!(spec.kind.len(), Skill::iter().count());
        assert!(matches!(
            spec.modifier,
            D20Modifier::Advantage(AdvantageType::Disadvantage)
        ));

        let spec: SkillModifierProvider = "insight critical_success".parse().unwrap();
        assert_eq!(spec.kind[0], Skill::Insight);
        assert!(matches!(
            spec.modifier,
            D20Modifier::ForceOutcome(D20CheckOutcome::CriticalSuccess)
        ));

        let spec: SkillModifierProvider = "stealth crit(-5)".parse().unwrap();
        assert_eq!(spec.kind[0], Skill::Stealth);
        assert!(matches!(spec.modifier, D20Modifier::CritThreshold(5)));
    }

    #[test]
    fn saving_throw_modifier_provider_parsing() {
        let spec: SavingThrowModifierProvider = "constitution advantage".parse().unwrap();
        assert_eq!(
            spec.kind[0],
            SavingThrowKind::Ability(Ability::Constitution)
        );
        assert!(matches!(
            spec.modifier,
            D20Modifier::Advantage(AdvantageType::Advantage)
        ));

        let spec: SavingThrowModifierProvider = "dex disadvantage".parse().unwrap();
        assert_eq!(spec.kind[0], SavingThrowKind::Ability(Ability::Dexterity));
        assert!(matches!(
            spec.modifier,
            D20Modifier::Advantage(AdvantageType::Disadvantage)
        ));

        let spec: SavingThrowModifierProvider = "wis+2".parse().unwrap();
        assert_eq!(spec.kind[0], SavingThrowKind::Ability(Ability::Wisdom));
        assert!(matches!(spec.modifier, D20Modifier::Flat(2)));

        let spec: SavingThrowModifierProvider = "all-1".parse().unwrap();
        assert_eq!(spec.kind.len(), SavingThrowKind::iter().count());
        assert!(matches!(spec.modifier, D20Modifier::Flat(-1)));

        let spec: SavingThrowModifierProvider = "int critical_failure".parse().unwrap();
        assert_eq!(
            spec.kind[0],
            SavingThrowKind::Ability(Ability::Intelligence)
        );
        assert!(matches!(
            spec.modifier,
            D20Modifier::ForceOutcome(D20CheckOutcome::CriticalFailure)
        ));
    }

    #[test]
    fn ability_modifier_provider_parsing() {
        let spec: AbilityModifierProvider = "strength+2".parse().unwrap();
        assert_eq!(spec.ability, Ability::Strength);
        assert_eq!(spec.delta, 2);

        let spec: AbilityModifierProvider = "intelligence-1".parse().unwrap();
        assert_eq!(spec.ability, Ability::Intelligence);
        assert_eq!(spec.delta, -1);
    }

    #[test]
    fn damage_resistance_provider_parsing() {
        let spec: DamageResistanceProvider = "fire resistance".parse().unwrap();
        assert_eq!(spec.damage_type, DamageType::Fire);
        assert_eq!(spec.operation, MitigationOperation::Resistance);

        let spec: DamageResistanceProvider = "cold immunity".parse().unwrap();
        assert_eq!(spec.damage_type, DamageType::Cold);
        assert_eq!(spec.operation, MitigationOperation::Immunity);

        let spec: DamageResistanceProvider = "force -2".parse().unwrap();
        assert_eq!(spec.damage_type, DamageType::Force);
        assert_eq!(spec.operation, MitigationOperation::FlatReduction(2));
    }

    #[test]
    fn attack_source_definition_parsing() {
        let spec: AttackSourceDefinition = "melee_weapon".parse().unwrap();
        assert_eq!(spec.source, AttackSource::Weapon(WeaponKind::Melee));

        let spec: AttackSourceDefinition = "ranged_weapon".parse().unwrap();
        assert_eq!(spec.source, AttackSource::Weapon(WeaponKind::Ranged));

        let spec: AttackSourceDefinition = "unarmed".parse().unwrap();
        assert_eq!(spec.source, AttackSource::Weapon(WeaponKind::Unarmed));

        let spec: AttackSourceDefinition = "spell".parse().unwrap();
        assert_eq!(spec.source, AttackSource::Spell);
    }
}
