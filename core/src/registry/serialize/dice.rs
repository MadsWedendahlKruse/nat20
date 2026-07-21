use std::{
    collections::HashMap,
    fmt::{self, Display},
    str::FromStr,
    sync::{Arc, LazyLock},
};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{ActionContext, DamageFunction, HealingFunction},
        damage::{DamageRoll, DamageType},
        modifier::{ModifierMap, ModifierSource},
    },
    registry::serialize::{
        parser::{Evaluable, Parser},
        schema::{EXPRESSION_VARIABLES_DOC, impl_string_schema},
        variables::PARSER_VARIABLES,
    },
    systems,
};

impl_string_schema!(
    DamageEquation,
    "DamageEquation",
    "description": format!(
        "Damage roll: `<dice expression>;<damage type>` (e.g. `8d6;fire`, `(1 + spell_level)d6;cold`) \
         or `attack_context` to derive the damage roll from the used attack context. {}", EXPRESSION_VARIABLES_DOC),
    "examples": ["8d6;fire", "(1 + spell_level)d4;force", "melee_weapon_damage_roll"]
);

impl_string_schema!(
    HealEquation,
    "HealEquation",
    "description": format!("Healing roll as a dice expression. {}", EXPRESSION_VARIABLES_DOC),
    "examples": ["1d8 + spell_level", "2d4 + 2"]
);

static DAMAGE_DEFAULTS: LazyLock<HashMap<String, Arc<DamageFunction>>> = LazyLock::new(|| {
    HashMap::from([(
        "attack_context".to_string(),
        Arc::new(
            |world: &World, entity: Entity, action_context: &ActionContext| {
                systems::loadout::attack_damage_roll(world, entity, action_context)
            },
        ) as Arc<DamageFunction>,
    )])
});

#[derive(Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct DamageEquation {
    pub raw: String,
    pub function: Arc<DamageFunction>,
}

impl Display for DamageEquation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl FromStr for DamageEquation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(function) = DAMAGE_DEFAULTS.get(s) {
            return Ok(DamageEquation {
                raw: s.to_string(),
                function: function.clone(),
            });
        }

        // Example format: "(8 + spell_level - 3)d6;fire"

        let parts: Vec<&str> = s.split(';').collect();
        if parts.len() != 2 {
            return Err(format!("Invalid damage formula: {}", s));
        }
        let dice_part = parts[0];
        let damage_type: DamageType = serde_plain::from_str(parts[1]).unwrap();

        if let Ok(modifier_expression) = Parser::new(dice_part).parse_modifier_expression() {
            let function = Arc::new(
                move |world: &World, entity: Entity, action_context: &ActionContext| {
                    let modifier = modifier_expression
                        .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                        .unwrap();
                    DamageRoll::new(
                        ModifierMap::from(ModifierSource::Base, modifier),
                        damage_type,
                    )
                },
            );

            return Ok(DamageEquation {
                raw: s.to_string(),
                function,
            });
        }

        Err(format!("Unknown damage formula: {}", s))
    }
}

impl TryFrom<String> for DamageEquation {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<DamageEquation> for String {
    fn from(equation: DamageEquation) -> Self {
        equation.raw
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct HealEquation {
    pub raw: String,
    pub function: Arc<HealingFunction>,
}

impl Display for HealEquation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl FromStr for HealEquation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Example format: "(1d8 + spell_level)"

        if let Ok(modifier_expression) = Parser::new(s).parse_modifier_expression() {
            let function = Arc::new(
                move |world: &World, entity: Entity, action_context: &ActionContext| {
                    ModifierMap::from(
                        ModifierSource::Base,
                        modifier_expression
                            .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                            .unwrap(),
                    )
                },
            );

            return Ok(HealEquation {
                raw: s.to_string(),
                function,
            });
        }

        Err(format!("Unknown heal formula: {}", s))
    }
}

impl TryFrom<String> for HealEquation {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<HealEquation> for String {
    fn from(equation: HealEquation) -> Self {
        equation.raw
    }
}

impl fmt::Debug for HealEquation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HealEquation")
            .field("raw", &self.raw)
            .finish()
    }
}
