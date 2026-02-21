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
        actions::action::{ActionContext, DamageFunction, HealFunction},
        damage::{DamageRoll, DamageSource, DamageType},
        dice::{DiceSet, DiceSetRoll},
        modifier::{Modifiable, ModifierSet, ModifierSource},
    },
    registry::serialize::{
        parser::{Evaluable, Parser},
        variables::PARSER_VARIABLES,
    },
    systems,
};

static DAMAGE_DEFAULTS: LazyLock<HashMap<String, Arc<DamageFunction>>> = LazyLock::new(|| {
    HashMap::from([(
        "weapon_damage_roll".to_string(),
        Arc::new(
            |world: &World, entity: Entity, action_context: &ActionContext| {
                if let ActionContext::Weapon { slot } = action_context {
                    return systems::loadout::weapon_damage_roll(world, entity, slot);
                }
                panic!("Action context must be Weapon");
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

        if let Ok(dice_expression) = Parser::new(dice_part).parse_dice_expression() {
            let function = Arc::new(
                move |world: &World, entity: Entity, action_context: &ActionContext| {
                    let (num_dice, size, modifier) = dice_expression
                        .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                        .unwrap();
                    // TODO: Absolutely cooked way to construct dice set
                    let dice_set =
                        DiceSet::from_str(format!("{}d{}", num_dice, size).as_str()).unwrap();
                    let mut damage_roll = DamageRoll::new(
                        dice_set,
                        damage_type,
                        // TODO: Determine source properly
                        // Source is also included in AttackRoll, so maybe we only
                        // need one of them?
                        DamageSource::from(action_context),
                    );
                    damage_roll
                        .primary
                        .dice_roll
                        .add_modifier(ModifierSource::Base, modifier);
                    damage_roll
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
    pub function: Arc<HealFunction>,
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

        if let Ok(dice_expression) = Parser::new(s).parse_dice_expression() {
            let function = Arc::new(
                move |world: &World, entity: Entity, action_context: &ActionContext| {
                    let (num_dice, size, modifier) = dice_expression
                        .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                        .unwrap();

                    DiceSetRoll {
                        dice: DiceSet::from_str(format!("{}d{}", num_dice, size).as_str()).unwrap(),
                        modifiers: ModifierSet::from(ModifierSource::Base, modifier),
                    }
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
