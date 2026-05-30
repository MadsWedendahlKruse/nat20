use std::str::FromStr;

use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::ActionContext,
        dice::{DiceSet, DiceSetRoll},
        id::{EntityIdentifier, ResourceId},
        modifier::{ModifierSet, ModifierSource},
    },
    registry::serialize::{
        d20::SavingThrowDefinition,
        parser::{DiceExpression, Evaluable, IntExpression, Parser},
        variables::PARSER_VARIABLES,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScriptEntity {
    pub id: u64,
}

impl From<Entity> for ScriptEntity {
    fn from(entity: Entity) -> Self {
        ScriptEntity {
            id: u64::from(entity.to_bits()),
        }
    }
}

impl From<EntityIdentifier> for ScriptEntity {
    fn from(identifier: EntityIdentifier) -> Self {
        ScriptEntity::from(identifier.id())
    }
}

impl Into<Entity> for ScriptEntity {
    fn into(self) -> Entity {
        Entity::from_bits(self.id).unwrap()
    }
}

impl From<u64> for ScriptEntity {
    fn from(id: u64) -> Self {
        ScriptEntity { id }
    }
}

#[derive(Clone)]
pub enum ScriptEntityRole {
    Actor,
    Reactor,
    Target,
}

impl FromStr for ScriptEntityRole {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "actor" => Ok(ScriptEntityRole::Actor),
            "reactor" => Ok(ScriptEntityRole::Reactor),
            "target" => Ok(ScriptEntityRole::Target),
            _ => Err(format!("Unknown ScriptEntityRole: {}", s)),
        }
    }
}

#[derive(Clone)]
pub enum ScriptEventRef {
    TriggerEvent,
}

#[derive(Clone)]
pub struct ScriptSavingThrow {
    pub entity: ScriptEntityRole,
    pub saving_throw: SavingThrowDefinition,
}

#[derive(Clone)]
pub enum ScriptDiceRollBonus {
    Flat(IntExpression),
    Dice(DiceExpression),
}

impl ScriptDiceRollBonus {
    pub fn evaluate(&self, world: &World, entity: Entity, action_context: &ActionContext) -> i32 {
        match self {
            ScriptDiceRollBonus::Flat(expr) => expr
                .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                .unwrap(),
            ScriptDiceRollBonus::Dice(expr) => {
                let (num_dice, size, modifier) = expr
                    .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                    .unwrap();

                DiceSetRoll {
                    dice: DiceSet::from_str(format!("{}d{}", num_dice, size).as_str()).unwrap(),
                    modifiers: ModifierSet::from(ModifierSource::Base, modifier),
                }
                .roll()
                .subtotal
            }
        }
    }
}

impl FromStr for ScriptDiceRollBonus {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(flat) = Parser::new(s).parse_int_expression() {
            Ok(ScriptDiceRollBonus::Flat(flat))
        } else if let Ok(expr) = Parser::new(s).parse_dice_expression() {
            Ok(ScriptDiceRollBonus::Dice(expr))
        } else {
            Err(format!("Invalid ScriptD20Bonus expression: {}", s))
        }
    }
}

// TODO: Can this be replaced with GameState methods inside the scripts?
#[derive(Clone)]
pub enum ScriptReactionPlan {
    None,
    Sequence(Vec<ScriptReactionPlan>),
    RequireSavingThrow {
        target: ScriptEntityRole,
        dc: ScriptSavingThrow,
        on_success: Box<ScriptReactionPlan>,
        on_failure: Box<ScriptReactionPlan>,
    },
    CancelEvent {
        event: ScriptEventRef,
        resources_to_refund: Vec<ResourceId>,
    },
}

#[derive(Clone)]
pub enum ScriptReactionBodyResult {
    Plan(ScriptReactionPlan),
}

impl ScriptReactionBodyResult {
    pub fn none() -> Self {
        Self::Plan(ScriptReactionPlan::None)
    }
}
