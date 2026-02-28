use std::{fmt::Display, marker::PhantomData, str::FromStr};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};
use uom::si::{
    f32::{Length, Time},
    length::{foot, meter},
    time::{hour, minute, second},
};

use crate::{
    components::actions::action::ActionContext,
    registry::serialize::{
        parser::{Evaluable, EvaluationError, IntExpression, Parser},
        variables::VariableMap,
    },
};

/// Dimension-specific behavior: how to turn a scalar + "unit" string into a uom quantity.
pub trait QuantityDimension: Clone + 'static {
    /// The uom quantity type for this dimension (e.g. `uom::si::f32::Length`).
    type Quantity;

    /// Parse the unit name and construct the quantity.
    fn make_quantity(value: f32, unit_name: &str) -> Result<Self::Quantity, String>;
}

/// Marker type for lengths
#[derive(Debug, Clone)]
pub struct LengthDim;

impl QuantityDimension for LengthDim {
    type Quantity = Length;

    fn make_quantity(value: f32, unit_name: &str) -> Result<Self::Quantity, String> {
        match unit_name.to_ascii_lowercase().as_str() {
            "m" | "meter" | "meters" => Ok(Length::new::<meter>(value)),
            "ft" | "foot" | "feet" => Ok(Length::new::<foot>(value)),
            other => Err(format!("Unknown length unit: '{}'", other)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TimeDim;

impl QuantityDimension for TimeDim {
    type Quantity = Time;

    fn make_quantity(value: f32, unit_name: &str) -> Result<Self::Quantity, String> {
        match unit_name.to_ascii_lowercase().as_str() {
            "s" | "sec" | "second" | "seconds" => Ok(Time::new::<second>(value)),
            "min" | "minute" | "minutes" => Ok(Time::new::<minute>(value)),
            "hr" | "hour" | "hours" => Ok(Time::new::<hour>(value)),
            other => Err(format!("Unknown time unit: '{}'", other)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct QuantityExpressionDefinition<D: QuantityDimension> {
    pub raw: String,
    #[serde(skip)]
    pub expression: IntExpression,
    #[serde(skip)]
    pub unit_name: String,
    #[serde(skip)]
    marker: PhantomData<D>,
}

impl<D: QuantityDimension> FromStr for QuantityExpressionDefinition<D> {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let trimmed = input.trim();

        // Split on last space: "<expression> <unit>"
        let (expr_str, unit_str) = trimmed
            .rsplit_once(' ')
            .ok_or_else(|| format!("Expected '<expr> <unit>', got '{}'", trimmed))?;

        let mut parser = Parser::new(expr_str.trim());
        let expression = parser.parse_int_expression()?;

        Ok(QuantityExpressionDefinition {
            raw: trimmed.to_string(),
            expression,
            unit_name: unit_str.trim().to_string(),
            marker: PhantomData,
        })
    }
}

impl<D: QuantityDimension> TryFrom<String> for QuantityExpressionDefinition<D> {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl<D: QuantityDimension> From<QuantityExpressionDefinition<D>> for String {
    fn from(spec: QuantityExpressionDefinition<D>) -> Self {
        spec.raw
    }
}

impl<D: QuantityDimension> Display for QuantityExpressionDefinition<D> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.raw)
    }
}

impl<D: QuantityDimension> Evaluable for QuantityExpressionDefinition<D> {
    type Output = D::Quantity;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
        variables: &VariableMap,
    ) -> Result<D::Quantity, EvaluationError> {
        let scalar = self
            .expression
            .evaluate(world, entity, action_context, variables)? as f32;

        let quantity = D::make_quantity(scalar, &self.unit_name)
            .map_err(|message| EvaluationError::UnknownVariable(message))?; // or add a new error kind

        Ok(quantity)
    }
}

impl<D: QuantityDimension> QuantityExpressionDefinition<D> {
    pub fn evaluate_without_variables(&self) -> Result<D::Quantity, EvaluationError> {
        let scalar = self.expression.evaluate_without_variables()? as f32;

        let quantity = D::make_quantity(scalar, &self.unit_name)
            .map_err(|message| EvaluationError::UnknownVariable(message))?; // or add a new error kind

        Ok(quantity)
    }
}

pub type LengthExpressionDefinition = QuantityExpressionDefinition<LengthDim>;
pub type TimeExpressionDefinition = QuantityExpressionDefinition<TimeDim>;

#[cfg(test)]
mod tests {
    use crate::{
        components::{
            class::ClassAndSubclass,
            id::{ClassId, ItemId, SpellId},
            spells::spellbook::{GrantedSpellSource, SpellSource},
        },
        registry::serialize::variables::PARSER_VARIABLES,
    };

    use super::*;

    #[test]
    fn length_expression_parsing() {
        let expr_str = "10 + spell_level ft";
        let expr: LengthExpressionDefinition = expr_str.parse().unwrap();

        assert_eq!(expr.raw, expr_str);
        assert_eq!(expr.unit_name, "ft");
    }

    #[test]
    fn length_expression_evaluation() {
        let expr_str = "10 + spell_level ft";
        let expr: LengthExpressionDefinition = expr_str.parse().unwrap();

        let mut world = World::new();
        let entity = world.spawn(());

        let action_context = ActionContext::spell(
            SpellId::new("nat20_core", "spell.test"),
            SpellSource::Class(ClassAndSubclass {
                class: ClassId::new("nat20_core", "class.wizard"),
                subclass: None,
            }),
            5,
        );

        let length = expr
            .evaluate(&world, entity, &action_context, &PARSER_VARIABLES)
            .unwrap();

        assert_eq!(length.get::<foot>(), 15.0);
    }

    #[test]
    fn time_expression_parsing() {
        let expr_str = "2 * spell_level minutes";
        let expr: QuantityExpressionDefinition<TimeDim> = expr_str.parse().unwrap();

        assert_eq!(expr.raw, expr_str);
        assert_eq!(expr.unit_name, "minutes");
    }

    #[test]
    fn time_expression_evaluation() {
        let expr_str = "2 * spell_level minutes";
        let expr: QuantityExpressionDefinition<TimeDim> = expr_str.parse().unwrap();

        let mut world = World::new();
        let entity = world.spawn(());

        let action_context = ActionContext::spell(
            SpellId::new("nat20_core", "spell.test"),
            SpellSource::Class(ClassAndSubclass {
                class: ClassId::new("nat20_core", "class.wizard"),
                subclass: None,
            }),
            3,
        );

        let time = expr
            .evaluate(&world, entity, &action_context, &PARSER_VARIABLES)
            .unwrap();

        assert_eq!(time.get::<minute>(), 6.0);
    }
}
