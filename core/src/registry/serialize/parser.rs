use std::{collections::HashMap, str::FromStr, sync::Arc};

use hecs::{Entity, World};
use strum::Display;

use crate::{
    components::{actions::action::ActionContext, dice::DiceSet, modifier::ModifierKind},
    registry::serialize::variables::VariableFunction,
};

#[derive(Debug, Clone)]
pub enum IntExpression {
    Literal(i32),
    Variable(String),
    Add(Box<IntExpression>, Box<IntExpression>),
    Subtract(Box<IntExpression>, Box<IntExpression>),
    Multiply(Box<IntExpression>, Box<IntExpression>),
    Divide(Box<IntExpression>, Box<IntExpression>),
    Negate(Box<IntExpression>),
}

#[derive(Debug, Clone, Display)]
pub enum EvaluationError {
    UnknownVariable(String),
    DivisionByZero,
    Other(String),
}

pub trait Evaluable {
    type Output;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
        variables: &HashMap<String, Arc<VariableFunction>>,
    ) -> Result<Self::Output, EvaluationError>;
}

pub trait EvaluableWithoutVariables {
    type Output;

    fn evaluate_without_variables(&self) -> Result<Self::Output, EvaluationError>;
}

impl Evaluable for IntExpression {
    type Output = i32;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
        variables: &HashMap<String, Arc<VariableFunction>>,
    ) -> Result<i32, EvaluationError> {
        match self {
            IntExpression::Literal(value) => Ok(*value),
            IntExpression::Variable(name) => {
                let variable_function = variables
                    .get(name.as_str())
                    .ok_or_else(|| EvaluationError::UnknownVariable(name.clone()))?;

                Ok(variable_function(world, entity, action_context))
            }
            IntExpression::Add(left, right) => {
                Ok(left.evaluate(world, entity, action_context, variables)?
                    + right.evaluate(world, entity, action_context, variables)?)
            }
            IntExpression::Subtract(left, right) => {
                Ok(left.evaluate(world, entity, action_context, variables)?
                    - right.evaluate(world, entity, action_context, variables)?)
            }
            IntExpression::Multiply(left, right) => {
                Ok(left.evaluate(world, entity, action_context, variables)?
                    * right.evaluate(world, entity, action_context, variables)?)
            }
            IntExpression::Divide(left, right) => {
                let denominator = right.evaluate(world, entity, action_context, variables)?;
                if denominator == 0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                Ok(left.evaluate(world, entity, action_context, variables)? / denominator)
            }
            IntExpression::Negate(inner) => {
                Ok(-inner.evaluate(world, entity, action_context, variables)?)
            }
        }
    }
}

impl EvaluableWithoutVariables for IntExpression {
    type Output = i32;

    fn evaluate_without_variables(&self) -> Result<i32, EvaluationError> {
        match self {
            IntExpression::Literal(value) => Ok(*value),
            IntExpression::Variable(name) => Err(EvaluationError::UnknownVariable(name.clone())),
            IntExpression::Add(left, right) => {
                Ok(left.evaluate_without_variables()? + right.evaluate_without_variables()?)
            }
            IntExpression::Subtract(left, right) => {
                Ok(left.evaluate_without_variables()? - right.evaluate_without_variables()?)
            }
            IntExpression::Multiply(left, right) => {
                Ok(left.evaluate_without_variables()? * right.evaluate_without_variables()?)
            }
            IntExpression::Divide(left, right) => {
                let denominator = right.evaluate_without_variables()?;
                if denominator == 0 {
                    return Err(EvaluationError::DivisionByZero);
                }
                Ok(left.evaluate_without_variables()? / denominator)
            }
            IntExpression::Negate(inner) => Ok(-inner.evaluate_without_variables()?),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DiceExpression {
    pub count_expression: IntExpression,
    pub size_expression: IntExpression,
}

impl Evaluable for DiceExpression {
    type Output = DiceSet;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
        variables: &HashMap<
            String,
            Arc<dyn Fn(&World, Entity, &ActionContext) -> i32 + Send + Sync>,
        >,
    ) -> Result<DiceSet, EvaluationError> {
        let count = self
            .count_expression
            .evaluate(world, entity, action_context, variables)?;
        let size = self
            .size_expression
            .evaluate(world, entity, action_context, variables)?;

        Ok(DiceSet::from_str(format!("{}d{}", count, size).as_str()).unwrap())
    }
}

impl EvaluableWithoutVariables for DiceExpression {
    type Output = DiceSet;

    fn evaluate_without_variables(&self) -> Result<DiceSet, EvaluationError> {
        let count = self.count_expression.evaluate_without_variables()?;
        let size = self.size_expression.evaluate_without_variables()?;

        Ok(DiceSet::from_str(format!("{}d{}", count, size).as_str()).unwrap())
    }
}

#[derive(Debug, Clone)]
pub enum ModifierExpression {
    Int(IntExpression),
    Dice(DiceExpression),
    Composite(Vec<ModifierExpression>),
}

impl Evaluable for ModifierExpression {
    type Output = ModifierKind;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
        variables: &HashMap<String, Arc<VariableFunction>>,
    ) -> Result<ModifierKind, EvaluationError> {
        match self {
            ModifierExpression::Int(int_expr) => {
                let value = int_expr.evaluate(world, entity, action_context, variables)?;
                Ok(ModifierKind::Flat(value))
            }
            ModifierExpression::Dice(dice_expr) => {
                let dice_set = dice_expr.evaluate(world, entity, action_context, variables)?;
                Ok(ModifierKind::Dice(dice_set))
            }
            ModifierExpression::Composite(expressions) => {
                let mut modifier_source = ModifierKind::Composite(Vec::new());
                for expr in expressions {
                    let evaluated = expr.evaluate(world, entity, action_context, variables)?;
                    if let ModifierKind::Composite(ref mut vec) = modifier_source {
                        vec.push(evaluated);
                    }
                }
                Ok(modifier_source)
            }
        }
    }
}

impl EvaluableWithoutVariables for ModifierExpression {
    type Output = ModifierKind;

    fn evaluate_without_variables(&self) -> Result<ModifierKind, EvaluationError> {
        match self {
            ModifierExpression::Int(int_expr) => {
                let value = int_expr.evaluate_without_variables()?;
                Ok(ModifierKind::Flat(value))
            }
            ModifierExpression::Dice(dice_expr) => {
                let dice_set = dice_expr.evaluate_without_variables()?;
                Ok(ModifierKind::Dice(dice_set))
            }
            ModifierExpression::Composite(expressions) => {
                let mut modifier_source = ModifierKind::Composite(Vec::new());
                for expr in expressions {
                    let evaluated = expr.evaluate_without_variables()?;
                    if let ModifierKind::Composite(ref mut vec) = modifier_source {
                        vec.push(evaluated);
                    }
                }
                Ok(modifier_source)
            }
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser { input, position: 0 }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.position..].chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        if let Some(character) = self.peek_char() {
            self.position += character.len_utf8();
            Some(character)
        } else {
            None
        }
    }

    fn consume_whitespace(&mut self) {
        while matches!(self.peek_char(), Some(character) if character.is_whitespace()) {
            self.next_char();
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), String> {
        self.consume_whitespace();
        match self.next_char() {
            Some(character) if character == expected => Ok(()),
            Some(character) => Err(format!("Expected '{}', found '{}'", expected, character)),
            None => Err(format!("Expected '{}', found end of input", expected)),
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn parse_identifier(&mut self) -> Result<String, String> {
        self.consume_whitespace();
        let mut identifier = String::new();

        match self.peek_char() {
            Some(character)
                if character.is_ascii_alphabetic() || character == '_' || character == '.' =>
            {
                identifier.push(self.next_char().unwrap());
            }
            _ => return Err("Expected identifier".to_string()),
        }

        while let Some(character) = self.peek_char() {
            if character.is_ascii_alphanumeric() || character == '_' || character == '.' {
                identifier.push(self.next_char().unwrap());
            } else {
                break;
            }
        }

        Ok(identifier)
    }

    fn parse_integer(&mut self) -> Result<i32, String> {
        self.consume_whitespace();
        let mut number_string = String::new();

        while let Some(character) = self.peek_char() {
            if character.is_ascii_digit() {
                number_string.push(self.next_char().unwrap());
            } else {
                break;
            }
        }

        if number_string.is_empty() {
            return Err("Expected integer literal".to_string());
        }

        number_string
            .parse::<i32>()
            .map_err(|error| format!("Invalid integer literal: {}", error))
    }

    fn parse_factor(&mut self) -> Result<IntExpression, String> {
        self.consume_whitespace();

        if let Some(character) = self.peek_char() {
            return match character {
                '(' => {
                    self.next_char(); // consume '('
                    let expression = self.parse_expression()?; // still fine: expression handles precedence
                    self.consume_whitespace();
                    self.expect_char(')')?;
                    Ok(expression)
                }
                '-' => {
                    self.next_char(); // consume '-'
                    let inner_expression = self.parse_factor()?;
                    Ok(IntExpression::Negate(Box::new(inner_expression)))
                }
                character if character.is_ascii_digit() => {
                    let value = self.parse_integer()?;
                    Ok(IntExpression::Literal(value))
                }
                character if character.is_ascii_alphabetic() || character == '_' => {
                    let name = self.parse_identifier()?;
                    Ok(IntExpression::Variable(name))
                }
                _ => Err(format!("Unexpected character in factor: '{}'", character)),
            };
        }

        Err("Unexpected end of input while parsing factor".to_string())
    }

    /// term := factor (('*' | '/') factor)*
    fn parse_term(&mut self) -> Result<IntExpression, String> {
        let mut expression = self.parse_factor()?;

        loop {
            self.consume_whitespace();
            let operator = match self.peek_char() {
                Some('*') => {
                    self.next_char();
                    '*'
                }
                Some('/') => {
                    self.next_char();
                    '/'
                }
                _ => break,
            };

            let right_expression = self.parse_factor()?;
            expression = match operator {
                '*' => IntExpression::Multiply(Box::new(expression), Box::new(right_expression)),
                '/' => IntExpression::Divide(Box::new(expression), Box::new(right_expression)),
                _ => unreachable!(),
            };
        }

        Ok(expression)
    }

    /// expression := term (('+' | '-') term)*
    fn parse_expression(&mut self) -> Result<IntExpression, String> {
        let mut expression = self.parse_term()?;

        loop {
            self.consume_whitespace();
            let operator = match self.peek_char() {
                Some('+') => {
                    self.next_char();
                    '+'
                }
                Some('-') => {
                    self.next_char();
                    '-'
                }
                _ => break,
            };

            let right_expression = self.parse_term()?;
            expression = match operator {
                '+' => IntExpression::Add(Box::new(expression), Box::new(right_expression)),
                '-' => IntExpression::Subtract(Box::new(expression), Box::new(right_expression)),
                _ => unreachable!(),
            };
        }

        Ok(expression)
    }

    /// Simple count for dice: either a single literal / variable, or a parenthesized expression.
    fn parse_dice_count(&mut self) -> Result<IntExpression, String> {
        self.consume_whitespace();

        match self.peek_char() {
            Some('(') => {
                // Parenthesized full expression, e.g. "(8 + spell_level - 3)"
                self.next_char(); // consume '('
                let expression = self.parse_expression()?;
                self.consume_whitespace();
                self.expect_char(')')?;
                Ok(expression)
            }
            Some(character) if character.is_ascii_digit() => {
                // Single integer literal
                let value = self.parse_integer()?;
                Ok(IntExpression::Literal(value))
            }
            Some(character)
                if character.is_ascii_alphabetic() || character == '_' || character == '.' =>
            {
                // Single variable name
                let name = self.parse_identifier()?;
                Ok(IntExpression::Variable(name))
            }
            Some(character) => Err(format!(
                "Invalid start of dice count: '{}'. Use a literal, variable, or '(...)'.",
                character
            )),
            None => Err("Unexpected end of input while parsing dice count".to_string()),
        }
    }

    fn parse_dice_size(&mut self) -> Result<IntExpression, String> {
        // TODO: Just do the same as parse_dice_count for now
        self.parse_dice_count()
    }

    /// dice_core := dice_count 'd' dice_size
    fn parse_dice_core(&mut self) -> Result<DiceExpression, String> {
        let count_expression = self.parse_dice_count()?;
        self.expect_char('d')?;
        let size_expression = self.parse_dice_size()?;

        Ok(DiceExpression {
            count_expression,
            size_expression,
        })
    }

    /// dice_expr := dice_core
    pub fn parse_dice_expression(&mut self) -> Result<DiceExpression, String> {
        let dice_expression = self.parse_dice_core()?;

        self.consume_whitespace();
        if !self.is_at_end() {
            return Err("Unexpected characters after dice expression".to_string());
        }

        Ok(dice_expression)
    }

    /// Parses a standalone integer expression and ensures there is no trailing junk.
    pub fn parse_int_expression(&mut self) -> Result<IntExpression, String> {
        let expression = self.parse_expression()?;
        self.consume_whitespace();
        if !self.is_at_end() {
            return Err("Unexpected characters after integer expression".to_string());
        }
        Ok(expression)
    }

    /// A single term of a modifier expression: either a dice core or an
    /// integer term ('*' and '/' bind tighter than the '+'/'-' between terms).
    fn parse_modifier_term(&mut self, negated: bool) -> Result<ModifierExpression, String> {
        let start = self.position;
        if let Ok(dice_expression) = self.parse_dice_core() {
            if negated {
                return Err(
                    "Subtracting dice is not supported in a modifier expression".to_string()
                );
            }
            return Ok(ModifierExpression::Dice(dice_expression));
        }

        self.position = start;
        let mut expression = self.parse_term()?;
        if negated {
            expression = IntExpression::Negate(Box::new(expression));
        }
        Ok(ModifierExpression::Int(expression))
    }

    /// modifier_expr := modifier_term (('+' | '-') modifier_term)*
    /// modifier_term := dice_core | term
    pub fn parse_modifier_expression(&mut self) -> Result<ModifierExpression, String> {
        let mut terms = vec![self.parse_modifier_term(false)?];

        loop {
            self.consume_whitespace();
            let negated = match self.peek_char() {
                Some('+') => false,
                Some('-') => true,
                _ => break,
            };
            self.next_char(); // consume '+' or '-'
            let term = self.parse_modifier_term(negated)?;

            // Fold adjacent int terms so pure-integer input stays a single Int
            // expression instead of a Composite of flat parts.
            match (terms.last_mut(), term) {
                (Some(ModifierExpression::Int(last)), ModifierExpression::Int(new_expression)) => {
                    let left = std::mem::replace(last, IntExpression::Literal(0));
                    *last = IntExpression::Add(Box::new(left), Box::new(new_expression));
                }
                (_, term) => terms.push(term),
            }
        }

        self.consume_whitespace();
        if !self.is_at_end() {
            return Err("Unexpected characters after modifier expression".to_string());
        }

        if terms.len() == 1 {
            Ok(terms.pop().unwrap())
        } else {
            Ok(ModifierExpression::Composite(terms))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::components::dice::DieSize;

    use super::*;

    fn expect_literal(expr: &IntExpression, expected: i32) {
        match expr {
            IntExpression::Literal(value) => assert_eq!(*value, expected),
            other => panic!("Expected literal {}, found {:?}", expected, other),
        }
    }

    fn expect_variable(expr: &IntExpression, expected: &str) {
        match expr {
            IntExpression::Variable(name) => assert_eq!(name, expected),
            other => panic!("Expected variable {}, found {:?}", expected, other),
        }
    }

    #[test]
    fn parses_simple_literal_dice_equation() {
        let mut parser = Parser::new("3d6");
        let dice_expression = parser.parse_dice_expression().expect("failed to parse");

        expect_literal(&dice_expression.count_expression, 3);
        expect_literal(&dice_expression.size_expression, 6);
    }

    #[test]
    fn respects_operator_precedence_in_count_expression() {
        let mut parser = Parser::new("(2 + 3 * 4)d6");
        let dice_expression = parser.parse_dice_expression().expect("failed to parse");

        match &dice_expression.count_expression {
            IntExpression::Add(left, right) => {
                expect_literal(left.as_ref(), 2);
                match right.as_ref() {
                    IntExpression::Multiply(mult_left, mult_right) => {
                        expect_literal(mult_left.as_ref(), 3);
                        expect_literal(mult_right.as_ref(), 4);
                    }
                    other => panic!("Expected multiplication, found {:?}", other),
                }
            }
            other => panic!("Expected addition, found {:?}", other),
        }

        expect_literal(&dice_expression.size_expression, 6);
    }

    #[test]
    fn parses_parenthesized_expressions() {
        let mut parser = Parser::new("(1 + 2) d (3 + 4)");
        let dice_expression = parser.parse_dice_expression().expect("failed to parse");

        match dice_expression.count_expression {
            IntExpression::Add(left, right) => {
                expect_literal(left.as_ref(), 1);
                expect_literal(right.as_ref(), 2);
            }
            other => panic!("Expected addition in count, found {:?}", other),
        }

        match dice_expression.size_expression {
            IntExpression::Add(left, right) => {
                expect_literal(left.as_ref(), 3);
                expect_literal(right.as_ref(), 4);
            }
            other => panic!("Expected addition in size, found {:?}", other),
        }
    }

    #[test]
    fn parses_dice_equation_with_modifier() {
        let mut parser = Parser::new("2d8 + 3");
        let modifier_expression = parser.parse_modifier_expression().expect("failed to parse");

        let terms = match modifier_expression {
            ModifierExpression::Composite(terms) => terms,
            other => panic!("Expected composite, found {:?}", other),
        };
        assert_eq!(terms.len(), 2);

        match &terms[0] {
            ModifierExpression::Dice(dice_expression) => {
                expect_literal(&dice_expression.count_expression, 2);
                expect_literal(&dice_expression.size_expression, 8);
            }
            other => panic!("Expected dice term, found {:?}", other),
        }

        match &terms[1] {
            ModifierExpression::Int(int_expression) => expect_literal(int_expression, 3),
            other => panic!("Expected int term, found {:?}", other),
        }
    }

    #[test]
    fn parses_single_term_modifier_expressions() {
        match Parser::new("1d6").parse_modifier_expression() {
            Ok(ModifierExpression::Dice(_)) => {}
            other => panic!("Expected dice expression, found {:?}", other),
        }

        match Parser::new("spell_level + 2").parse_modifier_expression() {
            Ok(ModifierExpression::Int(_)) => {}
            other => panic!("Expected int expression, found {:?}", other),
        }
    }

    #[test]
    fn parses_negated_flat_modifier_term() {
        let mut parser = Parser::new("1d4 - 1");
        let modifier_expression = parser.parse_modifier_expression().expect("failed to parse");

        let terms = match modifier_expression {
            ModifierExpression::Composite(terms) => terms,
            other => panic!("Expected composite, found {:?}", other),
        };

        match &terms[1] {
            ModifierExpression::Int(IntExpression::Negate(inner)) => {
                expect_literal(inner.as_ref(), 1)
            }
            other => panic!("Expected negated int term, found {:?}", other),
        }
    }

    #[test]
    fn subtracting_dice_error() {
        let result = Parser::new("1d4 - 1d6").parse_modifier_expression();
        assert!(result.is_err());
    }

    #[test]
    fn missing_parenthesis_error() {
        let mut parser = Parser::new("3 * 4 d6");
        let result = parser.parse_dice_expression();

        assert!(result.is_err());
    }

    #[test]
    fn parses_negation() {
        let mut parser = Parser::new("(8 - 2)d6");
        let dice_expression = parser.parse_dice_expression().expect("failed to parse");
        match &dice_expression.count_expression {
            IntExpression::Subtract(left, right) => {
                expect_literal(left.as_ref(), 8);
                expect_literal(right.as_ref(), 2);
            }
            other => panic!("Expected subtraction, found {:?}", other),
        }
        expect_literal(&dice_expression.size_expression, 6);
    }

    #[test]
    fn parses_variable_identifiers() {
        let mut parser = Parser::new("spell_level d spell_die");
        let dice_expression = parser.parse_dice_expression().expect("failed to parse");

        expect_variable(&dice_expression.count_expression, "spell_level");
        expect_variable(&dice_expression.size_expression, "spell_die");
    }

    #[test]
    fn trailing_characters_error() {
        let mut parser = Parser::new("1d6 + 1 lorem ipsum");
        let result = parser.parse_dice_expression();
        assert!(result.is_err());
    }

    fn variables()
    -> HashMap<String, Arc<dyn Fn(&World, Entity, &ActionContext) -> i32 + Send + Sync>> {
        let mut vars: HashMap<
            String,
            Arc<dyn Fn(&World, Entity, &ActionContext) -> i32 + Send + Sync>,
        > = HashMap::new();
        vars.insert("spell_level".to_string(), Arc::new(|_, _, _| 3));
        vars.insert("caster_level".to_string(), Arc::new(|_, _, _| 5));
        vars.insert("character_level".to_string(), Arc::new(|_, _, _| 7));
        vars
    }

    #[test]
    fn evaluates_modifier_expression_with_variables() {
        let mut parser = Parser::new("(spell_level + 2) d (caster_level * 2) + character_level");
        let modifier_expression = parser.parse_modifier_expression().expect("failed to parse");

        let variables = variables();
        let mut world = World::new();
        let entity = world.spawn(());
        let action_context = ActionContext::default();

        let modifier = modifier_expression
            .evaluate(&world, entity, &action_context, &variables)
            .expect("failed to evaluate");

        assert_eq!(
            modifier,
            ModifierKind::Composite(vec![
                ModifierKind::Dice(DiceSet::new(5, DieSize::D10)), // (spell_level (3) + 2) d (caster_level (5) * 2)
                ModifierKind::Flat(7),                             // character_level (7)
            ])
        );
    }
}
