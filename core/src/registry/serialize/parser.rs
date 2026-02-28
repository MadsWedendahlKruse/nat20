use std::{collections::HashMap, sync::Arc};

use hecs::{Entity, World};

use crate::{
    components::actions::action::ActionContext, registry::serialize::variables::VariableFunction,
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

#[derive(Debug)]
pub enum EvaluationError {
    UnknownVariable(String),
    DivisionByZero,
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

impl IntExpression {
    pub fn evaluate_without_variables(&self) -> Result<i32, EvaluationError> {
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
    /// Optional flat modifier applied after rolling the dice (e.g. "1d4 + 1").
    pub modifier_expression: Option<IntExpression>,
}

impl Evaluable for DiceExpression {
    type Output = (i32, i32, i32);

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
        variables: &HashMap<
            String,
            Arc<dyn Fn(&World, Entity, &ActionContext) -> i32 + Send + Sync>,
        >,
    ) -> Result<(i32, i32, i32), EvaluationError> {
        let count = self
            .count_expression
            .evaluate(world, entity, action_context, variables)?;
        let size = self
            .size_expression
            .evaluate(world, entity, action_context, variables)?;
        let modifier = if let Some(mod_expr) = &self.modifier_expression {
            mod_expr.evaluate(world, entity, action_context, variables)?
        } else {
            0
        };

        Ok((count, size, modifier))
    }
}

impl DiceExpression {
    pub fn evaluate_without_variables(&self) -> Result<(i32, i32, i32), EvaluationError> {
        let count = self.count_expression.evaluate_without_variables()?;
        let size = self.size_expression.evaluate_without_variables()?;
        let modifier = if let Some(mod_expr) = &self.modifier_expression {
            mod_expr.evaluate_without_variables()?
        } else {
            0
        };

        Ok((count, size, modifier))
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

    /// dice_expr := dice_core (('+' | '-') expression)?
    /// dice_core := dice_count 'd' dice_size
    pub fn parse_dice_expression(&mut self) -> Result<DiceExpression, String> {
        // 1) Left side: strict dice count
        let count_expression = self.parse_dice_count()?;
        self.consume_whitespace();

        // 2) The 'd'
        self.expect_char('d')?;

        // 3) Right side: size of the dice
        let size_expression = self.parse_dice_size()?;
        self.consume_whitespace();

        // 4) Optional flat modifier, e.g. "+ 1" in "1d4 + 1" or variables like
        // "1d10 + class.fighter.level"
        let modifier_expression = match self.peek_char() {
            Some('+') | Some('-') => {
                let operator = self.next_char().unwrap(); // consume '+' or '-'
                // Reuse full expression parsing (with * and / precedence)
                let mut expression = self.parse_expression()?;

                // If it is a minus, wrap the expression in a Negate so that
                // "1d4 - 1" becomes "1d4 + (-1)" in terms of evaluation.
                if operator == '-' {
                    expression = IntExpression::Negate(Box::new(expression));
                }

                Some(expression)
            }
            _ => None,
        };

        // 5) Ensure no trailing garbage
        self.consume_whitespace();
        if !self.is_at_end() {
            return Err("Unexpected characters after dice expression".to_string());
        }

        Ok(DiceExpression {
            count_expression,
            size_expression,
            modifier_expression,
        })
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
}

#[cfg(test)]
mod tests {
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
        let dice_expression = parser.parse_dice_expression().expect("failed to parse");

        expect_literal(&dice_expression.count_expression, 2);
        expect_literal(&dice_expression.size_expression, 8);

        match dice_expression.modifier_expression {
            Some(mod_expr) => expect_literal(&mod_expr, 3),
            None => panic!("Expected modifier expression"),
        }
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
    fn evaluates_dice_equation_with_variables() {
        let mut parser = Parser::new("(spell_level + 2) d (caster_level * 2) + character_level");
        let dice_expression = parser.parse_dice_expression().expect("failed to parse");

        let variables = variables();
        let mut world = World::new();
        let entity = world.spawn(());
        let action_context = ActionContext::default();

        let (count, size, modifier) = dice_expression
            .evaluate(&world, entity, &action_context, &variables)
            .expect("failed to evaluate");

        assert_eq!(count, 5); // spell_level (3) + 2
        assert_eq!(size, 10); // caster_level (5) * 2
        assert_eq!(modifier, 7); // character_level (7)
    }
}
