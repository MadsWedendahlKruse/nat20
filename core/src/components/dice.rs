use std::{
    fmt::{self, Display},
    str::FromStr,
};

use rand::Rng;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{components::range::Range, registry::serialize::schema::impl_string_schema};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum DieSize {
    D4 = 4,
    D6 = 6,
    D8 = 8,
    D10 = 10,
    D12 = 12,
    D20 = 20,
    D100 = 100,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct DiceSet {
    pub num_dice: u32,
    pub die_size: DieSize,
}

impl DiceSet {
    pub fn new(num_dice: u32, die_size: DieSize) -> Self {
        Self { num_dice, die_size }
    }

    pub fn is_zero(&self) -> bool {
        self.num_dice == 0
    }

    pub fn min_roll(&self) -> u32 {
        self.num_dice
    }

    pub fn max_roll(&self) -> u32 {
        self.num_dice * self.die_size as u32
    }

    pub fn roll_range(&self) -> Range<u32> {
        Range {
            min: self.min_roll(),
            max: self.max_roll(),
        }
    }

    pub fn roll(&self) -> DiceSetResult {
        let mut rng = rand::rng();
        let rolls: Vec<u32> = (0..self.num_dice)
            .map(|_| rng.random_range(1..=self.die_size as u32))
            .collect();
        let total = rolls.iter().sum();

        DiceSetResult {
            dice: *self,
            rolls,
            total,
        }
    }
}

impl Display for DiceSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}d{}", self.num_dice, self.die_size as u32)
    }
}

impl FromStr for DiceSet {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split('d').collect();
        if parts.len() != 2 {
            return Err("Invalid dice format".to_string());
        }
        let num_dice = parts[0].parse::<u32>().unwrap_or(1);
        let die_size = match parts[1] {
            "4" => DieSize::D4,
            "6" => DieSize::D6,
            "8" => DieSize::D8,
            "10" => DieSize::D10,
            "12" => DieSize::D12,
            "20" => DieSize::D20,
            "100" => DieSize::D100,
            _ => return Err(format!("Invalid die size: {}", parts[1])),
        };
        Ok(Self::new(num_dice, die_size))
    }
}

impl TryFrom<String> for DiceSet {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<DiceSet> for String {
    fn from(spec: DiceSet) -> Self {
        spec.to_string()
    }
}

impl_string_schema!(
    DiceSet,
    "DiceSet",
    "description": "A set of dice as `<count>d<size>`, e.g. `1d8`.",
    "examples": ["1d8", "2d6"]
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiceSetResult {
    dice: DiceSet,
    rolls: Vec<u32>,
    total: u32,
}

impl DiceSetResult {
    pub fn new(dice: DiceSet, rolls: Vec<u32>) -> Self {
        // TODO: Validate that rolls make sense for the given dice set
        let total = rolls.iter().sum();
        Self { dice, rolls, total }
    }

    pub fn dice(&self) -> DiceSet {
        self.dice
    }

    pub fn rolls(&self) -> &[u32] {
        &self.rolls
    }

    pub fn rolls_mut(&mut self) -> &mut Vec<u32> {
        &mut self.rolls
    }

    pub fn total(&self) -> u32 {
        self.total
    }

    pub fn reroll(&mut self) {
        *self = self.dice.roll();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dice_roll() {
        let dice = DiceSet::new(2, DieSize::D6);
        let result = dice.roll();
        assert_eq!(result.rolls.len(), 2);
        assert!(result.rolls.iter().all(|&r| r >= 1 && r <= 6));
        assert_eq!(result.total, result.rolls.iter().sum::<u32>());
    }

    #[test]
    fn parse_simple_dice_string() {
        let dice: DiceSet = "2d6".parse().unwrap();
        assert_eq!(dice.num_dice, 2);
        assert_eq!(dice.die_size, DieSize::D6);

        let dice: DiceSet = "1d20".parse().unwrap();
        assert_eq!(dice.num_dice, 1);
        assert_eq!(dice.die_size, DieSize::D20);

        let dice: DiceSet = "3d4".parse().unwrap();
        assert_eq!(dice.num_dice, 3);
        assert_eq!(dice.die_size, DieSize::D4);
    }

    #[test]
    fn parse_dice_string_with_missing_number_defaults_to_one() {
        let dice: DiceSet = "d8".parse().unwrap();
        assert_eq!(dice.num_dice, 1);
        assert_eq!(dice.die_size, DieSize::D8);
    }

    #[test]
    fn parse_dice_string_with_invalid_die_size_errors() {
        assert!(DiceSet::from_str("2d13").is_err());
    }

    #[test]
    fn parse_invalid_format_errors() {
        assert!(DiceSet::from_str("2x6").is_err());
    }

    #[test]
    fn parse_d100() {
        let dice: DiceSet = "1d100".parse().unwrap();
        assert_eq!(dice.num_dice, 1);
        assert_eq!(dice.die_size, DieSize::D100);
    }
}
