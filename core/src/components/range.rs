use std::fmt;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct Range<T> {
    pub min: T,
    pub max: T,
}

impl<T> Range<T>
where
    T: PartialOrd + PartialEq + Copy,
{
    pub fn new(min: T, max: T) -> Self {
        assert!(min <= max, "min must be less than or equal to max");
        Self { min, max }
    }

    pub fn single(value: T) -> Self {
        Self {
            min: value,
            max: value,
        }
    }

    pub fn is_single(&self) -> bool {
        self.min == self.max
    }

    pub fn contains<U>(&self, value: U) -> bool
    where
        U: Into<T> + Copy,
    {
        let value = value.into();
        value >= self.min && value <= self.max
    }

    pub fn limit_min<U>(&self, min: U) -> Self
    where
        U: Into<T> + Copy,
    {
        let min = min.into();
        let min = if min > self.min { min } else { self.min };
        Self { min, max: self.max }
    }

    pub fn limit_max<U>(&self, max: U) -> Self
    where
        U: Into<T> + Copy,
    {
        let max = max.into();
        let max = if max < self.max { max } else { self.max };
        Self { min: self.min, max }
    }

    pub fn convert<U>(&self) -> Range<U>
    where
        U: From<T> + Ord + PartialEq + Copy,
    {
        Range {
            min: U::from(self.min),
            max: U::from(self.max),
        }
    }

    pub fn add(&self, value: T) -> Self
    where
        T: std::ops::Add<Output = T>,
    {
        Self {
            min: self.min + value,
            max: self.max + value,
        }
    }

    pub fn scale(&self, factor: f32) -> Self
    where
        T: Into<f32> + From<f32> + Copy,
    {
        let min = self.min.into() * factor;
        let max = self.max.into() * factor;
        Self {
            min: T::from(min),
            max: T::from(max),
        }
    }
}

impl fmt::Display for Range<i32> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_single() {
            write!(f, "{}", self.min)?;
        } else if self.min > 0 && self.max > 0 {
            write!(f, "{}-{}", self.min, self.max)?;
        } else if self.min < 0 && self.max > 0 {
            write!(f, "({})-{}", self.min, self.max)?;
        } else if self.min < 0 && self.max < 0 {
            write!(f, "({})-({})", self.min, self.max)?;
        }

        Ok(())
    }
}
