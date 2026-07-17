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
}
