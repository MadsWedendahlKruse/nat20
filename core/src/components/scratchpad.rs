use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

/// General purpose data structure for storing simple values keyed by a string,
/// see [`ScratchValue`] for the types of values that can be stored.
/// Primary use case is for storing per-entity state that doesn't have a dedicated
/// place to live otherwise, e.g. Relentless Rage's escalating CON save DC.
#[derive(Debug, Clone, Default)]
pub struct Scratchpad {
    values: BTreeMap<String, ScratchValue>,
}

impl Scratchpad {
    pub fn get<T: Scratchable>(&self, key: &str) -> Option<&T> {
        self.values.get(key).and_then(T::as_ref)
    }

    pub fn get_scratch_value(&self, key: &str) -> Option<&ScratchValue> {
        self.values.get(key)
    }

    pub fn get_mut<T: Scratchable>(&mut self, key: &str) -> Option<&mut T> {
        self.values.get_mut(key).and_then(T::as_mut)
    }

    pub fn insert<T: Scratchable>(&mut self, key: String, value: T) {
        self.values.insert(key, value.into_value());
    }

    pub fn remove(&mut self, key: &str) -> Option<ScratchValue> {
        self.values.remove(key)
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.values.contains_key(key)
    }
}

/// Types of values that can be stored in a [`Scratchpad`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ScratchValue {
    Bool(bool),
    I32(i32),
    F32(f32),
    String(String),
}

/// Trait for mapping between Rust types and [`ScratchValue`].
pub trait Scratchable: Sized {
    fn as_ref(v: &ScratchValue) -> Option<&Self>;
    fn as_mut(v: &mut ScratchValue) -> Option<&mut Self>;
    fn into_value(self) -> ScratchValue;
}

macro_rules! impl_scratchable {
    ($t:ty, $variant:ident) => {
        impl Scratchable for $t {
            fn as_ref(v: &ScratchValue) -> Option<&Self> {
                if let ScratchValue::$variant(v) = v {
                    Some(v)
                } else {
                    None
                }
            }

            fn as_mut(v: &mut ScratchValue) -> Option<&mut Self> {
                if let ScratchValue::$variant(v) = v {
                    Some(v)
                } else {
                    None
                }
            }

            fn into_value(self) -> ScratchValue {
                ScratchValue::$variant(self)
            }
        }
    };
}

impl_scratchable!(bool, Bool);
impl_scratchable!(i32, I32);
impl_scratchable!(f32, F32);
impl_scratchable!(String, String);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_get_remove() {
        let mut scratchpad = Scratchpad::default();
        scratchpad.insert("key".to_string(), 42i32);
        assert_eq!(scratchpad.get::<i32>("key"), Some(&42));
        assert_eq!(scratchpad.remove("key"), Some(ScratchValue::I32(42)));
        assert_eq!(scratchpad.get::<i32>("key"), None);
    }
}
