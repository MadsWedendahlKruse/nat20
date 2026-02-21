// TODO: Consider a different name?

use std::collections::HashMap;

use tracing::warn;
use uom::si::{f32::Length, length::meter};

use crate::components::modifier::ModifierSource;

// Internally, speed is stored in meters (per turn).
#[derive(Debug, Clone)]
pub struct Speed {
    flat: HashMap<ModifierSource, f32>,
    multipliers: HashMap<ModifierSource, f32>,
    moved_this_turn: f32,

    free_movement_multipliers: HashMap<ModifierSource, f32>,
    free_movement_remaining: f32,
}

impl Speed {
    // Construct a new Speed with a base value from any length unit
    pub fn new(base: Length) -> Self {
        let mut flat = HashMap::new();
        flat.insert(ModifierSource::Base, base.get::<meter>());
        Self {
            flat,
            multipliers: HashMap::new(),
            moved_this_turn: 0.0,
            free_movement_multipliers: HashMap::new(),
            free_movement_remaining: 0.0,
        }
    }

    pub fn add_flat_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<f32>,
    {
        self.flat.insert(source, value.into());
    }

    pub fn remove_flat_modifier(&mut self, source: &ModifierSource) {
        self.flat.remove(source);
    }

    pub fn add_multiplier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<f32>,
    {
        self.multipliers.insert(source, value.into());
    }

    pub fn remove_multiplier(&mut self, source: &ModifierSource) {
        self.multipliers.remove(source);
    }

    pub fn total_speed(&self) -> Length {
        let base_speed: f32 = self.flat.values().sum();

        let total_multiplier: f32 = if self.multipliers.is_empty() {
            1.0
        } else {
            self.multipliers.values().product()
        };

        Length::new::<meter>(base_speed * total_multiplier)
    }

    pub fn moved_this_turn(&self) -> Length {
        Length::new::<meter>(self.moved_this_turn)
    }

    pub fn record_movement(&mut self, distance: Length) {
        let distance = distance.get::<meter>();

        if distance > self.remaining_movement().get::<meter>() {
            self.moved_this_turn = self.total_speed().get::<meter>();
        } else {
            self.moved_this_turn += distance;
        }

        if distance > self.free_movement_remaining {
            self.free_movement_remaining = 0.0;
        } else {
            self.free_movement_remaining -= distance;
        }
    }

    /// Should be called at the start (or end?) of each turn
    pub fn reset(&mut self) {
        self.moved_this_turn = 0.0;
        self.free_movement_remaining = self.max_free_movement();
    }

    pub fn remaining_movement(&self) -> Length {
        let total_speed = self.total_speed().get::<meter>();
        let remaining = (total_speed - self.moved_this_turn).max(0.0);
        Length::new::<meter>(remaining)
    }

    pub fn can_move(&self) -> bool {
        self.remaining_movement().get::<meter>() > 0.0
    }

    pub fn free_movement_remaining(&self) -> Length {
        Length::new::<meter>(self.free_movement_remaining)
    }

    fn max_free_movement(&self) -> f32 {
        if self.free_movement_multipliers.is_empty() {
            0.0
        } else {
            let free_movement_multiplier = self
                .free_movement_multipliers
                .values()
                .sum::<f32>()
                .min(1.0);
            free_movement_multiplier * self.total_speed().get::<meter>()
        }
    }

    pub fn add_free_movement_multiplier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<f32>,
    {
        let value = value.into();

        if value <= 0.0 || value > 1.0 {
            warn!(
                "Free movement multipliers should be between 0 and 1. Value of {} from source {:?} is invalid and will be ignored.",
                value, source
            );
            return;
        }
        self.free_movement_remaining += self.total_speed().get::<meter>() * value;
        self.free_movement_multipliers.insert(source, value.into());
    }

    pub fn remove_free_movement_multiplier(&mut self, source: &ModifierSource) {
        if let Some(value) = self.free_movement_multipliers.remove(source) {
            self.free_movement_remaining -= self.total_speed().get::<meter>() * value;
        }
    }

    pub fn flat_bonuses(&self) -> &HashMap<ModifierSource, f32> {
        &self.flat
    }

    pub fn multipliers(&self) -> &HashMap<ModifierSource, f32> {
        &self.multipliers
    }

    pub fn free_movement_multipliers(&self) -> &HashMap<ModifierSource, f32> {
        &self.free_movement_multipliers
    }
}

impl Default for Speed {
    fn default() -> Self {
        Self::new(Length::new::<meter>(10.0))
    }
}

#[cfg(test)]
mod tests {
    use crate::components::id::{EffectId, ItemId};

    use super::*;

    #[test]
    fn new_speed() {
        let speed = Speed::default();
        assert_eq!(speed.total_speed().get::<meter>(), 10.0);
        assert_eq!(speed.moved_this_turn().get::<meter>(), 0.0);
    }

    #[test]
    fn add_flat_modifier() {
        let mut speed = Speed::default();
        speed.add_flat_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "Boots of Speed!")),
            5.0,
        );
        assert_eq!(speed.total_speed().get::<meter>(), 15.0);
    }

    #[test]
    fn remove_flat_modifier() {
        let mut speed = Speed::default();
        speed.add_flat_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "Boots of Speed!")),
            5.0,
        );
        speed.remove_flat_modifier(&ModifierSource::Item(ItemId::new(
            "nat20_core",
            "Boots of Speed!",
        )));
        assert_eq!(speed.total_speed().get::<meter>(), 10.0);
    }

    #[test]
    fn add_multiplier() {
        let mut speed = Speed::default();
        speed.add_multiplier(
            ModifierSource::Effect(EffectId::new("nat20_core", "Expeditious Retreat!")),
            2.0,
        );
        assert_eq!(speed.total_speed().get::<meter>(), 20.0);
    }

    #[test]
    fn remove_multiplier() {
        let mut speed = Speed::default();
        speed.add_multiplier(
            ModifierSource::Effect(EffectId::new("nat20_core", "Expeditious Retreat!")),
            2.0,
        );
        speed.remove_multiplier(&ModifierSource::Effect(EffectId::new(
            "nat20_core",
            "Expeditious Retreat!",
        )));
        assert_eq!(speed.total_speed().get::<meter>(), 10.0);
    }

    #[test]
    fn record_movement_and_remaining() {
        let mut speed = Speed::default();
        speed.record_movement(Length::new::<meter>(3.0));
        assert_eq!(speed.moved_this_turn().get::<meter>(), 3.0);
        assert_eq!(speed.remaining_movement().get::<meter>(), 7.0);
    }

    #[test]
    fn reset() {
        let mut speed = Speed::default();
        speed.record_movement(Length::new::<meter>(5.0));
        speed.reset();
        assert_eq!(speed.moved_this_turn().get::<meter>(), 0.0);
    }

    #[test]
    fn can_move() {
        let mut speed = Speed::default();
        assert!(speed.can_move());
        speed.record_movement(Length::new::<meter>(10.0));
        assert!(!speed.can_move());
    }

    #[test]
    fn total_speed_with_zero_multiplier() {
        let mut speed = Speed::default();
        speed.add_multiplier(
            ModifierSource::Effect(EffectId::new("nat20_core", "Fear!")),
            0.0,
        );
        assert_eq!(speed.total_speed().get::<meter>(), 0.0);
    }

    #[test]
    fn flat_and_multiplier_combination() {
        let mut speed = Speed::default();
        speed.add_flat_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "Boots of Speed!")),
            5.0,
        );
        speed.add_multiplier(
            ModifierSource::Effect(EffectId::new("nat20_core", "Expeditious Retreat!")),
            2.0,
        );
        assert_eq!(speed.total_speed().get::<meter>(), 30.0);
    }
}
