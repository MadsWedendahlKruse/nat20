// TODO: Consider a different name?

use std::{collections::HashMap, ops::Deref};

use tracing::warn;
use uom::si::{f32::Length, length::meter};

use crate::components::modifier::ModifierSource;

/// Movement speed of an entity, internally stored in meters (per turn).
///
/// This tracks the base speed and multipliers, as well as the distance moved this
/// turn and the free movement used this turn.
/// Since some effects can (conditionally) modify the speed, stuff like the total
/// and remaining movement is retrieved from [`EffectiveSpeed`] instead, which is
/// a wrapper where all the effect hooks have been applied. This can be grabbed via
/// [`crate::systems::movement::speed`].
#[derive(Debug, Clone)]
pub struct Speed {
    flat: HashMap<ModifierSource, f32>,
    multipliers: HashMap<ModifierSource, f32>,
    moved_this_turn: f32,

    free_movement_multipliers: HashMap<ModifierSource, f32>,
    free_movement_used: f32,
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
            free_movement_used: 0.0,
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

    pub fn moved_this_turn(&self) -> Length {
        Length::new::<meter>(self.moved_this_turn)
    }

    // Both counters are pure turn state, so this stays correct on the stored component even
    // though the totals they're measured against depend on effect hooks.
    pub fn record_movement(&mut self, distance: Length) {
        let distance = distance.get::<meter>();
        self.moved_this_turn += distance;
        self.free_movement_used += distance;
    }

    /// Should be called at the start (or end?) of each turn
    pub fn reset(&mut self) {
        self.moved_this_turn = 0.0;
        self.free_movement_used = 0.0;
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
        self.free_movement_multipliers.insert(source, value);
    }

    pub fn remove_free_movement_multiplier(&mut self, source: &ModifierSource) {
        self.free_movement_multipliers.remove(source);
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

/// A [`Speed`] with every effect hook applied. It owns all the derived values,
/// so reading one is proof that the hooks ran.
/// The only way to get one is [`crate::systems::movement::speed`].
///
/// TODO: Not sure if I'm sold on the name
#[derive(Debug, Clone)]
pub struct EffectiveSpeed(Speed);

impl EffectiveSpeed {
    pub(crate) fn new(speed: Speed) -> Self {
        Self(speed)
    }

    pub fn total_speed(&self) -> Length {
        let base_speed: f32 = self.0.flat.values().sum();

        let total_multiplier: f32 = if self.0.multipliers.is_empty() {
            1.0
        } else {
            self.0.multipliers.values().product()
        };

        Length::new::<meter>(base_speed * total_multiplier)
    }

    pub fn remaining_movement(&self) -> Length {
        let total_speed = self.total_speed().get::<meter>();
        let remaining = (total_speed - self.0.moved_this_turn).max(0.0);
        Length::new::<meter>(remaining)
    }

    pub fn can_move(&self) -> bool {
        self.remaining_movement().get::<meter>() > 0.0
    }

    pub fn free_movement_remaining(&self) -> Length {
        let remaining = (self.max_free_movement() - self.0.free_movement_used).max(0.0);
        Length::new::<meter>(remaining)
    }

    fn max_free_movement(&self) -> f32 {
        if self.0.free_movement_multipliers.is_empty() {
            0.0
        } else {
            let free_movement_multiplier = self
                .0
                .free_movement_multipliers
                .values()
                .sum::<f32>()
                .min(1.0);
            free_movement_multiplier * self.total_speed().get::<meter>()
        }
    }
}

/// Convenience implementation to acces the underlying [`Speed`] as read-only.
impl Deref for EffectiveSpeed {
    type Target = Speed;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use crate::components::id::{EffectId, ItemId};

    use super::*;

    // Stands in for `systems::movement::speed` when there are no hooks to run
    fn effective(speed: &Speed) -> EffectiveSpeed {
        EffectiveSpeed::new(speed.clone())
    }

    #[test]
    fn new_speed() {
        let speed = Speed::default();
        assert_eq!(effective(&speed).total_speed().get::<meter>(), 10.0);
        assert_eq!(speed.moved_this_turn().get::<meter>(), 0.0);
    }

    #[test]
    fn add_flat_modifier() {
        let mut speed = Speed::default();
        speed.add_flat_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "Boots of Speed!")),
            5.0,
        );
        assert_eq!(effective(&speed).total_speed().get::<meter>(), 15.0);
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
        assert_eq!(effective(&speed).total_speed().get::<meter>(), 10.0);
    }

    #[test]
    fn add_multiplier() {
        let mut speed = Speed::default();
        speed.add_multiplier(
            ModifierSource::Effect(EffectId::new("nat20_core", "Expeditious Retreat!")),
            2.0,
        );
        assert_eq!(effective(&speed).total_speed().get::<meter>(), 20.0);
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
        assert_eq!(effective(&speed).total_speed().get::<meter>(), 10.0);
    }

    #[test]
    fn record_movement_and_remaining() {
        let mut speed = Speed::default();
        speed.record_movement(Length::new::<meter>(3.0));
        assert_eq!(speed.moved_this_turn().get::<meter>(), 3.0);
        assert_eq!(effective(&speed).remaining_movement().get::<meter>(), 7.0);
    }

    #[test]
    fn remaining_movement_never_goes_negative() {
        let mut speed = Speed::default();
        speed.record_movement(Length::new::<meter>(15.0));
        assert_eq!(effective(&speed).remaining_movement().get::<meter>(), 0.0);
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
        assert!(effective(&speed).can_move());
        speed.record_movement(Length::new::<meter>(10.0));
        assert!(!effective(&speed).can_move());
    }

    #[test]
    fn total_speed_with_zero_multiplier() {
        let mut speed = Speed::default();
        speed.add_multiplier(
            ModifierSource::Effect(EffectId::new("nat20_core", "Fear!")),
            0.0,
        );
        assert_eq!(effective(&speed).total_speed().get::<meter>(), 0.0);
    }

    #[test]
    fn free_movement_follows_the_current_total() {
        let mut speed = Speed::default();
        speed.add_free_movement_multiplier(
            ModifierSource::Effect(EffectId::new("nat20_core", "Tactical Shift!")),
            0.5,
        );
        assert_eq!(
            effective(&speed).free_movement_remaining().get::<meter>(),
            5.0
        );

        // A speed bump after the fact rescales the free movement too
        speed.add_flat_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "Boots of Speed!")),
            10.0,
        );
        assert_eq!(
            effective(&speed).free_movement_remaining().get::<meter>(),
            10.0
        );

        speed.record_movement(Length::new::<meter>(4.0));
        assert_eq!(
            effective(&speed).free_movement_remaining().get::<meter>(),
            6.0
        );
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
        assert_eq!(effective(&speed).total_speed().get::<meter>(), 30.0);
    }
}
