use crate::components::modifier::ModifierSource;

#[derive(Debug, Clone)]
pub struct TemporaryHitPoints {
    amount: u32,
    source: ModifierSource,
}

impl TemporaryHitPoints {
    pub fn new(amount: u32, source: &ModifierSource) -> Self {
        Self {
            amount,
            source: source.clone(),
        }
    }

    pub fn amount(&self) -> u32 {
        self.amount
    }

    pub fn source(&self) -> &ModifierSource {
        &self.source
    }
}

impl Default for TemporaryHitPoints {
    fn default() -> Self {
        Self {
            amount: 0,
            source: ModifierSource::None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HitPoints {
    current: u32,
    max: u32,
    temp: Option<TemporaryHitPoints>,
}

impl HitPoints {
    pub fn new(max: u32) -> Self {
        Self {
            current: max,
            max,
            temp: None,
        }
    }

    pub fn with_current(current: u32, max: u32) -> Self {
        Self {
            current,
            max,
            temp: None,
        }
    }

    pub fn with_temp(current: u32, max: u32, temp: TemporaryHitPoints) -> Self {
        Self {
            current,
            max,
            temp: Some(temp),
        }
    }

    pub fn current(&self) -> u32 {
        self.current
    }

    pub fn max(&self) -> u32 {
        self.max
    }

    pub fn temp(&self) -> Option<&TemporaryHitPoints> {
        self.temp.as_ref()
    }

    pub fn update_max(&mut self, new_max: u32) {
        if new_max < self.current {
            self.current = new_max;
        }
        self.max = new_max;
    }

    /// Applies damage to the hit points. If the entity has temporary hit points,
    /// damage is applied to them first. If the temporary hit points are depleted,
    /// the remaining damage is applied to the current hit points.
    /// Returns the source of any removed temporary hit points.
    pub(crate) fn damage(&mut self, amount: u32) -> Option<ModifierSource> {
        // Damage is applied to temp HP first, then to current HP
        let (remaining, removed_temp_hp_source) = if let Some(temp) = &mut self.temp {
            let temp_damage = amount.min(temp.amount);
            temp.amount -= temp_damage;
            let removed_temp_hp_source = if temp.amount == 0 {
                Some(self.temp.take().unwrap().source)
            } else {
                None
            };
            (amount - temp_damage, removed_temp_hp_source)
        } else {
            (amount, None)
        };

        if remaining >= self.current {
            self.current = 0;
        } else {
            self.current -= remaining;
        }

        removed_temp_hp_source
    }

    pub(crate) fn heal(&mut self, amount: u32) {
        self.current = (self.current + amount).min(self.max);
    }

    pub(crate) fn heal_full(&mut self) {
        self.current = self.max;
    }

    pub fn is_full(&self) -> bool {
        self.current == self.max
    }

    pub fn is_alive(&self) -> bool {
        self.current > 0
    }

    /// Sets temporary hit points. If the new value is higher than the current
    /// temp HP, replaces it. If lower or equal, does nothing (DND 5e rules).
    pub fn set_temp(&mut self, temp: TemporaryHitPoints) {
        match &self.temp {
            Some(current_temp) => {
                if temp.amount > current_temp.amount {
                    self.temp = Some(temp);
                }
            }
            None => {
                self.temp = Some(temp);
            }
        }
    }

    pub fn clear_temp(&mut self, source: &ModifierSource) {
        if let Some(temp) = &self.temp
            && &temp.source == source
        {
            self.temp = None;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::components::id::ActionId;

    use super::*;

    fn temp_hp(amount: u32) -> TemporaryHitPoints {
        TemporaryHitPoints::new(
            amount,
            &ModifierSource::Action(ActionId::new("nat20_core", "temp_hp_test")),
        )
    }

    #[test]
    fn new_initializes_current_and_max() {
        let hp = HitPoints::new(10);
        assert_eq!(hp.current(), 10);
        assert_eq!(hp.max(), 10);
        assert!(hp.temp().is_none());
    }

    #[test]
    fn with_current_initializes_current_and_max() {
        let hp = HitPoints::with_current(5, 10);
        assert_eq!(hp.current(), 5);
        assert_eq!(hp.max(), 10);
        assert!(hp.temp().is_none());
    }

    #[test]
    fn with_temp_initializes_all_fields() {
        let hp = HitPoints::with_temp(5, 10, temp_hp(7));
        assert_eq!(hp.current(), 5);
        assert_eq!(hp.max(), 10);
        assert_eq!(hp.temp().unwrap().amount(), 7);
    }

    #[test]
    fn damage_reduces_temp_first_then_current() {
        let mut hp = HitPoints::with_temp(10, 10, temp_hp(5));
        hp.damage(3);
        assert_eq!(hp.temp().unwrap().amount(), 2);
        assert_eq!(hp.current(), 10);

        hp.damage(4); // 2 temp left, so 2 temp gone, 2 to current
        assert!(hp.temp().is_none());
        assert_eq!(hp.current(), 8);
    }

    #[test]
    fn damage_does_not_go_below_zero() {
        let mut hp = HitPoints::with_temp(5, 5, temp_hp(3));
        hp.damage(10);
        assert!(hp.temp().is_none());
        assert_eq!(hp.current(), 0);
    }

    #[test]
    fn heal_increases_current_but_not_above_max() {
        let mut hp = HitPoints::new(10);
        hp.damage(5);
        hp.heal(3);
        assert_eq!(hp.current(), 8);

        hp.heal(10);
        assert_eq!(hp.current(), 10);
    }

    #[test]
    fn heal_when_at_max_does_nothing() {
        let mut hp = HitPoints::new(7);
        hp.heal(5);
        assert_eq!(hp.current(), 7);
    }

    #[test]
    fn damage_exact_to_zero_with_temp() {
        let mut hp = HitPoints::with_temp(4, 4, temp_hp(2));
        hp.damage(6);
        assert!(hp.temp().is_none());
        assert_eq!(hp.current(), 0);
    }

    #[test]
    fn set_temp_only_increases_temp() {
        let mut hp = HitPoints::new(10);
        hp.set_temp(temp_hp(5));
        assert_eq!(hp.temp().unwrap().amount(), 5);
        hp.set_temp(temp_hp(3));
        assert_eq!(hp.temp().unwrap().amount(), 5);
        hp.set_temp(temp_hp(8));
        assert_eq!(hp.temp().unwrap().amount(), 8);
    }

    #[test]
    fn clear_temp_sets_temp_to_zero() {
        let mut hp = HitPoints::with_temp(10, 10, temp_hp(7));
        hp.clear_temp(&ModifierSource::Action(ActionId::new(
            "nat20_core",
            "temp_hp_test",
        )));
        assert!(hp.temp().is_none());
    }

    #[test]
    fn clear_temp_with_different_source_does_nothing() {
        let mut hp = HitPoints::with_temp(10, 10, temp_hp(7));
        hp.clear_temp(&ModifierSource::Action(ActionId::new(
            "nat20_core",
            "different_source",
        )));
        assert!(hp.temp().is_some());
    }
}
