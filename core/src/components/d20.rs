use std::{cmp::max, collections::HashMap, fmt, hash::Hash};

use hecs::{Entity, World};
use rand::Rng;
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        effects::hooks::D20CheckHooks,
        modifier::{KeyedModifiable, Modifiable, ModifierSet, ModifierSource},
        proficiency::{Proficiency, ProficiencyLevel},
    },
    systems,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RollMode {
    Normal,
    Advantage,
    Disadvantage,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AdvantageType {
    Advantage,
    Disadvantage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdvantageSource {
    pub kind: AdvantageType,
    pub source: ModifierSource,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct AdvantageTracker {
    sources: Vec<AdvantageSource>,
}

impl AdvantageTracker {
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
        }
    }

    pub fn add(&mut self, kind: AdvantageType, source: ModifierSource) {
        self.sources.push(AdvantageSource { kind, source });
    }

    pub fn remove(&mut self, source: &ModifierSource) {
        self.sources.retain(|s| &s.source != source);
    }

    pub fn roll_mode(&self) -> RollMode {
        match self.sources.iter().fold(0, |acc, s| {
            acc + match s.kind {
                AdvantageType::Advantage => 1,
                AdvantageType::Disadvantage => -1,
            }
        }) {
            n if n > 0 => RollMode::Advantage,
            n if n < 0 => RollMode::Disadvantage,
            _ => RollMode::Normal,
        }
    }

    pub fn summary(&self) -> Vec<(&ModifierSource, AdvantageType)> {
        self.sources.iter().map(|s| (&s.source, s.kind)).collect()
    }
}

pub static D20_CRITICAL_SUCCESS: u8 = 20;
pub static D20_CRITICAL_FAILURE: u8 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum D20CheckOutcome {
    Success,
    Failure,
    CriticalSuccess,
    CriticalFailure,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct D20Check {
    modifiers: ModifierSet,
    proficiency: Proficiency,
    advantage_tracker: AdvantageTracker,
    forced_outcome: Option<(ModifierSource, D20CheckOutcome)>,
}

impl D20Check {
    pub fn new(proficiency: Proficiency) -> Self {
        Self {
            modifiers: ModifierSet::new(),
            proficiency,
            advantage_tracker: AdvantageTracker::new(),
            forced_outcome: None,
        }
    }

    pub fn advantage_tracker(&self) -> &AdvantageTracker {
        &self.advantage_tracker
    }

    pub fn advantage_tracker_mut(&mut self) -> &mut AdvantageTracker {
        &mut self.advantage_tracker
    }

    pub fn modifiers(&self) -> &ModifierSet {
        &self.modifiers
    }

    pub fn modifiers_mut(&mut self) -> &mut ModifierSet {
        &mut self.modifiers
    }

    pub fn set_forced_outcome(&mut self, source: ModifierSource, outcome: D20CheckOutcome) {
        self.forced_outcome = Some((source, outcome));
    }

    pub fn clear_forced_outcome(&mut self) {
        self.forced_outcome = None;
    }

    pub fn proficiency(&self) -> &Proficiency {
        &self.proficiency
    }

    pub fn set_proficiency(&mut self, proficiency: Proficiency) {
        self.proficiency = proficiency;
    }

    pub fn roll(&self, proficiency_bonus: u8) -> D20CheckResult {
        let mut modifiers = self.modifiers.clone();
        modifiers.add_modifier(
            ModifierSource::Proficiency(self.proficiency.level().clone()),
            self.proficiency.bonus(proficiency_bonus) as i32,
        );

        let mut rng = rand::rng();
        let roll_mode = self.advantage_tracker.roll_mode();
        let mut rolls = match roll_mode {
            RollMode::Normal => vec![rng.random_range(1..=20) as u8],
            _ => vec![
                rng.random_range(1..=20) as u8,
                rng.random_range(1..=20) as u8,
            ],
        };

        if let Some((source, forced_outcome)) = &self.forced_outcome {
            match forced_outcome {
                D20CheckOutcome::Success => {
                    // TODO: Success depends on the DC, so we can't just force it here?
                    todo!()
                }

                D20CheckOutcome::Failure => {
                    // We can approximate a failure by subtracting the total
                    let total = (rolls.iter().max().unwrap().clone() as i32) + modifiers.total();
                    modifiers.add_modifier(source.clone(), -total);
                }

                D20CheckOutcome::CriticalSuccess => {
                    for roll in rolls.iter_mut() {
                        *roll = D20_CRITICAL_SUCCESS;
                    }
                }

                D20CheckOutcome::CriticalFailure => {
                    for roll in rolls.iter_mut() {
                        *roll = D20_CRITICAL_FAILURE;
                    }
                }
            }
        }

        let selected_roll = match roll_mode {
            RollMode::Normal => rolls[0],
            RollMode::Advantage => rolls.iter().max().unwrap().clone(),
            RollMode::Disadvantage => rolls.iter().min().unwrap().clone(),
        };

        let outcome = if selected_roll == D20_CRITICAL_SUCCESS {
            Some(D20CheckOutcome::CriticalSuccess)
        } else if selected_roll == D20_CRITICAL_FAILURE {
            Some(D20CheckOutcome::CriticalFailure)
        } else {
            None
        };

        D20CheckResult {
            advantage_tracker: self.advantage_tracker.clone(),
            rolls,
            selected_roll,
            modifier_breakdown: modifiers.clone(),
            outcome,
        }
    }

    pub fn roll_hooks(
        &self,
        world: &World,
        entity: Entity,
        hooks: &Vec<D20CheckHooks>,
    ) -> D20CheckResult {
        let mut check = self.clone();
        for hook in hooks {
            (hook.check_hook)(world, entity, &mut check);
        }

        let proficiency_bonus = systems::helpers::level(world, entity)
            .unwrap()
            .proficiency_bonus();
        let mut result = check.roll(proficiency_bonus);

        for hook in hooks {
            (hook.result_hook)(world, entity, &mut result);
        }

        result
    }

    pub fn success_probability(&self, target_dc: u32, proficiency_bonus: u8) -> f64 {
        if let Some(forced_outcome) = &self.forced_outcome {
            return match forced_outcome.1 {
                D20CheckOutcome::Success | D20CheckOutcome::CriticalSuccess => 1.0,
                D20CheckOutcome::Failure | D20CheckOutcome::CriticalFailure => 0.0,
            };
        }

        let mut total_modifier = self.modifiers.total();
        total_modifier += self.proficiency.bonus(proficiency_bonus) as i32;

        let roll_mode = self.advantage_tracker.roll_mode();

        // Needed raw roll
        let needed_roll = (target_dc as i32 - total_modifier).clamp(2, 20);

        let single_roll_p = (21 - needed_roll) as f64 / 20.0;

        match roll_mode {
            RollMode::Normal => single_roll_p,
            RollMode::Advantage => 1.0 - (1.0 - single_roll_p).powi(2),
            RollMode::Disadvantage => single_roll_p.powi(2),
        }
    }
}

impl Modifiable for D20Check {
    fn add_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<i32>,
    {
        self.modifiers.add_modifier(source, value.into());
    }

    fn remove_modifier(&mut self, source: &ModifierSource) {
        self.modifiers.remove_modifier(source);
    }

    fn total(&self) -> i32 {
        self.modifiers.total()
    }
}

impl fmt::Display for D20Check {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "1d20")?;
        if self.proficiency.level() != &ProficiencyLevel::None {
            write!(f, " + {}", self.proficiency.level())?;
        }
        if self.modifiers.is_empty() {
            return Ok(());
        }
        write!(f, " {}", self.modifiers)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct D20CheckResult {
    pub advantage_tracker: AdvantageTracker,
    pub rolls: Vec<u8>,
    pub selected_roll: u8,
    pub modifier_breakdown: ModifierSet,
    pub outcome: Option<D20CheckOutcome>,
}

impl D20CheckResult {
    pub fn total_modifier(&self) -> i32 {
        self.modifier_breakdown.total()
    }

    pub fn total(&self) -> u32 {
        max(self.selected_roll as i32 + self.total_modifier(), 0) as u32
    }

    pub fn is_success<T>(&self, dc: &D20CheckDC<T>) -> bool
    where
        T: IntoEnumIterator + Copy + Eq + Hash,
    {
        match self.outcome {
            Some(D20CheckOutcome::CriticalSuccess) => true,
            Some(D20CheckOutcome::CriticalFailure) => false,
            _ => self.total() >= dc.dc.total() as u32,
        }
    }

    pub fn add_bonus(&mut self, source: ModifierSource, value: i32) {
        self.modifier_breakdown.add_modifier(source, value);
    }
}

impl fmt::Display for D20CheckResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (1d20)", self.selected_roll)?;
        if self.advantage_tracker.roll_mode() != RollMode::Normal {
            write!(
                f,
                " ({}, {}, {:?})",
                self.rolls[0],
                self.rolls[1],
                self.advantage_tracker.roll_mode()
            )?;
        }
        match self.outcome {
            Some(D20CheckOutcome::CriticalSuccess) => write!(f, " (Critical Success)")?,
            Some(D20CheckOutcome::CriticalFailure) => write!(f, " (Critical Failure)")?,
            _ => {}
        }
        if !self.modifier_breakdown.is_empty() {
            write!(f, " {}", self.modifier_breakdown)?;
        }
        write!(f, " = {}", self.total())?;
        Ok(())
    }
}

pub trait D20CheckKey: Eq + Hash + IntoEnumIterator + Copy {}

impl<T: Eq + Hash + IntoEnumIterator + Copy> D20CheckKey for T {}

#[derive(Debug, Clone)]
pub struct D20CheckMap<K>
where
    K: D20CheckKey,
{
    checks: HashMap<K, D20Check>,
    ability_mapper: fn(&K) -> Option<Ability>,
    get_hooks: fn(&K, &World, Entity) -> Vec<D20CheckHooks>,
}

impl<K> D20CheckMap<K>
where
    K: D20CheckKey,
{
    pub fn new(
        ability_mapper: fn(&K) -> Option<Ability>,
        get_hooks: fn(&K, &World, Entity) -> Vec<D20CheckHooks>,
    ) -> Self {
        let checks = K::iter()
            .map(|k| {
                (
                    k,
                    D20Check::new(Proficiency::new(
                        ProficiencyLevel::None,
                        ModifierSource::None,
                    )),
                )
            })
            .collect();
        Self {
            checks,
            ability_mapper,
            get_hooks,
        }
    }

    pub fn get(&self, key: &K) -> &D20Check {
        self.checks.get(&key).unwrap()
    }

    pub fn get_mut(&mut self, key: &K) -> &mut D20Check {
        self.checks.get_mut(key).unwrap()
    }

    pub fn set_proficiency(&mut self, key: &K, proficiency: Proficiency) {
        self.get_mut(key).set_proficiency(proficiency);
    }

    pub fn add_advantage(&mut self, key: &K, kind: AdvantageType, source: ModifierSource) {
        self.get_mut(key).advantage_tracker_mut().add(kind, source);
    }

    pub fn remove_advantage(&mut self, key: &K, source: &ModifierSource) {
        self.get_mut(key).advantage_tracker_mut().remove(source);
    }

    pub fn set_forced_outcome(
        &mut self,
        key: &K,
        source: ModifierSource,
        outcome: D20CheckOutcome,
    ) {
        self.get_mut(key).set_forced_outcome(source, outcome);
    }

    pub fn clear_forced_outcome(&mut self, key: &K) {
        self.get_mut(key).clear_forced_outcome();
    }

    pub fn check(&self, key: &K, world: &World, entity: Entity) -> D20CheckResult {
        let mut d20 = self.get(key).clone();
        if let Some(ability) = (self.ability_mapper)(key) {
            let ability_scores = systems::helpers::get_component::<AbilityScoreMap>(world, entity);
            d20.add_modifier(
                ModifierSource::Ability(ability),
                ability_scores.ability_modifier(&ability).total(),
            );
        }

        d20.roll_hooks(world, entity, &(self.get_hooks)(key, world, entity))
    }

    pub fn check_dc(&self, dc: &D20CheckDC<K>, world: &World, entity: Entity) -> D20CheckResult {
        let mut result = self.check(&dc.key, world, entity);
        if result.outcome.is_none() {
            result.outcome = if result.total() >= dc.dc.total() as u32 {
                Some(D20CheckOutcome::Success)
            } else {
                Some(D20CheckOutcome::Failure)
            };
        }

        result
    }
}

impl<K> KeyedModifiable<K> for D20CheckMap<K>
where
    K: D20CheckKey,
{
    fn add_modifier<T>(&mut self, key: &K, source: ModifierSource, value: T)
    where
        T: Into<i32>,
    {
        self.get_mut(key).add_modifier(source, value.into());
    }

    fn remove_modifier(&mut self, key: &K, source: &ModifierSource) {
        self.get_mut(key).remove_modifier(source);
    }

    fn total(&self, key: &K) -> i32 {
        self.get(key).modifiers().total()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct D20CheckDC<T>
where
    T: IntoEnumIterator + Copy + Eq + Hash,
{
    pub key: T,
    pub dc: ModifierSet,
}

impl fmt::Display for D20CheckDC<Ability> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DC {}", self.dc)?;
        write!(f, " {}", self.key)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::components::id::ItemId;

    use super::*;

    #[test]
    fn d20_check() {
        let mut check = D20Check::new(Proficiency::new(
            ProficiencyLevel::Proficient,
            ModifierSource::None,
        ));
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );
        println!("Check: {}", check);
        let result = check.roll(2);

        // 1d20 + 2 + 2
        // Min: 1 + 2 + 2 = 5
        // Max: 20 + 2 + 2 = 24
        assert!(result.total() >= 5 && result.total() <= 24);
        assert_eq!(result.rolls.len(), 1);
        assert_eq!(result.advantage_tracker.roll_mode(), RollMode::Normal);
        println!("Result: {}", result);
    }

    #[test]
    fn d20_check_with_advantage() {
        let mut check = D20Check::new(Proficiency::new(
            ProficiencyLevel::Proficient,
            ModifierSource::None,
        ));
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );
        check.advantage_tracker.add(
            AdvantageType::Advantage,
            ModifierSource::Item(ItemId::new("nat20_core", "item.lucky_charm")),
        );
        let result = check.roll(0);

        // 1d20 + 2
        // Min: 1 + 2 = 3
        // Max: 20 + 2 = 22
        assert!(result.total() >= 3 && result.total() <= 22);
        assert_eq!(result.rolls.len(), 2);
        assert_eq!(result.advantage_tracker.roll_mode(), RollMode::Advantage);
        // Check if the selected roll is the maximum
        assert_eq!(
            result.selected_roll,
            result.rolls.iter().max().unwrap().clone()
        );
        println!("Result: {}", result);
    }

    #[test]
    fn d20_check_with_disadvantage() {
        let mut check = D20Check::new(Proficiency::new(
            ProficiencyLevel::Expertise,
            ModifierSource::Custom("Somewhere".to_string()),
        ));
        check.advantage_tracker.add(
            AdvantageType::Disadvantage,
            ModifierSource::Item(ItemId::new("nat20_core", "item.cursed_ring")),
        );
        let result = check.roll(4);

        // 1d20
        // Min: 1 + 8 = 9
        // Max: 20 + 8 = 28
        assert!(result.total() >= 9 && result.total() <= 28);
        assert_eq!(result.rolls.len(), 2);
        assert_eq!(result.advantage_tracker.roll_mode(), RollMode::Disadvantage);
        // Check if the selected roll is the minimum
        assert_eq!(
            result.selected_roll,
            result.rolls.iter().min().unwrap().clone()
        );
        println!("Result: {}", result);
    }

    #[test]
    fn d20_check_with_advantage_and_disadvantage() {
        let mut check = D20Check::new(Proficiency::new(
            ProficiencyLevel::Expertise,
            ModifierSource::Custom("Genetics".to_string()),
        ));
        check.advantage_tracker.add(
            AdvantageType::Advantage,
            ModifierSource::Item(ItemId::new("nat20_core", "item.lucky_charm")),
        );
        check.advantage_tracker.add(
            AdvantageType::Disadvantage,
            ModifierSource::Item(ItemId::new("nat20_core", "item.cursed_ring")),
        );
        let result = check.roll(4);

        // 1d20
        // Min: 1 + 8 = 9
        // Max: 20 + 8 = 28
        assert!(result.total() >= 9 && result.total() <= 28);
        assert_eq!(result.rolls.len(), 1);
        assert_eq!(result.advantage_tracker.roll_mode(), RollMode::Normal);
        println!("Result: {}", result);
    }

    #[test]
    fn d20_check_critical_success() {
        let mut check = D20Check::new(Proficiency::new(
            ProficiencyLevel::Proficient,
            ModifierSource::None,
        ));
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );
        let mut result = check.roll(0);
        while result.selected_roll != 20 {
            // Simulate rolling again until we get a critical success
            result = check.roll(0);
        }

        // Simulate a critical success by setting the selected roll to 20
        assert!(matches!(
            result.outcome,
            Some(D20CheckOutcome::CriticalSuccess)
        ));
        println!("Result: {}", result);
    }

    #[test]
    fn d20_check_critical_failure() {
        let mut check = D20Check::new(Proficiency::new(
            ProficiencyLevel::Proficient,
            ModifierSource::None,
        ));
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );
        let mut result = check.roll(0);
        while result.selected_roll != 1 {
            // Simulate rolling again until we get a critical failure
            result = check.roll(0);
        }

        // Simulate a critical failure by setting the selected roll to 1
        assert!(matches!(
            result.outcome,
            Some(D20CheckOutcome::CriticalFailure)
        ));
        println!("Result: {}", result);
    }

    #[test]
    fn d20_check_success_probability() {
        let mut check = D20Check::new(Proficiency::new(
            ProficiencyLevel::Proficient,
            ModifierSource::None,
        ));
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );

        let proficiency_bonus = 2;
        let target_dc = 15;

        let success_probability = check.success_probability(target_dc, proficiency_bonus);
        println!(
            "Success probability against DC {}: {:.2}%",
            target_dc,
            success_probability * 100.0
        );

        assert!(success_probability > 0.0 && success_probability < 1.0);
    }

    #[test]
    fn d20_forced_outcome() {
        let mut check = D20Check::new(Proficiency::new(
            ProficiencyLevel::Proficient,
            ModifierSource::None,
        ));
        check.set_forced_outcome(
            ModifierSource::Custom("Test".to_string()),
            D20CheckOutcome::CriticalSuccess,
        );
        let result = check.roll(2);

        assert_eq!(result.selected_roll, D20_CRITICAL_SUCCESS);
        assert!(matches!(
            result.outcome,
            Some(D20CheckOutcome::CriticalSuccess)
        ));
        println!("Result with forced critical success: {}", result);
    }
}
