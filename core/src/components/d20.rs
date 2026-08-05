use std::{cmp::max, collections::HashMap, hash::Hash};

use hecs::Entity;
use rand::Rng;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumDiscriminants, IntoEnumIterator};

use crate::{
    components::{
        ability::Ability,
        damage::AttackSource,
        id::EntityIdentifier,
        items::equipment::armor::ArmorClass,
        modifier::{
            FlatModifiable, FlatModifierMap, KeyedModifiable, Modifiable, ModifierKind,
            ModifierMap, ModifierResult, ModifierSource,
        },
        proficiency::Proficiency,
        range::Range,
        saving_throw::SavingThrowKind,
        skill::Skill,
    },
    engine::game_state::GameState,
    systems::{self},
};

/// Trait describing rolls that can have advantage or disadvantage, currently only
/// relevant for D20Check and D20CheckResult, but could be expanded to other roll
/// types in the future?
/// Mostly a convenience trait to avoid duplication in the Lua API, though worth
/// noting that there's a difference applying it to a D20Check vs a D20CheckResult.
/// For the check itself it will affect the next roll, while for the result it will
/// have to take into account the previous roll and potentially reroll if the advantage
/// state changes.
pub trait AdvantageAware {
    fn add_advantage(&mut self, kind: AdvantageType, source: ModifierSource);
    fn remove_advantage(&mut self, source: &ModifierSource);
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RollMode {
    Normal,
    Advantage,
    Disadvantage,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, Serialize, Deserialize)]
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

pub const D20_CRITICAL_SUCCESS: u8 = 20;
pub const D20_CRITICAL_FAILURE: u8 = 1;
pub const D20_MIN_CRIT_THRESHOLD: u8 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum D20CheckOutcome {
    Success,
    Failure,
    CriticalSuccess,
    CriticalFailure,
}

impl D20CheckOutcome {
    pub fn is_success(&self) -> bool {
        match self {
            D20CheckOutcome::Success | D20CheckOutcome::CriticalSuccess => true,
            D20CheckOutcome::Failure | D20CheckOutcome::CriticalFailure => false,
        }
    }
}

// TODO: Why do we call it SavingTHROW and AttackROLL, but not SkillCHECK?
#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(
    name(D20CheckKindTag),
    derive(Hash, Serialize, Deserialize, JsonSchema),
    serde(rename_all = "snake_case")
)]
pub enum D20CheckKind {
    SavingThrow(SavingThrowKind),
    Skill(Skill),
    AttackRoll(AttackSource),
}

impl D20CheckKind {
    pub fn ability(&self) -> Option<Ability> {
        match self {
            D20CheckKind::SavingThrow(kind) => kind.ability(),
            D20CheckKind::Skill(skill) => Some(skill.ability()),
            D20CheckKind::AttackRoll(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct D20Check {
    kind: D20CheckKind,
    ability: Option<Ability>,
    modifiers: ModifierMap,
    proficiency: Proficiency,
    advantage_tracker: AdvantageTracker,
    forced_outcome: Option<(ModifierSource, D20CheckOutcome)>,
    crit_threshold_reduction: FlatModifierMap,
}

impl D20Check {
    pub fn new(kind: D20CheckKind, proficiency: Proficiency) -> Self {
        let ability = kind.ability();
        Self {
            kind,
            ability,
            modifiers: ModifierMap::default(),
            proficiency,
            advantage_tracker: AdvantageTracker::new(),
            forced_outcome: None,
            crit_threshold_reduction: FlatModifierMap::default(),
        }
    }

    pub fn kind(&self) -> &D20CheckKind {
        &self.kind
    }

    pub fn ability(&self) -> Option<Ability> {
        self.ability
    }

    pub fn set_ability(&mut self, ability: Option<Ability>) {
        self.ability = ability;
    }

    pub fn crit_threshold(&self) -> u8 {
        let reduction = self.crit_threshold_reduction.total().max(0) as u8;
        D20_CRITICAL_SUCCESS
            .saturating_sub(reduction)
            .max(D20_MIN_CRIT_THRESHOLD)
    }

    pub fn add_crit_threshold_reduction(&mut self, source: ModifierSource, reduction: u8) {
        self.crit_threshold_reduction
            .add_modifier(source, reduction as i32);
    }

    pub fn remove_crit_threshold_reduction(&mut self, source: &ModifierSource) {
        self.crit_threshold_reduction.remove_modifier(source);
    }

    pub fn crit_threshold_reduction(&self) -> &FlatModifierMap {
        &self.crit_threshold_reduction
    }

    pub fn crit_threshold_reduction_mut(&mut self) -> &mut FlatModifierMap {
        &mut self.crit_threshold_reduction
    }

    pub fn advantage_tracker(&self) -> &AdvantageTracker {
        &self.advantage_tracker
    }

    pub fn advantage_tracker_mut(&mut self) -> &mut AdvantageTracker {
        &mut self.advantage_tracker
    }

    pub fn set_forced_outcome(&mut self, source: ModifierSource, outcome: D20CheckOutcome) {
        self.forced_outcome = Some((source, outcome));
    }

    pub fn clear_forced_outcome(&mut self) {
        self.forced_outcome = None;
    }

    pub fn forced_outcome(&self) -> Option<&(ModifierSource, D20CheckOutcome)> {
        self.forced_outcome.as_ref()
    }

    pub fn proficiency(&self) -> &Proficiency {
        &self.proficiency
    }

    pub fn set_proficiency(&mut self, proficiency: Proficiency) {
        self.proficiency = proficiency;
    }

    pub fn roll(&self) -> D20CheckResult {
        let check: D20Check = self.clone();

        let mut rng = rand::rng();
        let roll_mode = self.advantage_tracker.roll_mode();
        let rolls = match roll_mode {
            RollMode::Normal => vec![rng.random_range(1..=20) as u8],
            _ => vec![
                rng.random_range(1..=20) as u8,
                rng.random_range(1..=20) as u8,
            ],
        };

        let selected_roll = match roll_mode {
            RollMode::Normal => rolls[0],
            RollMode::Advantage => rolls.iter().max().unwrap().clone(),
            RollMode::Disadvantage => rolls.iter().min().unwrap().clone(),
        };

        let modifier_result = check.modifiers.evaluate();
        let crit_threshold = self.crit_threshold();

        let outcome = if let Some((_, forced_outcome)) = &self.forced_outcome {
            Some(forced_outcome.clone())
        } else if selected_roll >= crit_threshold {
            Some(D20CheckOutcome::CriticalSuccess)
        } else if selected_roll == D20_CRITICAL_FAILURE {
            Some(D20CheckOutcome::CriticalFailure)
        } else {
            None
        };

        D20CheckResult {
            check,
            rolls,
            selected_roll,
            outcome,
            crit_threshold,
            modifier_result,
        }
    }

    pub fn roll_hooks(&self, game_state: &GameState, entity: Entity) -> D20CheckResult {
        let mut check = self.clone();

        let proficiency_bonus = systems::helpers::level(&game_state.world, entity)
            .unwrap()
            .proficiency_bonus();

        check.replace_modifier(
            ModifierSource::Proficiency(check.proficiency.level().clone()),
            check.proficiency.bonus(proficiency_bonus) as i32,
        );

        systems::effects::effects(&game_state.world, entity)
            .pre_d20_check(game_state, entity, &mut check);

        let mut result = check.roll();

        systems::effects::effects(&game_state.world, entity).post_d20_check(
            game_state,
            entity,
            &mut result,
        );

        result
    }

    pub fn roll_dc(
        &self,
        game_state: &GameState,
        entity: Entity,
        dc: &D20CheckDC,
    ) -> Result<D20CheckResult, D20Error> {
        if !self.matches_kind(&dc.kind()) {
            return Err(D20Error::KindMismatch {
                check_kind: self.kind.clone(),
                dc_kind: dc.kind().clone(),
            });
        }

        let mut result = self.roll_hooks(game_state, entity);

        if result.outcome.is_none() {
            result.outcome = if result.total() >= dc.total() {
                Some(D20CheckOutcome::Success)
            } else {
                Some(D20CheckOutcome::Failure)
            };
        }

        Ok(result)
    }

    pub fn matches_kind(&self, other: &D20CheckKind) -> bool {
        match (&self.kind, other) {
            (D20CheckKind::SavingThrow(a), D20CheckKind::SavingThrow(b)) => a == b,
            (D20CheckKind::Skill(a), D20CheckKind::Skill(b)) => a == b,
            (D20CheckKind::AttackRoll(a), D20CheckKind::AttackRoll(b)) => a == b,
            _ => false,
        }
    }

    /// Folds another check's persistent state (modifiers, advantage, crit
    /// threshold, forced outcome) into this one — used to combine a stored
    /// attack roll template with the weapon-derived check. Proficiency is
    /// deliberately left alone.
    pub fn merge_from(&mut self, other: &D20Check) {
        self.modifiers.add_modifier_map(&other.modifiers);

        for (source, advantage) in other.advantage_tracker.summary() {
            self.advantage_tracker.add(advantage, source.clone());
        }

        self.crit_threshold_reduction
            .add_modifier_map(&other.crit_threshold_reduction);

        if let Some((source, forced_outcome)) = &other.forced_outcome {
            self.set_forced_outcome(source.clone(), *forced_outcome);
        }
    }

    pub fn success_probability(&self, target_dc: u32, proficiency_bonus: u8) -> Range<f32> {
        if let Some(forced_outcome) = &self.forced_outcome {
            return match forced_outcome.1 {
                D20CheckOutcome::Success | D20CheckOutcome::CriticalSuccess => Range::single(1.0),
                D20CheckOutcome::Failure | D20CheckOutcome::CriticalFailure => Range::single(0.0),
            };
        }

        let total_modifier = self.modifiers.range();
        let total_modifier = total_modifier.add(self.proficiency.bonus(proficiency_bonus) as i32);

        let roll_mode = self.advantage_tracker.roll_mode();

        let roll_p = [total_modifier.min, total_modifier.max].map(|modifier| {
            let needed = (target_dc as i32 - modifier).clamp(2, 20);

            let single_roll_p = (21 - needed) as f64 / 20.0;

            match roll_mode {
                RollMode::Normal => single_roll_p,
                RollMode::Advantage => 1.0 - (1.0 - single_roll_p).powi(2),
                RollMode::Disadvantage => single_roll_p.powi(2),
            }
        });

        Range {
            min: roll_p[0] as f32,
            max: roll_p[1] as f32,
        }
    }
}

impl AdvantageAware for D20Check {
    fn add_advantage(&mut self, kind: AdvantageType, source: ModifierSource) {
        self.advantage_tracker.add(kind, source);
    }

    fn remove_advantage(&mut self, source: &ModifierSource) {
        self.advantage_tracker.remove(source);
    }
}

impl Modifiable for D20Check {
    fn modifiers(&self) -> &ModifierMap {
        &self.modifiers
    }

    fn modifiers_mut(&mut self) -> &mut ModifierMap {
        &mut self.modifiers
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum D20Error {
    KindMismatch {
        check_kind: D20CheckKind,
        dc_kind: D20CheckKind,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct D20CheckResult {
    pub check: D20Check,
    pub rolls: Vec<u8>,
    pub selected_roll: u8,
    pub outcome: Option<D20CheckOutcome>,
    pub crit_threshold: u8,
    pub modifier_result: ModifierResult,
}

impl D20CheckResult {
    pub fn modifiers(&self) -> &ModifierResult {
        &self.modifier_result
    }

    pub fn total_modifier(&self) -> i32 {
        self.modifier_result.total()
    }

    pub fn total(&self) -> i32 {
        max(self.selected_roll as i32 + self.total_modifier(), 0)
    }

    pub fn advantage_tracker(&self) -> &AdvantageTracker {
        &self.check.advantage_tracker
    }

    pub fn is_success(&self, dc: &D20CheckDC) -> bool {
        self.check.matches_kind(&dc.kind()) && self.is_success_vs(dc.total())
    }

    pub fn is_success_vs(&self, dc_total: i32) -> bool {
        if let Some(outcome) = &self.outcome {
            return outcome.is_success();
        }
        self.total() >= dc_total
    }

    pub fn add_modifier<T>(&mut self, source: ModifierSource, value: T)
    where
        T: Into<ModifierKind>,
    {
        let modifier = value.into();
        let result = modifier.evaluate();
        self.check.add_modifier(source.clone(), modifier);
        self.modifier_result.add_modifier_result(source, result);
    }

    pub fn reroll(&self) -> D20CheckResult {
        self.check.roll()
    }

    fn update_roll_based_on_mode(&mut self, original_roll_mode: RollMode, new_roll_mode: RollMode) {
        if new_roll_mode == original_roll_mode {
            return;
        }

        match (new_roll_mode, original_roll_mode) {
            (RollMode::Normal, RollMode::Advantage) => {
                // We had advantage before, but now we don't. Keep the highest roll.
                self.rolls.iter().max().map(|&r| self.selected_roll = r);
                self.rolls = vec![self.selected_roll];
            }
            (RollMode::Normal, RollMode::Disadvantage) => {
                // We had disadvantage before, but now we don't. Keep the lowest roll.
                self.rolls.iter().min().map(|&r| self.selected_roll = r);
                self.rolls = vec![self.selected_roll];
            }
            (RollMode::Advantage, RollMode::Normal) => {
                // We had no advantage before, but now we do. Reroll and keep the highest.
                let mut rng = rand::rng();
                let new_roll = rng.random_range(1..=20) as u8;
                self.rolls.push(new_roll);
                self.selected_roll = *self.rolls.iter().max().unwrap();
            }
            (RollMode::Disadvantage, RollMode::Normal) => {
                // We had no disadvantage before, but now we do. Reroll and keep the lowest.
                let mut rng = rand::rng();
                let new_roll = rng.random_range(1..=20) as u8;
                self.rolls.push(new_roll);
                self.selected_roll = *self.rolls.iter().min().unwrap();
            }
            _ => {}
        }
    }
}

impl AdvantageAware for D20CheckResult {
    fn add_advantage(&mut self, kind: AdvantageType, source: ModifierSource) {
        let original_roll_mode = self.check.advantage_tracker.roll_mode();

        self.check.advantage_tracker_mut().add(kind, source);

        let new_roll_mode = self.check.advantage_tracker().roll_mode();

        self.update_roll_based_on_mode(original_roll_mode, new_roll_mode);
    }

    fn remove_advantage(&mut self, source: &ModifierSource) {
        let original_roll_mode = self.check.advantage_tracker.roll_mode();

        self.check.advantage_tracker_mut().remove(source);

        let new_roll_mode = self.check.advantage_tracker().roll_mode();

        self.update_roll_based_on_mode(original_roll_mode, new_roll_mode);
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
}

impl<K> D20CheckMap<K>
where
    K: D20CheckKey,
{
    pub fn new(kind_mapper: fn(&K) -> D20CheckKind) -> Self {
        let checks = K::iter()
            .map(|k| (k, D20Check::new(kind_mapper(&k), Proficiency::default())))
            .collect();
        Self { checks }
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

    pub fn add_crit_threshold_reduction(&mut self, key: &K, source: ModifierSource, reduction: u8) {
        self.get_mut(key)
            .add_crit_threshold_reduction(source, reduction);
    }

    pub fn remove_crit_threshold_reduction(&mut self, key: &K, source: &ModifierSource) {
        self.get_mut(key).remove_crit_threshold_reduction(source);
    }

    pub fn check(&self, key: &K, game_state: &GameState, entity: Entity) -> D20CheckResult {
        self.get(key).clone().roll_hooks(game_state, entity)
    }
}

impl<K> KeyedModifiable<K> for D20CheckMap<K>
where
    K: D20CheckKey,
{
    type Entry = D20Check;

    fn entry_mut(&mut self, key: &K) -> &mut D20Check {
        self.get_mut(key)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum D20CheckDC {
    SavingThrow {
        saving_throw: SavingThrowKind,
        dc: ModifierResult,
    },
    Skill {
        skill: Skill,
        dc: ModifierResult,
    },
    AttackRoll {
        target: EntityIdentifier,
        source: AttackSource,
        armor_class: ArmorClass,
    },
}

impl D20CheckDC {
    pub fn kind(&self) -> D20CheckKind {
        match self {
            D20CheckDC::SavingThrow { saving_throw, .. } => {
                D20CheckKind::SavingThrow(*saving_throw)
            }
            D20CheckDC::Skill { skill, .. } => D20CheckKind::Skill(*skill),
            D20CheckDC::AttackRoll { source, .. } => D20CheckKind::AttackRoll(*source),
        }
    }

    pub fn total(&self) -> i32 {
        match self {
            D20CheckDC::SavingThrow { dc, .. } => dc.total(),
            D20CheckDC::Skill { dc, .. } => dc.total(),
            D20CheckDC::AttackRoll { armor_class, .. } => armor_class.total(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::components::{id::ItemId, proficiency::ProficiencyLevel};

    use super::*;

    #[test]
    fn d20_check() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
        );
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );
        println!("Check: {:?}", check);
        let result = check.roll();

        // 1d20 + 2
        // Min: 1 + 2 = 3
        // Max: 20 + 2 = 22
        assert!(result.total() >= 3 && result.total() <= 22);
        assert_eq!(result.rolls.len(), 1);
        assert_eq!(result.check.advantage_tracker.roll_mode(), RollMode::Normal);
        println!("Result: {:?}", result);
    }

    #[test]
    fn d20_check_with_advantage() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
        );
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );
        check.advantage_tracker.add(
            AdvantageType::Advantage,
            ModifierSource::Item(ItemId::new("nat20_core", "item.lucky_charm")),
        );
        let result = check.roll();

        // 1d20 + 2
        // Min: 1 + 2 = 3
        // Max: 20 + 2 = 22
        assert!(result.total() >= 3 && result.total() <= 22);
        assert_eq!(result.rolls.len(), 2);
        assert_eq!(
            result.check.advantage_tracker.roll_mode(),
            RollMode::Advantage
        );
        // Check if the selected roll is the maximum
        assert_eq!(
            result.selected_roll,
            result.rolls.iter().max().unwrap().clone()
        );
        println!("Result: {:?}", result);
    }

    #[test]
    fn d20_check_with_disadvantage() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(
                ProficiencyLevel::Expertise,
                ModifierSource::Custom("Somewhere".to_string()),
            ),
        );
        check.advantage_tracker.add(
            AdvantageType::Disadvantage,
            ModifierSource::Item(ItemId::new("nat20_core", "item.cursed_ring")),
        );
        let result = check.roll();

        assert_eq!(result.rolls.len(), 2);
        assert_eq!(
            result.check.advantage_tracker.roll_mode(),
            RollMode::Disadvantage
        );
        // Check if the selected roll is the minimum
        assert_eq!(
            result.selected_roll,
            result.rolls.iter().min().unwrap().clone()
        );
        println!("Result: {:?}", result);
    }

    #[test]
    fn d20_check_with_advantage_and_disadvantage() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(
                ProficiencyLevel::Expertise,
                ModifierSource::Custom("Genetics".to_string()),
            ),
        );
        check.advantage_tracker.add(
            AdvantageType::Advantage,
            ModifierSource::Item(ItemId::new("nat20_core", "item.lucky_charm")),
        );
        check.advantage_tracker.add(
            AdvantageType::Disadvantage,
            ModifierSource::Item(ItemId::new("nat20_core", "item.cursed_ring")),
        );
        let result = check.roll();

        assert_eq!(result.rolls.len(), 1);
        assert_eq!(result.check.advantage_tracker.roll_mode(), RollMode::Normal);
        println!("Result: {:?}", result);
    }

    #[test]
    fn d20_check_critical_success() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
        );
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );
        let mut result = check.roll();
        while result.selected_roll != 20 {
            // Simulate rolling again until we get a critical success
            result = check.roll();
        }

        assert!(matches!(
            result.outcome,
            Some(D20CheckOutcome::CriticalSuccess)
        ));
        println!("Result: {:?}", result);
    }

    #[test]
    fn d20_check_critical_failure() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
        );
        check.modifiers.add_modifier(
            ModifierSource::Item(ItemId::new("nat20_core", "item.ring_of_rolling")),
            2,
        );
        let mut result = check.roll();
        while result.selected_roll != 1 {
            // Simulate rolling again until we get a critical failure
            result = check.roll();
        }

        println!("Result: {:?}", result);
        assert!(matches!(
            result.outcome,
            Some(D20CheckOutcome::CriticalFailure)
        ));
    }

    #[test]
    fn d20_check_success_probability() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
        );
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
            success_probability.min * 100.0
        );

        assert!(success_probability.is_single());
        assert!(success_probability.min == 0.5);
    }

    #[test]
    fn d20_forced_outcome() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
        );
        check.set_forced_outcome(
            ModifierSource::Custom("Test".to_string()),
            D20CheckOutcome::CriticalSuccess,
        );
        let result = check.roll();

        assert!(matches!(
            result.outcome,
            Some(D20CheckOutcome::CriticalSuccess)
        ));
        println!("Result with forced critical success: {:?}", result);
    }

    #[test]
    fn d20_check_crit_threshold_reduction() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
        );
        check.add_crit_threshold_reduction(ModifierSource::Custom("Test".to_string()), 2);

        let mut result = check.roll();
        while result.selected_roll < 18 {
            // Simulate rolling again until we get a critical success with reduced threshold
            result = check.roll();
        }

        assert!(matches!(
            result.outcome,
            Some(D20CheckOutcome::CriticalSuccess)
        ));
    }

    #[test]
    fn d20_check_crit_threshold_reduction_above_20() {
        let mut check = D20Check::new(
            D20CheckKind::Skill(Skill::Athletics),
            Proficiency::new(ProficiencyLevel::Proficient, ModifierSource::None),
        );
        check.add_crit_threshold_reduction(ModifierSource::Custom("Test".to_string()), 100);

        let result = check.roll();

        println!(
            "Result with crit threshold reduction above 20: {:?}",
            result
        );
        assert!(matches!(
            result.outcome,
            Some(D20CheckOutcome::CriticalSuccess)
        ));
    }
}
