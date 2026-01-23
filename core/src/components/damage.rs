use std::{
    collections::HashMap,
    fmt::{self, Display},
    str::FromStr,
};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::ActionContext,
        d20::{D20Check, D20CheckOutcome, D20CheckResult},
        dice::{DiceSet, DiceSetRoll, DiceSetRollResult},
        id::{ActionId, SpellId},
        items::equipment::{
            slots::EquipmentSlot,
            weapon::{Weapon, WeaponKind},
        },
        modifier::{ModifierSet, ModifierSource},
    },
    systems::{self},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DamageType {
    Acid,
    Bludgeoning,
    Cold,
    Fire,
    Force,
    Lightning,
    Necrotic,
    Piercing,
    Poison,
    Psychic,
    Radiant,
    Slashing,
    Thunder,
}

impl fmt::Display for DamageType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// --- DAMAGE APPLICATION ---

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DamageComponent {
    pub dice_roll: DiceSetRoll,
    pub damage_type: DamageType,
}

impl DamageComponent {
    pub fn new(dice: DiceSet, damage_type: DamageType) -> Self {
        Self {
            dice_roll: DiceSetRoll::new(dice, ModifierSet::new()),
            damage_type,
        }
    }
}

impl fmt::Display for DamageComponent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.dice_roll, self.damage_type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DamageComponentResult {
    pub result: DiceSetRollResult,
    pub damage_type: DamageType,
}

impl fmt::Display for DamageComponentResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} ", self.result.subtotal, self.damage_type)?;
        write!(
            f,
            "({} ({}d{})",
            self.result.rolls.iter().sum::<u32>(),
            self.result.rolls.len(),
            self.result.die_size as u32,
        )?;
        if !self.result.modifiers.is_empty() {
            write!(f, " {}", self.result.modifiers)?;
        }
        write!(f, ")")
    }
}

impl Default for DamageComponentResult {
    fn default() -> Self {
        Self {
            result: DiceSetRollResult::default(),
            damage_type: DamageType::Slashing,
        }
    }
}

/// This is used in the attack roll hook so we e.g. only apply Fighting Style
/// Archery when making a ranged attack
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum DamageSource {
    Weapon(WeaponKind),
    Spell(SpellId),
}

impl From<&Weapon> for DamageSource {
    fn from(weapon: &Weapon) -> Self {
        DamageSource::Weapon(weapon.kind().clone())
    }
}

impl From<&ActionContext> for DamageSource {
    fn from(action_context: &ActionContext) -> Self {
        match action_context {
            ActionContext::Spell { id, .. } => DamageSource::Spell(id.clone()),
            ActionContext::Weapon { slot } => match slot {
                EquipmentSlot::MeleeMainHand => DamageSource::Weapon(WeaponKind::Melee),
                EquipmentSlot::MeleeOffHand => DamageSource::Weapon(WeaponKind::Melee),
                EquipmentSlot::RangedMainHand => DamageSource::Weapon(WeaponKind::Ranged),
                EquipmentSlot::RangedOffHand => DamageSource::Weapon(WeaponKind::Ranged),
                _ => panic!("Unsupported equipment slot for DamageSource"),
            },
            _ => panic!("Unsupported ActionContext for DamageSource"),
        }
    }
}

impl TryFrom<String> for DamageSource {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if let Ok(spell_id) = SpellId::try_from(value.clone()) {
            return Ok(DamageSource::Spell(spell_id));
        }
        match value.to_ascii_lowercase().as_str() {
            "melee" => Ok(DamageSource::Weapon(WeaponKind::Melee)),
            "ranged" => Ok(DamageSource::Weapon(WeaponKind::Ranged)),
            _ => Err(format!("Unknown DamageSource: {}", value)),
        }
    }
}

impl Into<String> for DamageSource {
    fn into(self) -> String {
        self.to_string()
    }
}

impl Default for DamageSource {
    fn default() -> Self {
        DamageSource::Weapon(WeaponKind::Melee)
    }
}

impl Display for DamageSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DamageSource::Weapon(kind) => write!(f, "{:?}", kind),
            DamageSource::Spell(spell_id) => write!(f, "{}", spell_id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DamageRoll {
    /// Separate the primary so we know where to apply e.g. ability modifiers
    /// TODO: Or just use index 0 as the primary and have all others be bonuses?
    pub primary: DamageComponent,
    pub bonus: Vec<DamageComponent>,
    pub source: DamageSource,
}

impl DamageRoll {
    pub fn new(dice: DiceSet, damage_type: DamageType, source: DamageSource) -> Self {
        Self {
            primary: DamageComponent::new(dice, damage_type),
            bonus: Vec::new(),
            source,
        }
    }

    pub fn add_bonus(&mut self, dice: DiceSet, damage_type: DamageType) {
        self.bonus.push(DamageComponent::new(dice, damage_type));
    }

    pub fn roll(&self, crit: bool) -> DamageRollResult {
        if crit {
            self.roll_internal(2)
        } else {
            self.roll_internal(1)
        }
    }

    fn roll_internal(&self, repeat: u32) -> DamageRollResult {
        let mut results = Vec::new();
        let mut total = 0;

        let mut damage_components = vec![self.primary.clone()];
        damage_components.extend(self.bonus.iter().cloned());

        for component in damage_components {
            let mut component_dice_roll = component.dice_roll.clone();
            component_dice_roll.dice.num_dice *= repeat;
            let result = component_dice_roll.roll();
            total += result.subtotal;
            results.push(DamageComponentResult {
                damage_type: component.damage_type,
                result,
            });
        }

        DamageRollResult {
            components: results,
            total,
            source: self.source.clone(),
            action: None,
        }
    }

    pub fn min_max_rolls(&self) -> Vec<(i32, i32, DamageType)> {
        let mut results = Vec::new();
        results.push((
            self.primary.dice_roll.min_roll(),
            self.primary.dice_roll.max_roll(),
            self.primary.damage_type.clone(),
        ));
        for comp in &self.bonus {
            results.push((
                comp.dice_roll.min_roll(),
                comp.dice_roll.max_roll(),
                comp.damage_type.clone(),
            ));
        }
        results
    }
}

impl fmt::Display for DamageRoll {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.primary)?;
        for comp in &self.bonus {
            write!(f, " + ({})", comp)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DamageRollResult {
    pub components: Vec<DamageComponentResult>,
    pub source: DamageSource,
    pub total: i32,
    // TODO: I don't think a full `ActionData` is necessary here, so let's just
    // store the actor and action id for now
    pub action: Option<(Entity, ActionId)>,
}

impl DamageRollResult {
    pub fn recalculate_total(&mut self) {
        self.total = self
            .components
            .iter_mut()
            .map(|c| {
                c.result.recalculate_total();
                c.result.subtotal
            })
            .sum();
    }

    pub fn add_component(&mut self, component: DamageComponentResult) {
        self.total += component.result.subtotal;
        self.components.push(component);
    }
}

impl fmt::Display for DamageRollResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.components[0])?;
        for comp in &self.components[1..] {
            write!(f, " + {}", comp)?;
        }
        write!(f, " = {}", self.total)
    }
}

impl Default for DamageRollResult {
    fn default() -> Self {
        Self {
            components: vec![DamageComponentResult::default()],
            total: 0,
            source: DamageSource::Weapon(WeaponKind::Melee),
            action: None,
        }
    }
}

/// --- DAMAGE MITIGATION ---

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum MitigationOperation {
    Resistance,         // divide by 2
    Vulnerability,      // multiply by 2
    Immunity,           // set to 0
    FlatReduction(i32), // subtract N
}

impl MitigationOperation {
    fn apply(&self, value: i32) -> i32 {
        match self {
            MitigationOperation::Resistance => value / 2,
            MitigationOperation::Vulnerability => value * 2,
            MitigationOperation::Immunity => 0,
            MitigationOperation::FlatReduction(amount) => (value - amount).max(0),
        }
    }

    fn priority(&self) -> u8 {
        match self {
            MitigationOperation::Immunity => 0,
            MitigationOperation::FlatReduction(_) => 1,
            MitigationOperation::Resistance => 2,
            MitigationOperation::Vulnerability => 3,
        }
    }
}

impl fmt::Display for MitigationOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MitigationOperation::Resistance => write!(f, "/ 2"),
            MitigationOperation::Vulnerability => write!(f, "* 2"),
            MitigationOperation::Immunity => write!(f, "* 0"),
            MitigationOperation::FlatReduction(amount) => write!(f, "- {}", amount),
        }
    }
}

impl FromStr for MitigationOperation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "resistance" => Ok(MitigationOperation::Resistance),
            "vulnerability" => Ok(MitigationOperation::Vulnerability),
            "immunity" => Ok(MitigationOperation::Immunity),
            _ if s.starts_with("flat_reduction") => {
                let start = s
                    .find('(')
                    .ok_or_else(|| format!("Invalid FlatReduction format, missing '(': {}", s))?;
                let end = s
                    .find(')')
                    .ok_or_else(|| format!("Invalid FlatReduction format, missing ')': {}", s))?;
                let amount_str = &s[start + 1..end];
                let amount: i32 = amount_str
                    .parse()
                    .map_err(|_| format!("Invalid FlatReduction amount '{}'", amount_str))?;
                Ok(MitigationOperation::FlatReduction(amount))
            }
            _ => Err(format!("Unknown MitigationOperation: {}", s)),
        }
    }
}

impl TryFrom<String> for MitigationOperation {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<MitigationOperation> for String {
    fn from(op: MitigationOperation) -> Self {
        match op {
            MitigationOperation::Resistance => "resistance".to_string(),
            MitigationOperation::Vulnerability => "vulnerability".to_string(),
            MitigationOperation::Immunity => "immunity".to_string(),
            MitigationOperation::FlatReduction(amount) => {
                format!("flat_reduction({})", amount)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DamageMitigationEffect {
    pub source: ModifierSource,
    pub operation: MitigationOperation,
}

#[derive(Debug, Clone)]
pub struct DamageResistances {
    pub effects: HashMap<DamageType, Vec<DamageMitigationEffect>>,
}

impl DamageResistances {
    pub fn new() -> Self {
        Self {
            effects: HashMap::new(),
        }
    }

    pub fn add_effect(&mut self, damage_type: DamageType, effect: DamageMitigationEffect) {
        self.effects
            .entry(damage_type)
            .or_insert_with(Vec::new)
            .push(effect);
    }

    pub fn remove_effect(&mut self, damage_type: DamageType, effect: &DamageMitigationEffect) {
        if let Some(effects) = self.effects.get_mut(&damage_type) {
            effects.retain(|e| e != effect);
            if effects.is_empty() {
                self.effects.remove(&damage_type);
            }
        }
    }

    pub fn effective_resistance(&self, damage_type: DamageType) -> Option<DamageMitigationEffect> {
        self.effects.get(&damage_type).and_then(|effects| {
            effects
                .iter()
                .min_by_key(|e| e.operation.priority())
                .cloned()
        })
    }

    pub fn apply(&self, roll: &DamageRollResult) -> DamageMitigationResult {
        let mut components = Vec::new();
        let mut total = 0;

        for comp in &roll.components {
            let damage_type = comp.damage_type;
            let mut value = comp.result.subtotal;
            let mut applied_mods = Vec::new();

            if let Some(effects) = self.effects.get(&damage_type) {
                // Sort by priority
                let mut sorted_effects = effects.clone();
                sorted_effects.sort_by_key(|e| e.operation.priority());

                for effect in sorted_effects {
                    value = effect.operation.apply(value);
                    applied_mods.push(effect);
                    if value <= 0 {
                        break;
                    }
                }
            }

            total += value;
            components.push(DamageComponentMitigation {
                damage_type,
                original: comp.result.clone(),
                after_mods: value,
                modifiers: applied_mods,
            });
        }

        DamageMitigationResult {
            components,
            total,
            source: roll.source.clone(),
            action: roll.action.clone(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.effects.is_empty()
    }
}

impl Default for DamageResistances {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for DamageResistances {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.effects.is_empty() {
            return write!(f, "No resistances");
        }
        for (damage_type, effects) in &self.effects {
            write!(f, "{}: ", damage_type)?;
            for effect in effects {
                write!(f, "{}, ", effect.operation)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DamageComponentMitigation {
    pub damage_type: DamageType,
    pub original: DiceSetRollResult,
    pub after_mods: i32,
    /// Sorted by priority
    pub modifiers: Vec<DamageMitigationEffect>,
}

impl DamageComponentMitigation {
    pub fn recalculate_total(&mut self) {
        let mut value = self.original.subtotal;
        for modifier in &self.modifiers {
            value = modifier.operation.apply(value);
        }
        self.after_mods = value;
    }
}

impl fmt::Display for DamageComponentMitigation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.original.subtotal == self.after_mods {
            return write!(f, "{} {}", self.original.subtotal, self.damage_type);
        }
        if self.after_mods == 0 {
            return write!(
                f,
                "0 {} ({:?})",
                self.damage_type,
                MitigationOperation::Immunity
            );
        }
        write!(f, "{} {} ", self.after_mods, self.damage_type)?;
        let mut amount = self.original.subtotal.to_string();
        for modifier in &self.modifiers {
            let explanation = match modifier.operation {
                MitigationOperation::FlatReduction(_) => format!("{}", modifier.source),
                _ => format!("{:?}", modifier.operation),
            };
            amount = format!("({} {} ({}))", amount, modifier.operation, explanation);
        }
        write!(f, "{}", amount)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DamageMitigationResult {
    pub components: Vec<DamageComponentMitigation>,
    pub source: DamageSource,
    pub total: i32,
    pub action: Option<(Entity, ActionId)>,
}

impl DamageMitigationResult {
    pub fn recalculate_total(&mut self) {
        self.total = 0;
        for comp in &mut self.components {
            comp.recalculate_total();
            self.total += comp.after_mods;
        }
    }

    pub fn add_component(&mut self, component: DamageComponentMitigation) {
        self.total += component.after_mods;
        self.components.push(component);
    }
}

impl fmt::Display for DamageMitigationResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.components[0])?;
        for comp in &self.components[1..] {
            write!(f, " + {}", comp)?;
        }
        write!(f, " = {}", self.total)
    }
}

impl Default for DamageMitigationResult {
    fn default() -> Self {
        Self {
            components: Vec::new(),
            total: 0,
            source: DamageSource::default(),
            action: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AttackSource {
    Weapon,
    Spell,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AttackRange {
    Melee,
    Ranged,
}

#[derive(Debug, Clone)]
pub struct AttackRoll {
    pub d20_check: D20Check,
    pub source: AttackSource,
    pub range: AttackRange,
    crit_threshold: u8, // Default critical threshold is 20
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttackRollResult {
    pub roll_result: D20CheckResult,
    pub source: AttackSource,
    pub range: AttackRange,
}

impl fmt::Display for AttackRollResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: Include source information?
        write!(f, "{}", self.roll_result)
    }
}

impl AttackRoll {
    pub fn new(d20_check: D20Check, source: AttackSource, range: AttackRange) -> Self {
        Self {
            d20_check,
            source,
            range,
            crit_threshold: 20,
        }
    }

    // TODO: Track the source of the crit threshold reduction?
    pub fn reduce_crit_threshold(&mut self, amount: u8) {
        if amount > self.crit_threshold {
            self.crit_threshold = 1; // Minimum crit threshold is 1
        } else {
            self.crit_threshold -= amount;
        }
    }

    pub fn roll_raw(&self, proficiency_bonus: u8) -> AttackRollResult {
        let mut roll_result = self.d20_check.roll(proficiency_bonus);
        if roll_result.selected_roll >= self.crit_threshold {
            roll_result.outcome = Some(D20CheckOutcome::CriticalSuccess);
        }

        AttackRollResult {
            roll_result,
            range: self.range,
            source: self.source,
        }
    }

    pub fn hit_chance(&self, world: &World, entity: Entity, target_ac: u32) -> f64 {
        self.d20_check.success_probability(
            target_ac,
            systems::helpers::level(world, entity)
                .unwrap()
                .proficiency_bonus(),
        )
    }
}

#[cfg(test)]
mod tests {
    use rstest::{fixture, rstest};

    use crate::components::{
        ability::Ability,
        dice::DieSize,
        id::{EffectId, ItemId},
        modifier::{Modifiable, ModifierSet, ModifierSource},
    };

    use super::*;

    #[rstest]
    fn damage_roll_values(damage_roll: DamageRoll) {
        println!("Roll: {}", damage_roll);
        let result = damage_roll.roll(false);
        assert_eq!(result.components.len(), 2);
        // 2d6 + 1d4 + 2 (str mod)
        // Min roll: 2 + 1 + 2 = 5
        // Max roll: 12 + 4 + 2 = 18
        assert!(result.total >= 5 && result.total <= 18);
        println!("Roll result:{}", result);
    }

    #[rstest]
    fn damage_roll_crit(damage_roll: DamageRoll) {
        println!("Roll: {}", damage_roll);
        let result = damage_roll.roll(true);
        assert_eq!(result.components.len(), 2);
        // 4d6 (2 * 2d6) + 2d4 (2 * 1d4) + 2 (str mod)
        // Min roll: 4 + 2 + 2 = 8
        // Max roll: 24 + 8 + 2 = 34
        assert!(result.total >= 8 && result.total <= 34);
        println!("Roll result: {}", result);
    }

    #[rstest]
    fn damage_mitigation_resistance(damage_roll_result: DamageRollResult) {
        let mut resistances = DamageResistances {
            effects: HashMap::new(),
        };
        resistances.effects.insert(
            DamageType::Slashing,
            vec![DamageMitigationEffect {
                source: ModifierSource::Item(ItemId::new(
                    "nat20_core",
                    "item.shield_of_resistance",
                )),
                operation: MitigationOperation::Resistance,
            }],
        );

        let mitigation_result = resistances.apply(&damage_roll_result);
        // 7 / 2 + 2 = 3.5
        // 3.5 + 2 = 5.5 -> round down to 5
        assert_eq!(mitigation_result.total, 5);
        println!("{}", mitigation_result);
    }

    #[rstest]
    fn damage_mitigation_immunity(damage_roll_result: DamageRollResult) {
        let mut resistances = DamageResistances {
            effects: HashMap::new(),
        };
        resistances.effects.insert(
            DamageType::Fire,
            vec![DamageMitigationEffect {
                source: ModifierSource::Item(ItemId::new(
                    "nat20_core",
                    "item.ring_of_fire_immunity",
                )),
                operation: MitigationOperation::Immunity,
            }],
        );

        let mitigation_result = resistances.apply(&damage_roll_result);
        // 7 + 0 = 7
        assert_eq!(mitigation_result.total, 7);
        println!("{}", mitigation_result);
    }

    #[rstest]
    fn damage_mitigation_vulnerability(damage_roll_result: DamageRollResult) {
        let mut resistances = DamageResistances {
            effects: HashMap::new(),
        };
        resistances.effects.insert(
            DamageType::Slashing,
            vec![DamageMitigationEffect {
                source: ModifierSource::Item(ItemId::new(
                    "nat20_core",
                    "item.shield_of_vulnerability",
                )),
                operation: MitigationOperation::Vulnerability,
            }],
        );

        let mitigation_result = resistances.apply(&damage_roll_result);
        // 7 * 2 + 2 = 16
        assert_eq!(mitigation_result.total, 16);
        println!("{}", mitigation_result);
    }

    #[rstest]
    fn damage_mitigation_flat_reduction(damage_roll_result: DamageRollResult) {
        let mut resistances = DamageResistances {
            effects: HashMap::new(),
        };
        resistances.effects.insert(
            DamageType::Slashing,
            vec![DamageMitigationEffect {
                source: ModifierSource::Item(ItemId::new(
                    "nat20_core",
                    "item.shield_of_flat_reduction",
                )),
                operation: MitigationOperation::FlatReduction(3),
            }],
        );

        let mitigation_result = resistances.apply(&damage_roll_result);
        // 7 - 3 + 2 = 6
        assert_eq!(mitigation_result.total, 6);
        println!("{}", mitigation_result);
    }

    #[rstest]
    fn damage_mitigation_multiple_effects(damage_roll_result: DamageRollResult) {
        let mut resistances = DamageResistances {
            effects: HashMap::new(),
        };
        resistances.effects.insert(
            DamageType::Slashing,
            vec![
                DamageMitigationEffect {
                    source: ModifierSource::Item(ItemId::new(
                        "nat20_core",
                        "item.shield_of_resistance",
                    )),
                    operation: MitigationOperation::Resistance,
                },
                DamageMitigationEffect {
                    source: ModifierSource::Item(ItemId::new(
                        "nat20_core",
                        "item.shield_of_flat_reduction",
                    )),
                    operation: MitigationOperation::FlatReduction(3),
                },
            ],
        );

        let mitigation_result = resistances.apply(&damage_roll_result);
        // Slashing: (7 - 3) / 2 = 2
        // 2 Slashing + 2 Fire = 4
        assert_eq!(mitigation_result.total, 4);
        println!("{}", mitigation_result);
    }

    #[rstest]
    fn damage_mitigation_multiple_types(damage_roll_result: DamageRollResult) {
        let mut resistances = DamageResistances {
            effects: HashMap::new(),
        };
        resistances.effects.insert(
            DamageType::Slashing,
            vec![DamageMitigationEffect {
                source: ModifierSource::Item(ItemId::new(
                    "nat20_core",
                    "item.shield_of_resistance",
                )),
                operation: MitigationOperation::Resistance,
            }],
        );
        resistances.effects.insert(
            DamageType::Fire,
            vec![DamageMitigationEffect {
                source: ModifierSource::Item(ItemId::new(
                    "nat20_core",
                    "item.ring_of_fire_immunity",
                )),
                operation: MitigationOperation::Immunity,
            }],
        );

        let mitigation_result = resistances.apply(&damage_roll_result);
        // Slashing: 7 / 2 = 3.5 -> round down to 3
        // Fire: 2 * 0 = 0
        assert_eq!(mitigation_result.total, 3);
        println!("{}", mitigation_result);
    }

    #[rstest]
    fn damage_mitigation_immunity_priority(damage_roll_result: DamageRollResult) {
        let mut resistances = DamageResistances {
            effects: HashMap::new(),
        };
        resistances.effects.insert(
            DamageType::Slashing,
            vec![
                DamageMitigationEffect {
                    source: ModifierSource::Item(ItemId::new(
                        "nat20_core",
                        "item.shield_of_resistance",
                    )),
                    operation: MitigationOperation::Resistance,
                },
                DamageMitigationEffect {
                    source: ModifierSource::Effect(EffectId::new(
                        "nat20_core",
                        "Curse of Slashing",
                    )),
                    operation: MitigationOperation::Vulnerability,
                },
                DamageMitigationEffect {
                    source: ModifierSource::Item(ItemId::new(
                        "nat20_core",
                        "item.ring_of_slashing_immunity",
                    )),
                    operation: MitigationOperation::Immunity,
                },
            ],
        );

        let mitigation_result = resistances.apply(&damage_roll_result);
        // Slashing immunity takes priority
        println!("{}", mitigation_result);
        // 7 * 0 = 0 slashing
        // 0 slashing + 2 fire = 2
        assert_eq!(mitigation_result.total, 2);
        assert_eq!(mitigation_result.components.len(), 2);
    }

    #[rstest]
    fn damage_mitigation_flat_reduction_and_resistance(damage_roll_result: DamageRollResult) {
        let mut resistances = DamageResistances {
            effects: HashMap::new(),
        };
        resistances.effects.insert(
            DamageType::Slashing,
            vec![
                DamageMitigationEffect {
                    source: ModifierSource::Item(ItemId::new(
                        "nat20_core",
                        "item.shield_of_resistance",
                    )),
                    operation: MitigationOperation::Resistance,
                },
                DamageMitigationEffect {
                    source: ModifierSource::Item(ItemId::new(
                        "nat20_core",
                        "item.shield_of_flat_reduction",
                    )),
                    operation: MitigationOperation::FlatReduction(3),
                },
            ],
        );

        let mitigation_result = resistances.apply(&damage_roll_result);
        // Slashing: (7 - 3) / 2 = 2
        // 2 Slashing + 2 Fire = 4
        assert_eq!(mitigation_result.total, 4);
        println!("{}", mitigation_result);
    }

    // TODO: Find a better way to test this
    // #[test]
    // fn attack_roll_crit_threshold() {
    //     // Character is a level 5 Champion Fighter, so crit threshold is 19 (Improved Critical)
    //     let character = fixtures::creatures::heroes::fighter();

    //     // TODO: This is a pretty crazy complicated way to get the attack roll
    //     let attack_action = character
    //         .available_actions()
    //         .iter()
    //         .find(|(action, context)| {
    //             **context
    //                 == ActionContext::Weapon {
    //                     weapon_type: WeaponType::Melee,
    //                     hand: HandSlot::Main,
    //                 }
    //         })
    //         .unwrap();

    //     let attack_roll = match &attack_action.0.kind {
    //         ActionKind::AttackRollDamage { attack_roll, .. } => {
    //             attack_roll(&World, Entity, &attack_action.1.clone())
    //         }
    //         _ => panic!("Expected AttackRollDamage action"),
    //     };

    //     // TODO: This is a pretty hacky way to test this
    //     let mut attack_roll_result = attack_roll.roll(&World, Entity);
    //     while attack_roll_result.roll_result.selected_roll != 19 {
    //         // Keep rolling until we get a 19 (could also check for 20, but that's always a crit, so doesn't
    //         // really test the reduced crit threshold)
    //         attack_roll_result = attack_roll.roll(&World, Entity);
    //     }

    //     assert_eq!(attack_roll_result.roll_result.selected_roll, 19);
    //     assert!(attack_roll_result.roll_result.is_crit);
    //     assert_eq!(
    //         attack_roll_result.source,
    //         DamageSource::Weapon(
    //             WeaponType::Melee,
    //             HashSet::from([WeaponProperties::TwoHanded])
    //         )
    //     );
    // }

    #[fixture]
    fn damage_roll() -> DamageRoll {
        let mut modifiers = ModifierSet::new();
        modifiers.add_modifier(ModifierSource::Ability(Ability::Strength), 2);
        DamageRoll {
            primary: DamageComponent {
                dice_roll: DiceSetRoll::new(
                    DiceSet {
                        num_dice: 2,
                        die_size: DieSize::D6,
                    },
                    modifiers,
                ),
                damage_type: DamageType::Slashing,
            },
            bonus: vec![DamageComponent {
                dice_roll: DiceSetRoll::new(
                    DiceSet {
                        num_dice: 1,
                        die_size: DieSize::D4,
                    },
                    ModifierSet::new(),
                ),
                damage_type: DamageType::Fire,
            }],
            source: DamageSource::Weapon(WeaponKind::Melee),
        }
    }

    #[fixture]
    fn damage_roll_result() -> DamageRollResult {
        DamageRollResult {
            components: vec![
                DamageComponentResult {
                    damage_type: DamageType::Slashing,
                    result: DiceSetRollResult {
                        rolls: vec![3, 4],
                        die_size: DieSize::D6,
                        modifiers: ModifierSet::new(),
                        subtotal: 7,
                    },
                },
                DamageComponentResult {
                    damage_type: DamageType::Fire,
                    result: DiceSetRollResult {
                        rolls: vec![2],
                        die_size: DieSize::D4,
                        modifiers: ModifierSet::new(),
                        subtotal: 2,
                    },
                },
            ],
            total: 9,
            source: DamageSource::Weapon(WeaponKind::Melee),
            action: None,
        }
    }
}
