use std::{
    collections::HashMap,
    fmt::{self, Display},
    str::FromStr,
};

use hecs::{Entity, World};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{ActionAttackKind, ActionContext},
        d20::{D20Check, D20CheckOutcome, D20CheckResult},
        id::{ActionId, SpellId},
        items::equipment::{
            armor::ArmorClass,
            weapon::{Weapon, WeaponKind},
        },
        modifier::{
            FlatModifiable, Modifiable, ModifierKind, ModifierKindResult, ModifierMap,
            ModifierResult, ModifierSource,
        },
        proficiency::Proficiency,
        range::Range,
    },
    registry::serialize::schema::impl_string_schema,
    systems::{self},
};

pub const CRIT_DICE_MULTIPLIER: u32 = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize, JsonSchema)]
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
    #[default]
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
    damage: ModifierMap,
    pub damage_type: DamageType,
}

impl DamageComponent {
    pub fn new(damage: impl Into<ModifierMap>, damage_type: DamageType) -> Self {
        Self {
            damage: damage.into(),
            damage_type,
        }
    }

    pub fn evaluate(&self) -> DamageComponentResult {
        DamageComponentResult {
            result: self.damage.evaluate(),
            damage_type: self.damage_type.clone(),
        }
    }
}

impl Modifiable for DamageComponent {
    fn modifiers(&self) -> &ModifierMap {
        &self.damage
    }

    fn modifiers_mut(&mut self) -> &mut ModifierMap {
        &mut self.damage
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DamageComponentResult {
    pub result: ModifierResult,
    pub damage_type: DamageType,
}

/// This is used in the attack roll hook so we e.g. only apply Fighting Style
/// Archery when making a ranged attack
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum DamageSource {
    Weapon(WeaponKind),
    Spell(SpellId),
    Environmental,
}

impl From<&Weapon> for DamageSource {
    fn from(weapon: &Weapon) -> Self {
        DamageSource::Weapon(weapon.kind().clone())
    }
}

impl From<&ActionContext> for DamageSource {
    fn from(action_context: &ActionContext) -> Self {
        if let Some(spell) = &action_context.spell {
            return DamageSource::Spell(spell.id.clone());
        }

        if let Some(attack) = &action_context.attack {
            return match attack.kind {
                ActionAttackKind::MeleeWeapon => DamageSource::Weapon(WeaponKind::Melee),
                ActionAttackKind::Unarmed => DamageSource::Weapon(WeaponKind::Unarmed),
                ActionAttackKind::RangedWeapon => DamageSource::Weapon(WeaponKind::Ranged),
            };
        }

        panic!("Unsupported ActionContext for DamageSource");
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
            "unarmed" => Ok(DamageSource::Weapon(WeaponKind::Unarmed)),
            _ => Err(format!("Unknown DamageSource: {}", value)),
        }
    }
}

impl Into<String> for DamageSource {
    fn into(self) -> String {
        self.to_string()
    }
}

impl_string_schema!(
    DamageSource,
    "DamageSource",
    "description": "The source of damage: `melee`, `ranged`, `unarmed` or a spell id.",
    "examples": ["melee", "nat20_core::spell.fireball"]
);

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
            DamageSource::Environmental => write!(f, "Environmental"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DamageRoll {
    pub components: Vec<DamageComponent>,
    pub source: DamageSource,
}

impl DamageRoll {
    pub fn new(damage: ModifierMap, damage_type: DamageType, source: DamageSource) -> Self {
        Self {
            components: vec![DamageComponent::new(damage, damage_type)],
            source,
        }
    }

    pub fn add_component(&mut self, damage: ModifierMap, damage_type: DamageType) {
        self.components
            .push(DamageComponent::new(damage, damage_type));
    }

    pub fn roll(&self, crit: bool) -> DamageRollResult {
        let mut results = Vec::new();
        let mut total = 0;

        let dice_multiplier = if crit { CRIT_DICE_MULTIPLIER } else { 1 };

        for component in &self.components {
            let mut result = ModifierResult::default();
            for (source, modifier) in component.damage.iter() {
                let evaluated = evaluate_modifier_dice_multiplier(modifier, dice_multiplier);
                result.add_modifier_result(source.clone(), evaluated);
            }
            total += result.total();
            results.push(DamageComponentResult {
                result,
                damage_type: component.damage_type.clone(),
            });
        }

        DamageRollResult {
            components: results,
            total,
            source: self.source.clone(),
            action: None,
            crit,
        }
    }

    pub fn min_max_rolls(&self) -> Vec<(i32, i32, DamageType)> {
        self.components
            .iter()
            .map(|component| {
                let min = component
                    .damage
                    .values()
                    .map(|modifier| modifier.min())
                    .sum();
                let max = component
                    .damage
                    .values()
                    .map(|modifier| modifier.max())
                    .sum();
                (min, max, component.damage_type.clone())
            })
            .collect()
    }
}

fn evaluate_modifier_dice_multiplier(
    modifier: &ModifierKind,
    multiplier: u32,
) -> ModifierKindResult {
    match modifier {
        ModifierKind::Flat(value) => ModifierKindResult::Flat(*value),
        ModifierKind::Dice(dice_set) => {
            let mut new_dice_set = dice_set.clone();
            new_dice_set.num_dice *= multiplier;
            ModifierKindResult::Dice(new_dice_set.roll())
        }
        ModifierKind::Composite(kinds) => {
            let results = kinds
                .iter()
                .map(|kind| evaluate_modifier_dice_multiplier(kind, multiplier))
                .collect();
            ModifierKindResult::Composite(results)
        }
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
    pub crit: bool,
}

impl DamageRollResult {
    pub fn recalculate_total(&mut self) {
        self.total = self.components.iter_mut().map(|c| c.result.total()).sum();
    }

    pub fn add_component(&mut self, mut component: DamageComponent) {
        for (_, modifier) in component.damage.iter_mut() {
            if let ModifierKind::Dice(dice_set) = modifier {
                if self.crit {
                    dice_set.num_dice *= CRIT_DICE_MULTIPLIER;
                }
            }
        }
        let component = component.evaluate();
        self.components.push(component);
    }
}

impl Default for DamageRollResult {
    fn default() -> Self {
        Self {
            components: vec![DamageComponentResult::default()],
            total: 0,
            source: DamageSource::Weapon(WeaponKind::Melee),
            action: None,
            crit: false,
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
            let mut value = comp.result.total();
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
    pub original: ModifierResult,
    pub after_mods: i32,
    /// Sorted by priority
    pub modifiers: Vec<DamageMitigationEffect>,
}

impl DamageComponentMitigation {
    pub fn recalculate_total(&mut self) {
        let mut value = self.original.total();
        for modifier in &self.modifiers {
            value = modifier.operation.apply(value);
        }
        self.after_mods = value;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum AttackSource {
    Weapon(WeaponKind),
    Spell,
}

#[derive(Debug, Clone)]
pub struct AttackRollTemplate {
    pub d20_check: D20Check,
}

impl AttackRollTemplate {
    pub fn new(d20_check: D20Check) -> Self {
        Self { d20_check }
    }

    pub fn apply_to_roll(&self, attack_roll: &mut AttackRoll) {
        attack_roll
            .d20_check
            .modifiers_mut()
            .add_modifier_map(self.d20_check.modifiers());

        for (source, advantage) in self.d20_check.advantage_tracker().summary() {
            attack_roll
                .d20_check
                .advantage_tracker_mut()
                .add(advantage, source.clone());
        }

        attack_roll
            .d20_check
            .crit_threshold_reduction_mut()
            .add_modifier_map(self.d20_check.crit_threshold_reduction());

        if let Some((source, forced_outcome)) = self.d20_check.forced_outcome() {
            attack_roll
                .d20_check
                .set_forced_outcome(source.clone(), forced_outcome.clone());
        }
    }

    pub fn instantiate(&self, source: AttackSource) -> AttackRoll {
        AttackRoll::new(self.d20_check.clone(), source)
    }
}

impl Default for AttackRollTemplate {
    fn default() -> Self {
        Self {
            d20_check: D20Check::new(Proficiency::default()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AttackRoll {
    pub d20_check: D20Check,
    pub source: AttackSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttackRollResult {
    pub roll_result: D20CheckResult,
    pub source: AttackSource,
}

impl AttackRollResult {
    pub fn is_success(&self, armor_class: &ArmorClass) -> bool {
        if let Some(outcome) = &self.roll_result.outcome {
            return outcome.is_success();
        }
        return self.roll_result.total() >= armor_class.total() as u32;
    }

    pub fn is_crit(&self) -> bool {
        matches!(
            self.roll_result.outcome,
            Some(D20CheckOutcome::CriticalSuccess)
        )
    }
}

impl AttackRoll {
    pub fn new(d20_check: D20Check, source: AttackSource) -> Self {
        Self { d20_check, source }
    }

    pub fn roll_raw(&self, proficiency_bonus: u8) -> AttackRollResult {
        let roll_result = self.d20_check.roll(proficiency_bonus);

        AttackRollResult {
            roll_result,
            source: self.source,
        }
    }

    pub fn hit_chance(&self, world: &World, entity: Entity, target_ac: u32) -> Range<f32> {
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
        dice::{DiceSet, DiceSetResult, DieSize},
        id::{EffectId, ItemId},
        modifier::ModifierSource,
    };

    use super::*;

    #[rstest]
    fn damage_roll_values(damage_roll: DamageRoll) {
        println!("Roll: {:?}", damage_roll);
        let result = damage_roll.roll(false);
        assert_eq!(result.components.len(), 2);
        // 2d6 + 1d4 + 2 (str mod)
        // Min roll: 2 + 1 + 2 = 5
        // Max roll: 12 + 4 + 2 = 18
        assert!(result.total >= 5 && result.total <= 18);
        println!("Roll result:{:?}", result);
    }

    #[rstest]
    fn damage_roll_crit(damage_roll: DamageRoll) {
        println!("Roll: {:?}", damage_roll);
        let result = damage_roll.roll(true);
        assert_eq!(result.components.len(), 2);
        // 4d6 (2 * 2d6) + 2d4 (2 * 1d4) + 2 (str mod)
        // Min roll: 4 + 2 + 2 = 8
        // Max roll: 24 + 8 + 2 = 34
        assert!(result.total >= 8 && result.total <= 34);
        println!("Roll result: {:?}", result);
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
        // (7 + 2) / 2 = 4.5
        // 4.5 + 2 = 4.5 -> round down to 6
        assert_eq!(mitigation_result.total, 6);
        println!("{:?}", mitigation_result);
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
        // 7 + 2 (Slashing) + 0 (Fire) = 9
        assert_eq!(mitigation_result.total, 9);
        println!("{:?}", mitigation_result);
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
        // (7 + 2) * 2 + 2 = 20
        assert_eq!(mitigation_result.total, 20);
        println!("{:?}", mitigation_result);
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
        // (7 + 2) - 3 Slashing + 2 Fire = 8
        assert_eq!(mitigation_result.total, 8);
        println!("{:?}", mitigation_result);
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
        // Slashing: (7 + 2 - 3) / 2 = 3
        // 3 Slashing + 2 Fire = 5
        assert_eq!(mitigation_result.total, 5);
        println!("{:?}", mitigation_result);
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
        // Slashing: (7 + 2) / 2 = 4.5 -> round down to 4
        // Fire: 2 * 0 = 0
        assert_eq!(mitigation_result.total, 4);
        println!("{:?}", mitigation_result);
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
        println!("{:?}", mitigation_result);
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
        // Slashing: (7 + 2 - 3) / 2 = 3
        // 3 Slashing + 2 Fire = 5
        assert_eq!(mitigation_result.total, 5);
        println!("{:?}", mitigation_result);
    }

    #[fixture]
    fn damage_roll() -> DamageRoll {
        DamageRoll {
            components: vec![
                DamageComponent {
                    damage: ModifierMap::from_iter(vec![
                        (
                            ModifierSource::Base,
                            ModifierKind::Dice(DiceSet {
                                num_dice: 2,
                                die_size: DieSize::D6,
                            }),
                        ),
                        (
                            ModifierSource::Ability(Ability::Strength),
                            ModifierKind::Flat(2),
                        ),
                    ]),
                    damage_type: DamageType::Slashing,
                },
                DamageComponent {
                    damage: ModifierMap::from(
                        ModifierSource::Base,
                        ModifierKind::Dice(DiceSet {
                            num_dice: 1,
                            die_size: DieSize::D4,
                        }),
                    ),
                    damage_type: DamageType::Fire,
                },
            ],
            source: DamageSource::Weapon(WeaponKind::Melee),
        }
    }

    #[fixture]
    fn damage_roll_result() -> DamageRollResult {
        DamageRollResult {
            components: vec![
                DamageComponentResult {
                    damage_type: DamageType::Slashing,
                    result: ModifierResult::from_iter(vec![
                        (
                            ModifierSource::Base,
                            ModifierKindResult::Dice(DiceSetResult::new(
                                DiceSet {
                                    num_dice: 2,
                                    die_size: DieSize::D6,
                                },
                                vec![3, 4],
                            )),
                        ),
                        (
                            ModifierSource::Ability(Ability::Strength),
                            ModifierKindResult::Flat(2),
                        ),
                    ]),
                },
                DamageComponentResult {
                    damage_type: DamageType::Fire,
                    result: ModifierResult::from_iter(vec![(
                        ModifierSource::Base,
                        ModifierKindResult::Dice(DiceSetResult::new(
                            DiceSet {
                                num_dice: 1,
                                die_size: DieSize::D4,
                            },
                            vec![2],
                        )),
                    )]),
                },
            ],
            total: 11,
            source: DamageSource::Weapon(WeaponKind::Melee),
            action: None,
            crit: false,
        }
    }
}
