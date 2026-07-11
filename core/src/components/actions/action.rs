use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    sync::{Arc, Mutex},
};

use hecs::{Entity, World};
use kinded::Kinded;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::{
            execution::PhaseState,
            targeting::{AreaShape, TargetInstance, TargetingContext},
        },
        d20::D20CheckResult,
        damage::{
            AttackRoll, AttackRollResult, DamageMitigationResult, DamageRoll, DamageRollResult,
        },
        dice::{DiceSetRoll, DiceSetRollResult},
        effects::effect::EffectInstanceTemplate,
        health::life_state::LifeState,
        id::{ActionId, EffectId, EntityIdentifier, IdProvider, ResourceId, SpellId},
        items::equipment::{armor::ArmorClass, slots::EquipmentSlot},
        resource::{RechargeRule, ResourceAmountMap},
        saving_throw::{SavingThrowDC, SavingThrowKind},
        spells::spellbook::SpellSource,
    },
    engine::{action_prompt::ActionData, event::Event, game_state::GameState},
    entities::projectile::ProjectileTemplate,
    registry::{registry::ActionsRegistry, serialize::action::ActionDefinition},
    systems::{
        self,
        geometry::{Displacement, DisplacementTemplate},
    },
};

pub type DamageFunction = dyn Fn(&World, Entity, &ActionContext) -> DamageRoll + Send + Sync;
pub type AttackRollFunction =
    dyn Fn(&World, Entity, Entity, &ActionContext) -> AttackRoll + Send + Sync;
pub type SavingThrowFunction =
    dyn Fn(&World, Entity, &ActionContext) -> SavingThrowDC + Send + Sync;
pub type HealingFunction = dyn Fn(&World, Entity, &ActionContext) -> DiceSetRoll + Send + Sync;
pub type TargetingFunction =
    dyn Fn(&World, Entity, &ActionContext) -> TargetingContext + Send + Sync;
pub type ReactionTriggerFunction = dyn Fn(&GameState, &Entity, &Event) -> bool + Send + Sync;
pub type ReactionBodyFunction =
    dyn Fn(&mut GameState, &ActionData, &mut Event) -> Option<ReactionResult> + Send + Sync;
pub type DisplacementFunction =
    dyn Fn(&World, Entity, &ActionContext) -> DisplacementTemplate + Send + Sync;
pub type AreaShapeFunction = dyn Fn(&World, Entity, &ActionContext) -> AreaShape + Send + Sync;

#[derive(Clone, Deserialize)]
#[serde(from = "ActionDefinition")]
pub struct Action {
    pub id: ActionId,
    pub description: String,
    pub kind: ActionKind,
    pub targeting: Arc<TargetingFunction>,
    /// e.g. Action, Bonus Action, Reaction
    pub resource_cost: ResourceAmountMap,
    /// Optional cooldown for the action
    pub cooldown: Option<RechargeRule>,
    /// If the action is a reaction, this will describe what triggers the reaction.
    pub reaction_trigger: Option<Arc<ReactionTriggerFunction>>,
    /// Timeline describing the sequence of events that occur when performing the
    /// action, which can be used to synchronize animations and other visual effects.
    pub timeline: ActionTimeline,
}

impl Action {
    pub fn perform(&self, game_state: &mut GameState, action_data: &ActionData) -> Vec<PhaseState> {
        let hooks = systems::effects::effects(&game_state.world, action_data.actor.id())
            .collect_hooks(|effect| effect.on_action.as_ref());
        for hook in hooks {
            hook(game_state, action_data);
        }

        self.kind.perform(game_state, action_data)
    }

    pub fn id(&self) -> &ActionId {
        &self.id
    }

    pub fn kind(&self) -> &ActionKind {
        &self.kind
    }

    pub fn targeting(
        &self,
    ) -> &Arc<dyn Fn(&World, Entity, &ActionContext) -> TargetingContext + Send + Sync> {
        &self.targeting
    }

    pub fn resource_cost(&self) -> &ResourceAmountMap {
        &self.resource_cost
    }

    pub fn resource_cost_mut(&mut self) -> &mut ResourceAmountMap {
        &mut self.resource_cost
    }

    pub fn is_reaction(&self) -> bool {
        self.kind.is_reaction()
            || self
                .resource_cost
                .map
                .contains_key(&ResourceId::new("nat20_core", "resource.reaction"))
    }
}

impl IdProvider for Action {
    type Id = ActionId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

impl Debug for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Action")
            .field("id", &self.id)
            .field("kind", &self.kind)
            .field("resource_cost", &self.resource_cost)
            .field("cooldown", &self.cooldown)
            .finish()
    }
}

impl PartialEq for Action {
    fn eq(&self, other: &Self) -> bool {
        // TODO: For now we just assume actions are equal if their IDs are the same.
        self.id == other.id
    }
}

#[derive(Clone)]
pub enum ActionKind {
    Standard { phases: Vec<ActionPhaseSpec> },
    Variant { variants: Vec<ActionId> },
}

impl ActionKind {
    pub fn perform(&self, game_state: &mut GameState, action_data: &ActionData) -> Vec<PhaseState> {
        let mut phases = Vec::new();

        match self {
            ActionKind::Standard { phases: specs } => {
                let outcomes = Arc::new(PhaseOutcomes::default());
                for (phase_index, spec) in specs.iter().enumerate() {
                    for (target_index, target) in action_data.targets.iter().enumerate() {
                        phases.push(PhaseState::new(
                            game_state,
                            action_data,
                            spec,
                            target.clone(),
                            (phase_index, target_index),
                            Arc::clone(&outcomes),
                        ));
                    }
                }
            }

            ActionKind::Variant { .. } => {
                panic!(
                    "ActionKind::Variants should be resolved to a specific variant before performing"
                );
            }
        }

        phases
    }

    pub fn is_reaction(&self) -> bool {
        self.phases().iter().any(|phase| {
            !phase
                .payload
                .component(ActionPayloadComponentKind::Reaction)
                .is_empty()
        })
    }

    pub fn phases(&self) -> &[ActionPhaseSpec] {
        match self {
            ActionKind::Standard { phases } => phases,
            ActionKind::Variant { .. } => &[],
        }
    }
}

impl Debug for ActionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActionKind::Standard { .. } => write!(f, "Standard"),
            ActionKind::Variant { variants } => write!(f, "Variants({:?})", variants),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ActionAttackKind {
    MeleeWeapon,
    RangedWeapon,
    Unarmed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ActionAttackContext {
    pub kind: ActionAttackKind,
    pub slot: Option<EquipmentSlot>,
}

impl ActionAttackContext {
    pub fn melee_weapon(slot: EquipmentSlot) -> Self {
        Self {
            kind: ActionAttackKind::MeleeWeapon,
            slot: Some(slot),
        }
    }

    pub fn ranged_weapon(slot: EquipmentSlot) -> Self {
        Self {
            kind: ActionAttackKind::RangedWeapon,
            slot: Some(slot),
        }
    }

    pub fn unarmed() -> Self {
        Self {
            kind: ActionAttackKind::Unarmed,
            slot: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ActionSpellContext {
    pub id: SpellId,
    pub source: SpellSource,
    pub level: u8,
}

/// Represents the context in which an action is performed.
#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct ActionContext {
    pub attack: Option<ActionAttackContext>,
    pub spell: Option<ActionSpellContext>,
}

impl ActionContext {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn melee_weapon(slot: EquipmentSlot) -> Self {
        Self {
            attack: Some(ActionAttackContext::melee_weapon(slot)),
            spell: None,
        }
    }

    pub fn ranged_weapon(slot: EquipmentSlot) -> Self {
        Self {
            attack: Some(ActionAttackContext::ranged_weapon(slot)),
            spell: None,
        }
    }

    pub fn unarmed_attack() -> Self {
        Self {
            attack: Some(ActionAttackContext::unarmed()),
            spell: None,
        }
    }

    pub fn spell(id: SpellId, source: SpellSource, level: u8) -> Self {
        Self {
            attack: None,
            spell: Some(ActionSpellContext { id, source, level }),
        }
    }

    pub fn is_spell(&self) -> bool {
        self.spell.is_some()
    }

    pub fn is_attack_action(&self) -> bool {
        self.attack.is_some()
    }

    pub fn is_weapon_attack(&self) -> bool {
        self.attack.as_ref().is_some_and(|attack| {
            matches!(
                attack.kind,
                ActionAttackKind::MeleeWeapon | ActionAttackKind::RangedWeapon
            )
        })
    }

    pub fn is_unarmed_attack(&self) -> bool {
        self.attack
            .as_ref()
            .is_some_and(|attack| matches!(attack.kind, ActionAttackKind::Unarmed))
    }

    pub fn is_melee_attack(&self) -> bool {
        self.attack.as_ref().is_some_and(|attack| {
            matches!(
                attack.kind,
                ActionAttackKind::MeleeWeapon | ActionAttackKind::Unarmed
            )
        })
    }

    pub fn is_ranged_attack(&self) -> bool {
        self.attack
            .as_ref()
            .is_some_and(|attack| matches!(attack.kind, ActionAttackKind::RangedWeapon))
    }
}

pub trait AttackRollProvider {
    fn attack_roll(
        &self,
        world: &World,
        actor: Entity,
        target: Entity,
        context: &ActionContext,
    ) -> AttackRoll;
}

pub trait SavingThrowProvider {
    fn saving_throw(
        &self,
        world: &World,
        actor: Entity,
        context: &ActionContext,
        kind: SavingThrowKind,
    ) -> SavingThrowDC;
}

#[derive(Clone)]
pub enum DamageOnFailure {
    Half,
    Custom(Arc<DamageFunction>),
}

impl Debug for DamageOnFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DamageOnFailure::Half => write!(f, "Half"),
            DamageOnFailure::Custom(_) => write!(f, "Custom"),
        }
    }
}

#[derive(Clone, Kinded)]
pub enum ActionCondition {
    None,
    AttackRoll(Arc<AttackRollFunction>),
    SavingThrow(Arc<SavingThrowFunction>),
}

impl Debug for ActionCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActionCondition::None => write!(f, "None"),
            ActionCondition::AttackRoll(_) => write!(f, "AttackRoll"),
            ActionCondition::SavingThrow(_) => write!(f, "SavingThrow"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PayloadDelivery {
    Immediate,
    Projectile { template: ProjectileTemplate },
}

#[derive(Clone, Kinded)]
pub enum ActionPayloadComponent {
    Damage {
        damage: Arc<DamageFunction>,
        damage_on_failure: Option<DamageOnFailure>,
    },
    Effect(EffectInstanceTemplate),
    Healing(Arc<HealingFunction>),
    Reaction(Arc<ReactionBodyFunction>),
    Displacement(Arc<DisplacementFunction>),
}

#[derive(Clone)]
pub struct ActionPayload {
    components: Vec<ActionPayloadComponent>,
    delivery: PayloadDelivery,
}

#[derive(Debug)]
pub enum ActionPayloadError {
    EmptyPayload,
}

impl ActionPayload {
    pub fn new(
        components: Vec<ActionPayloadComponent>,
        delivery: PayloadDelivery,
    ) -> Result<Self, ActionPayloadError> {
        let payload = ActionPayload {
            components,
            delivery,
        };

        if payload.is_empty() {
            Err(ActionPayloadError::EmptyPayload)
        } else {
            Ok(payload)
        }
    }

    pub fn is_empty(&self) -> bool {
        self.components.is_empty()
    }

    pub fn components(&self) -> &Vec<ActionPayloadComponent> {
        &self.components
    }

    pub fn component(&self, kind: ActionPayloadComponentKind) -> Vec<&ActionPayloadComponent> {
        self.components
            .iter()
            .filter(|component| component.kind() == kind)
            .collect()
    }

    pub fn delivery(&self) -> &PayloadDelivery {
        &self.delivery
    }
}

/// In case of multiple phases, this describes the requirement for a phase to be
/// executed, e.g. only execute if the previous phase succeeded or failed.
#[derive(Debug, Clone, Copy, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PhaseRequirement {
    #[default]
    None,
    Success,
    Failure,
}

#[derive(Clone)]
pub enum PhaseTargets {
    /// Same targets as the action's initial targets
    Inherited,
    /// The entities inside a shape centered on each chosen target
    Shape(Arc<AreaShapeFunction>),
}

#[derive(Clone)]
pub struct ActionPhaseSpec {
    pub requires: PhaseRequirement,
    pub condition: ActionCondition,
    pub payload: ActionPayload,
    pub targets: PhaseTargets,
}

/// Condition outcomes of the phases of one action execution, keyed by
/// (phase index, target index), shared between the phases so later ones can be
/// gated on earlier results.
#[derive(Debug, Default)]
pub struct PhaseOutcomes(Mutex<HashMap<(usize, usize), bool>>);

impl PhaseOutcomes {
    pub fn record(&self, phase_index: usize, target_index: usize, success: bool) {
        self.0
            .lock()
            .unwrap()
            .insert((phase_index, target_index), success);
    }

    pub fn get(&self, phase_index: usize, target_index: usize) -> Option<bool> {
        self.0
            .lock()
            .unwrap()
            .get(&(phase_index, target_index))
            .copied()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionConditionResolution {
    Unconditional,
    AttackRoll {
        attack_roll: AttackRollResult,
        armor_class: ArmorClass,
    },
    SavingThrow {
        saving_throw_dc: SavingThrowDC,
        saving_throw_result: D20CheckResult,
    },
}

impl ActionConditionResolution {
    pub fn is_success(&self) -> bool {
        match self {
            ActionConditionResolution::Unconditional => true,
            ActionConditionResolution::AttackRoll {
                attack_roll,
                armor_class,
            } => attack_roll.is_success(armor_class),
            ActionConditionResolution::SavingThrow {
                saving_throw_dc,
                saving_throw_result,
            } => {
                // For saving throws, a successful save means the action's effect
                // is avoided or reduced, so we check for failure here.
                !saving_throw_result.is_success(saving_throw_dc)
            }
        }
    }

    pub fn is_crit(&self) -> bool {
        match self {
            ActionConditionResolution::AttackRoll { attack_roll, .. } => attack_roll.is_crit(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DamageResult {
    pub resolution: ActionConditionResolution,
    pub damage_roll: Option<DamageRollResult>,
    pub damage_taken: Option<DamageMitigationResult>,
    pub new_life_state: Option<LifeState>,
}

impl DamageResult {
    pub fn unconditional(
        damage_roll: Option<DamageRollResult>,
        damage_taken: Option<DamageMitigationResult>,
        new_life_state: Option<LifeState>,
    ) -> Self {
        DamageResult {
            resolution: ActionConditionResolution::Unconditional,
            damage_roll,
            damage_taken,
            new_life_state,
        }
    }

    pub fn attack_roll(
        damage_roll: Option<DamageRollResult>,
        damage_taken: Option<DamageMitigationResult>,
        new_life_state: Option<LifeState>,
        attack_roll: AttackRollResult,
        armor_class: ArmorClass,
    ) -> Self {
        DamageResult {
            resolution: ActionConditionResolution::AttackRoll {
                attack_roll,
                armor_class,
            },
            damage_roll,
            damage_taken,
            new_life_state,
        }
    }

    pub fn saving_throw(
        damage_roll: Option<DamageRollResult>,
        damage_taken: Option<DamageMitigationResult>,
        new_life_state: Option<LifeState>,
        saving_throw_dc: SavingThrowDC,
        saving_throw_result: D20CheckResult,
    ) -> Self {
        DamageResult {
            resolution: ActionConditionResolution::SavingThrow {
                saving_throw_dc,
                saving_throw_result,
            },
            damage_roll,
            damage_taken,
            new_life_state,
        }
    }

    pub fn empty(resolution: ActionConditionResolution) -> Self {
        DamageResult {
            resolution,
            damage_roll: None,
            damage_taken: None,
            new_life_state: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EffectResultKind {
    Applied,
    Removed,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EffectResult {
    pub resolution: ActionConditionResolution,
    pub effect: EffectId,
    pub result: EffectResultKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HealingResult {
    // TODO: Dedicated type for healing rolls?
    pub healing: DiceSetRollResult,
    pub new_life_state: Option<LifeState>,
}

#[derive(Debug, Clone)]
pub enum ReactionResult {
    ModifyEvent {
        before: Event,
        after: Event,
    },
    CancelEvent {
        event: Box<Event>,
        resources_refunded: ResourceAmountMap,
    },
    NoEffect,
}

impl PartialEq for ReactionResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                ReactionResult::ModifyEvent {
                    before: b1,
                    after: a1,
                },
                ReactionResult::ModifyEvent {
                    before: b2,
                    after: a2,
                },
            ) => b1.id == b2.id && a1.id == a2.id,
            (
                ReactionResult::CancelEvent { event: e1, .. },
                ReactionResult::CancelEvent { event: e2, .. },
            ) => e1.id == e2.id,
            (ReactionResult::NoEffect, ReactionResult::NoEffect) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Kinded)]
pub enum ActionResultComponent {
    Damage(DamageResult),
    Effect(EffectResult),
    Healing(HealingResult),
    Reaction(ReactionResult),
    Displacement(Option<Displacement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionResult {
    pub target: EntityIdentifier,
    pub components: Vec<ActionResultComponent>,
}

impl ActionResult {
    pub fn is_empty(&self) -> bool {
        self.components.is_empty()
    }

    pub fn components(&self) -> &Vec<ActionResultComponent> {
        &self.components
    }

    pub fn component(&self, kind: ActionResultComponentKind) -> Vec<&ActionResultComponent> {
        self.components
            .iter()
            .filter(|component| component.kind() == kind)
            .collect()
    }

    pub fn resolution(&self) -> &ActionConditionResolution {
        for component in &self.components {
            match component {
                ActionResultComponent::Damage(damage) => return &damage.resolution,
                ActionResultComponent::Effect(effect) => return &effect.resolution,
                _ => {}
            }
        }
        &ActionConditionResolution::Unconditional
    }
}

/// Represents a provider of actions, which can be used to retrieve available actions
/// from a character or other entity that can perform actions.
pub trait ActionProvider {
    // TODO: Should probably find a way to avoid rebuilding the action collection every time.

    /// Returns a collection of ALL possible actions for the character, including
    /// actions that are not currently available (e.g. on cooldown, out of resources, etc.).
    /// Each action is paired with its context, which provides additional information
    /// about how the action can be performed (e.g. weapon type, spell level, etc.)
    /// as well as the resource cost of the action.
    fn actions(&self, world: &World, entity: Entity) -> ActionMap;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionTimeline {
    pub total_duration: f32,
    pub perform_time: f32,
    #[serde(default)]
    pub step_spacing: f32,
}

// TODO: Combine these two?
pub type ActionMap = BTreeMap<ActionId, Vec<(ActionContext, ResourceAmountMap)>>;

pub type ActionCooldownMap = HashMap<ActionId, RechargeRule>;

// TODO: Not sure if this is the best solution
pub fn default_actions() -> ActionMap {
    let mut actions = ActionMap::new();
    for (action, context) in [
        (
            ActionId::new("nat20_core", "action.dash"),
            ActionContext::default(),
        ),
        (
            ActionId::new("nat20_core", "action.disengage"),
            ActionContext::default(),
        ),
        (
            ActionId::new("nat20_core", "action.unarmed_attack"),
            ActionContext::unarmed_attack(),
        ),
        (
            ActionId::new("nat20_core", "action.opportunity_attack"),
            ActionContext::unarmed_attack(),
        ),
    ] {
        let resource_cost = ActionsRegistry::get(&action).unwrap().resource_cost.clone();
        actions.insert(action.clone(), vec![(context, resource_cost)]);
    }
    actions
}
