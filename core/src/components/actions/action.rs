use std::{collections::HashMap, fmt::Debug, sync::Arc};

use hecs::{Entity, World};
use serde::Deserialize;

use crate::{
    components::{
        actions::{
            action_execution::perform_standard_action,
            targeting::{TargetInstance, TargetingContext},
        },
        d20::D20CheckResult,
        damage::{
            AttackRoll, AttackRollResult, DamageMitigationResult, DamageRoll, DamageRollResult,
        },
        dice::{DiceSetRoll, DiceSetRollResult},
        effects::effect::EffectInstanceTemplate,
        health::life_state::LifeState,
        id::{ActionId, EffectId, EntityIdentifier, IdProvider, ScriptId, SpellId},
        items::equipment::{armor::ArmorClass, slots::EquipmentSlot},
        resource::{RechargeRule, ResourceAmountMap},
        saving_throw::{SavingThrowDC, SavingThrowKind},
        spells::spellbook::SpellSource,
    },
    engine::{action_prompt::ActionData, event::Event, game_state::GameState},
    registry::{registry::ActionsRegistry, serialize::action::ActionDefinition},
    systems::{self},
};

pub type DamageFunction = dyn Fn(&World, Entity, &ActionContext) -> DamageRoll + Send + Sync;
pub type AttackRollFunction =
    dyn Fn(&World, Entity, Entity, &ActionContext) -> AttackRoll + Send + Sync;
pub type SavingThrowFunction =
    dyn Fn(&World, Entity, &ActionContext) -> SavingThrowDC + Send + Sync;
pub type HealFunction = dyn Fn(&World, Entity, &ActionContext) -> DiceSetRoll + Send + Sync;
pub type TargetingFunction =
    dyn Fn(&World, Entity, &ActionContext) -> TargetingContext + Send + Sync;
pub type ReactionTriggerFunction = dyn Fn(&World, &Entity, &Event) -> bool + Send + Sync;

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
}

impl Action {
    /// Targets are very explicitly passed as a separate parameter here, since the
    /// targets in the `ActionData` can also be points, so prior to calling `perform`
    /// the targetted entities are resolved.
    pub fn perform(
        &mut self,
        game_state: &mut GameState,
        action_data: &ActionData,
        targets: &[Entity],
    ) {
        // TODO: Not a fan of having to clone to avoid borrowing issues, but
        // hopefully since most of the effect just have a no-op as their
        // on_action component it'll be cheap to clone
        let hooks: Vec<_> = systems::effects::effects(&game_state.world, action_data.actor)
            .values()
            .filter_map(|effect| Some(effect.effect().on_action.clone()))
            .collect();

        for hook in hooks {
            hook(&mut game_state.world, action_data);
        }

        self.kind.perform(game_state, action_data, targets);
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

/// Represents the context in which an action is performed.
/// This can be used to determine the type of action (e.g. weapon, spell, etc.)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActionContext {
    // TODO: Not sure if Weapon needs more info?
    Weapon {
        slot: EquipmentSlot,
    },
    Spell {
        id: SpellId,
        /// Having the source here allows us to track whether the spell is coming
        /// from a class, subclass, item, feat, etc., which is useful for determining
        /// e.g. spellcasting ability for spell save DCs and spell attack rolls.
        source: SpellSource,
        /// When casting a spell it is important to know the spell level, since
        /// most spells have different effects based on the level at which they are cast.
        /// For example, Fireball deals more damage when cast at a higher level.
        level: u8,
    },
    // TODO: Not sure if Other is needed
    Other,
}

pub trait AttackRollProvider {
    fn attack_roll(
        &self,
        world: &World,
        performer: Entity,
        target: Entity,
        context: &ActionContext,
    ) -> AttackRoll;
}

pub trait SavingThrowProvider {
    fn saving_throw(
        &self,
        world: &World,
        performer: Entity,
        context: &ActionContext,
        kind: SavingThrowKind,
    ) -> SavingThrowDC;
}

#[derive(Clone)]
pub enum DamageOnFailure {
    Half,
    Custom(Arc<DamageFunction>),
}

#[derive(Clone)]
pub enum ActionCondition {
    None,
    AttackRoll {
        attack_roll: Arc<AttackRollFunction>,
        damage_on_miss: Option<DamageOnFailure>,
    },
    SavingThrow {
        saving_throw: Arc<SavingThrowFunction>,
        damage_on_save: Option<DamageOnFailure>,
    },
}

#[derive(Clone)]
pub struct ActionPayload {
    damage: Option<Arc<DamageFunction>>,
    effect: Option<EffectInstanceTemplate>,
    healing: Option<Arc<HealFunction>>,
}

#[derive(Debug)]
pub enum ActionPayloadError {
    EmptyPayload,
}

impl ActionPayload {
    pub fn new(
        damage: Option<Arc<DamageFunction>>,
        effect: Option<EffectInstanceTemplate>,
        healing: Option<Arc<HealFunction>>,
    ) -> Result<Self, ActionPayloadError> {
        let payload = ActionPayload {
            damage,
            effect,
            healing,
        };

        if payload.is_empty() {
            Err(ActionPayloadError::EmptyPayload)
        } else {
            Ok(payload)
        }
    }

    pub fn is_empty(&self) -> bool {
        self.damage.is_none() && self.effect.is_none() && self.healing.is_none()
    }

    pub fn with_damage(damage: Arc<DamageFunction>) -> Self {
        Self {
            damage: Some(damage),
            effect: None,
            healing: None,
        }
    }

    pub fn with_effect(effect: EffectInstanceTemplate) -> Self {
        Self {
            damage: None,
            effect: Some(effect),
            healing: None,
        }
    }

    pub fn with_healing(healing: Arc<HealFunction>) -> Self {
        Self {
            damage: None,
            effect: None,
            healing: Some(healing),
        }
    }

    pub fn damage(&self) -> Option<&Arc<DamageFunction>> {
        self.damage.as_ref()
    }

    pub fn effect(&self) -> Option<&EffectInstanceTemplate> {
        self.effect.as_ref()
    }

    pub fn healing(&self) -> Option<&Arc<HealFunction>> {
        self.healing.as_ref()
    }
}

#[derive(Clone)]
pub enum ActionKind {
    Standard {
        condition: ActionCondition,
        payload: ActionPayload,
    },
    Composite {
        actions: Vec<ActionKind>,
    },
    Variant {
        variants: Vec<ActionId>,
    },
    Reaction {
        reaction: ScriptId,
    },
    Custom(Arc<dyn Fn(&World, Entity, &ActionContext) + Send + Sync>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct DamageOutcome {
    pub resolution: ActionConditionResolution,
    pub damage_roll: Option<DamageRollResult>,
    pub damage_taken: Option<DamageMitigationResult>,
    pub new_life_state: Option<LifeState>,
}

impl DamageOutcome {
    pub fn unconditional(
        damage_roll: Option<DamageRollResult>,
        damage_taken: Option<DamageMitigationResult>,
        new_life_state: Option<LifeState>,
    ) -> Self {
        DamageOutcome {
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
        DamageOutcome {
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
        DamageOutcome {
            resolution: ActionConditionResolution::SavingThrow {
                saving_throw_dc,
                saving_throw_result,
            },
            damage_roll,
            damage_taken,
            new_life_state,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EffectOutcome {
    pub resolution: ActionConditionResolution,
    pub effect: EffectId,
    pub applied: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HealingOutcome {
    pub healing: DiceSetRollResult,
    pub new_life_state: Option<LifeState>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionOutcomeBundle {
    pub damage: Option<DamageOutcome>,
    pub effect: Option<EffectOutcome>,
    pub healing: Option<HealingOutcome>,
}

impl ActionOutcomeBundle {
    pub fn is_empty(&self) -> bool {
        self.damage.is_none() && self.effect.is_none() && self.healing.is_none()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionKindResult {
    Standard(ActionOutcomeBundle),
    Utility,
    Composite { actions: Vec<ActionKindResult> },
    Reaction { result: ReactionResult },
    Custom {/* ... */},
}

#[derive(Clone)]
pub enum ReactionResult {
    ModifyEvent {
        modification: Arc<dyn Fn(&World, &mut Event) + Send + Sync>,
    },
    CancelEvent {
        event: Box<Event>,
        resources_refunded: ResourceAmountMap,
    },
    NoEffect,
}

impl Debug for ReactionResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReactionResult::ModifyEvent { .. } => write!(f, "ModifyEvent"),
            ReactionResult::CancelEvent {
                event,
                resources_refunded,
            } => f
                .debug_struct("CancelEvent")
                .field("event", &event.id)
                .field("resources_refunded", resources_refunded)
                .finish(),
            ReactionResult::NoEffect => write!(f, "NoEffect"),
        }
    }
}

impl PartialEq for ReactionResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ReactionResult::ModifyEvent { .. }, ReactionResult::ModifyEvent { .. }) => true,
            (
                ReactionResult::CancelEvent { event: e1, .. },
                ReactionResult::CancelEvent { event: e2, .. },
            ) => e1.id == e2.id,
            (ReactionResult::NoEffect, ReactionResult::NoEffect) => true,
            _ => false,
        }
    }
}

/// Represents the result of performing an action on a single target. For actions
/// that affect multiple targets, multiple `ActionResult` instances can be collected.
#[derive(Debug, Clone, PartialEq)]
pub struct ActionResult {
    pub performer: EntityIdentifier,
    pub target: TargetInstance,
    pub kind: ActionKindResult,
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

impl ActionKind {
    pub fn perform(
        &self,
        game_state: &mut GameState,
        action_data: &ActionData,
        targets: &[Entity],
    ) {
        match self {
            ActionKind::Standard { .. } => {
                for target in targets {
                    perform_standard_action(game_state, self, action_data, *target);
                }
            }

            ActionKind::Composite { actions } => {
                for action in actions {
                    match action {
                        ActionKind::Reaction { .. } => {
                            // Assume this is being performed as part of a reaction
                            // TODO: Also seems like a bit of a hack
                            continue;
                        }
                        _ => action.perform(game_state, action_data, targets),
                    }
                }
            }

            ActionKind::Variant { .. } => {
                panic!(
                    "ActionKind::Variants should be resolved to a specific variant before performing"
                );
            }

            ActionKind::Reaction { .. } => {
                panic!(
                    "ActionKind::Reaction should be performed via systems::actions::perform_reaction"
                );
            }

            ActionKind::Custom(custom) => {
                // custom(game_state.world, target, context)
                todo!("Custom actions are not yet implemented");
            }
        }
    }
}

impl Debug for ActionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActionKind::Standard { .. } => write!(f, "Standard"),
            ActionKind::Composite { actions } => write!(f, "Composite({:?})", actions),
            ActionKind::Variant { variants } => write!(f, "Variants({:?})", variants),
            ActionKind::Reaction { .. } => write!(f, "Reaction"),
            ActionKind::Custom(_) => write!(f, "CustomAction"),
        }
    }
}

impl ActionResult {
    pub fn new(world: &World, performer: Entity, target: Entity, kind: ActionKindResult) -> Self {
        ActionResult {
            performer: EntityIdentifier::from_world(world, performer),
            target: TargetInstance::Entity(target),
            kind,
        }
    }
}

// TODO: Combine these two?
pub type ActionMap = HashMap<ActionId, Vec<(ActionContext, ResourceAmountMap)>>;

pub type ActionCooldownMap = HashMap<ActionId, RechargeRule>;

// TODO: Not sure if this is the best solution
pub fn default_actions() -> ActionMap {
    let mut actions = ActionMap::new();
    for action in [
        ActionId::new("nat20_core", "action.dash"),
        ActionId::new("nat20_core", "action.disengage"),
    ] {
        let resource_cost = ActionsRegistry::get(&action).unwrap().resource_cost.clone();
        actions.insert(action.clone(), vec![(ActionContext::Other, resource_cost)]);
    }
    actions
}
