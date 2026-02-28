use std::{
    fmt::Debug,
    str::FromStr,
    sync::{Arc, RwLock},
};

use hecs::{Entity, World};

use crate::{
    components::{
        actions::{
            action::{
                ActionCondition, ActionConditionResolution, ActionContext, ActionKind,
                ActionKindResult, ActionOutcomeBundle, DamageOutcome,
            },
            targeting::TargetInstance,
        },
        damage::{
            DamageComponentResult, DamageMitigationEffect, DamageMitigationResult,
            DamageRollResult, DamageType, MitigationOperation,
        },
        dice::{DiceSet, DiceSetRoll},
        effects::effect::EffectInstance,
        id::{ActionId, ResourceId},
        items::equipment::loadout::Loadout,
        modifier::{Modifiable, ModifierSet, ModifierSource},
        resource::{ResourceAmount, ResourceAmountMap, ResourceBudgetKind, ResourceMap},
    },
    engine::{
        action_prompt::{ActionData, ReactionData},
        event::{Event, EventKind},
    },
    registry::serialize::{
        d20::SavingThrowDefinition,
        parser::{DiceExpression, Evaluable, IntExpression, Parser},
        variables::PARSER_VARIABLES,
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20ResultKind},
    },
};

/// Thread-safe shared wrapper for script-exposed data. The primary purpose is to
/// allow the scripts to mutate data without cloning it back and forth, e.g.
/// exposing a resource cost map that the script can modify in place.
///
/// However, note that sometimes we want to expose data as immutable (read-only).
/// This is due to the following reasons:
///     1. There are different types of scripts, and not all of them are supposed
///        to modify data. For example, a reaction trigger script should only inspect
///        the event which triggerd it, and then decide whether to react or not. It
///        should not be able to modify the event data itself.
///     2. To avoid duplicating structs/enums for mutable vs immutable variants,
///        we use a single `ScriptShared<T>` type with a flag indicating mutability.
///        This simplifies the API and reduces code duplication.
/// The `mutable` flag indicates whether the data can be mutated via `write()`.
/// Only data created for mutable script flows (e.g. via `take_from()` or `new_mut()`)
/// is intended to be mutable.
#[derive(Debug, Clone)]
pub struct ScriptShared<T> {
    inner: Arc<RwLock<T>>,
    mutable: bool,
}

impl<T> ScriptShared<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Arc::new(RwLock::new(value)),
            mutable: false,
        }
    }

    pub fn new_mut(value: T) -> Self {
        Self {
            inner: Arc::new(RwLock::new(value)),
            mutable: true,
        }
    }

    /// Moves the value out of `target` and replaces it with `T::default()`.
    pub fn take_from(target: &mut T) -> Self
    where
        T: Default,
    {
        Self {
            inner: Arc::new(RwLock::new(std::mem::take(target))),
            mutable: true,
        }
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    /// Moves the value out of this shared wrapper.
    /// Panics if the handle was cloned and is still shared.
    pub fn into_inner(self) -> T
    where
        T: Debug,
    {
        Arc::try_unwrap(self.inner)
            .expect("Shared value leaked (extra Arc clones exist)")
            .into_inner()
            .expect("RwLock poisoned")
    }

    pub fn read(&self) -> std::sync::RwLockReadGuard<'_, T> {
        self.inner.read().expect("RwLock poisoned")
    }

    pub fn write(&self) -> std::sync::RwLockWriteGuard<'_, T> {
        if !self.mutable {
            panic!("Attempted to get mutable access to an immutable ScriptShared value");
        }
        self.inner.write().expect("RwLock poisoned")
    }
}

macro_rules! impl_script_shared_methods {
    ($wrapper:ty, $inner:ty) => {
        impl $wrapper {
            pub fn new(value: $inner) -> Self {
                Self {
                    inner: ScriptShared::new(value),
                }
            }

            pub fn take_from(target: &mut $inner) -> Self {
                Self {
                    inner: ScriptShared::take_from(target),
                }
            }

            pub fn into_inner(self) -> $inner {
                self.inner.into_inner()
            }
        }
    };
}

macro_rules! impl_take_replace_world {
    ($wrapper:ty, $inner:ty) => {
        impl $wrapper {
            pub fn new_from_world(world: &World, entity: Entity) -> Self {
                let component = systems::helpers::get_component_clone::<$inner>(world, entity);
                Self::new(component)
            }

            pub fn take_from_world(world: &mut World, entity: Entity) -> Self {
                if let Ok(component) = world.query_one_mut::<&mut $inner>(entity) {
                    Self::take_from(component)
                } else {
                    panic!(
                        "Entity {:?} does not have component {}",
                        entity,
                        std::any::type_name::<$inner>()
                    );
                }
            }

            pub fn replace_in_world(self, world: &mut World, entity: Entity) {
                if let Ok(component) = world.query_one_mut::<&mut $inner>(entity) {
                    *component = self.into_inner();
                }
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScriptEntity {
    pub id: u64,
}

impl From<Entity> for ScriptEntity {
    fn from(entity: Entity) -> Self {
        ScriptEntity {
            id: u64::from(entity.to_bits()),
        }
    }
}

impl Into<Entity> for ScriptEntity {
    fn into(self) -> Entity {
        Entity::from_bits(self.id).unwrap()
    }
}

#[derive(Clone)]
pub struct ScriptEffectView {
    pub effect_id: String,
    pub source: String,
    pub applier: Option<ScriptEntity>,
}

impl ScriptEffectView {
    pub fn has_applier(&self) -> bool {
        self.applier.is_some()
    }

    pub fn get_applier(&self) -> &ScriptEntity {
        self.applier
            .as_ref()
            .expect("No applier associated with this effect")
    }
}

impl From<&EffectInstance> for ScriptEffectView {
    fn from(instance: &EffectInstance) -> Self {
        ScriptEffectView {
            effect_id: instance.effect_id.to_string(),
            source: instance.source.to_string(),
            applier: instance.applier.map(ScriptEntity::from),
        }
    }
}

#[derive(Clone)]
pub struct ScriptDamageRollResult {
    inner: ScriptShared<DamageRollResult>,
}

impl ScriptDamageRollResult {
    pub fn source(&self) -> String {
        self.inner.read().source.to_string()
    }

    pub fn clamp_damage_dice_min(&mut self, minimum_roll: u32) {
        let mut inner = self.inner.write();

        for component in &mut inner.components {
            // This assumes DamageComponentResult has `result: DiceSetRollResult` etc.
            for roll in &mut component.result.rolls {
                if *roll < minimum_roll {
                    *roll = minimum_roll;
                }
            }
            component.result.recalculate_total();
        }

        inner.recalculate_total();
    }

    pub fn has_actor(&self) -> bool {
        self.inner.read().action.is_some()
    }

    pub fn get_actor(&self) -> ScriptEntity {
        let inner = self.inner.read();
        let (actor_entity, _) = inner
            .action
            .as_ref()
            .expect("No action associated with this damage roll result");
        ScriptEntity::from(*actor_entity)
    }

    pub fn is_action_attack_roll(&self) -> bool {
        self.is_action_condition_type(|cond| matches!(cond, ActionCondition::AttackRoll { .. }))
    }

    pub fn is_action_saving_throw(&self) -> bool {
        self.is_action_condition_type(|cond| matches!(cond, ActionCondition::SavingThrow { .. }))
    }

    pub fn is_action_unconditional(&self) -> bool {
        self.is_action_condition_type(|cond| matches!(cond, ActionCondition::None))
    }

    fn is_action_condition_type(&self, predicate: fn(&ActionCondition) -> bool) -> bool {
        let inner = self.inner.read();
        if let Some((_, action_id)) = &inner.action {
            return is_action_condition_type(action_id, predicate);
        }
        false
    }

    pub fn add_damage(&mut self, bonus: ScriptDiceRollBonus, damage_type: DamageType) {
        let mut inner = self.inner.write();
        match bonus {
            ScriptDiceRollBonus::Flat(int_expression) => {
                let flat_bonus = int_expression.evaluate_without_variables().unwrap();
                // TODO: Not sure how to add a flat bonus properly here.
            }
            ScriptDiceRollBonus::Dice(dice_expression) => {
                let (num_dice, die_size, modifier) =
                    dice_expression.evaluate_without_variables().unwrap();
                let result = DiceSetRoll {
                    dice: DiceSet::from_str(format!("{}d{}", num_dice, die_size).as_str()).unwrap(),
                    modifiers: ModifierSet::from(ModifierSource::Base, modifier),
                }
                .roll();
                inner.add_component(DamageComponentResult {
                    result,
                    damage_type,
                });
            }
        }
    }
}

impl_script_shared_methods!(ScriptDamageRollResult, DamageRollResult);

#[derive(Clone)]
pub struct ScriptDamageMitigationResult {
    inner: ScriptShared<DamageMitigationResult>,
}

impl ScriptDamageMitigationResult {
    pub fn source(&self) -> String {
        self.inner.read().source.to_string()
    }

    pub fn add_immunity(&mut self) {
        let mut inner = self.inner.write();
        for component in &mut inner.components {
            component.modifiers.push(DamageMitigationEffect {
                source: ModifierSource::Custom(
                    "TODO: Figure out how to propagate the source".to_string(),
                ),
                operation: MitigationOperation::Immunity,
            });
        }
        inner.recalculate_total();
    }

    pub fn has_action(&self) -> bool {
        self.inner.read().action.is_some()
    }

    pub fn get_actor(&self) -> ScriptEntity {
        let inner = self.inner.read();
        let (actor_entity, _) = inner
            .action
            .as_ref()
            .expect("No action associated with this damage mitigation result");
        ScriptEntity::from(*actor_entity)
    }

    pub fn is_action_attack_roll(&self) -> bool {
        self.is_action_condition_type(|cond| matches!(cond, ActionCondition::AttackRoll { .. }))
    }

    pub fn is_action_saving_throw(&self) -> bool {
        self.is_action_condition_type(|cond| matches!(cond, ActionCondition::SavingThrow { .. }))
    }

    pub fn is_action_unconditional(&self) -> bool {
        self.is_action_condition_type(|cond| matches!(cond, ActionCondition::None))
    }

    pub fn is_action_condition_type(&self, predicate: fn(&ActionCondition) -> bool) -> bool {
        let inner = self.inner.read();
        if let Some((_, action_id)) = &inner.action {
            return is_action_condition_type(action_id, predicate);
        }
        false
    }
}

impl_script_shared_methods!(ScriptDamageMitigationResult, DamageMitigationResult);

fn is_action_condition_type(action_id: &ActionId, predicate: fn(&ActionCondition) -> bool) -> bool {
    let action = systems::actions::get_action(action_id).unwrap();
    match &action.kind {
        ActionKind::Standard { condition, .. } => predicate(condition),
        _ => false,
    }
}

#[derive(Clone)]
pub struct ScriptD20CheckDCKind {
    pub label: String,
    pub dc: i32,
    // Currently only used for AttackRoll
    pub target: Option<ScriptEntity>,
}

impl ScriptD20CheckDCKind {
    pub fn from(dc_kind: &D20CheckDCKind) -> Self {
        match dc_kind {
            D20CheckDCKind::SavingThrow(dc) => ScriptD20CheckDCKind {
                label: "SavingThrow".to_string(),
                dc: dc.dc.total(),
                target: None,
            },
            D20CheckDCKind::Skill(dc) => ScriptD20CheckDCKind {
                label: "Skill".to_string(),
                dc: dc.dc.total(),
                target: None,
            },
            D20CheckDCKind::AttackRoll(target_entity, armor_class) => ScriptD20CheckDCKind {
                label: "AttackRoll".to_string(),
                dc: armor_class.total() as i32,
                target: Some(ScriptEntity::from(*target_entity)),
            },
        }
    }
}

#[derive(Clone)]
pub struct ScriptD20Result {
    pub total: u32,
    pub dc_kind: ScriptD20CheckDCKind,
    pub is_success: bool,
}

impl ScriptD20Result {
    pub fn from(result_kind: &D20ResultKind, dc_kind: &D20CheckDCKind) -> Self {
        let result = match result_kind {
            D20ResultKind::Skill { result, .. } | D20ResultKind::SavingThrow { result, .. } => {
                result
            }
            D20ResultKind::AttackRoll { result } => &result.roll_result,
        };
        ScriptD20Result {
            total: result.total(),
            dc_kind: ScriptD20CheckDCKind::from(dc_kind),
            is_success: result_kind.is_success(dc_kind),
        }
    }
}

#[derive(Clone)]
pub(crate) struct ScriptMovingOutOfReach {
    pub mover: ScriptEntity,
    pub entity: ScriptEntity,
    pub continue_movement: bool,
}

#[derive(Clone)]
pub struct ScriptMovingOutOfReachView {
    pub(crate) inner: ScriptShared<ScriptMovingOutOfReach>,
}

impl ScriptMovingOutOfReachView {
    pub fn new(mover: ScriptEntity, entity: ScriptEntity, continue_movement: bool) -> Self {
        Self {
            inner: ScriptShared::new(ScriptMovingOutOfReach {
                mover,
                entity,
                continue_movement,
            }),
        }
    }

    pub fn new_mut(mover: ScriptEntity, entity: ScriptEntity, continue_movement: bool) -> Self {
        Self {
            inner: ScriptShared::new_mut(ScriptMovingOutOfReach {
                mover,
                entity,
                continue_movement,
            }),
        }
    }

    pub fn apply_to_event(&self, event: &mut Event) {
        if let EventKind::MovingOutOfReach {
            mover,
            entity,
            continue_movement,
        } = &mut event.kind
        {
            let inner = self.inner.read();
            *mover = inner.mover.clone().into();
            *entity = inner.entity.clone().into();
            *continue_movement = inner.continue_movement;
        } else {
            panic!(
                "ScriptMovingOutOfReachView applied to wrong event type: {:?}",
                event
            );
        }
    }
}

/// High-level event view that scripts can work with.
#[derive(Clone)]
pub enum ScriptEventView {
    ActionRequested(ScriptActionView),
    ActionPerformed(ScriptActionPerformedView),
    D20CheckPerformed(ScriptD20CheckView),
    MovingOutOfReach(ScriptMovingOutOfReachView),
}

impl ScriptEventView {
    pub fn from_event(event: &Event) -> Option<Self> {
        Self::from_event_with_mutability(event, false)
    }

    pub fn from_event_mut(event: &Event) -> Option<Self> {
        Self::from_event_with_mutability(event, true)
    }

    fn from_event_with_mutability(event: &Event, mutable: bool) -> Option<Self> {
        match &event.kind {
            EventKind::D20CheckPerformed(performer, result_kind, dc_kind) => {
                Some(ScriptEventView::D20CheckPerformed(
                    ScriptD20CheckView::from_parts(*performer, result_kind, dc_kind),
                ))
            }

            EventKind::ActionRequested { action } => Some(ScriptEventView::ActionRequested(
                ScriptActionView::from(action),
            )),

            EventKind::ReactionRequested { reaction } => {
                let action = ActionData::from(reaction);
                Some(ScriptEventView::ActionRequested(ScriptActionView::from(
                    &action,
                )))
            }

            EventKind::ActionPerformed { action, results } => {
                let action_view = ScriptActionView::from(action);

                let mut script_results = Vec::new();
                for result in results {
                    if let TargetInstance::Entity(target_entity) = result.target {
                        script_results.push(ScriptActionResultView::from_action_result(
                            action.actor,
                            target_entity,
                            &result.kind,
                        ));
                    }
                }

                Some(ScriptEventView::ActionPerformed(
                    ScriptActionPerformedView::new(action_view, script_results),
                ))
            }

            EventKind::MovingOutOfReach {
                mover,
                entity,
                continue_movement,
            } => Some(ScriptEventView::MovingOutOfReach(if mutable {
                ScriptMovingOutOfReachView::new_mut(
                    ScriptEntity::from(*mover),
                    ScriptEntity::from(*entity),
                    *continue_movement,
                )
            } else {
                ScriptMovingOutOfReachView::new(
                    ScriptEntity::from(*mover),
                    ScriptEntity::from(*entity),
                    *continue_movement,
                )
            })),

            _ => None,
        }
    }

    pub fn apply_to_event(&self, event: &mut Event) {
        match self {
            ScriptEventView::MovingOutOfReach(view) => view.apply_to_event(event),
            // Other event view variants are currently read-only.
            _ => {}
        }
    }
}

macro_rules! impl_event_accessors {
    ($enum_name:ident {
        $(
            $is_name:ident => $as_name:ident : $variant:ident ( $ty:ty )
        ),+ $(,)?
    }) => {
        impl $enum_name {
            $(
                pub fn $is_name(&self) -> bool {
                    matches!(self, Self::$variant(_))
                }

                pub fn $as_name(&self) -> &$ty {
                    if let Self::$variant(value) = self {
                        value
                    } else {
                        panic!(concat!("Not a ", stringify!($variant), " event view"));
                    }
                }
            )+
        }
    };
}

impl_event_accessors!(ScriptEventView {
    is_d20_check_performed => as_d20_check_performed: D20CheckPerformed(ScriptD20CheckView),
    is_action_requested    => as_action_requested:    ActionRequested(ScriptActionView),
    is_action_performed    => as_action_performed:    ActionPerformed(ScriptActionPerformedView),
    is_moving_out_of_reach => as_moving_out_of_reach: MovingOutOfReach(ScriptMovingOutOfReachView),
});

/// View of a "D20CheckPerformed" event.
#[derive(Clone)]
pub struct ScriptD20CheckView {
    pub performer: ScriptEntity,
    pub result: ScriptD20Result,
}

impl ScriptD20CheckView {
    pub fn from_parts(
        performer: Entity,
        result_kind: &D20ResultKind,
        dc_kind: &D20CheckDCKind,
    ) -> Self {
        ScriptD20CheckView {
            performer: ScriptEntity::from(performer),
            result: ScriptD20Result::from(result_kind, dc_kind),
        }
    }
}

#[derive(Clone)]
pub struct ScriptActionContext {
    pub inner: ActionContext,
}

impl ScriptActionContext {
    pub fn is_spell(&self) -> bool {
        matches!(self.inner, ActionContext::Spell { .. })
    }

    pub fn is_weapon_attack(&self) -> bool {
        matches!(self.inner, ActionContext::Weapon { .. })
    }
}

impl From<&ActionContext> for ScriptActionContext {
    fn from(context: &ActionContext) -> Self {
        ScriptActionContext {
            inner: context.clone(),
        }
    }
}

#[derive(Clone)]
pub struct ScriptResourceCost {
    inner: ScriptShared<ResourceAmountMap>,
}

impl ScriptResourceCost {
    pub fn costs_resource(&self, resource_id: &ResourceId) -> bool {
        self.inner.read().contains_key(resource_id)
    }

    pub fn replace_resource(&mut self, from: &ResourceId, to: &ResourceId, amount: ResourceAmount) {
        let mut cost = self.inner.write();

        if let Some(from_amount) = cost.get(from) {
            if &amount >= from_amount {
                cost.remove(from);
            } else {
                cost.entry(from.clone())
                    .and_modify(|e| *e -= amount.clone());
            }
            cost.entry(to.clone())
                .and_modify(|e| *e += amount.clone())
                .or_insert(amount);
        }
    }
}

impl_script_shared_methods!(ScriptResourceCost, ResourceAmountMap);

impl From<&ResourceAmountMap> for ScriptResourceCost {
    fn from(cost: &ResourceAmountMap) -> Self {
        ScriptResourceCost::new(cost.clone())
    }
}

/// Script-facing view of an action (or a reaction treated as an action).
#[derive(Clone)]
pub struct ScriptActionView {
    pub action_id: String,
    pub actor: Entity,
    pub action_context: ScriptActionContext,
    pub resource_cost: ScriptResourceCost,
    pub targets: Vec<ScriptEntity>,
}

impl ScriptActionView {
    pub fn new(
        action_id: &ActionId,
        actor: Entity,
        action_context: &ActionContext,
        resource_cost: ScriptResourceCost,
        targets: Vec<ScriptEntity>,
    ) -> Self {
        ScriptActionView {
            action_id: action_id.to_string(),
            actor,
            action_context: ScriptActionContext::from(action_context),
            resource_cost,
            targets,
        }
    }
}

impl From<&ActionData> for ScriptActionView {
    fn from(action: &ActionData) -> Self {
        ScriptActionView {
            action_id: action.action_id.to_string(),
            actor: action.actor,
            action_context: ScriptActionContext::from(&action.context),
            resource_cost: ScriptResourceCost::new(action.resource_cost.clone()),
            targets: action
                .targets
                .iter()
                .filter_map(|t| match t {
                    TargetInstance::Entity(entity) => Some(ScriptEntity::from(*entity)),
                    TargetInstance::Point(_) => None, // TODO: Handle point targets if needed
                })
                .collect(),
        }
    }
}

#[derive(Clone)]
pub struct ScriptActionPerformedView {
    pub action: ScriptActionView,
    pub results: Vec<ScriptActionResultView>,
}

impl ScriptActionPerformedView {
    pub fn new(action: ScriptActionView, results: Vec<ScriptActionResultView>) -> Self {
        Self { action, results }
    }

    pub fn results_len(&self) -> i32 {
        self.results.len() as i32
    }

    pub fn results(&self) -> &Vec<ScriptActionResultView> {
        &self.results
    }
}

#[derive(Clone)]
pub struct ScriptActionResultView {
    pub performer: ScriptEntity,
    pub target: ScriptEntity,
    pub kind: ScriptActionKindResultView,
}

impl ScriptActionResultView {
    pub fn from_action_result(performer: Entity, target: Entity, kind: &ActionKindResult) -> Self {
        Self {
            performer: ScriptEntity::from(performer),
            target: ScriptEntity::from(target),
            kind: ScriptActionKindResultView::from(kind),
        }
    }

    pub fn is_against(&self, entity: &ScriptEntity) -> bool {
        self.target.id == entity.id
    }
}

#[derive(Clone)]
pub enum ScriptActionKindResultView {
    Standard(ScriptActionOutcomeBundleView),
    Utility,
    Composite {
        actions: Vec<ScriptActionKindResultView>,
    },
    Reaction, // You can extend later if you want more details here.
    Custom,
}

impl ScriptActionKindResultView {
    pub fn from(kind: &ActionKindResult) -> Self {
        match kind {
            ActionKindResult::Standard(bundle) => {
                ScriptActionKindResultView::Standard(ScriptActionOutcomeBundleView::from(bundle))
            }
            ActionKindResult::Utility => ScriptActionKindResultView::Utility,
            ActionKindResult::Composite { actions } => ScriptActionKindResultView::Composite {
                actions: actions
                    .iter()
                    .map(ScriptActionKindResultView::from)
                    .collect(),
            },
            ActionKindResult::Reaction { .. } => ScriptActionKindResultView::Reaction,
            ActionKindResult::Custom { .. } => ScriptActionKindResultView::Custom,
        }
    }
}

macro_rules! impl_action_kind_result_accessors {
    ($enum_name:ident {
        $(
            $is_name:ident => $as_name:ident : $variant:ident ( $ty:ty )
        ),+ $(,)?
    }) => {
        impl $enum_name {
            $(
                pub fn $is_name(&self) -> bool {
                    matches!(self, Self::$variant(_))
                }

                pub fn $as_name(&self) -> &$ty {
                    if let Self::$variant(value) = self {
                        value
                    } else {
                        panic!(concat!("Not a ", stringify!($variant), " result"));
                    }
                }
            )+
        }
    };
}

impl_action_kind_result_accessors!(ScriptActionKindResultView {
    is_standard => as_standard: Standard(ScriptActionOutcomeBundleView),
    // if you later add more payload for composite, you can extend similarly
});

#[derive(Clone)]
pub struct ScriptActionOutcomeBundleView {
    damage: Option<ScriptDamageOutcomeView>,
    // effect/healing can be added the same way when you need them
}

impl ScriptActionOutcomeBundleView {
    pub fn from(bundle: &ActionOutcomeBundle) -> Self {
        Self {
            damage: bundle.damage.as_ref().map(ScriptDamageOutcomeView::from),
        }
    }

    pub fn has_damage(&self) -> bool {
        self.damage.is_some()
    }

    pub fn get_damage(&self) -> &ScriptDamageOutcomeView {
        self.damage.as_ref().expect("No damage outcome")
    }
}

#[derive(Clone)]
pub struct ScriptDamageOutcomeView {
    kind: ScriptDamageResolutionKindView,
    damage_roll: Option<ScriptDamageRollResult>,
    damage_taken: Option<ScriptDamageMitigationResult>,
    // If you want: new_life_state: Option<ScriptLifeStateView>
}

impl ScriptDamageOutcomeView {
    pub fn from(outcome: &DamageOutcome) -> Self {
        Self {
            kind: ScriptDamageResolutionKindView::from(&outcome.resolution),
            damage_roll: outcome.damage_roll.clone().map(ScriptDamageRollResult::new),
            damage_taken: outcome
                .damage_taken
                .clone()
                .map(ScriptDamageMitigationResult::new),
        }
    }

    pub fn get_kind(&self) -> &ScriptDamageResolutionKindView {
        &self.kind
    }

    pub fn has_damage_roll(&self) -> bool {
        self.damage_roll.is_some()
    }

    pub fn get_damage_roll(&self) -> &ScriptDamageRollResult {
        self.damage_roll.as_ref().expect("No damage roll")
    }

    pub fn has_damage_taken(&self) -> bool {
        self.damage_taken.is_some()
    }

    pub fn get_damage_taken(&self) -> &ScriptDamageMitigationResult {
        self.damage_taken.as_ref().expect("No damage taken")
    }

    /// Total damage rolled (pre-mitigation). Returns 0 if absent.
    pub fn damage_roll_total(&self) -> i32 {
        self.damage_roll
            .as_ref()
            .map(|v| v.inner.read().total)
            .unwrap_or(0)
    }

    /// Total damage taken (post-mitigation). Returns 0 if absent.
    pub fn damage_taken_total(&self) -> i32 {
        self.damage_taken
            .as_ref()
            .map(|v| v.inner.read().total)
            .unwrap_or(0)
    }
}

#[derive(Clone)]
pub enum ScriptDamageResolutionKindView {
    Unconditional,
    AttackRoll,
    SavingThrow,
}

impl ScriptDamageResolutionKindView {
    pub fn from(kind: &ActionConditionResolution) -> Self {
        match kind {
            ActionConditionResolution::Unconditional => Self::Unconditional,
            ActionConditionResolution::AttackRoll { .. } => Self::AttackRoll,
            ActionConditionResolution::SavingThrow { .. } => Self::SavingThrow,
        }
    }

    pub fn is_unconditional(&self) -> bool {
        matches!(self, Self::Unconditional)
    }

    pub fn is_attack_roll(&self) -> bool {
        matches!(self, Self::AttackRoll)
    }

    pub fn is_saving_throw(&self) -> bool {
        matches!(self, Self::SavingThrow)
    }
}

/// Which entity are we talking about? We keep this abstract so scripts do
/// not need entity IDs, only roles.
#[derive(Clone)]
pub enum ScriptEntityRole {
    /// The entity performing the action. For a reaction, this would be the
    /// entity which performed the event that triggered the reaction.
    Actor,
    /// The entity reacting to the event (only for reactions).
    Reactor,
    /// The target of the action/event.
    Target,
}

impl FromStr for ScriptEntityRole {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "actor" => Ok(ScriptEntityRole::Actor),
            "reactor" => Ok(ScriptEntityRole::Reactor),
            "target" => Ok(ScriptEntityRole::Target),
            _ => Err(format!("Unknown ScriptEntityRole: {}", s)),
        }
    }
}

/// Which event are we referring to?
#[derive(Clone)]
pub enum ScriptEventRef {
    TriggerEvent, // the event that caused this reaction
                  // later: SomeOtherEventById(EventId) if needed
}

/// How to compute a saving throw DC.
#[derive(Clone)]
pub struct ScriptSavingThrow {
    /// Entity role where the saving throw originates
    pub entity: ScriptEntityRole,
    pub saving_throw: SavingThrowDefinition,
}

/// Bonus to apply to a D20 roll.
#[derive(Clone)]
pub enum ScriptDiceRollBonus {
    Flat(IntExpression),
    Dice(DiceExpression),
}

impl ScriptDiceRollBonus {
    pub fn evaluate(
        &self,
        world: &hecs::World,
        entity: Entity,
        action_context: &ActionContext,
    ) -> i32 {
        match self {
            ScriptDiceRollBonus::Flat(expr) => expr
                .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                .unwrap(),
            ScriptDiceRollBonus::Dice(expr) => {
                let (num_dice, size, modifier) = expr
                    .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                    .unwrap();

                DiceSetRoll {
                    dice: DiceSet::from_str(format!("{}d{}", num_dice, size).as_str()).unwrap(),
                    modifiers: ModifierSet::from(ModifierSource::Base, modifier),
                }
                .roll()
                .subtotal
            }
        }
    }
}

impl FromStr for ScriptDiceRollBonus {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(flat) = Parser::new(s).parse_int_expression() {
            Ok(ScriptDiceRollBonus::Flat(flat))
        } else if let Ok(expr) = Parser::new(s).parse_dice_expression() {
            Ok(ScriptDiceRollBonus::Dice(expr))
        } else {
            Err(format!("Invalid ScriptD20Bonus expression: {}", s))
        }
    }
}

/// Plan/description of what the reaction actually does.
/// This is interpreted by Rust; scripts only *describe* the behaviour.
#[derive(Clone)]
pub enum ScriptReactionPlan {
    /// Do nothing.
    None,

    /// Execute multiple steps in order.
    Sequence(Vec<ScriptReactionPlan>),

    /// Add a flat modifier to the most recent D20 roll for this event.
    ModifyD20Result { bonus: ScriptDiceRollBonus },

    /// Add a flat modifier to the DC for this event.
    ModifyD20DC { modifier: ScriptDiceRollBonus },

    /// Reroll the most recent D20 roll for this event with an optional modifier.
    /// Can also be set to force using the new roll.
    RerollD20Result {
        bonus: Option<ScriptDiceRollBonus>,
        force_use_new: bool,
    },

    /// Ask an entity to make a saving throw against a DC.
    /// Then branch into `on_success` or `on_failure`.
    RequireSavingThrow {
        target: ScriptEntityRole,
        dc: ScriptSavingThrow,
        on_success: Box<ScriptReactionPlan>,
        on_failure: Box<ScriptReactionPlan>,
    },

    /// Cancel a specific event (usually the trigger) and maybe refund resources.
    CancelEvent {
        event: ScriptEventRef,
        resources_to_refund: Vec<ResourceId>, // e.g. spell slots
    },
}

/// Result of a reaction body. Scripts can either return the traditional
/// `ScriptReactionPlan` or provide a modified trigger event view.
#[derive(Clone)]
pub enum ScriptReactionBodyResult {
    Plan(ScriptReactionPlan),
    TriggerEvent(ScriptEventView),
}

impl ScriptReactionBodyResult {
    pub fn none() -> Self {
        Self::Plan(ScriptReactionPlan::None)
    }
}

/// Snapshot of a loadout for scripts to inspect.
#[derive(Debug, Clone)]
pub struct ScriptLoadoutView {
    pub loadout: Loadout,
}

impl From<&Loadout> for ScriptLoadoutView {
    fn from(loadout: &Loadout) -> Self {
        ScriptLoadoutView {
            loadout: loadout.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScriptResourceView {
    pub inner: ScriptShared<ResourceMap>,
}

impl ScriptResourceView {
    pub fn can_afford_resource(&self, resource_id: &ResourceId, amount: &ResourceAmount) -> bool {
        return self.inner.read().can_afford(resource_id, amount);
    }

    pub fn add_resource(&mut self, resource_id: &ResourceId, amount: &ResourceAmount) {
        self.inner.write().add(
            resource_id.clone(),
            ResourceBudgetKind::from(amount.clone()),
            true,
        );
    }
}

impl_script_shared_methods!(ScriptResourceView, ResourceMap);
impl_take_replace_world!(ScriptResourceView, ResourceMap);

/// Snapshot of an entity for scripts to inspect.
#[derive(Debug, Clone)]
pub struct ScriptEntityView {
    pub entity: ScriptEntity,
    pub resources: ScriptResourceView,
    pub loadout: ScriptLoadoutView,
    // Add more fields as needed
}

impl ScriptEntityView {
    pub fn new_from_world(world: &World, entity: Entity) -> Self {
        ScriptEntityView {
            entity: ScriptEntity::from(entity),
            resources: ScriptResourceView::new_from_world(world, entity),
            loadout: ScriptLoadoutView::from(&*systems::loadout::loadout(world, entity)),
        }
    }

    pub fn take_from_world(world: &mut World, entity: Entity) -> Self {
        ScriptEntityView {
            entity: ScriptEntity::from(entity),
            resources: ScriptResourceView::take_from_world(world, entity),
            loadout: ScriptLoadoutView::from(&*systems::loadout::loadout(world, entity)),
        }
    }

    pub fn replace_in_world(self, world: &mut World) {
        let entity: Entity = self.entity.clone().into();
        self.resources.replace_in_world(world, entity);
    }
}

#[derive(Clone)]
pub struct ScriptOptionalEntityView {
    pub inner: Option<ScriptEntityView>,
}

impl ScriptOptionalEntityView {
    pub fn empty() -> Self {
        ScriptOptionalEntityView { inner: None }
    }

    pub fn new_from_world(world: &World, entity: Option<Entity>) -> Self {
        if let Some(e) = entity {
            ScriptOptionalEntityView {
                inner: Some(ScriptEntityView::new_from_world(world, e)),
            }
        } else {
            ScriptOptionalEntityView { inner: None }
        }
    }

    pub fn take_from_world(world: &mut World, entity: Option<Entity>) -> Self {
        if let Some(e) = entity {
            ScriptOptionalEntityView {
                inner: Some(ScriptEntityView::take_from_world(world, e)),
            }
        } else {
            ScriptOptionalEntityView { inner: None }
        }
    }

    pub fn replace_in_world(self, world: &mut World) {
        if let Some(view) = self.inner {
            view.replace_in_world(world);
        }
    }

    pub fn is_some(&self) -> bool {
        self.inner.is_some()
    }

    pub fn get(&self) -> &ScriptEntityView {
        self.inner
            .as_ref()
            .expect("No entity in ScriptOptionalEntityView")
    }
}

impl From<&ScriptEntityView> for ScriptOptionalEntityView {
    fn from(view: &ScriptEntityView) -> Self {
        ScriptOptionalEntityView {
            inner: Some(view.clone()),
        }
    }
}

impl From<Option<&ScriptEntityView>> for ScriptOptionalEntityView {
    fn from(view: Option<&ScriptEntityView>) -> Self {
        ScriptOptionalEntityView {
            inner: view.cloned(),
        }
    }
}

/// What the trigger function logically receives.
#[derive(Clone)]
pub struct ScriptReactionTriggerContext {
    pub reactor: ScriptEntity,
    pub event: ScriptEventView,
}

/// What the body function logically receives (you can extend later).
/// TODO: Implement this properly - if it's even need at all?
#[derive(Clone)]
pub struct ScriptReactionBodyContext {
    pub reactor: ScriptEntity,
    pub event: ScriptEventView,
    pub reaction_id: String,
    pub context: ScriptActionContext,
    pub resource_cost: ScriptResourceCost,
}

impl From<&ReactionData> for ScriptReactionBodyContext {
    fn from(data: &ReactionData) -> Self {
        ScriptReactionBodyContext {
            reactor: ScriptEntity::from(data.reactor),
            event: ScriptEventView::from_event_mut(&data.event).unwrap(),
            reaction_id: data.reaction_id.to_string(),
            context: ScriptActionContext::from(&data.context),
            resource_cost: ScriptResourceCost::new(data.resource_cost.clone()),
        }
    }
}
