use std::{
    str::FromStr,
    sync::{Arc, RwLock},
};

use hecs::{Entity, World};

use crate::{
    components::{
        actions::{
            action::{
                ActionConditionResolution, ActionContext, ActionKindResult, ActionOutcomeBundle,
                DamageOutcome,
            },
            targeting::TargetInstance,
        },
        damage::{DamageMitigationResult, DamageRollResult},
        dice::{DiceSet, DiceSetRoll},
        id::{ActionId, EntityIdentifier, ResourceId},
        modifier::{Modifiable, ModifierSet, ModifierSource},
        resource::ResourceAmountMap,
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

impl From<EntityIdentifier> for ScriptEntity {
    fn from(identifier: EntityIdentifier) -> Self {
        ScriptEntity::from(identifier.id())
    }
}

impl Into<Entity> for ScriptEntity {
    fn into(self) -> Entity {
        Entity::from_bits(self.id).unwrap()
    }
}

impl From<u64> for ScriptEntity {
    fn from(id: u64) -> Self {
        ScriptEntity { id }
    }
}

#[derive(Clone)]
pub struct ScriptD20CheckDCKind {
    pub label: String,
    pub dc: i32,
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
            D20CheckDCKind::AttackRoll(target_entity, _, armor_class) => ScriptD20CheckDCKind {
                label: "AttackRoll".to_string(),
                dc: armor_class.total() as i32,
                target: Some(ScriptEntity::from(target_entity.id())),
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
pub(crate) enum ScriptD20CheckModification {
    ModifyResult {
        bonus: ScriptDiceRollBonus,
    },
    ModifyDC {
        modifier: ScriptDiceRollBonus,
    },
    RerollResult {
        bonus: Option<ScriptDiceRollBonus>,
        force_use_new: bool,
    },
}

#[derive(Clone)]
pub(crate) struct ScriptD20Check {
    pub performer: ScriptEntity,
    pub result_kind: D20ResultKind,
    pub dc_kind: D20CheckDCKind,
    pub modifications: Vec<ScriptD20CheckModification>,
}

/// View of a "D20CheckPerformed" event. Holds the modifications the script
/// asked for; the engine applies them back to the actual event after the hook
/// returns.
///
/// The inner data is wrapped in `Arc<RwLock<_>>` so that *clones share state*.
/// This is essential because Lua field/method access returns owned clones —
/// without sharing, mutations to one clone would be invisible to the original.
/// Pattern: script calls `event:as_d20_check_performed():modify_result(...)`
/// and then `return event`; the engine reads back the modifications from the
/// returned event's view, which sees the same `Arc` storage.
#[derive(Clone)]
pub struct ScriptD20CheckView {
    pub(crate) inner: Arc<RwLock<ScriptD20Check>>,
}

impl ScriptD20CheckView {
    pub fn from_parts(
        performer: Entity,
        result_kind: &D20ResultKind,
        dc_kind: &D20CheckDCKind,
    ) -> Self {
        ScriptD20CheckView {
            inner: Arc::new(RwLock::new(ScriptD20Check {
                performer: ScriptEntity::from(performer),
                result_kind: result_kind.clone(),
                dc_kind: dc_kind.clone(),
                modifications: Vec::new(),
            })),
        }
    }

    pub fn performer(&self) -> ScriptEntity {
        self.inner.read().unwrap().performer.clone()
    }

    pub fn result(&self) -> ScriptD20Result {
        let inner = self.inner.read().unwrap();
        ScriptD20Result::from(&inner.result_kind, &inner.dc_kind)
    }

    pub fn modify_result(&mut self, bonus: ScriptDiceRollBonus) {
        self.inner
            .write()
            .unwrap()
            .modifications
            .push(ScriptD20CheckModification::ModifyResult { bonus });
    }

    pub fn modify_dc(&mut self, modifier: ScriptDiceRollBonus) {
        self.inner
            .write()
            .unwrap()
            .modifications
            .push(ScriptD20CheckModification::ModifyDC { modifier });
    }

    pub fn reroll_result(&mut self, bonus: Option<ScriptDiceRollBonus>, force_use_new: bool) {
        self.inner
            .write()
            .unwrap()
            .modifications
            .push(ScriptD20CheckModification::RerollResult {
                bonus,
                force_use_new,
            });
    }

    pub fn apply_to_event(&self, world: &World, reaction_data: &ReactionData, event: &mut Event) {
        let EventKind::D20CheckPerformed(performer, existing_result, dc_kind) = &mut event.kind
        else {
            panic!(
                "ScriptD20CheckView applied to wrong event type: {:?}",
                event
            );
        };

        let inner = self.inner.read().unwrap();
        for modification in &inner.modifications {
            match modification {
                ScriptD20CheckModification::ModifyResult { bonus } => {
                    let bonus_value =
                        bonus.evaluate(world, reaction_data.reactor.id(), &reaction_data.context);

                    match existing_result {
                        D20ResultKind::Skill { result, .. }
                        | D20ResultKind::SavingThrow { result, .. } => {
                            result.add_bonus(
                                ModifierSource::Action(reaction_data.reaction_id.clone()),
                                bonus_value,
                            );
                        }
                        D20ResultKind::AttackRoll { result } => {
                            result.roll_result.add_bonus(
                                ModifierSource::Action(reaction_data.reaction_id.clone()),
                                bonus_value,
                            );
                        }
                    }
                }

                ScriptD20CheckModification::ModifyDC { modifier } => {
                    let modifier_value = modifier.evaluate(
                        world,
                        reaction_data.reactor.id(),
                        &reaction_data.context,
                    );

                    match dc_kind {
                        D20CheckDCKind::SavingThrow(d20_check_dc) => {
                            d20_check_dc.dc.add_modifier(
                                ModifierSource::Action(reaction_data.reaction_id.clone()),
                                modifier_value,
                            );
                        }
                        D20CheckDCKind::Skill(d20_check_dc) => {
                            d20_check_dc.dc.add_modifier(
                                ModifierSource::Action(reaction_data.reaction_id.clone()),
                                modifier_value,
                            );
                        }
                        D20CheckDCKind::AttackRoll(_, _, armor_class) => {
                            armor_class.add_modifier(
                                ModifierSource::Action(reaction_data.reaction_id.clone()),
                                modifier_value,
                            );
                        }
                    }
                }

                ScriptD20CheckModification::RerollResult {
                    bonus,
                    force_use_new,
                } => {
                    let bonus_value = if let Some(bonus_expr) = bonus {
                        bonus_expr.evaluate(
                            world,
                            reaction_data.reactor.id(),
                            &reaction_data.context,
                        )
                    } else {
                        0
                    };

                    let mut new_roll = systems::d20::check_no_event(world, performer.id(), dc_kind);
                    new_roll.d20_result_mut().add_bonus(
                        ModifierSource::Action(reaction_data.reaction_id.clone()),
                        bonus_value,
                    );

                    if *force_use_new {
                        *existing_result = new_roll;
                    } else {
                        let existing_total = existing_result.d20_result().total();
                        let new_total = new_roll.d20_result().total();
                        if new_total > existing_total {
                            *existing_result = new_roll;
                        }
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Moving-out-of-reach event view
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub(crate) struct ScriptMovingOutOfReach {
    pub mover: ScriptEntity,
    pub entity: ScriptEntity,
    pub continue_movement: bool,
}

/// View of a "MovingOutOfReach" event. Reactions like Opportunity Attack can
/// mutate `continue_movement`, and the engine reads it back after the hook
/// returns. The inner data is `Arc<RwLock<_>>` so clones share state (see
/// [`ScriptD20CheckView`] for the rationale).
#[derive(Clone)]
pub struct ScriptMovingOutOfReachView {
    pub(crate) inner: Arc<RwLock<ScriptMovingOutOfReach>>,
}

impl ScriptMovingOutOfReachView {
    pub fn new(mover: ScriptEntity, entity: ScriptEntity, continue_movement: bool) -> Self {
        Self {
            inner: Arc::new(RwLock::new(ScriptMovingOutOfReach {
                mover,
                entity,
                continue_movement,
            })),
        }
    }

    pub fn apply_to_event(&self, world: &World, event: &mut Event) {
        if let EventKind::MovingOutOfReach {
            mover,
            entity,
            continue_movement,
        } = &mut event.kind
        {
            let inner = self.inner.read().unwrap();
            *mover = EntityIdentifier::from_world(world, inner.mover.clone().into());
            *entity = EntityIdentifier::from_world(world, inner.entity.clone().into());
            *continue_movement = inner.continue_movement;
        } else {
            panic!(
                "ScriptMovingOutOfReachView applied to wrong event type: {:?}",
                event
            );
        }
    }
}

// ---------------------------------------------------------------------------
// ScriptEventView — top-level "what kind of event triggered this hook" enum.
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub enum ScriptEventView {
    ActionRequested(ScriptActionView),
    ActionPerformed(ScriptActionPerformedView),
    D20CheckPerformed(ScriptD20CheckView),
    MovingOutOfReach(ScriptMovingOutOfReachView),
}

impl ScriptEventView {
    pub fn from_event(event: &Event) -> Option<Self> {
        match &event.kind {
            EventKind::D20CheckPerformed(performer, result_kind, dc_kind) => {
                Some(ScriptEventView::D20CheckPerformed(
                    ScriptD20CheckView::from_parts(performer.id(), result_kind, dc_kind),
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
                    if let TargetInstance::Entity { entity, .. } = &result.target {
                        script_results.push(ScriptActionResultView::from_action_result(
                            action.actor.id(),
                            entity.id(),
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
            } => Some(ScriptEventView::MovingOutOfReach(
                ScriptMovingOutOfReachView::new(
                    ScriptEntity::from(mover.clone()),
                    ScriptEntity::from(entity.clone()),
                    *continue_movement,
                ),
            )),

            _ => None,
        }
    }

    pub fn apply_to_event(&self, world: &World, reaction_data: &ReactionData, event: &mut Event) {
        match self {
            ScriptEventView::D20CheckPerformed(view) => {
                view.apply_to_event(world, reaction_data, event)
            }
            ScriptEventView::MovingOutOfReach(view) => view.apply_to_event(world, event),
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

// ---------------------------------------------------------------------------
// Action views
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub struct ScriptActionContext {
    pub inner: ActionContext,
}

impl ScriptActionContext {
    pub fn is_spell(&self) -> bool {
        self.inner.is_spell()
    }

    pub fn is_attack_action(&self) -> bool {
        self.inner.is_attack_action()
    }

    pub fn is_weapon_attack(&self) -> bool {
        self.inner.is_weapon_attack()
    }

    pub fn is_unarmed_attack(&self) -> bool {
        self.inner.is_unarmed_attack()
    }

    pub fn is_melee_attack(&self) -> bool {
        self.inner.is_melee_attack()
    }

    pub fn is_ranged_attack(&self) -> bool {
        self.inner.is_ranged_attack()
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
pub struct ScriptActionView {
    pub action_id: String,
    pub actor: ScriptEntity,
    pub action_context: ScriptActionContext,
    pub targets: Vec<ScriptEntity>,
    /// Snapshot of the action's resource cost at the time the view was built.
    /// Scripts use this (via `action:costs_resource(id)`) to distinguish e.g.
    /// "the action originally cost an action resource and that wasn't yet
    /// substituted" from "the cost was substituted (so this is an extra attack)".
    pub resource_cost: ResourceAmountMap,
}

impl ScriptActionView {
    pub fn new(
        action_id: &ActionId,
        actor: Entity,
        action_context: &ActionContext,
        targets: Vec<ScriptEntity>,
        resource_cost: ResourceAmountMap,
    ) -> Self {
        ScriptActionView {
            action_id: action_id.to_string(),
            actor: ScriptEntity::from(actor),
            action_context: ScriptActionContext::from(action_context),
            targets,
            resource_cost,
        }
    }

    pub fn costs_resource(&self, resource_id: &ResourceId) -> bool {
        self.resource_cost.map.contains_key(resource_id)
    }
}

impl From<&ActionData> for ScriptActionView {
    fn from(action: &ActionData) -> Self {
        ScriptActionView {
            action_id: action.action_id.to_string(),
            actor: ScriptEntity::from(action.actor.id()),
            action_context: ScriptActionContext::from(&action.context),
            targets: action
                .targets
                .iter()
                .filter_map(|t| match t {
                    TargetInstance::Entity { entity, .. } => Some(ScriptEntity::from(entity.id())),
                    TargetInstance::Point(_) => None,
                })
                .collect(),
            resource_cost: action.resource_cost.clone(),
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

    pub fn results(&self) -> &Vec<ScriptActionResultView> {
        &self.results
    }

    pub fn resolution(&self) -> Option<ScriptActionConditionResolution> {
        for result in &self.results {
            if let ScriptActionKindResultView::Standard(bundle) = &result.kind {
                if let Some(resolution) = bundle.get_resolution() {
                    return Some(resolution.clone());
                }
            }
        }
        None
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
}

#[derive(Clone)]
pub enum ScriptActionKindResultView {
    Standard(ScriptActionOutcomeBundleView),
    Composite {
        actions: Vec<ScriptActionKindResultView>,
    },
    Reaction,
}

impl ScriptActionKindResultView {
    pub fn from(kind: &ActionKindResult) -> Self {
        match kind {
            ActionKindResult::Standard(bundle) => {
                ScriptActionKindResultView::Standard(ScriptActionOutcomeBundleView::from(bundle))
            }
            ActionKindResult::Composite { actions } => ScriptActionKindResultView::Composite {
                actions: actions
                    .iter()
                    .map(ScriptActionKindResultView::from)
                    .collect(),
            },
            ActionKindResult::Reaction { .. } => ScriptActionKindResultView::Reaction,
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
});

#[derive(Clone)]
pub struct ScriptActionOutcomeBundleView {
    damage: Option<ScriptDamageOutcomeView>,
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

    pub fn get_resolution(&self) -> Option<&ScriptActionConditionResolution> {
        self.damage.as_ref().map(|damage| damage.get_resolution())
    }

    pub fn has_attack_hit(&self) -> bool {
        self.damage
            .as_ref()
            .is_some_and(|damage| damage.resolution.is_attack_roll_hit())
    }

    pub fn has_attack_critical_hit(&self) -> bool {
        self.damage
            .as_ref()
            .is_some_and(|damage| damage.resolution.is_attack_roll_critical_hit())
    }

    pub fn has_attack_miss(&self) -> bool {
        self.damage.as_ref().is_some_and(|damage| {
            damage.resolution.is_attack_roll() && !damage.resolution.is_attack_roll_hit()
        })
    }
}

/// View of a damage outcome inside an action result. The script can read
/// totals/details but does not get to mutate `DamageRollResult` /
/// `DamageMitigationResult` from this context — those are already finalized.
#[derive(Clone)]
pub struct ScriptDamageOutcomeView {
    resolution: ScriptActionConditionResolution,
    damage_roll: Option<DamageRollResult>,
    damage_taken: Option<DamageMitigationResult>,
}

impl ScriptDamageOutcomeView {
    pub fn from(outcome: &DamageOutcome) -> Self {
        Self {
            resolution: ScriptActionConditionResolution::from(&outcome.resolution),
            damage_roll: outcome.damage_roll.clone(),
            damage_taken: outcome.damage_taken.clone(),
        }
    }

    pub fn get_resolution(&self) -> &ScriptActionConditionResolution {
        &self.resolution
    }

    pub fn has_damage_roll(&self) -> bool {
        self.damage_roll.is_some()
    }

    pub fn get_damage_roll(&self) -> &DamageRollResult {
        self.damage_roll.as_ref().expect("No damage roll")
    }

    pub fn has_damage_taken(&self) -> bool {
        self.damage_taken.is_some()
    }

    pub fn get_damage_taken(&self) -> &DamageMitigationResult {
        self.damage_taken.as_ref().expect("No damage taken")
    }

    pub fn damage_roll_total(&self) -> i32 {
        self.damage_roll.as_ref().map(|v| v.total).unwrap_or(0)
    }

    pub fn damage_taken_total(&self) -> i32 {
        self.damage_taken.as_ref().map(|v| v.total).unwrap_or(0)
    }
}

#[derive(Clone)]
pub struct ScriptActionConditionResolution {
    inner: ActionConditionResolution,
}

impl ScriptActionConditionResolution {
    pub fn is_unconditional(&self) -> bool {
        matches!(self.inner, ActionConditionResolution::Unconditional)
    }

    pub fn is_attack_roll(&self) -> bool {
        matches!(self.inner, ActionConditionResolution::AttackRoll { .. })
    }

    pub fn is_saving_throw(&self) -> bool {
        matches!(self.inner, ActionConditionResolution::SavingThrow { .. })
    }

    pub fn is_success(&self) -> bool {
        self.inner.is_success()
    }

    pub fn is_attack_roll_hit(&self) -> bool {
        self.is_attack_roll() && self.is_success()
    }

    pub fn is_attack_roll_critical_hit(&self) -> bool {
        self.is_attack_roll() && self.inner.is_crit()
    }

    pub fn is_saving_throw_success(&self) -> bool {
        self.is_saving_throw() && self.is_success()
    }
}

impl From<&ActionConditionResolution> for ScriptActionConditionResolution {
    fn from(resolution: &ActionConditionResolution) -> Self {
        ScriptActionConditionResolution {
            inner: resolution.clone(),
        }
    }
}

impl From<ScriptActionConditionResolution> for ActionConditionResolution {
    fn from(script_resolution: ScriptActionConditionResolution) -> Self {
        script_resolution.inner
    }
}

#[derive(Clone)]
pub enum ScriptEntityRole {
    Actor,
    Reactor,
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

#[derive(Clone)]
pub enum ScriptEventRef {
    TriggerEvent,
}

#[derive(Clone)]
pub struct ScriptSavingThrow {
    pub entity: ScriptEntityRole,
    pub saving_throw: SavingThrowDefinition,
}

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

// TODO: Can this be replaced with GameState methods inside the scripts?
#[derive(Clone)]
pub enum ScriptReactionPlan {
    None,
    Sequence(Vec<ScriptReactionPlan>),
    RequireSavingThrow {
        target: ScriptEntityRole,
        dc: ScriptSavingThrow,
        on_success: Box<ScriptReactionPlan>,
        on_failure: Box<ScriptReactionPlan>,
    },
    CancelEvent {
        event: ScriptEventRef,
        resources_to_refund: Vec<ResourceId>,
    },
}

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

#[derive(Clone)]
pub struct ScriptReactionTriggerContext {
    pub reactor: ScriptEntity,
    pub event: ScriptEventView,
}

#[derive(Clone)]
pub struct ScriptReactionBodyContext {
    pub reactor: ScriptEntity,
    pub event: ScriptEventView,
    pub reaction_id: String,
    pub context: ScriptActionContext,
    pub resource_cost: ResourceAmountMap,
}

impl From<&ReactionData> for ScriptReactionBodyContext {
    fn from(data: &ReactionData) -> Self {
        ScriptReactionBodyContext {
            reactor: ScriptEntity::from(data.reactor.id()),
            event: ScriptEventView::from_event(&data.event).unwrap(),
            reaction_id: data.reaction_id.to_string(),
            context: ScriptActionContext::from(&data.context),
            resource_cost: data.resource_cost.clone(),
        }
    }
}
