use std::{
    str::FromStr,
    sync::{Arc, RwLock},
};

use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::{ActionContext, ActionResult},
        dice::{DiceSet, DiceSetRoll},
        id::{EntityIdentifier, ResourceId},
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
    ActionRequested(ActionData),
    ActionPerformed(ActionData, Vec<ActionResult>),
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

            EventKind::ActionRequested { action } => {
                Some(ScriptEventView::ActionRequested(action.clone()))
            }

            EventKind::ReactionRequested { reaction } => {
                let action = ActionData::from(reaction);
                Some(ScriptEventView::ActionRequested(action.clone()))
            }

            EventKind::ActionPerformed { action, results } => Some(
                ScriptEventView::ActionPerformed(action.clone(), results.clone()),
            ),

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

    pub fn is_action_requested(&self) -> bool {
        matches!(self, ScriptEventView::ActionRequested(_))
    }

    pub fn as_action_requested(&self) -> &ActionData {
        if let ScriptEventView::ActionRequested(action) = self {
            action
        } else {
            panic!("Not an ActionRequested event view");
        }
    }

    pub fn is_action_performed(&self) -> bool {
        matches!(self, ScriptEventView::ActionPerformed(_, _))
    }

    pub fn as_action_performed(&self) -> (ActionData, Vec<ActionResult>) {
        if let ScriptEventView::ActionPerformed(action, results) = self {
            (action.clone(), results.clone())
        } else {
            panic!("Not an ActionPerformed event view");
        }
    }

    pub fn is_d20_check_performed(&self) -> bool {
        matches!(self, ScriptEventView::D20CheckPerformed(_))
    }

    pub fn as_d20_check_performed(&self) -> &ScriptD20CheckView {
        if let ScriptEventView::D20CheckPerformed(view) = self {
            view
        } else {
            panic!("Not a D20CheckPerformed event view");
        }
    }

    pub fn is_moving_out_of_reach(&self) -> bool {
        matches!(self, ScriptEventView::MovingOutOfReach(_))
    }

    pub fn as_moving_out_of_reach(&self) -> &ScriptMovingOutOfReachView {
        if let ScriptEventView::MovingOutOfReach(view) = self {
            view
        } else {
            panic!("Not a MovingOutOfReach event view");
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
    pub context: ActionContext,
    pub resource_cost: ResourceAmountMap,
}

impl From<&ReactionData> for ScriptReactionBodyContext {
    fn from(data: &ReactionData) -> Self {
        ScriptReactionBodyContext {
            reactor: ScriptEntity::from(data.reactor.id()),
            event: ScriptEventView::from_event(&data.event).unwrap(),
            reaction_id: data.reaction_id.to_string(),
            context: data.context.clone(),
            resource_cost: data.resource_cost.clone(),
        }
    }
}
