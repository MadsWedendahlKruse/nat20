use hecs::{Entity, World};

use crate::{
    components::{
        d20::{D20CheckDC, D20CheckOutcome, D20CheckResult},
        damage::AttackRollResult,
        items::equipment::armor::ArmorClass,
        modifier::Modifiable,
        saving_throw::{SavingThrowKind, SavingThrowSet},
        skill::{Skill, SkillSet},
    },
    engine::{
        event::{Event, EventKind},
        game_state::GameState,
    },
    systems,
};

// TODO: Do we even need it without the DC? Does that make sense?
#[derive(Debug, Clone)]
pub enum D20CheckKind {
    SavingThrow(SavingThrowKind),
    Skill(Skill),
    AttackRoll,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum D20CheckDCKind {
    SavingThrow(D20CheckDC<SavingThrowKind>),
    Skill(D20CheckDC<Skill>),
    AttackRoll(Entity, ArmorClass),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum D20ResultKind {
    SavingThrow {
        kind: SavingThrowKind,
        result: D20CheckResult,
    },
    Skill {
        skill: Skill,
        result: D20CheckResult,
    },
    AttackRoll {
        result: AttackRollResult,
    },
}

impl D20ResultKind {
    pub fn is_success(&self, dc: &D20CheckDCKind) -> bool {
        match (self, dc) {
            (D20ResultKind::SavingThrow { result, .. }, D20CheckDCKind::SavingThrow(dc)) => {
                result.is_success(dc)
            }
            (D20ResultKind::Skill { result, .. }, D20CheckDCKind::Skill(dc)) => {
                result.is_success(dc)
            }
            (D20ResultKind::AttackRoll { result }, D20CheckDCKind::AttackRoll(_, armor_class)) => {
                let result = &result.roll_result;
                match result.outcome {
                    Some(D20CheckOutcome::CriticalSuccess) => true,
                    Some(D20CheckOutcome::CriticalFailure) => false,
                    _ => result.total() >= armor_class.total() as u32,
                }
            }
            _ => false,
        }
    }

    pub fn d20_result(&self) -> &D20CheckResult {
        match self {
            D20ResultKind::SavingThrow { result, .. } => result,
            D20ResultKind::Skill { result, .. } => result,
            D20ResultKind::AttackRoll { result } => &result.roll_result,
        }
    }

    pub fn d20_result_mut(&mut self) -> &mut D20CheckResult {
        match self {
            D20ResultKind::SavingThrow { result, .. } => result,
            D20ResultKind::Skill { result, .. } => result,
            D20ResultKind::AttackRoll { result } => &mut result.roll_result,
        }
    }
}

pub fn check_no_event(world: &World, entity: Entity, dc: &D20CheckDCKind) -> D20ResultKind {
    match dc {
        D20CheckDCKind::SavingThrow(dc) => D20ResultKind::SavingThrow {
            kind: dc.key,
            result: systems::helpers::get_component::<SavingThrowSet>(world, entity)
                .check_dc(dc, world, entity),
        },
        D20CheckDCKind::Skill(dc) => D20ResultKind::Skill {
            skill: dc.key,
            result: systems::helpers::get_component::<SkillSet>(world, entity)
                .check_dc(dc, world, entity),
        },
        // D20CheckDCKind::AttackRoll(slot, target, armor_class) => D20ResultKind::AttackRoll {
        //     result: systems::combat::attack_roll_against_target(world, entity, slot, target),
        // },
        D20CheckDCKind::AttackRoll(_, _) => {
            todo!("systems::d20 attack roll checks are not yet implemented");
        }
    }
}

#[must_use]
pub fn check(game_state: &mut GameState, entity: Entity, dc: &D20CheckDCKind) -> Event {
    Event::new(EventKind::D20CheckPerformed(
        entity,
        check_no_event(&game_state.world, entity, dc),
        dc.clone(),
    ))
}
