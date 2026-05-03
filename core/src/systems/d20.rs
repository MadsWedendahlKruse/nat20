use hecs::{Entity, World};

use crate::{
    components::{
        d20::{D20Check, D20CheckDC, D20CheckResult},
        damage::{AttackRollResult, AttackSource},
        id::EntityIdentifier,
        items::equipment::{armor::ArmorClass, loadout::Loadout},
        saving_throw::{SavingThrowKind, SavingThrowSet},
        skill::{Skill, SkillSet},
        spells::spellbook::Spellbook,
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
    AttackRoll(AttackSource),
}

// The D20Check is a temporary reference, so we can't return it from here
pub fn get_mut(
    world: &mut World,
    entity: Entity,
    kind: &D20CheckKind,
    mutator: impl FnOnce(&mut D20Check),
) {
    match kind {
        D20CheckKind::SavingThrow(saving_throw) => mutator(
            &mut systems::helpers::get_component_mut::<SavingThrowSet>(world, entity)
                .get_mut(saving_throw),
        ),

        D20CheckKind::Skill(skill) => mutator(
            &mut systems::helpers::get_component_mut::<SkillSet>(world, entity).get_mut(skill),
        ),

        D20CheckKind::AttackRoll(source) => match source {
            AttackSource::Weapon(weapon_kind) => {
                mutator(
                    &mut systems::helpers::get_component_mut::<Loadout>(world, entity)
                        .attack_roll_template_mut(weapon_kind)
                        .d20_check,
                );
            }

            AttackSource::Spell => mutator(
                &mut systems::helpers::get_component_mut::<Spellbook>(world, entity)
                    .attack_roll_template_mut()
                    .d20_check,
            ),
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum D20CheckDCKind {
    SavingThrow(D20CheckDC<SavingThrowKind>),
    Skill(D20CheckDC<Skill>),
    AttackRoll(EntityIdentifier, ArmorClass),
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
                result.is_success(armor_class)
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
        EntityIdentifier::from_world(&game_state.world, entity),
        check_no_event(&game_state.world, entity, dc),
        dc.clone(),
    ))
}
