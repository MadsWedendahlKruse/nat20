use hecs::{Entity, Ref, World};

use crate::{
    components::{
        d20::{AdvantageAware, AdvantageType, D20Check, D20CheckDC, D20CheckKind, D20CheckResult},
        damage::AttackSource,
        id::EntityIdentifier,
        items::equipment::{armor::ArmorClass, loadout::Loadout},
        modifier::{FlatModifiable, ModifierResult, ModifierSource},
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

pub fn get(world: &World, entity: Entity, kind: D20CheckKind) -> Ref<'_, D20Check> {
    match kind {
        D20CheckKind::SavingThrow(saving_throw) => Ref::map(
            systems::helpers::get_component::<SavingThrowSet>(world, entity),
            |set| set.get(&saving_throw),
        ),

        D20CheckKind::Skill(skill) => Ref::map(
            systems::helpers::get_component::<SkillSet>(world, entity),
            |set| set.get(&skill),
        ),

        D20CheckKind::AttackRoll(source) => match source {
            AttackSource::Weapon(weapon_kind) => Ref::map(
                systems::helpers::get_component::<Loadout>(world, entity),
                |loadout| loadout.attack_roll_template(&weapon_kind),
            ),

            AttackSource::Spell => Ref::map(
                systems::helpers::get_component::<Spellbook>(world, entity),
                |spellbook| spellbook.attack_roll_template(),
            ),
        },
    }
}

pub fn get_mut(world: &mut World, entity: Entity, kind: D20CheckKind) -> &mut D20Check {
    match kind {
        D20CheckKind::SavingThrow(saving_throw) => {
            systems::helpers::get_component_mut::<SavingThrowSet>(world, entity)
                .get_mut(&saving_throw)
        }

        D20CheckKind::Skill(skill) => {
            systems::helpers::get_component_mut::<SkillSet>(world, entity).get_mut(&skill)
        }

        D20CheckKind::AttackRoll(source) => match source {
            AttackSource::Weapon(weapon_kind) => {
                systems::helpers::get_component_mut::<Loadout>(world, entity)
                    .attack_roll_template_mut(&weapon_kind)
            }

            AttackSource::Spell => systems::helpers::get_component_mut::<Spellbook>(world, entity)
                .attack_roll_template_mut(),
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum D20CheckDCKind {
    SavingThrow(D20CheckDC<SavingThrowKind>),
    Skill(D20CheckDC<Skill>),
    AttackRoll(EntityIdentifier, AttackSource, ArmorClass),
}

impl D20CheckDCKind {
    pub fn saving_throw(kind: SavingThrowKind, dc: ModifierResult) -> Self {
        D20CheckDCKind::SavingThrow(D20CheckDC { key: kind, dc })
    }

    pub fn skill_check(skill: Skill, dc: ModifierResult) -> Self {
        D20CheckDCKind::Skill(D20CheckDC { key: skill, dc })
    }

    pub fn attack_roll(
        target: EntityIdentifier,
        source: AttackSource,
        armor_class: ArmorClass,
    ) -> Self {
        D20CheckDCKind::AttackRoll(target, source, armor_class)
    }

    pub fn kind(&self) -> D20CheckKind {
        match self {
            D20CheckDCKind::SavingThrow(dc) => D20CheckKind::SavingThrow(dc.key),
            D20CheckDCKind::Skill(dc) => D20CheckKind::Skill(dc.key),
            D20CheckDCKind::AttackRoll(_, source, _) => D20CheckKind::AttackRoll(source.clone()),
        }
    }

    pub fn dc_total(&self) -> u32 {
        match self {
            D20CheckDCKind::SavingThrow(dc) => dc.dc.total() as u32,
            D20CheckDCKind::Skill(dc) => dc.dc.total() as u32,
            D20CheckDCKind::AttackRoll(_, _, armor_class) => armor_class.total() as u32,
        }
    }
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
        source: AttackSource,
        result: D20CheckResult,
    },
}

impl D20ResultKind {
    pub fn is_success(&self, dc: &D20CheckDCKind) -> bool {
        self.d20_result().is_success_vs(dc.dc_total())
    }

    pub fn d20_result(&self) -> &D20CheckResult {
        match self {
            D20ResultKind::SavingThrow { result, .. }
            | D20ResultKind::Skill { result, .. }
            | D20ResultKind::AttackRoll { result, .. } => result,
        }
    }

    pub fn d20_result_mut(&mut self) -> &mut D20CheckResult {
        match self {
            D20ResultKind::SavingThrow { result, .. }
            | D20ResultKind::Skill { result, .. }
            | D20ResultKind::AttackRoll { result, .. } => result,
        }
    }

    pub fn kind(&self) -> D20CheckKind {
        match self {
            D20ResultKind::SavingThrow { kind, .. } => D20CheckKind::SavingThrow(*kind),
            D20ResultKind::Skill { skill, .. } => D20CheckKind::Skill(*skill),
            D20ResultKind::AttackRoll { source, .. } => D20CheckKind::AttackRoll(source.clone()),
        }
    }

    pub fn reroll(&self) -> D20ResultKind {
        let mut new_result = self.clone();
        *(new_result.d20_result_mut()) = new_result.d20_result().reroll();
        new_result
    }
}

impl AdvantageAware for D20ResultKind {
    fn add_advantage(&mut self, kind: AdvantageType, source: ModifierSource) {
        self.d20_result_mut().add_advantage(kind, source);
    }

    fn remove_advantage(&mut self, source: &ModifierSource) {
        self.d20_result_mut().remove_advantage(source);
    }
}

pub fn check_no_event(
    game_state: &GameState,
    entity: Entity,
    dc: &D20CheckDCKind,
) -> D20ResultKind {
    match dc {
        D20CheckDCKind::SavingThrow(dc) => D20ResultKind::SavingThrow {
            kind: dc.key,
            result: systems::helpers::get_component::<SavingThrowSet>(&game_state.world, entity)
                .check_dc(dc, game_state, entity),
        },
        D20CheckDCKind::Skill(dc) => D20ResultKind::Skill {
            skill: dc.key,
            result: systems::helpers::get_component::<SkillSet>(&game_state.world, entity)
                .check_dc(dc, game_state, entity),
        },
        D20CheckDCKind::AttackRoll(_, _, _) => {
            // An attack's D20Check depends on the action context (weapon in
            // hand etc.), so it can't be derived from the DC alone
            panic!(
                "Attack rolls are rolled through systems::d20::check_attack, not check_no_event"
            );
        }
    }
}

#[must_use]
pub fn check(game_state: &mut GameState, entity: Entity, dc: &D20CheckDCKind) -> Event {
    Event::new(EventKind::D20CheckPerformed {
        actor: EntityIdentifier::from_world(&game_state.world, entity),
        result: check_no_event(game_state, entity, dc),
        dc: dc.clone(),
    })
}

#[must_use]
pub fn check_attack(
    game_state: &mut GameState,
    attacker: Entity,
    target: Entity,
    source: AttackSource,
    mut check: D20Check,
) -> Event {
    let attacked_hooks = systems::effects::effects_mut(&mut game_state.world, target)
        .collect_one_shot_hooks_with_instance(|effect| effect.on_attacked.as_ref());
    for (hook, instance) in &attacked_hooks {
        hook(&game_state.world, instance, target, attacker, &mut check);
    }

    let result = check.roll_hooks(game_state, attacker);

    let armor_class = systems::loadout::armor_class(game_state, target);

    Event::new(EventKind::D20CheckPerformed {
        actor: EntityIdentifier::from_world(&game_state.world, attacker),
        result: D20ResultKind::AttackRoll { source, result },
        dc: D20CheckDCKind::AttackRoll(
            EntityIdentifier::from_world(&game_state.world, target),
            source,
            armor_class,
        ),
    })
}
