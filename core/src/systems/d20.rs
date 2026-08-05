use hecs::{Entity, Ref, World};

use crate::{
    components::{
        d20::{D20Check, D20CheckDC, D20CheckKind, D20CheckResult},
        damage::AttackSource,
        id::EntityIdentifier,
        items::equipment::loadout::Loadout,
        saving_throw::SavingThrowSet,
        skill::SkillSet,
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

pub fn check_no_event(game_state: &GameState, entity: Entity, dc: &D20CheckDC) -> D20CheckResult {
    // TODO: Figure out a way around this
    if let D20CheckDC::AttackRoll { .. } = dc {
        panic!("check_no_event cannot be used for attack rolls; use check_attack instead");
    }

    get(&game_state.world, entity, dc.kind())
        .roll_dc(game_state, entity, dc)
        .unwrap()
}

#[must_use]
pub fn check(game_state: &mut GameState, entity: Entity, dc: &D20CheckDC) -> Event {
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
    systems::effects::effects(&game_state.world, target).on_attacked(
        &game_state.world,
        target,
        attacker,
        &mut check,
    );

    let result = check.roll_hooks(game_state, attacker);

    let armor_class = systems::loadout::armor_class(game_state, target);

    Event::new(EventKind::D20CheckPerformed {
        actor: EntityIdentifier::from_world(&game_state.world, attacker),
        result,
        dc: D20CheckDC::AttackRoll {
            target: EntityIdentifier::from_world(&game_state.world, target),
            source,
            armor_class,
        },
    })
}
