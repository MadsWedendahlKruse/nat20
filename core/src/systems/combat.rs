use std::collections::HashSet;

use hecs::Entity;
use serde::{Deserialize, Serialize};
use tracing::warn;

use crate::{
    components::{
        actions::targeting::EntityFilter,
        d20::{D20CheckDC, D20CheckResult},
        health::life_state::{DEATH_SAVING_THROW_DC, LifeState},
        modifier::{ModifierMap, ModifierSource},
        saving_throw::SavingThrowKind,
        skill::{Skill, SkillSet},
        time::{TimeMode, TimeStep, TurnBoundary},
    },
    engine::{
        action_prompt::{ActionPrompt, ActionPromptKind},
        encounter::{Encounter, EncounterId},
        engine_state::EngineState,
        event::{CallbackResult, EncounterEvent, Event, EventCallback, EventKind, EventScope},
    },
    systems,
};

// TODO: Not sure where this should live
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum CombatState {
    #[default]
    OutOfCombat,
    InCombat(EncounterId),
}

impl CombatState {
    pub fn enter_combat(&mut self, encounter_id: EncounterId) {
        *self = CombatState::InCombat(encounter_id);
    }

    pub fn leave_combat(&mut self) {
        *self = CombatState::OutOfCombat;
    }

    pub fn is_in_combat(&self) -> bool {
        matches!(self, CombatState::InCombat(_))
    }
}

/// Used often enough to warrant a helper function
pub fn is_in_combat(engine_state: &EngineState, entity: Entity) -> bool {
    if let Ok(combat_state) = engine_state.world.get::<&CombatState>(entity) {
        combat_state.is_in_combat()
    } else {
        false
    }
}

pub fn start_encounter(
    engine_state: &mut EngineState,
    participants: HashSet<Entity>,
) -> EncounterId {
    start_encounter_with_id(engine_state, participants, EncounterId::new_v4())
}

pub fn start_encounter_with_id(
    engine_state: &mut EngineState,
    participants: HashSet<Entity>,
    encounter_id: EncounterId,
) -> EncounterId {
    for entity in &participants {
        systems::helpers::get_component_mut::<CombatState>(&mut engine_state.world, *entity)
            .enter_combat(encounter_id);
        systems::time::set_time_mode(
            &mut engine_state.world,
            *entity,
            TimeMode::TurnBased {
                encounter_id: Some(encounter_id),
            },
        );
    }

    engine_state
        .event_log
        .push(Event::encounter_event(EncounterEvent::EncounterStarted(
            encounter_id,
        )));

    let initiative_order = roll_initiative(engine_state, &participants);
    let encounter = Encounter::new(encounter_id, participants, initiative_order);
    log_new_round(engine_state, encounter_id, encounter.round());

    // The encounter has to be in the engine state before the first turn starts,
    // otherwise nothing that happens during that turn can find it
    engine_state.encounters.insert(encounter_id, encounter);
    start_turn(engine_state, encounter_id);

    encounter_id
}

fn roll_initiative(
    engine_state: &EngineState,
    participants: &HashSet<Entity>,
) -> Vec<(Entity, D20CheckResult)> {
    let mut rolls: Vec<(Entity, D20CheckResult)> = participants
        .iter()
        .map(|entity| {
            let roll = systems::helpers::get_component::<SkillSet>(&engine_state.world, *entity)
                .check(&Skill::Initiative, engine_state, *entity);
            (*entity, roll)
        })
        .collect();

    rolls.sort_by_key(|(_, roll)| -roll.total());
    rolls
}

fn log_new_round(engine_state: &mut EngineState, encounter_id: EncounterId, round: usize) {
    engine_state.event_log.push(
        Event::encounter_event(EncounterEvent::NewRound(encounter_id, round))
            .with_scope(EventScope::Encounter(encounter_id)),
    );
}

pub fn end_encounter(engine_state: &mut EngineState, encounter_id: &EncounterId) {
    if let Some(encounter) = engine_state.encounters.remove(encounter_id) {
        for entity in encounter.participants(&engine_state.world, &[EntityFilter::All]) {
            systems::helpers::get_component_mut::<CombatState>(&mut engine_state.world, entity)
                .leave_combat();
            systems::time::set_time_mode(&mut engine_state.world, entity, TimeMode::RealTime);
        }
        engine_state
            .event_log
            .push(Event::encounter_event(EncounterEvent::EncounterEnded(
                *encounter_id,
            )));
    }
}

pub fn end_turn(engine_state: &mut EngineState, entity: Entity) {
    if let Some(scope) = engine_state.scope_for_entity(entity)
        && !scope.pending_events().is_empty()
    {
        warn!(
            "Ending turn for {:?} while there are pending events: {:#?}",
            entity,
            scope.pending_events()
        );
    }

    let Some(encounter) = engine_state.encounter_for_entity(entity) else {
        return;
    };
    let encounter_id = *encounter.id();

    if entity != encounter.current_entity() {
        panic!("Cannot end turn for entity that is not the current entity");
    }

    advance_time(engine_state, encounter_id, TurnBoundary::End);

    // Whatever happened at the boundary might already have moved the turn on,
    // e.g. if the entity got removed from the encounter
    if engine_state
        .encounter(&encounter_id)
        .is_none_or(|encounter| encounter.current_entity() != entity)
    {
        return;
    }

    let scope = engine_state
        .prompts
        .scope_mut(EventScope::Encounter(encounter_id));

    for prompt in scope.pending_prompts().iter() {
        for respondent in prompt.actors() {
            if respondent != entity {
                panic!(
                    "Attempted to end turn for {:?} but there is a pending prompt for {:?}",
                    entity, respondent
                );
            }
        }
    }

    advance_turn(engine_state, encounter_id);
    start_turn(engine_state, encounter_id);
}

/// Hands the turn to the next participant, without starting it
fn advance_turn(engine_state: &mut EngineState, encounter_id: EncounterId) {
    engine_state
        .prompts
        .scope_mut(EventScope::Encounter(encounter_id))
        .clear_prompts();

    let encounter = engine_state
        .encounter_mut(&encounter_id)
        .expect("Inconsistent state: encounter not found");

    if encounter.advance_turn() {
        let round = encounter.round();
        log_new_round(engine_state, encounter_id, round);
    }
}

fn start_turn(engine_state: &mut EngineState, encounter_id: EncounterId) {
    advance_time(engine_state, encounter_id, TurnBoundary::Start);

    let current_entity = current_entity(engine_state, encounter_id);

    if should_skip_turn(engine_state, current_entity) {
        end_turn(engine_state, current_entity);
        return;
    }

    engine_state
        .prompts
        .scope_mut(EventScope::Encounter(encounter_id))
        .queue_prompt(
            ActionPrompt::new(ActionPromptKind::Action {
                actor: current_entity,
            }),
            false,
        );
}

fn current_entity(engine_state: &EngineState, encounter_id: EncounterId) -> Entity {
    engine_state
        .encounter(&encounter_id)
        .expect("Inconsistent state: encounter not found")
        .current_entity()
}

fn advance_time(engine_state: &mut EngineState, encounter_id: EncounterId, boundary: TurnBoundary) {
    let encounter = engine_state
        .encounter(&encounter_id)
        .expect("Inconsistent state: encounter not found");
    let current_entity = encounter.current_entity();

    for entity in encounter.participants(&engine_state.world, &[EntityFilter::All]) {
        systems::time::advance_time(
            engine_state,
            entity,
            TimeStep::TurnBoundary {
                entity: current_entity,
                boundary,
            },
        );
    }
}

// TODO: Some of this feels like it should belong somewhere else?
fn should_skip_turn(engine_state: &mut EngineState, current_entity: Entity) -> bool {
    let is_unconscious = matches!(
        *systems::helpers::get_component::<LifeState>(&engine_state.world, current_entity),
        LifeState::Unconscious(_)
    );

    if is_unconscious {
        let death_saving_throw_event = systems::d20::check(
            engine_state,
            current_entity,
            &D20CheckDC::SavingThrow {
                saving_throw: SavingThrowKind::Death,
                dc: ModifierMap::from(
                    ModifierSource::Custom("Death Saving Throw".to_string()),
                    DEATH_SAVING_THROW_DC as i32,
                )
                .evaluate(),
            },
        );

        engine_state.process_event_with_response_callback(
            death_saving_throw_event,
            EventCallback::new({
                move |engine_state, event, _source| match &event.kind {
                    EventKind::D20CheckResolved {
                        actor,
                        result,
                        dc: _,
                    } => {
                        let life_state = systems::helpers::get_component_mut::<LifeState>(
                            &mut engine_state.world,
                            actor.id(),
                        );

                        if let LifeState::Unconscious(ref mut death_saving_throws) = *life_state {
                            death_saving_throws.update(result);

                            let next_state = death_saving_throws.next_state();

                            if next_state != *life_state {
                                *life_state = next_state;

                                CallbackResult::Event(Event::new(EventKind::LifeStateChanged {
                                    entity: actor.clone(),
                                    new_state: next_state,
                                    actor: None,
                                }))
                            } else {
                                CallbackResult::None
                            }
                        } else {
                            CallbackResult::None
                        }
                    }
                    _ => panic!("Expected D20CheckResolved event"),
                }
            }),
        );

        return true;
    }

    if matches!(
        *systems::helpers::get_component::<LifeState>(&engine_state.world, current_entity),
        LifeState::Normal
    ) {
        return systems::resources::can_act(&engine_state.world, current_entity).is_err();
    }

    // Normal / other states => decide if they can act
    true
}

pub fn remove_participant(
    engine_state: &mut EngineState,
    encounter_id: EncounterId,
    entity: Entity,
) {
    let Some(encounter) = engine_state.encounter(&encounter_id) else {
        return;
    };

    // If the current participant is removed make sure their turn actually ends
    // and is advanced to the next participant
    let was_current = encounter.current_entity() == entity;
    if was_current {
        advance_turn(engine_state, encounter_id);
    }

    let encounter = engine_state
        .encounter_mut(&encounter_id)
        .expect("Inconsistent state: encounter not found");
    encounter.remove_participant(entity);

    if encounter.is_empty() {
        end_encounter(engine_state, &encounter_id);
    } else if was_current {
        start_turn(engine_state, encounter_id);
    }
}
