use std::{collections::HashSet, sync::Arc};

use hecs::{Entity, World};
use uuid::Uuid;

use crate::{
    components::{
        actions::targeting::EntityFilter,
        d20::{D20CheckDC, D20CheckResult},
        health::life_state::{DEATH_SAVING_THROW_DC, LifeState},
        modifier::{ModifierSet, ModifierSource},
        saving_throw::SavingThrowKind,
        skill::{Skill, SkillSet},
        time::{TimeStep, TurnBoundary},
    },
    engine::{
        action_prompt::{ActionPrompt, ActionPromptKind},
        event::{CallbackResult, EncounterEvent, Event, EventCallback, EventKind, EventLog},
        game_state::GameState,
        interaction::InteractionScopeId,
    },
    systems::{self, d20::D20CheckDCKind},
};

pub type EncounterId = Uuid;

#[derive(Debug)]
pub struct Encounter {
    id: EncounterId,
    participants: HashSet<Entity>,
    round: usize,
    turn_index: usize,
    initiative_order: Vec<(Entity, D20CheckResult)>,
    event_log: EventLog,
}

impl Encounter {
    pub fn new(game_state: &mut GameState, participants: HashSet<Entity>, id: EncounterId) -> Self {
        let mut encounter = Self {
            id,
            participants,
            round: 1,
            turn_index: 0,
            initiative_order: Vec::new(),
            event_log: EventLog::new(),
        };
        encounter.roll_initiative(&game_state.world);
        encounter.start_turn(game_state);
        encounter
            .event_log
            .push(Event::encounter_event(EncounterEvent::NewRound(
                encounter.id.clone(),
                encounter.round(),
            )));
        encounter
    }

    fn roll_initiative(&mut self, world: &World) {
        let mut indexed_rolls: Vec<(Entity, D20CheckResult)> = self
            .participants
            .iter()
            .map(|entity| {
                let roll = systems::helpers::get_component::<SkillSet>(world, *entity).check(
                    &Skill::Initiative,
                    world,
                    *entity,
                );
                (entity.clone(), roll)
            })
            .collect();

        indexed_rolls.sort_by_key(|(_, roll)| -(roll.total() as i32));
        self.initiative_order = indexed_rolls
            .into_iter()
            .map(|(i, roll)| (i, roll))
            .collect();
    }

    pub fn id(&self) -> &EncounterId {
        &self.id
    }

    pub fn initiative_order(&self) -> &Vec<(Entity, D20CheckResult)> {
        &self.initiative_order
    }

    pub fn current_entity(&self) -> Entity {
        let (idx, _) = self.initiative_order[self.turn_index];
        idx
    }

    pub fn participants(&self, world: &World, filters: &[EntityFilter]) -> Vec<Entity> {
        self.participants
            .iter()
            .filter(|entity| filters.iter().all(|filter| filter.matches(world, *entity)))
            .cloned()
            .collect()
    }

    fn start_turn(&mut self, game_state: &mut GameState) {
        self.advance_time(game_state, TurnBoundary::Start);

        if self.should_skip_turn(game_state) {
            self.end_turn(game_state, self.current_entity());
            return;
        }

        let session = game_state
            .interaction_engine
            .session_mut(InteractionScopeId::Encounter(self.id));

        session.queue_prompt(
            ActionPrompt::new(ActionPromptKind::Action {
                actor: self.current_entity(),
            }),
            false,
        );
    }

    pub fn end_turn(&mut self, game_state: &mut GameState, entity: Entity) {
        if entity != self.current_entity() {
            panic!("Cannot end turn for entity that is not the current entity");
        }

        self.advance_time(game_state, TurnBoundary::End);

        let session = game_state
            .interaction_engine
            .session_mut(InteractionScopeId::Encounter(self.id));

        for prompt in session.pending_prompts().iter() {
            for respondent in prompt.actors() {
                if respondent != entity {
                    panic!(
                        "Attempted to end turn for {:?} but there is a pending prompt for {:?}",
                        entity, respondent
                    );
                }
            }
        }

        session.clear_prompts();

        self.turn_index = (self.turn_index + 1) % self.participants.len();
        if self.turn_index == 0 {
            self.round += 1;
            self.event_log
                .push(Event::encounter_event(EncounterEvent::NewRound(
                    self.id.clone(),
                    self.round(),
                )));
        }

        self.start_turn(game_state);
    }

    // TODO: Some of this feels like it should belong somewhere else?
    fn should_skip_turn(&mut self, game_state: &mut GameState) -> bool {
        let current_entity = self.current_entity();

        let is_unconscious = matches!(
            *systems::helpers::get_component::<LifeState>(&game_state.world, current_entity),
            LifeState::Unconscious(_)
        );

        if is_unconscious {
            let death_saving_throw_event = systems::d20::check(
                game_state,
                current_entity,
                &D20CheckDCKind::SavingThrow(D20CheckDC {
                    dc: ModifierSet::from_iter([(
                        ModifierSource::Custom("Death Saving Throw".to_string()),
                        DEATH_SAVING_THROW_DC as i32,
                    )]),
                    key: SavingThrowKind::Death,
                }),
            );

            game_state.process_event_with_response_callback(
                death_saving_throw_event,
                EventCallback::new({
                    move |game_state, event, source| match &event.kind {
                        EventKind::D20CheckResolved(performer, result, dc) => {
                            let mut life_state = systems::helpers::get_component_mut::<LifeState>(
                                &mut game_state.world,
                                *performer,
                            );

                            if let LifeState::Unconscious(ref mut death_saving_throws) = *life_state
                            {
                                death_saving_throws.update(result.d20_result());

                                let next_state = death_saving_throws.next_state();

                                if next_state != *life_state {
                                    *life_state = next_state.clone();

                                    return CallbackResult::Event(Event::new(
                                        EventKind::LifeStateChanged {
                                            entity: current_entity,
                                            new_state: next_state,
                                            actor: None,
                                        },
                                    ));
                                } else {
                                    return CallbackResult::None;
                                }
                            } else {
                                return CallbackResult::None;
                            }
                        }
                        _ => panic!("Expected D20CheckResolved event"),
                    }
                }),
            );

            return true;
        } else {
            // Normal / other states => decide if they can act
            return !matches!(
                *systems::helpers::get_component::<LifeState>(&game_state.world, current_entity),
                LifeState::Normal
            );
        };
    }

    pub fn round(&self) -> usize {
        self.round
    }

    pub(crate) fn log_event(&mut self, event: Event) {
        self.event_log.push(event);
    }

    fn advance_time(&mut self, game_state: &mut GameState, boundary: TurnBoundary) {
        // TODO: Not sure if this is the correct place to do it?
        match boundary {
            TurnBoundary::Start => {
                systems::time::on_turn_start(&mut game_state.world, self.current_entity());
            }
            TurnBoundary::End => {
                systems::time::on_turn_end(&mut game_state.world, self.current_entity());
            }
        }
        for entity in self.participants.iter() {
            systems::time::advance_time(
                game_state,
                *entity,
                TimeStep::TurnBoundary {
                    entity: self.current_entity(),
                    boundary,
                },
            );
        }
    }

    pub fn combat_log(&self) -> &EventLog {
        &self.event_log
    }

    pub fn combat_log_move(&mut self) -> EventLog {
        std::mem::take(&mut self.event_log)
    }
}
