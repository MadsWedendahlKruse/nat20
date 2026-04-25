use std::{collections::BTreeMap, sync::LazyLock};

use hecs::Entity;

use crate::{
    components::{
        actions::targeting::TargetInstance,
        activity::{Activity, ActivityError},
        id::{ActionId, EntityIdentifier},
        time::{EntityClock, TimeMode},
    },
    engine::{
        action_prompt::{ActionData, ActionDecision, ActionDecisionKind, ReactionData},
        game_state::GameState,
    },
    systems,
    test_utils::{
        fixtures::creatures::{heroes, monsters},
        probe::{Probe, lookup_action_contexts},
    },
};

/// `(game_state, level/challenge_rating, optional_entity_to_spawn_at) -> spawned`
pub type CreatureTemplateFunction = fn(&mut GameState, u8, Option<Entity>) -> EntityIdentifier;

pub static CREATURE_TEMPLATES: LazyLock<BTreeMap<&'static str, CreatureTemplateFunction>> =
    LazyLock::new(|| {
        BTreeMap::from([
            ("hero.fighter", heroes::fighter as CreatureTemplateFunction),
            ("hero.wizard", heroes::wizard as CreatureTemplateFunction),
            ("hero.warlock", heroes::warlock as CreatureTemplateFunction),
            (
                "monster.goblin_warrior",
                monsters::goblin_warrior as CreatureTemplateFunction,
            ),
        ])
    });

pub fn creature(id: &str) -> &CreatureTemplateFunction {
    CREATURE_TEMPLATES.get(id).unwrap_or_else(|| {
        let mut known: Vec<&&str> = CREATURE_TEMPLATES.keys().collect();
        known.sort();
        panic!(
            "Unknown creature template '{}'. Known templates: {:?}",
            id, known
        )
    })
}

pub struct CreatureBuilder<'gs> {
    gs: &'gs mut GameState,
    template: CreatureTemplateFunction,
    level: u8,
    time_mode: TimeMode,
    spawn_at: Option<Entity>,
}

impl<'gs> CreatureBuilder<'gs> {
    pub fn from_template(gs: &'gs mut GameState, id: &str) -> Self {
        Self {
            gs,
            template: *creature(id),
            level: 1,
            time_mode: TimeMode::RealTime,
            spawn_at: None,
        }
    }

    pub fn level(mut self, level: u8) -> Self {
        assert!(level >= 1, "Character level must be >= 1");
        self.level = level;
        self
    }

    pub fn turn_based(mut self) -> Self {
        self.time_mode = TimeMode::TurnBased { encounter_id: None };
        self
    }

    pub fn at_entity(mut self, entity: Entity) -> Self {
        self.spawn_at = Some(entity);
        self
    }

    pub fn spawn(self) -> Probe<'gs> {
        let identifier = (self.template)(self.gs, self.level, self.spawn_at);
        let entity = identifier.id();

        systems::helpers::get_component_mut::<EntityClock>(&mut self.gs.world, entity)
            .set_mode(self.time_mode);

        let _ = systems::health::heal_full(&mut self.gs.world, entity);

        Probe::new(self.gs, entity)
    }
}

pub struct ActionBuilder<'p, 'gs> {
    probe: &'p mut Probe<'gs>,
    action_id: ActionId,
    targets: Option<Vec<TargetInstance>>,
    context_index: usize,
}

impl<'p, 'gs> ActionBuilder<'p, 'gs> {
    pub(crate) fn new(probe: &'p mut Probe<'gs>, id: &str) -> Self {
        let action_id = ActionId::new_core(id);
        Self {
            probe,
            action_id,
            targets: None,
            context_index: 0,
        }
    }

    /// Default — targets the actor themself.
    pub fn on_self(mut self) -> Self {
        self.targets = Some(vec![TargetInstance::Entity(
            self.probe.identifier().clone(),
        )]);
        self
    }

    /// Target another entity (typically another `Probe`).
    pub fn at(mut self, other: &Probe<'_>) -> Self {
        self.targets = Some(vec![TargetInstance::Entity(other.identifier().clone())]);
        self
    }

    /// Target a world point (e.g. fireball).
    pub fn at_point(mut self, p: [f32; 3]) -> Self {
        self.targets = Some(vec![TargetInstance::Point(p.into())]);
        self
    }

    /// Multiple targets at once.
    pub fn targets(mut self, targets: Vec<TargetInstance>) -> Self {
        self.targets = Some(targets);
        self
    }

    /// Pick a context+cost other than the first one returned by
    /// `available_actions`. Useful when an action has multiple valid forms
    /// (e.g. a weapon attack with a different hand).
    pub fn with_context(mut self, idx: usize) -> Self {
        self.context_index = idx;
        self
    }

    /// Submit through `submit_activity`, then drain with `update(0.0)`.
    /// Returns the activity result. Panics with a useful message if the
    /// action is not available to this actor at all.
    pub fn submit(self) -> Result<(), ActivityError> {
        let contexts = lookup_action_contexts(self.probe, &self.action_id).unwrap_or_else(|| {
            panic!(
                "Action {} is not available to {} ({:?}). Available actions:\n  {}",
                self.action_id,
                self.probe.name(),
                self.probe.entity(),
                self.probe
                    .available_action_ids()
                    .iter()
                    .map(|id| id.to_string())
                    .collect::<Vec<_>>()
                    .join("\n  "),
            )
        });

        let (context, cost) = contexts
            .get(self.context_index)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Action {} only has {} context(s); index {} requested",
                    self.action_id,
                    contexts.len(),
                    self.context_index
                )
            });

        let targets = self
            .targets
            .unwrap_or_else(|| vec![TargetInstance::Entity(self.probe.identifier().clone())]);

        let actor = self.probe.identifier().clone();
        let result = self.probe.game_state_mut().submit_activity(Activity::Act {
            action: ActionDecision::without_response_to(ActionDecisionKind::Action {
                action: ActionData::new(actor, self.action_id.clone(), context, cost, targets),
            }),
        });
        // Drain queued events without ticking real time.
        self.probe.game_state_mut().update(0.0);
        result
    }

    /// Convenience: submit and panic if the activity errored.
    pub fn submit_ok(self) {
        let action_id = self.action_id.clone();
        match self.submit() {
            Ok(()) => (),
            Err(e) => panic!("Submitting {} failed: {:?}", action_id, e),
        }
    }
}

pub struct ReactionBuilder<'p, 'gs> {
    probe: &'p mut Probe<'gs>,
    options: Vec<ReactionData>,
    id: Option<ActionId>,
    index: usize,
}

impl<'p, 'gs> ReactionBuilder<'p, 'gs> {
    pub(crate) fn new(probe: &'p mut Probe<'gs>) -> Self {
        let options = probe.pending_reactions();
        Self {
            probe,
            options,
            id: None,
            index: 0,
        }
    }

    /// Pick the reaction with the given ID. Panics if it's not available.
    pub fn id(mut self, id: &str) -> Self {
        let id = ActionId::new_core(id);
        let options = self
            .options
            .into_iter()
            .filter(|opt| opt.reaction_id == id)
            .collect::<Vec<_>>();
        if options.is_empty() {
            panic!(
                "No pending reaction with ID '{}' for {}. Available reactions:\n  {}",
                id,
                self.probe.name(),
                self.probe
                    .pending_reactions()
                    .iter()
                    .map(|opt| opt.reaction_id.to_string())
                    .collect::<Vec<_>>()
                    .join("\n  "),
            );
        }
        self.options = options;
        self.id = Some(id);
        self
    }

    pub fn index(mut self, index: usize) -> Self {
        if index >= self.options.len() {
            panic!(
                "Reaction index {} out of bounds; only {} option(s) available for {}",
                index,
                self.options.len(),
                self.probe.name(),
            );
        }
        self.index = index;
        self
    }

    /// Submit through `submit_activity`, then drain with `update(0.0)`.
    /// Returns the activity result. Panics with a useful message if the
    /// reaction is not available to this actor at all.
    pub fn submit(self) -> Result<(), ActivityError> {
        let option = self.options.get(self.index).unwrap_or_else(|| {
            panic!(
                "No pending reaction at index {} for {}. Available reactions:\n  {}",
                self.index,
                self.probe.name(),
                self.probe
                    .pending_reactions()
                    .iter()
                    .map(|opt| opt.reaction_id.to_string())
                    .collect::<Vec<_>>()
                    .join("\n  "),
            )
        });
        let prompt_id = self
            .probe
            .game_state()
            .next_prompt_entity(option.reactor.id())
            .unwrap_or_else(|| {
                panic!(
                    "No prompt found for reaction option {} for {}",
                    option.reaction_id,
                    self.probe.name(),
                )
            })
            .id;
        let result = self.probe.game_state_mut().submit_activity(Activity::Act {
            action: ActionDecision {
                response_to: prompt_id,
                kind: ActionDecisionKind::Reaction {
                    choice: Some(option.clone()),
                    event: *option.event.clone(),
                    reactor: option.reactor.id(),
                },
            },
        });
        // Drain queued events without ticking real time.
        self.probe.game_state_mut().update(0.0);
        result
    }

    /// Convenience: submit and panic if the activity errored.
    pub fn submit_ok(self) {
        let result = self.submit();
        if let Err(e) = result {
            panic!("Submitting reaction failed: {:?}", e);
        }
    }
}
