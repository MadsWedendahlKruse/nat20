use std::collections::{HashMap, HashSet, VecDeque};

use hecs::Entity;

use crate::{
    components::actions::action_step::ActionPhase,
    engine::{
        action_prompt::{ActionDecision, ActionPrompt, ActionPromptId},
        encounter::EncounterId,
        event::Event,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InteractionScopeId {
    Global,
    Encounter(EncounterId),
}

/// A paused event together with the reactors that are still holding it up.
///
/// Populated with the full set of potential reactors at park time. Each reactor
/// is cleared either when their decision resolves without spawning an activity
/// (decline or instant modifier reaction), or when their resulting reaction
/// activity completes. The event becomes drainable when `blocked_by` is empty.
#[derive(Debug, Clone)]
pub struct PendingEvent {
    pub event: Event,
    pub blocked_by: HashSet<Entity>,
}

impl PendingEvent {
    pub fn new(event: Event, blocked_by: HashSet<Entity>) -> Self {
        Self { event, blocked_by }
    }
}

/// One place for prompts, decisions, and paused events.
#[derive(Debug, Default)]
pub struct InteractionSession {
    pending_prompts: VecDeque<ActionPrompt>,
    decisions_by_prompt: HashMap<ActionPromptId, HashMap<Entity, ActionDecision>>,
    pending_events: VecDeque<PendingEvent>,
    /// Phases parked while waiting for a pending event to be resumed and resolved.
    pending_phases: VecDeque<(Entity, ActionPhase)>,
}

impl InteractionSession {
    pub fn pending_prompts(&self) -> &VecDeque<ActionPrompt> {
        &self.pending_prompts
    }

    pub fn pending_prompts_mut(&mut self) -> &mut VecDeque<ActionPrompt> {
        &mut self.pending_prompts
    }

    pub fn next_prompt(&self) -> Option<&ActionPrompt> {
        self.pending_prompts.front()
    }

    pub fn next_prompt_mut(&mut self) -> Option<&mut ActionPrompt> {
        self.pending_prompts.front_mut()
    }

    pub fn queue_prompt(&mut self, prompt: ActionPrompt, front: bool) {
        self.decisions_by_prompt.entry(prompt.id).or_default();
        if front {
            self.pending_prompts.push_front(prompt);
        } else {
            self.pending_prompts.push_back(prompt);
        }
    }

    pub fn decisions_for_prompt(
        &self,
        prompt_id: &ActionPromptId,
    ) -> Option<&HashMap<Entity, ActionDecision>> {
        self.decisions_by_prompt.get(prompt_id)
    }

    pub fn record_decision(&mut self, decision: ActionDecision) {
        if let Some(decisions) = self.decisions_by_prompt.get_mut(&decision.response_to) {
            decisions.insert(decision.actor(), decision);
        }
    }

    pub fn pop_prompt(&mut self) -> Option<ActionPrompt> {
        if let Some(prompt) = self.pending_prompts.pop_front() {
            self.decisions_by_prompt.remove(&prompt.id);
            Some(prompt)
        } else {
            None
        }
    }

    pub fn pop_prompt_by_id(&mut self, prompt_id: &ActionPromptId) -> Option<ActionPrompt> {
        if let Some(pos) = self.pending_prompts.iter().position(|p| &p.id == prompt_id) {
            let prompt = self.pending_prompts.remove(pos).unwrap();
            self.decisions_by_prompt.remove(prompt_id);
            Some(prompt)
        } else {
            None
        }
    }

    pub fn queue_pending_event(&mut self, pending: PendingEvent, front: bool) {
        if front {
            self.pending_events.push_front(pending);
        } else {
            self.pending_events.push_back(pending);
        }
    }

    pub fn pending_events(&self) -> &VecDeque<PendingEvent> {
        &self.pending_events
    }

    pub fn pending_events_mut(&mut self) -> &mut VecDeque<PendingEvent> {
        &mut self.pending_events
    }

    pub fn clear_prompts(&mut self) {
        self.pending_prompts.clear();
        self.decisions_by_prompt.clear();
    }

    /// Index of the first pending event whose blockers have all cleared, if any.
    pub fn first_drainable_event(&self) -> Option<usize> {
        self.pending_events
            .iter()
            .position(|pe| pe.blocked_by.is_empty())
    }

    /// Remove `reactor` from every pending event's `blocked_by` set. Called when
    /// a reaction activity completes, or when a reactor's decision resolves
    /// without spawning an activity.
    pub fn clear_blocker(&mut self, reactor: Entity) {
        for pe in self.pending_events.iter_mut() {
            pe.blocked_by.remove(&reactor);
        }
    }

    pub fn queue_phase(&mut self, entity: Entity, phase: ActionPhase, front: bool) {
        if front {
            self.pending_phases.push_front((entity, phase));
        } else {
            self.pending_phases.push_back((entity, phase));
        }
    }

    pub fn pending_phases_mut(&mut self) -> &mut VecDeque<(Entity, ActionPhase)> {
        &mut self.pending_phases
    }
}

#[derive(Debug, Default)]
pub struct InteractionEngine {
    pub sessions: HashMap<InteractionScopeId, InteractionSession>,
}

impl InteractionEngine {
    pub fn session_mut(&mut self, id: InteractionScopeId) -> &mut InteractionSession {
        self.sessions.entry(id).or_default()
    }

    pub fn session(&self, id: InteractionScopeId) -> Option<&InteractionSession> {
        self.sessions.get(&id)
    }

    pub fn remove_session(&mut self, id: InteractionScopeId) {
        self.sessions.remove(&id);
    }
}
