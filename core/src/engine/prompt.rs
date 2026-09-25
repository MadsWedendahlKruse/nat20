use std::collections::{HashMap, HashSet, VecDeque};

use hecs::Entity;

use crate::engine::{
    action_prompt::{ActionDecision, ActionPrompt, ActionPromptId},
    encounter::EncounterId,
    event::{Event, EventId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PromptScopeId {
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
    pub canceled: bool,
}

impl PendingEvent {
    pub fn new(event: Event, blocked_by: HashSet<Entity>) -> Self {
        Self {
            event,
            blocked_by,
            canceled: false,
        }
    }
}

/// One place for prompts, decisions, and paused events.
#[derive(Debug, Default)]
pub struct PromptScope {
    pending_prompts: VecDeque<ActionPrompt>,
    decisions_by_prompt: HashMap<ActionPromptId, HashMap<Entity, ActionDecision>>,
    pending_events: VecDeque<PendingEvent>,
}

impl PromptScope {
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

    pub fn find_prompt(&self, prompt_id: &ActionPromptId) -> Option<&ActionPrompt> {
        self.pending_prompts.iter().find(|p| &p.id == prompt_id)
    }

    pub fn decisions_for_prompt(
        &self,
        prompt_id: &ActionPromptId,
    ) -> Option<&HashMap<Entity, ActionDecision>> {
        self.decisions_by_prompt.get(prompt_id)
    }

    #[must_use]
    pub fn take_decisions_for_prompt(
        &mut self,
        prompt_id: &ActionPromptId,
    ) -> Option<HashMap<Entity, ActionDecision>> {
        self.decisions_by_prompt.remove(prompt_id)
    }

    pub fn all_actors_submitted(&self, prompt_id: &ActionPromptId) -> bool {
        if let Some(prompt) = self.find_prompt(prompt_id)
            && let Some(decisions) = self.decisions_by_prompt.get(prompt_id)
        {
            return prompt.actors().iter().all(|a| decisions.contains_key(a));
        }
        false
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

    pub fn pop_front_if_ready(&mut self) -> Option<PendingEvent> {
        if let Some(front) = self.pending_events.front()
            && front.blocked_by.is_empty()
        {
            Some(self.pending_events.pop_front().unwrap())
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

    pub fn take_pending_event(&mut self, event_id: &EventId) -> Option<(PendingEvent, usize)> {
        if let Some(pos) = self
            .pending_events
            .iter()
            .position(|pe| &pe.event.id == event_id)
        {
            Some((self.pending_events.remove(pos).unwrap(), pos))
        } else {
            None
        }
    }

    pub fn clear_prompts(&mut self) {
        self.pending_prompts.clear();
        self.decisions_by_prompt.clear();
    }

    pub fn clear_blocker(&mut self, event_id: &EventId, reactor: Entity) {
        if let Some(pending_event) = self
            .pending_events
            .iter_mut()
            .find(|pending_event| &pending_event.event.id == event_id)
        {
            pending_event.blocked_by.remove(&reactor);
        }
    }
}

#[derive(Debug, Default)]
pub struct PromptManager {
    pub scopes: HashMap<PromptScopeId, PromptScope>,
}

impl PromptManager {
    pub fn scope_mut(&mut self, id: PromptScopeId) -> &mut PromptScope {
        self.scopes.entry(id).or_default()
    }

    pub fn scope(&self, id: &PromptScopeId) -> Option<&PromptScope> {
        self.scopes.get(id)
    }

    pub fn remove_scope(&mut self, id: &PromptScopeId) {
        self.scopes.remove(id);
    }

    pub fn scopes(&self) -> &HashMap<PromptScopeId, PromptScope> {
        &self.scopes
    }
}
