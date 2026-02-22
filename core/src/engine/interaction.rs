use std::collections::{HashMap, VecDeque};

use hecs::Entity;
use tracing::info;

use crate::engine::{
    action_prompt::{ActionDecision, ActionPrompt, ActionPromptId, ActionPromptKind},
    encounter::EncounterId,
    event::{Event, EventQueue},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InteractionScopeId {
    Global,
    Encounter(EncounterId),
}

/// One place for prompts, decisions, and paused events.
#[derive(Debug, Default)]
pub struct InteractionSession {
    pending_prompts: VecDeque<ActionPrompt>,
    decisions_by_prompt: HashMap<ActionPromptId, HashMap<Entity, ActionDecision>>,
    pending_events: EventQueue, // paused due to reactions
    pending_movement_events: HashMap<Entity, EventQueue>, // paused due to movement reactions
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

    pub fn queue_event(&mut self, event: Event, front: bool) {
        if front {
            self.pending_events.push_front(event);
        } else {
            self.pending_events.push_back(event);
        }
    }

    pub fn pending_events(&self) -> &VecDeque<Event> {
        &self.pending_events
    }

    pub fn pending_events_mut(&mut self) -> &mut VecDeque<Event> {
        &mut self.pending_events
    }

    pub fn clear_prompts(&mut self) {
        self.pending_prompts.clear();
        self.decisions_by_prompt.clear();
    }

    pub fn ready_to_resume(&self) -> bool {
        if self.pending_prompts().is_empty() {
            // Not sure this ever actually happens
            info!("No pending prompts; ready to resume pending events.");
            true
        } else if let Some(front) = self.next_prompt()
            && !matches!(front.kind, ActionPromptKind::Reactions { .. })
        {
            info!("Next prompt is not a reaction; ready to resume pending events.");
            true
        } else {
            false
        }
    }

    pub fn queue_movement_event(&mut self, event: Event, front: bool) {
        if let Some(actor) = event.actor() {
            let queue = self
                .pending_movement_events
                .entry(actor)
                .or_insert_with(VecDeque::new);
            if front {
                queue.push_front(event);
            } else {
                queue.push_back(event);
            }
        }
    }

    pub fn pop_movement_event(&mut self, actor: Entity) -> Option<Event> {
        if let Some(queue) = self.pending_movement_events.get_mut(&actor) {
            queue.pop_front()
        } else {
            None
        }
    }

    pub fn clear_movement_events(&mut self, actor: Entity) {
        self.pending_movement_events.remove(&actor);
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
