use std::collections::{HashMap, HashSet, VecDeque};

use hecs::Entity;
use tracing::{info, warn};

use crate::{
    components::actions::action_step::ActionPhase,
    engine::{
        action_prompt::{ActionDecision, ActionPrompt, ActionPromptId, ActionPromptKind},
        encounter::EncounterId,
        event::{Event, EventQueue},
    },
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
    /// Entities performing a reaction activity that must complete before pending
    /// events can be resumed.
    pending_reaction_actors: HashSet<Entity>,
    /// Phase parked while waiting for a pending event to be resumed and resolved.
    pending_phase: Option<(Entity, ActionPhase)>,
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
        if !self.pending_reaction_actors.is_empty() {
            return false;
        }

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

    pub fn add_pending_reactor(&mut self, entity: Entity) {
        self.pending_reaction_actors.insert(entity);
    }

    pub fn remove_pending_reactor(&mut self, entity: Entity) {
        self.pending_reaction_actors.remove(&entity);
    }

    pub fn set_pending_phase(&mut self, entity: Entity, phase: ActionPhase) {
        if let Some((pending_entity, pending_phase)) = &self.pending_phase {
            warn!(
                "Overwriting pending phase {:?} for entity {:?} with new phase {:?} for entity {:?}",
                pending_phase, pending_entity, phase, entity
            );
        }
        self.pending_phase = Some((entity, phase));
    }

    pub fn take_pending_phase(&mut self) -> Option<(Entity, ActionPhase)> {
        self.pending_phase.take()
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
