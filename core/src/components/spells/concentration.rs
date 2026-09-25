use std::hash::Hash;

use hecs::Entity;
use serde::{Deserialize, Serialize};

use crate::{
    components::{effects::effect::EffectInstanceId, id::EffectId, modifier::ModifierSource},
    engine::{action_prompt::ActionExecutionInstanceId, game_state::GameState},
    systems::{self},
};

pub const CONCENTRATION_SAVING_THROW_DC_DEFAULT: i32 = 10;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ConcentrationInstance {
    Effect {
        entity: Entity,
        effect: EffectId,
        instance: EffectInstanceId,
    },
    // TODO: Environmental effects (e.g. web)
}

impl ConcentrationInstance {
    pub fn break_concentration(&self, game_state: &mut GameState) {
        match self {
            ConcentrationInstance::Effect {
                entity, instance, ..
            } => {
                systems::effects::remove_effect(game_state, *entity, instance);
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ConcentrationTracker {
    instances: Vec<ConcentrationInstance>,
    action_instance: Option<ActionExecutionInstanceId>,
    concentration_blockers: Vec<ModifierSource>,
}

impl ConcentrationTracker {
    pub fn instances(&self) -> &Vec<ConcentrationInstance> {
        &self.instances
    }

    pub fn add_instance(
        &mut self,
        instance: ConcentrationInstance,
        action_instance: &ActionExecutionInstanceId,
    ) -> Result<(), ConcentrationError> {
        self.can_concentrate()?;
        self.instances.push(instance);
        self.action_instance = Some(*action_instance);
        Ok(())
    }

    /// In most cases we would want to remove all instances at once, but if e.g.
    /// a spell applies effects to multiple targets, and one of the targets dies,
    /// then we need to be able to remove just that one instance.
    pub fn remove_instances_by_entity(&mut self, entity: Entity) {
        self.instances.retain(|instance| match instance {
            ConcentrationInstance::Effect { entity: e, .. } => *e != entity,
        });
        if self.instances.is_empty() {
            self.action_instance = None;
        }
    }

    pub fn is_concentrating(&self) -> bool {
        !self.instances.is_empty()
    }

    pub fn take_instances(&mut self) -> Vec<ConcentrationInstance> {
        self.action_instance = None;
        std::mem::take(&mut self.instances)
    }

    pub fn action_instance(&self) -> Option<&ActionExecutionInstanceId> {
        self.action_instance.as_ref()
    }

    pub fn can_concentrate(&self) -> Result<(), ConcentrationError> {
        if self.concentration_blockers.is_empty() {
            Ok(())
        } else {
            Err(ConcentrationError::ConcentrationBlocked(
                self.concentration_blockers.clone(),
            ))
        }
    }

    pub fn concentration_blockers(&self) -> &Vec<ModifierSource> {
        &self.concentration_blockers
    }

    pub fn block_concentration(&mut self, source: ModifierSource) {
        self.concentration_blockers.push(source);
    }

    pub fn unblock_concentration(&mut self, source: &ModifierSource) {
        self.concentration_blockers.retain(|s| s != source);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConcentrationError {
    ConcentrationBlocked(Vec<ModifierSource>),
}
