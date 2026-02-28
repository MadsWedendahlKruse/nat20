use core::fmt;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    sync::Arc,
};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{
    components::{
        actions::action::{ActionConditionResolution, ActionContext, ActionResult},
        damage::{
            AttackRoll, AttackRollResult, DamageMitigationResult, DamageRoll, DamageRollResult,
        },
        effects::hooks::{
            ActionHook, ActionResultHook, ApplyEffectHook, ArmorClassHook, AttackRollHook,
            AttackRollResultHook, AttackedHook, D20CheckHooks, DamageRollHook, DamageRollResultHook,
            DeathHook, PostDamageMitigationHook, PreDamageMitigationHook, ResourceCostHook,
            UnapplyEffectHook,
        },
        id::{ActionId, EffectId, IdProvider},
        items::equipment::armor::ArmorClass,
        modifier::ModifierSource,
        resource::ResourceAmountMap,
        saving_throw::SavingThrowKind,
        skill::Skill,
        time::{TimeDuration, TimeStep, TurnBoundary},
    },
    engine::{
        action_prompt::ActionData,
        event::{EventCallback, EventFilter},
        game_state::GameState,
    },
    registry::{
        registry::EffectsRegistry,
        serialize::effect::{
            EffectDefinition, EffectEventFilterDefinition, EffectInstanceDefinition,
        },
    },
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EffectLifetime {
    Permanent,

    /// Expire at Start/End of `entity`'s turn, after `remaining` boundaries.
    /// - remaining = 1 => expire at the next matching boundary
    TurnBoundary {
        entity: Entity,
        boundary: TurnBoundary,
        duration: TimeDuration,
        remaining: TimeDuration,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EffectEntiyReference {
    Applier,
    Target,
}

/// Effect lifetimes are unique in the sense that they can refer to different entities,
/// but those entities are only known at runtime. Therefore, we need a template
/// that can be instantiated into a concrete `EffectLifetime` when the effect is applied.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EffectLifetimeTemplate {
    Permanent,
    TurnBoundary {
        entity: EffectEntiyReference,
        boundary: TurnBoundary,
        duration: TimeDuration,
    },
}

impl EffectLifetimeTemplate {
    pub fn instantiate(&self, applier: Entity, target: Entity) -> EffectLifetime {
        match self {
            EffectLifetimeTemplate::Permanent => EffectLifetime::Permanent,

            EffectLifetimeTemplate::TurnBoundary {
                entity,
                boundary,
                duration,
            } => {
                let entity = match entity {
                    EffectEntiyReference::Applier => applier,
                    EffectEntiyReference::Target => target,
                };
                EffectLifetime::TurnBoundary {
                    entity,
                    boundary: *boundary,
                    duration: *duration,
                    remaining: *duration,
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EffectKind {
    Buff,
    Debuff,
}

#[derive(Clone, Deserialize)]
#[serde(from = "EffectDefinition")]
pub struct Effect {
    pub id: EffectId,
    pub kind: EffectKind,
    pub description: String,
    pub replaces: Option<EffectId>,
    pub children: Vec<EffectId>,

    // on_turn_start: EffectHook,
    // TODO: Do we need to differentiate between when an effect explicitly expires and when
    // the effect is removed from the character?
    // pub on_expire: EffectHook,
    pub on_apply: ApplyEffectHook,
    pub on_unapply: UnapplyEffectHook,
    pub on_skill_check: HashMap<Skill, D20CheckHooks>,
    pub on_saving_throw: HashMap<SavingThrowKind, D20CheckHooks>,
    pub pre_attack_roll: AttackRollHook,
    pub post_attack_roll: AttackRollResultHook,
    pub on_attacked: AttackedHook,
    pub on_armor_class: ArmorClassHook,
    pub pre_damage_roll: DamageRollHook,
    pub post_damage_roll: DamageRollResultHook,
    pub on_action: ActionHook,
    pub on_action_result: ActionResultHook,
    pub on_resource_cost: ResourceCostHook,
    pub pre_damage_mitigation: PreDamageMitigationHook,
    pub post_damage_mitigation: PostDamageMitigationHook,
    pub on_death: DeathHook,
}

impl Effect {
    pub fn new(id: EffectId, kind: EffectKind, description: String) -> Self {
        Self {
            id,
            kind,
            description,
            replaces: None,
            children: Vec::new(),

            on_apply: Arc::new(|_: &mut GameState, _: Entity, _: Option<&ActionContext>| {})
                as ApplyEffectHook,
            on_unapply: Arc::new(|_: &mut GameState, _: Entity| {}) as UnapplyEffectHook,
            on_skill_check: HashMap::new(),
            on_saving_throw: HashMap::new(),
            pre_attack_roll: Arc::new(|_: &World, _: Entity, _: &mut AttackRoll| {})
                as AttackRollHook,
            post_attack_roll: Arc::new(|_: &World, _: Entity, _: &mut AttackRollResult| {})
                as AttackRollResultHook,
            on_attacked: Arc::new(
                |_: &World, _victim: Entity, _attacker: Entity, _: &mut AttackRoll| {},
            ) as AttackedHook,
            on_armor_class: Arc::new(|_: &World, _: Entity, _: &mut ArmorClass| {})
                as ArmorClassHook,
            pre_damage_roll: Arc::new(|_: &World, _: Entity, _: &mut DamageRoll| {})
                as DamageRollHook,
            post_damage_roll: Arc::new(|_: &World, _: Entity, _: &mut DamageRollResult| {})
                as DamageRollResultHook,
            on_action: Arc::new(|_: &mut World, _: &ActionData| {}) as ActionHook,
            on_action_result: Arc::new(
                |_: &mut GameState, _: &ActionData, _: &[ActionResult]| {},
            ) as ActionResultHook,
            on_resource_cost: Arc::new(
                |_: &World,
                 _: Entity,
                 _: &ActionId,
                 _: &ActionContext,
                 _: &mut ResourceAmountMap| {},
            ) as ResourceCostHook,
            pre_damage_mitigation: Arc::new(
                |_: &World, _: Entity, _: &EffectInstance, _: &mut DamageRollResult| {},
            ) as PreDamageMitigationHook,
            post_damage_mitigation: Arc::new(
                |_: &World, _: Entity, _: &mut DamageMitigationResult| {},
            ) as PostDamageMitigationHook,
            on_death: Arc::new(
                |_: &mut World,
                 _victim: Entity,
                 _killer: Option<Entity>,
                 _applier: Option<Entity>| {},
            ) as DeathHook,
        }
    }

    pub fn id(&self) -> &EffectId {
        &self.id
    }
}

impl IdProvider for Effect {
    type Id = EffectId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

#[derive(Clone)]
pub struct EffectEndCondition {
    pub event_filter: EventFilter,
    pub callback: EventCallback,
}

impl fmt::Debug for EffectEndCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EffectEndCondition")
            .field("event_filter", &"...")
            .field("callback", &"...")
            .finish()
    }
}

#[derive(Clone, Deserialize)]
#[serde(from = "EffectEventFilterDefinition")]
pub enum EffectEventFilter {
    TurnBoundary {
        entity: EffectEntiyReference,
        boundary: TurnBoundary,
    },
    Custom(EventFilter),
}

impl EffectEventFilter {
    pub fn instantiate(&self, applier: Entity, target: Entity) -> EventFilter {
        match self {
            EffectEventFilter::TurnBoundary { entity, boundary } => {
                let entity = match entity {
                    EffectEntiyReference::Applier => applier,
                    EffectEntiyReference::Target => target,
                };
                EventFilter::new({
                    let entity = entity;
                    let boundary = *boundary;
                    move |event| {
                        if let crate::engine::event::EventKind::TurnBoundary {
                            entity: e,
                            boundary: b,
                        } = &event.kind
                        {
                            *e == entity && *b == boundary
                        } else {
                            false
                        }
                    }
                })
            }
            EffectEventFilter::Custom(filter) => filter.clone(),
        }
    }
}

#[derive(Clone)]
pub struct EffectEndConditionTemplate {
    pub event_filter: EffectEventFilter,
    pub callback: EventCallback,
}

impl EffectEndConditionTemplate {
    pub fn instantiate(&self, applier: Entity, target: Entity) -> EffectEndCondition {
        EffectEndCondition {
            event_filter: self.event_filter.instantiate(applier, target),
            callback: self.callback.clone(),
        }
    }
}

impl fmt::Debug for EffectEndConditionTemplate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EffectEndConditionTemplate")
            .field("event_filter", &"...")
            .field("callback", &"...")
            .finish()
    }
}

pub type EffectInstanceId = Uuid;

#[derive(Debug, Clone)]
pub struct EffectInstance {
    pub instance_id: EffectInstanceId,
    pub effect_id: EffectId,
    pub source: ModifierSource,
    pub applier: Option<Entity>,
    pub action_resolution: ActionConditionResolution,
    pub lifetime: EffectLifetime,
    pub parent: Option<EffectInstanceId>,
    pub children: HashSet<EffectInstanceId>,
    pub end_condition: Option<EffectEndCondition>,
}

impl EffectInstance {
    pub fn new(
        effect_id: EffectId,
        source: ModifierSource,
        lifetime: EffectLifetime,
        applier: Option<Entity>,
        action_resolution: ActionConditionResolution,
        end_condition: Option<EffectEndCondition>,
    ) -> Self {
        Self {
            instance_id: Uuid::new_v4(),
            effect_id,
            source,
            lifetime,
            applier,
            action_resolution,
            parent: None,
            children: HashSet::new(),
            end_condition,
        }
    }

    pub fn permanent(effect_id: EffectId, source: ModifierSource) -> Self {
        Self::new(
            effect_id,
            source,
            EffectLifetime::Permanent,
            None,
            ActionConditionResolution::Unconditional,
            None,
        )
    }

    pub fn effect(&self) -> &Effect {
        EffectsRegistry::get(&self.effect_id)
            .expect(format!("Effect definition not found for ID `{}`", self.effect_id).as_str())
    }

    pub fn advance_time(&mut self, time_step: TimeStep) {
        match self.lifetime {
            EffectLifetime::Permanent => { /* Do nothing */ }

            EffectLifetime::TurnBoundary {
                entity: life_time_entity,
                boundary: lifetime_boundary,
                ref mut remaining,
                ..
            } => {
                match time_step {
                    TimeStep::TurnBoundary {
                        entity: time_step_entity,
                        boundary: time_step_boundary,
                    } => {
                        if !(time_step_entity == life_time_entity
                            && time_step_boundary == lifetime_boundary)
                        {
                            return;
                        }
                    }
                    _ => { /* Do nothing */ }
                }
                remaining.decrement(&time_step);
            }
        }
    }

    pub fn is_expired(&self) -> bool {
        match self.lifetime {
            EffectLifetime::Permanent => false,

            EffectLifetime::TurnBoundary { ref remaining, .. } => remaining.as_turns() == 0,
        }
    }

    pub fn is_permanent(&self) -> bool {
        matches!(self.lifetime, EffectLifetime::Permanent)
    }

    pub fn is_parent(&self) -> bool {
        self.parent.is_none()
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(from = "EffectInstanceDefinition")]
pub struct EffectInstanceTemplate {
    pub effect_id: EffectId,
    pub lifetime: EffectLifetimeTemplate,
    #[serde(default)]
    pub end_condition: Option<EffectEndConditionTemplate>,
}

impl EffectInstanceTemplate {
    /// Return multiple instantiated effects in case the effect spawns child effects.
    pub fn instantiate(
        &self,
        applier: Entity,
        target: Entity,
        source: ModifierSource,
        action_resolution: ActionConditionResolution,
    ) -> (EffectInstanceId, EffectsMap) {
        let parent_lifetime = self.lifetime.instantiate(applier, target);
        let mut parent_instance = EffectInstance::new(
            self.effect_id.clone(),
            source.clone(),
            parent_lifetime,
            Some(applier),
            action_resolution.clone(),
            self.end_condition
                .as_ref()
                .map(|cond| cond.instantiate(applier, target)),
        );
        let parent_id = parent_instance.instance_id;

        let mut instances = EffectsMap::new();

        // Instantiate child effects
        let effect_definition = self.effect();
        for child_effect_id in &effect_definition.children {
            let child_template = EffectInstanceTemplate {
                effect_id: child_effect_id.clone(),
                lifetime: self.lifetime, // Child effects inherit the same lifetime template
                end_condition: self.end_condition.clone(),
            };

            let (child_root_id, child_instances) = child_template.instantiate(
                applier,
                target,
                source.clone(),
                action_resolution.clone(),
            );

            // Register only the *root* child as a direct child of the parent
            parent_instance.children.insert(child_root_id);

            // Merge the subtree into `instances`, preserving IDs and parent links.
            // Only override the root child’s parent to point at `parent_id`.
            for (instance_id, mut instance) in child_instances {
                if instance_id == child_root_id {
                    instance.parent = Some(parent_id);
                }
                instances.insert(instance_id, instance);
            }
        }

        instances.insert(parent_id, parent_instance);
        (parent_id, instances)
    }

    pub fn effect(&self) -> &Effect {
        EffectsRegistry::get(&self.effect_id)
            .expect(format!("Effect definition not found for ID `{}`", self.effect_id).as_str())
    }
}

pub type EffectsMap = HashMap<EffectInstanceId, EffectInstance>;
