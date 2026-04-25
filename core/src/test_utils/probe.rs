use std::collections::HashMap;

use hecs::Entity;
use parry3d::na::Point3;

use crate::{
    components::{
        actions::{action::ActionContext, targeting::TargetInstance},
        d20::D20CheckOutcome,
        damage::{AttackSource, DamageRollResult},
        effects::effect_manager::EffectManager,
        health::hit_points::HitPoints,
        id::{ActionId, EffectId, EntityIdentifier, ResourceId},
        items::equipment::loadout::Loadout,
        modifier::ModifierSource,
        resource::{ResourceAmount, ResourceBudgetKind, ResourceMap},
        saving_throw::SavingThrowSet,
        skill::SkillSet,
        spells::spellbook::Spellbook,
        time::{EntityClock, TimeMode, TimeStep, TurnBoundary},
    },
    engine::{
        action_prompt::{ActionPromptKind, ReactionData},
        event::EventCallback,
        game_state::GameState,
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20CheckKind},
    },
    test_utils::builders::{ActionBuilder, ReactionBuilder},
};

pub struct Probe<'gs> {
    game_state: &'gs mut GameState,
    entity: Entity,
    identifier: EntityIdentifier,
}

impl<'gs> Probe<'gs> {
    pub fn new(game_state: &'gs mut GameState, entity: Entity) -> Self {
        let identifier = EntityIdentifier::from_world(&game_state.world, entity);
        Self {
            game_state,
            entity,
            identifier,
        }
    }

    pub fn entity(&self) -> Entity {
        self.entity
    }

    pub fn identifier(&self) -> &EntityIdentifier {
        &self.identifier
    }

    pub fn name(&self) -> &str {
        self.identifier.name().as_str()
    }

    pub fn game_state(&self) -> &GameState {
        self.game_state
    }

    pub fn game_state_mut(&mut self) -> &mut GameState {
        self.game_state
    }

    // --- Health ---------------------------------------------------------

    pub fn hp(&self) -> u32 {
        systems::helpers::get_component::<HitPoints>(&self.game_state.world, self.entity).current()
    }

    pub fn max_hp(&self) -> u32 {
        systems::helpers::get_component::<HitPoints>(&self.game_state.world, self.entity).max()
    }

    pub fn is_at_full_hp(&self) -> bool {
        self.hp() == self.max_hp()
    }

    pub fn heal_full(&mut self) {
        let _ = systems::health::heal_full(&mut self.game_state.world, self.entity);
    }

    pub fn damage_raw(&mut self, amount: u32) {
        // TODO: This technically bypasses the normal damage pipeline, which is
        // quite complex, but I guess it's fine for testing?
        systems::helpers::get_component_mut::<HitPoints>(&mut self.game_state.world, self.entity)
            .damage(amount);
    }

    pub fn damage(&mut self, damage_result: &DamageRollResult) {
        systems::health::damage(&mut self.game_state, self.entity, damage_result, None);
    }

    // --- Resources ------------------------------------------------------

    /// `true` if the entity can afford the given flat amount of the resource.
    pub fn can_afford(&self, id: &str, amount: &ResourceAmount) -> bool {
        let resources =
            systems::helpers::get_component::<ResourceMap>(&self.game_state.world, self.entity);
        resources.can_afford(&ResourceId::new_core(id), amount)
    }

    /// Current `Flat` resource amount, or 0 if the resource isn't present.
    /// Panics on `Tiered` resources — use a tier-aware accessor for those.
    pub fn resource_amount(&self, id: &str) -> u8 {
        let resources =
            systems::helpers::get_component::<ResourceMap>(&self.game_state.world, self.entity);
        match resources.get(&ResourceId::new_core(id)) {
            None => 0,
            Some(ResourceBudgetKind::Flat(budget)) => budget.current_uses,
            Some(ResourceBudgetKind::Tiered(_)) => panic!(
                "resource_amount called on tiered resource '{}'; use a tier-aware accessor",
                id
            ),
        }
    }

    /// Pretty-printed dump of the entity's resources, for failure messages.
    pub fn resource_summary(&self) -> String {
        let resources =
            systems::helpers::get_component::<ResourceMap>(&self.game_state.world, self.entity);
        let mut entries: Vec<String> = resources
            .iter()
            .map(|(id, budget)| format!("  {id} -> {budget:?}"))
            .collect();
        entries.sort();
        entries.join("\n")
    }

    // --- Effects --------------------------------------------------------

    pub fn has_effect(&self, id: &str) -> bool {
        let effects =
            systems::helpers::get_component::<EffectManager>(&self.game_state.world, self.entity);
        effects
            .iter()
            .any(|(_, instance)| instance.effect_id == EffectId::new_core(id))
    }

    pub fn effect_ids(&self) -> Vec<EffectId> {
        let effects =
            systems::helpers::get_component::<EffectManager>(&self.game_state.world, self.entity);
        let mut ids: Vec<EffectId> = effects
            .iter()
            .map(|(_, inst)| inst.effect_id.clone())
            .collect();
        ids.sort();
        ids.dedup();
        ids
    }

    // --- Actions --------------------------------------------------------

    pub fn has_action(&self, id: &str) -> bool {
        systems::actions::available_actions(&self.game_state.world, self.entity)
            .contains_key(&ActionId::new_core(id))
    }

    pub fn available_action_ids(&self) -> Vec<ActionId> {
        let mut ids: Vec<ActionId> =
            systems::actions::available_actions(&self.game_state.world, self.entity)
                .keys()
                .cloned()
                .collect();
        ids.sort();
        ids
    }

    pub fn on_cooldown(&self, id: &str) -> bool {
        systems::actions::on_cooldown(&self.game_state.world, self.entity, &ActionId::new_core(id))
            .is_some()
    }

    /// Build & submit an action through the activity pipeline.
    /// Default targets to `self`; override with `.at(...)` / `.at_point(...)`.
    pub fn act<'p>(&'p mut self, id: &str) -> ActionBuilder<'p, 'gs> {
        ActionBuilder::new(self, id)
    }

    // --- Reactions ------------------------------------------------------

    pub fn pending_reactions(&self) -> Vec<ReactionData> {
        let Some(prompt) = self.game_state.next_prompt_entity(self.entity) else {
            return vec![];
        };

        match &prompt.kind {
            ActionPromptKind::Action { .. } => vec![],
            ActionPromptKind::Reactions { options, .. } => {
                options.get(&self.entity).cloned().unwrap_or(vec![])
            }
        }
    }

    pub fn has_pending_reaction(&self, id: &str) -> bool {
        self.pending_reactions()
            .iter()
            .any(|option| option.reaction_id == ActionId::new_core(id))
    }

    pub fn react<'p>(&'p mut self) -> ReactionBuilder<'p, 'gs> {
        ReactionBuilder::new(self)
    }

    // --- D20 rolls ------------------------------------------------------

    pub fn d20_check(&mut self, dc: &D20CheckDCKind, callback: Option<EventCallback>) {
        let event = systems::d20::check(&mut self.game_state, self.entity, dc);
        if let Some(callback) = callback {
            self.game_state
                .process_event_with_response_callback(event, callback);
        } else {
            self.game_state.process_event(event);
        }
    }

    // TODO: Currently persists for the entire test, should probably have a way
    // to clear it after the next check or something.
    pub fn d20_force_outcome(&mut self, kind: D20CheckKind, outcome: D20CheckOutcome) {
        match kind {
            D20CheckKind::SavingThrow(saving_throw_kind) => {
                systems::helpers::get_component_mut::<SavingThrowSet>(
                    &mut self.game_state.world,
                    self.entity,
                )
                .set_forced_outcome(
                    &saving_throw_kind,
                    ModifierSource::Custom("test".to_string()),
                    outcome,
                );
            }

            D20CheckKind::Skill(skill) => {
                systems::helpers::get_component_mut::<SkillSet>(
                    &mut self.game_state.world,
                    self.entity,
                )
                .set_forced_outcome(
                    &skill,
                    ModifierSource::Custom("test".to_string()),
                    outcome,
                );
            }

            D20CheckKind::AttackRoll(source, range) => match source {
                AttackSource::Weapon(weapon_kind) => {
                    systems::helpers::get_component_mut::<Loadout>(
                        &mut self.game_state.world,
                        self.entity,
                    )
                    .attack_roll_template_mut(&weapon_kind)
                    .d20_check
                    .set_forced_outcome(ModifierSource::Custom("test".to_string()), outcome);
                }
                AttackSource::Spell => {
                    systems::helpers::get_component_mut::<Spellbook>(
                        &mut self.game_state.world,
                        self.entity,
                    )
                    .attack_roll_template_mut()
                    .d20_check
                    .set_forced_outcome(ModifierSource::Custom("test".to_string()), outcome);
                }
            },
        }
    }

    // --- Time -----------------------------------------------------------

    /// Switch the entity to turn-based mode (no encounter binding) — useful
    /// for solo-actor tests that want to fire `TurnBoundary` events without
    /// going through the full encounter machinery.
    pub fn set_turn_based(&mut self) {
        systems::helpers::get_component_mut::<EntityClock>(&mut self.game_state.world, self.entity)
            .set_mode(TimeMode::TurnBased { encounter_id: None });
    }

    pub fn start_turn(&mut self) {
        let entity = self.entity;
        systems::time::advance_time(
            self.game_state,
            entity,
            TimeStep::TurnBoundary {
                entity,
                boundary: TurnBoundary::Start,
            },
        );
        // Drain any expirations / queued events triggered by the boundary.
        self.game_state.update(0.0);
    }

    pub fn end_turn(&mut self) {
        let entity = self.entity;
        systems::time::advance_time(
            self.game_state,
            entity,
            TimeStep::TurnBoundary {
                entity,
                boundary: TurnBoundary::End,
            },
        );
        self.game_state.update(0.0);
    }

    /// Advance ambient real-time for this entity (effect decay, durations).
    /// Use sparingly — turn-based scenarios should not need this.
    pub fn advance_real_time(&mut self, delta_seconds: f32) {
        self.game_state.update(delta_seconds);
    }
}

pub(crate) fn lookup_action_contexts(
    probe: &Probe<'_>,
    action_id: &ActionId,
) -> Option<Vec<(ActionContext, HashMap<ResourceId, ResourceAmount>)>> {
    systems::actions::available_actions(&probe.game_state.world, probe.entity)
        .get(action_id)
        .cloned()
}
