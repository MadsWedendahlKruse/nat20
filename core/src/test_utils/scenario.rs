use std::{collections::HashSet, u32};

use hecs::{Entity, World};
use parry3d::{na::Point3, utils::hashmap::HashMap};
use tracing::subscriber::DefaultGuard;
use tracing_subscriber::{EnvFilter, util::SubscriberInitExt};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{
        actions::{
            action::{ActionContext, ActionResultComponent},
            action_builder::{ActionBuilder, ReactionBuilder},
            targeting::TargetInstance,
        },
        d20::{AdvantageType, D20Check, D20CheckOutcome},
        damage::{AttackSource, DamageComponent, DamageResistances, DamageType},
        health::hit_points::HitPoints,
        id::{ActionId, EffectId, EntityIdentifier, ItemId, ResourceId},
        items::equipment::{loadout::Loadout, slots::EquipmentSlot},
        modifier::{
            FlatModifiable, FlatModifierMap, Modifiable, ModifierKind, ModifierMap, ModifierResult,
            ModifierSource,
        },
        resource::{ResourceAmountMap, ResourceBudgetKind, ResourceMap},
        saving_throw::SavingThrowSet,
        skill::{Skill, SkillSet},
        speed::Speed,
        spells::{spell::ConcentrationInstance, spellbook::Spellbook},
        time::{TimeMode, TimeStep, TurnBoundary},
    },
    engine::{
        action_prompt::ActionData,
        encounter::EncounterId,
        event::{Event, EventCallback, EventFilter, EventKind},
        game_state::GameState,
    },
    registry::registry::ItemsRegistry,
    systems::{
        self,
        d20::{D20CheckDCKind, D20CheckKind},
    },
    test_utils::{creature_builder::CreatureBuilder, fixtures},
};

pub struct Scenario {
    pub game_state: GameState,
    pub creatures: HashMap<String, EntityIdentifier>,
    pub encounter_id: Option<EncounterId>,
    _log_guard: DefaultGuard,
}

impl Scenario {
    pub fn from_game_state(game_state: GameState) -> Self {
        Self {
            game_state,
            creatures: HashMap::default(),
            encounter_id: None,
            _log_guard: Self::init_test_logging(),
        }
    }

    pub fn new() -> Self {
        Self::from_game_state(fixtures::engine::game_state())
    }

    fn init_test_logging() -> DefaultGuard {
        tracing_subscriber::fmt()
            .with_env_filter(
                EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug")),
            )
            .with_test_writer() // print!() — cargo captures, shows on failure only
            .with_target(true)
            .with_level(true)
            .with_ansi(true) // ANSI escapes look ugly in cargo's captured output
            .finish()
            .set_default()
    }

    pub fn probe(&mut self, handle: impl Into<String>) -> ScenarioProbe<'_> {
        ScenarioProbe {
            scenario: self,
            handle: handle.into(),
        }
    }

    /// The `EntityIdentifier` behind a handle (cloned for use in messages).
    fn creature(&self, handle: &str) -> EntityIdentifier {
        self.creatures
            .get(handle)
            .cloned()
            .unwrap_or_else(|| panic!("No creature with handle {handle} in scenario"))
    }

    fn entity(&self, handle: &str) -> Entity {
        self.creature(handle).id()
    }

    pub fn spawn(
        &mut self,
        handle: impl Into<String>,
        template: impl Into<String>,
    ) -> ScenarioCreatureBuilder<'_> {
        let handle = handle.into();
        let template = template.into();
        if self.creatures.contains_key(&handle) {
            panic!("Creature with handle {handle} already exists in scenario");
        }

        let builder = CreatureBuilder::new(&template);
        ScenarioCreatureBuilder {
            scenario: self,
            handle,
            builder,
        }
    }

    pub fn encounter<'s>(&'s mut self) -> ScenarioEncounterBuilder<'s> {
        ScenarioEncounterBuilder {
            scenario: self,
            initiative_order: Vec::new(),
        }
    }

    pub fn act<'s>(
        &mut self,
        handle: impl Into<String>,
        action: impl Into<ActionId>,
    ) -> ScenarioActionBuilder<'_> {
        let handle = handle.into();
        let entity = self.entity(&handle);
        let mut builder = ActionBuilder::available(&self.game_state, entity);
        builder.action(&self.game_state, &action.into());
        ScenarioActionBuilder {
            scenario: self,
            builder,
        }
    }

    pub fn react<'s>(&mut self, handle: impl Into<String>) -> ScenarioReactionBuilder<'_> {
        let handle = handle.into();
        let entity = self.entity(&handle);
        let builder = ReactionBuilder::new(&self.game_state, entity);
        ScenarioReactionBuilder {
            scenario: self,
            builder,
        }
    }

    pub fn update(&mut self, delta_time: f32) {
        self.game_state.update(delta_time);
    }

    pub fn filter_events(&self, event_filter: EventFilter) -> Vec<&Event> {
        let event_log = if let Some(encounter_id) = &self.encounter_id {
            self.game_state
                .encounters
                .get(encounter_id)
                .unwrap_or_else(|| panic!("No encounter with id {encounter_id} in game state"))
                .event_log()
        } else {
            &self.game_state.event_log
        };

        event_log
            .events
            .iter()
            .rev()
            .filter(|event| event_filter.matches(event))
            .collect()
    }

    pub fn event_filter(&self) -> ScenarioEventFilterBuilder<'_> {
        ScenarioEventFilterBuilder {
            scenario: self,
            actor: None,
            kind: None,
        }
    }
}

pub struct ScenarioCreatureBuilder<'s> {
    scenario: &'s mut Scenario,
    handle: String,
    builder: CreatureBuilder,
}

impl ScenarioCreatureBuilder<'_> {
    pub fn level(mut self, level: u8) -> Self {
        self.builder.level(level);
        self
    }

    pub fn position(mut self, position: impl Into<Point3<f32>>, on_ground: bool) -> Self {
        self.builder.position(position, on_ground);
        self
    }

    pub fn time_mode(mut self, time_mode: TimeMode) -> Self {
        self.builder.time_mode(time_mode);
        self
    }

    pub fn spawn(mut self) {
        let creature = self.builder.spawn(&mut self.scenario.game_state);
        self.scenario.creatures.insert(self.handle, creature);
    }
}

pub struct ScenarioEncounterBuilder<'s> {
    scenario: &'s mut Scenario,
    /// The order in which creatures take their turns, by handle
    initiative_order: Vec<String>,
}

impl<'s> ScenarioEncounterBuilder<'s> {
    pub fn initiative_order(mut self, initiative_order: Vec<impl Into<String>>) -> Self {
        self.initiative_order = initiative_order.into_iter().map(|s| s.into()).collect();
        self
    }

    #[track_caller]
    pub fn build(self) {
        let participants = self
            .scenario
            .creatures
            .values()
            .map(|creature| creature.id())
            .collect::<HashSet<_>>();

        if self.initiative_order.len() > participants.len() {
            panic!(
                "Initiative order has more entries ({}) than creatures in the scenario ({})",
                self.initiative_order.len(),
                participants.len()
            );
        }

        for (i, handle) in self.initiative_order.iter().enumerate() {
            if !self.scenario.creatures.contains_key(handle) {
                panic!("Initiative order contains handle {handle} which is not in the scenario");
            }

            let initiative_bonus = (participants.len() - 1 - i) * 20;
            let creature = self.scenario.creatures.get(handle).unwrap();
            let entity = creature.id();
            let mut skills = systems::helpers::get_component_mut::<SkillSet>(
                &mut self.scenario.game_state.world,
                entity,
            );
            skills
                .get_mut(&Skill::Initiative)
                .modifiers_mut()
                .add_modifier(
                    ModifierSource::Custom("Test scenario initiative order".to_string()),
                    initiative_bonus as i32,
                );
        }

        self.scenario.encounter_id = Some(self.scenario.game_state.start_encounter(participants));
    }
}

pub struct ScenarioActionBuilder<'s> {
    scenario: &'s mut Scenario,
    builder: ActionBuilder,
}

impl ScenarioActionBuilder<'_> {
    pub fn variant(mut self, variant: impl Into<ActionId>) -> Self {
        self.builder
            .action(&mut self.scenario.game_state, &variant.into());
        self
    }

    pub fn context_index(mut self, index: usize) -> Self {
        self.builder
            .context_index(&self.scenario.game_state.world, index);
        self
    }

    pub fn context_filter(
        mut self,
        filter: impl Fn(&ActionContext, &ResourceAmountMap) -> bool,
    ) -> Self {
        self.builder
            .context_filter(&self.scenario.game_state.world, filter);
        self
    }

    pub fn context_spell_level(mut self, spell_level: u8) -> Self {
        self.builder
            .context_spell_level(&self.scenario.game_state.world, spell_level);
        self
    }

    pub fn target(mut self, target: TargetInstance) -> Self {
        self.builder.target(&mut self.scenario.game_state, target);
        self
    }

    pub fn target_point(mut self, point: impl Into<Point3<f32>>) -> Self {
        self.builder
            .target_point(&mut self.scenario.game_state, point);
        self
    }

    pub fn target_entity(mut self, handle: impl Into<String>) -> Self {
        self.builder.target_entity(
            &mut self.scenario.game_state,
            self.scenario
                .creatures
                .get(&handle.into())
                .unwrap()
                .id(),
        );
        self
    }

    pub fn target_entities(mut self, handles: Vec<impl Into<String>>) -> Self {
        let targets = handles
            .into_iter()
            .map(|handle| {
                self.scenario
                    .creatures
                    .get(&handle.into())
                    .unwrap()
                    .id()
            })
            .collect::<Vec<_>>();
        for target in targets {
            self.builder
                .target_entity(&mut self.scenario.game_state, target);
        }
        self
    }

    pub fn perform(self) {
        self.builder.perform_ok(&mut self.scenario.game_state);
        // TODO: Do this in a more elegant way
        for _ in 0..10 {
            self.scenario.game_state.update(0.5);
        }
    }
}

pub struct ScenarioReactionBuilder<'s> {
    scenario: &'s mut Scenario,
    builder: ReactionBuilder,
}

impl ScenarioReactionBuilder<'_> {
    pub fn option_none(mut self) -> Self {
        self.builder.option_none();
        self
    }

    pub fn option_index(mut self, index: usize) -> Self {
        self.builder.option_index(index);
        self
    }

    pub fn option_filter(mut self, filter_fn: impl Fn(&ActionData) -> bool) -> Self {
        self.builder.option_filter(filter_fn);
        self
    }

    pub fn option_id(mut self, option_id: impl Into<ActionId>) -> Self {
        self.builder.option_id(option_id);
        self
    }

    pub fn perform(self) {
        self.builder.perform_ok(&mut self.scenario.game_state);
        // TODO: Do this in a more elegant way
        for _ in 0..10 {
            self.scenario.game_state.update(0.5);
        }
    }
}

/// A handle-scoped view over one creature in a `Scenario`. All the assertions
/// and interactions operate on that creature, borrowing the scenario's
/// `GameState` as needed. Chainable methods return `&mut Self` so calls can be
/// strung together.
pub struct ScenarioProbe<'s> {
    scenario: &'s mut Scenario,
    handle: String,
}

impl ScenarioProbe<'_> {
    fn creature(&self) -> EntityIdentifier {
        self.scenario.creature(&self.handle)
    }

    fn entity(&self) -> Entity {
        self.scenario.entity(&self.handle)
    }

    fn world(&self) -> &World {
        &self.scenario.game_state.world
    }

    // -- Actions --

    pub fn act(&mut self, action: impl Into<ActionId>) -> ScenarioActionBuilder<'_> {
        self.scenario.act(&self.handle, action)
    }

    pub fn react(&mut self) -> ScenarioReactionBuilder<'_> {
        self.scenario.react(&self.handle)
    }

    // -- World mutations --

    pub fn start_turn(&mut self) -> &mut Self {
        self.turn_boundary(TurnBoundary::Start)
    }

    pub fn end_turn(&mut self) -> &mut Self {
        self.turn_boundary(TurnBoundary::End)
    }

    fn turn_boundary(&mut self, boundary: TurnBoundary) -> &mut Self {
        let entity = self.entity();
        systems::time::advance_time(
            &mut self.scenario.game_state,
            entity,
            TimeStep::TurnBoundary { entity, boundary },
        );
        // The update sweeps any effects that expired at the boundary
        self.scenario.game_state.update(0.0);
        self
    }

    pub fn equip(&mut self, item: impl Into<ItemId>) -> &mut Self {
        let entity = self.entity();
        let creature = self.creature();
        let item_id = item.into();
        let item = ItemsRegistry::get(&item_id)
            .unwrap_or_else(|| panic!("No item with id {item_id} in registry"))
            .clone();
        if systems::loadout::equip(&mut self.scenario.game_state, entity, item).is_err() {
            panic!("Failed to equip {item_id} on {:?}", creature);
        }
        self
    }

    pub fn unequip(&mut self, slot: &EquipmentSlot) -> &mut Self {
        let entity = self.entity();
        systems::loadout::unequip(&mut self.scenario.game_state, entity, slot);
        self
    }

    pub fn damage_raw(&mut self, amount: u32) -> &mut Self {
        // TODO: This technically bypasses the normal damage pipeline, which is
        // quite complex, but I guess it's fine for testing?
        let entity = self.entity();
        systems::helpers::get_component_mut::<HitPoints>(&mut self.scenario.game_state.world, entity)
            .damage(amount);
        self
    }

    pub fn d20_check(&mut self, dc: &D20CheckDCKind) -> &mut Self {
        let entity = self.entity();
        let event = systems::d20::check(&mut self.scenario.game_state, entity, dc);
        self.scenario.game_state.process_event(event);
        self
    }

    pub fn d20_check_with_callback(
        &mut self,
        dc: &D20CheckDCKind,
        callback: EventCallback,
    ) -> &mut Self {
        let entity = self.entity();
        let event = systems::d20::check(&mut self.scenario.game_state, entity, dc);
        self.scenario
            .game_state
            .process_event_with_response_callback(event, callback);
        self
    }

    pub fn d20_force_outcome(
        &mut self,
        kind: D20CheckKind,
        outcome: D20CheckOutcome,
    ) -> &mut Self {
        let entity = self.entity();
        systems::d20::get_mut(&mut self.scenario.game_state.world, entity, &kind, |check| {
            check.set_forced_outcome(ModifierSource::Custom("Test outcome".to_string()), outcome);
        });
        self
    }

    pub fn d20_clear_forced_outcome(&mut self, kind: D20CheckKind) -> &mut Self {
        let entity = self.entity();
        systems::d20::get_mut(&mut self.scenario.game_state.world, entity, &kind, |check| {
            check.clear_forced_outcome();
        });
        self
    }

    // -- Value getters --

    pub fn hp(&self) -> u32 {
        systems::helpers::get_component::<HitPoints>(self.world(), self.entity()).current()
    }

    pub fn max_hp(&self) -> u32 {
        systems::helpers::get_component::<HitPoints>(self.world(), self.entity()).max()
    }

    pub fn is_alive(&self) -> bool {
        self.hp() > 0
    }

    pub fn movement_speed(&self) -> Length {
        systems::helpers::get_component::<Speed>(self.world(), self.entity()).total_speed()
    }

    pub fn position(&self) -> Point3<f32> {
        systems::geometry::get_foot_position(self.world(), self.entity())
            .unwrap_or_else(|| panic!("No position for creature {:?}", self.creature()))
    }

    pub fn resource(&self, resource: impl Into<ResourceId>) -> u8 {
        let resource = resource.into();
        let resources = systems::helpers::get_component::<ResourceMap>(self.world(), self.entity());
        let Some(amount) = resources.get(&resource) else {
            panic!(
                "Expected creature {:?} to have resource {:?}, but it was not found. Current resources: {:#?}",
                self.creature(), resource, resources
            );
        };
        let ResourceBudgetKind::Flat(budget) = amount else {
            panic!(
                "Expected resource {:?} to have a flat budget, but it was {:?}",
                resource, amount
            );
        };
        budget.current_uses
    }

    #[track_caller]
    pub fn resource_tiered(&self, resource: impl Into<ResourceId>, tier: u8) -> u8 {
        let resource = resource.into();
        let resources = systems::helpers::get_component::<ResourceMap>(self.world(), self.entity());
        let Some(amount) = resources.get(&resource) else {
            panic!(
                "Expected creature {:?} to have resource {:?}, but it was not found. Current resources: {:#?}",
                self.creature(), resource, resources
            );
        };
        let ResourceBudgetKind::Tiered(budgets) = amount else {
            panic!(
                "Expected resource {:?} to have a tiered budget, but it was {:?}",
                resource, amount
            );
        };
        let Some(budget) = budgets.get(&tier) else {
            panic!(
                "Expected resource {:?} to have a tier {} budget, but it has tiers {:?}",
                resource,
                tier,
                budgets.keys().collect::<Vec<_>>()
            );
        };
        budget.current_uses
    }

    pub fn effect_remaining_turns(&self, effect: impl Into<EffectId>) -> Option<u32> {
        systems::effects::effect_remaining_duration(
            &self.scenario.game_state,
            self.entity(),
            &effect.into(),
        )
        .map(|duration| duration.as_turns())
    }

    // -- Assertions --

    #[track_caller]
    pub fn assert_position(
        &mut self,
        expected: impl Into<Point3<f32>>,
        tolerance: Length,
    ) -> &mut Self {
        let expected = expected.into();
        let position = self.position();
        let distance = (position - expected).norm();
        assert!(
            distance <= tolerance.get::<meter>(),
            "Expected creature {:?} to be within {:?} m of {:?}, but it was at {:?} ({:?} m away)",
            self.creature(),
            tolerance.get::<meter>(),
            expected,
            position,
            distance
        );
        self
    }

    #[track_caller]
    pub fn assert_has_action(&mut self, action: impl Into<ActionId>) -> &mut Self {
        let action: ActionId = action.into();
        let actions = systems::actions::all_actions(self.world(), self.entity());
        assert!(
            actions.contains_key(&action),
            "Expected creature {:?} to have action {:?}, but it was not found. Available actions: {:#?}",
            self.creature(),
            action,
            actions
        );
        self
    }

    #[track_caller]
    pub fn assert_no_action(&mut self, action: impl Into<ActionId>) -> &mut Self {
        let action: ActionId = action.into();
        let actions = systems::actions::all_actions(self.world(), self.entity());
        assert!(
            !actions.contains_key(&action),
            "Expected creature {:?} to not have action {:?}, but it was found. Available actions: {:#?}",
            self.creature(),
            action,
            actions
        );
        self
    }

    #[track_caller]
    pub fn assert_action_available(&mut self, action: impl Into<ActionId>) -> &mut Self {
        let action: ActionId = action.into();
        let actions = systems::actions::available_actions(&self.scenario.game_state, self.entity());
        assert!(
            actions.contains_key(&action),
            "Expected creature {:?} to have action {:?}, but it was not found. Available actions: {:#?}",
            self.creature(),
            action,
            actions
        );
        self
    }

    #[track_caller]
    pub fn assert_action_unavailable(&mut self, action: impl Into<ActionId>) -> &mut Self {
        let action: ActionId = action.into();
        let actions = systems::actions::available_actions(&self.scenario.game_state, self.entity());
        assert!(
            !actions.contains_key(&action),
            "Expected creature {:?} to not have action {:?}, but it was found. Available actions: {:#?}",
            self.creature(),
            action,
            actions
        );
        self
    }

    #[track_caller]
    pub fn assert_resource(
        &mut self,
        resource: impl Into<ResourceId>,
        operator: Operator<u8>,
    ) -> &mut Self {
        let resource = resource.into();
        let current_uses = self.resource(resource.clone());
        assert!(
            operator.evaluate(&current_uses),
            "Resource {:?} expected to satisfy condition {:?} but was {:?}",
            resource,
            operator,
            current_uses
        );
        self
    }

    #[track_caller]
    pub fn assert_resource_tiered(
        &mut self,
        resource: impl Into<ResourceId>,
        tier: u8,
        operator: Operator<u8>,
    ) -> &mut Self {
        let resource = resource.into();
        let current_uses = self.resource_tiered(resource.clone(), tier);
        assert!(
            operator.evaluate(&current_uses),
            "Resource {:?} tier {} expected to satisfy condition {:?} but was {:?}",
            resource,
            tier,
            operator,
            current_uses
        );
        self
    }

    #[track_caller]
    pub fn assert_no_resource(&mut self, resource: impl Into<ResourceId>) -> &mut Self {
        let resource = resource.into();
        let resources = systems::helpers::get_component::<ResourceMap>(self.world(), self.entity());
        assert!(
            resources.get(&resource).is_none(),
            "Expected creature {:?} to not have resource {:?}, but it was found. Current resources: {:#?}",
            self.creature(),
            resource,
            resources
        );
        drop(resources);
        self
    }

    #[track_caller]
    pub fn assert_effect(&mut self, effect_id: impl Into<EffectId>) -> &mut Self {
        let effect_id = effect_id.into();
        let effects = systems::effects::effects(self.world(), self.entity());
        assert!(
            effects
                .iter()
                .any(|(_, instance)| instance.effect_id == effect_id),
            "Expected creature {:?} to have effect {:?}, but it was not found. Current effects: {:#?}",
            self.creature(),
            effect_id,
            effects
        );
        drop(effects);
        self
    }

    #[track_caller]
    pub fn assert_no_effect(&mut self, effect_id: impl Into<EffectId>) -> &mut Self {
        let effect_id = effect_id.into();
        let effects = systems::effects::effects(self.world(), self.entity());
        assert!(
            !effects
                .iter()
                .any(|(_, instance)| instance.effect_id == effect_id),
            "Expected creature {:?} to not have effect {:?}, but it was found. Current effects: {:#?}",
            self.creature(),
            effect_id,
            effects
        );
        drop(effects);
        self
    }

    #[track_caller]
    pub fn assert_effect_instances(
        &mut self,
        effect_id: impl Into<EffectId>,
        expected: usize,
    ) -> &mut Self {
        let effect_id = effect_id.into();
        let count = systems::effects::effects(self.world(), self.entity())
            .values()
            .filter(|instance| instance.effect_id == effect_id)
            .count();
        assert_eq!(
            count, expected,
            "Expected creature {:?} to have {} instances of effect {:?}, but found {}",
            self.creature(),
            expected,
            effect_id,
            count
        );
        self
    }

    #[track_caller]
    pub fn assert_damage_resistance(&mut self, damage_type: DamageType) -> &mut Self {
        let resistances =
            systems::helpers::get_component::<DamageResistances>(self.world(), self.entity());
        assert!(
            resistances.effective_resistance(damage_type).is_some(),
            "Expected creature {:?} to have {:?} resistance, but it was not found. Current resistances: {:#?}",
            self.creature(),
            damage_type,
            resistances
        );
        drop(resistances);
        self
    }

    #[track_caller]
    pub fn assert_no_damage_resistance(&mut self, damage_type: DamageType) -> &mut Self {
        let resistances =
            systems::helpers::get_component::<DamageResistances>(self.world(), self.entity());
        assert!(
            resistances.effective_resistance(damage_type).is_none(),
            "Expected creature {:?} to not have {:?} resistance, but it was found. Current resistances: {:#?}",
            self.creature(),
            damage_type,
            resistances
        );
        drop(resistances);
        self
    }

    #[track_caller]
    pub fn assert_on_cooldown(&mut self, action_id: impl Into<ActionId>) -> &mut Self {
        let action_id = action_id.into();
        let cooldown = systems::actions::on_cooldown(self.world(), self.entity(), &action_id);
        assert!(
            cooldown.is_some(),
            "Expected creature {:?} to have action {:?} on cooldown, but it was not found",
            self.creature(),
            action_id,
        );
        self
    }

    #[track_caller]
    pub fn assert_hp(&mut self, operator: Operator<u32>) -> &mut Self {
        let current = self.hp();
        assert!(
            operator.evaluate(&current),
            "Expected creature {:?} to have HP satisfying condition {:?}, but it was {:?}",
            self.creature(),
            operator,
            current
        );
        self
    }

    #[track_caller]
    pub fn assert_movement_speed(&mut self, operator: Operator<Length>) -> &mut Self {
        let speed = self.movement_speed();
        assert!(
            operator.evaluate(&speed),
            "Expected creature {:?} to have movement speed satisfying condition {:?}, but it was {:?}",
            self.creature(),
            operator,
            speed
        );
        self
    }

    #[track_caller]
    pub fn assert_free_movement(
        &mut self,
        source: ModifierSource,
        operator: Operator<f32>,
    ) -> &mut Self {
        let speed = systems::helpers::get_component::<Speed>(self.world(), self.entity());
        assert!(
            speed
                .free_movement_multipliers()
                .get(&source)
                .map(|v| operator.evaluate(v))
                .unwrap_or(false),
            "Expected creature {:?} to have free multiplier movement from {:?} satisfying condition {:?}, but it has: {:#?}",
            self.creature(),
            source,
            operator,
            speed.free_movement_multipliers()
        );
        drop(speed);
        self
    }

    #[track_caller]
    pub fn assert_d20_advantage(
        &mut self,
        kind: &D20CheckKind,
        source: &ModifierSource,
        advantage_type: AdvantageType,
    ) -> &mut Self {
        let creature = self.creature();
        let kind = kind.clone();
        let source = source.clone();
        self.assert_d20(&kind, |check| {
            assert!(
                check.advantage_tracker().summary().contains(&(&source, advantage_type)),
                "Expected creature {:?} to have {:?} {:?} on {:?} check, but it was not found. Current advantages: {:#?}",
                creature,
                advantage_type,
                source,
                kind,
                check.advantage_tracker().summary()
            );
        });
        self
    }

    #[track_caller]
    pub fn assert_d20_crit_threshold_reduction(
        &mut self,
        kind: &D20CheckKind,
        source: &ModifierSource,
        operator: Operator<i32>,
    ) -> &mut Self {
        let creature = self.creature();
        let kind = kind.clone();
        let source = source.clone();
        self.assert_d20(&kind, |check| {
            let reduction = check.crit_threshold_reduction().get(&source);
            assert!(
                reduction.map(|r| operator.evaluate(r)).unwrap_or(false),
                "Expected creature {:?} to have critical threshold reduction from {:?} on {:?} check satisfying condition {:?}, but it was {:?}. Current reductions: {:#?}",
                creature,
                source,
                kind,
                operator,
                reduction,
                check.crit_threshold_reduction()
            );
        });
        self
    }

    #[track_caller]
    fn assert_d20(&self, kind: &D20CheckKind, assertor: impl Fn(&D20Check)) {
        let entity = self.entity();
        let world = self.world();
        match kind {
            D20CheckKind::SavingThrow(saving_throw_kind) => {
                assertor(
                    systems::helpers::get_component::<SavingThrowSet>(world, entity)
                        .get(saving_throw_kind),
                );
            }

            D20CheckKind::Skill(skill) => {
                assertor(systems::helpers::get_component::<SkillSet>(world, entity).get(skill));
            }

            D20CheckKind::AttackRoll(attack_source) => match attack_source {
                AttackSource::Weapon(weapon_kind) => {
                    assertor(
                        &systems::helpers::get_component::<Loadout>(world, entity)
                            .attack_roll_template(weapon_kind)
                            .d20_check,
                    );
                }

                AttackSource::Spell => {
                    assertor(
                        &systems::helpers::get_component::<Spellbook>(world, entity)
                            .attack_roll_template()
                            .d20_check,
                    );
                }
            },
        }
    }

    // TODO: Update this if we add support for concentration on things other than effects
    #[track_caller]
    pub fn assert_concentration(&mut self, effect_id: impl Into<EffectId>) -> &mut Self {
        let effect_id = effect_id.into();
        let spellbook = systems::helpers::get_component::<Spellbook>(self.world(), self.entity());
        let concentration = spellbook
            .concentration_tracker()
            .instances()
            .iter()
            .find(|instance| match instance {
                ConcentrationInstance::Effect { effect, .. } => *effect == effect_id,
            });
        assert!(
            concentration.is_some(),
            "Expected creature {:?} to be concentrating on effect {:?}, but it was not found. Current concentration instances: {:#?}",
            self.creature(),
            effect_id,
            spellbook.concentration_tracker().instances()
        );
        drop(spellbook);
        self
    }

    #[track_caller]
    pub fn assert_no_concentration(&mut self) -> &mut Self {
        let spellbook = systems::helpers::get_component::<Spellbook>(self.world(), self.entity());
        assert!(
            spellbook.concentration_tracker().instances().is_empty(),
            "Expected creature {:?} to not be concentrating, but it has concentration instances: {:#?}",
            self.creature(),
            spellbook.concentration_tracker().instances()
        );
        drop(spellbook);
        self
    }

    #[track_caller]
    pub fn assert_armor_class(&mut self, modifiers: &FlatModifierMap) -> &mut Self {
        let entity = self.entity();
        let armor_class = systems::helpers::get_component::<Loadout>(self.world(), entity)
            .armor_class(&self.scenario.game_state, entity);
        assert_eq!(
            armor_class.modifiers(),
            modifiers,
            "Expected creature {:?} to have armor class modifiers {:#?}, but it was {:#?}",
            self.creature(),
            modifiers,
            armor_class.modifiers()
        );
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator<T> {
    Equal(T),
    NotEqual(T),
    Greater(T),
    Less(T),
    AtLeast(T),
    AtMost(T),
}

impl<T: PartialOrd + PartialEq> Operator<T> {
    pub fn evaluate(&self, value: &T) -> bool {
        match self {
            Operator::Equal(target) => value == target,
            Operator::NotEqual(target) => value != target,
            Operator::Greater(target) => value > target,
            Operator::Less(target) => value < target,
            Operator::AtLeast(target) => value >= target,
            Operator::AtMost(target) => value <= target,
        }
    }
}

// TODO: Not sure if this is overkill?
pub struct ScenarioEventFilterBuilder<'s> {
    scenario: &'s Scenario,
    actor: Option<Entity>,
    kind: Option<EventFilterKind>,
}

impl ScenarioEventFilterBuilder<'_> {
    pub fn actor(mut self, handle: impl Into<String>) -> Self {
        let handle = handle.into();
        self.actor = Some(
            self.scenario
                .creatures
                .get(&handle)
                .unwrap_or_else(|| panic!("No creature with handle {handle} in scenario"))
                .id(),
        );
        self
    }

    pub fn d20_modifier<T>(mut self, kind: D20CheckKind, source: ModifierSource, value: T) -> Self
    where
        T: Into<ModifierKind>,
    {
        self.kind = Some(EventFilterKind::D20Check {
            kind,
            modifier: Some((source, value.into())),
            advantage: None,
        });
        self
    }

    pub fn d20_advantage(
        mut self,
        kind: D20CheckKind,
        source: ModifierSource,
        advantage_type: AdvantageType,
    ) -> Self {
        self.kind = Some(EventFilterKind::D20Check {
            kind,
            modifier: None,
            advantage: Some((source, advantage_type)),
        });
        self
    }

    pub fn damage_roll(mut self, damage: DamageComponent) -> Self {
        self.kind = Some(EventFilterKind::DamageRoll { damage });
        self
    }

    pub fn damage_dealt(mut self, target: impl Into<String>, damage: DamageComponent) -> Self {
        let target = target.into();
        let target = self
            .scenario
            .creatures
            .get(&target)
            .unwrap_or_else(|| panic!("No creature with handle {target} in scenario"))
            .id();
        self.kind = Some(EventFilterKind::DamageDealt { target, damage });
        self
    }

    fn build(&self) -> EventFilter {
        EventFilter::new({
            let actor = self.actor;
            let kind = self.kind.clone();
            move |event| {
                if let Some(actor) = actor
                    && let Some(event_actor) = event.actor()
                    && actor != event_actor
                {
                    return false;
                }

                if let Some(kind) = &kind {
                    return kind.matches(event);
                } else {
                    true
                }
            }
        })
    }

    pub fn filter(&self) -> Vec<&Event> {
        self.scenario.filter_events(self.build())
    }

    #[track_caller]
    pub fn assert_event(&self) {
        assert!(
            !self.filter().is_empty(),
            "Expected event matching filter, but no such event was found. Events: {:?}",
            self.scenario.game_state.event_log.events
        );
    }

    #[track_caller]
    pub fn assert_event_count(&self, expected_count: usize) {
        let events = self.filter();
        assert!(
            events.len() == expected_count,
            "Expected {} events matching filter, but found {}. \
            \n--- \
            \nFilter: {:?} \
            \n--- \
            \nEvents: {:?}",
            expected_count,
            events.len(),
            self,
            self.event_log()
        );
    }

    fn event_log(&self) -> &Vec<Event> {
        if let Some(encounter_id) = &self.scenario.encounter_id {
            &self
                .scenario
                .game_state
                .encounters
                .get(encounter_id)
                .unwrap_or_else(|| panic!("No encounter with id {encounter_id} in game state"))
                .event_log()
                .events
        } else {
            &self.scenario.game_state.event_log.events
        }
    }
}

impl std::fmt::Debug for ScenarioEventFilterBuilder<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ScenarioEventFilterBuilder")
            .field("actor", &self.actor)
            .field("kind", &self.kind)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum EventFilterKind {
    D20Check {
        kind: D20CheckKind,
        modifier: Option<(ModifierSource, ModifierKind)>,
        advantage: Option<(ModifierSource, AdvantageType)>,
    },
    DamageRoll {
        damage: DamageComponent,
    },
    DamageDealt {
        target: Entity,
        damage: DamageComponent,
    },
}

impl EventFilterKind {
    pub fn matches(&self, event: &Event) -> bool {
        match (self, &event.kind) {
            (
                EventFilterKind::D20Check {
                    kind,
                    modifier,
                    advantage,
                },
                EventKind::D20CheckResolved { result, dc, .. },
            ) => {
                if *kind != dc.kind() {
                    return false;
                }

                if let Some((modifier_source, modifier_value)) = modifier {
                    let Some(event_modifier) =
                        result.d20_result().modifier_result.get(modifier_source)
                    else {
                        return false;
                    };

                    if !event_modifier.matches_kind(modifier_value) {
                        return false;
                    }
                }

                if let Some((advantage_source, advantage_type)) = advantage
                    && !result
                        .d20_result()
                        .advantage_tracker()
                        .summary()
                        .contains(&(advantage_source, *advantage_type))
                {
                    return false;
                }

                true
            }

            (
                EventFilterKind::DamageRoll {
                    damage: expected_damage,
                },
                EventKind::DamageRollResolved { result, .. },
            ) => {
                for component in &result.components {
                    if component.damage_type == expected_damage.damage_type
                        && result_contains_expected(expected_damage.modifiers(), &component.result)
                    {
                        return true;
                    }
                }

                false
            }

            (
                EventFilterKind::DamageDealt {
                    target: expected_target,
                    damage: expected_damage,
                },
                EventKind::ActionResult { result, .. },
            ) => {
                if result.target.id() != *expected_target {
                    return false;
                }

                for component in result.components() {
                    if let ActionResultComponent::Damage(damage_result) = component
                        && let Some(damage_result) = &damage_result.damage_taken
                    {
                        for component in &damage_result.components {
                            if component.damage_type == expected_damage.damage_type
                                && result_contains_expected(
                                    expected_damage.modifiers(),
                                    &component.original,
                                )
                            {
                                return true;
                            }
                        }
                    }
                }

                false
            }

            _ => false,
        }
    }
}

fn result_contains_expected(expected: &ModifierMap, result: &ModifierResult) -> bool {
    expected.iter().all(|(source, expected_kind)| {
        result
            .get(source)
            .map(|r| r.matches_kind(expected_kind))
            .unwrap_or(false)
    })
}
