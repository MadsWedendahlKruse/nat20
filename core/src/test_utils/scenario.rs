use std::{collections::HashSet, u32};

use hecs::Entity;
use parry3d::{na::Point3, utils::hashmap::HashMap};
use tracing::subscriber::DefaultGuard;
use tracing_subscriber::{EnvFilter, util::SubscriberInitExt};
use uom::si::f32::Length;

use crate::{
    components::{
        actions::{
            action::{ActionContext, ActionResultComponent},
            action_builder::{ActionBuilder, ReactionBuilder},
            targeting::TargetInstance,
        },
        d20::{AdvantageType, D20CheckOutcome},
        damage::{DamageComponent, DamageSource},
        id::{ActionId, EffectId, ResourceId},
        modifier::{ModifierKind, ModifierMap, ModifierResult, ModifierSource},
        resource::ResourceAmountMap,
        skill::{Skill, SkillSet},
        time::TimeMode,
    },
    engine::{
        action_prompt::ActionData,
        encounter::EncounterId,
        event::{Event, EventCallback, EventFilter, EventKind},
        game_state::GameState,
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20CheckKind},
    },
    test_utils::{
        creature_builder::CreatureBuilder,
        creature_probe::{CreatureProbe, Operator},
        fixtures,
    },
};

pub struct Scenario {
    pub game_state: GameState,
    pub creatures: HashMap<String, CreatureProbe>,
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

    pub fn probes(
        &mut self,
        handles: impl IntoIterator<Item = impl Into<String>>,
    ) -> ScenarioProbes<'_> {
        ScenarioProbes {
            scenario: self,
            handles: handles.into_iter().map(Into::into).collect(),
        }
    }

    fn probe_parts(&mut self, handle: &str) -> (&mut CreatureProbe, &mut GameState) {
        let probe = self
            .creatures
            .get_mut(handle)
            .unwrap_or_else(|| panic!("No creature with handle {handle} in scenario"));
        (probe, &mut self.game_state)
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
        let builder = self
            .creatures
            .get(&handle)
            .unwrap_or_else(|| panic!("No creature with handle {handle} in scenario"))
            .act(&mut self.game_state, action);
        ScenarioActionBuilder {
            scenario: self,
            builder,
        }
    }

    pub fn react<'s>(&mut self, handle: impl Into<String>) -> ScenarioReactionBuilder<'_> {
        let handle = handle.into();
        let builder = self
            .creatures
            .get(&handle)
            .unwrap_or_else(|| panic!("No creature with handle {handle} in scenario"))
            .react(&mut self.game_state);
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
        self.scenario.creatures.insert(
            self.handle,
            self.builder.probe(&mut self.scenario.game_state),
        );
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
            .map(|probe| probe.creature.id())
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
            let entity = creature.creature.id();
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
                .creature
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
                    .creature
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

/// Emits the same delegated probe methods on both `ScenarioProbe` (one
/// creature) and `ScenarioProbes` (several creatures, arguments cloned per
/// creature), so the two can never drift apart.
macro_rules! delegate_probe_methods {
    ( $( fn $name:ident($($param:ident : $type:ty),*) );* $(;)? ) => {
        impl ScenarioProbe<'_> {
            $(
                #[track_caller]
                pub fn $name(&mut self, $($param: $type),*) -> &mut Self {
                    let (probe, game_state) = self.scenario.probe_parts(&self.handle);
                    probe.$name(game_state, $($param),*);
                    self
                }
            )*
        }

        impl ScenarioProbes<'_> {
            $(
                #[track_caller]
                pub fn $name(&mut self, $($param: $type),*) -> &mut Self {
                    for handle in &self.handles {
                        let (probe, game_state) = self.scenario.probe_parts(handle);
                        // UFCS so a &T parameter clones the reference, not the T
                        probe.$name(game_state, $(Clone::clone(&$param)),*);
                    }
                    self
                }
            )*
        }
    };
}

pub struct ScenarioProbe<'s> {
    scenario: &'s mut Scenario,
    handle: String,
}

pub struct ScenarioProbes<'s> {
    scenario: &'s mut Scenario,
    handles: Vec<String>,
}

delegate_probe_methods! {
    fn start_turn();

    fn damage_raw(amount: u32);
    fn d20_force_outcome(kind: D20CheckKind, outcome: D20CheckOutcome);
    fn d20_clear_forced_outcome(kind: D20CheckKind);
    fn d20_check(dc: &D20CheckDCKind);
    fn d20_check_with_callback(dc: &D20CheckDCKind, callback: EventCallback);

    fn assert_has_action(action: impl Into<ActionId> + Clone);
    fn assert_no_action(action: impl Into<ActionId> + Clone);
    fn assert_action_available(action: impl Into<ActionId> + Clone);
    fn assert_action_unavailable(action: impl Into<ActionId> + Clone);
    fn assert_resource(resource: impl Into<ResourceId> + Clone, operator: Operator<u8>);
    fn assert_resource_tiered(resource: impl Into<ResourceId> + Clone, tier: u8, operator: Operator<u8>);
    fn assert_position(expected: impl Into<Point3<f32>> + Clone, tolerance: Length);
    fn assert_no_resource(resource: impl Into<ResourceId> + Clone);
    fn assert_effect(effect: impl Into<EffectId> + Clone);
    fn assert_no_effect(effect: impl Into<EffectId> + Clone);
    fn assert_on_cooldown(action: impl Into<ActionId> + Clone);
    fn assert_hp(amount: Operator<u32>);
    fn assert_movement_speed(operator: Operator<Length>);
    fn assert_free_movement(source: ModifierSource, operator: Operator<f32>);
    fn assert_d20_advantage(kind: &D20CheckKind, source: &ModifierSource, advantage_type: AdvantageType);
    fn assert_d20_crit_threshold_reduction(kind: &D20CheckKind, source: &ModifierSource, operator: Operator<i32>);
    fn assert_concentration(effect: impl Into<EffectId> + Clone);
    fn assert_no_concentration();
}

impl ScenarioProbe<'_> {
    pub fn act(&mut self, action: impl Into<ActionId>) -> ScenarioActionBuilder<'_> {
        self.scenario.act(&self.handle, action)
    }

    pub fn react(&mut self) -> ScenarioReactionBuilder<'_> {
        self.scenario.react(&self.handle)
    }

    pub fn hp(&mut self) -> u32 {
        let (probe, game_state) = self.scenario.probe_parts(&self.handle);
        probe.hp(game_state)
    }

    pub fn max_hp(&mut self) -> u32 {
        let (probe, game_state) = self.scenario.probe_parts(&self.handle);
        probe.max_hp(game_state)
    }

    pub fn movement_speed(&mut self) -> Length {
        let (probe, game_state) = self.scenario.probe_parts(&self.handle);
        probe.movement_speed(game_state)
    }

    pub fn position(&mut self) -> Point3<f32> {
        let (probe, game_state) = self.scenario.probe_parts(&self.handle);
        probe.position(game_state)
    }

    pub fn reource(&mut self, resource: impl Into<ResourceId>) -> u8 {
        let (probe, game_state) = self.scenario.probe_parts(&self.handle);
        probe.resource(game_state, resource)
    }

    pub fn resource_tiered(&mut self, resource: impl Into<ResourceId>, tier: u8) -> u8 {
        let (probe, game_state) = self.scenario.probe_parts(&self.handle);
        probe.resource_tiered(game_state, resource, tier)
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
                .creature
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

    pub fn damage_roll(mut self, damage: DamageComponent, source: DamageSource) -> Self {
        self.kind = Some(EventFilterKind::DamageRoll { damage, source });
        self
    }

    pub fn damage_dealt(
        mut self,
        target: impl Into<String>,
        damage: DamageComponent,
        source: DamageSource,
    ) -> Self {
        let target = target.into();
        let target = self
            .scenario
            .creatures
            .get(&target)
            .unwrap_or_else(|| panic!("No creature with handle {target} in scenario"))
            .creature
            .id();
        self.kind = Some(EventFilterKind::DamageDealt {
            target,
            damage,
            source,
        });
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
        source: DamageSource,
    },
    DamageDealt {
        target: Entity,
        damage: DamageComponent,
        source: DamageSource,
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
                    let Some(event_modifier) = result
                        .d20_result()
                        .check
                        .modifiers()
                        .modifiers
                        .get(modifier_source)
                    else {
                        return false;
                    };

                    if event_modifier != modifier_value {
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
                    source: expected_source,
                },
                EventKind::DamageRollResolved { result, .. },
            ) => {
                if &result.source != expected_source {
                    return false;
                }

                for component in &result.components {
                    if component.damage_type == expected_damage.damage_type
                        && result_contains_expected(&expected_damage.damage, &component.result)
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
                    source: expected_source,
                },
                EventKind::ActionResult { result, .. },
            ) => {
                if result.target.id() != *expected_target {
                    return false;
                }

                for component in result.components() {
                    if let ActionResultComponent::Damage(damage_result) = component
                        && let Some(damage_result) = &damage_result.damage_taken
                        && damage_result.source == *expected_source
                    {
                        for component in &damage_result.components {
                            if component.damage_type == expected_damage.damage_type
                                && result_contains_expected(
                                    &expected_damage.damage,
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
    expected.modifiers.iter().all(|(source, expected_kind)| {
        result
            .results
            .get(source)
            .map(|r| r.matches_kind(expected_kind))
            .unwrap_or(false)
    })
}
