use std::collections::HashSet;

use parry3d::{na::Point3, utils::hashmap::HashMap};

use crate::{
    components::{
        actions::{
            action::ActionContext,
            action_builder::{ActionBuilder, ReactionBuilder},
            targeting::TargetInstance,
        },
        d20::D20CheckOutcome,
        id::{ActionId, EffectId, ResourceId},
        modifier::{Modifiable, ModifierSource},
        resource::ResourceAmountMap,
        skill::{Skill, SkillSet},
        time::TimeMode,
    },
    engine::{
        action_prompt::ReactionData,
        encounter::EncounterId,
        event::{Event, EventCallback, EventFilter},
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
}

impl Scenario {
    pub fn from_game_state(game_state: GameState) -> Self {
        Self {
            game_state,
            creatures: HashMap::default(),
            encounter_id: None,
        }
    }

    pub fn new() -> Self {
        Self::from_game_state(fixtures::engine::game_state())
    }

    pub fn probe(&mut self, handle: impl Into<String>) -> ScenarioProbe<'_> {
        ScenarioProbe {
            scenario: self,
            handle: handle.into(),
        }
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

    pub fn filter_events(&self, event_filter: EventFilter) -> Option<&Event> {
        let event_log = if let Some(encounter_id) = &self.encounter_id {
            self.game_state
                .encounters
                .get(encounter_id)
                .unwrap_or_else(|| panic!("No encounter with id {encounter_id} in game state"))
                .combat_log()
        } else {
            &self.game_state.event_log
        };

        event_log
            .events
            .iter()
            .rev()
            .find(|event| event_filter.matches(event))
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

// impl Deref for ScenarioCreatureBuilder<'_> {
//     type Target = CreatureBuilder;

//     fn deref(&self) -> &Self::Target {
//         &self.builder
//     }
// }

// impl DerefMut for ScenarioCreatureBuilder<'_> {
//     fn deref_mut(&mut self) -> &mut Self::Target {
//         &mut self.builder
//     }
// }

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
            .variant(&self.scenario.game_state.world, &variant.into());
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

    pub fn option_filter(mut self, filter_fn: impl Fn(&ReactionData) -> bool) -> Self {
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

macro_rules! delegate_probe_methods {
    ( $( fn $name:ident($($param:ident : $type:ty),*) );* $(;)? ) => {
        $(
            #[track_caller]
            pub fn $name(&mut self, $($param: $type),*) -> &mut Self {
                let probe: &mut CreatureProbe = self
                    .scenario
                    .creatures
                    .get_mut(&self.handle)
                    .unwrap_or_else(|| panic!("No creature with handle {} in scenario", self.handle));
                probe.$name(&mut self.scenario.game_state, $($param),*);
                self
            }
        )*
    };
}

pub struct ScenarioProbe<'s> {
    scenario: &'s mut Scenario,
    handle: String,
}

impl ScenarioProbe<'_> {
    delegate_probe_methods! {
        fn start_turn();

        fn hp();
        fn max_hp();
        fn is_alive();
        fn damage_raw(amount: u32);
        fn d20_force_outcome(kind: D20CheckKind, outcome: D20CheckOutcome);
        fn d20_clear_forced_outcome(kind: D20CheckKind);
        fn d20_check(dc: &D20CheckDCKind);
        fn d20_check_with_callback(dc: &D20CheckDCKind, callback: EventCallback);

        fn assert_has_action(action: impl Into<ActionId>);
        fn assert_resource(resource: impl Into<ResourceId>, operator: Operator<u8>);
        fn assert_no_resource(resource: impl Into<ResourceId>);
        fn assert_effect(effect: impl Into<EffectId>);
        fn assert_no_effect(effect: impl Into<EffectId>);
        fn assert_on_cooldown(action: impl Into<ActionId>);
        fn assert_hp(amount: Operator<u32>);
    }

    pub fn act(&mut self, action: impl Into<ActionId>) -> ScenarioActionBuilder<'_> {
        self.scenario.act(&self.handle, action)
    }

    pub fn react(&mut self) -> ScenarioReactionBuilder<'_> {
        self.scenario.react(&self.handle)
    }
}
