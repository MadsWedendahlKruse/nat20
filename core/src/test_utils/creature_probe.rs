use parry3d::na::Point3;
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{
        actions::action_builder::{ActionBuilder, ReactionBuilder},
        d20::{AdvantageType, D20Check, D20CheckOutcome},
        damage::AttackSource,
        health::hit_points::HitPoints,
        id::{ActionId, EffectId, EntityIdentifier, ResourceId},
        items::equipment::loadout::Loadout,
        modifier::ModifierSource,
        resource::{ResourceBudgetKind, ResourceMap},
        saving_throw::SavingThrowSet,
        skill::SkillSet,
        speed::Speed,
        spells::{spell::ConcentrationInstance, spellbook::Spellbook},
        time::{TimeStep, TurnBoundary},
    },
    engine::{event::EventCallback, game_state::GameState},
    systems::{
        self,
        d20::{D20CheckDCKind, D20CheckKind},
    },
};

// TODO: Not sure about the name
pub struct CreatureProbe {
    pub creature: EntityIdentifier,
}

impl CreatureProbe {
    pub fn new(creature: EntityIdentifier) -> Self {
        Self { creature }
    }

    pub fn act(&self, game_state: &mut GameState, action: impl Into<ActionId>) -> ActionBuilder {
        let mut builder = ActionBuilder::available(game_state, self.creature.id());
        builder.action(game_state, &action.into());
        builder
    }

    pub fn react(&self, game_state: &mut GameState) -> ReactionBuilder {
        ReactionBuilder::new(game_state, self.creature.id())
    }

    pub fn start_turn(&self, game_state: &mut GameState) {
        systems::time::advance_time(
            game_state,
            self.creature.id(),
            TimeStep::TurnBoundary {
                entity: self.creature.id(),
                boundary: TurnBoundary::Start,
            },
        );
        game_state.update(0.0);
    }

    pub fn hp(&self, game_state: &GameState) -> u32 {
        let hit_points =
            systems::helpers::get_component::<HitPoints>(&game_state.world, self.creature.id());
        hit_points.current()
    }

    pub fn max_hp(&self, game_state: &GameState) -> u32 {
        let hit_points =
            systems::helpers::get_component::<HitPoints>(&game_state.world, self.creature.id());
        hit_points.max()
    }

    pub fn is_alive(&self, game_state: &GameState) -> bool {
        self.hp(game_state) > 0
    }

    pub fn damage_raw(&mut self, game_state: &mut GameState, amount: u32) {
        // TODO: This technically bypasses the normal damage pipeline, which is
        // quite complex, but I guess it's fine for testing?
        systems::helpers::get_component_mut::<HitPoints>(&mut game_state.world, self.creature.id())
            .damage(amount);
    }

    pub fn d20_check(&mut self, game_state: &mut GameState, dc: &D20CheckDCKind) {
        let event = systems::d20::check(game_state, self.creature.id(), dc);
        game_state.process_event(event);
    }

    pub fn d20_check_with_callback(
        &mut self,
        game_state: &mut GameState,
        dc: &D20CheckDCKind,
        callback: EventCallback,
    ) {
        let event = systems::d20::check(game_state, self.creature.id(), dc);
        game_state.process_event_with_response_callback(event, callback);
    }

    pub fn d20_force_outcome(
        &mut self,
        game_state: &mut GameState,
        kind: D20CheckKind,
        outcome: D20CheckOutcome,
    ) {
        systems::d20::get_mut(&mut game_state.world, self.creature.id(), &kind, |check| {
            check.set_forced_outcome(ModifierSource::Custom("Test outcome".to_string()), outcome);
        });
    }

    pub fn d20_clear_forced_outcome(&mut self, game_state: &mut GameState, kind: D20CheckKind) {
        systems::d20::get_mut(&mut game_state.world, self.creature.id(), &kind, |check| {
            check.clear_forced_outcome();
        });
    }

    pub fn position(&self, game_state: &GameState) -> Point3<f32> {
        systems::geometry::get_foot_position(&game_state.world, self.creature.id())
            .unwrap_or_else(|| panic!("No position for creature {:?}", self.creature))
    }

    #[track_caller]
    pub fn assert_position(
        &self,
        game_state: &GameState,
        expected: impl Into<Point3<f32>>,
        tolerance: Length,
    ) {
        let expected = expected.into();
        let position = self.position(game_state);
        let distance = (position - expected).norm();
        assert!(
            distance <= tolerance.get::<meter>(),
            "Expected creature {:?} to be within {:?} m of {:?}, but it was at {:?} ({:?} m away)",
            self.creature,
            tolerance.get::<meter>(),
            expected,
            position,
            distance
        );
    }

    pub fn movement_speed(&self, game_state: &GameState) -> Length {
        let speed = systems::helpers::get_component::<Speed>(&game_state.world, self.creature.id());
        speed.total_speed()
    }

    #[track_caller]
    pub fn assert_has_action(&self, game_state: &GameState, action: impl Into<ActionId>) {
        let action: ActionId = action.into();
        let actions = systems::actions::all_actions(&game_state.world, self.creature.id());
        assert!(
            actions.contains_key(&action),
            "Expected creature {:?} to have action {:?}, but it was not found. Available actions: {:#?}",
            self.creature,
            action,
            actions
        );
    }

    #[track_caller]
    pub fn assert_no_action(&self, game_state: &GameState, action: impl Into<ActionId>) {
        let action: ActionId = action.into();
        let actions = systems::actions::all_actions(&game_state.world, self.creature.id());
        assert!(
            !actions.contains_key(&action),
            "Expected creature {:?} to not have action {:?}, but it was found. Available actions: {:#?}",
            self.creature,
            action,
            actions
        );
    }

    #[track_caller]
    pub fn assert_action_available(&self, game_state: &GameState, action: impl Into<ActionId>) {
        let action: ActionId = action.into();
        let actions = systems::actions::available_actions(game_state, self.creature.id());
        assert!(
            actions.contains_key(&action),
            "Expected creature {:?} to have action {:?}, but it was not found. Available actions: {:#?}",
            self.creature,
            action,
            actions
        );
    }

    #[track_caller]
    pub fn assert_action_unavailable(&self, game_state: &GameState, action: impl Into<ActionId>) {
        let action: ActionId = action.into();
        let actions = systems::actions::available_actions(game_state, self.creature.id());
        assert!(
            !actions.contains_key(&action),
            "Expected creature {:?} to not have action {:?}, but it was found. Available actions: {:#?}",
            self.creature,
            action,
            actions
        );
    }

    pub fn resource(&self, game_state: &GameState, resource: impl Into<ResourceId>) -> u8 {
        let resource = resource.into();
        let resources =
            systems::helpers::get_component::<ResourceMap>(&game_state.world, self.creature.id());
        let Some(amount) = resources.get(&resource) else {
            panic!(
                "Expected creature {:?} to have resource {:?}, but it was not found. Current resources: {:#?}",
                self.creature, resource, resources
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
    pub fn assert_resource(
        &self,
        game_state: &GameState,
        resource: impl Into<ResourceId>,
        operator: Operator<u8>,
    ) {
        let resource = resource.into();
        let resources =
            systems::helpers::get_component::<ResourceMap>(&game_state.world, self.creature.id());
        let Some(amount) = resources.get(&resource) else {
            panic!(
                "Expected creature {:?} to have resource {:?}, but it was not found. Current resources: {:#?}",
                self.creature, resource, resources
            );
        };

        // TODO: Handle tier resource amounts (if needed?)
        let ResourceBudgetKind::Flat(budget) = amount else {
            panic!(
                "Expected resource {:?} to have a flat budget, but it was {:?}",
                resource, amount
            );
        };

        assert!(
            operator.evaluate(&budget.current_uses),
            "Resource {:?} expected to satisfy condition {:?} but was {:?}",
            resource,
            operator,
            budget
        );
    }

    #[track_caller]
    pub fn resource_tiered(
        &self,
        game_state: &GameState,
        resource: impl Into<ResourceId>,
        tier: u8,
    ) -> u8 {
        let resource = resource.into();
        let resources =
            systems::helpers::get_component::<ResourceMap>(&game_state.world, self.creature.id());
        let Some(amount) = resources.get(&resource) else {
            panic!(
                "Expected creature {:?} to have resource {:?}, but it was not found. Current resources: {:#?}",
                self.creature, resource, resources
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

    #[track_caller]
    pub fn assert_resource_tiered(
        &self,
        game_state: &GameState,
        resource: impl Into<ResourceId>,
        tier: u8,
        operator: Operator<u8>,
    ) {
        let resource = resource.into();
        let current_uses = self.resource_tiered(game_state, resource.clone(), tier);
        assert!(
            operator.evaluate(&current_uses),
            "Resource {:?} tier {} expected to satisfy condition {:?} but was {:?}",
            resource,
            tier,
            operator,
            current_uses
        );
    }

    #[track_caller]
    pub fn assert_no_resource(&self, game_state: &GameState, resource: impl Into<ResourceId>) {
        let resource = resource.into();
        let resources =
            systems::helpers::get_component::<ResourceMap>(&game_state.world, self.creature.id());
        assert!(
            resources.get(&resource).is_none(),
            "Expected creature {:?} to not have resource {:?}, but it was found. Current resources: {:#?}",
            self.creature,
            resource,
            resources
        );
    }

    #[track_caller]
    pub fn assert_effect(&self, game_state: &GameState, effect_id: impl Into<EffectId>) {
        let effect_id = effect_id.into();
        let effects = systems::effects::effects(&game_state.world, self.creature.id());
        assert!(
            effects
                .iter()
                .any(|(_, instance)| instance.effect_id == effect_id),
            "Expected creature {:?} to have effect {:?}, but it was not found. Current effects: {:#?}",
            self.creature,
            effect_id,
            effects
        );
    }

    #[track_caller]
    pub fn assert_no_effect(&self, game_state: &GameState, effect_id: impl Into<EffectId>) {
        let effect_id = effect_id.into();
        let effects = systems::effects::effects(&game_state.world, self.creature.id());
        assert!(
            !effects
                .iter()
                .any(|(_, instance)| instance.effect_id == effect_id),
            "Expected creature {:?} to not have effect {:?}, but it was found. Current effects: {:#?}",
            self.creature,
            effect_id,
            effects
        );
    }

    #[track_caller]
    pub fn assert_on_cooldown(&self, game_state: &GameState, action_id: impl Into<ActionId>) {
        let action_id = action_id.into();
        let cooldown =
            systems::actions::on_cooldown(&game_state.world, self.creature.id(), &action_id);
        assert!(
            cooldown.is_some(),
            "Expected creature {:?} to have action {:?} on cooldown, but it was not found",
            self.creature,
            action_id,
        );
    }

    #[track_caller]
    pub fn assert_hp(&self, game_state: &GameState, operator: Operator<u32>) {
        let hit_points =
            systems::helpers::get_component::<HitPoints>(&game_state.world, self.creature.id());
        assert!(
            operator.evaluate(&hit_points.current()),
            "Expected creature {:?} to have HP satisfying condition {:?}, but it was {:?}",
            self.creature,
            operator,
            hit_points.current()
        );
    }

    #[track_caller]
    pub fn assert_movement_speed(&self, game_state: &GameState, operator: Operator<Length>) {
        let speed = systems::helpers::get_component::<Speed>(&game_state.world, self.creature.id());
        assert!(
            operator.evaluate(&speed.total_speed()),
            "Expected creature {:?} to have movement speed satisfying condition {:?}, but it was {:?}",
            self.creature,
            operator,
            speed.total_speed()
        );
    }

    #[track_caller]
    pub fn assert_free_movement(
        &self,
        game_state: &GameState,
        source: ModifierSource,
        operator: Operator<f32>,
    ) {
        let speed = systems::helpers::get_component::<Speed>(&game_state.world, self.creature.id());
        assert!(
            speed
                .free_movement_multipliers()
                .get(&source)
                .map(|v| operator.evaluate(v))
                .unwrap_or(false),
            "Expected creature {:?} to have free multiplier movement from {:?} satisfying condition {:?}, but it has: {:#?}",
            self.creature,
            source,
            operator,
            speed.free_movement_multipliers()
        );
    }

    #[track_caller]
    pub fn assert_d20_advantage(
        &self,
        game_state: &GameState,
        kind: &D20CheckKind,
        source: &ModifierSource,
        advantage_type: AdvantageType,
    ) {
        self.assert_d20(game_state, kind, {
            let kind = kind.clone();
            let source = source.clone();
            let advantage_type = advantage_type.clone();
            move |check| {
            assert!(
                check.advantage_tracker().summary().contains(&(&source, advantage_type)),
                "Expected creature {:?} to have {:?} {:?} on {:?} check, but it was not found. Current advantages: {:#?}",
                self.creature,
                advantage_type,
                source,
                kind,
                check.advantage_tracker().summary()
            );
        }});
    }

    #[track_caller]
    pub fn assert_d20_crit_threshold_reduction(
        &self,
        game_state: &GameState,
        kind: &D20CheckKind,
        source: &ModifierSource,
        operator: Operator<i32>,
    ) {
        self.assert_d20(game_state, kind, {
            let kind = kind.clone();
            let source = source.clone();
            move |check| {
                let reduction = check.crit_threshold_reduction().get(&source);
                assert!(
                    reduction
                        .map(|r| operator.evaluate(r))
                        .unwrap_or(false),
                    "Expected creature {:?} to have critical threshold reduction from {:?} on {:?} check satisfying condition {:?}, but it was {:?}. Current reductions: {:#?}",
                    self.creature,
                    source,
                    kind,
                    operator,
                    reduction,
                    check.crit_threshold_reduction()
                );
            }
        });
    }

    fn assert_d20(
        &self,
        game_state: &GameState,
        kind: &D20CheckKind,
        assertor: impl Fn(&D20Check),
    ) {
        match kind {
            D20CheckKind::SavingThrow(saving_throw_kind) => {
                assertor(
                    systems::helpers::get_component::<SavingThrowSet>(
                        &game_state.world,
                        self.creature.id(),
                    )
                    .get(saving_throw_kind),
                );
            }

            D20CheckKind::Skill(skill) => {
                assertor(
                    systems::helpers::get_component::<SkillSet>(
                        &game_state.world,
                        self.creature.id(),
                    )
                    .get(skill),
                );
            }

            D20CheckKind::AttackRoll(attack_source) => match attack_source {
                AttackSource::Weapon(weapon_kind) => {
                    assertor(
                        &systems::helpers::get_component::<Loadout>(
                            &game_state.world,
                            self.creature.id(),
                        )
                        .attack_roll_template(weapon_kind)
                        .d20_check,
                    );
                }

                AttackSource::Spell => {
                    assertor(
                        &systems::helpers::get_component::<Spellbook>(
                            &game_state.world,
                            self.creature.id(),
                        )
                        .attack_roll_template()
                        .d20_check,
                    );
                }
            },
        }
    }

    // TODO: Update this is we add support for concentration on things other than effects
    pub fn assert_concentration(&self, game_state: &GameState, effect_id: impl Into<EffectId>) {
        let effect_id = effect_id.into();
        let spellbook =
            systems::helpers::get_component::<Spellbook>(&game_state.world, self.creature.id());
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
            self.creature,
            effect_id,
            spellbook.concentration_tracker().instances()
        );
    }

    pub fn assert_no_concentration(&self, game_state: &GameState) {
        let spellbook =
            systems::helpers::get_component::<Spellbook>(&game_state.world, self.creature.id());
        assert!(
            spellbook.concentration_tracker().instances().is_empty(),
            "Expected creature {:?} to not be concentrating, but it has concentration instances: {:#?}",
            self.creature,
            spellbook.concentration_tracker().instances()
        );
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
