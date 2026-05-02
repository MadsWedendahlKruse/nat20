use crate::{
    components::{
        actions::action_builder::ActionBuilder,
        health::hit_points::HitPoints,
        id::{ActionId, EffectId, EntityIdentifier, ResourceId},
        resource::{ResourceBudgetKind, ResourceMap},
        time::{TimeStep, TurnBoundary},
    },
    engine::game_state::GameState,
    systems,
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
        ActionBuilder::available(&game_state.world, self.creature.id())
            .action(&game_state.world, &action.into())
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

    pub fn damage_raw(&mut self, game_state: &mut GameState, amount: u32) {
        // TODO: This technically bypasses the normal damage pipeline, which is
        // quite complex, but I guess it's fine for testing?
        systems::helpers::get_component_mut::<HitPoints>(&mut game_state.world, self.creature.id())
            .damage(amount);
    }

    #[track_caller]
    pub fn assert_has_action(&self, game_state: &GameState, action: impl Into<ActionId>) {
        let action: ActionId = action.into();
        let actions = systems::actions::available_actions(&game_state.world, self.creature.id());
        assert!(
            actions.contains_key(&action),
            "Expected creature {:?} to have action {:?}, but it was not found. Available actions: {:#?}",
            self.creature,
            action,
            actions
        );
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
