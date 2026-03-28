use std::collections::HashSet;

use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::{ActionContext, ActionResult},
        damage::{
            AttackRoll, AttackRollResult, DamageMitigationResult, DamageRoll, DamageRollResult,
        },
        effects::effect::{Effect, EffectInstance, EffectInstanceId, EffectsMap},
        id::ActionId,
        items::equipment::armor::ArmorClass,
        resource::ResourceAmountMap,
        time::TimeStep,
    },
    engine::{action_prompt::ActionData, game_state::GameState},
};

#[derive(Debug, Clone, Default)]
pub struct EffectManager {
    pub effects: EffectsMap,
    pub marked_for_removal: HashSet<EffectInstanceId>,
}

impl EffectManager {
    pub fn new() -> Self {
        Self {
            effects: EffectsMap::new(),
            marked_for_removal: HashSet::new(),
        }
    }

    pub fn get(&self, instance_id: &EffectInstanceId) -> Option<&EffectInstance> {
        self.effects.get(instance_id)
    }

    pub fn insert(&mut self, instance: EffectInstance) {
        self.effects.insert(instance.instance_id, instance);
    }

    pub fn remove(&mut self, instance_id: &EffectInstanceId) -> Option<EffectInstance> {
        let removed = self.effects.remove(instance_id);
        self.marked_for_removal.remove(instance_id);
        removed
    }

    pub fn iter(&self) -> impl Iterator<Item = (&EffectInstanceId, &EffectInstance)> {
        self.effects.iter()
    }

    pub fn values(&self) -> impl Iterator<Item = &EffectInstance> {
        self.effects.values()
    }

    pub fn advance_time(&mut self, time_step: TimeStep) {
        for instance in self.effects.values_mut() {
            instance.advance_time(time_step);
            if instance.is_expired() {
                self.marked_for_removal.insert(instance.instance_id);
            }
        }
    }

    pub fn take_marked_for_removal(&mut self) -> HashSet<EffectInstanceId> {
        std::mem::take(&mut self.marked_for_removal)
    }

    fn for_each<H>(&self, get_hook: impl Fn(&Effect) -> Option<&H>, mut f: impl FnMut(&H)) {
        for instance in self.effects.values() {
            if let Some(hook) = get_hook(instance.effect()) {
                f(hook);
            }
        }
    }

    fn for_each_one_shot<H>(
        &mut self,
        get_hook: impl Fn(&Effect) -> Option<&H>,
        mut f: impl FnMut(&H),
    ) {
        for instance in self.effects.values() {
            if let Some(hook) = get_hook(instance.effect()) {
                if self.marked_for_removal.contains(&instance.instance_id) {
                    continue;
                }
                f(hook);
                if instance.one_shot {
                    self.marked_for_removal.insert(instance.instance_id);
                }
            }
        }
    }

    fn for_each_with_instance<H>(
        &self,
        get_hook: impl Fn(&Effect) -> Option<&H>,
        mut f: impl FnMut(&H, &EffectInstance),
    ) {
        for instance in self.effects.values() {
            if let Some(hook) = get_hook(instance.effect()) {
                f(hook, instance);
            }
        }
    }

    fn for_each_one_shot_with_instance<H>(
        &mut self,
        get_hook: impl Fn(&Effect) -> Option<&H>,
        mut f: impl FnMut(&H, &EffectInstance),
    ) {
        for instance in self.effects.values() {
            if let Some(hook) = get_hook(instance.effect()) {
                if self.marked_for_removal.contains(&instance.instance_id) {
                    continue;
                }
                f(hook, instance);
                if instance.one_shot {
                    self.marked_for_removal.insert(instance.instance_id);
                }
            }
        }
    }

    pub fn apply(&self, state: &mut GameState, entity: Entity, ctx: Option<&ActionContext>) {
        self.for_each(
            |effect| effect.on_apply.as_ref(),
            |hook| hook(state, entity, ctx),
        );
    }

    pub fn unapply(&self, state: &mut GameState, entity: Entity) {
        self.for_each(
            |effect| effect.on_unapply.as_ref(),
            |hook| hook(state, entity),
        );
    }

    pub fn pre_attack_roll(&self, world: &World, entity: Entity, roll: &mut AttackRoll) {
        self.for_each(
            |effect| effect.pre_attack_roll.as_ref(),
            |hook| hook(world, entity, roll),
        );
    }

    pub fn post_attack_roll(&self, world: &World, entity: Entity, result: &mut AttackRollResult) {
        self.for_each(
            |effect| effect.post_attack_roll.as_ref(),
            |hook| hook(world, entity, result),
        );
    }

    pub fn armor_class(&self, world: &World, entity: Entity, ac: &mut ArmorClass) {
        self.for_each(
            |effect| effect.on_armor_class.as_ref(),
            |hook| hook(world, entity, ac),
        );
    }

    pub fn pre_damage_roll(&self, world: &World, entity: Entity, roll: &mut DamageRoll) {
        self.for_each(
            |effect| effect.pre_damage_roll.as_ref(),
            |hook| hook(world, entity, roll),
        );
    }

    pub fn post_damage_roll(&self, world: &World, entity: Entity, result: &mut DamageRollResult) {
        self.for_each(
            |effect| effect.post_damage_roll.as_ref(),
            |hook| hook(world, entity, result),
        );
    }

    pub fn action(&self, world: &mut World, data: &ActionData) {
        self.for_each(|effect| effect.on_action.as_ref(), |hook| hook(world, data));
    }

    pub fn action_result(
        &self,
        state: &mut GameState,
        data: &ActionData,
        results: &[ActionResult],
    ) {
        self.for_each(
            |effect| effect.on_action_result.as_ref(),
            |hook| hook(state, data, results),
        );
    }

    pub fn resource_cost(
        &self,
        world: &World,
        entity: Entity,
        id: &ActionId,
        ctx: &ActionContext,
        costs: &mut ResourceAmountMap,
    ) {
        self.for_each(
            |effect| effect.on_resource_cost.as_ref(),
            |hook| hook(world, entity, id, ctx, costs),
        );
    }

    pub fn post_damage_mitigation(
        &self,
        world: &World,
        entity: Entity,
        result: &mut DamageMitigationResult,
    ) {
        self.for_each(
            |effect| effect.post_damage_mitigation.as_ref(),
            |hook| hook(world, entity, result),
        );
    }

    pub fn death(&self, world: &mut World, victim: Entity, killer: Option<Entity>) {
        self.for_each_with_instance(
            |effect| effect.on_death.as_ref(),
            |hook, effect| hook(world, victim, killer, effect.applier),
        );
    }

    pub fn pre_damage_mitigation(
        &self,
        world: &World,
        entity: Entity,
        result: &mut DamageRollResult,
    ) {
        self.for_each_with_instance(
            |effect| effect.pre_damage_mitigation.as_ref(),
            |hook, inst| hook(world, inst, entity, result),
        );
    }

    pub fn attacked(
        &mut self,
        world: &World,
        victim: Entity,
        attacker: Entity,
        roll: &mut AttackRoll,
    ) {
        self.for_each_one_shot_with_instance(
            |effect| effect.on_attacked.as_ref(),
            |hook, inst| hook(world, inst, victim, attacker, roll),
        );
    }

    // Similar to `attacked`, but doesn't consume one-shot effects, allowing it to
    // be used for previews (e.g. showing the player that they would have advantage
    // on an attack before they commit to it).
    // TODO: Consider a more robust solution for this
    pub fn attacked_preview(
        &self,
        world: &World,
        victim: Entity,
        attacker: Entity,
        roll: &mut AttackRoll,
    ) {
        self.for_each_with_instance(
            |effect| effect.on_attacked.as_ref(),
            |hook, inst| hook(world, inst, victim, attacker, roll),
        );
    }
}
