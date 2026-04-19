use std::collections::HashSet;

use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::ActionContext,
        damage::{
            AttackRoll, AttackRollResult, DamageMitigationResult, DamageRoll, DamageRollResult,
        },
        effects::effect::{Effect, EffectInstance, EffectInstanceId, EffectsMap},
        id::ActionId,
        items::equipment::armor::ArmorClass,
        resource::ResourceAmountMap,
        time::TimeStep,
    },
    engine::game_state::GameState,
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

    /// Collect clones of all hook `Arc`s matching `get_hook`. Callers can then
    /// drop the shared borrow on the world and invoke the hooks with `&mut GameState`
    /// or `&mut World` without hitting a double-borrow. Any effects added by a hook
    /// go directly into the world and are never overwritten.
    pub fn collect_hooks<H: Clone>(
        &self,
        get_hook: impl Fn(&Effect) -> Option<&H>,
    ) -> Vec<H> {
        self.effects
            .values()
            .filter_map(|inst| get_hook(inst.effect()).cloned())
            .collect()
    }

    /// Like `collect_hooks`, but for one-shot hooks that also need the `EffectInstance`
    /// (e.g. `on_attacked`). Returns `(hook, instance)` pairs. One-shot instances are
    /// marked for removal immediately as part of collection — consuming them is the act
    /// of collecting them.
    pub fn collect_one_shot_hooks_with_instance<H: Clone>(
        &mut self,
        get_hook: impl Fn(&Effect) -> Option<&H>,
    ) -> Vec<(H, EffectInstance)> {
        let mut result = Vec::new();
        for inst in self.effects.values() {
            if self.marked_for_removal.contains(&inst.instance_id) {
                continue;
            }
            if let Some(hook) = get_hook(inst.effect()).cloned() {
                if inst.one_shot {
                    self.marked_for_removal.insert(inst.instance_id);
                }
                result.push((hook, inst.clone()));
            }
        }
        result
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
