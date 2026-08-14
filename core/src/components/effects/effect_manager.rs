use std::collections::HashSet;

use hecs::{Entity, World};

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::action::{ActionConditionResolution, ActionContext},
        d20::{D20Check, D20CheckKind, D20CheckResult},
        damage::{DamageMitigationResult, DamageRoll, DamageRollResult},
        effects::{
            effect::{Effect, EffectInstance, EffectInstanceId, EffectsMap},
            hooks::D20CheckHooks,
        },
        id::ActionId,
        items::equipment::armor::ArmorClass,
        modifier::{Modifiable, ModifierSource},
        resource::ResourceAmountMap,
        speed::Speed,
        time::TimeStep,
    },
    engine::{action_prompt::ActionData, game_state::GameState},
    systems,
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
        let removed = self.effects.shift_remove(instance_id);
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

    /// Collect clones of all hook `Arc`s matching `get_hook`. Callers can then
    /// drop the shared borrow on the world and invoke the hooks with `&mut GameState`
    /// or `&mut World` without hitting a double-borrow. Any effects added by a hook
    /// go directly into the world and are never overwritten.
    pub fn collect_hooks<H: Clone>(&self, get_hook: impl Fn(&Effect) -> Option<&H>) -> Vec<H> {
        self.effects
            .values()
            .filter_map(|inst| get_hook(inst.effect()).cloned())
            .collect()
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

    pub fn armor_class(&self, game_state: &GameState, entity: Entity, ac: &mut ArmorClass) {
        self.for_each(
            |effect| effect.on_armor_class.as_ref(),
            |hook| hook(game_state, entity, ac),
        );
    }

    pub fn speed(&self, game_state: &GameState, entity: Entity, speed: &mut Speed) {
        self.for_each(
            |effect| effect.on_speed.as_ref(),
            |hook| hook(game_state, entity, speed),
        );
    }

    // TODO: Could be argued that this has perhaps a bit too much logic to live in
    // the EffectManager, but it makes it a lot easier to apply everything correctly
    pub fn pre_d20_check(&self, game_state: &GameState, entity: Entity, check: &mut D20Check) {
        let kind = check.kind().clone();

        for instance in self.effects.values() {
            if let Some(hook) = Self::get_d20_hooks(&kind)(instance.effect()) {
                if let Some(ability) = (hook.ability_hook)(game_state, entity, check) {
                    check.set_ability(Some(ability));
                }
            }
        }

        if let Some(ability) = check.ability() {
            let ability_scores =
                systems::helpers::get_component::<AbilityScoreMap>(&game_state.world, entity);
            check.replace_modifier(
                ModifierSource::Ability(ability),
                ability_scores.ability_modifier(&ability).total(),
            );
        }

        self.for_each(Self::get_d20_hooks(&kind), |hook| {
            (hook.check_hook)(game_state, entity, check)
        });
    }

    pub fn post_d20_check(
        &self,
        game_state: &GameState,
        entity: Entity,
        result: &mut D20CheckResult,
    ) {
        self.for_each(Self::get_d20_hooks(&result.check.kind().clone()), |hook| {
            (hook.result_hook)(game_state, entity, result)
        });
    }

    fn get_d20_hooks(kind: &D20CheckKind) -> impl Fn(&Effect) -> Option<&D20CheckHooks> {
        move |effect| match kind {
            D20CheckKind::SavingThrow(saving_throw_kind) => {
                effect.on_saving_throw.get(saving_throw_kind)
            }
            D20CheckKind::Skill(skill) => effect.on_skill_check.get(skill),
            D20CheckKind::AttackRoll(attack_source) => effect.on_attack_roll.get(attack_source),
        }
    }

    pub fn pre_damage_roll(
        &self,
        game_state: &GameState,
        entity: Entity,
        roll: &mut DamageRoll,
        action: &ActionData,
        resolution: &ActionConditionResolution,
    ) {
        self.for_each(
            |effect| effect.pre_damage_roll.as_ref(),
            |hook| hook(game_state, entity, roll, action, resolution),
        );
    }

    pub fn post_damage_roll(
        &self,
        game_state: &GameState,
        entity: Entity,
        result: &mut DamageRollResult,
        action: &ActionData,
        resolution: &ActionConditionResolution,
    ) {
        self.for_each(
            |effect| effect.post_damage_roll.as_ref(),
            |hook| hook(game_state, entity, result, action, resolution),
        );
    }

    pub fn resource_cost(
        &self,
        game_state: &GameState,
        entity: Entity,
        id: &ActionId,
        ctx: &ActionContext,
        costs: &mut ResourceAmountMap,
    ) {
        self.for_each(
            |effect| effect.on_resource_cost.as_ref(),
            |hook| hook(game_state, entity, id, ctx, costs),
        );
    }

    pub fn action_usability(
        &self,
        game_state: &GameState,
        entity: Entity,
        id: &ActionId,
        ctx: &ActionContext,
    ) -> Option<String> {
        self.effects
            .values()
            .filter_map(|instance| instance.effect().on_action_usability.as_ref())
            .find_map(|hook| hook(game_state, entity, id, ctx))
    }

    pub fn pre_damage_mitigation(
        &self,
        game_state: &GameState,
        entity: Entity,
        result: &mut DamageRollResult,
        action: Option<&ActionData>,
        resolution: Option<&ActionConditionResolution>,
    ) {
        self.for_each_with_instance(
            |effect| effect.pre_damage_mitigation.as_ref(),
            |hook, inst| hook(game_state, inst, entity, result, action, resolution),
        );
    }

    pub fn post_damage_mitigation(
        &self,
        game_state: &GameState,
        entity: Entity,
        result: &mut DamageMitigationResult,
        action: Option<&ActionData>,
        resolution: Option<&ActionConditionResolution>,
    ) {
        self.for_each(
            |effect| effect.post_damage_mitigation.as_ref(),
            |hook| hook(game_state, entity, result, action, resolution),
        );
    }

    pub fn on_attacked(
        &self,
        game_state: &GameState,
        victim: Entity,
        attacker: Entity,
        check: &mut D20Check,
    ) {
        self.for_each_with_instance(
            |effect| effect.on_attacked.as_ref(),
            |hook, inst| hook(game_state, inst, victim, attacker, check),
        );
    }
}
