use std::{fmt, sync::Arc};

use hecs::Entity;

use crate::{
    components::{
        ability::Ability,
        actions::action::{ActionConditionResolution, ActionContext, ActionResult},
        d20::{D20Check, D20CheckResult},
        damage::{DamageMitigationResult, DamageRoll, DamageRollResult},
        effects::effect::EffectInstance,
        id::ActionId,
        items::equipment::armor::ArmorClass,
        resource::ResourceAmountMap,
        speed::Speed,
    },
    engine::{action_prompt::ActionData, game_state::GameState},
};

pub type ApplyEffectHook =
    Arc<dyn Fn(&mut GameState, Entity, Option<&ActionContext>) + Send + Sync>;
pub type UnapplyEffectHook = Arc<dyn Fn(&mut GameState, Entity) + Send + Sync>;
/// Hook for when an entity is attacked. Parameters are: world, effect instance,
/// victim, attacker, the attacker's (not yet rolled) check.
pub type AttackedHook =
    Arc<dyn Fn(&GameState, &EffectInstance, Entity, Entity, &mut D20Check) + Send + Sync>;
pub type ArmorClassHook = Arc<dyn Fn(&GameState, Entity, &mut ArmorClass) + Send + Sync>;
pub type SpeedHook = Arc<dyn Fn(&GameState, Entity, &mut Speed) + Send + Sync>;
pub type D20AbilityHook =
    Arc<dyn Fn(&GameState, Entity, &D20Check) -> Option<Ability> + Send + Sync>;
pub type D20CheckHook = Arc<dyn Fn(&GameState, Entity, &mut D20Check) + Send + Sync>;
pub type D20CheckResultHook = Arc<dyn Fn(&GameState, Entity, &mut D20CheckResult) + Send + Sync>;
pub type DamageRollHook = Arc<
    dyn Fn(&GameState, Entity, &mut DamageRoll, &ActionData, &ActionConditionResolution)
        + Send
        + Sync,
>;
pub type DamageRollResultHook = Arc<
    dyn Fn(&GameState, Entity, &mut DamageRollResult, &ActionData, &ActionConditionResolution)
        + Send
        + Sync,
>;
pub type ActionHook = Arc<dyn Fn(&mut GameState, &ActionData) + Send + Sync>;
pub type ActionResultHook = Arc<dyn Fn(&mut GameState, &ActionData, &ActionResult) + Send + Sync>;
pub type ResourceCostHook = Arc<
    dyn Fn(&GameState, Entity, &ActionId, &ActionContext, &mut ResourceAmountMap) + Send + Sync,
>;
/// Hook for effects which block certain actions, returns block reason if any
pub type ActionUsabilityHook =
    Arc<dyn Fn(&GameState, Entity, &ActionId, &ActionContext) -> Option<String> + Send + Sync>;
pub type PreDamageMitigationHook = Arc<
    dyn Fn(
            &GameState,
            &EffectInstance,
            Entity,
            &mut DamageRollResult,
            Option<&ActionData>,
            Option<&ActionConditionResolution>,
        ) + Send
        + Sync,
>;
pub type PostDamageMitigationHook = Arc<
    dyn Fn(
            &GameState,
            Entity,
            &mut DamageMitigationResult,
            Option<&ActionData>,
            Option<&ActionConditionResolution>,
        ) + Send
        + Sync,
>;
// Entitys in order: 1. victim, 2. killer (if any), 3. effect applier (if any)
pub type DeathHook =
    Arc<dyn Fn(&mut GameState, Entity, Option<Entity>, Option<Entity>) + Send + Sync>;
pub type TurnStartHook = Arc<dyn Fn(&mut GameState, Entity) + Send + Sync>;

#[derive(Clone)]
pub struct D20CheckHooks {
    pub ability_hook: D20AbilityHook,
    pub check_hook: D20CheckHook,
    pub result_hook: D20CheckResultHook,
}

impl fmt::Debug for D20CheckHooks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("D20CheckHookPair")
            .field("ability_hook", &"<Fn>")
            .field("check_hook", &"<Fn>")
            .field("result_hook", &"<Fn>")
            .finish()
    }
}

impl D20CheckHooks {
    pub fn combined(hooks: Vec<D20CheckHooks>) -> D20CheckHooks {
        let check_hooks: Vec<D20CheckHook> = hooks.iter().map(|h| h.check_hook.clone()).collect();
        let result_hooks: Vec<D20CheckResultHook> =
            hooks.iter().map(|h| h.result_hook.clone()).collect();
        Self {
            ability_hook: Arc::new(move |game_state, entity, check| {
                // TODO: Not really sure what to do here, but on the other hand I
                // don't think we'll ever have more than one ability hook on the
                // same effect, so this is probably fine
                for hook in &hooks {
                    if let Some(ability) = (hook.ability_hook)(game_state, entity, check) {
                        return Some(ability);
                    }
                }
                None
            }),
            check_hook: Arc::new(move |game_state, entity, check| {
                for hook in &check_hooks {
                    hook(game_state, entity, check);
                }
            }),
            result_hook: Arc::new(move |game_state, entity, result| {
                for hook in &result_hooks {
                    hook(game_state, entity, result);
                }
            }),
        }
    }
}
