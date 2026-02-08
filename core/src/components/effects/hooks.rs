use std::{fmt, sync::Arc};

use hecs::{Entity, World};

use crate::{
    components::{
        actions::action::ActionContext,
        d20::{D20Check, D20CheckResult},
        damage::{
            AttackRoll, AttackRollResult, DamageMitigationResult, DamageRoll, DamageRollResult,
        },
        effects::effect::EffectInstance,
        id::ActionId,
        items::equipment::armor::ArmorClass,
        resource::ResourceAmountMap,
    },
    engine::action_prompt::ActionData,
};

pub type ApplyEffectHook = Arc<dyn Fn(&mut World, Entity, Option<&ActionContext>) + Send + Sync>;
pub type UnapplyEffectHook = Arc<dyn Fn(&mut World, Entity) + Send + Sync>;
pub type AttackRollHook = Arc<dyn Fn(&World, Entity, &mut AttackRoll) + Send + Sync>;
pub type AttackRollResultHook = Arc<dyn Fn(&World, Entity, &mut AttackRollResult) + Send + Sync>;
/// Hook for when an entity is attacked. Parameters are: world, victim, attacker, attack roll result.
pub type AttackedHook = Arc<dyn Fn(&World, Entity, Entity, &mut AttackRoll) + Send + Sync>;
pub type ArmorClassHook = Arc<dyn Fn(&World, Entity, &mut ArmorClass) + Send + Sync>;
pub type D20CheckHook = Arc<dyn Fn(&World, Entity, &mut D20Check) + Send + Sync>;
pub type D20CheckResultHook = Arc<dyn Fn(&World, Entity, &mut D20CheckResult) + Send + Sync>;
pub type DamageRollHook = Arc<dyn Fn(&World, Entity, &mut DamageRoll) + Send + Sync>;
pub type DamageRollResultHook = Arc<dyn Fn(&World, Entity, &mut DamageRollResult) + Send + Sync>;
pub type ActionHook = Arc<dyn Fn(&mut World, &ActionData) + Send + Sync>;
pub type ResourceCostHook =
    Arc<dyn Fn(&World, Entity, &ActionId, &ActionContext, &mut ResourceAmountMap) + Send + Sync>;
pub type PreDamageMitigationHook =
    Arc<dyn Fn(&World, Entity, &EffectInstance, &mut DamageRollResult) + Send + Sync>;
pub type PostDamageMitigationHook =
    Arc<dyn Fn(&World, Entity, &mut DamageMitigationResult) + Send + Sync>;
// Entitys in order: 1. victim, 2. killer (if any), 3. effect applier (if any)
pub type DeathHook = Arc<dyn Fn(&mut World, Entity, Option<Entity>, Option<Entity>) + Send + Sync>;

#[derive(Clone)]
pub struct D20CheckHooks {
    pub check_hook: D20CheckHook,
    pub result_hook: D20CheckResultHook,
}

impl fmt::Debug for D20CheckHooks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("D20CheckHookPair")
            .field("check_hook", &"<Fn>")
            .field("result_hook", &"<Fn>")
            .finish()
    }
}

impl D20CheckHooks {
    pub fn new() -> Self {
        Self {
            check_hook: Arc::new(|_, _, _| {}),
            result_hook: Arc::new(|_, _, _| {}),
        }
    }

    pub fn with_check_hook<F>(hook: F) -> Self
    where
        F: Fn(&World, Entity, &mut D20Check) + Send + Sync + 'static,
    {
        Self {
            check_hook: Arc::new(hook),
            result_hook: Arc::new(|_, _, _| {}),
        }
    }

    pub fn with_result_hook<F>(hook: F) -> Self
    where
        F: Fn(&World, Entity, &mut D20CheckResult) + Send + Sync + 'static,
    {
        Self {
            check_hook: Arc::new(|_, _, _| {}),
            result_hook: Arc::new(hook),
        }
    }
}
