//! Lua bindings for the script-facing types.
//!
//! Domain types (`DamageRollResult`, `DamageMitigationResult`, `EffectInstance`,
//! `GameState`) implement `UserData` directly. Reaction-shaped types live in
//! [`crate::scripts::script_api`] and have their `UserData` impls here.
//!
//! Method calls in Lua use colon syntax (`obj:method(args)`); fields use dot
//! syntax (`obj.field`). Mutating methods register with `add_method_mut`.

use std::str::FromStr;

use mlua::{
    FromLua, Lua, Result as LuaResult, UserData, UserDataFields, UserDataMethods, Value, Variadic,
};

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::action::{ActionCondition, ActionConditionResolution, ActionContext, ActionKind},
        damage::{
            CRIT_DICE_MULTIPLIER, DamageComponentResult, DamageMitigationEffect,
            DamageMitigationResult, DamageRollResult, MitigationOperation,
        },
        dice::{DiceSet, DiceSetRoll},
        effects::effect::{
            EffectEntiyReference, EffectInstance, EffectInstanceTemplate, EffectLifetimeTemplate,
        },
        health::hit_points::HitPoints,
        id::{ActionId, EffectId, ResourceId},
        modifier::{Modifiable, ModifierSet, ModifierSource},
        resource::{ResourceAmount, ResourceAmountMap, ResourceMap},
        time::{TimeDuration, TurnBoundary},
    },
    engine::{action_prompt::ActionData, game_state::GameState},
    scripts::script_api::{
        ScriptActionConditionResolution, ScriptActionKindResultView, ScriptActionOutcomeBundleView,
        ScriptActionPerformedView, ScriptActionResultView, ScriptD20CheckDCKind,
        ScriptD20CheckView, ScriptD20Result, ScriptDamageOutcomeView, ScriptDiceRollBonus,
        ScriptEntity, ScriptEventRef, ScriptEventView, ScriptMovingOutOfReachView,
        ScriptReactionBodyContext, ScriptReactionBodyResult, ScriptReactionPlan,
        ScriptReactionTriggerContext, ScriptSavingThrow,
    },
    systems,
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Accept either an integer or a string for resource amounts. Scripts that pass
/// `"1"` or `1` both work.
fn amount_to_string(value: Value) -> LuaResult<String> {
    match value {
        Value::String(s) => Ok(s.to_str()?.to_string()),
        Value::Integer(i) => Ok(i.to_string()),
        Value::Number(n) => Ok(n.to_string()),
        other => Err(mlua::Error::RuntimeError(format!(
            "Expected string or integer for resource amount, got {}",
            other.type_name()
        ))),
    }
}

fn parse_resource_amount(value: Value) -> LuaResult<ResourceAmount> {
    let s = amount_to_string(value)?;
    serde_plain::from_str(&s)
        .map_err(|e| mlua::Error::RuntimeError(format!("Failed to parse ResourceAmount: {e}")))
}

fn parse_resource_id(s: &str) -> LuaResult<ResourceId> {
    s.parse()
        .map_err(|e| mlua::Error::RuntimeError(format!("Failed to parse ResourceId: {e}")))
}

fn parse_effect_id(s: &str) -> LuaResult<EffectId> {
    s.parse()
        .map_err(|e| mlua::Error::RuntimeError(format!("Failed to parse EffectId: {e}")))
}

/// mlua's blanket impls cover `IntoLua` for `UserData + Clone` but not
/// `FromLua` — when a script-facing function takes a `UserData` type as an
/// argument, we need an explicit `FromLua` impl. This macro generates one
/// that borrows the userdata and clones it out.
macro_rules! impl_from_lua_userdata {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl FromLua for $ty {
                fn from_lua(value: Value, _: &Lua) -> LuaResult<Self> {
                    match value {
                        Value::UserData(ud) => Ok(ud.borrow::<Self>()?.clone()),
                        other => Err(mlua::Error::FromLuaConversionError {
                            from: other.type_name(),
                            to: stringify!($ty).to_string(),
                            message: Some(format!(
                                "expected userdata of type {}",
                                stringify!($ty)
                            )),
                        }),
                    }
                }
            }
        )+
    };
}

impl_from_lua_userdata!(
    ScriptEntity,
    ScriptD20CheckDCKind,
    ScriptD20Result,
    ScriptD20CheckView,
    ScriptMovingOutOfReachView,
    ScriptSavingThrow,
    ScriptReactionPlan,
    ScriptReactionBodyResult,
    ActionContext,
    ActionData,
    ScriptActionConditionResolution,
    ScriptDamageOutcomeView,
    ScriptActionOutcomeBundleView,
    ScriptActionKindResultView,
    ScriptActionResultView,
    ScriptActionPerformedView,
    ScriptEventView,
    ScriptReactionTriggerContext,
    ScriptReactionBodyContext,
    DamageRollResult,
    DamageMitigationResult,
    EffectInstance,
    ModifierSet,
    ModifierSource,
    ResourceAmountMap,
);

// ---------------------------------------------------------------------------
// ScriptEntity
// ---------------------------------------------------------------------------

impl UserData for ScriptEntity {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("id", |_, this| Ok(this.id));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_meta_method(mlua::MetaMethod::Eq, |_, a, b: ScriptEntity| {
            Ok(a.id == b.id)
        });
    }
}

// ---------------------------------------------------------------------------
// DamageRollResult — domain type, exposed directly.
// ---------------------------------------------------------------------------

impl UserData for DamageRollResult {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("source", |_, this| Ok(this.source.to_string()));
        fields.add_field_method_get("actor", |_, this| {
            Ok(this.action.as_ref().map(|(e, _)| ScriptEntity::from(*e)))
        });
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_action_attack_roll", |_, this, ()| {
            Ok(is_action_condition_type(this, |c| {
                matches!(c, ActionCondition::AttackRoll { .. })
            }))
        });
        methods.add_method("is_action_saving_throw", |_, this, ()| {
            Ok(is_action_condition_type(this, |c| {
                matches!(c, ActionCondition::SavingThrow { .. })
            }))
        });
        methods.add_method("is_action_unconditional", |_, this, ()| {
            Ok(is_action_condition_type(this, |c| {
                matches!(c, ActionCondition::None)
            }))
        });

        methods.add_method_mut("clamp_damage_dice_min", |_, this, min: i64| {
            let minimum_roll = min as u32;
            for component in &mut this.components {
                for roll in &mut component.result.rolls {
                    if *roll < minimum_roll {
                        *roll = minimum_roll;
                    }
                }
                component.result.recalculate_total();
            }
            this.recalculate_total();
            Ok(())
        });

        methods.add_method_mut(
            "add_damage",
            |_, this, (amount, damage_type): (String, String)| {
                let bonus: ScriptDiceRollBonus = amount.parse().map_err(|e: String| {
                    mlua::Error::RuntimeError(format!("Invalid bonus: {e}"))
                })?;
                let damage_type = serde_plain::from_str(&damage_type)
                    .map_err(|e| mlua::Error::RuntimeError(format!("Invalid damage type: {e}")))?;

                match bonus {
                    ScriptDiceRollBonus::Flat(_int_expression) => {
                        // TODO: figure out how to add a flat bonus properly
                    }
                    ScriptDiceRollBonus::Dice(dice_expression) => {
                        let (mut num_dice, die_size, modifier) =
                            dice_expression.evaluate_without_variables().unwrap();
                        if this.crit {
                            num_dice *= CRIT_DICE_MULTIPLIER as i32;
                        }
                        let result = DiceSetRoll {
                            dice: DiceSet::from_str(format!("{}d{}", num_dice, die_size).as_str())
                                .unwrap(),
                            modifiers: ModifierSet::from(ModifierSource::Base, modifier),
                        }
                        .roll();
                        this.add_component(DamageComponentResult {
                            result,
                            damage_type,
                        });
                    }
                }
                Ok(())
            },
        );
    }
}

fn is_action_condition_type(
    damage_roll_result: &DamageRollResult,
    predicate: fn(&ActionCondition) -> bool,
) -> bool {
    let Some((_, action_id)) = &damage_roll_result.action else {
        return false;
    };
    let Some(action) = systems::actions::get_action(action_id) else {
        return false;
    };
    let ActionKind::Standard { condition, .. } = &action.kind else {
        return false;
    };
    predicate(condition)
}

// ---------------------------------------------------------------------------
// DamageMitigationResult — domain type, exposed directly.
// ---------------------------------------------------------------------------

impl UserData for DamageMitigationResult {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("source", |_, this| Ok(this.source.to_string()));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method_mut("add_immunity", |_, this, ()| {
            for component in &mut this.components {
                component.modifiers.push(DamageMitigationEffect {
                    source: ModifierSource::Custom(
                        "TODO: Figure out how to propagate the source".to_string(),
                    ),
                    operation: MitigationOperation::Immunity,
                });
            }
            this.recalculate_total();
            Ok(())
        });
    }
}

// ---------------------------------------------------------------------------
// EffectInstance — domain type, exposed directly.
// ---------------------------------------------------------------------------

impl UserData for EffectInstance {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("effect_id", |_, this| Ok(this.effect_id.to_string()));
        fields.add_field_method_get("source", |_, this| Ok(this.source.to_string()));
        fields.add_field_method_get("applier", |_, this| {
            Ok(this.applier.map(ScriptEntity::from))
        });
    }
}

// ---------------------------------------------------------------------------
// D20 check views
// ---------------------------------------------------------------------------

impl UserData for ScriptD20CheckDCKind {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("label", |_, this| Ok(this.label.clone()));
        fields.add_field_method_get("dc", |_, this| Ok(this.dc));
        fields.add_field_method_get("target", |_, this| {
            Ok(this.target.as_ref().map(|e| e.id).unwrap_or(0))
        });
    }
}

impl UserData for ScriptD20Result {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("total", |_, this| Ok(this.total));
        fields.add_field_method_get("dc_kind", |_, this| Ok(this.dc_kind.clone()));
        fields.add_field_method_get("is_success", |_, this| Ok(this.is_success));
    }
}

impl UserData for ScriptD20CheckView {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("performer", |_, this| Ok(this.performer()));
        fields.add_field_method_get("result", |_, this| Ok(this.result()));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        // `modify_*` methods take `&mut self` on the Rust side but the
        // mutation actually flows through `Arc<RwLock>` interior mutability,
        // so we can register them as `add_method` (shared borrow). This lets
        // scripts call them on a userdata that was returned by a field getter
        // (which always returns an owned clone — `add_method_mut` would fail
        // there because mlua treats the borrow as exclusive).
        methods.add_method("modify_result", |_, this, bonus: String| {
            let parsed = bonus
                .parse()
                .map_err(|e: String| mlua::Error::RuntimeError(format!("Invalid bonus: {e}")))?;
            this.clone().modify_result(parsed);
            Ok(())
        });
        methods.add_method("modify_dc", |_, this, modifier: String| {
            let parsed = modifier
                .parse()
                .map_err(|e: String| mlua::Error::RuntimeError(format!("Invalid modifier: {e}")))?;
            this.clone().modify_dc(parsed);
            Ok(())
        });
        methods.add_method(
            "reroll_result",
            |_, this, (bonus, force_use_new): (String, bool)| {
                let parsed = if bonus.is_empty() {
                    None
                } else {
                    Some(bonus.parse().map_err(|e: String| {
                        mlua::Error::RuntimeError(format!("Invalid bonus: {e}"))
                    })?)
                };
                this.clone().reroll_result(parsed, force_use_new);
                Ok(())
            },
        );
    }
}

impl UserData for ScriptMovingOutOfReachView {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("mover", |_, this| Ok(this.inner.read().unwrap().mover.id));
        fields.add_field_method_get("entity", |_, this| Ok(this.inner.read().unwrap().entity.id));
        fields.add_field_method_get("continue_movement", |_, this| {
            Ok(this.inner.read().unwrap().continue_movement)
        });
        fields.add_field_method_set("continue_movement", |_, this, v: bool| {
            this.inner.write().unwrap().continue_movement = v;
            Ok(())
        });
    }
}

// ---------------------------------------------------------------------------
// Opaque marker UserData impls (no methods; used as opaque values)
// ---------------------------------------------------------------------------

impl UserData for ScriptSavingThrow {}
impl UserData for ScriptReactionPlan {}
impl UserData for ScriptReactionBodyResult {}
impl UserData for ModifierSource {}

// ---------------------------------------------------------------------------
// ModifierSet
// ---------------------------------------------------------------------------

impl UserData for ModifierSet {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method_mut(
            "add_modifier",
            |_, this, (source, value): (ModifierSource, i64)| {
                this.add_modifier(source, value as i32);
                Ok(())
            },
        );
    }
}

// ---------------------------------------------------------------------------
// Action context / view
// ---------------------------------------------------------------------------

impl UserData for ActionContext {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_spell", |_, this, ()| Ok(this.is_spell()));
        methods.add_method(
            "is_attack_action",
            |_, this, ()| Ok(this.is_attack_action()),
        );
        methods.add_method(
            "is_weapon_attack",
            |_, this, ()| Ok(this.is_weapon_attack()),
        );
        methods.add_method("is_unarmed_attack", |_, this, ()| {
            Ok(this.is_unarmed_attack())
        });
        methods.add_method("is_melee_attack", |_, this, ()| Ok(this.is_melee_attack()));
        methods.add_method(
            "is_ranged_attack",
            |_, this, ()| Ok(this.is_ranged_attack()),
        );
    }
}

impl UserData for ActionData {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("action_id", |_, this| Ok(this.action_id.to_string()));
        fields.add_field_method_get("actor", |_, this| Ok(ScriptEntity::from(this.actor.id())));
        fields.add_field_method_get("action_context", |_, this| Ok(this.context.clone()));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("costs_resource", |_, this, resource_id: String| {
            let id = parse_resource_id(&resource_id)?;
            Ok(this.resource_cost.map.contains_key(&id))
        });
    }
}

impl UserData for ScriptActionConditionResolution {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method(
            "is_unconditional",
            |_, this, ()| Ok(this.is_unconditional()),
        );
        methods.add_method("is_attack_roll", |_, this, ()| Ok(this.is_attack_roll()));
        methods.add_method("is_saving_throw", |_, this, ()| Ok(this.is_saving_throw()));
        methods.add_method("is_attack_roll_hit", |_, this, ()| {
            Ok(this.is_attack_roll_hit())
        });
        methods.add_method("is_attack_roll_critical_hit", |_, this, ()| {
            Ok(this.is_attack_roll_critical_hit())
        });
        methods.add_method("is_saving_throw_success", |_, this, ()| {
            Ok(this.is_saving_throw_success())
        });
    }
}

impl UserData for ScriptDamageOutcomeView {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("has_damage_roll", |_, this, ()| Ok(this.has_damage_roll()));
        methods.add_method("get_damage_roll", |_, this, ()| {
            Ok(this.get_damage_roll().clone())
        });
        methods.add_method(
            "has_damage_taken",
            |_, this, ()| Ok(this.has_damage_taken()),
        );
        methods.add_method("get_damage_taken", |_, this, ()| {
            Ok(this.get_damage_taken().clone())
        });
        methods.add_method("damage_roll_total", |_, this, ()| {
            Ok(this.damage_roll_total())
        });
        methods.add_method("damage_taken_total", |_, this, ()| {
            Ok(this.damage_taken_total())
        });
    }
}

impl UserData for ScriptActionOutcomeBundleView {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("has_damage", |_, this, ()| Ok(this.has_damage()));
        methods.add_method("get_damage", |_, this, ()| Ok(this.get_damage().clone()));
        methods.add_method("has_attack_hit", |_, this, ()| Ok(this.has_attack_hit()));
        methods.add_method("has_attack_critical_hit", |_, this, ()| {
            Ok(this.has_attack_critical_hit())
        });
        methods.add_method("has_attack_miss", |_, this, ()| Ok(this.has_attack_miss()));
    }
}

impl UserData for ScriptActionKindResultView {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_standard", |_, this, ()| Ok(this.is_standard()));
        methods.add_method("as_standard", |_, this, ()| Ok(this.as_standard().clone()));
    }
}

impl UserData for ScriptActionResultView {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("performer", |_, this| Ok(this.performer.clone()));
        fields.add_field_method_get("target", |_, this| Ok(this.target.clone()));
        fields.add_field_method_get("kind", |_, this| Ok(this.kind.clone()));
    }
}

impl UserData for ScriptActionPerformedView {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("action", |_, this| Ok(this.action.clone()));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("results", |_, this, ()| Ok(this.results().clone()));
    }
}

impl UserData for ScriptEventView {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_d20_check_performed", |_, this, ()| {
            Ok(this.is_d20_check_performed())
        });
        methods.add_method("as_d20_check_performed", |_, this, ()| {
            Ok(this.as_d20_check_performed().clone())
        });
        methods.add_method("is_action_requested", |_, this, ()| {
            Ok(this.is_action_requested())
        });
        methods.add_method("as_action_requested", |_, this, ()| {
            Ok(this.as_action_requested().clone())
        });
        methods.add_method("is_action_performed", |_, this, ()| {
            Ok(this.is_action_performed())
        });
        methods.add_method("as_action_performed", |_, this, ()| {
            Ok(this.as_action_performed().clone())
        });
        methods.add_method("is_moving_out_of_reach", |_, this, ()| {
            Ok(this.is_moving_out_of_reach())
        });
        methods.add_method("as_moving_out_of_reach", |_, this, ()| {
            Ok(this.as_moving_out_of_reach().clone())
        });
    }
}

// ---------------------------------------------------------------------------
// Reaction contexts
// ---------------------------------------------------------------------------

impl UserData for ScriptReactionTriggerContext {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("reactor", |_, this| Ok(this.reactor.clone()));
        fields.add_field_method_get("event", |_, this| Ok(this.event.clone()));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_own_failed_d20_check", |_, this, dc_kind: String| {
            if !this.event.is_d20_check_performed() {
                return Ok(false);
            }
            let d20_check = this.event.as_d20_check_performed();
            let result = d20_check.result();
            Ok(d20_check.performer() == this.reactor
                && !result.is_success
                && result.dc_kind.label == dc_kind)
        });
    }
}

impl UserData for ScriptReactionBodyContext {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("reactor", |_, this| Ok(this.reactor.clone()));
        fields.add_field_method_get("event", |_, this| Ok(this.event.clone()));
        fields.add_field_method_get("reaction_id", |_, this| Ok(this.reaction_id.clone()));
        fields.add_field_method_get("context", |_, this| Ok(this.context.clone()));
    }
}

// ---------------------------------------------------------------------------
// GameState — the script-facing entry point for everything world-related.
// ---------------------------------------------------------------------------

impl UserData for GameState {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        // -- Resource queries / mutations --
        methods.add_method(
            "can_afford_resource",
            |_, this, (entity, resource_id, amount): (ScriptEntity, String, Value)| {
                let id = parse_resource_id(&resource_id)?;
                let amount = parse_resource_amount(amount)?;
                Ok(
                    systems::helpers::get_component::<ResourceMap>(&this.world, entity.into())
                        .can_afford(&id, &amount),
                )
            },
        );

        methods.add_method_mut(
            "add_resource",
            |_, this, (entity, resource_id, amount): (ScriptEntity, String, Value)| {
                let id = parse_resource_id(&resource_id)?;
                let amount = parse_resource_amount(amount)?;
                systems::helpers::get_component_mut::<ResourceMap>(&mut this.world, entity.into())
                    .add(id, amount.into(), true);
                Ok(())
            },
        );

        // -- HP --
        methods.add_method("hp_current", |_, this, entity: ScriptEntity| {
            let hp = systems::helpers::get_component::<HitPoints>(&this.world, entity.into());
            Ok(hp.current() as i64)
        });
        methods.add_method("hp_max", |_, this, entity: ScriptEntity| {
            let hp = systems::helpers::get_component::<HitPoints>(&this.world, entity.into());
            Ok(hp.max() as i64)
        });

        // -- Ability modifier --
        methods.add_method(
            "ability_modifier",
            |_, this, (entity, name): (ScriptEntity, String)| {
                let ability = name
                    .parse()
                    .map_err(|e| mlua::Error::RuntimeError(format!("Invalid ability name: {e}")))?;
                let scores =
                    systems::helpers::get_component::<AbilityScoreMap>(&this.world, entity.into());
                Ok(scores.ability_modifier(&ability).total() as i64)
            },
        );

        // -- Loadout --
        methods.add_method("armor_type", |_, this, entity: ScriptEntity| {
            let loadout = systems::loadout::loadout(&this.world, entity.into());
            Ok(match loadout.armor() {
                Some(armor) => armor.armor_type.to_string(),
                None => "None".to_string(),
            })
        });
        methods.add_method(
            "wielding_with_both_hands",
            |_, this, (entity, weapon_kind): (ScriptEntity, String)| {
                let kind = serde_plain::from_str(&weapon_kind.to_lowercase()).map_err(|e| {
                    mlua::Error::RuntimeError(format!("Failed to parse WeaponKind: {e}"))
                })?;
                let loadout = systems::loadout::loadout(&this.world, entity.into());
                Ok(loadout.is_wielding_weapon_with_both_hands(&kind))
            },
        );

        // -- Effects --
        methods.add_method_mut(
            "apply_effect",
            |_,
             this,
             (applier, target, effect_id, action): (
                ScriptEntity,
                ScriptEntity,
                String,
                ScriptActionPerformedView,
            )| {
                let effect_id = parse_effect_id(&effect_id)?;
                apply_effect_impl(this, applier, target, effect_id, None, false, action);
                Ok(())
            },
        );

        methods.add_method_mut(
            "apply_effect_for_turns",
            |_,
             this,
             (applier, target, effect_id, turns, one_shot, action): (
                ScriptEntity,
                ScriptEntity,
                String,
                i64,
                bool,
                ScriptActionPerformedView,
            )| {
                if turns <= 0 {
                    return Err(mlua::Error::RuntimeError(
                        "turns must be greater than 0".into(),
                    ));
                }
                let effect_id = parse_effect_id(&effect_id)?;
                apply_effect_impl(
                    this,
                    applier,
                    target,
                    effect_id,
                    Some(turns as u32),
                    one_shot,
                    action,
                );
                Ok(())
            },
        );

        methods.add_method_mut(
            "remove_effect",
            |_, this, (target, effect_id): (ScriptEntity, String)| {
                let id = parse_effect_id(&effect_id)?;
                systems::effects::remove_effects_by_id(this, target.into(), &id);
                Ok(())
            },
        );

        methods.add_method_mut(
            "heal",
            |_,
             this,
             (target, dice, bonus, source): (
                ScriptEntity,
                String,
                ModifierSet,
                ModifierSource,
            )| {
                let mut dice_roll: DiceSetRoll = dice.parse().map_err(|e| {
                    mlua::Error::RuntimeError(format!("Invalid dice expression: {e}"))
                })?;
                dice_roll.modifiers.add_modifier_set(&bonus);
                let healing = dice_roll.roll();
                let target_entity: hecs::Entity = target.into();
                systems::health::heal(&mut this.world, target_entity, healing.subtotal as u32);
                let event = crate::engine::event::Event::new(
                    crate::engine::event::EventKind::Healing {
                        entity: crate::components::id::EntityIdentifier::from_world(
                            &this.world,
                            target_entity,
                        ),
                        amount: healing,
                        source,
                    },
                );
                this.process_event(event);
                Ok(())
            },
        );
    }
}

/// Helper: queue an apply-effect command via the existing system path.
fn apply_effect_impl(
    game_state: &mut GameState,
    applier: ScriptEntity,
    target: ScriptEntity,
    effect_id: EffectId,
    turns: Option<u32>,
    one_shot: bool,
    action: ScriptActionPerformedView,
) {
    let lifetime = match turns {
        Some(t) => EffectLifetimeTemplate::TurnBoundary {
            entity: EffectEntiyReference::Applier,
            boundary: TurnBoundary::Start,
            duration: TimeDuration::from_turns(t),
        },
        None => EffectLifetimeTemplate::Permanent,
    };

    let action_id = action.action.action_id.clone();
    let action_context = action.action.context.clone();
    let action_resolution = action
        .resolution()
        .map(|r| r.into())
        .unwrap_or(ActionConditionResolution::Unconditional);

    let target_entity: hecs::Entity = target.into();

    game_state.process_event(crate::engine::event::Event::new(
        crate::engine::event::EventKind::GainedEffect {
            entity: crate::components::id::EntityIdentifier::from_world(
                &game_state.world,
                target_entity,
            ),
            effect: effect_id.clone(),
        },
    ));

    systems::effects::add_effect_template(
        game_state,
        applier.into(),
        target_entity,
        ModifierSource::Action(action_id),
        &EffectInstanceTemplate {
            effect_id,
            lifetime,
            end_condition: None,
            one_shot,
        },
        Some(&action_context),
        action_resolution,
    );
}

impl UserData for ResourceAmountMap {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("costs_resource", |_, this, resource_id: String| {
            let id = parse_resource_id(&resource_id)?;
            Ok(this
                .map
                .get(&id)
                .map(|resource| !resource.is_empty())
                .unwrap_or(false))
        });
        methods.add_method_mut(
            "replace_resource",
            |_, this, (from, to, new_amount): (String, String, String)| {
                let from = parse_resource_id(&from)?;
                let to = parse_resource_id(&to)?;
                let amount: ResourceAmount = serde_plain::from_str(&new_amount).map_err(|e| {
                    mlua::Error::RuntimeError(format!("Failed to parse ResourceAmount: {e}"))
                })?;
                this.replace_resource(&from, &to, &amount);
                Ok(())
            },
        );
    }
}

// ---------------------------------------------------------------------------
// Global module tables (ReactionPlan, SavingThrow, ModifierSet, ModifierSource)
// ---------------------------------------------------------------------------

pub fn register_globals(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    // ReactionPlan ----------------------------------------------------------
    let reaction_plan = lua.create_table()?;
    reaction_plan.set(
        "none",
        lua.create_function(|_, ()| Ok(ScriptReactionPlan::None))?,
    )?;
    reaction_plan.set(
        "sequence",
        lua.create_function(|_, plans: Variadic<ScriptReactionPlan>| {
            Ok(ScriptReactionPlan::Sequence(plans.into_iter().collect()))
        })?,
    )?;
    reaction_plan.set(
        "require_saving_throw",
        lua.create_function(
            |_,
             (target_role, dc, on_success, on_failure): (
                String,
                ScriptSavingThrow,
                ScriptReactionPlan,
                ScriptReactionPlan,
            )| {
                let target = target_role.parse().map_err(|e| {
                    mlua::Error::RuntimeError(format!("Failed to parse ScriptEntityRole: {e}"))
                })?;
                Ok(ScriptReactionPlan::RequireSavingThrow {
                    target,
                    dc,
                    on_success: Box::new(on_success),
                    on_failure: Box::new(on_failure),
                })
            },
        )?,
    )?;
    reaction_plan.set(
        "cancel_trigger_event",
        lua.create_function(|_, resources: Variadic<String>| {
            let parsed: Result<Vec<ResourceId>, _> = resources
                .into_iter()
                .map(|s| s.parse::<ResourceId>())
                .collect();
            let resources_to_refund = parsed.map_err(|e| {
                mlua::Error::RuntimeError(format!("Failed to parse ResourceId: {e}"))
            })?;
            Ok(ScriptReactionPlan::CancelEvent {
                event: ScriptEventRef::TriggerEvent,
                resources_to_refund,
            })
        })?,
    )?;
    globals.set("ReactionPlan", reaction_plan)?;

    // SavingThrow -----------------------------------------------------------
    let saving_throw = lua.create_table()?;
    saving_throw.set(
        "dc",
        lua.create_function(|_, (entity_role, saving_throw): (String, String)| {
            let entity = entity_role.parse().map_err(|e| {
                mlua::Error::RuntimeError(format!("Failed to parse ScriptEntityRole: {e}"))
            })?;
            let saving_throw = saving_throw.parse().map_err(|e| {
                mlua::Error::RuntimeError(format!("Failed to parse SavingThrowProvider: {e}"))
            })?;
            Ok(ScriptSavingThrow {
                entity,
                saving_throw,
            })
        })?,
    )?;
    globals.set("SavingThrow", saving_throw)?;

    // ModifierSet -----------------------------------------------------------
    let modifier_set = lua.create_table()?;
    modifier_set.set(
        "empty",
        lua.create_function(|_, ()| Ok(ModifierSet::new()))?,
    )?;
    modifier_set.set(
        "from",
        lua.create_function(|_, (source, value): (ModifierSource, i64)| {
            Ok(ModifierSet::from(source, value as i32))
        })?,
    )?;
    globals.set("ModifierSet", modifier_set)?;

    // ModifierSource --------------------------------------------------------
    let modifier_source = lua.create_table()?;
    modifier_source.set(
        "ability",
        lua.create_function(|_, ability_name: String| {
            let ability = ability_name
                .parse()
                .map_err(|e| mlua::Error::RuntimeError(format!("Invalid ability name: {e}")))?;
            Ok(ModifierSource::Ability(ability))
        })?,
    )?;
    modifier_source.set(
        "base",
        lua.create_function(|_, ()| Ok(ModifierSource::Base))?,
    )?;
    modifier_source.set(
        "effect",
        lua.create_function(|_, effect_id: String| {
            let id = parse_effect_id(&effect_id)?;
            Ok(ModifierSource::Effect(id))
        })?,
    )?;
    globals.set("ModifierSource", modifier_source)?;

    Ok(())
}
