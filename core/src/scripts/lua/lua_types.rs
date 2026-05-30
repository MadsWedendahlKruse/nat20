use std::str::FromStr;

use hecs::Entity;
use mlua::{
    FromLua, Function, Lua, MetaMethod, Result as LuaResult, UserData, UserDataFields,
    UserDataMethods,
    Value::{self},
    Variadic,
};

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::{
            action::{
                ActionCondition, ActionConditionResolution, ActionContext, ActionKind,
                ActionKindResult, ActionOutcomeBundle, ActionResult, DamageOutcome,
            },
            targeting::TargetInstance,
        },
        damage::{
            AttackSource, CRIT_DICE_MULTIPLIER, DamageComponentResult, DamageMitigationEffect,
            DamageMitigationResult, DamageRollResult, MitigationOperation,
        },
        dice::{DiceSet, DiceSetRoll},
        effects::effect::{
            EffectEntiyReference, EffectInstance, EffectInstanceTemplate, EffectLifetimeTemplate,
        },
        health::hit_points::HitPoints,
        id::{ClassId, EffectId, EntityIdentifier, ResourceId},
        level::CharacterLevels,
        modifier::{Modifiable, ModifierSet, ModifierSource},
        resource::{ResourceAmount, ResourceAmountMap, ResourceMap},
        time::{TimeDuration, TurnBoundary},
    },
    engine::{
        action_prompt::{ActionData, ReactionData},
        event::{Event, EventKind},
        game_state::GameState,
    },
    registry::serialize::parser::Parser,
    scripts::script_api::{
        ScriptDiceRollBonus, ScriptEntity, ScriptEventRef, ScriptReactionBodyResult,
        ScriptReactionPlan, ScriptSavingThrow,
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20CheckKind, D20ResultKind},
    },
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
    D20CheckDCKind,
    D20ResultKind,
    ScriptSavingThrow,
    ScriptReactionPlan,
    ScriptReactionBodyResult,
    ActionContext,
    ActionData,
    ActionConditionResolution,
    DamageOutcome,
    ActionOutcomeBundle,
    ActionResult,
    ActionKindResult,
    Event,
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
        methods.add_meta_method(MetaMethod::Eq, |_, a, b: ScriptEntity| Ok(a.id == b.id));
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

impl UserData for D20CheckKind {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("saving_throw", |_, this, ()| {
            let D20CheckKind::SavingThrow(kind) = this else {
                return Ok(None);
            };
            Ok(Some(kind.to_string().to_lowercase()))
        });
        methods.add_method("skill", |_, this, ()| {
            let D20CheckKind::Skill(skill) = this else {
                return Ok(None);
            };
            Ok(Some(skill.to_string().to_lowercase()))
        });
        methods.add_method("attack_roll", |_, this, ()| {
            let D20CheckKind::AttackRoll(attack_source) = this else {
                return Ok(None);
            };
            Ok(match attack_source {
                AttackSource::Weapon(weapon_kind) => Some(weapon_kind.to_string().to_lowercase()),
                AttackSource::Spell => Some("spell".to_string()),
            })
        });
    }
}

impl UserData for D20ResultKind {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("kind", |_, this| Ok(this.kind().clone()));
        fields.add_field_method_get("total", |_, this| Ok(this.d20_result().total()));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_success", |_, this, dc: D20CheckDCKind| {
            Ok(this.is_success(&dc))
        });

        methods.add_method_mut(
            "reroll_bonus",
            |_, this, (bonus, source, force_use_new): (Value, String, bool)| {
                let bonus = match bonus {
                    Value::Integer(int) => int,
                    Value::String(string) => todo!(),
                    _ => {
                        return Err(mlua::Error::RuntimeError(
                            "Bonus must be an integer or string".to_string(),
                        ));
                    }
                };
                let mut new_result = this.reroll();
                new_result
                    .d20_result_mut()
                    .add_modifier(parse_source(&source), bonus as i32);

                if force_use_new {
                    *this = new_result;
                } else {
                    if new_result.d20_result().total() > this.d20_result().total() {
                        *this = new_result;
                    }
                }

                Ok(())
            },
        );

        methods.add_method_mut(
            "modify_result",
            |_, this, (bonus, source): (String, String)| {
                if let Ok(dice_expression) = Parser::new(&bonus).parse_dice_expression() {
                    match dice_expression.evaluate_without_variables() {
                        Ok((count, size, modifier)) => {
                            let roll = DiceSetRoll {
                                dice: DiceSet::from_str(format!("{}d{}", count, size).as_str())
                                    .unwrap(),
                                modifiers: ModifierSet::from(parse_source(&source), modifier),
                            }
                            .roll();
                            this.d20_result_mut()
                                .add_modifier(parse_source(&source), roll.subtotal as i32);
                            Ok(())
                        }
                        Err(err) => Err(mlua::Error::RuntimeError(format!(
                            "Failed to evaluate dice expression: {err}"
                        ))),
                    }
                } else {
                    Err(mlua::Error::RuntimeError(format!(
                        "Invalid dice expression: {}",
                        bonus
                    )))
                }
            },
        );
    }
}

fn parse_source(source: &str) -> ModifierSource {
    if source.contains("action") {
        ModifierSource::Action(source.into())
    } else {
        ModifierSource::Custom(source.into())
    }
}

impl UserData for D20CheckDCKind {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("kind", |_, this| Ok(this.kind().clone()));
        // TODO: Kind of a funky method
        fields.add_field_method_get("target", |_, this| {
            Ok(match this {
                D20CheckDCKind::SavingThrow { .. } => None,
                D20CheckDCKind::Skill { .. } => None,
                D20CheckDCKind::AttackRoll(target, ..) => Some(ScriptEntity::from(target.id())),
            })
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

impl UserData for ReactionData {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("reactor", |_, this| {
            Ok(ScriptEntity::from(this.reactor.id()))
        });
        fields.add_field_method_get("event", |_, this| Ok(this.event.as_ref().clone()));
        fields.add_field_method_get("reaction_id", |_, this| Ok(this.reaction_id.to_string()));
        fields.add_field_method_get("context", |_, this| Ok(this.context.clone()));
    }
}

impl UserData for ActionConditionResolution {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_unconditional", |_, this, ()| {
            Ok(matches!(this, ActionConditionResolution::Unconditional))
        });
        methods.add_method("is_attack_roll", |_, this, ()| {
            Ok(matches!(this, ActionConditionResolution::AttackRoll { .. }))
        });
        methods.add_method("is_saving_throw", |_, this, ()| {
            Ok(matches!(
                this,
                ActionConditionResolution::SavingThrow { .. }
            ))
        });
        methods.add_method("is_success", |_, this, ()| Ok(this.is_success()));
        methods.add_method("is_crit", |_, this, ()| Ok(this.is_crit()));
    }
}

impl UserData for DamageOutcome {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("damage_roll", |_, this, ()| Ok(this.damage_roll.clone()));
        methods.add_method("damage_taken", |_, this, ()| Ok(this.damage_taken.clone()));
        methods.add_method("damage_roll_total", |_, this, ()| {
            Ok(this
                .damage_roll
                .as_ref()
                .map(|damage_roll| damage_roll.total)
                .unwrap_or(0))
        });
        methods.add_method("damage_taken_total", |_, this, ()| {
            Ok(this
                .damage_taken
                .as_ref()
                .map(|mitigation| mitigation.total)
                .unwrap_or(0))
        });
    }
}

impl UserData for ActionOutcomeBundle {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("damage", |_, this, ()| Ok(this.damage.clone()));
        methods.add_method("resolution", |_, this, ()| Ok(this.resolution().clone()));
    }
}

impl UserData for ActionKindResult {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("as_standard", |_, this, ()| {
            Ok(match this {
                ActionKindResult::Standard(outcome) => Some(outcome.clone()),
                _ => None,
            })
        });
    }
}

impl UserData for ActionResult {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("performer", |_, this| {
            Ok(ScriptEntity::from(this.performer.id()))
        });
        fields.add_field_method_get("target", |_, this| Ok(this.target.clone()));
        fields.add_field_method_get("kind", |_, this| Ok(this.kind.clone()));
    }
}

impl UserData for TargetInstance {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("entity", |_, this, ()| {
            Ok(match this {
                TargetInstance::Entity { entity, .. } => Some(ScriptEntity::from(entity.id())),
                TargetInstance::Point { .. } => None,
            })
        });
    }
}

impl UserData for Event {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_d20_check_performed", |_, this, ()| {
            Ok(matches!(this.kind, EventKind::D20CheckPerformed(_, _, _)))
        });
        methods.add_method("as_d20_check_performed", |_, this, ()| {
            let EventKind::D20CheckPerformed(actor, kind, result) = &this.kind else {
                return Ok((None, None, None));
            };
            Ok((
                Some(ScriptEntity::from(actor.id())),
                Some(kind.clone()),
                Some(result.clone()),
            ))
        });
        methods.add_method_mut("with_d20_check", |lua, this, callback: Function| {
            let EventKind::D20CheckPerformed(_, result, dc) = &mut this.kind else {
                return Ok(());
            };
            lua.scope(|scope| {
                callback.call::<()>((
                    scope.create_userdata_ref_mut(result)?,
                    scope.create_userdata_ref_mut(dc)?,
                ))
            })?;
            Ok(())
        });
        methods.add_method("is_action_requested", |_, this, ()| {
            Ok(matches!(this.kind, EventKind::ActionRequested { .. }))
        });
        methods.add_method("as_action_requested", |_, this, ()| {
            let EventKind::ActionRequested { action } = &this.kind else {
                return Ok(None);
            };
            Ok(Some(action.clone()))
        });
        methods.add_method("is_action_performed", |_, this, ()| {
            Ok(matches!(this.kind, EventKind::ActionPerformed { .. }))
        });
        methods.add_method("as_action_performed", |_, this, ()| {
            let EventKind::ActionPerformed { action, results } = &this.kind else {
                return Ok((None, None));
            };
            Ok((Some(action.clone()), Some(results.clone())))
        });
        methods.add_method("is_moving_out_of_reach", |_, this, ()| {
            Ok(matches!(this.kind, EventKind::MovingOutOfReach { .. }))
        });
        methods.add_method("as_moving_out_of_reach", |_, this, ()| {
            let EventKind::MovingOutOfReach {
                mover,
                entity,
                continue_movement,
            } = &this.kind
            else {
                return Ok((None, None, None));
            };
            Ok((
                Some(ScriptEntity::from(mover.id())),
                Some(ScriptEntity::from(entity.id())),
                Some(*continue_movement),
            ))
        });
        methods.add_method_mut("with_moving_out_of_reach", |_, this, callback: Function| {
            let EventKind::MovingOutOfReach {
                mover,
                entity,
                continue_movement,
            } = &mut this.kind
            else {
                return Ok(());
            };
            let new_continue: bool = callback.call((
                ScriptEntity::from(mover.id()),
                ScriptEntity::from(entity.id()),
                *continue_movement,
            ))?;
            *continue_movement = new_continue;
            Ok(())
        });
    }
}

// ---------------------------------------------------------------------------
// GameState — the script-facing entry point for everything world-related.
// ---------------------------------------------------------------------------

impl UserData for GameState {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method(
            "class_level",
            |_, this, (entity, class): (ScriptEntity, String)| {
                let class: ClassId = class.parse().map_err(|e| {
                    mlua::Error::RuntimeError(format!("Failed to parse class name: {e}"))
                })?;
                Ok(
                    systems::helpers::get_component::<CharacterLevels>(&this.world, entity.into())
                        .class_level(&class)
                        .map(|level| level.level())
                        .unwrap_or(0),
                )
            },
        );

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
             (applier, target, effect_id, source_effect, context): (
                ScriptEntity,
                ScriptEntity,
                String,
                String,
                ActionContext,
            )| {
                apply_effect_impl(
                    this,
                    applier,
                    target,
                    parse_effect_id(&effect_id)?,
                    None,
                    false,
                    parse_effect_id(&source_effect)?,
                    context,
                    ActionConditionResolution::Unconditional,
                );
                Ok(())
            },
        );

        methods.add_method_mut(
            "apply_effect_for_turns",
            |_,
             this,
             (applier, target, effect_id, turns, one_shot, source_effect, context, resolution): (
                ScriptEntity,
                ScriptEntity,
                String,
                i64,
                bool,
                String,
                ActionContext,
                ActionConditionResolution,
            )| {
                if turns <= 0 {
                    return Err(mlua::Error::RuntimeError(
                        "turns must be greater than 0".into(),
                    ));
                }
                apply_effect_impl(
                    this,
                    applier,
                    target,
                    parse_effect_id(&effect_id)?,
                    Some(turns as u32),
                    one_shot,
                    parse_effect_id(&source_effect)?,
                    context,
                    resolution,
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
    source: EffectId,
    context: ActionContext,
    resolution: ActionConditionResolution,
) {
    let lifetime = match turns {
        Some(t) => EffectLifetimeTemplate::TurnBoundary {
            entity: EffectEntiyReference::Applier,
            boundary: TurnBoundary::Start,
            duration: TimeDuration::from_turns(t),
        },
        None => EffectLifetimeTemplate::Permanent,
    };

    let target_entity: Entity = target.into();

    game_state.process_event(Event::new(EventKind::GainedEffect {
        entity: EntityIdentifier::from_world(&game_state.world, target_entity),
        effect: effect_id.clone(),
    }));

    systems::effects::add_effect_template(
        game_state,
        applier.into(),
        target_entity,
        ModifierSource::Effect(source),
        &EffectInstanceTemplate {
            effect_id,
            lifetime,
            end_condition: None,
            one_shot,
        },
        Some(&context),
        resolution,
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
