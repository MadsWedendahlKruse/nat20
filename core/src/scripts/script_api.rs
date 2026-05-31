use std::str::FromStr;

use hecs::{Entity, World};
use mlua::{
    FromLua, Function, Lua, MetaMethod, UserData, UserDataFields, UserDataMethods,
    Value::{self},
    prelude::{LuaError, LuaResult},
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
        dice::DiceSetRoll,
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
        action_prompt::ActionData,
        event::{Event, EventKind},
        game_state::GameState,
    },
    registry::serialize::{
        parser::{DiceExpression, Evaluable, IntExpression, Parser},
        variables::PARSER_VARIABLES,
    },
    systems::{
        self,
        d20::{D20CheckDCKind, D20CheckKind, D20ResultKind},
    },
};

/// `Entity` is owned by hecs, so to implement `UserData` for it we need to wrap
/// it in a struct that we own
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScriptEntity {
    pub id: u64,
}

impl From<Entity> for ScriptEntity {
    fn from(entity: Entity) -> Self {
        ScriptEntity {
            id: u64::from(entity.to_bits()),
        }
    }
}

impl From<EntityIdentifier> for ScriptEntity {
    fn from(identifier: EntityIdentifier) -> Self {
        ScriptEntity::from(identifier.id())
    }
}

impl Into<Entity> for ScriptEntity {
    fn into(self) -> Entity {
        Entity::from_bits(self.id).unwrap()
    }
}

impl From<u64> for ScriptEntity {
    fn from(id: u64) -> Self {
        ScriptEntity { id }
    }
}

#[derive(Clone)]
pub enum ScriptDiceRollBonus {
    Flat(IntExpression),
    Dice(DiceExpression),
}

impl ScriptDiceRollBonus {
    pub fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
    ) -> Result<i32, LuaError> {
        match self {
            ScriptDiceRollBonus::Flat(expr) => expr
                .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                .map_err(|e| {
                    LuaError::RuntimeError(format!("Failed to evaluate flat bonus expression: {e}"))
                }),
            ScriptDiceRollBonus::Dice(expr) => {
                let dice_roll = expr
                    .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                    .map_err(|e| {
                        LuaError::RuntimeError(format!(
                            "Failed to evaluate dice bonus expression: {e}"
                        ))
                    })?;

                Ok(dice_roll.roll().subtotal)
            }
        }
    }

    pub fn evaluate_without_variables(&self) -> Result<i32, LuaError> {
        match self {
            ScriptDiceRollBonus::Flat(expr) => expr.evaluate_without_variables().map_err(|e| {
                LuaError::RuntimeError(format!("Failed to evaluate flat bonus expression: {e}"))
            }),
            ScriptDiceRollBonus::Dice(expr) => {
                let dice_roll = expr.evaluate_without_variables().map_err(|e| {
                    LuaError::RuntimeError(format!("Failed to evaluate dice bonus expression: {e}"))
                })?;

                Ok(dice_roll.roll().subtotal)
            }
        }
    }
}

impl FromStr for ScriptDiceRollBonus {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(flat) = Parser::new(s).parse_int_expression() {
            Ok(ScriptDiceRollBonus::Flat(flat))
        } else if let Ok(expr) = Parser::new(s).parse_dice_expression() {
            Ok(ScriptDiceRollBonus::Dice(expr))
        } else {
            Err(format!("Invalid ScriptDiceRollBonus expression: {}", s))
        }
    }
}

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

fn parse_id<T>(s: &str) -> LuaResult<T>
where
    T: FromStr,
    T::Err: std::fmt::Display,
{
    s.parse()
        .map_err(|e| mlua::Error::RuntimeError(format!("Failed to parse ID: {e}")))
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

impl UserData for ScriptEntity {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("id", |_, this| Ok(this.id));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Eq, |_, a, b: ScriptEntity| Ok(a.id == b.id));
    }
}

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
                        return Err(mlua::Error::RuntimeError(
                            "Flat bonuses not implemented yet".to_string(),
                        ));
                    }
                    ScriptDiceRollBonus::Dice(dice_expression) => {
                        let mut dice_roll = dice_expression.evaluate_without_variables().unwrap();
                        if this.crit {
                            dice_roll.dice.num_dice *= CRIT_DICE_MULTIPLIER as u32;
                        }
                        this.add_component(DamageComponentResult {
                            result: dice_roll.roll(),
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

impl UserData for EffectInstance {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("effect_id", |_, this| Ok(this.effect_id.to_string()));
        fields.add_field_method_get("source", |_, this| Ok(this.source.to_string()));
        fields.add_field_method_get("applier", |_, this| {
            Ok(this.applier.map(ScriptEntity::from))
        });
    }
}

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
            |_, this, (bonus, source, force_use_new): (String, String, bool)| {
                let bonus: ScriptDiceRollBonus = bonus.parse().map_err(|e: String| {
                    mlua::Error::RuntimeError(format!("Invalid bonus: {e}"))
                })?;
                let mut new_result = this.reroll();
                let bonus_value = bonus.evaluate_without_variables()?;
                new_result
                    .d20_result_mut()
                    .add_modifier(parse_source(&source), bonus_value);

                if force_use_new || new_result.d20_result().total() > this.d20_result().total() {
                    *this = new_result;
                }

                Ok(())
            },
        );

        methods.add_method_mut(
            "modify_result",
            |_, this, (bonus, source): (String, String)| {
                let bonus: ScriptDiceRollBonus = bonus.parse().map_err(|e: String| {
                    mlua::Error::RuntimeError(format!("Invalid bonus: {e}"))
                })?;
                let bonus_value = bonus.evaluate_without_variables()?;
                this.d20_result_mut()
                    .add_modifier(parse_source(&source), bonus_value);
                Ok(())
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

impl UserData for ModifierSource {}

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
        fields.add_field_method_get("trigger_event", |_, this| {
            Ok(this.trigger_event.as_ref().map(|e| e.as_ref().clone()))
        });
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("costs_resource", |_, this, resource_id: String| {
            let id = parse_id::<ResourceId>(&resource_id)?;
            Ok(this.resource_cost.map.contains_key(&id))
        });
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
                let id = parse_id::<ResourceId>(&resource_id)?;
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
                let id = parse_id::<ResourceId>(&resource_id)?;
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
                    parse_id::<EffectId>(&effect_id)?,
                    None,
                    false,
                    parse_id::<EffectId>(&source_effect)?,
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
                    parse_id::<EffectId>(&effect_id)?,
                    Some(turns as u32),
                    one_shot,
                    parse_id::<EffectId>(&source_effect)?,
                    context,
                    resolution,
                );
                Ok(())
            },
        );

        methods.add_method_mut(
            "remove_effect",
            |_, this, (target, effect_id): (ScriptEntity, String)| {
                let id = parse_id::<EffectId>(&effect_id)?;
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
                let event = Event::new(
                    EventKind::Healing {
                        entity: EntityIdentifier::from_world(
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
            let id = parse_id::<ResourceId>(&resource_id)?;
            Ok(this
                .map
                .get(&id)
                .map(|resource| !resource.is_empty())
                .unwrap_or(false))
        });
        methods.add_method_mut(
            "replace_resource",
            |_, this, (from, to, new_amount): (String, String, String)| {
                let from = parse_id::<ResourceId>(&from)?;
                let to = parse_id::<ResourceId>(&to)?;
                let amount: ResourceAmount = serde_plain::from_str(&new_amount).map_err(|e| {
                    mlua::Error::RuntimeError(format!("Failed to parse ResourceAmount: {e}"))
                })?;
                this.replace_resource(&from, &to, &amount);
                Ok(())
            },
        );
    }
}

pub fn register_globals(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

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
            let id = parse_id::<EffectId>(&effect_id)?;
            Ok(ModifierSource::Effect(id))
        })?,
    )?;
    globals.set("ModifierSource", modifier_source)?;

    Ok(())
}
