use std::{collections::BTreeMap, str::FromStr};

use hecs::{Entity, World};
use mlua::{
    FromLua, Function, Lua, MetaMethod, Table, UserData, UserDataFields, UserDataMethods,
    Value::{self},
    prelude::{LuaError, LuaResult},
};

use crate::{
    components::{
        ability::{Ability, AbilityScoreMap},
        actions::{
            action::{
                ActionConditionResolution, ActionContext, ActionResult, ActionResultComponent,
                DamageResult, EffectResult, EffectResultKind, HealingResult,
            },
            targeting::TargetInstance,
        },
        damage::{
            AttackSource, DamageComponent, DamageComponentResult, DamageMitigationEffect,
            DamageMitigationResult, DamageModifiable, DamageRoll, DamageRollResult,
            MitigationOperation,
        },
        effects::effect::{
            EffectEntiyReference, EffectInstance, EffectInstanceTemplate, EffectLifetimeTemplate,
        },
        health::hit_points::HitPoints,
        id::{ClassId, EffectId, EntityIdentifier, Id, ResourceId},
        items::inventory::ItemInstance,
        level::CharacterLevels,
        modifier::{
            FlatModifierMap, Modifiable, ModifierKindResult, ModifierMap, ModifierResult,
            ModifierSource,
        },
        resource::{ResourceAmount, ResourceAmountMap, ResourceMap},
        time::{TimeDuration, TurnBoundary},
    },
    engine::{
        action_prompt::ActionData,
        event::{Event, EventKind},
        game_state::GameState,
    },
    registry::{
        registry::ItemsRegistry,
        serialize::{
            parser::{
                Evaluable, EvaluableWithoutVariables, EvaluationError, ModifierExpression, Parser,
            },
            variables::{PARSER_VARIABLES, VariableMap},
        },
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

impl FromStr for ModifierExpression {
    type Err = LuaError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Parser::new(s).parse_modifier_expression().map_err(|e| {
            LuaError::RuntimeError(format!("Failed to parse modifier expression: {e}"))
        })
    }
}

/// Accept either an integer or a string for resource amounts. Scripts that pass
/// `"1"` or `1` both work.
fn amount_to_string(value: Value) -> LuaResult<String> {
    match value {
        Value::String(s) => Ok(s.to_str()?.to_string()),
        Value::Integer(i) => Ok(i.to_string()),
        Value::Number(n) => Ok(n.to_string()),
        other => Err(LuaError::RuntimeError(format!(
            "Expected string or integer for resource amount, got {}",
            other.type_name()
        ))),
    }
}

fn parse_resource_amount(value: Value) -> LuaResult<ResourceAmount> {
    let s = amount_to_string(value)?;
    serde_plain::from_str(&s)
        .map_err(|e| LuaError::RuntimeError(format!("Failed to parse ResourceAmount: {e}")))
}

fn parse_id<T>(s: &str) -> LuaResult<T>
where
    T: FromStr,
    T::Err: std::fmt::Display,
{
    s.parse()
        .map_err(|e| LuaError::RuntimeError(format!("Failed to parse ID: {e}")))
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
                        other => Err(LuaError::FromLuaConversionError {
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
    ActionConditionResolution,
    ActionContext,
    ActionData,
    ActionResult,
    D20CheckDCKind,
    D20ResultKind,
    DamageComponent,
    DamageMitigationResult,
    DamageResult,
    DamageRoll,
    DamageRollResult,
    EffectInstance,
    Event,
    FlatModifierMap,
    ModifierMap,
    ModifierSource,
    ResourceAmountMap,
    ScriptEntity,
);

impl UserData for ScriptEntity {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("id", |_, this| Ok(this.id));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Eq, |_, a, b: ScriptEntity| Ok(a.id == b.id));
    }
}

impl UserData for DamageComponent {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("damage_type", |_, this| {
            Ok(this.damage_type.to_string().to_lowercase())
        });
        fields.add_field_method_get("damage", |_, this| Ok(this.modifiers().clone()));
    }
}

impl UserData for DamageRoll {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("components", |_, this| Ok(this.components.clone()));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method_mut(
            "add_damage",
            |_, this, (dice, damage_type, source): (String, String, String)| {
                add_damage_component(this, dice, damage_type, source)
            },
        );
    }
}

impl UserData for DamageComponentResult {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("damage_type", |_, this| {
            Ok(this.damage_type.to_string().to_lowercase())
        });
        fields.add_field_method_get("result", |_, this| Ok(this.result.clone()));
    }
}

impl UserData for DamageRollResult {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("components", |_, this| Ok(this.components.clone()));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method_mut("clamp_damage_dice_min", |_, this, min: i64| {
            let minimum_roll = min as u32;
            for component in &mut this.components {
                for (_, modifier) in component.result.iter_mut() {
                    if let ModifierKindResult::Dice(dice_set) = modifier {
                        for roll in dice_set.rolls_mut() {
                            if *roll < minimum_roll {
                                *roll = minimum_roll;
                            }
                        }
                    }
                }
            }
            this.recalculate_total();
            Ok(())
        });

        methods.add_method_mut(
            "add_damage",
            |_, this, (amount, damage_type, source): (String, String, String)| {
                add_damage_component(this, amount, damage_type, source)
            },
        );
    }
}

fn add_damage_component(
    damage_modifiable: &mut dyn DamageModifiable,
    amount: String,
    damage_type: String,
    source: String,
) -> LuaResult<()> {
    let modifier: ModifierExpression = amount.parse()?;
    let damage_type = serde_plain::from_str(&damage_type)
        .map_err(|e| LuaError::RuntimeError(format!("Invalid damage type: {e}")))?;
    let source = parse_source(&source)?;

    let modifier = modifier
        .evaluate_without_variables()
        .map_err(|e| LuaError::RuntimeError(format!("Failed to evaluate damage amount: {e}")))?;

    damage_modifiable.add_damage_component(DamageComponent::new(
        ModifierMap::from(source, modifier),
        damage_type,
    ));

    Ok(())
}

impl UserData for DamageMitigationResult {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("total", |_, this| Ok(this.total));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method_mut("add_immunity", |_, this, source: String| {
            let source = parse_source(&source)?;
            for component in &mut this.components {
                component.modifiers.push(DamageMitigationEffect {
                    source: source.clone(),
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
                let bonus: ModifierExpression = bonus.parse()?;
                let mut new_result = this.reroll();
                let bonus_value = bonus.evaluate_without_variables().map_err(|e| {
                    LuaError::RuntimeError(format!("Failed to evaluate bonus expression: {e}"))
                })?;
                let source = parse_source(&source)?;
                new_result
                    .d20_result_mut()
                    .add_modifier(source, bonus_value);

                if force_use_new || new_result.d20_result().total() > this.d20_result().total() {
                    *this = new_result;
                }

                Ok(())
            },
        );

        methods.add_method_mut(
            "modify_result",
            |_, this, (bonus, source): (String, String)| {
                let bonus: ModifierExpression = bonus.parse()?;
                let bonus_value = bonus.evaluate_without_variables().map_err(|e| {
                    LuaError::RuntimeError(format!("Failed to evaluate bonus expression: {e}"))
                })?;
                let source = parse_source(&source)?;
                this.d20_result_mut().add_modifier(source, bonus_value);
                Ok(())
            },
        );
    }
}

// TODO: Should this not live somewhere else?
fn parse_source(source: &str) -> Result<ModifierSource, LuaError> {
    if source == "base" {
        return Ok(ModifierSource::Base);
    }
    if let Ok(ability) = source.parse::<Ability>() {
        return Ok(ModifierSource::Ability(ability));
    }
    let id: Id = source.parse().map_err(|e| {
        LuaError::RuntimeError(format!(
            "Failed to parse modifier source ID '{}': {e}",
            source
        ))
    })?;
    Ok(ModifierSource::from(id))
}

type ModifierTable = BTreeMap<ModifierSource, ModifierExpression>;

fn parse_modifier_table(table: Table) -> Result<ModifierTable, LuaError> {
    let mut map = BTreeMap::new();
    for pair in table.pairs::<String, String>() {
        let (key, value) = pair
            .map_err(|e| LuaError::RuntimeError(format!("Failed to parse modifier table: {e}")))?;
        let source = parse_source(&key)?;
        let modifier: ModifierExpression = value.parse()?;
        map.insert(source, modifier);
    }
    Ok(map)
}

impl Evaluable for ModifierTable {
    type Output = ModifierMap;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        action_context: &ActionContext,
        variables: &VariableMap,
    ) -> Result<ModifierMap, EvaluationError> {
        let mut map = ModifierMap::default();
        for (source, modifier) in self {
            let value = modifier.evaluate(world, entity, action_context, variables)?;
            map.add_modifier(source.clone(), value);
        }
        Ok(map)
    }
}

impl EvaluableWithoutVariables for ModifierTable {
    type Output = ModifierMap;

    fn evaluate_without_variables(&self) -> Result<ModifierMap, EvaluationError> {
        let mut map = ModifierMap::default();
        for (source, modifier) in self {
            let value = modifier.evaluate_without_variables()?;
            map.add_modifier(source.clone(), value);
        }
        Ok(map)
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

impl UserData for ModifierMap {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method_mut(
            "add_modifier",
            |_, this, (source, value): (String, String)| {
                let source = parse_source(&source)?;
                let modifier: ModifierExpression = value.parse()?;
                this.add_modifier(
                    source,
                    modifier.evaluate_without_variables().map_err(|e| {
                        LuaError::RuntimeError(format!(
                            "Failed to evaluate modifier expression: {e}"
                        ))
                    })?,
                );
                Ok(())
            },
        );

        methods.add_method("get_modifier", |_, this, source: String| {
            // TODO: Consider if we need to expose the actual modifier, or if the
            // string version is sufficient
            let source = parse_source(&source)?;
            Ok(this.get(&source).map(|modifier| modifier.to_string()))
        });
    }
}

impl UserData for FlatModifierMap {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("total", |_, this| Ok(this.total()));
    }
}

impl UserData for ModifierResult {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("total", |_, this| Ok(this.total()));
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

impl UserData for DamageResult {
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

impl UserData for ActionResult {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("target", |_, this| Ok(ScriptEntity::from(this.target.id())));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("damage", |_, this, ()| {
            Ok(this
                .components()
                .iter()
                .filter_map(|component| match &component {
                    ActionResultComponent::Damage(damage_result) => Some(damage_result.clone()),
                    _ => None,
                })
                .collect::<Vec<_>>())
        });
        methods.add_method("resolution", |_, this, ()| Ok(this.resolution().clone()));
        methods.add_method("has_applied_effect", |_, this, effect_id: String| {
            let id = parse_id::<EffectId>(&effect_id)?;
            Ok(this.components().iter().any(|component| {
                matches!(component,
                    ActionResultComponent::Effect(EffectResult {
                        result: EffectResultKind::Applied,
                        effects,
                        ..
                    }) if effects.contains(&id))
            }))
        });
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
        methods.add_method("as_d20_check_performed", |_, this, ()| {
            let EventKind::D20CheckPerformed { actor, result, dc } = &this.kind else {
                return Ok((None, None, None));
            };
            Ok((
                Some(ScriptEntity::from(actor.id())),
                Some(result.clone()),
                Some(dc.clone()),
            ))
        });
        methods.add_method_mut("with_d20_check", |lua, this, callback: Function| {
            let EventKind::D20CheckPerformed { result, dc, .. } = &mut this.kind else {
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
        methods.add_method("as_action_requested", |_, this, ()| {
            let EventKind::ActionRequested { action } = &this.kind else {
                return Ok(None);
            };
            Ok(Some(action.clone()))
        });
        methods.add_method("as_action_result", |_, this, ()| {
            let EventKind::ActionResult { result, actor } = &this.kind else {
                return Ok((None, None));
            };
            Ok((
                Some(result.clone()),
                actor.as_ref().map(|p| ScriptEntity::from(p.id())),
            ))
        });
        methods.add_method("as_moving_out_of_reach", |_, this, ()| {
            let EventKind::MovingOutOfReach { mover, entity } = &this.kind else {
                return Ok((None, None));
            };
            Ok((
                Some(ScriptEntity::from(mover.id())),
                Some(ScriptEntity::from(entity.id())),
            ))
        });
        methods.add_method("as_equipment_changed", |_, this, ()| {
            let EventKind::EquipmentChanged {
                entity,
                item,
                equipped,
            } = &this.kind
            else {
                return Ok((None, None, None, None));
            };

            // TODO: Lowercase when we stringify enums?
            let armor_type = match ItemsRegistry::get(item) {
                Some(ItemInstance::Armor(armor)) => Some(armor.armor_type.to_string()),
                _ => None,
            };
            Ok((
                Some(ScriptEntity::from(entity.id())),
                Some(item.to_string()),
                armor_type,
                Some(*equipped),
            ))
        });
    }
}

impl UserData for GameState {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method(
            "class_level",
            |_, this, (entity, class): (ScriptEntity, String)| {
                let class: ClassId = class.parse().map_err(|e| {
                    LuaError::RuntimeError(format!("Failed to parse class name: {e}"))
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
                    .map_err(|e| LuaError::RuntimeError(format!("Invalid ability name: {e}")))?;
                let scores =
                    systems::helpers::get_component::<AbilityScoreMap>(&this.world, entity.into());
                Ok(scores.ability_modifier(&ability))
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
                    LuaError::RuntimeError(format!("Failed to parse WeaponKind: {e}"))
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
                Value,
            )| {
                apply_effect_impl(
                    this,
                    applier,
                    target,
                    parse_id::<EffectId>(&effect_id)?,
                    None,
                    false,
                    parse_id::<EffectId>(&source_effect)?,
                    get_type_from_value(context)?,
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
                Value,
                Value,
            )| {
                if turns <= 0 {
                    return Err(LuaError::RuntimeError(
                        "turns must be greater than 0".into(),
                    ));
                }
                let resolution = get_type_from_value(resolution)?;
                apply_effect_impl(
                    this,
                    applier,
                    target,
                    parse_id::<EffectId>(&effect_id)?,
                    Some(turns as u32),
                    one_shot,
                    parse_id::<EffectId>(&source_effect)?,
                    get_type_from_value(context)?,
                    resolution.unwrap_or(ActionConditionResolution::Unconditional),
                );
                Ok(())
            },
        );

        methods.add_method(
            "has_effect",
            |_, this, (target, effect_id): (ScriptEntity, String)| {
                let id = parse_id::<EffectId>(&effect_id)?;
                Ok(systems::effects::has_effect(this, target.into(), &id))
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
            |_, this, (target, amount): (ScriptEntity, Table)| {
                let target: Entity = target.into();

                let healing_table = parse_modifier_table(amount)?;

                let healing_map = healing_table
                    .evaluate(
                        &this.world,
                        target,
                        &ActionContext::default(),
                        &PARSER_VARIABLES,
                    )
                    .map_err(|e| {
                        LuaError::RuntimeError(format!("Failed to evaluate healing amount: {e}"))
                    })?;

                let healing = healing_map.evaluate();

                let new_life_state =
                    systems::health::heal(&mut this.world, target, healing.total() as u32);

                let event = Event::action_result_event(
                    EntityIdentifier::from_world(&this.world, target),
                    ActionResultComponent::Healing(HealingResult {
                        healing,
                        new_life_state,
                    }),
                );

                this.process_event(event);

                Ok(())
            },
        );
    }
}

fn get_type_from_value<T: UserData + Clone + 'static>(value: Value) -> LuaResult<Option<T>> {
    match value {
        Value::Nil => Ok(None),
        Value::UserData(ud) => {
            let ctx = ud.borrow::<T>()?;
            Ok(Some(ctx.clone()))
        }
        other => Err(LuaError::RuntimeError(format!(
            "Expected {} or nil, got {}",
            std::any::type_name::<T>(),
            other.type_name()
        ))),
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
    context: Option<ActionContext>,
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

    game_state.process_event(Event::action_result_event(
        EntityIdentifier::from_world(&game_state.world, target_entity),
        ActionResultComponent::Effect(EffectResult {
            resolution: resolution.clone(),
            effects: systems::effects::effect_id_and_children(&effect_id),
            result: EffectResultKind::Applied,
        }),
    ));

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
        context.as_ref(),
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
                    LuaError::RuntimeError(format!("Failed to parse ResourceAmount: {e}"))
                })?;
                this.replace_resource(&from, &to, &amount);
                Ok(())
            },
        );
    }
}
