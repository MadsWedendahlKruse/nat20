use rhai::{Array, CustomType, TypeBuilder, plugin::*};

use crate::{
    components::id::ResourceId,
    scripts::script_api::{
        ScriptActionContext, ScriptActionKindResultView, ScriptActionOutcomeBundleView,
        ScriptActionPerformedView, ScriptActionResultView, ScriptActionView, ScriptD20CheckDCKind,
        ScriptD20CheckView, ScriptD20Result, ScriptDamageMitigationResult, ScriptDamageOutcomeView,
        ScriptDamageResolutionKindView, ScriptDamageRollResult, ScriptEffectView, ScriptEntity,
        ScriptEntityView, ScriptEventRef, ScriptEventView, ScriptLoadoutView,
        ScriptMovingOutOfReachView, ScriptOptionalEntityView, ScriptReactionBodyContext,
        ScriptReactionPlan, ScriptReactionTriggerContext, ScriptResourceCost, ScriptResourceView,
        ScriptSavingThrow,
    },
};

impl CustomType for ScriptEntity {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("Entity")
            .with_get("id", |s: &mut Self| s.id);
    }
}

impl CustomType for ScriptEffectView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("EffectView")
            .with_fn("has_applier", |s: &mut Self| s.has_applier())
            .with_fn("get_applier", |s: &mut Self| s.get_applier().id);
    }
}

impl CustomType for ScriptDamageRollResult {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("DamageRollResult")
            .with_get("source", |s: &mut Self| s.source())
            .with_fn("clamp_damage_dice_min", |s: &mut Self, min: i64| {
                s.clamp_damage_dice_min(min as u32);
            })
            .with_fn("has_actor", |s: &mut Self| s.has_actor())
            .with_fn("get_actor", |s: &mut Self| s.get_actor().id)
            .with_fn("is_action_attack_roll", |s: &mut Self| {
                s.is_action_attack_roll()
            })
            .with_fn("is_action_saving_throw", |s: &mut Self| {
                s.is_action_saving_throw()
            })
            .with_fn("is_action_unconditional", |s: &mut Self| {
                s.is_action_unconditional()
            })
            .with_fn(
                "add_damage",
                |s: &mut Self, amount: String, damage_type: String| {
                    s.add_damage(
                        amount.parse().unwrap(),
                        serde_plain::from_str(&damage_type).unwrap(),
                    );
                },
            );
    }
}

impl CustomType for ScriptDamageMitigationResult {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("DamageMitigationResult")
            .with_get("source", |s: &mut Self| s.source())
            .with_fn("add_immunity", |s: &mut Self| s.add_immunity());
    }
}

impl CustomType for ScriptD20CheckDCKind {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("D20CheckDCKind")
            .with_get("label", |s: &mut Self| s.label.clone())
            .with_get("dc", |s: &mut Self| s.dc.clone())
            .with_get("target", |s: &mut Self| {
                if let Some(target) = &s.target {
                    target.id
                } else {
                    0
                }
            });
    }
}

impl CustomType for ScriptD20Result {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("D20Result")
            .with_get("total", |s: &mut Self| s.total)
            .with_get("dc_kind", |s: &mut Self| s.dc_kind.clone())
            .with_get("is_success", |s: &mut Self| s.is_success);
    }
}

impl CustomType for ScriptD20CheckView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("D20CheckPerformedView")
            .with_get("performer", |s: &mut Self| s.performer.clone())
            .with_get("result", |s: &mut Self| s.result.clone());
    }
}

impl CustomType for ScriptMovingOutOfReachView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("MovingOutOfReachView")
            .with_get("mover", |s: &mut Self| s.mover.id)
            .with_get("entity", |s: &mut Self| s.entity.id)
            .with_get("continue_movement", |s: &mut Self| s.continue_movement);
    }
}

impl CustomType for ScriptSavingThrow {
    fn build(mut builder: TypeBuilder<Self>) {
        builder.with_name("SavingThrow");
    }
}

#[export_module]
pub mod saving_throw_module {
    use super::*;

    pub fn dc(entity_role: String, saving_throw: String) -> ScriptSavingThrow {
        let role = entity_role
            .parse()
            .expect(format!("Failed to parse ScriptEntityRole: {}", entity_role).as_str());

        ScriptSavingThrow {
            entity: role,
            saving_throw: saving_throw
                .parse()
                .expect("Failed to parse SavingThrowProvider"),
        }
    }
}

impl CustomType for ScriptActionContext {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ActionContext")
            .with_fn("is_spell", |s: &mut Self| s.is_spell())
            .with_fn("is_weapon_attack", |s: &mut Self| s.is_weapon_attack());
    }
}

impl CustomType for ScriptResourceCost {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ResourceCost")
            .with_fn("costs_resource", |s: &mut Self, resource_id: String| {
                s.costs_resource(&resource_id.parse().expect("Failed to parse ResourceId"))
            })
            .with_fn(
                "replace_resource",
                |s: &mut Self, from: String, to: String, new_amount: String| {
                    s.replace_resource(
                        &from.parse().expect("Failed to parse ResourceId"),
                        &to.parse().expect("Failed to parse ResourceId"),
                        serde_plain::from_str(&new_amount).expect("Failed to parse ResourceAmount"),
                    )
                },
            );
    }
}

impl CustomType for ScriptActionView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ActionView")
            .with_get("action_id", |s: &mut Self| s.action_id.clone())
            // Expose the actor as a numeric entity id
            .with_get("actor", |s: &mut Self| u64::from(s.actor.to_bits()))
            .with_get("action_context", |s: &mut Self| s.action_context.clone())
            .with_get("resource_cost", |s: &mut Self| s.resource_cost.clone())
            .with_fn("is_targetting_entity", |s: &mut Self, entity_id: u64| {
                s.targets.iter().any(|t| t.id == entity_id)
            });
    }
}

impl CustomType for ScriptDamageResolutionKindView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("DamageResolutionKindView")
            .with_fn("is_unconditional", |s: &mut Self| s.is_unconditional())
            .with_fn("is_attack_roll", |s: &mut Self| s.is_attack_roll())
            .with_fn("is_saving_throw", |s: &mut Self| s.is_saving_throw());
    }
}

impl CustomType for ScriptDamageOutcomeView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("DamageOutcomeView")
            .with_fn("has_damage_roll", |s: &mut Self| s.has_damage_roll())
            .with_fn("get_damage_roll", |s: &mut Self| {
                s.get_damage_roll().clone()
            })
            .with_fn("has_damage_taken", |s: &mut Self| s.has_damage_taken())
            .with_fn("get_damage_taken", |s: &mut Self| {
                s.get_damage_taken().clone()
            })
            .with_fn("damage_roll_total", |s: &mut Self| {
                s.damage_roll_total() as i64
            })
            .with_fn("damage_taken_total", |s: &mut Self| {
                s.damage_taken_total() as i64
            });
    }
}

impl CustomType for ScriptActionOutcomeBundleView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ActionOutcomeBundleView")
            .with_fn("has_damage", |s: &mut Self| s.has_damage())
            .with_fn("get_damage", |s: &mut Self| s.get_damage().clone());
    }
}

impl CustomType for ScriptActionKindResultView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ActionKindResultView")
            .with_fn("is_standard", |s: &mut Self| s.is_standard())
            .with_fn("as_standard", |s: &mut Self| s.as_standard().clone());
    }
}

impl CustomType for ScriptActionResultView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ActionKindResultView")
            .with_get("performer", |s: &mut Self| s.performer.clone())
            .with_get("target", |s: &mut Self| s.target.id)
            .with_get("kind", |s: &mut Self| s.kind.clone());
    }
}

impl CustomType for ScriptActionPerformedView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ActionPerformedView")
            .with_get("action", |s: &mut Self| s.action.clone())
            .with_fn("results", |s: &mut Self| {
                s.results()
                    .iter()
                    .map(|value| Dynamic::from(value.clone()))
                    .collect::<Vec<_>>()
            });
    }
}

impl CustomType for ScriptEventView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("EventView")
            .with_fn("is_d20_check_performed", |s: &mut Self| {
                s.is_d20_check_performed()
            })
            .with_fn("as_d20_check_performed", |s: &mut Self| {
                s.as_d20_check_performed().clone()
            })
            .with_fn("is_action_requested", |s: &mut Self| {
                s.is_action_requested()
            })
            .with_fn("as_action_requested", |s: &mut Self| {
                s.as_action_requested().clone()
            })
            .with_fn("is_action_performed", |s: &mut Self| {
                s.is_action_performed()
            })
            .with_fn("as_action_performed", |s: &mut Self| {
                s.as_action_performed().clone()
            })
            .with_fn("is_moving_out_of_reach", |s: &mut Self| {
                s.is_moving_out_of_reach()
            })
            .with_fn("as_moving_out_of_reach", |s: &mut Self| {
                s.as_moving_out_of_reach().clone()
            });
    }
}

impl CustomType for ScriptReactionPlan {
    fn build(mut builder: TypeBuilder<Self>) {
        builder.with_name("ReactionPlan");
    }
}

#[export_module]
pub mod reaction_plan_module {
    use super::*;

    pub fn none() -> ScriptReactionPlan {
        ScriptReactionPlan::None
    }

    pub fn sequence(plans: Array) -> ScriptReactionPlan {
        let inner_plans: Vec<ScriptReactionPlan> = plans
            .into_iter()
            .map(|v| v.cast::<ScriptReactionPlan>())
            .collect();
        ScriptReactionPlan::Sequence(inner_plans)
    }

    pub fn modify_d20_result(bonus: String) -> ScriptReactionPlan {
        ScriptReactionPlan::ModifyD20Result {
            bonus: bonus.parse().unwrap(),
        }
    }

    pub fn modify_d20_dc(modifier: String) -> ScriptReactionPlan {
        ScriptReactionPlan::ModifyD20DC {
            modifier: modifier.parse().unwrap(),
        }
    }

    pub fn reroll_d20_result(bonus: String, force_use_new: bool) -> ScriptReactionPlan {
        let bonus = if bonus.is_empty() {
            None
        } else {
            bonus.parse().ok()
        };
        ScriptReactionPlan::RerollD20Result {
            bonus,
            force_use_new,
        }
    }

    pub fn require_saving_throw(
        target_role: ImmutableString,
        dc: ScriptSavingThrow,
        on_success: ScriptReactionPlan,
        on_failure: ScriptReactionPlan,
    ) -> ScriptReactionPlan {
        let target = target_role
            .parse()
            .expect(format!("Failed to parse ScriptEntityRole: {}", target_role).as_str());

        ScriptReactionPlan::RequireSavingThrow {
            target,
            dc,
            on_success: Box::new(on_success),
            on_failure: Box::new(on_failure),
        }
    }

    pub fn cancel_trigger_event(resources_to_refund: Array) -> ScriptReactionPlan {
        let resources: Vec<ResourceId> = resources_to_refund
            .into_iter()
            .map(|v| {
                v.cast::<String>()
                    .parse()
                    .expect("Failed to parse ResourceId")
            })
            .collect();

        ScriptReactionPlan::CancelEvent {
            event: ScriptEventRef::TriggerEvent,
            resources_to_refund: resources,
        }
    }
}

impl CustomType for ScriptLoadoutView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("LoadoutView")
            .with_get("armor_type", |s: &mut Self| match &s.loadout.armor() {
                Some(armor) => armor.armor_type.to_string(),
                None => "None".to_string(),
            })
            .with_fn(
                "wielding_with_both_hands",
                |s: &mut Self, weapon_kind: String| {
                    let kind = serde_plain::from_str(&weapon_kind.to_lowercase())
                        .expect("Failed to parse WeaponKind");
                    s.loadout.is_wielding_weapon_with_both_hands(&kind)
                },
            );
    }
}

impl CustomType for ScriptResourceView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ResourceView")
            .with_fn(
                "can_afford_resource",
                |s: &mut Self, resource_id: String, amount: String| {
                    s.can_afford_resource(
                        &resource_id.parse().expect("Failed to parse ResourceId"),
                        &serde_plain::from_str(&amount).expect("Failed to parse ResourceAmount"),
                    )
                },
            )
            .with_fn(
                "add_resource",
                |s: &mut Self, resource_id: String, amount: Dynamic| {
                    let amount = if amount.is::<String>() {
                        amount.cast::<String>()
                    } else if amount.is::<i64>() {
                        amount.cast::<i64>().to_string()
                    } else {
                        panic!("Unexpected type for amount: {:?}", amount.type_name());
                    };
                    s.add_resource(
                        &resource_id.parse().expect("Failed to parse ResourceId"),
                        &serde_plain::from_str(&amount).expect("Failed to parse ResourceAmount"),
                    )
                },
            );
    }
}

impl CustomType for ScriptEntityView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("EntityView")
            .with_get("entity", |s: &mut Self| s.entity.id)
            .with_get("loadout", |s: &mut Self| s.loadout.clone())
            .with_get_set(
                "resources",
                |s: &mut Self| s.resources.clone(),
                |s: &mut Self, v: ScriptResourceView| s.resources = v,
            );
    }
}

impl CustomType for ScriptOptionalEntityView {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("OptionalEntityView")
            .with_get("is_some", |s: &mut Self| s.is_some())
            .with_fn("get", |s: &mut Self| s.get().clone());
    }
}

impl CustomType for ScriptReactionTriggerContext {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ReactionTriggerContext")
            .with_get("reactor", |s: &mut Self| s.reactor.id)
            .with_get("event", |s: &mut Self| s.event.clone())
            .with_fn(
                "is_own_failed_d20_check",
                |s: &mut Self, dc_kind: String| {
                    if !s.event.is_d20_check_performed() {
                        return false;
                    }
                    let d20_check = s.event.as_d20_check_performed();
                    return d20_check.performer == s.reactor
                        && !d20_check.result.is_success
                        && d20_check.result.dc_kind.label == dc_kind;
                },
            );
    }
}

impl CustomType for ScriptReactionBodyContext {
    fn build(mut builder: TypeBuilder<Self>) {
        builder
            .with_name("ReactionBodyContext")
            .with_get("reactor", |s: &mut Self| s.reactor.id)
            .with_get("event", |s: &mut Self| s.event.clone());
    }
}
