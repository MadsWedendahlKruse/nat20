use serde::{Deserialize, de::Deserializer};

use crate::{
    components::id::ScriptId,
    registry::registry_reference::{self, RegistryReference},
    scripts::script::ScriptFunction,
};

macro_rules! script_requirements {
    ($($name:ident => [$($function:ident),+],)*) => {$(
        pub fn $name<'de, D: Deserializer<'de>>(deserializer: D) -> Result<ScriptId, D::Error> {
            let id = ScriptId::deserialize(deserializer)?;
            registry_reference::record_reference(|| RegistryReference::Script(
                id.clone(),
                vec![$(ScriptFunction::$function),+],
            ));
            Ok(id)
        }
    )*};
}

script_requirements! {
    action_hook => [ActionHook],
    action_result_hook => [ActionResultHook],
    action_usability => [ActionUsability],
    action_usability_hook => [ActionUsabilityHook],
    armor_class_hook => [ArmorClassHook],
    attacked_hook => [AttackedHook],
    d20_hook => [D20AbilityHook, D20CheckHook, D20CheckResultHook],
    damage_roll_hook => [DamageRollHook],
    damage_roll_result_hook => [DamageRollResultHook],
    death_hook => [DeathHook],
    effect_lifetime_hook => [EffectLifetimeHook],
    event_filter => [EventFilter],
    post_damage_mitigation_hook => [PostDamageMitigationHook],
    pre_damage_mitigation_hook => [PreDamageMitigationHook],
    pre_death_hook => [PreDeathHook],
    reaction_body => [ReactionBody],
    reaction_trigger => [ReactionTrigger],
    resource_cost_hook => [ResourceCostHook],
    rest_hook => [RestHook],
    speed_hook => [SpeedHook],
    target_usability => [TargetUsability],
    turn_start_hook => [TurnStartHook],
}
