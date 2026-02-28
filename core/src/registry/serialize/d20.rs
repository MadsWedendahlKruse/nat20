use std::{fmt::Display, str::FromStr, sync::Arc};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        actions::action::{
            ActionAttackKind, ActionContext, AttackRollFunction, AttackRollProvider,
            SavingThrowFunction, SavingThrowProvider,
        },
        saving_throw::SavingThrowKind,
        spells::spellbook::Spellbook,
    },
    systems,
};

#[derive(Clone, Copy)]
enum AttackRollScope {
    MeleeWeapon,
    RangedWeapon,
    Unarmed,
    Melee,
}

impl AttackRollScope {
    fn is_allowed(&self, kind: ActionAttackKind) -> bool {
        match self {
            AttackRollScope::MeleeWeapon => matches!(kind, ActionAttackKind::MeleeWeapon),
            AttackRollScope::RangedWeapon => matches!(kind, ActionAttackKind::RangedWeapon),
            AttackRollScope::Unarmed => matches!(kind, ActionAttackKind::Unarmed),
            AttackRollScope::Melee => {
                matches!(kind, ActionAttackKind::MeleeWeapon | ActionAttackKind::Unarmed)
            }
        }
    }
}

fn scoped_attack_roll(scope: AttackRollScope, provider_name: &'static str) -> Arc<AttackRollFunction> {
    Arc::new(
        move |world: &World, entity: Entity, target: Entity, action_context: &ActionContext| {
            let attack = action_context
                .attack
                .as_ref()
                .unwrap_or_else(|| panic!("{provider_name} requires an attack context"));
            if !scope.is_allowed(attack.kind) {
                panic!("{provider_name} does not support this attack context");
            }
            systems::loadout::loadout(world, entity).attack_roll(world, entity, target, action_context)
        },
    ) as Arc<AttackRollFunction>
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct AttackRollDefinition {
    pub raw: String,
    pub function: Arc<AttackRollFunction>,
}

impl Display for AttackRollDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl FromStr for AttackRollDefinition {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let function = match s {
            "melee_weapon_attack_roll" => {
                scoped_attack_roll(AttackRollScope::MeleeWeapon, "melee_weapon_attack_roll")
            }
            "melee_attack_roll" => scoped_attack_roll(AttackRollScope::Melee, "melee_attack_roll"),
            "ranged_attack_roll" => {
                scoped_attack_roll(AttackRollScope::RangedWeapon, "ranged_attack_roll")
            }
            "unarmed_attack_roll" => {
                scoped_attack_roll(AttackRollScope::Unarmed, "unarmed_attack_roll")
            }
            "spell_attack_roll" => Arc::new({
                |world: &World, entity: Entity, target: Entity, action_context: &ActionContext| {
                    systems::helpers::get_component::<Spellbook>(world, entity).attack_roll(
                        world,
                        entity,
                        target,
                        action_context,
                    )
                }
            }) as Arc<AttackRollFunction>,
            _ => {
                return Err(format!("Unknown AttackRollProvider: {}", s));
            }
        };

        Ok(Self {
            raw: s.to_string(),
            function,
        })
    }
}

impl TryFrom<String> for AttackRollDefinition {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<AttackRollDefinition> for String {
    fn from(equation: AttackRollDefinition) -> Self {
        equation.raw
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct SavingThrowDefinition {
    pub raw: String,
    pub function: Arc<SavingThrowFunction>,
}

impl Display for SavingThrowDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl FromStr for SavingThrowDefinition {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Example format: "spell_save_dc;dexterity"

        let parts: Vec<&str> = s.split(';').collect();
        if parts.len() != 2 {
            return Err(format!("Invalid SavingThrowDefinition format: {}", s));
        }

        let kind: SavingThrowKind = serde_plain::from_str(parts[1]).unwrap();

        let function =
            match parts[0] {
                "weapon_save_dc" => {
                    Arc::new({
                        move |world: &World, entity: Entity, action_context: &ActionContext| {
                            systems::loadout::loadout(world, entity).saving_throw(
                                world,
                                entity,
                                action_context,
                                kind,
                            )
                        }
                    }) as Arc<SavingThrowFunction>
                }
                "spell_save_dc" => {
                    Arc::new({
                        move |world: &World, entity: Entity, action_context: &ActionContext| {
                            systems::helpers::get_component::<Spellbook>(world, entity)
                                .saving_throw(world, entity, action_context, kind)
                        }
                    }) as Arc<SavingThrowFunction>
                }
                _ => {
                    return Err(format!("Unknown SavingThrowDefinition: {}", s));
                }
            };

        Ok(Self {
            raw: s.to_string(),
            function,
        })
    }
}

impl TryFrom<String> for SavingThrowDefinition {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<SavingThrowDefinition> for String {
    fn from(equation: SavingThrowDefinition) -> Self {
        equation.raw
    }
}
