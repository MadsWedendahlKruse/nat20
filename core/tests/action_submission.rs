//! When submitting an action you're supposed to use the `ActionBuilder`, which
//! takes care of only allowing valid actions to be submitted, but in reality
//! there's nothing stopping you from manually constructing `ActionData` and
//! submitting it directly, in which case we should make sure you're not suddenly
//! able to submit something illegal.

use std::collections::HashSet;

use nat20_core::{
    components::{
        actions::{
            action::ActionContext,
            targeting::{EntityFilter, TargetFilter, TargetInstance, TargetingError},
        },
        activity::ActivityError,
        class::ClassAndSubclass,
        d20::{D20CheckKind, D20CheckOutcome},
        damage::AttackSource,
        health::life_state::LifeState,
        items::equipment::{slots::EquipmentSlot, weapon::WeaponKind},
        resource::{RechargeRule, ResourceAmount, ResourceAmountMap},
        spells::{concentration::ConcentrationError, spellbook::SpellSource},
        time::TurnBoundary,
    },
    engine::{
        action_prompt::{ActionData, ActionDecisionKind, ActionError},
        event::{Event, EventKind},
    },
    systems::{
        actions::{ActionUsabilityError, ReactionUsabilityError, VariantUsabilityError},
        time::RestKind,
    },
    test_utils::scenario::Scenario,
};
use rstest::{fixture, rstest};

#[fixture]
fn scenario() -> Scenario {
    let mut scenario = Scenario::new();

    // Spawn some random entities so we have something to chose form
    scenario
        .spawn("barbarian", "hero.barbarian")
        .level(9)
        .position([0.0, 0.0, 0.0], true)
        .spawn();
    scenario
        .spawn("goblin", "monster.goblin_warrior")
        // Higher level so he doesn't get one-shot
        .level(5)
        .position([1.5, 0.0, 0.0], true)
        .spawn();
    scenario
        .spawn("wizard", "hero.wizard")
        .level(5)
        .position([0.0, 0.0, 1.5], true)
        .spawn();

    scenario
}

#[rstest]
fn known_action(mut scenario: Scenario) {
    // Barbarian should be allowed to make a melee attack with their equipped weapon
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert!(result.is_ok(), "expected Ok(()), got {:?}", result.err());
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.melee_attack")
        .assert_event();
}

#[rstest]
fn known_action_invalid_resource(mut scenario: Scenario) {
    // Melee attacks normally cost an action, so it shouldn't be allowed to do it
    // with a bonus action instead
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![(
            "resource.bonus_action".into(),
            ResourceAmount::Flat(1),
        )]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActionNotKnown {
                action_id: Some("action.melee_attack".into()),
                context: Some(ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand)),
                resource_cost: Some(ResourceAmountMap::from_iter(vec![(
                    "resource.bonus_action".into(),
                    ResourceAmount::Flat(1),
                )]),)
            }
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.melee_attack")
        .assert_event_count(0);
}

#[rstest]
fn known_action_invalid_context(mut scenario: Scenario) {
    // Attempt to perform a known action with an invalid context
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::empty(),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActionNotKnown {
                action_id: Some("action.melee_attack".into()),
                context: Some(ActionContext::empty()),
                resource_cost: Some(ResourceAmountMap::from_iter(vec![(
                    "resource.action".into(),
                    ResourceAmount::Flat(1)
                )])),
            }
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.melee_attack")
        .assert_event_count(0);
}

#[rstest]
fn unknown_action(mut scenario: Scenario) {
    // Barbarian does not have a ranged weapon equipped
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.ranged_attack".into(),
        ActionContext::ranged_weapon(EquipmentSlot::RangedMainHand),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActionNotKnown {
                action_id: Some("action.ranged_attack".into()),
                context: None,
                resource_cost: None,
            }
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.ranged_attack")
        .assert_event_count(0);
}

#[rstest]
fn nonexistent_action(mut scenario: Scenario) {
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.nonexistent".into(),
        ActionContext::empty(),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActionDoesNotExist("action.nonexistent".into())
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.nonexistent")
        .assert_event_count(0);
}

#[rstest]
fn known_cantrip(mut scenario: Scenario) {
    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.fire_bolt".into(),
        ActionContext::spell(
            "spell.fire_bolt".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            0,
        ),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert!(result.is_ok(), "expected Ok(()), got {:?}", result.err());
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.fire_bolt")
        .assert_event_count(1);
}

#[rstest]
fn known_cantrip_leveled(mut scenario: Scenario) {
    // Cantrips cannot be upcast
    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.fire_bolt".into(),
        ActionContext::spell(
            "spell.fire_bolt".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            1,
        ),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActionNotKnown {
                action_id: Some("action.fire_bolt".into()),
                context: Some(ActionContext::spell(
                    "spell.fire_bolt".into(),
                    SpellSource::Class(ClassAndSubclass {
                        class: "class.wizard".into(),
                        subclass: None,
                    }),
                    1,
                ),),
                resource_cost: Some(ResourceAmountMap::from_iter(vec![(
                    "resource.action".into(),
                    ResourceAmount::Flat(1)
                )])),
            }
        )))
    );
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.fire_bolt")
        .assert_event_count(0);
}

#[rstest]
fn unknown_cantrip(mut scenario: Scenario) {
    // Wizard doesn't know Eldritch Blast
    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.eldritch_blast".into(),
        ActionContext::spell(
            "spell.eldritch_blast".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            0,
        ),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActionNotKnown {
                action_id: Some("action.eldritch_blast".into()),
                context: None,
                resource_cost: None,
            }
        )))
    );
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.eldritch_blast")
        .assert_event_count(0);
}

#[rstest]
fn unknown_spell_own_class(mut scenario: Scenario) {
    // Ray of Sickness is a Wizard spell, but the wizard doesn't know it
    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.ray_of_sickness".into(),
        ActionContext::spell(
            "spell.ray_of_sickness".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            1,
        ),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActionNotKnown {
                action_id: Some("action.ray_of_sickness".into()),
                context: None,
                resource_cost: None,
            }
        )))
    );
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.ray_of_sickness")
        .assert_event_count(0);
}

#[rstest]
fn action_other_class(mut scenario: Scenario) {
    // Barbarian doesn't know Action Surge
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.fighter.action_surge".into(),
        ActionContext::empty(),
        ResourceAmountMap::from_iter(vec![(
            "resource.action_surge".into(),
            ResourceAmount::Flat(1),
        )]),
        vec![TargetInstance::entity(
            scenario.entity_identifier("barbarian"),
        )],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActionNotKnown {
                action_id: Some("action.fighter.action_surge".into()),
                context: None,
                resource_cost: None,
            }
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.fighter.action_surge")
        .assert_event_count(0);
}

#[rstest]
fn reaction_as_action(mut scenario: Scenario) {
    // Counterspell cannot be used as an action
    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.counterspell".into(),
        ActionContext::spell(
            "spell.counterspell".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            3,
        ),
        ResourceAmountMap::from_iter(vec![
            ("resource.reaction".into(), ResourceAmount::Flat(1)),
            (
                "resource.spell_slot".into(),
                ResourceAmount::Tiered { tier: 3, amount: 1 },
            ),
        ]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert!(matches!(
        result,
        Err(ActivityError::ActionError(ActionError::FieldMismatch {
            field: "action.is_reaction",
            ..
        }))
    ));

    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.counterspell")
        .assert_event_count(0);
}

#[rstest]
fn reaction_no_trigger_event(mut scenario: Scenario) {
    // Counterspell cannot be used without a trigger event in the `ActionData`
    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.counterspell".into(),
        ActionContext::spell(
            "spell.counterspell".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            3,
        ),
        ResourceAmountMap::from_iter(vec![
            ("resource.reaction".into(), ResourceAmount::Flat(1)),
            (
                "resource.spell_slot".into(),
                ResourceAmount::Tiered { tier: 3, amount: 1 },
            ),
        ]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Reaction {
        // Make a random event just to have something in the `event` field
        event: Event::new(EventKind::TurnBoundary {
            entity: action.actor.clone(),
            boundary: TurnBoundary::End,
        }),
        reactor: action.actor.id(),
        choice: Some(action),
    });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ReactionError(ReactionUsabilityError::NoTriggerEvent)
        )))
    );

    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.counterspell")
        .assert_event_count(0);
}

#[rstest]
fn reaction_no_pending_event(mut scenario: Scenario) {
    // A missed Strength attack opens a Reckless Attack reaction window, which
    // would leave a prompt pending, so make sure the priming attack lands
    scenario.probe("barbarian").d20_force_outcome(
        D20CheckKind::AttackRoll(AttackSource::Weapon(WeaponKind::Melee)),
        D20CheckOutcome::CriticalSuccess,
    );

    // Get the scenario's prompt scope going, and let it run dry again
    let melee_attack = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );
    scenario
        .submit_action_decision(ActionDecisionKind::Action {
            action: melee_attack,
        })
        .expect("melee attack should be submittable");

    // Counterspell has a trigger event, but there's nothing pending to react to
    let trigger = Event::new(EventKind::ActionRequested {
        action: ActionData::new(
            scenario.entity_identifier("goblin"),
            "action.melee_attack".into(),
            ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
            ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
            vec![TargetInstance::entity(scenario.entity_identifier("wizard"))],
        ),
    });

    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.counterspell".into(),
        ActionContext::spell(
            "spell.counterspell".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            3,
        ),
        ResourceAmountMap::from_iter(vec![
            ("resource.reaction".into(), ResourceAmount::Flat(1)),
            (
                "resource.spell_slot".into(),
                ResourceAmount::Tiered { tier: 3, amount: 1 },
            ),
        ]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    )
    .with_trigger_event(trigger);

    let result = scenario.submit_action_decision(ActionDecisionKind::Reaction {
        // Make a random event just to have something in the `event` field
        event: Event::new(EventKind::TurnBoundary {
            entity: action.actor.clone(),
            boundary: TurnBoundary::End,
        }),
        reactor: action.actor.id(),
        choice: Some(action),
    });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ReactionError(ReactionUsabilityError::NoPendingEvent)
        )))
    );
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.counterspell")
        .assert_event_count(0);
}

#[rstest]
fn dead_actor(mut scenario: Scenario) {
    // Dead men swing no axes
    scenario.probe("barbarian").kill();

    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::ActorNotAlive(scenario.entity("barbarian"))
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.melee_attack")
        .assert_event_count(0);
}

#[rstest]
fn action_on_cooldown(mut scenario: Scenario) {
    // Action Surge doesn't come back until the fighter has had a short rest
    scenario.spawn("fighter", "hero.fighter").level(5).spawn();

    fn action_surge(scenario: &Scenario) -> ActionData {
        ActionData::new(
            scenario.entity_identifier("fighter"),
            "action.fighter.action_surge".into(),
            ActionContext::empty(),
            ResourceAmountMap::from_iter(vec![(
                "resource.fighter.action_surge".into(),
                ResourceAmount::Flat(1),
            )]),
            vec![TargetInstance::entity(
                scenario.entity_identifier("fighter"),
            )],
        )
    }

    let action = action_surge(&scenario);
    let first = scenario.submit_action_decision(ActionDecisionKind::Action { action });
    assert!(first.is_ok(), "expected Ok(()), got {:?}", first.err());

    let action = action_surge(&scenario);
    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::OnCooldown(RechargeRule::Rest(RestKind::Short))
        )))
    );
    scenario
        .event_filter()
        .actor("fighter")
        .action_requested("action.fighter.action_surge")
        .assert_event_count(1);
}

#[rstest]
fn not_enough_resources(mut scenario: Scenario) {
    // You only get the one action per turn
    fn fire_bolt(scenario: &Scenario) -> ActionData {
        ActionData::new(
            scenario.entity_identifier("wizard"),
            "action.fire_bolt".into(),
            ActionContext::spell(
                "spell.fire_bolt".into(),
                SpellSource::Class(ClassAndSubclass {
                    class: "class.wizard".into(),
                    subclass: None,
                }),
                0,
            ),
            ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
            vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
        )
    }

    let action = fire_bolt(&scenario);
    let first = scenario.submit_action_decision(ActionDecisionKind::Action { action });
    assert!(first.is_ok(), "expected Ok(()), got {:?}", first.err());

    let action = fire_bolt(&scenario);
    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::NotEnoughResources(vec!["resource.action".into()])
        )))
    );
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.fire_bolt")
        .assert_event_count(1);
}

#[rstest]
fn no_targets(mut scenario: Scenario) {
    // Swinging at thin air
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        Vec::new(),
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::TargetingError(TargetingError::NoTargetsProvided)
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.melee_attack")
        .assert_event_count(0);
}

#[rstest]
fn too_many_targets(mut scenario: Scenario) {
    // A melee attack only hits one creature, no matter how many you name
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![
            TargetInstance::entity(scenario.entity_identifier("goblin")),
            TargetInstance::entity(scenario.entity_identifier("goblin")),
        ],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::TargetingError(TargetingError::ExceedsMaxTargets)
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.melee_attack")
        .assert_event_count(0);
}

#[rstest]
fn target_out_of_range(mut scenario: Scenario) {
    scenario
        .spawn("distant_goblin", "monster.goblin_warrior")
        .level(1)
        .position([30.0, 0.0, 0.0], true)
        .spawn();

    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(
            scenario.entity_identifier("distant_goblin"),
        )],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    // The exact distance falls out of the geometry, so only the target is pinned
    assert!(
        matches!(
            &result,
            Err(ActivityError::ActionError(ActionError::Usability(
                ActionUsabilityError::TargetingError(TargetingError::OutOfRange { target, .. })
            ))) if target == &TargetInstance::entity(scenario.entity_identifier("distant_goblin"))
        ),
        "expected OutOfRange, got {:?}",
        result
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.melee_attack")
        .assert_event_count(0);
}

#[rstest]
fn target_not_self(mut scenario: Scenario) {
    // Rage is self-targeted, you can't make somebody else angry
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.barbarian.rage".into(),
        ActionContext::empty(),
        ResourceAmountMap::from_iter(vec![
            ("resource.bonus_action".into(), ResourceAmount::Flat(1)),
            ("resource.barbarian.rage".into(), ResourceAmount::Flat(1)),
        ]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::TargetingError(TargetingError::NotSelf {
                target: TargetInstance::entity(scenario.entity_identifier("goblin")),
                actor: scenario.entity_identifier("barbarian"),
            })
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.barbarian.rage")
        .assert_event_count(0);
}

#[rstest]
fn invalid_target(mut scenario: Scenario) {
    // Fire Bolt doesn't target the dead
    scenario.probe("goblin").kill();

    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.fire_bolt".into(),
        ActionContext::spell(
            "spell.fire_bolt".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            0,
        ),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::TargetingError(TargetingError::InvalidTarget {
                target: TargetInstance::entity(scenario.entity_identifier("goblin")),
                violated_filters: vec![TargetFilter::Entity(EntityFilter::NotLifeStates(
                    HashSet::from([LifeState::Dead])
                ))],
            })
        )))
    );
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.fire_bolt")
        .assert_event_count(0);
}

#[rstest]
fn action_usability_script(mut scenario: Scenario) {
    // Rage's own usability script refuses to stack a second Rage
    scenario
        .probe("barbarian")
        .apply_effect("effect.barbarian.rage");

    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.barbarian.rage".into(),
        ActionContext::empty(),
        ResourceAmountMap::from_iter(vec![
            ("resource.bonus_action".into(), ResourceAmount::Flat(1)),
            ("resource.barbarian.rage".into(), ResourceAmount::Flat(1)),
        ]),
        vec![TargetInstance::entity(
            scenario.entity_identifier("barbarian"),
        )],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::UsabilityFunctionError("Rage is already active".to_string())
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.barbarian.rage")
        .assert_event_count(0);
}

#[rstest]
fn effect_blocks_action(mut scenario: Scenario) {
    // Rage's effect hook vetoes spellcasting, however the wizard came by it
    scenario
        .probe("wizard")
        .apply_effect("effect.barbarian.rage");

    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.fire_bolt".into(),
        ActionContext::spell(
            "spell.fire_bolt".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            0,
        ),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::UsabilityFunctionError(
                "Cannot cast spells while Raging".to_string()
            )
        )))
    );
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.fire_bolt")
        .assert_event_count(0);
}

#[rstest]
fn concentration_blocked(mut scenario: Scenario) {
    // Rage blocks Concentration, which is checked before the spellcasting veto
    scenario
        .probe("wizard")
        .apply_effect("effect.barbarian.rage");

    let action = ActionData::new(
        scenario.entity_identifier("wizard"),
        "action.expeditious_retreat".into(),
        ActionContext::spell(
            "spell.expeditious_retreat".into(),
            SpellSource::Class(ClassAndSubclass {
                class: "class.wizard".into(),
                subclass: None,
            }),
            1,
        ),
        ResourceAmountMap::from_iter(vec![
            ("resource.action".into(), ResourceAmount::Flat(1)),
            (
                "resource.spell_slot".into(),
                ResourceAmount::Tiered { tier: 1, amount: 1 },
            ),
        ]),
        vec![TargetInstance::entity(scenario.entity_identifier("wizard"))],
    );

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert!(
        matches!(
            &result,
            Err(ActivityError::ActionError(ActionError::Usability(
                ActionUsabilityError::ConcentrationError(ConcentrationError::ConcentrationBlocked(
                    blockers
                ))
            ))) if !blockers.is_empty()
        ),
        "expected ConcentrationBlocked, got {:?}",
        result
    );
    scenario
        .event_filter()
        .actor("wizard")
        .action_requested("action.expeditious_retreat")
        .assert_event_count(0);
}

fn brutal_strike(scenario: &Scenario) -> ActionData {
    ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.barbarian.brutal_strike".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![
            ("resource.action".into(), ResourceAmount::Flat(1)),
            (
                "resource.barbarian.brutal_strike".into(),
                ResourceAmount::Flat(1),
            ),
        ]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    )
}

#[rstest]
fn variant_action(mut scenario: Scenario) {
    let action = brutal_strike(&scenario).with_variant("variant.brutal_strike.forceful_blow");
    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert!(result.is_ok(), "expected Ok(()), got {:?}", result.err());
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.barbarian.brutal_strike")
        .assert_event();
}

#[rstest]
fn variant_action_without_variant(mut scenario: Scenario) {
    // Brutal Strike is a choice between blows, so submitting it without one is
    // not a complete action
    let action = brutal_strike(&scenario);
    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::VariantError(VariantUsabilityError::VariantRequired {
                action_id: "action.barbarian.brutal_strike".into(),
                variants: vec![
                    "variant.brutal_strike.forceful_blow".into(),
                    "variant.brutal_strike.hamstring_blow".into(),
                ],
            })
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.barbarian.brutal_strike")
        .assert_event_count(0);
}

#[rstest]
fn variant_action_unknown_variant(mut scenario: Scenario) {
    // Staggering Blow only shows up with Improved Brutal Strike at level 13
    let action = brutal_strike(&scenario).with_variant("variant.brutal_strike.staggering_blow");
    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::VariantError(VariantUsabilityError::UnknownVariant {
                action_id: "action.barbarian.brutal_strike".into(),
                options: vec![
                    "variant.brutal_strike.forceful_blow".into(),
                    "variant.brutal_strike.hamstring_blow".into(),
                ],
                choice: "variant.brutal_strike.staggering_blow".into(),
            })
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.barbarian.brutal_strike")
        .assert_event_count(0);
}

#[rstest]
fn variant_on_standard_action(mut scenario: Scenario) {
    // A plain melee attack has no variants to choose between
    let action = ActionData::new(
        scenario.entity_identifier("barbarian"),
        "action.melee_attack".into(),
        ActionContext::melee_weapon(EquipmentSlot::MeleeMainHand),
        ResourceAmountMap::from_iter(vec![("resource.action".into(), ResourceAmount::Flat(1))]),
        vec![TargetInstance::entity(scenario.entity_identifier("goblin"))],
    )
    .with_variant("variant.brutal_strike.forceful_blow");

    let result = scenario.submit_action_decision(ActionDecisionKind::Action { action });

    assert_eq!(
        result,
        Err(ActivityError::ActionError(ActionError::Usability(
            ActionUsabilityError::VariantError(VariantUsabilityError::UnexpectedVariant {
                action_id: "action.melee_attack".into(),
                variant: "variant.brutal_strike.forceful_blow".into(),
            })
        )))
    );
    scenario
        .event_filter()
        .actor("barbarian")
        .action_requested("action.melee_attack")
        .assert_event_count(0);
}
