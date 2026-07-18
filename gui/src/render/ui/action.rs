use hecs::{Entity, World};
use nat20_core::{
    components::{
        actions::{
            action::{
                ActionCondition, ActionConditionResolution, ActionContext, ActionKind,
                ActionPayloadComponent, ActionResultComponent, DamageResult, EffectResultKind,
                ReactionResult,
            },
            targeting::{AreaShape, TargetingKind, TargetingRange},
        },
        id::{ActionId, EntityIdentifier, SpellId},
        items::equipment::weapon::MELEE_RANGE_DEFAULT,
        modifier::FlatModifiable,
        resource::{RechargeRule, ResourceAmountMap},
        saving_throw::SavingThrowKind,
        spells::spell::ConcentrationError,
    },
    registry::registry::SpellsRegistry,
    systems::{self, actions::ActionUsabilityError, geometry::Displacement, time::RestKind},
};
use uom::si::{angle::degree, length::meter};

use crate::render::ui::{
    engine::render_event_description,
    modifier::modifiers_to_string,
    text::{TextKind, TextSegment, TextSegments},
    utils::{ImguiRenderable, ImguiRenderableWithContext},
};

// TODO: Pretty janky 'type' here
impl ImguiRenderableWithContext<(&World, Entity, Option<&ActionUsabilityError>)>
    for (&ActionId, &ActionContext, &ResourceAmountMap)
{
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        (world, entity, usability_error): (&World, Entity, Option<&ActionUsabilityError>),
    ) {
        let (action_id, context, cost) = self;
        let action = systems::actions::get_action(action_id).unwrap();

        ui.child_window("Action Tooltip")
            .size([400.0, 0.0])
            .child_flags(imgui::ChildFlags::ALWAYS_AUTO_RESIZE | imgui::ChildFlags::AUTO_RESIZE_Y)
            .build(|| {
                ui.separator_with_text(&action_id.to_string());

                if let Some(usability_error) = usability_error {
                    usability_error.render(ui);
                }

                let spell_id: SpellId = action.id.clone().into();
                let spell = SpellsRegistry::get(&spell_id);
                if let Some(spell) = spell {
                    if spell.base_level() > 0 {
                        TextSegment::new(
                            format!("Level {} {} Spell", spell.base_level(), spell.school()),
                            TextKind::Details,
                        )
                        .render(ui);
                    } else {
                        TextSegment::new(format!("{} Cantrip", spell.school()), TextKind::Details)
                            .render(ui);
                    }
                }

                action
                    .kind
                    .render_with_context(ui, (world, entity, context));

                ui.separator();

                let targeting = (action.targeting)(world, entity, context);
                targeting.range.render(ui);
                targeting.kind.render(ui);

                for phase in action.kind().phases() {
                    match &phase.condition {
                        ActionCondition::AttackRoll(_) => {
                            TextSegment::new("Attack Roll", TextKind::Details).render(ui);
                        }
                        ActionCondition::SavingThrow(saving_throw) => {
                            let saving_throw = saving_throw(world, entity, &context);
                            let saving_throw_ability = match saving_throw.key {
                                SavingThrowKind::Ability(ability) => ability,
                                _ => todo!(),
                            };
                            TextSegments::new(vec![
                                (saving_throw_ability.to_string(), TextKind::Ability),
                                ("Saving Throw".to_string(), TextKind::Details),
                            ])
                            .render(ui);
                        }
                        _ => {}
                    }
                }

                if let Some(spell) = spell {
                    ui.separator();
                    for flag in spell.flags() {
                        TextSegment::new(format!("{:?}", flag), TextKind::Details).render(ui);
                    }
                }

                ui.separator();

                cost.render(ui);

                ui.separator();

                TextSegment::new(action.description.as_str(), TextKind::Details)
                    .wrap_text(true)
                    .render(ui);
            });
    }
}

impl ImguiRenderableWithContext<(&World, Entity, &ActionContext)> for ActionKind {
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        (world, entity, action_context): (&World, Entity, &ActionContext),
    ) {
        for phase in self.phases() {
            for component in phase.payload.components() {
                match component {
                    ActionPayloadComponent::Damage { damage, .. } => {
                        let damage_roll = damage(world, entity, action_context);
                        // TODO: Apply pre-damage roll effects? (e.g. Barbarian Rage)
                        damage_roll.render(ui);
                    }
                    ActionPayloadComponent::Effect(effect) => {
                        TextSegment::new(format!("{}", effect.effect_id), TextKind::Effect)
                            .render(ui)
                    }
                    ActionPayloadComponent::Healing(healing) => {
                        // TODO: More info? Modifiers?
                        let healing = healing(world, entity, action_context);
                        TextSegment::new(
                            format!("{}-{} Healing", healing.min(), healing.max()),
                            TextKind::Healing,
                        )
                        .render(ui);
                    }
                    ActionPayloadComponent::Reaction(_) => { /* Not sure what (if anything) to render here */
                    }
                    ActionPayloadComponent::Displacement(displacement) => {
                        displacement(world, entity, action_context).render(ui);
                    }
                }
            }
        }
    }
}

impl ImguiRenderable for TargetingRange {
    fn render(&self, ui: &imgui::Ui) {
        let range_text = if self.max().get::<meter>() == 0.0 {
            return;
        } else if *self == *MELEE_RANGE_DEFAULT {
            "Melee".to_string()
        } else if self.max() == self.normal() {
            format!("{:.1} meters", self.max().get::<meter>())
        } else {
            format!(
                "{:.1} ({:.1}) meters",
                self.normal().get::<meter>(),
                self.max().get::<meter>()
            )
        };
        TextSegments::new(vec![
            ("Range:".to_string(), TextKind::Details),
            (range_text, TextKind::Details),
        ])
        .render(ui);
    }
}

impl ImguiRenderable for TargetingKind {
    fn render(&self, ui: &imgui::Ui) {
        let text = match self {
            TargetingKind::SelfTarget => "Self Target".to_string(),
            TargetingKind::Single => "Single Target".to_string(),
            TargetingKind::Multiple {
                max_targets,
                allow_duplicates,
            } => {
                if *allow_duplicates {
                    format!("{} Targets", max_targets)
                } else {
                    format!("{} Targets (Unique)", max_targets)
                }
            }
            TargetingKind::Area { shape, .. } => match shape {
                AreaShape::Cone { angle, length } => {
                    format!(
                        "AoE: Arc\n\tAngle: {:.0}°, Length: {:.1} meters",
                        angle.get::<degree>(),
                        length.get::<meter>()
                    )
                }
                AreaShape::Sphere { radius } => {
                    format!("AoE: Sphere\n\tRadius: {:.1} meters", radius.get::<meter>())
                }
                AreaShape::Cube { side_length } => {
                    format!(
                        "AoE: Cube\n\tSide Length: {:.1} meters",
                        side_length.get::<meter>()
                    )
                }
                AreaShape::Cylinder { radius, height } => {
                    format!(
                        "AoE: Cylinder\n\tRadius: {:.1} meters, Height: {:.1} meters",
                        radius.get::<meter>(),
                        height.get::<meter>()
                    )
                }
                AreaShape::Line { length, width } => {
                    format!(
                        "AoE: Line\n\tLength: {:.1} meters, Width: {:.1} meters",
                        length.get::<meter>(),
                        width.get::<meter>()
                    )
                }
                AreaShape::Capsule {
                    half_height,
                    radius,
                } => {
                    format!(
                        "AoE: Capsule\n\tRadius: {:.1} meters, Half Height: {:.1} meters",
                        radius.get::<meter>(),
                        half_height.get::<meter>()
                    )
                }
            },
        };
        TextSegment::new(text, TextKind::Details).render(ui);
    }
}

impl ImguiRenderable for ActionUsabilityError {
    fn render(&self, ui: &imgui::Ui) {
        match self {
            ActionUsabilityError::EntityNotAlive(_) => {
                TextSegment::new("Must be alive to use", TextKind::Red).render(ui);
            }

            ActionUsabilityError::OnCooldown(recharge_rule) => {
                let text = match recharge_rule {
                    RechargeRule::Turn => "Action can only be used once per turn",
                    RechargeRule::Rest(rest_kind) => match rest_kind {
                        RestKind::Short => "Action can only be used once per short rest",
                        RestKind::Long => "Action can only be used once per long rest",
                    },
                    RechargeRule::Daily => "Action can only be used once per day",
                    RechargeRule::Never => "Action can only be used once",
                };
                TextSegment::new(text, TextKind::Red).render(ui);
            }

            ActionUsabilityError::NotEnoughResources(resource_ids) => {
                if resource_ids.len() == 1 {
                    TextSegment::new(
                        format!("Not enough resource: {}", resource_ids[0]),
                        TextKind::Red,
                    )
                    .render(ui);
                } else {
                    let mut segments = vec![("Not enough resources:".to_string(), TextKind::Red)];
                    resource_ids.iter().for_each(|resource_id| {
                        segments.push((format!("- {}", resource_id), TextKind::Red));
                    });
                    TextSegments::new(segments).render(ui);
                }
            }

            ActionUsabilityError::ResourceNotFound(resource_id) => {
                TextSegment::new(
                    format!("Resource not found: {}", resource_id),
                    TextKind::Red,
                )
                .render(ui);
            }

            ActionUsabilityError::TargetingError(targeting_error) => {
                TextSegment::new(
                    format!("Invalid target: {:?}", targeting_error),
                    TextKind::Red,
                )
                .render(ui);
            }

            ActionUsabilityError::ConcentrationError(concentration_error) => {
                match concentration_error {
                    ConcentrationError::ConcentrationBlocked(sources) => {
                        if sources.len() == 1 {
                            TextSegment::new(
                                format!("Concentration is blocked by {}", sources[0]),
                                TextKind::Red,
                            )
                            .wrap_text(true)
                            .render(ui);
                        } else {
                            let mut segments =
                                vec![("Concentration is blocked by:".to_string(), TextKind::Red)];
                            sources.iter().for_each(|source| {
                                segments.push((format!("- {}", source), TextKind::Red));
                            });
                            TextSegments::new(segments).render(ui);
                        }
                    }
                }
            }

            ActionUsabilityError::UsabilityFunctionError(error) => {
                TextSegment::new(format!("{}", error), TextKind::Red)
                    .wrap_text(true)
                    .render(ui);
            }
        }
    }
}

impl ImguiRenderableWithContext<(&Option<&EntityIdentifier>, &str)> for ActionResultComponent {
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        (actor, target_name): (&Option<&EntityIdentifier>, &str),
    ) {
        match self {
            ActionResultComponent::Damage(damage) => {
                let no_damage_text = match damage.resolution {
                    ActionConditionResolution::Unconditional => "took no damage",
                    ActionConditionResolution::AttackRoll { .. } => "was not hit",
                    ActionConditionResolution::SavingThrow { .. } => "took no damage",
                };

                ui.group(|| {
                    damage
                        .damage_taken
                        .render_with_context(ui, (&target_name, no_damage_text, None));
                    damage.new_life_state.render_with_context(
                        ui,
                        (&target_name, actor.as_ref().map(|p| p.name().as_str())),
                    );
                });

                if ui.is_item_hovered() {
                    ui.tooltip(|| {
                        render_damage_resolution(ui, target_name, damage);
                    });
                }
            }
            ActionResultComponent::Effect(effect) => match effect.result {
                EffectResultKind::Applied => {
                    TextSegments::new(vec![
                        (target_name, TextKind::Target),
                        ("gained effect", TextKind::Normal),
                        (&effect.root().to_string(), TextKind::Effect),
                    ])
                    .render(ui);
                }
                EffectResultKind::Removed => {
                    TextSegments::new(vec![
                        (target_name, TextKind::Target),
                        ("lost effect", TextKind::Normal),
                        (&effect.root().to_string(), TextKind::Effect),
                    ])
                    .render(ui);
                }
                EffectResultKind::None => {
                    TextSegments::new(vec![
                        (target_name, TextKind::Target),
                        ("was unaffected by", TextKind::Normal),
                        (&effect.root().to_string(), TextKind::Effect),
                    ])
                    .render(ui);
                }
            },
            ActionResultComponent::Healing(healing) => {
                ui.group(|| {
                    TextSegments::new(vec![
                        (target_name, TextKind::Target),
                        ("was healed for", TextKind::Normal),
                        (
                            &format!("{} HP", healing.healing.total()),
                            TextKind::Healing,
                        ),
                    ])
                    .render(ui);
                    healing.new_life_state.render_with_context(
                        ui,
                        (&target_name, actor.as_ref().map(|p| p.name().as_str())),
                    );
                });

                if ui.is_item_hovered() {
                    ui.tooltip(|| {
                        ui.text("Healing:");
                        ui.same_line();
                        TextSegment::new(
                            &format!("{} HP", modifiers_to_string(&healing.healing)),
                            TextKind::Healing,
                        )
                        .render(ui);
                    });
                }
            }
            ActionResultComponent::Reaction(reaction) => {
                let Some(actor) = actor else {
                    TextSegment::new("Reaction result without actor", TextKind::Red).render(ui);
                    return;
                };

                match reaction {
                    ReactionResult::ModifyEvent { before, after } => {
                        TextSegments::new(vec![
                            (
                                format!("{}'s", actor.name().as_str()).as_str(),
                                TextKind::Actor,
                            ),
                            ("reaction modified", TextKind::Normal),
                        ])
                        .render(ui);
                        ui.same_line();
                        render_event_description(ui, before);

                        if ui.is_item_hovered() {
                            ui.tooltip(|| {
                                ui.separator_with_text("Before");
                                ui.text("TODO");
                                ui.separator_with_text("After");
                                ui.text("TODO");
                            });
                        }
                    }

                    ReactionResult::CancelEvent {
                        event,
                        resources_refunded,
                    } => {
                        TextSegments::new(vec![
                            (
                                format!("{}'s", actor.name().as_str()).as_str(),
                                TextKind::Actor,
                            ),
                            ("reaction canceled", TextKind::Normal),
                        ])
                        .render(ui);
                        ui.same_line();
                        render_event_description(ui, event);
                    }

                    ReactionResult::NoEffect => {
                        TextSegments::new(vec![
                            (
                                format!("{}'s", actor.name().as_str()).as_str(),
                                TextKind::Actor,
                            ),
                            ("reaction had no effect on", TextKind::Normal),
                            (target_name, TextKind::Target),
                        ])
                        .render(ui);
                    }
                }
            }
            // TODO: Consider changing the "*verb*-ing *target*" wording here
            ActionResultComponent::Displacement(displacement) => {
                let verb = match displacement {
                    Some(Displacement::Teleport) => "teleporting",
                    Some(Displacement::Push { .. }) => "pushing",
                    Some(Displacement::Pull { .. }) => "pulling",
                    None => "failing to displace",
                };
                TextSegment::new(verb, TextKind::Normal).render(ui);
                ui.same_line();
                TextSegment::new(target_name, TextKind::Target).render(ui);
            }
        }
    }
}

fn render_damage_resolution(ui: &imgui::Ui, target_name: &str, damage: &DamageResult) {
    match damage.resolution {
        ActionConditionResolution::Unconditional => {
            (&damage.damage_roll, &damage.damage_taken).render(ui);
        }

        ActionConditionResolution::AttackRoll {
            ref attack_roll,
            ref armor_class,
        } => {
            TextSegment::new(format!("{}'s", target_name), TextKind::Target).render(ui);
            ui.same_line();
            ui.text("Armor Class:");
            ui.same_line();
            armor_class.render(ui);

            ui.text("");
            ui.text("Attack Roll:");
            ui.same_line();
            attack_roll.render(ui);

            if let Some(damage_taken) = &damage.damage_taken {
                ui.text("");
                ui.text("Damage Roll:");
                ui.same_line();
                damage.damage_roll.as_ref().unwrap().render(ui);

                ui.text("");
                ui.text("Damage Taken:");
                ui.same_line();
                damage_taken.render(ui);
            } else {
                ui.text(format!(
                    "Attack did not hit. Attack roll ({}) was less than Armor Class ({})",
                    attack_roll.roll_result.total(),
                    armor_class.total()
                ));
            }
        }

        ActionConditionResolution::SavingThrow {
            ref saving_throw_dc,
            ref saving_throw_result,
        } => {
            ui.text("Saving Throw DC:");
            ui.same_line();
            saving_throw_dc.render(ui);

            ui.text("");
            ui.text("Saving Throw:");
            ui.same_line();
            saving_throw_result.render(ui);

            ui.same_line();
            let (label, kind) = if saving_throw_result.is_success(saving_throw_dc) {
                ("Success", TextKind::Green)
            } else {
                ("Failure", TextKind::Red)
            };
            TextSegment::new(label, kind).render(ui);

            ui.text("");
            (&damage.damage_roll, &damage.damage_taken).render(ui);
        }
    }
}
