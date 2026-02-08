use hecs::Entity;
use nat20_core::{
    components::{
        ai::PlayerControlledTag,
        d20::D20CheckDC,
        modifier::{ModifierSet, ModifierSource},
        saving_throw::SavingThrowKind,
        skill::Skill,
        time::{EntityClock, TimeStep, TurnBoundary},
    },
    engine::game_state::GameState,
    systems::{self, d20::D20CheckDCKind, geometry::CreaturePose, time::RestKind},
};
use parry3d::na::UnitQuaternion;
use strum::IntoEnumIterator;

use crate::render::ui::utils::{
    ImguiRenderableMutWithContext, render_uniform_buttons_with_padding,
};

pub enum CheckKind {
    SavingThrow,
    SkillCheck,
}

pub enum CreatureDebugState {
    MainMenu,
    Check {
        kind: CheckKind,
        dc_value: i32,
    },
    Clock,
    TogglePlayerControl,
    MoveTo {
        starting_pose: CreaturePose,
        target_pose: CreaturePose,
    },
}

pub struct CreatureDebugWindow {
    pub state: CreatureDebugState,
    pub creature: Entity,
}

impl CreatureDebugWindow {
    pub fn new(creature: Entity) -> Self {
        Self {
            state: CreatureDebugState::MainMenu,
            creature,
        }
    }
}

impl ImguiRenderableMutWithContext<&mut GameState> for CreatureDebugWindow {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, game_state: &mut GameState) {
        match &mut self.state {
            CreatureDebugState::MainMenu => {
                if let Some(index) = render_uniform_buttons_with_padding(
                    ui,
                    [
                        "Despawn",
                        "Heal Full",
                        "Clock (Advance Time)",
                        "Toggle Player Control",
                        "Saving Throw",
                        "Skill Check",
                        "Move To",
                    ],
                    [20.0, 5.0],
                ) {
                    match index {
                        0 => {
                            game_state.world.despawn(self.creature).ok();
                            ui.close_current_popup();
                        }
                        1 => {
                            systems::health::heal_full(&mut game_state.world, self.creature);
                            ui.close_current_popup();
                        }
                        2 => {
                            self.state = CreatureDebugState::Clock;
                        }
                        3 => {
                            self.state = CreatureDebugState::TogglePlayerControl;
                        }
                        4 => {
                            self.state = CreatureDebugState::Check {
                                kind: CheckKind::SavingThrow,
                                dc_value: 10,
                            };
                        }
                        5 => {
                            self.state = CreatureDebugState::Check {
                                kind: CheckKind::SkillCheck,
                                dc_value: 10,
                            };
                        }
                        6 => {
                            let starting_pose = game_state
                                .world
                                .get::<&CreaturePose>(self.creature)
                                .map(|p| *p)
                                .unwrap_or_else(|_| CreaturePose::identity());
                            self.state = CreatureDebugState::MoveTo {
                                starting_pose,
                                target_pose: starting_pose,
                            };
                        }
                        _ => unreachable!(),
                    }
                }
            }

            CreatureDebugState::Check { kind, dc_value } => match kind {
                CheckKind::SavingThrow => {
                    ui.separator_with_text("Saving Throw");

                    let width_token = ui.push_item_width(50.0);
                    ui.input_int("DC", dc_value)
                        .auto_select_all(true)
                        .enter_returns_true(true)
                        .build();
                    width_token.end();
                    ui.separator();
                    let choice = render_uniform_buttons_with_padding(
                        ui,
                        SavingThrowKind::iter().map(|ability| ability.to_string()),
                        [20.0, 5.0],
                    );

                    if let Some(index) = choice {
                        let kind = SavingThrowKind::iter()
                            .nth(index)
                            .expect("Invalid ability index");
                        let dc = D20CheckDCKind::SavingThrow(D20CheckDC {
                            dc: ModifierSet::from_iter([(
                                ModifierSource::Custom("Saving Throw DC".to_string()),
                                *dc_value,
                            )]),
                            key: kind,
                        });
                        let event = systems::d20::check(game_state, self.creature, &dc);
                        game_state.process_event(event);
                        ui.close_current_popup();
                    }
                }

                CheckKind::SkillCheck => {
                    ui.separator_with_text("Skill Check");

                    let width_token = ui.push_item_width(50.0);
                    ui.input_int("DC", dc_value)
                        .auto_select_all(true)
                        .enter_returns_true(true)
                        .build();
                    width_token.end();
                    ui.separator();
                    let choice = render_uniform_buttons_with_padding(
                        ui,
                        Skill::iter().map(|skill| skill.to_string()),
                        [20.0, 5.0],
                    );

                    if let Some(index) = choice {
                        let skill = Skill::iter().nth(index).expect("Invalid skill index");
                        let dc = D20CheckDCKind::Skill(D20CheckDC {
                            dc: ModifierSet::from_iter([(
                                ModifierSource::Custom("Skill Check DC".to_string()),
                                *dc_value,
                            )]),
                            key: skill,
                        });
                        let event = systems::d20::check(game_state, self.creature, &dc);
                        game_state.process_event(event);
                        ui.close_current_popup();
                    }
                }
            },

            CreatureDebugState::Clock => {
                {
                    let clock = systems::helpers::get_component::<EntityClock>(
                        &game_state.world,
                        self.creature,
                    );
                    ui.separator_with_text("Creature Clock");
                    ui.text(format!("Mode: {:?}", clock.mode()));
                    ui.text(format!(
                        "Local Time: {:.2} seconds",
                        clock.local_time_seconds()
                    ));
                    ui.separator();
                }

                if let Some(index) = render_uniform_buttons_with_padding(
                    ui,
                    ["New Turn", "Short Rest", "Long Rest"],
                    [20.0, 5.0],
                ) {
                    match index {
                        0 => {
                            systems::time::advance_time(
                                game_state,
                                self.creature,
                                TimeStep::TurnBoundary {
                                    entity: self.creature,
                                    boundary: TurnBoundary::Start,
                                },
                            );
                            // TODO: Temporary, should be handled in advance_time
                            systems::time::on_turn_start(&mut game_state.world, self.creature);
                        }
                        1 | 2 => {
                            let rest_kind = match index {
                                1 => RestKind::Short,
                                2 => RestKind::Long,
                                _ => unreachable!(),
                            };
                            systems::time::start_rest(game_state, vec![self.creature], &rest_kind)
                                .unwrap();
                            // For debugging, immediately finish the rest
                            systems::time::finish_rest(game_state, vec![self.creature]).unwrap();
                        }
                        _ => unreachable!(),
                    };

                    ui.close_current_popup();
                }
            }

            CreatureDebugState::TogglePlayerControl => {
                if let Some(index) = render_uniform_buttons_with_padding(
                    ui,
                    ["Set Player Controlled", "Set AI Controlled"],
                    [20.0, 5.0],
                ) {
                    match index {
                        0 => {
                            game_state
                                .world
                                .insert_one(self.creature, PlayerControlledTag)
                                .ok();
                        }
                        1 => {
                            game_state
                                .world
                                .remove_one::<PlayerControlledTag>(self.creature)
                                .ok();
                        }
                        _ => unreachable!(),
                    }
                    ui.close_current_popup();
                }
            }

            CreatureDebugState::MoveTo {
                starting_pose,
                target_pose,
            } => {
                ui.separator_with_text("Current Position");
                ui.text(format!(
                    "X: {:.2}, Y: {:.2}, Z: {:.2}",
                    starting_pose.translation.x,
                    starting_pose.translation.y,
                    starting_pose.translation.z
                ));

                ui.separator_with_text("Move To");

                let width_token = ui.push_item_width(200.0);
                let mut pos = [
                    target_pose.translation.x,
                    target_pose.translation.y,
                    target_pose.translation.z,
                ];
                if ui.input_float3("Position", &mut pos).build() {
                    target_pose.translation.x = pos[0];
                    target_pose.translation.y = pos[1];
                    target_pose.translation.z = pos[2];
                }
                let mut rot = [
                    target_pose.rotation.euler_angles().0,
                    target_pose.rotation.euler_angles().1,
                    target_pose.rotation.euler_angles().2,
                ];
                if ui.input_float3("Rotation (radians)", &mut rot).build() {
                    target_pose.rotation =
                        UnitQuaternion::from_euler_angles(rot[0], rot[1], rot[2]);
                }
                width_token.end();

                ui.separator();
                if ui.button("Set Position") {
                    game_state
                        .world
                        .insert_one(self.creature, *target_pose)
                        .ok();
                }
                ui.same_line();
                if ui.button("Reset Position") {
                    // Revert to starting pose
                    game_state
                        .world
                        .insert_one(self.creature, *starting_pose)
                        .ok();
                    ui.close_current_popup();
                }
            }
        }
    }
}
