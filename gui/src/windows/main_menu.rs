use std::{fs::File, io::BufReader};

use imgui::{ChildFlags, MouseButton};
use nat20_core::{
    components::{
        health::{hit_points::HitPoints, life_state::LifeState},
        id::Name,
    },
    engine::{action_prompt::ActionPromptKind, game_state::GameState, geometry::WorldGeometry},
    systems::{
        self,
        geometry::{CreaturePose, RaycastFilter, RaycastHitKind},
    },
};
use parry3d::na::{Matrix4, Point3};
use strum::IntoEnumIterator;
use tracing::error;

use crate::{
    render::{
        common::utils::RenderableMutWithContext,
        ui::{
            engine::LogLevel,
            entities::render_if_present,
            utils::{
                ImguiRenderable, ImguiRenderableMutWithContext, ImguiRenderableWithContext,
                render_button_disabled_conditionally, render_uniform_buttons_with_padding,
            },
        },
        world::{
            camera::OrbitCamera,
            mesh::{Mesh, MeshRenderMode},
            shapes::{self},
        },
    },
    state::{self, gui_state::GuiState},
    windows::{
        action_bar::ActionBarWindow,
        anchor::{self, AUTO_RESIZE, WindowManager},
        creature_debug::CreatureDebugWindow,
        creature_right_click::CreatureRightClickWindow,
        encounter::EncounterWindow,
        level_up::LevelUpWindow,
        line_of_sight_debug::LineOfSightDebugWindow,
        navigation_debug::NavigationDebugWindow,
        reactions::ReactionsWindow,
        spawn_predefined::SpawnPredefinedWindow,
    },
};

pub enum MainMenuState {
    World {
        game_state: GameState,
        auto_scroll_event_log: bool,
        log_level: LogLevel,
        log_source: usize,
        encounters: Vec<EncounterWindow>,
        level_up: Option<LevelUpWindow>,
        spawn_predefined: Option<SpawnPredefinedWindow>,
        creature_debug: Option<CreatureDebugWindow>,
        creature_right_click: Option<CreatureRightClickWindow>,
        action_bar: Option<ActionBarWindow>,
        reactions: ReactionsWindow,
        navigation_debug: NavigationDebugWindow,
        line_of_sight_debug: LineOfSightDebugWindow,
    },
}

pub struct MainMenuWindow {
    state: MainMenuState,
}

impl MainMenuWindow {
    pub fn new() -> Self {
        // TODO: I guess we should save/load this from/to a config file
        let mut initial_config = rerecast::ConfigBuilder::default();
        initial_config.agent_radius = 0.5;
        initial_config.cell_size_fraction = 8.0;
        initial_config.min_region_size = 4;
        initial_config.max_vertices_per_polygon = 4;

        let geometry_name = "test_terrain_2";
        // Attempt to load from the local cache first
        let geometry = if let Ok(file) = File::open(format!(
            "assets/models/geometry/cache/{}.json",
            geometry_name
        )) {
            serde_json::from_reader(BufReader::new(file)).expect("Failed to load cached geometry")
        } else {
            let geometry = WorldGeometry::from_obj_path(
                format!("assets/models/geometry/{}.obj", geometry_name),
                &initial_config.clone().build(),
            );
            // Save to local cache for next time
            let file = File::create(format!(
                "assets/models/geometry/cache/{}.json",
                geometry_name
            ))
            .expect("Failed to create cache file for geometry");
            serde_json::to_writer_pretty(file, &geometry).expect("Failed to write cached geometry");
            geometry
        };

        Self {
            state: MainMenuState::World {
                auto_scroll_event_log: true,
                log_level: LogLevel::Info,
                log_source: 0,
                game_state: GameState::new(geometry),
                encounters: Vec::new(),
                level_up: None,
                spawn_predefined: None,
                creature_debug: None,
                creature_right_click: None,
                action_bar: None,
                reactions: ReactionsWindow::new(),
                navigation_debug: NavigationDebugWindow::new(&initial_config),
                line_of_sight_debug: LineOfSightDebugWindow::new(),
            },
        }
    }

    pub fn render(&mut self, ui: &imgui::Ui, gui_state: &mut GuiState) {
        match &mut self.state {
            MainMenuState::World {
                game_state,
                auto_scroll_event_log,
                log_level,
                log_source,
                encounters,
                level_up,
                spawn_predefined,
                creature_debug,
                creature_right_click,
                action_bar,
                reactions,
                navigation_debug,
                line_of_sight_debug,
            } => {
                game_state.update(ui.io().delta_time);

                navigation_debug.render_mut_with_context(ui, gui_state, game_state);
                line_of_sight_debug.render_mut_with_context(ui, gui_state, game_state);

                gui_state.camera.render_mut_with_context(
                    ui,
                    (
                        game_state,
                        gui_state
                            .settings
                            .get_mut::<bool>(state::parameters::RENDER_CAMERA_DEBUG),
                        &mut gui_state.window_manager,
                    ),
                );

                // Make the raycast result available to the other parts of the UI
                // If anyone of them want to use a mouse click, e.g. spawning a
                // creature at the cursor, they should .take() it
                gui_state.cursor_ray_result = if ui.io().want_capture_mouse {
                    None
                } else if let Some(ray_from_cursor) = gui_state.camera.ray_from_cursor() {
                    systems::geometry::raycast(
                        &game_state.world,
                        &game_state.geometry,
                        &ray_from_cursor,
                        &RaycastFilter::All,
                    )
                } else {
                    None
                };

                if let Some(entity) = gui_state.selected_entity {
                    if (action_bar.is_some() && action_bar.as_ref().unwrap().entity != entity)
                        || action_bar.is_none()
                    {
                        action_bar.replace(ActionBarWindow::new(game_state, entity));
                    }

                    if !reactions.is_active()
                        && let Some(prompt) = game_state.next_prompt_entity(entity)
                    {
                        match &prompt.kind {
                            ActionPromptKind::Reactions { event, options } => {
                                reactions.activate(prompt.id, &event, &options);
                            }
                            _ => {}
                        }
                    }
                } else {
                    *action_bar = None;
                }

                if let Some(action_bar) = action_bar {
                    action_bar.render_mut_with_context(ui, gui_state, game_state);
                }
                reactions.render_mut_with_context(ui, gui_state, game_state);

                let window_manager_ptr =
                    unsafe { &mut *(&mut gui_state.window_manager as *mut WindowManager) };

                window_manager_ptr.render_window(
                    ui,
                    "World",
                    &anchor::TOP_LEFT,
                    AUTO_RESIZE,
                    &mut true,
                    || {
                        Self::render_character_menu(
                            ui,
                            gui_state,
                            game_state,
                            level_up,
                            spawn_predefined,
                            encounters,
                            creature_debug,
                            log_source,
                        );
                    },
                );

                Self::render_event_log(
                    ui,
                    &mut gui_state.window_manager,
                    game_state,
                    encounters,
                    auto_scroll_event_log,
                    log_level,
                    log_source,
                );

                let mut encounter_finished = None;
                for encounter in &mut *encounters {
                    encounter.render_mut_with_context(ui, gui_state, game_state);
                    if encounter.finished() {
                        encounter_finished = Some(encounter.id().clone());
                    }
                }
                if let Some(id) = encounter_finished {
                    encounters.retain(|encounter| encounter.id() != &id);
                }

                // If the raycast result was not taken by anyone, we can fallback
                // to using it for inspecting entities or for movement
                if let Some(raycast) = &gui_state.cursor_ray_result
                    && let Some(closest) = raycast.closest()
                {
                    match &closest.kind {
                        RaycastHitKind::Creature(entity) => {
                            if ui.is_mouse_clicked(MouseButton::Right) {
                                ui.open_popup("CreatureRightClick");
                                creature_right_click
                                    .replace(CreatureRightClickWindow::new(*entity));
                            }

                            if ui.is_mouse_clicked(MouseButton::Left)
                                && systems::ai::is_player_controlled(&game_state.world, *entity)
                            {
                                if gui_state.selected_entity.is_some()
                                    && gui_state.selected_entity.unwrap() == *entity
                                {
                                    gui_state.selected_entity.take();
                                } else {
                                    gui_state.selected_entity.replace(*entity);
                                }
                            }
                        }

                        RaycastHitKind::World => {
                            if ui.is_mouse_clicked(MouseButton::Left)
                                && let Some(entity) = gui_state.selected_entity
                            {
                                let result = game_state.submit_movement(entity, closest.poi);

                                match result {
                                    Ok(path_result) => {
                                        gui_state.path_cache.insert(entity, path_result);
                                    }
                                    Err(err) => {
                                        error!("Failed to submit movement: {:?}", err);
                                    }
                                }
                            }
                        }
                    }
                }

                if let Some(creature_right_click) = creature_right_click {
                    ui.popup("CreatureRightClick", || {
                        creature_right_click.render_mut_with_context(ui, game_state);
                    });
                }

                Self::render_world(ui, gui_state, game_state);
            }
        }
    }

    fn render_character_menu(
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
        level_up_window: &mut Option<LevelUpWindow>,
        spawn_predefined_window: &mut Option<SpawnPredefinedWindow>,
        encounters: &mut Vec<EncounterWindow>,
        debug_window: &mut Option<CreatureDebugWindow>,
        log_source: &mut usize,
    ) {
        ui.child_window("Characters")
            .child_flags(
                ChildFlags::ALWAYS_AUTO_RESIZE
                    | ChildFlags::AUTO_RESIZE_X
                    | ChildFlags::AUTO_RESIZE_Y,
            )
            .build(|| {
                ui.separator_with_text("Creatures");

                let mut entities = game_state
                    .world
                    .query::<&Name>()
                    .into_iter()
                    .map(|(entity, name)| (entity, name.clone()))
                    .collect::<Vec<_>>();

                let entitiy_count = entities.len();

                entities.iter_mut().for_each(|(entity, name)| {
                    if ui.collapsing_header(
                        format!("{}##{:?}", name.as_str(), entity),
                        imgui::TreeNodeFlags::FRAMED,
                    ) {
                        entity.render_mut_with_context(ui, &mut game_state.world);
                        ui.separator();

                        if ui.button(format!("Debug##{:?}", entity)) {
                            *debug_window = Some(CreatureDebugWindow::new(*entity));
                            ui.open_popup("Debug");
                        }
                    }
                });

                if let Some(debug_gui) = debug_window {
                    ui.popup("Debug", || {
                        debug_gui.render_mut_with_context(ui, game_state);
                    });
                }

                ui.separator();
                if ui.button("Spawn Creature") {
                    ui.open_popup("Spawn Creature");
                }
                Self::render_spawn_creature(
                    ui,
                    gui_state,
                    game_state,
                    level_up_window,
                    spawn_predefined_window,
                );

                ui.separator();
                if render_button_disabled_conditionally(
                    ui,
                    "New Encounter",
                    [0.0, 0.0],
                    entitiy_count < 2,
                    "You must have at least two characters to create an encounter.",
                ) {
                    let window = EncounterWindow::new();
                    encounters.push(window);
                    *log_source = encounters.len(); // Select the new encounter as log source
                }
            });
    }

    fn render_spawn_creature(
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
        level_up_window: &mut Option<LevelUpWindow>,
        spawn_predefined_window: &mut Option<SpawnPredefinedWindow>,
    ) {
        ui.popup("Spawn Creature", || {
            if let Some(index) = render_uniform_buttons_with_padding(
                ui,
                ["New Character", "Predefined Creature"],
                [20.0, 5.0],
            ) {
                match index {
                    0 => *level_up_window = Some(LevelUpWindow::new(&game_state.world, None)),
                    // TODO: Don't create the window from scratch every time
                    1 => *spawn_predefined_window = Some(SpawnPredefinedWindow::new()),
                    _ => unreachable!(),
                }
                ui.close_current_popup();
            }
        });

        if let Some(level_up) = level_up_window {
            level_up.render_mut_with_context(ui, &mut game_state.world);
            if level_up.is_level_up_complete() {
                level_up_window.take();
            }
        }

        if let Some(spawn_predefined) = spawn_predefined_window {
            spawn_predefined.render_mut_with_context(ui, gui_state, game_state);
            if spawn_predefined.is_spawning_completed() {
                spawn_predefined_window.take();
            }
        }
    }

    fn render_event_log(
        ui: &imgui::Ui,
        window_manager: &mut WindowManager,
        game_state: &mut GameState,
        encounters: &mut Vec<EncounterWindow>,
        auto_scroll_event_log: &mut bool,
        log_level: &mut LogLevel,
        log_source: &mut usize,
    ) {
        window_manager.render_window(
            ui,
            "Event Log",
            &anchor::BOTTOM_RIGHT,
            AUTO_RESIZE,
            &mut true,
            || {
                let mut log_sources = vec!["World".to_string()];
                log_sources.extend(
                    game_state
                        .encounters
                        .iter()
                        .map(|e| format!("Encounter {}", e.0)),
                );
                *log_source = log_sources.len().min(*log_source);

                let width_token = ui.push_item_width(150.0);
                ui.combo("Log source", log_source, &log_sources[..], |s| {
                    s.to_string().into()
                });
                width_token.end();

                let event_log = if *log_source == 0 || encounters.len() < *log_source {
                    &game_state.event_log
                } else {
                    let id = encounters.get(*log_source - 1).map(|e| e.id()).unwrap();
                    game_state
                        .encounters
                        .get(&id)
                        .map(|e| e.combat_log())
                        .unwrap_or(&game_state.event_log)
                };

                ui.child_window("Event Log Content")
                    .child_flags(
                        ChildFlags::ALWAYS_AUTO_RESIZE
                            | ChildFlags::AUTO_RESIZE_X
                            | ChildFlags::BORDERS,
                    )
                    .size([0.0, 200.0])
                    .build(|| {
                        event_log.render_with_context(ui, &(&game_state.world, &*log_level));

                        if *auto_scroll_event_log && ui.scroll_y() >= ui.scroll_max_y() - 5.0 {
                            ui.set_scroll_here_y_with_ratio(1.0);
                        }
                    });

                ui.checkbox("Auto-scroll", auto_scroll_event_log);

                let mut current_log_level = log_level.clone() as usize;
                let width_token = ui.push_item_width(60.0);
                if ui.combo(
                    "Log level",
                    &mut current_log_level,
                    &LogLevel::iter().collect::<Vec<_>>()[..],
                    |lvl| lvl.to_string().into(),
                ) {
                    *log_level = current_log_level.into();
                }
                width_token.end();
            },
        );
    }

    fn render_world(ui: &imgui::Ui, gui_state: &mut GuiState, game_state: &mut GameState) {
        if *gui_state
            .settings
            .get::<bool>(state::parameters::RENDER_GRID)
        {
            gui_state
                .grid_renderer
                .draw(gui_state.ig_renderer.gl_context());
        }

        let mesh_cache = &mut gui_state.mesh_cache;
        // TODO: Do something less "hardcoded" with the mesh cache
        if let Some(mesh) = mesh_cache.get("world") {
            mesh.draw(
                gui_state.ig_renderer.gl_context(),
                &gui_state.program,
                &Matrix4::identity(),
                [0.75, 0.75, 0.75, 1.0],
                &MeshRenderMode::MeshOnly,
            );
        } else {
            let mesh = Mesh::from_parry_trimesh(
                gui_state.ig_renderer.gl_context(),
                &game_state.geometry.trimesh,
            );
            mesh_cache.insert("world".to_string(), mesh);
        }

        if let Some(mesh) = mesh_cache.get("navmesh") {
            if *gui_state
                .settings
                .get_mut::<bool>(state::parameters::RENDER_NAVIGATION_NAVMESH)
            {
                mesh.draw(
                    gui_state.ig_renderer.gl_context(),
                    &gui_state.program,
                    &Matrix4::identity(),
                    [0.2, 0.8, 0.2, 0.5],
                    &MeshRenderMode::MeshWithWireFrame {
                        color: [0.0, 0.5, 0.0, 0.5],
                        width: 2.0,
                    },
                );
            }
        } else {
            let mesh = Mesh::from_poly_navmesh(
                gui_state.ig_renderer.gl_context(),
                &game_state.geometry.poly_navmesh,
            );
            mesh_cache.insert("navmesh".to_string(), mesh);
        }

        // TODO: I feel like this should be somewhere else
        for (entity, pose) in game_state.world.query::<&CreaturePose>().iter() {
            systems::geometry::get_shape(&game_state.world, entity).map(|(shape, shape_pose)| {
                let key = format!("{:#?}", shape);
                if let Some(mesh) = mesh_cache.get(&key) {
                    if let Some(current_entity) = gui_state.selected_entity
                        && current_entity == entity
                    {
                        // Render a ring around the feet of the currently selected entity
                        gui_state.line_renderer.add_circle(
                            [
                                pose.translation.vector.x,
                                pose.translation.vector.y + 0.1,
                                pose.translation.vector.z,
                            ],
                            shape.radius + 0.1,
                            [1.0, 1.0, 1.0],
                        );
                    }

                    // Highlight if mouse is over the creature
                    let mode = if let Some(raycast) = &gui_state.cursor_ray_result
                        && let Some(closest) = raycast.closest()
                        && let RaycastHitKind::Creature(e) = &closest.kind
                        && *e == entity
                    {
                        &MeshRenderMode::MeshWithWireFrame {
                            color: [1.0, 1.0, 1.0, 1.0],
                            width: 2.0,
                        }
                    } else {
                        gui_state
                            .creature_render_mode
                            .get(&entity)
                            .unwrap_or(&MeshRenderMode::MeshOnly)
                    };

                    mesh.draw(
                        gui_state.ig_renderer.gl_context(),
                        &gui_state.program,
                        &shape_pose.to_homogeneous(),
                        [0.8, 0.8, 0.8, 1.0],
                        mode,
                    );

                    // TEMP
                    gui_state.path_cache.get(&entity).map(|path| {
                        [
                            (&path.taken_path, [1.0, 1.0, 1.0]),
                            (&path.full_path, [1.0, 0.0, 0.0]),
                        ]
                        .iter()
                        .for_each(|(path, color)| {
                            gui_state.line_renderer.add_polyline(
                                &path
                                    .points
                                    .iter()
                                    .map(|p| [p.x, p.y, p.z])
                                    .collect::<Vec<[f32; 3]>>(),
                                *color,
                            );
                        });
                    });
                } else {
                    let mesh = shapes::build_capsule_mesh(
                        gui_state.ig_renderer.gl_context(),
                        8,
                        16,
                        shape.radius,
                        shape.half_height(),
                    );
                    mesh_cache.insert(key, mesh);
                }
            });
        }

        Self::render_creature_labels(ui, game_state, &gui_state.camera);

        // TODO: Not sure where to put this?
        gui_state.line_renderer.draw(
            gui_state.ig_renderer.gl_context(),
            &Matrix4::identity(),
            2.0,
        );
    }

    fn render_creature_labels(ui: &imgui::Ui, game_state: &GameState, camera: &OrbitCamera) {
        for (entity, name) in game_state.world.query::<&Name>().iter() {
            if let Some(pose) = game_state.world.get::<&CreaturePose>(entity).ok() {
                let translation = pose.translation.vector;
                let pos = camera.world_to_screen(&Point3::new(
                    translation.x,
                    translation.y
                        + systems::geometry::get_height(&game_state.world, entity).unwrap() * 1.5,
                    translation.z,
                ));

                if let Some((x, y)) = pos {
                    let height = ui.calc_text_size(name.as_str())[1] * 2.0;
                    let width = ui.calc_text_size("HP:")[0] + 150.0; // rough estimate for width
                    let window_pos = [x - width / 2.0, y - height];
                    ui.window(&format!("Label##{:?}", entity))
                        .always_auto_resize(true)
                        .position(window_pos, imgui::Condition::Always)
                        .bg_alpha(0.5)
                        .title_bar(false)
                        .resizable(false)
                        .movable(false)
                        .scrollable(false)
                        .focus_on_appearing(false)
                        .collapsed(false, imgui::Condition::Always)
                        .mouse_inputs(false)
                        .build(|| {
                            name.render(ui);
                            render_if_present::<HitPoints>(ui, &game_state.world, entity);
                            ui.same_line();
                            render_if_present::<LifeState>(ui, &game_state.world, entity);
                        });
                }
            }
        }
    }
}
