use core::f32;

use hecs::{Entity, World};
use imgui::MouseButton;
use nat20_core::{
    components::id::{EntityIdentifier, Name},
    engine::{game_state::GameState, geometry::WorldGeometry},
    entities::{
        character::{Character, CharacterTag},
        monster::{Monster, MonsterTag},
    },
    systems::{self, time::RestKind},
    test_utils::fixtures::{self},
};
use parry3d::na::Point3;
use rerecast::ConfigBuilder;
use tracing::info;

use crate::{
    render::{
        common::utils::RenderableMutWithContext,
        ui::{entities::CreatureRenderMode, utils::ImguiRenderableWithContext},
    },
    state::gui_state::GuiState,
    windows::anchor::{AUTO_RESIZE, TOP_LEFT},
};

struct Spawner {
    spawn_fn: Box<dyn Fn(&mut GameState, u8, Option<Entity>) -> EntityIdentifier>,
    max_level: u8,
    current_level: u8,
    spawned_entity: Option<Entity>,
}

impl Spawner {
    fn new<F>(spawn_fn: F, max_level: u8) -> Self
    where
        F: Fn(&mut GameState, u8, Option<Entity>) -> EntityIdentifier + 'static,
    {
        Self {
            spawn_fn: Box::new(spawn_fn),
            max_level,
            current_level: max_level,
            spawned_entity: None,
        }
    }

    fn spawn(&mut self, game_state: &mut GameState, id: Option<Entity>) -> EntityIdentifier {
        let entity = (self.spawn_fn)(game_state, self.current_level, id);
        self.spawned_entity = Some(entity.id());
        // Ensure all resources are fully recharged
        systems::time::on_rest_end(&mut game_state.world, &[entity.id()], &RestKind::Long);
        entity
    }
}

pub struct SpawnPredefinedWindow {
    /// Dummy World used to store the predefined entities. Once an entity has been
    /// selected from this window, it will be spawned into the actual game world.
    game_state: GameState,
    entity_to_spawn: Option<Entity>,
    current_entity: Option<Entity>,
    spawning_completed: bool,
    spawners: Vec<Spawner>,
}

impl SpawnPredefinedWindow {
    pub fn new() -> Self {
        // TODO: Seems pretty scuffed to construct an entire new GameState here?
        let mut game_state = GameState::new(WorldGeometry::from_obj_path(
            "assets/models/geometry/test_terrain.obj",
            &ConfigBuilder::default().build(),
        ));

        let mut spawners = vec![
            Spawner::new(fixtures::creatures::heroes::fighter, 18),
            Spawner::new(fixtures::creatures::heroes::wizard, 6),
            Spawner::new(fixtures::creatures::heroes::warlock, 5),
            Spawner::new(fixtures::creatures::monsters::goblin_warrior, 1),
        ];

        for spawner in &mut spawners {
            let entity = spawner.spawn(&mut game_state, None);
            info!("Spawned predefined entity: {:?}", entity);
        }

        Self {
            game_state,
            entity_to_spawn: None,
            current_entity: None,
            spawning_completed: false,
            spawners,
        }
    }

    pub fn is_spawning_completed(&self) -> bool {
        self.spawning_completed
    }
}

impl RenderableMutWithContext<&mut GameState> for SpawnPredefinedWindow {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let mut opened = !self.spawning_completed;

        if !opened {
            return;
        }

        gui_state.window_manager.render_window(
            ui,
            "Spawn",
            &TOP_LEFT,
            AUTO_RESIZE,
            &mut opened,
            || {
                for spawner in &mut self.spawners {
                    if let Some(entity) = spawner.spawned_entity {
                        if ui.collapsing_header(
                            format!(
                                "{}##{:?}",
                                systems::helpers::get_component::<Name>(
                                    &self.game_state.world,
                                    entity
                                )
                                .as_str(),
                                entity
                            ),
                            imgui::TreeNodeFlags::FRAMED,
                        ) {
                            if ui.button(format!("Spawn##{:?}", entity)) {
                                self.entity_to_spawn = Some(entity);
                                if let Some(entity) = self.current_entity {
                                    game_state.world.despawn(entity).unwrap();
                                    self.current_entity = None;
                                }
                            }

                            ui.separator();

                            let mut updated_level = false;

                            // TODO: Level slider probably doesn't make sense for monsters?
                            if let Ok(_) = self.game_state.world.get::<&CharacterTag>(entity) {
                                ui.set_next_item_width(150.0);
                                if ui.slider(
                                    format!("Level##{:?}", entity),
                                    1,
                                    spawner.max_level,
                                    &mut spawner.current_level,
                                ) {
                                    updated_level = true;
                                }

                                ui.separator();
                            }

                            entity.render_with_context(
                                ui,
                                (&self.game_state.world, &CreatureRenderMode::Full),
                            );

                            if updated_level {
                                self.game_state.world.despawn(entity).unwrap();
                                spawner.spawn(&mut self.game_state, Some(entity));
                            }
                        }
                    }
                }

                if let Some(entity) = self.entity_to_spawn {
                    if self.current_entity.is_none() {
                        let spawned_entity =
                            if let Ok(_) = self.game_state.world.get::<&CharacterTag>(entity) {
                                game_state
                                    .world
                                    .spawn(Character::from_world(&self.game_state.world, entity))
                            } else if let Ok(_) = self.game_state.world.get::<&MonsterTag>(entity) {
                                game_state
                                    .world
                                    .spawn(Monster::from_world(&self.game_state.world, entity))
                            } else {
                                panic!("Failed to take entity from spawner");
                            };

                        // Spawn it somewhere we can't see it, we'll move it later
                        systems::geometry::teleport_to(
                            &mut game_state.world,
                            spawned_entity,
                            &Point3::new(f32::MAX, f32::MAX, f32::MAX),
                        );

                        // Ensure the spawned entity has a unique name in the main world
                        // (much easier to debug this way)
                        set_unique_name(&mut game_state.world, spawned_entity);

                        self.current_entity = Some(spawned_entity);
                    }

                    if let Some(entity) = self.current_entity {
                        ui.tooltip(|| {
                            ui.text("LEFT-CLICK: Spawn here");
                            ui.text("RIGHT-CLICK: Cancel");
                        });

                        if ui.is_mouse_clicked(MouseButton::Right) {
                            gui_state.cursor_ray_result.take();
                            game_state.world.despawn(entity).unwrap();
                            self.current_entity = None;
                            self.entity_to_spawn = None;
                        }

                        if let Some(raycast) = gui_state.cursor_ray_result.take()
                            && let Some(raycast_world) = raycast.world_hit()
                            && let Some(navmesh_point) = systems::geometry::navmesh_nearest_point(
                                &game_state.geometry,
                                raycast_world.poi,
                            )
                        {
                            systems::geometry::teleport_to_ground(
                                &mut game_state.world,
                                &game_state.geometry,
                                entity,
                                &navmesh_point,
                            );

                            if ui.is_mouse_clicked(MouseButton::Left) {
                                self.current_entity = None;
                            }
                        }
                    }
                }

                ui.separator();
                if ui.button_with_size("Done", [100.0, 30.0]) {
                    self.spawning_completed = true;
                }
            },
        );

        if self.spawning_completed {
            if let Some(entity) = self.current_entity {
                game_state.world.despawn(entity).unwrap();
                self.current_entity = None;
                self.entity_to_spawn = None;
            }
        } else {
            self.spawning_completed = !opened;
        }
    }
}

fn set_unique_name(world: &mut World, entity: Entity) {
    if let Ok(mut name) = world.get::<&mut Name>(entity) {
        *name = Name::new(format!("{} ({:?})", name.as_str(), entity));
    }
}
