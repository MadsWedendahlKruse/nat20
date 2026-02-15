use core::f32;

use hecs::{Entity, World};
use imgui::MouseButton;
use nat20_core::{
    components::id::Name,
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

pub struct SpawnPredefinedWindow {
    /// Dummy World used to store the predefined entities. Once an entity has been
    /// selected from this window, it will be spawned into the actual game world.
    world: World,
    entity_to_spawn: Option<Entity>,
    current_entity: Option<Entity>,
    spawning_completed: bool,
}

impl SpawnPredefinedWindow {
    pub fn new() -> Self {
        // TODO: Seems pretty scuffed to construct an entire new GameState here?
        let mut game_state = GameState::new(WorldGeometry::from_obj_path(
            "assets/models/geometry/test_terrain.obj",
            &ConfigBuilder::default().build(),
        ));

        let spawners = vec![
            fixtures::creatures::heroes::fighter,
            fixtures::creatures::heroes::wizard,
            fixtures::creatures::heroes::warlock,
            fixtures::creatures::monsters::goblin_warrior,
        ];

        for spawner in spawners {
            let entity = spawner(&mut game_state).id();
            info!("Spawned predefined entity: {:?}", entity);
            // Ensure all resources are fully recharged
            systems::time::on_rest_end(&mut game_state.world, &[entity], &RestKind::Long);
        }

        Self {
            world: game_state.world,
            entity_to_spawn: None,
            current_entity: None,
            spawning_completed: false,
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
                self.world
                    .query::<&Name>()
                    .into_iter()
                    .for_each(|(entity, name)| {
                        if ui.collapsing_header(
                            format!("{}##{:?}", name.as_str(), entity),
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
                            entity
                                .render_with_context(ui, (&self.world, &CreatureRenderMode::Full));
                        }
                    });

                if let Some(entity) = self.entity_to_spawn {
                    if self.current_entity.is_none() {
                        let spawned_entity = if let Ok(_) = self.world.get::<&CharacterTag>(entity)
                        {
                            game_state
                                .world
                                .spawn(Character::from_world(&self.world, entity))
                        } else if let Ok(_) = self.world.get::<&MonsterTag>(entity) {
                            game_state
                                .world
                                .spawn(Monster::from_world(&self.world, entity))
                        } else {
                            panic!("Entity to spawn is neither a Character nor a Monster");
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

                        if let Some(raycast) = &gui_state.cursor_ray_result
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
                                gui_state.cursor_ray_result.take();
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
