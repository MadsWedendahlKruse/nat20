use core::f32;

use hecs::{Entity, World};
use imgui::MouseButton;
use nat20_core::{
    components::id::{EntityIdentifier, MonsterId, Name},
    engine::{
        engine_state::EngineState,
        geometry::{WorldGeometry, WorldGeometrySource},
    },
    entities::{
        common::EntityTemplate,
        creature::{Character, CharacterTemplate, Monster, MonsterTemplate},
    },
    registry::registry::MonstersRegistry,
    systems::{self, entities::EntityKind, time::RestKind},
    test_utils::fixtures::{self},
};
use parry3d::na::Point3;
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
    kind: SpawnerKind,
    spawned_entity: Option<Entity>,
}

impl Spawner {
    fn character(template: CharacterTemplate, max_level: u8) -> Self {
        Self {
            kind: SpawnerKind::Character {
                template,
                max_level,
            },
            spawned_entity: None,
        }
    }

    fn monster(monster_id: MonsterId) -> Self {
        Self {
            kind: SpawnerKind::Monster {
                template: MonsterTemplate { monster_id },
            },
            spawned_entity: None,
        }
    }

    fn spawn(&mut self, engine_state: &mut EngineState, id: Option<Entity>) -> EntityIdentifier {
        let entity = match &mut self.kind {
            SpawnerKind::Character { template, .. } => template.spawn(engine_state, id).unwrap(),
            SpawnerKind::Monster { template } => template.spawn(engine_state, id).unwrap(),
        };

        self.spawned_entity = Some(entity);
        // Ensure all resources are fully recharged
        systems::time::on_rest_end(engine_state, &[entity], &RestKind::Long);

        EntityIdentifier::from_world(&engine_state.world, entity)
    }
}

enum SpawnerKind {
    Character {
        template: CharacterTemplate,
        max_level: u8,
    },
    Monster {
        template: MonsterTemplate,
    },
}

pub struct SpawnPredefinedWindow {
    /// Dummy World used to store the predefined entities. Once an entity has been
    /// selected from this window, it will be spawned into the actual game world.
    engine_state: EngineState,
    entity_to_spawn: Option<Entity>,
    current_entity: Option<Entity>,
    spawning_completed: bool,
    spawners: Vec<Spawner>,
}

impl SpawnPredefinedWindow {
    pub fn new() -> Self {
        // TODO: Seems pretty scuffed to construct an entire new EngineState here?
        let mut engine_state = EngineState::new(
            WorldGeometry::try_from(WorldGeometrySource::Cached {
                identifier: "test_terrain".to_string(),
            })
            .unwrap(),
        );

        let mut spawners = vec![
            Spawner::character(fixtures::creatures::heroes::barbarian(17), 17),
            Spawner::character(fixtures::creatures::heroes::fighter(18), 18),
            Spawner::character(fixtures::creatures::heroes::wizard(7), 7),
            Spawner::character(fixtures::creatures::heroes::warlock(5), 5),
        ];

        spawners.extend(
            MonstersRegistry::keys().map(|monster_id| Spawner::monster(monster_id.clone())),
        );

        for spawner in &mut spawners {
            let entity = spawner.spawn(&mut engine_state, None);
            info!("Spawned predefined entity: {:?}", entity);
        }

        Self {
            engine_state,
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

impl RenderableMutWithContext<&mut EngineState> for SpawnPredefinedWindow {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        engine_state: &mut EngineState,
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
                    if let Some(entity) = spawner.spawned_entity
                        && ui.collapsing_header(
                            format!(
                                "{}##{:?}",
                                systems::helpers::get_component::<Name>(
                                    &self.engine_state.world,
                                    entity
                                )
                                .as_str(),
                                entity
                            ),
                            imgui::TreeNodeFlags::FRAMED,
                        )
                    {
                        if ui.button(format!("Spawn##{:?}", entity)) {
                            self.entity_to_spawn = Some(entity);
                            if let Some(entity) = self.current_entity {
                                engine_state.world.despawn(entity).unwrap();
                                self.current_entity = None;
                            }
                        }

                        ui.separator();

                        let mut updated_level = false;

                        // TODO: Level slider probably doesn't make sense for monsters?
                        if let SpawnerKind::Character {
                            template,
                            max_level,
                        } = &mut spawner.kind
                        {
                            ui.set_next_item_width(150.0);
                            if ui.slider(
                                format!("Level##{:?}", entity),
                                1,
                                *max_level,
                                &mut template.levels,
                            ) {
                                updated_level = true;
                            }

                            ui.separator();
                        }

                        entity.render_with_context(
                            ui,
                            (&self.engine_state, &CreatureRenderMode::Full),
                        );

                        if updated_level {
                            self.engine_state.world.despawn(entity).unwrap();
                            spawner.spawn(&mut self.engine_state, Some(entity));
                        }
                    }
                }

                if let Some(entity) = self.entity_to_spawn {
                    if self.current_entity.is_none() {
                        let spawned_entity = if let Ok(entity_kind) =
                            self.engine_state.world.get::<&EntityKind>(entity)
                        {
                            match *entity_kind {
                                EntityKind::Projectile => {
                                    panic!("Cannot spawn a projectile directly")
                                }

                                EntityKind::Character => engine_state
                                    .world
                                    .spawn(Character::from_world(&self.engine_state.world, entity)),

                                EntityKind::Monster => engine_state
                                    .world
                                    .spawn(Monster::from_world(&self.engine_state.world, entity)),
                            }
                        } else {
                            panic!("Entity {:?} does not have an EntityKind component", entity);
                        };

                        // Spawn it somewhere we can't see it, we'll move it later
                        systems::geometry::teleport_to(
                            &mut engine_state.world,
                            spawned_entity,
                            &Point3::new(f32::MAX, f32::MAX, f32::MAX),
                        );

                        // Ensure the spawned entity has a unique name in the main world
                        // (much easier to debug this way)
                        set_unique_name(&mut engine_state.world, spawned_entity);

                        self.current_entity = Some(spawned_entity);
                    }

                    if let Some(entity) = self.current_entity {
                        ui.tooltip(|| {
                            ui.text("LEFT-CLICK: Spawn here");
                            ui.text("RIGHT-CLICK: Cancel");
                        });

                        if ui.is_mouse_clicked(MouseButton::Right) {
                            gui_state.cursor_ray_result.take();
                            engine_state.world.despawn(entity).unwrap();
                            self.current_entity = None;
                            self.entity_to_spawn = None;
                        }

                        if let Some(raycast) = gui_state.cursor_ray_result.take()
                            && let Some(raycast_world) = raycast.world_hit()
                            && let Some(navmesh_point) = systems::geometry::navmesh_nearest_point(
                                &engine_state.geometry,
                                raycast_world.poi,
                            )
                        {
                            systems::geometry::teleport_to_ground(
                                &mut engine_state.world,
                                &engine_state.geometry,
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
                engine_state.world.despawn(entity).unwrap();
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
