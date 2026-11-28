use std::{collections::BTreeMap, fmt::Debug, sync::LazyLock};

use hecs::Entity;
use parry3d::na::Point3;

use crate::{
    components::{
        id::EntityIdentifier,
        resource::RechargeRule,
        time::{EntityClock, TimeMode},
    },
    engine::engine_state::EngineState,
    entities::{
        common::EntityTemplate,
        creature::{CharacterTemplate, MonsterTemplate},
    },
    registry::registry::MonstersRegistry,
    systems,
    test_utils::fixtures::creatures::heroes,
};

pub static HERO_TEMPLATES: LazyLock<BTreeMap<&'static str, CharacterTemplate>> =
    LazyLock::new(|| {
        BTreeMap::from([
            ("hero.barbarian", heroes::barbarian(1)),
            ("hero.fighter", heroes::fighter(1)),
            ("hero.wizard", heroes::wizard(1)),
            ("hero.warlock", heroes::warlock(1)),
        ])
    });

// TODO: Not sure where this should live
pub struct CreatureBuilder<T> {
    template: T,
    position: Option<(Point3<f32>, bool)>,
    time_mode: TimeMode,
    entity_id: Option<Entity>,
}

impl CreatureBuilder<CharacterTemplate> {
    #[track_caller]
    pub fn hero(template: &str) -> Self {
        Self::new(
            HERO_TEMPLATES
                .get(template)
                .unwrap_or_else(|| panic!("No hero template with name {template}"))
                .clone(),
        )
    }

    pub fn level(&mut self, level: u8) -> &mut Self {
        self.template.levels = level;
        self
    }
}

impl CreatureBuilder<MonsterTemplate> {
    #[track_caller]
    pub fn monster(id: &str) -> Self {
        let monster_id = MonstersRegistry::keys()
            .find(|key| key.id == id)
            .unwrap_or_else(|| panic!("No monster with id {id}"))
            .clone();
        Self::new(MonsterTemplate { monster_id })
    }
}

impl<T: EntityTemplate> CreatureBuilder<T>
where
    T::TemplateError: Debug,
{
    fn new(template: T) -> Self {
        Self {
            template,
            time_mode: TimeMode::RealTime,
            position: None,
            entity_id: None,
        }
    }

    pub fn position(&mut self, position: impl Into<Point3<f32>>, on_ground: bool) -> &mut Self {
        self.position = Some((position.into(), on_ground));
        self
    }

    pub fn time_mode(&mut self, time_mode: TimeMode) -> &mut Self {
        self.time_mode = time_mode;
        self
    }

    pub fn with_id(&mut self, entity: Entity) -> &mut Self {
        self.entity_id = Some(entity);
        self
    }

    pub fn spawn(&mut self, engine_state: &mut EngineState) -> EntityIdentifier {
        let entity = match self.template.spawn(engine_state, self.entity_id) {
            Ok(entity) => entity,
            Err(err) => panic!("Failed to spawn creature: {:?}", err),
        };

        if let Some((position, on_ground)) = self.position {
            if on_ground {
                systems::geometry::teleport_to_ground(
                    &mut engine_state.world,
                    &engine_state.geometry,
                    entity,
                    &position,
                );
            } else {
                systems::geometry::teleport_to(&mut engine_state.world, entity, &position);
            }
        }

        systems::helpers::get_component_mut::<EntityClock>(&mut engine_state.world, entity)
            .set_mode(self.time_mode);

        let _ = systems::health::heal_full(&mut engine_state.world, entity);

        systems::resources::recharge(&mut engine_state.world, entity, &RechargeRule::Daily);

        EntityIdentifier::from_world(&engine_state.world, entity)
    }
}
