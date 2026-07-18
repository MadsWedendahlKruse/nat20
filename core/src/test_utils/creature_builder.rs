use std::{collections::BTreeMap, sync::LazyLock};

use hecs::Entity;
use parry3d::na::Point3;

use crate::{
    components::{
        id::EntityIdentifier,
        resource::RechargeRule,
        time::{EntityClock, TimeMode},
    },
    engine::game_state::GameState,
    systems,
    test_utils::{
        creature_probe::CreatureProbe,
        fixtures::creatures::{heroes, monsters},
    },
};

/// `(game_state, level/challenge_rating, optional_entity_to_spawn_at) -> spawned`
pub type CreatureTemplateFunction = fn(&mut GameState, u8, Option<Entity>) -> EntityIdentifier;

pub static CREATURE_TEMPLATES: LazyLock<BTreeMap<&'static str, CreatureTemplateFunction>> =
    LazyLock::new(|| {
        BTreeMap::from([
            (
                "hero.barbarian",
                heroes::barbarian as CreatureTemplateFunction,
            ),
            ("hero.fighter", heroes::fighter as CreatureTemplateFunction),
            ("hero.wizard", heroes::wizard as CreatureTemplateFunction),
            ("hero.warlock", heroes::warlock as CreatureTemplateFunction),
            (
                "monster.goblin_warrior",
                monsters::goblin_warrior as CreatureTemplateFunction,
            ),
        ])
    });

// TODO: Not sure where this should live
pub struct CreatureBuilder {
    template: CreatureTemplateFunction,
    level: u8,
    position: Option<(Point3<f32>, bool)>,
    time_mode: TimeMode,
    entity_id: Option<Entity>,
}

impl CreatureBuilder {
    pub fn new(template: &str) -> Self {
        Self {
            template: *CREATURE_TEMPLATES
                .get(template)
                .unwrap_or_else(|| panic!("No creature template with name {template}")),
            level: 1,
            time_mode: TimeMode::RealTime,
            position: None,
            entity_id: None,
        }
    }

    pub fn level(&mut self, level: u8) -> &mut Self {
        self.level = level;
        self
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

    pub fn spawn(&mut self, game_state: &mut GameState) -> EntityIdentifier {
        let entity_id = (self.template)(game_state, self.level, self.entity_id);
        let entity = entity_id.id();

        if let Some((position, on_ground)) = self.position {
            if on_ground {
                systems::geometry::teleport_to_ground(
                    &mut game_state.world,
                    &game_state.geometry,
                    entity,
                    &position,
                );
            } else {
                systems::geometry::teleport_to(&mut game_state.world, entity, &position);
            }
        }

        systems::helpers::get_component_mut::<EntityClock>(&mut game_state.world, entity)
            .set_mode(self.time_mode);

        let _ = systems::health::heal_full(&mut game_state.world, entity);

        systems::resources::recharge(&mut game_state.world, entity, &RechargeRule::Daily);

        entity_id
    }

    pub fn probe(&mut self, game_state: &mut GameState) -> CreatureProbe {
        let entity_id = self.spawn(game_state);
        CreatureProbe::new(entity_id)
    }
}
