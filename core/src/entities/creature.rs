//! Characters and Monsters share a lot of functionality, so might as well put them
//! together in the same file :^)

use std::collections::HashMap;

use hecs::{Bundle, Entity};
use tracing::{debug, error, info, warn};
use uom::si::{f32::Length, length::meter};

use crate::{
    components::{
        ability::AbilityScoreMap,
        actions::{
            action::{ActionCooldownMap, ActionMap, ActionTimeline, default_actions},
            execution::ExecutionStatus,
        },
        activity::{ActivityState, ActivityStateKind, ActivityStateKindTag},
        ai::PlayerControlledTag,
        damage::DamageResistances,
        effects::effect_manager::EffectManager,
        faction::FactionSet,
        health::{
            hit_points::HitPoints,
            life_state::{DeathPolicy, LifeState},
        },
        id::{AIControllerId, BackgroundId, FactionId, FeatId, Name, SpeciesId, SubspeciesId},
        items::{
            equipment::{armor::ArmorTrainingSet, loadout::Loadout, weapon::WeaponProficiencyMap},
            inventory::Inventory,
        },
        level::{ChallengeRating, CharacterLevels},
        resource::ResourceMap,
        saving_throw::SavingThrowSet,
        skill::SkillSet,
        species::{CreatureSize, CreatureType},
        speed::Speed,
        spells::spellbook::Spellbook,
        time::{EntityClock, TimeStep},
    },
    engine::game_state::GameState,
    from_world, registry,
    systems::{
        self,
        entities::EntityKind,
        geometry::Pose,
        movement::{MOVEMENT_SPEED, MoveMode},
    },
};

from_world!(
    #[derive(Bundle, Clone)]
    pub struct Character {
        pub entity_kind: EntityKind,
        /// By default, characters are player controlled. In case the player gets
        /// possessed or mind controlled, this component can be removed from the
        /// entity to make it AI controlled.
        pub player_controlled: PlayerControlledTag,
        /// AI controller for this character. Ignored if `player_controlled` is present.
        pub brain: AIControllerId,
        pub pose: Pose,
        pub time: EntityClock,
        pub activity_state: ActivityState,
        pub name: Name,
        pub species: SpeciesId,
        pub subspecies: Option<SubspeciesId>,
        pub size: CreatureSize,
        pub creature_type: CreatureType,
        pub speed: Speed,
        pub background: BackgroundId,
        pub levels: CharacterLevels,
        pub hit_points: HitPoints,
        pub life_state: LifeState,
        pub death_policy: DeathPolicy,
        pub ability_scores: AbilityScoreMap,
        pub skills: SkillSet,
        pub saving_throws: SavingThrowSet,
        pub resistances: DamageResistances,
        pub weapon_proficiencies: WeaponProficiencyMap,
        pub armor_training: ArmorTrainingSet,
        pub inventory: Inventory,
        pub loadout: Loadout,
        pub spellbook: Spellbook,
        pub resources: ResourceMap,
        pub effects: EffectManager,
        pub feats: Vec<FeatId>,
        pub actions: ActionMap,
        pub cooldowns: ActionCooldownMap,
        pub factions: FactionSet,
    }
);

impl Character {
    pub fn new(name: Name) -> Self {
        Self {
            entity_kind: EntityKind::Character,
            player_controlled: PlayerControlledTag,
            // TODO: Update to an actual ID
            brain: registry::ai::RANDOM_CONTROLLER_ID.clone(),
            pose: Pose::identity(),
            time: EntityClock::new(),
            activity_state: ActivityState::default(),
            name,
            species: SpeciesId::new("nat20_core", ""),
            subspecies: None,
            background: BackgroundId::new("nat20_core", ""),
            size: CreatureSize::Medium,
            creature_type: CreatureType::Humanoid,
            speed: Speed::default(),
            levels: CharacterLevels::new(),
            hit_points: HitPoints::new(1),
            life_state: LifeState::Normal,
            death_policy: DeathPolicy::Unconscious,
            ability_scores: AbilityScoreMap::new(),
            skills: SkillSet::default(),
            saving_throws: SavingThrowSet::default(),
            resistances: DamageResistances::new(),
            armor_training: ArmorTrainingSet::new(),
            weapon_proficiencies: WeaponProficiencyMap::new(),
            loadout: Loadout::new(),
            inventory: Inventory::new(),
            spellbook: Spellbook::new(),
            resources: ResourceMap::default(),
            effects: EffectManager::new(),
            feats: Vec::new(),
            actions: default_actions(),
            cooldowns: HashMap::new(),
            factions: FactionSet::from([FactionId::new("nat20_core", "faction.players")]),
        }
    }
}

impl Default for Character {
    fn default() -> Self {
        Character::new(Name::new("John Doe"))
    }
}

from_world!(
    #[derive(Bundle, Clone)]
    pub struct Monster {
        pub entity_kind: EntityKind,
        pub name: Name,
        // Monsters can be player controlled e.g. if they're Mind Controlled
        pub brain: AIControllerId,
        pub pose: Pose,
        pub time: EntityClock,
        pub activity_state: ActivityState,
        pub challenge_rating: ChallengeRating,
        pub hit_points: HitPoints,
        pub life_state: LifeState,
        pub death_policy: DeathPolicy,
        pub size: CreatureSize,
        pub creature_type: CreatureType,
        pub speed: Speed,
        pub abilities: AbilityScoreMap,
        pub skills: SkillSet,
        pub saving_throws: SavingThrowSet,
        pub resistances: DamageResistances,
        // TODO: alignment?
        // TODO: ArmorClass or just Loadout?
        pub loadout: Loadout,
        pub spellbook: Spellbook,
        pub resources: ResourceMap,
        pub effects: EffectManager,
        pub actions: ActionMap,
        pub cooldowns: ActionCooldownMap,
        pub weapon_proficiencies: WeaponProficiencyMap,
        pub armor_training: ArmorTrainingSet,
        pub factions: FactionSet,
    }
);

impl Monster {
    pub fn new(
        name: Name,
        brain: AIControllerId,
        challenge_rating: ChallengeRating,
        hit_points: HitPoints,
        size: CreatureSize,
        creature_type: CreatureType,
        speed: Speed,
        abilities: AbilityScoreMap,
        factions: FactionSet,
    ) -> Self {
        Self {
            entity_kind: EntityKind::Monster,
            name,
            brain,
            pose: Pose::default(),
            time: EntityClock::new(),
            activity_state: ActivityState::default(),
            challenge_rating,
            hit_points,
            life_state: LifeState::Normal,
            death_policy: DeathPolicy::Die,
            size,
            creature_type,
            speed,
            abilities,
            skills: SkillSet::default(),
            saving_throws: SavingThrowSet::default(),
            resistances: DamageResistances::default(),
            loadout: Loadout::default(),
            spellbook: Spellbook::new(),
            resources: ResourceMap::default(),
            effects: EffectManager::new(),
            actions: default_actions(),
            cooldowns: ActionCooldownMap::default(),
            weapon_proficiencies: WeaponProficiencyMap::new(),
            armor_training: ArmorTrainingSet::default(),
            factions,
        }
    }
}

pub fn update(game_state: &mut GameState, delta_time: f32, entity: Entity) {
    let time_step = TimeStep::RealTime {
        delta_seconds: delta_time,
    };

    systems::time::advance_time(game_state, entity, time_step);

    update_effects(game_state, entity);

    handle_ai(game_state, entity);

    update_activity(game_state, delta_time, entity);
}

fn update_effects(game_state: &mut GameState, entity: Entity) {
    let marked_effects =
        systems::effects::effects_mut(&mut game_state.world, entity).take_marked_for_removal();
    if !marked_effects.is_empty() {
        systems::effects::remove_effects(
            game_state,
            entity,
            &marked_effects.into_iter().collect::<Vec<_>>(),
        );
    }
}

fn handle_ai(game_state: &mut GameState, entity: Entity) {
    if !systems::ai::is_player_controlled(&game_state.world, entity)
        && systems::helpers::get_component::<ActivityState>(&game_state.world, entity).is_idle()
        && let Some(prompt) = game_state.next_prompt_entity(entity).cloned()
        && prompt.kind.actors().contains(&entity)
    {
        if let Some(activity) = systems::ai::decide_activity(game_state, &prompt, entity) {
            let result = game_state.submit_activity(activity);
            info!("AI submitted activity: {:?}", result);
        } else {
            game_state.end_turn(entity);
        }
    }
}

fn update_activity(game_state: &mut GameState, delta_time: f32, entity: Entity) {
    let kind = {
        let activity = systems::helpers::get_component::<ActivityState>(&game_state.world, entity);

        if activity.is_paused() {
            return;
        }

        ActivityStateKindTag::from(&activity.state)
    };

    match kind {
        ActivityStateKindTag::Idle => { /* Do nothing */ }
        ActivityStateKindTag::Moving => update_moving(game_state, delta_time, entity),
        ActivityStateKindTag::Acting => update_acting(game_state, delta_time, entity),
        ActivityStateKindTag::Displaced => update_displaced(game_state, delta_time, entity),
    }
}

fn update_moving(game_state: &mut GameState, delta_time: f32, entity: Entity) {
    let target_point = {
        let activity = systems::helpers::get_component::<ActivityState>(&game_state.world, entity);
        let ActivityStateKind::Moving {
            path,
            current_target,
            ..
        } = &activity.state
        else {
            return;
        };

        if *current_target >= path.points.len() {
            warn!(
                "Current target index {} is out of bounds for path with length {}. Target appears to have reached goal, but follow up state was not set correctly. Did the action submission fail?",
                current_target,
                path.points.len()
            );
            None
        } else {
            Some(path.points[*current_target])
        }
    };

    let Some(target_point) = target_point else {
        warn!(
            "Entity {:?} is moving but has no target point. Setting to idle.",
            entity
        );
        systems::helpers::get_component_mut::<ActivityState>(&mut game_state.world, entity)
            .set_idle();
        return;
    };

    let position = systems::geometry::get_foot_position(&game_state.world, entity).unwrap();
    let direction = target_point - position;
    let distance_to_target = direction.norm();

    if distance_to_target != 0.0 {
        systems::movement::move_entity(
            game_state,
            entity,
            &(position + direction.normalize() * MOVEMENT_SPEED * delta_time),
            MoveMode::Voluntary,
        );
    }

    // Haven't reached the target yet
    if distance_to_target >= MOVEMENT_SPEED * delta_time {
        return;
    }

    // Actually reached the target. We're reborrowing since moving can trigger an
    // opportunity attack which can change the activity state
    let follow_up = {
        let Ok(mut activity) = game_state.world.get::<&mut ActivityState>(entity) else {
            return;
        };
        let ActivityStateKind::Moving {
            path,
            current_target,
            action,
        } = &mut activity.state
        else {
            return;
        };

        let path_length = path.points.len();
        *current_target += 1;

        // Haven't reached the end of the path yet
        if *current_target < path_length {
            return;
        }

        // Reached the end of the path
        debug!("Entity {:?} reached destination {:?}", entity, target_point);

        let follow_up = action.take();
        if follow_up.is_none() {
            debug!(
                "Entity {:?} has no follow-up action, setting to idle",
                entity
            );
            activity.set_idle();
        }
        follow_up
    };

    let Some(action_decision) = follow_up else {
        return;
    };

    debug!(
        "Entity {:?} has a follow-up action, setting to act after movement",
        entity
    );
    if let Err(error) = game_state.submit_decision(action_decision) {
        error!("Failed to submit action decision: {:?}", error);
    }
}

fn update_acting(game_state: &mut GameState, delta_time: f32, entity: Entity) {
    let status = systems::actions::execution_status(game_state, entity);

    let (advance, finished) = {
        let Ok(mut activity) = game_state.world.get::<&mut ActivityState>(entity) else {
            return;
        };
        let ActivityStateKind::Acting {
            timeline:
                ActionTimeline {
                    total_duration,
                    perform_time,
                    step_spacing,
                },
            elapsed_time,
            phase_cooldown,
        } = &mut activity.state
        else {
            return;
        };

        *elapsed_time += delta_time;

        let mut advance = false;
        if *elapsed_time >= *perform_time && status == Some(ExecutionStatus::Running) {
            *phase_cooldown += delta_time;

            if *phase_cooldown >= *step_spacing {
                debug!(
                    "Action phase cooldown elapsed for entity {:?}, advancing execution",
                    entity
                );
                *phase_cooldown = 0.0;
                advance = true;
            }
        }

        let finished = *elapsed_time >= *total_duration
            && status.is_none_or(|status| status == ExecutionStatus::Done);

        if finished {
            debug!(
                "Entity {:?} finished action after {:?} seconds",
                entity, total_duration
            );
            activity.set_idle();
        }

        (advance, finished)
    };

    if advance {
        systems::actions::advance_execution(game_state, entity);
    }

    if !finished {
        return;
    }

    debug!(
        "Action completed for entity {:?}, clearing blockers and resuming pending events if ready",
        entity
    );

    let scope = game_state.scope_for_entity(entity);
    game_state.action_executions.remove(&entity);
    game_state
        .interaction_engine
        .session_mut(scope)
        .clear_blocker(entity);
    game_state.resume_pending_events_if_ready(scope);
}

fn update_displaced(game_state: &mut GameState, delta_time: f32, entity: Entity) {
    let (new_position, fall_distance) = {
        let Ok(mut activity) = game_state.world.get::<&mut ActivityState>(entity) else {
            return;
        };
        let ActivityStateKind::Displaced {
            trajectory,
            elapsed_time,
        } = &mut activity.state
        else {
            return;
        };

        let (max_time, origin_y) = (trajectory.max_time, trajectory.origin.y);

        *elapsed_time += delta_time;
        let new_position = trajectory.position_at_time(elapsed_time.min(max_time));

        if *elapsed_time < max_time {
            (new_position, None)
        } else {
            debug!(
                "Entity {:?} finished displacement after {:?} seconds",
                entity, max_time
            );
            (
                new_position,
                Some(Length::new::<meter>(origin_y - new_position.y)),
            )
        }
    };

    systems::movement::move_entity(game_state, entity, &new_position, MoveMode::Displace);

    let Some(fall_distance) = fall_distance else {
        return;
    };

    systems::movement::apply_fall_damage(game_state, entity, fall_distance);

    let Ok(mut activity) = game_state.world.get::<&mut ActivityState>(entity) else {
        return;
    };
    if matches!(activity.state, ActivityStateKind::Displaced { .. }) {
        activity.set_idle();
    }
}
