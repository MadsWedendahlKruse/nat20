use hecs::{Entity, World};

use crate::{
    components::{id::FeatId, level_up::LevelUpPrompt, modifier::ModifierSource},
    engine::game_state::{self, GameState},
    registry::registry::FeatsRegistry,
    systems,
};

#[derive(Debug, Clone)]
pub enum FeatError {
    RegistryMissing(String),
    PrequisiteNotMet { feat_id: FeatId, entity: Entity },
    AlreadyHasUnrepeatableFeat { feat_id: FeatId, entity: Entity },
}

pub fn feats(world: &World, entity: Entity) -> hecs::Ref<'_, Vec<FeatId>> {
    systems::helpers::get_component::<Vec<FeatId>>(world, entity)
}

pub fn feats_mut(world: &mut World, entity: Entity) -> hecs::RefMut<'_, Vec<FeatId>> {
    systems::helpers::get_component_mut::<Vec<FeatId>>(world, entity)
}

pub fn can_acquire_feat(world: &World, entity: Entity, feat_id: &FeatId) -> Result<(), FeatError> {
    let feat = FeatsRegistry::get(feat_id);
    if feat.is_none() {
        return Err(FeatError::RegistryMissing(feat_id.to_string()));
    }

    let feat = feat.unwrap();

    if !feat.meets_prerequisite(world, entity) {
        return Err(FeatError::PrequisiteNotMet {
            feat_id: feat.id().clone(),
            entity,
        });
    }

    if !feat.is_repeatable() && feats(&world, entity).contains(feat_id) {
        return Err(FeatError::AlreadyHasUnrepeatableFeat {
            feat_id: feat.id().clone(),
            entity,
        });
    }

    Ok(())
}

pub fn add_feat(
    game_state: &mut GameState,
    entity: Entity,
    feat_id: &FeatId,
) -> Result<Vec<LevelUpPrompt>, FeatError> {
    let mut prompts = Vec::new();

    can_acquire_feat(&game_state.world, entity, feat_id)?;
    let feat = FeatsRegistry::get(feat_id).unwrap();

    systems::effects::add_permanent_effects(
        game_state,
        entity,
        feat.effects().clone(),
        &ModifierSource::Feat(feat.id().clone()),
        None,
    );

    prompts.extend(feat.prompts().iter().cloned());

    feats_mut(&mut game_state.world, entity).push(feat.id().clone());

    Ok(prompts)
}
