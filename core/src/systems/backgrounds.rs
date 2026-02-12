use hecs::{Entity, World};

use crate::{
    components::{
        id::BackgroundId,
        level_up::LevelUpPrompt,
        modifier::ModifierSource,
        proficiency::{Proficiency, ProficiencyLevel},
        skill::SkillSet,
    },
    engine::game_state::GameState,
    registry::registry::BackgroundsRegistry,
    systems,
};

pub fn background(world: &World, entity: Entity) -> hecs::Ref<'_, BackgroundId> {
    systems::helpers::get_component::<BackgroundId>(world, entity)
}

pub fn background_mut(world: &mut World, entity: Entity) -> hecs::RefMut<'_, BackgroundId> {
    systems::helpers::get_component_mut::<BackgroundId>(world, entity)
}

pub fn set_background(
    game_state: &mut GameState,
    entity: Entity,
    background_id: &BackgroundId,
) -> Vec<LevelUpPrompt> {
    let background = BackgroundsRegistry::get(background_id).expect(&format!(
        "Background with ID `{}` not found in the registry",
        background_id
    ));

    *background_mut(&mut game_state.world, entity) = background_id.clone();

    let feat_result = systems::feats::add_feat(game_state, entity, &background.feat);
    if let Err(e) = feat_result {
        // TODO: Not sure what to do here
        panic!("Error adding background feat: {:?}", e);
    }
    let mut prompts = feat_result.unwrap();

    let mut skill_set =
        systems::helpers::get_component_mut::<SkillSet>(&mut game_state.world, entity);
    for skill in background.skill_proficiencies {
        skill_set.set_proficiency(
            &skill,
            Proficiency::new(
                ProficiencyLevel::Proficient,
                ModifierSource::Background(background_id.clone()),
            ),
        );
    }

    // Set tool proficiencies

    // Set languages

    prompts.push(LevelUpPrompt::Choice(background.equipment.clone()));

    prompts
}
