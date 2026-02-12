use hecs::{Entity, World};

use crate::{
    components::{
        id::{SpeciesId, SubspeciesId},
        level_up::LevelUpPrompt,
        modifier::ModifierSource,
        species::{CreatureSize, CreatureType, SpeciesBase},
        speed::Speed,
    },
    engine::game_state::GameState,
    registry::registry::{SpeciesRegistry, SubspeciesRegistry},
    systems,
};

pub enum SpeciesIdentifier {
    Species(SpeciesId),
    Subspecies(SubspeciesId),
}

impl SpeciesIdentifier {
    pub fn modifier_source(&self) -> ModifierSource {
        match self {
            SpeciesIdentifier::Species(id) => ModifierSource::Species(id.clone()),
            SpeciesIdentifier::Subspecies(id) => ModifierSource::Subspecies(id.clone()),
        }
    }
}

pub fn set_species(
    game_state: &mut GameState,
    entity: Entity,
    species: &SpeciesId,
) -> Vec<LevelUpPrompt> {
    let mut prompts = Vec::new();

    let species = SpeciesRegistry::get(&species).expect(&format!(
        "Species with ID `{}` not found in the registry",
        species
    ));

    systems::helpers::set_component::<SpeciesId>(&mut game_state.world, entity, species.id.clone());

    // TODO: The species is presumably always set at level 1?
    apply_species_base(
        game_state,
        entity,
        &species.base,
        SpeciesIdentifier::Species(species.id.clone()),
        1,
    );

    if !species.subspecies.is_empty() {
        prompts.push(LevelUpPrompt::subspecies(&species.id));
    }

    systems::helpers::set_component::<CreatureSize>(
        &mut game_state.world,
        entity,
        species.size.clone(),
    );
    systems::helpers::set_component::<CreatureType>(
        &mut game_state.world,
        entity,
        species.creature_type.clone(),
    );
    systems::helpers::set_component::<Speed>(&mut game_state.world, entity, species.speed.clone());

    prompts
}

pub fn set_subspecies(game_state: &mut GameState, entity: Entity, subspecies: &SubspeciesId) {
    let species_id = systems::helpers::get_component_clone::<SpeciesId>(&game_state.world, entity);

    let species = SpeciesRegistry::get(&species_id).expect(&format!(
        "Species with ID `{}` not found in the registry",
        species_id
    ));

    let subspecies = SubspeciesRegistry::get(&subspecies).expect(&format!(
        "Subspecies with ID `{}` not found in the registry",
        subspecies
    ));

    systems::helpers::set_component::<Option<SubspeciesId>>(
        &mut game_state.world,
        entity,
        Some(subspecies.id.clone()),
    );

    // TODO: Always level 1?
    apply_species_base(
        game_state,
        entity,
        &subspecies.base,
        SpeciesIdentifier::Subspecies(subspecies.id.clone()),
        1,
    );
}

fn apply_species_base(
    game_state: &mut GameState,
    entity: Entity,
    base: &SpeciesBase,
    id: SpeciesIdentifier,
    level: u8,
) {
    if let Some(effects) = base.effects_by_level.get(&level) {
        systems::effects::add_permanent_effects(
            game_state,
            entity,
            effects.clone(),
            &id.modifier_source(),
            None,
        );
    }
    if let Some(actions) = base.actions_by_level.get(&level) {
        systems::actions::add_actions(&mut game_state.world, entity, actions);
    }
}
