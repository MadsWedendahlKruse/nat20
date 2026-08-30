use std::ops::Deref;

use hecs::{Entity, World};

use crate::{
    components::{
        faction::{Attitude, AttitudeOverride, Faction, FactionSet},
        id::FactionId,
    },
    registry::registry::FactionsRegistry,
};

pub fn get_faction(faction_id: &FactionId) -> &Faction {
    FactionsRegistry::get(faction_id)
        .unwrap_or_else(|| panic!("Faction with ID `{}` not found in the registry", faction_id))
}

pub fn attitude_from_to(world: &World, source: Entity, destination: Entity) -> Attitude {
    if source == destination {
        return Attitude::Friendly;
    }

    // 1) Source-side entity override wins outright
    if let Ok(overrides) = world.get::<&AttitudeOverride>(source)
        && let Some(attitude) = overrides.entities.get(&destination)
    {
        return *attitude;
    }

    let source_factions = &world.get::<&FactionSet>(source).ok();
    let desination_factions = &world.get::<&FactionSet>(destination).ok();

    // 2) If source has no factions, it has no opinion -> Neutral (unless an override above)
    if source_factions.is_none() && desination_factions.is_none() {
        return Attitude::Neutral;
    }
    if source_factions.is_none() {
        return Attitude::Neutral;
    }

    let source_factions = source_factions.as_deref().unwrap();

    // 3) Source-side faction overrides (toward any of dst's factions) take precedence
    if let (Some(desination_factions), Ok(overrides)) =
        (desination_factions, world.get::<&AttitudeOverride>(source))
    {
        let mut best: Option<Attitude> = None;
        for faction in desination_factions.deref() {
            if let Some(attitude) = overrides.factions.get(faction) {
                // most hostile wins
                best = Some(best.map_or(*attitude, |other| other.max(*attitude)));
            }
        }
        if let Some(attitude) = best {
            return attitude;
        }
    }

    // 4) Fold across all (src_faction x dst_faction) with max-hostility semantics
    //    If dst has no factions, use each src faction's default_cross_attitude.
    let mut best = Attitude::Friendly; // rely on enum order: Friendly < Neutral < Hostile
    match &desination_factions {
        Some(desination_factions) => {
            for source_faction in source_factions {
                let source_faction = get_faction(source_faction);
                for destination_faction in desination_factions.deref() {
                    let destination_faction = get_faction(destination_faction);
                    best = best.max(source_faction.attitude_towards(destination_faction));
                }
            }
        }
        None => {
            for source_faction in source_factions {
                let source_faction = get_faction(source_faction);
                best = best.max(source_faction.default_cross_attitude());
            }
        }
    }

    best
}

// UI coloring from viewer A's perspective: are they dangerous to me?
pub fn perceived_threat(world: &World, viewer: Entity, other: Entity) -> Attitude {
    attitude_from_to(world, other, viewer) // note the flipped order
}

// Sometimes handy: a symmetric summary ("at war" if either side hostile)
pub fn mutual_attitude(world: &World, a: Entity, b: Entity) -> Attitude {
    attitude_from_to(world, a, b).max(attitude_from_to(world, b, a))
}
