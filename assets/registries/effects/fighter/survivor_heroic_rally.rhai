fn turn_start_hook(entity, commands) {
    let current_hp = entity.hp_current;
    let max_hp = entity.hp_max;
    if current_hp > 0 && current_hp <= max_hp / 2 {
        commands.heal(
            entity.entity, 
            "0d6 +5", // Die size doesn't matter here since count is 0, but we need to provide a valid expression
            ModifierSet::from(
                ModifierSource::ability("constitution"), 
                entity.ability_modifier("constitution")
            ),
            ModifierSource::effect("nat20_core::effect.fighter.champion.survivor.heroic_rally")
        );
    }
}