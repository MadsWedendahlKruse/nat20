fn action_result_hook(action_performed_view, entity_view, commands) {
    for result in action_performed_view.results() {
        let kind = result.kind;

        if kind.is_standard() && kind.as_standard().has_attack_critical_hit() {
            commands.apply_effect_for_turns(
                action_performed_view.action.actor,
                action_performed_view.action.actor,
                "nat20_core::effect.fighter.champion.remarkable_athlete_disengage",
                1
            );
            return;
        }
    }
}
