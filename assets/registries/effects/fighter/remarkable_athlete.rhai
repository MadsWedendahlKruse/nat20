fn action_result_hook(action_performed_view, entity_view) {
    for result in action_performed_view.results() {
        let kind = result.kind;

        if kind.is_standard() && kind.as_standard().has_critical_hit() {
            entity_view.apply_effect_for_turns(
                "nat20_core::effect.fighter.champion.remarkable_athlete_disengage",
                1
            );
            return;
        }
    }
}
