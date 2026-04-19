fn action_result_hook(action_performed_view, entity_view, commands) {
    for result in action_performed_view.results() {
        let kind = result.kind;
        let actor = action_performed_view.action.actor;

        if kind.is_standard() && kind.as_standard().has_attack_miss() {
            commands.apply_effect_for_turns(
                actor,
                result.target,
                "nat20_core::effect.fighter.studied_attacks_advantage",
                1,
                true,
                action_performed_view
            );
        }
    }
}
