fn action_result_hook(action_performed_view, entity_view, commands) {
    if action_performed_view.action.action_id == "nat20_core::action.fighter.second_wind" {
        commands.apply_effect_for_turns(
            action_performed_view.action.actor,
            action_performed_view.action.actor,
            "nat20_core::effect.fighter.tactical_shift_disengage",
            1,
            false,
            action_performed_view
        );
    }
}
