fn action_result_hook(action_performed_view, entity_view) {
    if action_performed_view.action.action_id == "nat20_core::action.fighter.second_wind" {
        entity_view.apply_effect_for_turns(
            "nat20_core::effect.fighter.tactical_shift_disengage",
            1
        );
    }
}
