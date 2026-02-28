export const EXTRA_ATTACK_RESOURCE_ID = "nat20_core::resource.extra_attack";
export const ACTION_RESOURCE_ID = "nat20_core::resource.action";

private fn should_apply_extra_attack(action_view) {
    // 1. Only apply to weapon attacks
    // 2. Only apply if the action costs an action resource, i.e. don't proc extra
    //    attack if the action is a bonus action or reaction
    action_view.action_context.is_attack_action()
        && action_view.resource_cost.costs_resource(global::ACTION_RESOURCE_ID)
}

fn extra_attack_action_hook(action_view, entity_view, max_extra_attacks) {
    if !should_apply_extra_attack(action_view) {
        return;
    }

    // The first time the character triggers Extra Attack their action is
    // consumed and they're given a number of charges of an "Extra Attack"
    // resource.
    // Check if the character has any of those charges (i.e. they've already
    // triggered Extra Attack). Otherwise, give them the "Extra Attack" charges.
    if !entity_view.resources.can_afford_resource(global::EXTRA_ATTACK_RESOURCE_ID, "1") {
        entity_view.resources.add_resource(
            global::EXTRA_ATTACK_RESOURCE_ID,
            max_extra_attacks
        );
    }
}

fn resource_cost_hook(action_view, entity_view) {
    if !should_apply_extra_attack(action_view) {
        return;
    }

    // If the entity has any charges of extra attack use those instead of the action
    if entity_view.resources.can_afford_resource(global::EXTRA_ATTACK_RESOURCE_ID, "1") {
        action_view.resource_cost.replace_resource(
            global::ACTION_RESOURCE_ID,
            global::EXTRA_ATTACK_RESOURCE_ID,
            "1"
        );
    }
}
