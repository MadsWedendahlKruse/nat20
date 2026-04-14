import "effects/extra_attack_common" as common;

fn action_hook(action_view, entity_view) {
    common::extra_attack_action_hook(action_view, entity_view, 1)
}

fn resource_cost_hook(action_view, entity_view) {
    common::resource_cost_hook(action_view, entity_view)
}
