fn reaction_trigger(context) {
    context.is_own_failed_d20_check("Skill")
}

fn reaction_body(context) {
    let event = context.event;

    if !event.is_d20_check_performed() {
        return ReactionPlan::none();
    }

    let d20_check = event.as_d20_check_performed();
    d20_check.modify_result("1d10");

    event
}
