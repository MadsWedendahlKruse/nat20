fn reaction_trigger(context) {
    let event = context.event;

    if !event.is_moving_out_of_reach() {
        return false;
    }

    let moving_out_of_reach = event.as_moving_out_of_reach();

    moving_out_of_reach.entity == context.reactor
}