//! Assertion macros built on `Probe`. Failure messages dump adjacent state
//! (full resource map / available action list / effect list) so a failed
//! assertion points at the cause without further digging.

/// Assert the actor can afford `amount` of the given resource id.
#[macro_export]
macro_rules! assert_resource {
    ($probe:expr, $id:expr, >= $amount:expr) => {{
        let probe = &$probe;
        let id = $id;
        let amount: u8 = $amount;
        if !probe.can_afford(
            id,
            // TODO: Not sure why we need these fully qualified paths here
            &nat20_core::components::resource::ResourceAmount::Flat(amount),
        ) {
            panic!(
                "{}: expected resource '{}' >= {}\nResources:\n{}",
                probe.name(),
                id,
                amount,
                probe.resource_summary(),
            );
        }
    }};
    ($probe:expr, $id:expr, == $amount:expr) => {{
        let probe = &$probe;
        let id = $id;
        let amount: u8 = $amount;
        // "exactly N" = has N AND not has N+1 (when N+1 wouldn't overflow)
        let has_n = probe.can_afford(
            id,
            &nat20_core::components::resource::ResourceAmount::Flat(amount),
        );
        let has_more = amount < u8::MAX
            && probe.can_afford(
                id,
                &nat20_core::components::resource::ResourceAmount::Flat(amount + 1),
            );
        if !has_n || has_more {
            panic!(
                "{}: expected resource '{}' == {} (has_n={}, has_more={})\nResources:\n{}",
                probe.name(),
                id,
                amount,
                has_n,
                has_more,
                probe.resource_summary(),
            );
        }
    }};
}

/// Assert the actor cannot afford even 1 of the resource (or it's missing).
#[macro_export]
macro_rules! assert_no_resource {
    ($probe:expr, $id:expr) => {{
        let probe = &$probe;
        let id = $id;
        if probe.can_afford(id, &nat20_core::components::resource::ResourceAmount::Flat(1)) {
            panic!(
                "{}: expected resource '{}' to be exhausted but actor still has it.\nResources:\n{}",
                probe.name(),
                id,
                probe.resource_summary(),
            );
        }
    }};
}

/// Assert the actor currently has the named effect applied.
#[macro_export]
macro_rules! assert_effect {
    ($probe:expr, $id:expr) => {{
        let probe = &$probe;
        let id = $id;
        if !probe.has_effect(id) {
            panic!(
                "{}: expected effect '{}' to be applied. Active effects: {:?}",
                probe.name(),
                id,
                probe.effect_ids(),
            );
        }
    }};
}

/// Assert the actor does NOT have the named effect applied.
#[macro_export]
macro_rules! assert_no_effect {
    ($probe:expr, $id:expr) => {{
        let probe = &$probe;
        let id = $id;
        if probe.has_effect(id) {
            panic!(
                "{}: expected effect '{}' to be absent but it is applied. Active effects: {:?}",
                probe.name(),
                id,
                probe.effect_ids(),
            );
        }
    }};
}

/// Assert the actor has the named action available.
#[macro_export]
macro_rules! assert_action {
    ($probe:expr, $id:expr) => {{
        let probe = &$probe;
        let id = $id;
        if !probe.has_action(id) {
            panic!(
                "{}: expected action '{}' to be available. Available actions:\n  {}",
                probe.name(),
                id,
                probe
                    .available_action_ids()
                    .iter()
                    .map(|id| id.to_string())
                    .collect::<Vec<_>>()
                    .join("\n  "),
            );
        }
    }};
}

/// Assert the actor does NOT have the named action available.
#[macro_export]
macro_rules! assert_no_action {
    ($probe:expr, $id:expr) => {{
        let probe = &$probe;
        let id = $id;
        if probe.has_action(id) {
            panic!(
                "{}: expected action '{}' to be unavailable but it is available.",
                probe.name(),
                id,
            );
        }
    }};
}

/// Assert the named action is currently on cooldown for this actor.
#[macro_export]
macro_rules! assert_on_cooldown {
    ($probe:expr, $id:expr) => {{
        let probe = &$probe;
        let id = $id;
        if !probe.on_cooldown(id) {
            panic!(
                "{}: expected action '{}' to be on cooldown but it is not.",
                probe.name(),
                id,
            );
        }
    }};
}

/// Assert a reaction is currently pending for this actor, where the action with
/// `id` is one of the available options.
#[macro_export]
macro_rules! assert_pending_reaction {
    ($probe:expr, $id:expr) => {{
        let probe = &$probe;
        let id = $id;
        if !probe.has_pending_reaction(id) {
            panic!(
                "{}: expected a pending reaction to '{}' but none is available.",
                probe.name(),
                id,
            );
        }
    }};
}

#[macro_export]
macro_rules! assert_no_pending_reaction {
    ($probe:expr, $id:expr) => {{
        let probe = &$probe;
        let id = $id;
        if probe.has_pending_reaction(id) {
            panic!(
                "{}: expected no pending reaction to '{}' but one is available.",
                probe.name(),
                id,
            );
        }
    }};
}

/// Compare HP against a value or an existing `u32`.
#[macro_export]
macro_rules! assert_hp {
    ($probe:expr, == $value:expr) => {{
        let probe = &$probe;
        let actual = probe.hp();
        let expected: u32 = $value;
        if actual != expected {
            panic!(
                "{}: expected hp == {}, got {} (max {})",
                probe.name(),
                expected,
                actual,
                probe.max_hp(),
            );
        }
    }};
    ($probe:expr, > $value:expr) => {{
        let probe = &$probe;
        let actual = probe.hp();
        let expected: u32 = $value;
        if !(actual > expected) {
            panic!(
                "{}: expected hp > {}, got {} (max {})",
                probe.name(),
                expected,
                actual,
                probe.max_hp(),
            );
        }
    }};
    ($probe:expr, < $value:expr) => {{
        let probe = &$probe;
        let actual = probe.hp();
        let expected: u32 = $value;
        if !(actual < expected) {
            panic!(
                "{}: expected hp < {}, got {} (max {})",
                probe.name(),
                expected,
                actual,
                probe.max_hp(),
            );
        }
    }};
}
