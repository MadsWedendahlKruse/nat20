//! Assertion helpers built on `Probe`. Functions over macros — they get
//! `#[track_caller]`, IDE jump-to-definition, and proper type inference.
//! Failure messages dump adjacent state (full resource map / available action
//! list / effect list) so a failed assertion points at the cause without
//! further digging.

use std::fmt::{self, Display};

use crate::test_utils::probe::Probe;

/// Comparison operator paired with its expected value. Used by
/// [`assert_resource`] and [`assert_hp`] to support the full set of `==`,
/// `!=`, `<`, `<=`, `>`, `>=` checks without exploding the surface area into
/// `_eq` / `_lt` / `_gt` variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cmp<T> {
    Equal(T),
    NotEqual(T),
    Less(T),
    Greater(T),
    AtLeast(T),
    AtMost(T),
}

impl<T: PartialOrd> Cmp<T> {
    pub fn check(&self, actual: &T) -> bool {
        match self {
            Cmp::Equal(v) => actual == v,
            Cmp::NotEqual(v) => actual != v,
            Cmp::Less(v) => actual < v,
            Cmp::Greater(v) => actual > v,
            Cmp::AtLeast(v) => actual >= v,
            Cmp::AtMost(v) => actual <= v,
        }
    }
}

impl<T: Display> Display for Cmp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Cmp::Equal(v) => write!(f, "== {v}"),
            Cmp::NotEqual(v) => write!(f, "!= {v}"),
            Cmp::Less(v) => write!(f, "< {v}"),
            Cmp::Greater(v) => write!(f, "> {v}"),
            Cmp::AtLeast(v) => write!(f, ">= {v}"),
            Cmp::AtMost(v) => write!(f, "<= {v}"),
        }
    }
}

// --- Resources ----------------------------------------------------------

/// Assert the entity's flat resource amount satisfies the comparison.
#[track_caller]
pub fn assert_resource(probe: &Probe<'_>, id: &str, cmp: Cmp<u8>) {
    let actual = probe.resource_amount(id);
    if !cmp.check(&actual) {
        panic!(
            "{}: expected resource '{}' {}, got {}\nResources:\n{}",
            probe.name(),
            id,
            cmp,
            actual,
            probe.resource_summary(),
        );
    }
}

/// Convenience: resource is present at zero (or absent entirely).
#[track_caller]
pub fn assert_no_resource(probe: &Probe<'_>, id: &str) {
    assert_resource(probe, id, Cmp::Equal(0));
}

// --- Effects ------------------------------------------------------------

#[track_caller]
pub fn assert_effect(probe: &Probe<'_>, id: &str) {
    if !probe.has_effect(id) {
        panic!(
            "{}: expected effect '{}' to be applied. Active effects: {:?}",
            probe.name(),
            id,
            probe.effect_ids(),
        );
    }
}

#[track_caller]
pub fn assert_no_effect(probe: &Probe<'_>, id: &str) {
    if probe.has_effect(id) {
        panic!(
            "{}: expected effect '{}' to be absent but it is applied. Active effects: {:?}",
            probe.name(),
            id,
            probe.effect_ids(),
        );
    }
}

// --- Actions ------------------------------------------------------------

#[track_caller]
pub fn assert_action(probe: &Probe<'_>, id: &str) {
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
}

#[track_caller]
pub fn assert_no_action(probe: &Probe<'_>, id: &str) {
    if probe.has_action(id) {
        panic!(
            "{}: expected action '{}' to be unavailable but it is available.",
            probe.name(),
            id,
        );
    }
}

#[track_caller]
pub fn assert_on_cooldown(probe: &Probe<'_>, id: &str) {
    if !probe.on_cooldown(id) {
        panic!(
            "{}: expected action '{}' to be on cooldown but it is not.",
            probe.name(),
            id,
        );
    }
}

// --- Reactions ----------------------------------------------------------

#[track_caller]
pub fn assert_pending_reaction(probe: &Probe<'_>, id: &str) {
    if !probe.has_pending_reaction(id) {
        panic!(
            "{}: expected a pending reaction to '{}' but none is available.",
            probe.name(),
            id,
        );
    }
}

#[track_caller]
pub fn assert_no_pending_reaction(probe: &Probe<'_>, id: &str) {
    if probe.has_pending_reaction(id) {
        panic!(
            "{}: expected no pending reaction to '{}' but one is available.",
            probe.name(),
            id,
        );
    }
}

// --- Health -------------------------------------------------------------

#[track_caller]
pub fn assert_hp(probe: &Probe<'_>, cmp: Cmp<u32>) {
    let actual = probe.hp();
    if !cmp.check(&actual) {
        panic!(
            "{}: expected hp {}, got {} (max {})",
            probe.name(),
            cmp,
            actual,
            probe.max_hp(),
        );
    }
}
