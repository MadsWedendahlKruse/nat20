//! Process-wide RNG with a thread-local store.
//!
//! Production code calls `with_rng(|r| ...)` — the same ergonomic shape as
//! `rand::rng()` but routed through a `StdRng` that tests can re-seed via
//! [`seed`] for reproducibility.

use std::cell::RefCell;

use rand::SeedableRng;
use rand::rngs::StdRng;

thread_local! {
    static RNG: RefCell<StdRng> = RefCell::new(StdRng::from_os_rng());
}

pub fn with_rng<R>(f: impl FnOnce(&mut StdRng) -> R) -> R {
    RNG.with(|cell| f(&mut cell.borrow_mut()))
}

#[cfg(any(test, feature = "test-utils"))]
pub fn seed(seed: u64) {
    RNG.with(|cell| *cell.borrow_mut() = StdRng::seed_from_u64(seed));
}

#[cfg(any(test, feature = "test-utils"))]
pub fn reseed_from_os() {
    RNG.with(|cell| *cell.borrow_mut() = StdRng::from_os_rng());
}
