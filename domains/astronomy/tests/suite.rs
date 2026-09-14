//! Consolidated integration-test binary for `hornvale-astronomy`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 4 separate integration-test
//! binaries into 1.

#[path = "suite/anchor_coherence.rs"]
mod anchor_coherence;
#[path = "suite/anchor_radius.rs"]
mod anchor_radius;
#[path = "suite/anchor_state.rs"]
mod anchor_state;
#[path = "suite/calendar_negative_time.rs"]
mod calendar_negative_time;
#[path = "suite/constellation_candidates.rs"]
mod constellation_candidates;
#[path = "suite/day_is_a_whole_tick_count.rs"]
mod day_is_a_whole_tick_count;
#[path = "suite/eclipse_rhythm_view.rs"]
mod eclipse_rhythm_view;
#[path = "suite/genesis_properties.rs"]
mod genesis_properties;
#[path = "suite/golden_seed_42.rs"]
mod golden_seed_42;
#[path = "suite/night_sky_regimes.rs"]
mod night_sky_regimes;
#[path = "suite/planetarium_geometry.rs"]
mod planetarium_geometry;
#[path = "suite/sky_conformance.rs"]
mod sky_conformance;
