//! Consolidated integration-test binary for `hornvale-terrain`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 8 separate integration-test
//! binaries into 1.

#[path = "suite/carve_properties.rs"]
mod carve_properties;
#[path = "suite/cave_construction_proof.rs"]
mod cave_construction_proof;
#[path = "suite/channel_golden.rs"]
mod channel_golden;
#[path = "suite/channel_properties.rs"]
mod channel_properties;
#[path = "suite/rift_probe.rs"]
mod rift_probe;
#[path = "suite/rill_probe.rs"]
mod rill_probe;
#[path = "suite/rill_properties.rs"]
mod rill_properties;
#[path = "suite/tectonic_properties.rs"]
mod tectonic_properties;
