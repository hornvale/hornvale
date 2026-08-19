//! Consolidated integration-test binary for `hornvale-climate`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 6 separate integration-test
//! binaries into 1.

#[path = "suite/coarse_constrains_fine.rs"]
mod coarse_constrains_fine;
#[path = "suite/column.rs"]
mod column;
#[path = "suite/facets.rs"]
mod facets;
#[path = "suite/held_out_marine.rs"]
mod held_out_marine;
#[path = "suite/preregistration.rs"]
mod preregistration;
#[path = "suite/underworld.rs"]
mod underworld;
