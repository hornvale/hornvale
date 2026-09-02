//! Consolidated integration-test binary for `hornvale-locale`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 4 separate integration-test
//! binaries into 1.

#[path = "suite/column_delegation.rs"]
mod column_delegation;
#[path = "suite/site_address_agreement.rs"]
mod site_address_agreement;
#[path = "suite/surface_mixture.rs"]
mod surface_mixture;
#[path = "suite/water_reading.rs"]
mod water_reading;
#[path = "suite/wetness_reading.rs"]
mod wetness_reading;
