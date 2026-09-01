//! Consolidated integration-test binary for `hornvale-scene`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 3 separate integration-test
//! binaries into 1.
//!
//! `tests/common/` is shared test-only scaffolding declared once here
//! (ordinary module resolution, since this file sits directly in
//! `tests/`, sibling to `common/`) and reached from `illumination_hypotheses`
//! via `use crate::common;` rather than its own `mod common;`.

mod common;

#[path = "suite/golden.rs"]
mod golden;
#[path = "suite/illumination_hypotheses.rs"]
mod illumination_hypotheses;
#[path = "suite/one_projection.rs"]
mod one_projection;
#[path = "suite/wind_contract.rs"]
mod wind_contract;
