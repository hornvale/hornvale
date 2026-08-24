//! Consolidated integration-test binary for `hornvale-species`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 5 separate integration-test
//! binaries into 1.

#[path = "suite/biome_affinity.rs"]
mod biome_affinity;
#[path = "suite/coverage.rs"]
mod coverage;
#[path = "suite/instance_lens.rs"]
mod instance_lens;
#[path = "suite/life_history_golden.rs"]
mod life_history_golden;
#[path = "suite/potency_assay.rs"]
mod potency_assay;
#[path = "suite/social_form.rs"]
mod social_form;
