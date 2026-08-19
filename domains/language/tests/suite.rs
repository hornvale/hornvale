//! Consolidated integration-test binary for `hornvale-language`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 6 separate integration-test
//! binaries into 1.

#[path = "suite/accession_properties.rs"]
mod accession_properties;
#[path = "suite/anthroponym.rs"]
mod anthroponym;
#[path = "suite/anthroponym_render.rs"]
mod anthroponym_render;
#[path = "suite/paradigm_properties.rs"]
mod paradigm_properties;
#[path = "suite/rule_witness.rs"]
mod rule_witness;
#[path = "suite/speakable_properties.rs"]
mod speakable_properties;
