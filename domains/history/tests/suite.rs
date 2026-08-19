//! Consolidated integration-test binary for `hornvale-history`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 3 separate integration-test
//! binaries into 1.

#[path = "suite/descent.rs"]
mod descent;
#[path = "suite/flesh.rs"]
mod flesh;
#[path = "suite/record.rs"]
mod record;
