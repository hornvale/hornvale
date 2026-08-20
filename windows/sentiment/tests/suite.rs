//! Consolidated integration-test binary for `hornvale-sentiment` (the
//! test-binary consolidation convention: one `tests/*.rs` compilation unit
//! per crate, with every actual test file living under `tests/suite/` and
//! declared here via an explicit `#[path]`).

#[path = "suite/axes.rs"]
mod axes;
#[path = "suite/axis_spread_probe.rs"]
mod axis_spread_probe;
#[path = "suite/believability.rs"]
mod believability;
#[path = "suite/judgment.rs"]
mod judgment;
