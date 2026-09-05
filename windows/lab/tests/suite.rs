//! Consolidated integration-test binary for `hornvale-lab`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 29 separate integration-test
//! binaries into 1.
//!
//! The shared `seed_sweep` helper `the_fare_calibration` and
//! `the_mire_calibration` use for their own preregistered sweeps now lives
//! in `hornvale_worldgen::seed_sweep` (The Governor, Task 9; moved out of a
//! `windows/lab`-only `tests/seed_sweep/` module so `windows/worldgen` and
//! `windows/hearsay` panel tests could reach it too — see that module's own
//! doc comment for why `hornvale-worldgen` was the chosen host). Both
//! callers `use hornvale_worldgen::seed_sweep;` directly; there is nothing
//! left to declare here.

#[path = "suite/affect_trace_golden.rs"]
mod affect_trace_golden;
#[path = "suite/anomaly_holdout.rs"]
mod anomaly_holdout;
#[path = "suite/anomaly_injection.rs"]
mod anomaly_injection;
#[path = "suite/branches_family_calibration.rs"]
mod branches_family_calibration;
#[path = "suite/calibration.rs"]
mod calibration;
#[path = "suite/cascade_firing.rs"]
mod cascade_firing;
#[path = "suite/cave_rate_calibration.rs"]
mod cave_rate_calibration;
#[path = "suite/census_sentinel.rs"]
mod census_sentinel;
#[path = "suite/depth_ladder.rs"]
mod depth_ladder;
#[path = "suite/disposition_calibration.rs"]
mod disposition_calibration;
#[path = "suite/earth_anchor.rs"]
mod earth_anchor;
#[path = "suite/fixture_staleness.rs"]
mod fixture_staleness;
#[path = "suite/gathering_calibration.rs"]
mod gathering_calibration;
#[path = "suite/health_calibration.rs"]
mod health_calibration;
#[path = "suite/hearth_population_calibration.rs"]
mod hearth_population_calibration;
#[path = "suite/individuation.rs"]
mod individuation;
#[path = "suite/metric_roster_safety.rs"]
mod metric_roster_safety;
#[path = "suite/millrace_probe.rs"]
mod millrace_probe;
#[path = "suite/namesake_metrics.rs"]
mod namesake_metrics;
#[path = "suite/preregistration_guard.rs"]
mod preregistration_guard;
#[path = "suite/reticence.rs"]
mod reticence;
#[path = "suite/reticence_calibration.rs"]
mod reticence_calibration;
#[path = "suite/roster_parity.rs"]
mod roster_parity;
#[path = "suite/rung_selection.rs"]
mod rung_selection;
#[path = "suite/site_density.rs"]
mod site_density;
#[path = "suite/terminator_acceptance.rs"]
mod terminator_acceptance;
#[path = "suite/the_dial.rs"]
mod the_dial;
#[path = "suite/the_doctrine.rs"]
mod the_doctrine;
#[path = "suite/the_explanations.rs"]
mod the_explanations;
#[path = "suite/the_fare_calibration.rs"]
mod the_fare_calibration;
#[path = "suite/the_mire_calibration.rs"]
mod the_mire_calibration;
#[path = "suite/tripwire.rs"]
mod tripwire;
#[path = "suite/warp_calibration.rs"]
mod warp_calibration;
#[path = "suite/warp_instrument.rs"]
mod warp_instrument;
#[path = "suite/warp_probe.rs"]
mod warp_probe;
#[path = "suite/wear_funnel.rs"]
mod wear_funnel;
#[path = "suite/weft_density.rs"]
mod weft_density;
