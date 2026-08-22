//! Consolidated integration-test binary for `hornvale-vessel`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 22 separate integration-test
//! binaries into 1.
//!
//! `tests/common/` is shared test-only scaffolding declared once here
//! (ordinary module resolution, since this file sits directly in
//! `tests/`, sibling to `common/`) and reached from each consuming file
//! (course_properties, session_snapshot, the_blocking) via
//! `use crate::common;` rather than its own `mod common;`.

mod common;

#[path = "suite/action_module.rs"]
mod action_module;
#[path = "suite/body_mass.rs"]
mod body_mass;
#[path = "suite/course_properties.rs"]
mod course_properties;
#[path = "suite/display_handle.rs"]
mod display_handle;
#[path = "suite/lantern_fabric.rs"]
mod lantern_fabric;
#[path = "suite/lantern_lens.rs"]
mod lantern_lens;
#[path = "suite/lantern_light.rs"]
mod lantern_light;
#[path = "suite/lantern_night.rs"]
mod lantern_night;
#[path = "suite/lantern_seam.rs"]
mod lantern_seam;
#[path = "suite/ledger_query_equivalence.rs"]
mod ledger_query_equivalence;
#[path = "suite/liveness_genesis.rs"]
mod liveness_genesis;
#[path = "suite/possess_target.rs"]
mod possess_target;
#[path = "suite/possession_moves.rs"]
mod possession_moves;
#[path = "suite/session.rs"]
mod session;
#[path = "suite/session_snapshot.rs"]
mod session_snapshot;
#[path = "suite/submerged.rs"]
mod submerged;
#[path = "suite/submerged_before_arm.rs"]
mod submerged_before_arm;
#[path = "suite/the_blocking.rs"]
mod the_blocking;
#[path = "suite/the_first_mark.rs"]
mod the_first_mark;
#[path = "suite/the_handle.rs"]
mod the_handle;
#[path = "suite/the_lintel.rs"]
mod the_lintel;
#[path = "suite/the_purview.rs"]
mod the_purview;
#[path = "suite/underworld_level_generation.rs"]
mod underworld_level_generation;
#[path = "suite/walker_battery.rs"]
mod walker_battery;
#[path = "suite/world_context.rs"]
mod world_context;
