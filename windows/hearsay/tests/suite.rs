//! Consolidated integration-test binary for `hornvale-hearsay`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 23 separate integration-test
//! binaries into 1.
//!
//! `tests/common/` is shared test-only scaffolding declared once here
//! (ordinary module resolution, since this file sits directly in
//! `tests/`, sibling to `common/`) and reached from every consuming file
//! below via `use crate::common;` rather than its own `mod common;`.

mod common;

#[path = "suite/accumulate.rs"]
mod accumulate;
#[path = "suite/amplitude.rs"]
mod amplitude;
#[path = "suite/augmented_walk.rs"]
mod augmented_walk;
#[path = "suite/clock.rs"]
mod clock;
#[path = "suite/contact.rs"]
mod contact;
#[path = "suite/cupel_fine_ladder.rs"]
mod cupel_fine_ladder;
#[path = "suite/cupel_penalty.rs"]
mod cupel_penalty;
#[path = "suite/derive.rs"]
mod derive;
#[path = "suite/divergence.rs"]
mod divergence;
#[path = "suite/durations.rs"]
mod durations;
#[path = "suite/hop_depth_seed42.rs"]
mod hop_depth_seed42;
#[path = "suite/ladder.rs"]
mod ladder;
#[path = "suite/lineage.rs"]
mod lineage;
#[path = "suite/palimpsest_readout.rs"]
mod palimpsest_readout;
#[path = "suite/palimpsest_readout_units.rs"]
mod palimpsest_readout_units;
#[path = "suite/parley_readout.rs"]
mod parley_readout;
#[path = "suite/probe_contact_substrate.rs"]
mod probe_contact_substrate;
#[path = "suite/probe_filter_mismatch.rs"]
mod probe_filter_mismatch;
#[path = "suite/probe_filter_variation.rs"]
mod probe_filter_variation;
#[path = "suite/probe_lossy_quadrants.rs"]
mod probe_lossy_quadrants;
#[path = "suite/probe_stance_cost.rs"]
mod probe_stance_cost;
#[path = "suite/probe_teller_relations.rs"]
mod probe_teller_relations;
#[path = "suite/retelling_readout_seed42.rs"]
mod retelling_readout_seed42;
#[path = "suite/stance.rs"]
mod stance;
#[path = "suite/transmission.rs"]
mod transmission;
