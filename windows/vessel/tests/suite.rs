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
//! (session_snapshot, the_blocking, tick_commit_budget) via
//! `use crate::common;` rather than its own `mod common;`.

mod common;

#[path = "suite/action_module.rs"]
mod action_module;
#[path = "suite/action_mood.rs"]
mod action_mood;
#[path = "suite/affordance.rs"]
mod affordance;
#[path = "suite/ask_verb.rs"]
mod ask_verb;
#[path = "suite/body_fields.rs"]
mod body_fields;
#[path = "suite/body_mass.rs"]
mod body_mass;
#[path = "suite/clock_lattice.rs"]
mod clock_lattice;
#[path = "suite/coercion_calibration.rs"]
mod coercion_calibration;
#[path = "suite/controller_swap.rs"]
mod controller_swap;
#[path = "suite/corner_rule.rs"]
mod corner_rule;
#[path = "suite/display_handle.rs"]
mod display_handle;
#[path = "suite/doctrine.rs"]
mod doctrine;
#[path = "suite/felt_state_concepts.rs"]
mod felt_state_concepts;
#[path = "suite/furnishing_marks.rs"]
mod furnishing_marks;
#[path = "suite/gate_table.rs"]
mod gate_table;
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
#[path = "suite/noun_entity.rs"]
mod noun_entity;
#[path = "suite/object_property_concepts.rs"]
mod object_property_concepts;
#[path = "suite/octile_cost.rs"]
mod octile_cost;
#[path = "suite/one_roster.rs"]
mod one_roster;
#[path = "suite/ooc_namespace.rs"]
mod ooc_namespace;
#[path = "suite/ooc_objective.rs"]
mod ooc_objective;
#[path = "suite/overrides.rs"]
mod overrides;
#[path = "suite/passage.rs"]
mod passage;
#[path = "suite/player_acts_commit.rs"]
mod player_acts_commit;
#[path = "suite/possess_target.rs"]
mod possess_target;
#[path = "suite/possession_facts.rs"]
mod possession_facts;
#[path = "suite/possession_moves.rs"]
mod possession_moves;
#[path = "suite/session.rs"]
mod session;
#[path = "suite/session_snapshot.rs"]
mod session_snapshot;
#[path = "suite/stance.rs"]
mod stance;
#[path = "suite/strongbox_reachability.rs"]
mod strongbox_reachability;
#[path = "suite/submerged.rs"]
mod submerged;
#[path = "suite/submerged_before_arm.rs"]
mod submerged_before_arm;
#[path = "suite/tableau.rs"]
mod tableau;
#[path = "suite/testimony.rs"]
mod testimony;
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
#[path = "suite/thing.rs"]
mod thing;
#[path = "suite/tick_commit_budget.rs"]
mod tick_commit_budget;
#[path = "suite/underworld_level_generation.rs"]
mod underworld_level_generation;
#[path = "suite/walker_battery.rs"]
mod walker_battery;
#[path = "suite/world_context.rs"]
mod world_context;
