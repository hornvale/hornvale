//! Consolidated integration-test binary for `hornvale`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 42 separate integration-test
//! binaries into 1.

#[path = "suite/accession.rs"]
mod accession;
#[path = "suite/anchor_thing_correspondence.rs"]
mod anchor_thing_correspondence;
#[path = "suite/architecture.rs"]
mod architecture;
#[path = "suite/attest.rs"]
mod attest;
#[path = "suite/audio_artifacts.rs"]
mod audio_artifacts;
#[path = "suite/branches_coverage.rs"]
mod branches_coverage;
#[path = "suite/branches_identity.rs"]
mod branches_identity;
#[path = "suite/build_path_embedding.rs"]
mod build_path_embedding;
#[path = "suite/cave_kind_correspondence.rs"]
mod cave_kind_correspondence;
#[path = "suite/census_duration.rs"]
mod census_duration;
#[path = "suite/claim_shape.rs"]
mod claim_shape;
#[path = "suite/client_band_coverage.rs"]
mod client_band_coverage;
#[path = "suite/common_is_total.rs"]
mod common_is_total;
#[path = "suite/correspondence.rs"]
mod correspondence;
#[path = "suite/docs_consistency.rs"]
mod docs_consistency;
#[path = "suite/exit_criterion.rs"]
mod exit_criterion;
#[path = "suite/generated_paths.rs"]
mod generated_paths;
#[path = "suite/graph_cost.rs"]
mod graph_cost;
#[path = "suite/heavy_tier.rs"]
mod heavy_tier;
#[path = "suite/history_battery.rs"]
mod history_battery;
#[path = "suite/history_render.rs"]
mod history_render;
#[path = "suite/id_shift_invariance.rs"]
mod id_shift_invariance;
#[path = "suite/id_stability_under_insertion.rs"]
mod id_stability_under_insertion;
#[path = "suite/lane_claim_roundtrip.rs"]
mod lane_claim_roundtrip;
#[path = "suite/lane_sets.rs"]
mod lane_sets;
#[path = "suite/lexicon_guard.rs"]
mod lexicon_guard;

#[path = "suite/lens_purity.rs"]
mod lens_purity;
#[path = "suite/locale_cli.rs"]
mod locale_cli;
#[path = "suite/no_entity_id_values_in_prose.rs"]
mod no_entity_id_values_in_prose;
#[path = "suite/prose_is_not_a_contract.rs"]
mod prose_is_not_a_contract;
#[path = "suite/provision.rs"]
mod provision;
#[path = "suite/release_determinism.rs"]
mod release_determinism;
#[path = "suite/repertory_corpus.rs"]
mod repertory_corpus;
#[path = "suite/repose_byte_identity.rs"]
mod repose_byte_identity;
#[path = "suite/retired_gate_signposts.rs"]
mod retired_gate_signposts;
#[path = "suite/scene_context_discipline.rs"]
mod scene_context_discipline;
#[path = "suite/scene_cost.rs"]
mod scene_cost;
#[path = "suite/scene_moons_cli.rs"]
mod scene_moons_cli;
#[path = "suite/scene_surrounds_colour_cli.rs"]
mod scene_surrounds_colour_cli;
#[path = "suite/sentence_corpus.rs"]
mod sentence_corpus;
#[path = "suite/session_cost.rs"]
mod session_cost;
#[path = "suite/sky_exit_criterion.rs"]
mod sky_exit_criterion;
#[path = "suite/star_class_is_a_concept.rs"]
mod star_class_is_a_concept;
#[path = "suite/subfloor_roster_coverage.rs"]
mod subfloor_roster_coverage;
#[path = "suite/system_coverage.rs"]
mod system_coverage;
#[path = "suite/test_binary_ratchet.rs"]
mod test_binary_ratchet;
#[path = "suite/the_unnameable.rs"]
mod the_unnameable;
#[path = "suite/timings_alarm.rs"]
mod timings_alarm;
#[path = "suite/trope_coverage.rs"]
mod trope_coverage;
#[path = "suite/walk_depth_agreement.rs"]
mod walk_depth_agreement;
