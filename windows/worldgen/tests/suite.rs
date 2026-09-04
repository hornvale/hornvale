//! Consolidated integration-test binary for `hornvale-worldgen`.
//!
//! Cargo compiles each top-level `tests/*.rs` file as its own
//! compilation unit but does not compile subdirectories, so every test
//! file that used to live directly under `tests/` now lives under
//! `tests/suite/` and is declared here as a module (with an explicit
//! `#[path]` because this file is itself a crate root, whose default
//! module search looks beside itself in `tests/`, not in
//! `tests/suite/`). This collapses 93 separate integration-test
//! binaries into 1, which is most of `gate-commit`'s
//! per-compilation-unit system-time cost for this crate. See
//! `.superpowers/sdd/consolidate-worldgen-tests-brief.md`.

#[path = "suite/approach_ease_calibration.rs"]
mod approach_ease_calibration;
#[path = "suite/artifacts.rs"]
mod artifacts;
#[path = "suite/axis_geometry.rs"]
mod axis_geometry;
#[path = "suite/beholding_probe.rs"]
mod beholding_probe;
#[path = "suite/beta_calibration_freeze.rs"]
mod beta_calibration_freeze;
#[path = "suite/beta_calibration_sweep.rs"]
mod beta_calibration_sweep;
#[path = "suite/branch_character.rs"]
mod branch_character;
#[path = "suite/breach.rs"]
mod breach;
#[path = "suite/capacity_cost_probe.rs"]
mod capacity_cost_probe;
#[path = "suite/chorus_params.rs"]
mod chorus_params;
#[path = "suite/color_naming.rs"]
mod color_naming;
#[path = "suite/confluence.rs"]
mod confluence;
#[path = "suite/consonance_properties.rs"]
mod consonance_properties;
#[path = "suite/deep_grammar.rs"]
mod deep_grammar;
#[path = "suite/deep_realm_chamber.rs"]
mod deep_realm_chamber;
#[path = "suite/deep_realm_mutation.rs"]
mod deep_realm_mutation;
#[path = "suite/deep_realm_rehome.rs"]
mod deep_realm_rehome;
#[path = "suite/deep_realm_substrate.rs"]
mod deep_realm_substrate;
#[path = "suite/defensibility_field.rs"]
mod defensibility_field;
#[path = "suite/delve_depth.rs"]
mod delve_depth;
#[path = "suite/delver_bind_audit.rs"]
mod delver_bind_audit;
#[path = "suite/delver_depth_probe.rs"]
mod delver_depth_probe;
#[path = "suite/delver_distinctness.rs"]
mod delver_distinctness;
#[path = "suite/delver_readout.rs"]
mod delver_readout;
#[path = "suite/demesne.rs"]
mod demesne;
#[path = "suite/depth.rs"]
mod depth;
#[path = "suite/descent_graph.rs"]
mod descent_graph;
#[path = "suite/diachronic.rs"]
mod diachronic;
#[path = "suite/dissolve_equivalence.rs"]
mod dissolve_equivalence;
#[path = "suite/doctrine.rs"]
mod doctrine;
#[path = "suite/drift_reach_probe.rs"]
mod drift_reach_probe;
#[path = "suite/era_substrate.rs"]
mod era_substrate;
#[path = "suite/explanations.rs"]
mod explanations;
#[path = "suite/exposure.rs"]
mod exposure;
#[path = "suite/fallow_feasibility.rs"]
mod fallow_feasibility;
#[path = "suite/fathom_column_probe.rs"]
mod fathom_column_probe;
#[path = "suite/fixture.rs"]
mod fixture;
#[path = "suite/founder_collision.rs"]
mod founder_collision;
#[path = "suite/generalist_baseline.rs"]
mod generalist_baseline;
#[path = "suite/generalist_distinctness.rs"]
mod generalist_distinctness;
#[path = "suite/graph_byte_identity.rs"]
mod graph_byte_identity;
#[path = "suite/graph_derive.rs"]
mod graph_derive;
#[path = "suite/history_bake.rs"]
mod history_bake;
#[path = "suite/history_byte_identity.rs"]
mod history_byte_identity;
#[path = "suite/history_emit.rs"]
mod history_emit;
#[path = "suite/history_gates.rs"]
mod history_gates;
#[path = "suite/history_placement.rs"]
mod history_placement;
#[path = "suite/history_shape_probe.rs"]
mod history_shape_probe;
#[path = "suite/history_sundering.rs"]
mod history_sundering;
#[path = "suite/history_tithe.rs"]
mod history_tithe;
#[path = "suite/history_tumult.rs"]
mod history_tumult;
#[path = "suite/history_units.rs"]
mod history_units;
#[path = "suite/hollow_readout.rs"]
mod hollow_readout;
#[path = "suite/insolation_probe.rs"]
mod insolation_probe;
#[path = "suite/junctions.rs"]
mod junctions;
#[path = "suite/keeping_probe.rs"]
mod keeping_probe;
#[path = "suite/kinship_facts.rs"]
mod kinship_facts;
#[path = "suite/lantern_probe.rs"]
mod lantern_probe;
#[path = "suite/mines_exist.rs"]
mod mines_exist;
#[path = "suite/name_pattern.rs"]
mod name_pattern;
#[path = "suite/niche_breadth_probe.rs"]
mod niche_breadth_probe;
#[path = "suite/non_void_roster.rs"]
mod non_void_roster;
#[path = "suite/occupancy_readout.rs"]
mod occupancy_readout;
#[path = "suite/off_lithology_decorrelation_probe.rs"]
mod off_lithology_decorrelation_probe;
#[path = "suite/ore_separation_probe.rs"]
mod ore_separation_probe;
#[path = "suite/ore_siting_probe.rs"]
mod ore_siting_probe;
#[path = "suite/ore_viability_probe.rs"]
mod ore_viability_probe;
#[path = "suite/person_promotion.rs"]
mod person_promotion;
#[path = "suite/pin_enumeration.rs"]
mod pin_enumeration;
#[path = "suite/portolan_resolution.rs"]
mod portolan_resolution;
#[path = "suite/profile.rs"]
mod profile;
#[path = "suite/promoted_forebear_yield.rs"]
mod promoted_forebear_yield;
#[path = "suite/proto_goblinoid_golden.rs"]
mod proto_goblinoid_golden;
#[path = "suite/radiation_admission.rs"]
mod radiation_admission;
#[path = "suite/radiation_affinity.rs"]
mod radiation_affinity;
#[path = "suite/radiation_language.rs"]
mod radiation_language;
#[path = "suite/radiation_readout.rs"]
mod radiation_readout;
#[path = "suite/raid_attribution_probe.rs"]
mod raid_attribution_probe;
#[path = "suite/range_affinity.rs"]
mod range_affinity;
#[path = "suite/range_identity.rs"]
mod range_identity;
#[path = "suite/range_readout.rs"]
mod range_readout;
#[path = "suite/repose_exposure.rs"]
mod repose_exposure;
#[path = "suite/repose_laws.rs"]
mod repose_laws;
#[path = "suite/soil_attribution_probe.rs"]
mod soil_attribution_probe;
#[path = "suite/solitary_tongue.rs"]
mod solitary_tongue;
#[path = "suite/species_worlds.rs"]
mod species_worlds;
#[path = "suite/stope_variety_probe.rs"]
mod stope_variety_probe;
#[path = "suite/subterranean_energy_probe.rs"]
mod subterranean_energy_probe;
#[path = "suite/survivorship_probe.rs"]
mod survivorship_probe;
#[path = "suite/tense_shadow.rs"]
mod tense_shadow;
#[path = "suite/termination_probe.rs"]
mod termination_probe;
#[path = "suite/threading_equivalence.rs"]
mod threading_equivalence;
#[path = "suite/tilth_phase_diagram.rs"]
mod tilth_phase_diagram;
#[path = "suite/tilth_probe.rs"]
mod tilth_probe;
#[path = "suite/tolerance_draw.rs"]
mod tolerance_draw;
#[path = "suite/tolerance_mutation.rs"]
mod tolerance_mutation;
#[path = "suite/traversal.rs"]
mod traversal;
#[path = "suite/tribute_stock_agreement_probe.rs"]
mod tribute_stock_agreement_probe;
#[path = "suite/underworld_capacity_probe.rs"]
mod underworld_capacity_probe;
#[path = "suite/underworld_chamber_reach.rs"]
mod underworld_chamber_reach;
#[path = "suite/underworld_conditions_probe.rs"]
mod underworld_conditions_probe;
#[path = "suite/underworld_ladder_probe.rs"]
mod underworld_ladder_probe;
#[path = "suite/underworld_lithology_probe.rs"]
mod underworld_lithology_probe;
#[path = "suite/underworld_per_rung_switch.rs"]
mod underworld_per_rung_switch;
#[path = "suite/underworld_separation.rs"]
mod underworld_separation;
#[path = "suite/underworld_water_table_probe.rs"]
mod underworld_water_table_probe;
#[path = "suite/warren_gate.rs"]
mod warren_gate;
#[path = "suite/warren_liebig_probe.rs"]
mod warren_liebig_probe;
#[path = "suite/warren_readout.rs"]
mod warren_readout;
#[path = "suite/waterline_probe.rs"]
mod waterline_probe;
#[path = "suite/watershed_measure.rs"]
mod watershed_measure;
#[path = "suite/weft_fieldpack.rs"]
mod weft_fieldpack;
#[path = "suite/weft_prevalence.rs"]
mod weft_prevalence;
#[path = "suite/winze_energy_probe.rs"]
mod winze_energy_probe;
#[path = "suite/winze_scale_probe.rs"]
mod winze_scale_probe;
