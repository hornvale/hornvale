//! Hornvale laboratory: batch studies over generated worlds.
#![warn(missing_docs)]

pub mod blackbox;
pub mod census_claim;
pub mod census_guard;
pub mod chart;
pub mod diff;
pub mod domesday;
pub mod health;
mod metrics;
pub mod publish;
mod reticence;
pub mod roster;
pub mod runner;
pub mod schema;
pub mod study;
pub mod summary;
pub mod synthetic;
pub mod timings;
pub mod tongue_distance;

pub use blackbox::record_failure;
pub use census_guard::{
    CENSUS_GOLDENS_DIR, canonical_host, current_hostname, require_canonical_host_for,
};
pub use chart::{bar_chart_svg, charts_for};
pub use diff::{render_diff, render_diff_results};
pub use metrics::{
    AstronomyView, BuiltView, ClimateView, Extractor, FullView, Metric, MetricValue,
    SettlementView, SummaryKind, TerrainView, ViewRung, WorldView, registry,
    render_confidant_report, render_metric_list, steepable_concept_roster,
};
pub use publish::publish;
pub use reticence::render_reticence_report;
pub use roster::{
    awakened_owlbear_components, goblin_solo_components, goblin_twin_solo_components,
    serpent_tonal_solo_components,
};
pub use runner::{
    Row, RunResult, canonical_row, canonical_value, load_rows, run, run_forced_full, write_csv,
};
pub use schema::{fnv1a64, render_schema};
pub use study::{MetricSelection, PinSet, Seeds, Study, StudyError, load_study};
pub use summary::render_summary;
