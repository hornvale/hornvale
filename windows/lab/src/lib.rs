//! Hornvale laboratory: batch studies over generated worlds.
#![warn(missing_docs)]

pub mod blackbox;
pub mod chart;
pub mod diff;
pub mod health;
mod metrics;
pub mod publish;
pub mod roster;
pub mod runner;
pub mod schema;
pub mod study;
pub mod summary;
pub mod synthetic;

pub use blackbox::record_failure;
pub use chart::{bar_chart_svg, charts_for};
pub use diff::{render_diff, render_diff_results};
pub use metrics::{
    AstronomyView, BuiltView, ClimateView, Extractor, FullView, Metric, MetricValue,
    SettlementView, SummaryKind, TerrainView, WorldView, registry, render_metric_list,
};
pub use publish::publish;
pub use roster::{
    awakened_owlbear_components, goblin_solo_components, goblin_twin_solo_components,
    serpent_tonal_solo_components,
};
pub use runner::{Row, RunResult, canonical_row, load_rows, run, run_forced_full, write_csv};
pub use schema::{fnv1a64, render_schema};
pub use study::{MetricSelection, PinSet, Seeds, Study, StudyError, load_study};
pub use summary::render_summary;
