//! Hornvale laboratory: batch studies over generated worlds.
#![warn(missing_docs)]

pub mod chart;
pub mod diff;
mod metrics;
pub mod publish;
pub mod roster;
pub mod runner;
pub mod study;
pub mod summary;

pub use chart::{bar_chart_svg, charts_for};
pub use diff::{render_diff, render_diff_results};
pub use metrics::{Metric, MetricValue, SummaryKind, WorldView, registry, render_metric_list};
pub use publish::publish;
pub use roster::{goblin_solo_roster, goblin_twin_solo_roster, serpent_tonal_solo_roster};
pub use runner::{Row, RunResult, canonical_row, load_rows, run, write_csv};
pub use study::{MetricSelection, PinSet, Seeds, Study, StudyError, load_study};
pub use summary::render_summary;
