//! Hornvale laboratory: batch studies over generated worlds.
#![warn(missing_docs)]

mod metrics;
pub mod study;

pub use metrics::{Metric, MetricValue, SummaryKind, WorldView, registry, render_metric_list};
pub use study::{MetricSelection, PinSet, Seeds, Study, StudyError, load_study};
