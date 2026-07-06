//! Hornvale laboratory: batch studies over generated worlds.
#![warn(missing_docs)]

mod metrics;

pub use metrics::{Metric, MetricValue, SummaryKind, WorldView, registry, render_metric_list};
