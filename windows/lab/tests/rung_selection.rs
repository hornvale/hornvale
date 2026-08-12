//! Every registered metric must be runnable in a study that selects it ALONE.
//! A metric whose declared rung does not match the view the runner hands it
//! panics (Climate) or reports a silent all-`Absent` column (Settlement/Full
//! past an early return) — see TOOL-rung-tag-unchecked.

use hornvale_lab::{MetricSelection, PinSet, Seeds, Study};

/// A study selecting exactly one metric, over one seed.
fn solo_study(metric: &str) -> Study {
    Study {
        name: format!("solo-{metric}"),
        description: "rung-selection guard: one metric, one seed".to_string(),
        seeds: Seeds { from: 1, count: 1 },
        pin_sets: vec![PinSet {
            label: "default".to_string(),
            pins: vec![],
            // None = the shipped {goblin, kobold} roster. Required in a
            // struct literal even though the JSON path defaults it.
            roster: None,
        }],
        metrics: MetricSelection::Named(vec![metric.to_string()]),
    }
}

#[test]
fn a_climate_rung_metric_runs_when_selected_alone() {
    // mean-land-temperature-c is Extractor::Climate. Before the fix this
    // panics with "climate-rung extractor on a shallower built view".
    let result = hornvale_lab::run(&solo_study("mean-land-temperature-c"));
    assert!(
        result.is_ok(),
        "a Climate-rung metric must run when selected alone: {:?}",
        result.err()
    );
}

#[test]
fn a_terrain_rung_metric_still_runs_when_selected_alone() {
    // Guards against fixing Climate by over-building everything.
    let result = hornvale_lab::run(&solo_study("mountain-coverage"));
    assert!(result.is_ok(), "{:?}", result.err());
}
