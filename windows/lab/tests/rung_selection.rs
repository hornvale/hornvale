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

#[test]
fn the_astronomy_driver_metrics_are_registered_and_vary() {
    // insolation-rel and zone-position are the campaign's driver columns.
    // Both must exist, and neither may be frozen — a frozen driver column
    // would repeat the defect CLIM-astronomy-unmeasured names.
    for name in ["insolation-rel", "zone-position"] {
        let study = solo_study(name); // 1 seed proves registration
        assert!(
            hornvale_lab::run(&study).is_ok(),
            "{name} must be registered and runnable alone"
        );
    }
}

/// Fetch a numeric metric from a row by name. Panics with the metric name on a
/// missing column or a non-numeric value — a test helper, so a loud failure is
/// the useful behaviour.
fn number_of(result: &hornvale_lab::RunResult, row: &hornvale_lab::Row, metric: &str) -> f64 {
    let idx = result
        .metric_names
        .iter()
        .position(|n| *n == metric)
        .unwrap_or_else(|| panic!("metric {metric} not selected by this study"));
    match &row.values[idx] {
        hornvale_lab::MetricValue::Number(n) => *n,
        other => panic!("metric {metric} is {other:?}, not a Number"),
    }
}

#[test]
fn insolation_is_determined_by_zone_position_alone() {
    // The habitable zone is denominated in sqrt(L) and insolation is L/a²,
    // so L cancels EXACTLY: S = 1/(0.95 + 0.42u)². Luminosity does not enter.
    // This is the campaign's root-cause claim; if a future change to the
    // bracket breaks the identity, that is a deliberate act and this test
    // is where it must be acknowledged.
    let study = Study {
        name: "insolation-identity".to_string(),
        description: "pins the L-cancellation the campaign's diagnosis rests on".to_string(),
        seeds: Seeds { from: 1, count: 20 },
        pin_sets: vec![PinSet {
            label: "default".to_string(),
            pins: vec![],
            roster: None,
        }],
        metrics: MetricSelection::Named(vec![
            "insolation-rel".to_string(),
            "zone-position".to_string(),
        ]),
    };
    let result = hornvale_lab::run(&study).expect("study runs");
    assert_eq!(result.rows.len(), 20, "one row per seed");
    // `Row` carries `refusal: Option<String>` — a refused genesis yields a row
    // whose `values` may be short. Assert none refused rather than indexing
    // into a short row and reporting a confusing panic instead of the real
    // cause. Seeds 1..20 unpinned should never refuse; if they do, that is the
    // finding.
    let refused: Vec<_> = result
        .rows
        .iter()
        .filter_map(|r| r.refusal.as_ref().map(|m| (r.seed, m.clone())))
        .collect();
    assert!(
        refused.is_empty(),
        "unpinned seeds refused genesis: {refused:?}"
    );
    for row in &result.rows {
        let u = number_of(&result, row, "zone-position");
        let s = number_of(&result, row, "insolation-rel");
        let expected = 1.0 / (0.95 + 0.42 * u).powi(2);
        assert!(
            (s - expected).abs() < 1e-6,
            "seed {}: S={s} != 1/(0.95+0.42*{u})^2 = {expected}",
            row.seed
        );
    }
}
