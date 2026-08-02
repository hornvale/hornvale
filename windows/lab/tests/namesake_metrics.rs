//! The Namesake (Task 7): the metrics the campaign's two preregistered
//! claims (spec §5.1, §5.2) are judged against.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{Extractor, FullView, MetricValue, registry};

#[test]
fn the_four_namesake_metrics_are_registered() {
    let names: Vec<&str> = registry().iter().map(|m| m.name).collect();
    for expected in [
        "name-pattern-signatures",
        // The n behind §5.1(2)'s 1/n chance baseline, so that criterion's
        // verdict is re-derivable from rows.csv. See the metric's doc.
        "peoples-placed",
        "name-people-recoverability",
        "name-prefix-settlement-scope",
        "name-prefix-region-scope",
        // §5.2(2) is two-sided; the median reads one side and this reads the
        // other. See the metric's doc.
        "name-prefix-region-full-stack",
    ] {
        assert!(names.contains(&expected), "missing metric {expected}");
    }
}

/// Build `seed` to `Full` depth and read every named metric off the one
/// view, so a five-metric probe costs one world rather than five. All five
/// Namesake metrics are `Full`-rung; a metric of a different rung here is a
/// test-selection bug, and panicking says so loudly rather than silently
/// returning `Absent`.
fn metric_values_on_seed(metric_names: &[&str], seed: u64) -> Vec<MetricValue> {
    let view = FullView::build(Seed(seed), &SkyPins::default())
        .unwrap_or_else(|e| panic!("seed {seed}: world failed to build: {e:?}"));
    metric_names
        .iter()
        .map(|name| {
            let metric = registry()
                .into_iter()
                .find(|m| m.name == *name)
                .unwrap_or_else(|| panic!("{name} is registered"));
            match metric.extract {
                Extractor::Full(f) => f(&view),
                _ => panic!("{name}: expected an Extractor::Full metric"),
            }
        })
        .collect()
}

/// The six metrics report a value at all on a real world, and each reports
/// one inside the range its own definition allows.
///
/// Not a pin on the measured numbers — those are the campaign's finding, not
/// its contract, and pinning them here would freeze the result the study
/// exists to report. What this does catch is a metric that is structurally
/// broken: `Absent` on a fully-peopled world (the extractor never found the
/// population it claims to measure), a share outside `[0, 1]`, or a median
/// element count below one (impossible — every pattern carries a `Stem`).
#[test]
fn the_namesake_metrics_report_in_range_on_a_real_world() {
    let names = [
        "name-pattern-signatures",
        "peoples-placed",
        "name-people-recoverability",
        "name-prefix-settlement-scope",
        "name-prefix-region-scope",
        "name-prefix-region-full-stack",
    ];
    let values = metric_values_on_seed(&names, 42);
    for (name, value) in names.iter().zip(values.iter()) {
        let MetricValue::Number(n) = value else {
            panic!("{name} on seed 42: expected a Number, got {value:?}");
        };
        println!("seed 42  {name} = {n}");
        let (lo, hi) = match *name {
            "name-pattern-signatures" | "peoples-placed" => (1.0, 16.0),
            "name-prefix-region-scope" => (1.0, 16.0),
            _ => (0.0, 1.0),
        };
        assert!(
            *n >= lo && *n <= hi,
            "{name} on seed 42 reported {n}, outside [{lo}, {hi}]"
        );
    }
}
