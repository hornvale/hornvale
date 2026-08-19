//! The cascade rule-firing metric: does a species' drawn sound-change
//! cascade actually change any of its words?

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{Extractor, FullView, MetricValue, registry};

#[test]
fn cascade_rules_fired_is_registered_for_both_probe_species() {
    let names: Vec<&str> = registry().iter().map(|m| m.name).collect();
    assert!(
        names.contains(&"cascade-rules-fired-goblin"),
        "expected cascade-rules-fired-goblin in the metric registry, got {names:?}"
    );
    assert!(
        names.contains(&"cascade-rules-fired-bugbear"),
        "expected cascade-rules-fired-bugbear in the metric registry"
    );
}

/// Build `seed` to `Full` depth and read `metric_name`'s value directly off
/// it, bypassing the study runner entirely. Every metric this test file
/// exercises is `Full`-rung (`Extractor::Full`), so there is no need to
/// dispatch on `Extractor`'s other variants — a metric of a different rung
/// here is a test-selection bug, and panicking says so loudly rather than
/// silently returning `Absent`.
fn metric_value_on_seed(metric_name: &str, seed: u64) -> MetricValue {
    let view = FullView::build(Seed(seed), &SkyPins::default())
        .unwrap_or_else(|e| panic!("seed {seed}: world failed to build: {e:?}"));
    let metric = registry()
        .into_iter()
        .find(|m| m.name == metric_name)
        .unwrap_or_else(|| panic!("{metric_name} is registered"));
    match metric.extract {
        Extractor::Full(f) => f(&view),
        _ => panic!("{metric_name}: expected an Extractor::Full metric"),
    }
}

#[test]
fn the_two_probe_species_do_not_report_the_same_inertness_on_seed_42() {
    // Not an assertion about WHICH is higher — that is the finding, not the
    // contract. Only that the metric discriminates at all: if both species
    // returned an identical value on every seed, the metric would be
    // measuring nothing and would still pass a naive smoke test.
    let goblin = metric_value_on_seed("cascade-rules-fired-goblin", 42);
    let bugbear = metric_value_on_seed("cascade-rules-fired-bugbear", 42);
    assert_ne!(
        goblin, bugbear,
        "seed 42 measured goblin 0 and bugbear 1; if these now agree, either \
         the metric is inert or the language engine changed — both are findings"
    );
}
