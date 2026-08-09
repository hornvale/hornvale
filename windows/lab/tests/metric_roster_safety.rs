//! Every registered metric must survive every roster a shipped study builds
//! with.
//!
//! ## Why this exists
//!
//! Both shipped studies select `"metrics": "all"`, which resolves to the
//! WHOLE registry — so every metric runs against every roster in every study,
//! including the ones it was never written for.
//! `census-of-the-meeting`'s two solo rosters (`goblin-solo`,
//! `goblin-twin-solo`) carry a single re-keyed kind and none of the canonical
//! ones; `the-census`' default roster carries the canonical kinds and none of
//! the re-keyed ones. A metric that assumes either shape does not read
//! `Absent` on the other — it PANICS, because worldgen's `resolve_kind`
//! fails loudly on a species outside the component set it is handed, and the
//! `language_of_in` / `lexicon_of_in_from` family unwraps that failure.
//!
//! The Delvers (C2c) shipped exactly that class of defect, and the census
//! crashed on the canonical box against a roster no unit test had ever built
//! the full metric registry against.
//!
//! So: build one world per roster and run the ENTIRE registry against it,
//! collecting every panic rather than stopping at the first. This is the
//! cheap standing guard for the whole class — a new metric, or a new roster,
//! that disagrees with an existing one reddens here instead of on the
//! canonical box eight hours into a census.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{BuiltView, MetricValue, registry};
use hornvale_worldgen::WorldComponents;

/// The rosters the two shipped studies build with, in
/// `hornvale_lab::runner::resolve_roster`'s own closed set: `the-census`'
/// default (canonical) plus `census-of-the-meeting`'s two solo null-control
/// rosters.
fn study_rosters() -> Vec<(&'static str, WorldComponents)> {
    vec![
        (
            "default",
            WorldComponents::assemble().expect("canonical registries assemble"),
        ),
        ("goblin-solo", hornvale_lab::goblin_solo_components()),
        (
            "goblin-twin-solo",
            hornvale_lab::goblin_twin_solo_components(),
        ),
    ]
}

/// Run the whole registry against one built world, returning the name of
/// every metric that panicked. `catch_unwind` so one bad metric does not
/// hide the rest — a census would report the same list, one worker at a
/// time, over hours.
fn panicking_metrics(view: &BuiltView) -> Vec<String> {
    let mut failed = Vec::new();
    for metric in registry() {
        let outcome =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| metric.extract.apply(view)));
        match outcome {
            Ok(MetricValue::Number(x)) if !x.is_finite() => {
                failed.push(format!("{} (non-finite: {x})", metric.name));
            }
            Ok(_) => {}
            Err(_) => failed.push(metric.name.to_string()),
        }
    }
    failed
}

/// NOT `#[ignore]`d, deliberately. Three full-depth builds plus the whole
/// registry measure ~21 s on the Mac — an ordinary commit-gate cost against a
/// suite whose slowest tests run three to six minutes. The heavy tier would
/// defer this to `make gate-full`, which is exactly the blind spot that let
/// the defect reach the canonical box: the cheapest guard against a
/// census-killing panic is worth nothing if it only runs where the census
/// already runs.
#[test]
fn every_metric_survives_every_study_roster() {
    // Quiet the per-panic backtrace spam: `catch_unwind` already reports
    // which metric failed, and a 34-line dump per metric buries the list.
    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let mut report: Vec<String> = Vec::new();
    let mut family: Vec<String> = Vec::new();
    for (label, wc) in study_rosters() {
        let view = BuiltView::build_to(
            Seed(42),
            &SkyPins::default(),
            wc,
            hornvale_worldgen::BuildDepth::Full,
        )
        .expect("a full-depth build at seed 42");
        for name in panicking_metrics(&view) {
            report.push(format!("roster {label}: {name}"));
        }
        family.push(format!(
            "{label}={:?}",
            registry()
                .iter()
                .find(|m| m.name == "lexicon-regular-family")
                .expect("lexicon-regular-family is registered")
                .extract
                .apply(&view)
        ));
    }
    std::panic::set_hook(previous);
    assert!(
        report.is_empty(),
        "metrics that do not survive a shipped study's roster:\n  {}",
        report.join("\n  ")
    );
    // The guard must DISCRIMINATE, not merely survive. A metric that reads
    // `Absent` on every roster panics on none of them, so "nothing panicked"
    // alone would stay green through a regression that quietly stopped
    // measuring. `lexicon-regular-family` is the metric the census died in,
    // and it ranges over the roster's OWN derived lexicon population — so it
    // must read a real `Flag` on all three, including the two solo rosters
    // whose single kind is the only daughter they have.
    assert_eq!(
        family.join(" "),
        "default=Flag(true) goblin-solo=Flag(true) goblin-twin-solo=Flag(true)",
        "lexicon-regular-family must be MEASURED on every study roster, not Absent"
    );
}
