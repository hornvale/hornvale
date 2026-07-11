//! Always-on staleness probe for the committed census fixtures (TOOL-16).
//!
//! Decision `calibration-loads-the-census-fixture` accepted one cost
//! knowingly: a developer who changes worldgen and runs only `cargo test`
//! sees calibration pass against the stale fixture until CI's artifact
//! drift check catches it. This probe closes most of that gap for a few
//! seconds' cost: it regenerates each census's first [`PROBE_SEEDS`] seeds
//! live and compares them, canonicalized, against the committed rows — so
//! a worldgen change that moves the census fails HERE, with the
//! regeneration instruction, not in CI an hour later.
//!
//! Not a replacement for the full ignored guard
//! (`census_fixture_matches_live_run`) or CI's regenerate-and-diff: a
//! change that only moves seeds ≥ [`PROBE_SEEDS`] slips past this probe
//! and is caught there.

use hornvale_kernel::quantize;
use hornvale_lab::{MetricValue, Row, RunResult, Study, canonical_row, load_rows, load_study, run};
use std::path::Path;

/// Seeds probed per census: 3 seeds × (1 + 2) pin sets ≈ 9 worlds ≈ a
/// couple of seconds — cheap enough for every workspace test run.
const PROBE_SEEDS: u64 = 3;

/// The two committed, CI-drift-checked censuses (decision
/// `ci-checks-500-seed-censuses`).
const CENSUSES: [(&str, &str); 2] = [
    (
        "../../studies/census-lands-drift.study.json",
        "../../book/src/laboratory/generated/census-lands-drift/rows.csv",
    ),
    (
        "../../studies/census-of-the-meeting.study.json",
        "../../book/src/laboratory/generated/census-of-the-meeting/rows.csv",
    ),
];

/// Compare every live row against its fixture counterpart, failing with
/// the actionable regeneration instruction on any mismatch.
fn assert_fixture_fresh(live: &RunResult, fixture: &RunResult, study_path: &str, rows_path: &str) {
    for row in &live.rows {
        let pinned = fixture
            .rows
            .iter()
            .find(|r| r.seed == row.seed && r.pin_set == row.pin_set)
            .unwrap_or_else(|| {
                panic!(
                    "census fixture {rows_path} has no row for seed {} / pin set '{}' — the \
                     fixture is stale or truncated; run `make rebaseline` (or `cargo run \
                     --release -p hornvale -- lab run {study_path}`) and commit the diff \
                     (decision calibration-loads-the-census-fixture)",
                    row.seed, row.pin_set
                )
            });
        assert_eq!(
            *pinned,
            canonical_row(row),
            "worldgen changed but the census fixture {rows_path} was not regenerated (seed {} \
             / pin set '{}' differs). Run `make rebaseline` (or `cargo run --release -p \
             hornvale -- lab run {study_path}`), review the diff, and commit it WITH the \
             change that moved it (decision calibration-loads-the-census-fixture).",
            row.seed,
            row.pin_set
        );
    }
}

#[test]
fn census_fixtures_match_a_probe_of_live_seeds() {
    for (study_path, rows_path) in CENSUSES {
        let study = load_study(Path::new(study_path)).expect("load study");
        let csv = std::fs::read_to_string(rows_path).expect("read census fixture");
        let fixture = load_rows(&study, &csv).expect("reconstruct census from fixture");
        let mut mini = study.clone();
        mini.seeds.count = mini.seeds.count.min(PROBE_SEEDS);
        let live = run(&mini).expect("probe census");
        assert_fixture_fresh(&live, &fixture, study_path, rows_path);
    }
}

/// A tiny synthetic study/result pair for the probe's own rejection tests
/// (the PROC-6 pattern: the guard proves it rejects, not only that the
/// current tree passes). `ocean-fraction` is a real registry metric, so
/// the study validates.
fn synthetic(value: f64) -> RunResult {
    let study: Study = serde_json::from_str(
        r#"{
            "name": "probe-self-test",
            "description": "synthetic rows for the staleness probe's own tests",
            "seeds": { "from": 0, "count": 1 },
            "pin_sets": [ { "label": "default", "pins": [] } ],
            "metrics": ["ocean-fraction"]
        }"#,
    )
    .expect("valid study json");
    study.validate().expect("study validates");
    RunResult {
        study,
        metric_names: vec!["ocean-fraction"],
        rows: vec![Row {
            seed: 0,
            pin_set: "default".to_string(),
            values: vec![MetricValue::Number(value)],
            refusal: None,
        }],
    }
}

#[test]
fn a_stale_fixture_fails_with_the_regeneration_instruction() {
    let live = synthetic(0.42);
    let fixture = synthetic(0.43);
    let err = std::panic::catch_unwind(|| {
        assert_fixture_fresh(&live, &fixture, "studies/example.study.json", "rows.csv")
    })
    .expect_err("a diverging fixture must fail the probe");
    let msg = err
        .downcast_ref::<String>()
        .expect("panic payload is a String");
    assert!(
        msg.contains("make rebaseline"),
        "message must name the fix: {msg}"
    );
    assert!(
        msg.contains("was not regenerated"),
        "message must name the cause: {msg}"
    );
}

#[test]
fn a_full_precision_live_value_matches_its_quantized_fixture() {
    // What `run()` produces vs what `load_rows()` reconstructs: the probe
    // must canonicalize, or every numeric metric is a false staleness.
    let raw = 0.123_456_789_012_345;
    let live = synthetic(raw);
    let fixture = synthetic(quantize(raw));
    assert_fixture_fresh(&live, &fixture, "s.json", "rows.csv");
}

#[test]
fn a_missing_fixture_row_is_reported_as_truncated() {
    let live = synthetic(0.42);
    let mut fixture = synthetic(0.42);
    fixture.rows.clear();
    let err =
        std::panic::catch_unwind(|| assert_fixture_fresh(&live, &fixture, "s.json", "rows.csv"))
            .expect_err("a missing row must fail the probe");
    let msg = err
        .downcast_ref::<String>()
        .expect("panic payload is a String");
    assert!(msg.contains("stale or truncated"), "got: {msg}");
}
