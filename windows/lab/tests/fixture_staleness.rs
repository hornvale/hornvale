//! Staleness probe for the committed census fixtures (TOOL-16,
//! TOOL-drift-scan-probes).
//!
//! Decision 0032 accepted one cost
//! knowingly: a developer who changes worldgen sees calibration pass
//! against the stale fixture until the artifact drift check catches it.
//! This probe narrows that gap: it regenerates each census's first
//! [`PROBE_SEEDS`] seeds live, PLUS a rotating [`WINDOW_SEEDS`]-seed window
//! (see [`window_start`]), and compares both against the committed rows,
//! canonicalized. The window's position is a pure function of the committed
//! fixture bytes, so successive fixture regenerations sweep different slices
//! of seed space instead of always re-checking the same six seeds.
//!
//! It was authored as an always-on, few-seconds probe for `cargo test`. As
//! the worldgen pipeline deepened, its cost grew to minutes (measured ~15
//! min under load, 2026-07-13), so it is now `#[ignore]`d into the heavy
//! tier and runs in `make gate-full`, not the commit gate: a worldgen change
//! that moves a census now surfaces there and in CI's regenerate-and-diff,
//! not on the developer's next `cargo test`. Restoring the shift-left signal
//! (a probe cheap enough to stay in the commit gate) waits on cheaper
//! worldgen.
//!
//! Not a replacement for the full ignored guard
//! (`census_fixture_matches_live_run`) or CI's regenerate-and-diff: a
//! change that only moves seeds outside the fixed head and the current
//! window slips past this probe and is caught there.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Seed, quantize};
use hornvale_lab::{
    MetricValue, Row, RunResult, Study, canonical_row, load_rows, load_study, record_failure, run,
};
use std::path::Path;

/// Seeds probed per census from the fixed head: seeds 0–2 — cheap enough
/// for every workspace test run.
const PROBE_SEEDS: u64 = 3;

/// Width of the rotating window: 3 seeds, mirroring [`PROBE_SEEDS`].
const WINDOW_SEEDS: u64 = 3;

/// The smallest study span the rotating window still fits in: the fixed
/// head (seeds `0..3`) plus a 3-seed window need at least 3 seeds of
/// margin at each end, so `[3, span - 3]` must be non-empty.
const MIN_SPAN_FOR_WINDOW: u64 = 7;

/// Derive the rotating window's start seed as a pure function of the
/// committed fixture bytes: `Seed` derivation over the CSV content, then a
/// bounded draw over `[3, span - 3]`. Never git state, wall clock, or env
/// — the same fixture bytes always produce the same window, and only
/// regenerating the fixture moves it. Returns `None` when `span` is too
/// small to hold a fixed block, a moving block, and the required margin
/// (design constraint: fall back to the fixed window only).
fn window_start(csv: &str, span: u64) -> Option<u64> {
    if span < MIN_SPAN_FOR_WINDOW {
        return None;
    }
    let mut stream = Seed(0).derive(StreamLabel::dynamic(csv)).stream();
    let hi = u32::try_from(span - 3).expect("census spans fit in u32");
    Some(u64::from(stream.range_u32(3, hi)))
}

/// The committed, CI-drift-checked censuses (decisions
/// 0029 and 0032):
/// the-census (the 1,000-seed canonical census) and the 500-seed
/// census-of-the-meeting null control, each backing a fixture-loading
/// calibration suite. `branches-family` is EXCLUDED here: it is frozen
/// (census-as-data spec §1) — a frozen fixture is intentionally stale and
/// must not be staleness-checked.
const CENSUSES: [(&str, &str); 2] = [
    (
        "../../studies/the-census.study.json",
        "../../book/src/laboratory/generated/the-census/rows.csv",
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
                     fixture is stale or truncated; regenerate on the AWS box: `make \
                     regen-remote` (census regen is never local; study \
                     {study_path}) and commit the diff (decision 0032)",
                    row.seed, row.pin_set
                )
            });
        let canon = canonical_row(row);
        if *pinned != canon {
            // Best-effort: a failure to record the black-box world must never
            // mask this mismatch. On success it names the recorded world; on
            // an io failure it degrades to the plain message.
            let recording_note = match record_failure(&live.study, row.seed, &row.pin_set) {
                Ok(path) => format!(" Failing world recorded at {}.", path.display()),
                Err(_) => String::new(),
            };
            panic!(
                "worldgen changed but the census fixture {rows_path} was not regenerated (seed {} \
                 / pin set '{}' differs). Regenerate on the AWS box: `make regen-remote` (census \
                 regen is never local; study {study_path}), review the diff, and commit it \
                 WITH the change that moved it (decision 0032).{recording_note}",
                row.seed, row.pin_set
            );
        }
    }
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn census_fixtures_match_a_probe_of_live_seeds() {
    for (study_path, rows_path) in CENSUSES {
        let study = load_study(Path::new(study_path)).expect("load study");
        let csv = std::fs::read_to_string(rows_path).expect("read census fixture");
        let fixture = load_rows(&study, &csv).expect("reconstruct census from fixture");

        let mut mini = study.clone();
        mini.seeds.count = mini.seeds.count.min(PROBE_SEEDS);
        let live = run(&mini).expect("probe census");
        assert_fixture_fresh(&live, &fixture, study_path, rows_path);

        if let Some(start) = window_start(&csv, study.seeds.count) {
            let mut window = study.clone();
            window.seeds.from = study.seeds.from + start;
            window.seeds.count = WINDOW_SEEDS;
            let live_window = run(&window).expect("probe rotating window");
            assert_fixture_fresh(&live_window, &fixture, study_path, rows_path);
        }
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
        msg.contains("make regen-remote"),
        "message must name the fix: {msg}"
    );
    assert!(
        msg.contains("was not regenerated"),
        "message must name the cause: {msg}"
    );

    // Clean up: the mismatch path above calls `record_failure`, which
    // writes into the shared workspace target/failures/ directory (see
    // `blackbox.rs`'s own unit test for the idiom), not a private temp
    // dir. The study name ("probe-self-test"), seed (0), and pin set
    // ("default") come from `synthetic()` above, so the file names are
    // fixed.
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("hornvale-lab lives two directories below the workspace root");
    let base = workspace_root.join("target/failures/probe-self-test-seed0-default");
    let _ = std::fs::remove_file(base.with_extension("json"));
    let _ = std::fs::remove_file(base.with_extension("repro.txt"));
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

// The rotating window's own self-tests (PROC-6 pattern: prove the
// mechanism — determinism, bounds, movement — not just that today's
// fixtures happen to pass).

#[test]
fn window_start_is_deterministic_for_fixed_bytes() {
    let csv = "seed,value\n0,1\n1,2\n";
    assert_eq!(window_start(csv, 500), window_start(csv, 500));
}

#[test]
fn window_start_is_in_bounds_for_a_representative_span() {
    let span = 1000;
    let start =
        window_start("some committed fixture bytes", span).expect("span >= 7 must yield a window");
    assert!(
        (3..=span - 3).contains(&start),
        "window start {start} out of [3, {}]",
        span - 3
    );
}

#[test]
fn window_start_is_in_bounds_at_the_span_seven_boundary() {
    // span 7 is the smallest span the design constraint still guarantees a
    // window for: [3, span - 3] must be non-empty.
    let start = window_start("boundary-bytes", 7).expect("span == 7 must still yield a window");
    assert!((3..=4).contains(&start), "got {start}");
}

#[test]
fn window_start_offset_lands_within_the_studys_own_range_for_nonzero_from() {
    // `window_start` returns an offset in `[3, span - 3]` relative to the
    // study's own seed range, not an absolute seed number — the caller
    // (`census_fixtures_match_a_probe_of_live_seeds`) must add
    // `study.seeds.from` before using it as `window.seeds.from`. All three
    // committed censuses happen to start at seed 0, where the offset and
    // the absolute seed coincide; this test proves the arithmetic still
    // lands in range for a study whose `from` is nonzero.
    let span = 500;
    let from = 1_000;
    let start = window_start("some committed fixture bytes", span).expect("window");
    let absolute_start = from + start;
    assert!(
        (from + 3..=from + span - 3).contains(&absolute_start),
        "absolute window start {absolute_start} out of [{}, {}]",
        from + 3,
        from + span - 3
    );
}

#[test]
fn window_start_moves_when_the_bytes_change() {
    let span = 500;
    let a = window_start("fixture-content-v1", span).expect("window");
    let b = window_start("fixture-content-v2", span).expect("window");
    assert_ne!(a, b, "changing the fixture bytes must move the window");
}

#[test]
fn window_start_falls_back_to_none_below_span_seven() {
    assert_eq!(window_start("anything", 6), None);
    assert_eq!(window_start("anything", 0), None);
    assert_eq!(window_start("anything", 1), None);
}
