//! Calibration for the-gathering (Task 8): the carrying-capacity field's
//! headline biomass-by-latitude gradient, measured over the 200-seed
//! `census-of-the-gathering` study and pinned per ADR 0016 — the direction
//! (mean well above 1) was preregistered before the sweep ran (design spec
//! §5, "Population as a Field, Settlements as Condensations"); `rank-size-
//! slope` is recorded here as an OBSERVED metric only, never a calibration
//! target for this campaign's interim per-species condensation (full Zipf
//! calibration is the later MAP-22 coexistence-stack campaign's job, once
//! size is measured by mass and composition is real).
use hornvale_lab::{MetricValue, RunResult, canonical_row, load_rows, load_study, run};
use std::path::Path;
use std::sync::LazyLock;

/// The study driving this file's fixture.
const STUDY_PATH: &str = "../../studies/census-of-the-gathering.study.json";
/// The committed, CI-drift-checked census rows this file loads from.
const ROWS_PATH: &str = "../../book/src/laboratory/generated/census-of-the-gathering/rows.csv";

/// The 200-seed gradient census, loaded ONCE from its committed `rows.csv`
/// fixture and shared by every calibration in this file (mirrors
/// `calibration.rs`'s `DRIFT`/`MEETING` pattern, decision 0032). The fixture
/// is published by `lab run` and regenerated + drift-checked in CI's
/// "Artifacts are current" step; `gathering_fixture_matches_live_run` below
/// pins fixture == live. Loading instead of recomputing keeps the full sweep
/// off every local `cargo test`. Init panics on a load error (a test-setup
/// failure, not a calibration).
static GATHERING: LazyLock<RunResult> = LazyLock::new(|| {
    let study = load_study(Path::new(STUDY_PATH)).expect("load census-of-the-gathering study");
    let csv = std::fs::read_to_string(ROWS_PATH).expect("read census-of-the-gathering fixture");
    load_rows(&study, &csv).expect("reconstruct census-of-the-gathering from fixture")
});

/// Guard — ignored by default because it pays the full sweep (~2 min
/// release, longer under the test profile): the committed fixture
/// reconstructs *exactly* what a live `run` produces, so every other test in
/// this file may trust the fixture. Run it after regenerating the fixture,
/// or explicitly: `cargo test -p hornvale-lab --test gathering_calibration
/// -- --ignored`.
#[test]
#[ignore = "runs the full gathering census; the fixture is drift-checked in CI"]
fn gathering_fixture_matches_live_run() {
    let study = load_study(Path::new(STUDY_PATH)).expect("load census-of-the-gathering study");
    let live = run(&study).expect("run census-of-the-gathering study");
    // Canonicalize live Numbers before comparing: the fixture's floats passed
    // the quantizing serialization boundary (`render_csv`), the live run's
    // have not (shared helper: `hornvale_lab::canonical_row`).
    let live = RunResult {
        study: live.study.clone(),
        metric_names: live.metric_names.clone(),
        rows: live.rows.iter().map(canonical_row).collect(),
    };
    let csv = std::fs::read_to_string(ROWS_PATH).expect("read census-of-the-gathering fixture");
    let loaded = load_rows(&study, &csv).expect("reconstruct census from fixture");
    assert_eq!(
        loaded, live,
        "fixture diverged from a live run — regenerate with \
         `lab run studies/census-of-the-gathering.study.json`"
    );
}

/// The headline calibration (design spec §5): the carrying-capacity field's
/// mean `capacity-by-abs-latitude` over the census must read well above 1 —
/// preregistered floor 3, comfortably clear of the trivial "poles support as
/// much as the tropics" failure mode. Individual barren/marginal worlds
/// (little land in EITHER band) may legitimately read low; only the mean is
/// gated, per the preregistration — this is a population-level claim, not a
/// per-row invariant.
#[test]
fn capacity_by_abs_latitude_gradient_clears_the_preregistered_floor() {
    let result = &*GATHERING;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let g_i = idx("capacity-by-abs-latitude");
    let (mut sum, mut n) = (0.0_f64, 0u32);
    for row in &result.rows {
        if let MetricValue::Number(g) = row.values[g_i] {
            sum += g;
            n += 1;
        }
    }
    assert!(
        n > 0,
        "no world reported a capacity-by-abs-latitude gradient"
    );
    let mean = sum / f64::from(n);
    // Directional preregistration (design spec §5): well above 1.
    assert!(
        mean >= 3.0,
        "capacity-by-abs-latitude mean {mean:.4} fell below the preregistered floor of 3"
    );
    // Pinned calibration row (measured 2026-07-13, 200-seed census-of-the-
    // gathering, THRESHOLD=10.0 against the frozen K constants — see
    // `carrying_capacity.rs`'s freeze note for the full measurement). The
    // placeholder K constants already reproduced the gradient decisively, so
    // no retuning was needed before freezing them.
    assert!(
        (mean - 27.1467).abs() < 1e-3,
        "capacity-by-abs-latitude mean drifted: {mean:.4} (expected ~27.1467)"
    );
}

/// `rank-size-slope` is recorded, never gated to a target: this campaign's
/// interim per-species condensation is deliberately NOT tuned to a Zipf
/// target (design spec §5). The only structural guard here is that it is a
/// real, mostly-negative signal (rank-size relationships are conventionally
/// negative — a handful of large settlements, many small ones) — never that
/// it hits any particular slope.
#[test]
fn rank_size_slope_is_observed_not_tuned() {
    let result = &*GATHERING;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let r_i = idx("rank-size-slope");
    let (mut sum, mut n, mut negative) = (0.0_f64, 0u32, 0u32);
    for row in &result.rows {
        if let MetricValue::Number(r) = row.values[r_i] {
            sum += r;
            n += 1;
            if r < 0.0 {
                negative += 1;
            }
        }
    }
    assert!(n > 0, "no world reported a rank-size-slope");
    let mean = sum / f64::from(n);
    // Recorded for the record, not calibration-gated (see module doc).
    assert!(
        mean < 0.0,
        "mean rank-size-slope {mean:.4} is not negative — recorded, not tuned, but this many \
         worlds inverting the conventional direction would be a genuine finding worth a note"
    );
    assert!(
        negative > n / 2,
        "rank-size-slope should be negative (larger settlements rarer) in most worlds; \
         observed only {negative}/{n}"
    );
}
