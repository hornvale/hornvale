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

// TODO(census-regen): repointed at the-census merge — census-of-the-gathering
// folded into main's `the-census` (1000 seeds, `metrics: "all"`). The committed
// the-census fixture is STALE for the-gathering's added field columns
// (capacity-by-abs-latitude, total-population, pop-weighted-abs-latitude,
// rank-size-slope), so this file's pinned gradient/latitude assertions are
// EXPECTED to fail until the census is regenerated and these values re-pinned.
/// The study driving this file's fixture.
const STUDY_PATH: &str = "../../studies/the-census.study.json";
/// The committed, CI-drift-checked census rows this file loads from.
const ROWS_PATH: &str = "../../book/src/laboratory/generated/the-census/rows.csv";

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

/// The second preregistered hypothesis the brief names (Task 8 review): the
/// carrying-capacity field concentrates population off the poles, so the
/// population-weighted mean absolute latitude across settlements should read
/// BELOW the uniform-sphere baseline — the area-weighted mean |latitude| a
/// sphere's surface would show if population were spread with no regard to
/// climate, ≈32.7° (the classic `arccos`-weighted uniform-sphere integral).
#[test]
fn pop_weighted_abs_latitude_reads_below_the_uniform_sphere_baseline() {
    /// The area-weighted mean absolute latitude on a uniform sphere: a
    /// preregistered constant, not something this census measures.
    const UNIFORM_SPHERE_BASELINE: f64 = 32.7;
    let result = &*GATHERING;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let p_i = idx("pop-weighted-abs-latitude");
    let (mut sum, mut n) = (0.0_f64, 0u32);
    for row in &result.rows {
        if let MetricValue::Number(p) = row.values[p_i] {
            sum += p;
            n += 1;
        }
    }
    assert!(n > 0, "no world reported a pop-weighted-abs-latitude");
    let mean = sum / f64::from(n);
    // Directional preregistration: below the uniform-sphere baseline.
    assert!(
        mean < UNIFORM_SPHERE_BASELINE,
        "pop-weighted-abs-latitude mean {mean:.4} did not clear the preregistered \
         uniform-sphere baseline of {UNIFORM_SPHERE_BASELINE}"
    );
    // Pinned calibration row (measured 2026-07-13, same 200-seed
    // census-of-the-gathering fixture the gradient calibration above uses).
    assert!(
        (mean - 10.3411).abs() < 1e-3,
        "pop-weighted-abs-latitude mean drifted: {mean:.4} (expected ~10.3411)"
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

/// World-level conservation guard (brief Step 7 / Task 8 review): a built
/// seed-42 world's total committed settlement population must stay bounded
/// by, and in the same order of magnitude as, the total carrying-capacity
/// field it was condensed from — a coarse guard against the founder-floor
/// and threshold-culling interaction breaking outright (e.g. a double-count,
/// a lost-population regression, or the founder floor firing far more than
/// intended).
///
/// **Not a tight "≈" bound.** Recomputing Σ K directly (the same
/// `carrying_inputs_of` / `species_carrying_input` / `carrying_capacity`
/// formula the composition root feeds into demography, summed over every
/// species and every cell) against seed 42's committed population shows the
/// gap is dominated by `condense()`'s threshold *culling* mass outright, not
/// by rounding or the founder floor: at `THRESHOLD = 10.0`, every attractor
/// whose catchment accumulation falls below the threshold is dropped from
/// the settlement list entirely — its mass is never reassigned to a
/// neighbouring surviving attractor (see `condense.rs`'s module doc; only
/// `threshold == 0.0` conserves exactly, per that module's own test). Measured
/// once (2026-07-13): Σ K = 7187.7407, Σ committed pop = 3971 — a ratio of
/// ≈0.552, i.e. the threshold discards roughly 45% of the field's mass as
/// sub-threshold catchments, not "a few percent". The founder floor and
/// `.round()` at the emit boundary are comparatively tiny (bounded by one
/// person per settlement, ~182 here) and only ever push population UP past
/// what its catchment alone would round to, never account for the bulk of
/// the gap. The bounds below reflect that reality rather than assume a
/// near-exact conservation the design does not provide at this threshold.
#[test]
fn world_level_population_conserves_against_total_capacity() {
    use hornvale_kernel::{Seed, Value};
    let world = hornvale_worldgen::build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed-42 world must build");
    let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain");
    let climate = hornvale_worldgen::climate_of(&world).expect("climate");
    let geo = terrain.geosphere();
    let base_inputs = hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate);
    let roster = hornvale_worldgen::default_roster();
    let mut total_k = 0.0_f64;
    for def in &roster {
        let inputs = hornvale_kernel::CellMap::from_fn(geo, |c| {
            hornvale_worldgen::species_carrying_input(*base_inputs.get(c), &def.psych)
        });
        let k = hornvale_demography::carrying_capacity(geo, &inputs);
        total_k += geo.cells().map(|c| *k.get(c)).sum::<f64>();
    }
    let settlements: Vec<_> = world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .collect();
    let total_pop: f64 = settlements
        .iter()
        .filter_map(|f| {
            match world
                .ledger
                .value_of(f.subject, hornvale_settlement::POPULATION)
            {
                Some(Value::Number(n)) => Some(*n),
                _ => None,
            }
        })
        .sum();
    let count = settlements.len();
    assert!(
        total_pop > 0.0,
        "a peopled seed-42 world has positive population"
    );
    // Upper bound: committed population may only ever exceed Σ K by the
    // per-settlement founder-floor/rounding allowance (never conjure mass
    // beyond that from the field).
    assert!(
        total_pop <= total_k + count as f64,
        "committed population {total_pop} exceeded Σ K {total_k} by more than the \
         {count}-settlement founder-floor/rounding allowance"
    );
    // Lower bound: a generous floor (well under the measured ~0.552 ratio)
    // that still catches a catastrophic conservation break — e.g. population
    // collapsing toward zero — while tolerating the threshold's expected,
    // already-documented mass culling.
    assert!(
        total_pop >= total_k * 0.25,
        "committed population {total_pop} fell far below Σ K {total_k} \
         (ratio {:.4}) — below the threshold-culling floor this guard tolerates",
        total_pop / total_k
    );
}
