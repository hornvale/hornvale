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

// Repointed at the-census merge — census-of-the-gathering folded into main's
// `the-census` (1000 seeds, `metrics: "all"`). The census has since been
// regenerated (2026-07-14, `6ae415c`, folding in the-gathering's field
// condensation and the night-sky campaign's phenomena) and this file's
// gradient/latitude pins re-measured against it below.
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
    //
    // Census regen (2026-07-14, the 1000-seed `the-census`, folding in
    // the-gathering's field condensation + the night-sky campaign's
    // phenomena): re-measured; the preregistered floor of 3 still clears
    // decisively.
    //
    // Census regen (2026-07-16, post-sculpting/isotherm/true-name 1000-seed
    // regen, commit 1c954d0): re-measured (26.2645 -> 26.6509); the
    // preregistered floor of 3 still clears decisively.
    assert!(
        (mean - 26.6509).abs() < 1e-3,
        "capacity-by-abs-latitude mean drifted: {mean:.4} (expected ~26.6509)"
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
    //
    // Census regen (2026-07-14, the 1000-seed `the-census`, folding in
    // the-gathering's field condensation + the night-sky campaign's
    // phenomena): re-measured; still comfortably below the uniform-sphere
    // baseline.
    //
    // Census regen (2026-07-16, post-sculpting/isotherm/true-name 1000-seed
    // regen, commit 1c954d0): re-measured (10.7459 -> 12.5595); still
    // comfortably below the uniform-sphere baseline of 32.7.
    assert!(
        (mean - 12.5595).abs() < 1e-3,
        "pop-weighted-abs-latitude mean drifted: {mean:.4} (expected ~12.5595)"
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
/// **Re-based onto niche-K post-cutover.** Settlement genesis no longer
/// packs against the flat, psychology-only `carrying_inputs_of` /
/// `species_carrying_input` / `carrying_capacity` path this guard used to
/// recompute — Task A15a cut genesis over onto the niche-differentiated K
/// (`niche_per_species_k`, The Niche) the coexistence stack actually
/// competes against (windows/worldgen `build_to`'s `climate+settlements`
/// stage). Comparing committed population against the OLD flat Σ K would
/// measure the invariant against a capacity the population was never
/// realized from. Σ K is now recomputed via
/// `hornvale_worldgen::demography_report` — the pure, deterministic
/// accessor that mirrors genesis's own `niche_per_species_k` → `coexist::
/// pack` → `stack_condense::condense_stack` pipeline byte-for-byte at the
/// frozen `BETA`/`FLOOR` constants — summing `per_species_k` over every
/// peopled species and every cell, exactly as the brief's re-basing
/// instructs.
///
/// **Not a tight "≈" bound, and looser than before in a second way now.**
/// Two effects widen the gap between Σ K and committed population: (1)
/// `condense()`'s threshold still *culls* mass outright at `THRESHOLD =
/// 10.0` rather than reassigning it (see `condense.rs`'s module doc;
/// unchanged by this cutover); AND (2) the coexistence stack commits a
/// `population` fact for only the DOMINANT species at each attractor —
/// bugbear and kobold are present in every settlement's composition (see
/// `bugbear_and_kobold_are_present_in_settlement_composition`,
/// `cli/tests/branches_identity.rs`) and so contribute to Σ K, but their
/// share of the local capacity is never committed as anyone's population.
/// Measured (2026-07-16, seed 42): Σ K = 315.8861753551 (the niche K's
/// saturating, per-species-bounded scale — not the old flat formula's
/// larger absolute-headcount scale), Σ committed pop = 110, count = 66
/// settlements — a ratio of ≈0.3482, comfortably inside the same
/// structural bounds (population within the founder-floor/rounding
/// allowance above Σ K; population at least a quarter of Σ K) the
/// pre-cutover guard used, so those bounds are kept as-is rather than
/// retuned.
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
    let roster: Vec<hornvale_species::SpeciesDef> = hornvale_worldgen::default_roster()
        .into_iter()
        .filter(|d| d.peopled.is_some())
        .collect();
    let report = hornvale_worldgen::demography_report(&world, &roster)
        .expect("demography_report must recompute over an already-built world's committed facts");
    // Sum the niche-differentiated K (the coexistence stack's actual
    // packing capacity) over every peopled species and every cell —
    // mirroring genesis's own unpinned `species_set` (all-peopled at seed
    // 42, so no filtering difference from `roster` above).
    let total_k: f64 = report
        .per_species_k
        .iter()
        .map(|(_, k)| k.iter().map(|(_, v)| *v).sum::<f64>())
        .sum();
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
    // Lower bound: a generous floor (well under the measured ~0.3482 ratio)
    // that still catches a catastrophic conservation break — e.g. population
    // collapsing toward zero — while tolerating the threshold's expected,
    // already-documented mass culling AND the coexistence stack's
    // dominant-only population commit (see this test's doc comment).
    assert!(
        total_pop >= total_k * 0.25,
        "committed population {total_pop} fell far below Σ K {total_k} \
         (ratio {:.4}) — below the threshold-culling floor this guard tolerates",
        total_pop / total_k
    );
}
