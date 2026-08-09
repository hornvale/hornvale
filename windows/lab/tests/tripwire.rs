//! The staleness tripwire (The Assay, spec §4.2).
//!
//! A census-resident check is verified when the census regenerates — once per
//! campaign at the close. Decision 0097 §4 records the cost of leaving that
//! unguarded: The Siding found the census stale for 139 commits while every
//! gate ran green. This test bounds that window to a single commit, for the
//! seeds it covers and the columns named in `GUARDED`.
//!
//! It calls the metrics' OWN extractors, reached through
//! `hornvale_lab::registry()`, rather than reimplementing their logic — a
//! reimplementation would drift from the thing it guards, which is the one
//! failure a staleness guard cannot afford.
//!
//! **What it does not do.** Three seeds cannot prove the whole fixture fresh.
//! A drift that moves only seeds outside `TRIPWIRE_SEEDS` still waits for the
//! regen; the full proof remains `calibration.rs`'s ignored
//! `census_fixture_matches_live_run`. This bounds staleness, it does not
//! eliminate it (spec §10). **Nor does it reach along the column axis**:
//! `GUARDED` names 4 of the census's 190 columns, so this guard compares 12
//! of 190,000 cells (3 seeds × 4 columns, against 1,000 rows × 190 columns) —
//! about 0.006%. A drift that moves any of the other 186 columns, even on
//! seed 0 itself, is invisible to this test, and the 24 pre-existing
//! calibration checks in `windows/lab/tests/calibration.rs` that read the
//! committed census (`&*DRIFT`) are exactly as unguarded on that axis as
//! they were before this campaign. `fixture_staleness.rs` IS the column-axis
//! complement here — it regenerates every metric, all 190 columns, for 6
//! seeds (3 fixed + a 3-seed rotating window) rather than this test's 3 fixed
//! seeds at 4 columns; it sits in the heavy tier rather than the gate, for
//! the cost reasons in its own module doc. It also does not guard a row's
//! *refusal status*:
//! when `row.refusal.is_some()` the loop `continue`s before building or
//! comparing anything, so a tripwire seed whose refusal goes stale — a world
//! that used to refuse now builds, or the reverse — is silently skipped
//! rather than flagged. Confirmed below under "Mutation evidence".
//!
//! # Mutation evidence
//!
//! Recorded 2026-08-07, against the census fixture committed at `d36be41b`
//! (regenerated on the canonical host, lefford).
//!
//! **The tripwire catches a genuine value drift.** With the fixture
//! untouched, the test passes in 11.330s. Editing exactly one cell — seed 0's
//! `crisis-fires` value in the committed `rows.csv`, from `true` to `false`,
//! leaving every other cell untouched — turns the test red with this verbatim
//! output:
//!
//! ```text
//!     FAIL [   4.072s] (1/1) hornvale-lab::tripwire the_committed_census_agrees_with_a_live_rebuild_of_the_tripwire_seeds
//!   stderr ───
//!     thread 'the_committed_census_agrees_with_a_live_rebuild_of_the_tripwire_seeds' (559802970) panicked at windows/lab/tests/tripwire.rs:137:13:
//!     assertion `left == right` failed: STALE CENSUS FIXTURE: metric "crisis-fires" at seed 0 reads Flag(true) live but Flag(false) in the committed rows.csv. The worldgen path moved after the fixture was authored, so every census-backed assertion in windows/lab/tests/ is measuring old worlds. Regenerate with `bash scripts/census-run.sh`, review `make lab-diff STUDY=the-census`, and commit the result.
//!       left: Flag(true)
//!      right: Flag(false)
//! ```
//!
//! Restoring the cell (`git checkout` of the fixture, confirmed clean with
//! `git diff --exit-code`) returns the test to green (11.409s). The guard
//! fires on the exact metric, seed, and both values, as designed.
//!
//! **The refusal-skip gap is real, and confirmed rather than assumed.**
//! Restoring the fixture first, then setting seed 0's `refusal` cell (the
//! last CSV column, empty by default) to a non-empty probe string —
//! `"probe: refusal-staleness test"` — while leaving `crisis-fires` at its
//! true, correct value: the test still **passes** (7.209s — about 4s faster,
//! consistent with seed 0's build being skipped entirely once
//! `row.refusal.is_some()` short-circuits the loop before `built()` runs).
//! No assertion for seed 0 ever executes. This is the expected, worse
//! outcome named above: a stale refusal status is invisible to this guard.
//! Restoring the fixture (`git checkout`, confirmed clean) returns the test
//! to its normal green (11.314s).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{
    BuiltView, FullView, RunResult, TerrainView, canonical_value, load_rows, load_study, registry,
};
use hornvale_worldgen::BuildDepth;
use std::path::Path;

/// The seeds rebuilt live on every commit. Three, and the lowest three, so the
/// set is obvious rather than chosen — a tripwire whose seeds look
/// hand-picked invites the suspicion that they were picked to pass.
const TRIPWIRE_SEEDS: [u64; 3] = [0, 1, 2];

/// The metric columns this tripwire guards: every metric The Assay moved a
/// check onto. Four now (`hydro-variant-coverage`, `toponymic-core-size`,
/// `toponymic-roots-won`, `crisis-fires`) — Tasks 4-6 registered them; the
/// list started empty (a green test, deliberately, so this task could land
/// before the metrics existed) and grows as a later campaign moves more.
const GUARDED: &[&str] = &[
    "hydro-variant-coverage",
    "toponymic-core-size",
    "toponymic-roots-won",
    "crisis-fires",
];

/// The canonical census and its committed rows.
const STUDY: &str = "../../studies/the-census.study.json";
const ROWS: &str = "../../book/src/laboratory/generated/the-census/rows.csv";

/// Load the committed census exactly as `calibration.rs` does.
fn committed() -> RunResult {
    let study = load_study(Path::new(STUDY)).expect("load the census study");
    let csv = std::fs::read_to_string(ROWS).expect("read the census fixture");
    load_rows(&study, &csv).expect("reconstruct the census from its fixture")
}

/// Build one seed to `depth`, as `BuiltView` so the registry's extractors
/// apply unchanged. Uses the public rung constructors, which assemble the
/// shipped roster internally — the same roster the census uses.
fn built(seed: u64, depth: BuildDepth) -> BuiltView {
    let pins = SkyPins::default();
    match depth {
        BuildDepth::Full => BuiltView::Full(
            FullView::build(Seed(seed), &pins).expect("tripwire seed builds to Full"),
        ),
        _ => BuiltView::Terrain(
            TerrainView::build(Seed(seed), &pins).expect("tripwire seed builds to Terrain"),
        ),
    }
}

/// The deepest rung any guarded metric needs — so a tranche of Terrain-rung
/// checks pays 3 x 0.49 s rather than 3 x 3.90 s (spec §3.2's depth rule).
fn deepest_guarded_rung() -> BuildDepth {
    let all = registry();
    let mut deepest = BuildDepth::Terrain;
    for name in GUARDED {
        let m = all
            .iter()
            .find(|m| m.name == *name)
            .unwrap_or_else(|| panic!("guarded metric {name:?} is not in the registry"));
        if matches!(m.rung(), BuildDepth::Settlements | BuildDepth::Full) {
            deepest = BuildDepth::Full;
        }
    }
    deepest
}

/// The committed census fixture must agree with a live rebuild of the three
/// tripwire seeds, for every metric named in `GUARDED`. This is the load-
/// bearing assumption behind moving a check onto the census (spec §4.2): if
/// the fixture ever drifts from the worldgen path it claims to describe, this
/// test — not a human noticing months later — is what turns red. Green by
/// construction while `GUARDED` is empty; gains teeth as Tasks 4-6 register
/// metrics.
/// claim: invariant(forall-seed) — the tripwire mechanism itself (Task 2/3);
/// GUARDED now holds four names (Tasks 4-6), so the early-return below is
/// dead in practice but kept as a defensive no-op for an empty roster
#[test]
fn the_committed_census_agrees_with_a_live_rebuild_of_the_tripwire_seeds() {
    if GUARDED.is_empty() {
        // Defensive no-op: GUARDED holds four names today (Tasks 4-6), so
        // this branch is not live, but it keeps the test green by
        // construction if a future edit ever empties the roster again.
        return;
    }
    let census = committed();
    let all = registry();
    let depth = deepest_guarded_rung();

    for seed in TRIPWIRE_SEEDS {
        let row = census
            .rows
            .iter()
            .find(|r| r.seed == seed && r.pin_set == "default")
            .unwrap_or_else(|| panic!("seed {seed} has no default-pin row in the fixture"));
        if row.refusal.is_some() {
            // A refused world has Absent for every metric; nothing to compare.
            continue;
        }
        let view = built(seed, depth);

        for name in GUARDED {
            let metric = all
                .iter()
                .find(|m| m.name == *name)
                .expect("guarded metric exists");
            let column = census
                .metric_names
                .iter()
                .position(|n| n == name)
                .unwrap_or_else(|| {
                    panic!(
                        "the committed fixture has no {name:?} column — regenerate the \
                         census (scripts/census-run.sh) before this metric can be guarded"
                    )
                });

            // Canonicalize the LIVE value: the fixture's floats crossed
            // `render_csv`'s quantizing boundary and the live value has not.
            let live = canonical_value(metric.extract.apply(&view));
            let stored = &row.values[column];

            assert_eq!(
                &live, stored,
                "STALE CENSUS FIXTURE: metric {name:?} at seed {seed} reads {live:?} live \
                 but {stored:?} in the committed rows.csv. The worldgen path moved after \
                 the fixture was authored, so every census-backed assertion in \
                 windows/lab/tests/ is measuring old worlds. Regenerate with \
                 `bash scripts/census-run.sh`, review `make lab-diff STUDY=the-census`, \
                 and commit the result."
            );
        }
    }
}
