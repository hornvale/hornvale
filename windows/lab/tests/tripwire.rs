//! The staleness tripwire (The Assay, spec §4.2).
//!
//! A census-resident check is verified when the census regenerates — once per
//! campaign at the close. Decision 0097 §4 records the cost of leaving that
//! unguarded: The Siding found the census stale for 139 commits while every
//! gate ran green. This test bounds that window to a single commit, for the
//! seeds it covers.
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
//! eliminate it (spec §10).

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
/// check onto. Empty until Task 4 adds the first — an empty guard list is a
/// green test, deliberately, so this task lands before the metrics exist.
const GUARDED: &[&str] = &[];

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
#[test]
fn the_committed_census_agrees_with_a_live_rebuild_of_the_tripwire_seeds() {
    if GUARDED.is_empty() {
        // No check has moved onto the census yet. Green by construction, and
        // it stays that way until Task 4 adds the first guarded name.
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
