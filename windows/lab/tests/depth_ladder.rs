//! Depth-scoped metric values are byte-identical to full-build values, per
//! study. This is Stage 2's acceptance gate (spec §4 / MAP-25): the ladder
//! may build shallow, but a shallow build must return the exact number a
//! full build does. `run` now builds each study only to the deepest rung
//! its selected metrics need (`runner::required_depth`); `run_forced_full`
//! always builds `Full` — this file is the metamorphic guard proving the
//! two never disagree.

use hornvale_lab::{Study, load_study, registry, run, run_forced_full};
use hornvale_worldgen::BuildDepth;
use std::path::Path;

/// The required build depth for a study: the max rung over its selected
/// metrics, mirroring the private `runner::required_depth` this guard
/// checks indirectly (through `run`'s observable behavior).
fn required_depth_of(study: &Study) -> BuildDepth {
    study
        .selected_metrics()
        .expect("study metrics resolve")
        .iter()
        .map(|m| m.rung())
        .max()
        .unwrap_or(BuildDepth::Astronomy)
}

/// Load a committed study and cap its seed count so the guard stays cheap
/// enough for the per-commit gate. `census-of-skies` and `census-of-coasts`
/// are 10,000 seeds in production; recomputing even a handful of seeds
/// TWICE (scoped, then forced-Full) is the whole cost here, so the cap is
/// small. The depth-scoped-vs-full divergence a shallow build could
/// introduce is per-seed (an extractor either reads the right facts for a
/// given world or it doesn't), so a small sample proves it as well as the
/// full census would — the same tradeoff decision
/// `calibration-loads-the-census-fixture` made for the fixture-backed
/// calibration suite.
fn load_capped(study_file: &str, cap: u64) -> Study {
    let study = load_study(Path::new(&format!("../../studies/{study_file}.study.json")))
        .unwrap_or_else(|e| panic!("{study_file} loads: {e}"));
    let mut capped = study;
    capped.seeds.count = capped.seeds.count.min(cap);
    capped
}

/// Assert `run(&study)` (depth-scoped) and `run_forced_full(&study)` produce
/// byte-identical rows, and that the study's computed required depth
/// matches `expected` (a rung-map regression would silently under- or
/// over-build, so this pins the depth as well as the values).
fn assert_scoped_matches_full(study_file: &str, expected_depth: BuildDepth, cap: u64) {
    let study = load_capped(study_file, cap);
    assert_eq!(
        required_depth_of(&study),
        expected_depth,
        "study {study_file}: required build depth changed from what this guard expects — if \
         the rung map moved on purpose, update this expectation"
    );

    let scoped = run(&study).expect("scoped run");
    let full = run_forced_full(&study).expect("full run");
    assert_eq!(
        scoped.rows, full.rows,
        "study {study_file}: depth-scoped rows differ from full-build rows"
    );
}

/// The guard's real teeth: `census-of-skies` and `census-of-coasts` are the
/// two committed studies whose required depth is shallower than `Full`, so
/// `run` and `run_forced_full` build genuinely different worlds
/// (Astronomy-only / Terrain-only vs. Full) and this is the only thing that
/// can catch a rung under-build or a wrong `AsRef` coercion.
///
/// - `census-of-skies` selects only Astronomy-rung metrics (star class,
///   orbital mechanics, moons, calendar) — required depth `Astronomy`.
/// - `census-of-coasts` selects only Terrain-rung metrics (continental
///   shape) — required depth `Terrain`.
///
/// Seed count is capped at 3: each seed here is measured TWICE (once at the
/// study's shallow required depth, once forced to `Full`), and a `Full`
/// build runs the entire settlement/culture/religion/language pipeline
/// (measured ~12-15s per world under the debug profile, even with the
/// terrain/climate/worldgen crates' `opt-level = 2` overrides), so this is
/// the dominant cost in this file — a few tens of seconds total, dwarfed by
/// the rest of `cargo test -p hornvale-lab` (its `--lib` suite alone runs
/// ~15 minutes). The divergence being checked is per-seed, so 3 seeds
/// already exercises the comparison at more than one world; the
/// fixture-backed calibration suite (`tests/calibration.rs`,
/// `tests/fixture_staleness.rs`) is what actually walks these studies at
/// their committed 10,000-seed scale, in CI.
#[test]
fn depth_scoped_metrics_match_full_build() {
    const SEED_CAP: u64 = 3;
    assert_scoped_matches_full("census-of-skies", BuildDepth::Astronomy, SEED_CAP);
    assert_scoped_matches_full("census-of-coasts", BuildDepth::Terrain, SEED_CAP);
}

/// The `"all"`-metric studies always require `Full` (the registry includes
/// Full-rung metrics like `belief-kind`), so `run` and `run_forced_full`
/// take the *identical* code path for them — there is no depth divergence
/// to catch here, only a check that the always-Full path still dispatches
/// correctly under the new `required`-depth machinery.
///
/// Ignored by default: unlike the Astronomy/Terrain-only studies above, an
/// `"all"` selection evaluates the full ~110-metric registry per world (most
/// of it Full-rung, several metrics themselves iterating every cell), which
/// costs seconds per world even at `Terrain`'s or `Astronomy`'s multiplied-
/// out share (measured ~15-35s per seed, roster-dependent, with the
/// terrain/climate/worldgen crates' `opt-level = 2` overrides already
/// applied) — capping the seed count doesn't fix this the way it does for
/// the two studies above, because the bottleneck is metric-evaluation
/// count, not build depth. Run explicitly after touching the runner, or in
/// CI: `cargo test -p hornvale-lab --test depth_ladder -- --ignored`.
#[test]
#[ignore = "pays the full ~110-metric registry evaluation at Full depth, twice, per seed; see \
            this test's doc comment"]
fn depth_scoped_metrics_match_full_build_for_all_metric_studies() {
    const SEED_CAP: u64 = 1;
    assert_scoped_matches_full("census-lands-drift", BuildDepth::Full, SEED_CAP);
    assert_scoped_matches_full("census-of-the-meeting", BuildDepth::Full, SEED_CAP);
}

/// The `"all"`-metric studies' required depth, checked without paying for
/// an actual run: cheap enough to stay in the default gate, and it is what
/// actually matters about them for this guard (that `required_depth`
/// resolves to `Full`, not that the tautological `Full == Full` comparison
/// ran end to end — the ignored test above does that, at cost).
#[test]
fn all_metric_studies_require_full_depth() {
    for study_file in ["census-lands-drift", "census-of-the-meeting"] {
        let study = load_capped(study_file, 1);
        assert_eq!(
            required_depth_of(&study),
            BuildDepth::Full,
            "study {study_file}: an \"all\" metric selection must require Full depth"
        );
    }
}

/// The rung-map corrections that motivated this stage (commit history:
/// "correct rung map — belief-kind is Full, hue-depth is Astronomy")
/// checked directly against the registry, so a future edit that moves
/// either metric's extractor to the wrong rung fails here immediately
/// rather than only showing up as a missed speedup that was never measured.
#[test]
fn belief_kind_and_hue_depth_have_the_expected_rung() {
    let reg = registry();
    let belief_kind = reg
        .iter()
        .find(|m| m.name == "belief-kind")
        .expect("belief-kind is registered");
    assert_eq!(
        belief_kind.rung(),
        BuildDepth::Full,
        "belief-kind reads religion facts and must require a Full build"
    );

    let hue_depth = reg
        .iter()
        .find(|m| m.name == "hue-depth-goblin")
        .expect("hue-depth-goblin is registered");
    assert_eq!(
        hue_depth.rung(),
        BuildDepth::Astronomy,
        "hue-depth-* is derived from the perception vector alone and must require only an \
         Astronomy build"
    );
}
