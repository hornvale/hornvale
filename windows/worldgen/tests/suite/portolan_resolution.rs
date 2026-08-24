//! The Portolan's preregistered H2 (Task 4): "the same cell resolves to the
//! same name on repeat visits within a session, and across a rebuild of the
//! world from its seed." Falsified by any variation, which would mean
//! [`FeatureClass::salience`]'s ordering is not actually total in practice —
//! a gap between the declared order and what `VertexFeatureIndex::build`
//! enforces. See `docs/superpowers/specs/2026-08-19-the-portolan-design.md`
//! and `.superpowers/sdd/2026-08-19-the-portolan/task-4-brief.md`.
//!
//! **Read this before trusting a green run here.** This campaign's lineage
//! has already shipped two measurements that came out at exactly 100% and
//! could not have come out otherwise (the Gazetteer's cross-people H2 —
//! `gazetteer_naming.rs`'s own doc comment tells that story in full — and
//! this campaign's H1). H2 as literally stated risks being a third:
//! determinism is Hornvale's constitutional guarantee (same seed, same
//! pins, byte-identical output), so "does a deterministic function return
//! the same value twice" is close to structurally incapable of failing —
//! the only way it could fail is a *pre-existing* determinism bug
//! elsewhere in the pipeline (a banned `HashMap`/`HashSet` slipping past
//! `clippy.toml`'s `disallowed-types`, an iteration order that leaks
//! through, a stray platform transcendental), which this file would catch
//! only as a side effect, not because it targets the mechanism.
//!
//! `seed_42_resolution_is_stable_across_an_independent_rebuild` below is
//! that literal, low-teeth reading, kept because the brief asks for it
//! directly and because "no side-effect determinism bug snuck in" is still
//! worth one assertion. The test that actually has teeth is
//! `seed_42_index_order_is_independent_of_build_input_order`: determinism
//! guarantees a *given* build of `VertexFeatureIndex` returns the same
//! answer every time, but it says nothing about whether two *different*
//! orderings of the same feature set, fed to `VertexFeatureIndex::build`,
//! produce the same stacks. A tie broken by insertion order (e.g. an
//! unstable sort keyed on `salience` alone, with no secondary key) would
//! still be perfectly deterministic per-build — call it twice on the same
//! input, get the same output both times — while being unstable across a
//! reordering of that input. That is exactly the gap Task 3's salience
//! docstring calls out ("total order -- every pair must disagree") without
//! anything upstream of this file exercising it against `build`'s actual
//! sort. `VertexFeatureIndex::build` (`domains/terrain/src/landscape.rs`)
//! sorts each cell's stack by `(id.class.salience(), *id)` — salience
//! first, the full `FeatureId` (class then canonical cell) as a
//! never-tying secondary key — so today's implementation is already immune
//! to this; this file exists to keep it that way and to name the concrete
//! defect a regression here would be.

use hornvale_kernel::{Geosphere, Seed};
use hornvale_language::{Envelope, ExoticSeg, MorphOptions, Phonology, draw_phonology};
use hornvale_terrain::landscape::{Feature, VertexFeatureIndex};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::{gazetteer_features, resolve_at};

/// The production mesh level (`hornvale_terrain::GLOBE_LEVEL`), the same
/// level every other seed-42 measurement in this crate uses (`gazetteer.rs`,
/// `gazetteer_naming.rs`, `resolve.rs`'s own test module) so this file's
/// counts are comparable to the ones already on record.
const LEVEL: u32 = 6;
const PEOPLE: &str = "aeldrin";

/// A fixed phonology/morphology pair, drawn once per test the same way
/// `resolve.rs`'s own test module does — the identity of the people asking
/// is not what H2 is about, so it is held constant throughout this file.
fn test_phonology() -> (Phonology, MorphOptions) {
    let ph = draw_phonology(
        &Seed(7),
        PEOPLE,
        &Envelope {
            labiality: 1.0,
            vowel_space: 1.0,
            voicing: 1.0,
            sibilance: 1.0,
            voice_loudness: 1.0,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        },
        &hornvale_language::typology::concatenative(),
    );
    let morph = MorphOptions {
        honorifics: false,
        shape_weights: [1.0, 1.0, 1.0],
        shape_beta: 1.0,
    };
    (ph, morph)
}

/// Generate seed 42's real terrain and its full feature set from scratch —
/// the same `hornvale_terrain::generate` -> `GeneratedTerrain::new` ->
/// `gazetteer_features` chain every other seed-42 test in this crate uses.
/// Called twice with the same seed by the rebuild test below, each call a
/// fully independent genesis (a new `Geosphere`, a new `generate` call, a
/// new features `Vec`) — nothing here is shared or cached between calls.
fn build_seed_42_from_scratch(seed: Seed) -> (Geosphere, Vec<Feature>, VertexFeatureIndex) {
    let geo = Geosphere::new(LEVEL);
    let outcome = hornvale_terrain::generate(seed, &geo, &TerrainPins::default())
        .expect("default pins generate");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    let features = gazetteer_features(seed, &geo, &terrain);
    let index = VertexFeatureIndex::build(&features);
    (geo, features, index)
}

/// The literal H2 reading from the brief: build seed 42 twice from scratch,
/// resolve every cell in both, assert the answers are identical.
///
/// **Could this test have failed?** Only via a pre-existing determinism bug
/// upstream of this file (a banned nondeterministic collection, a leaked
/// iteration order, a stray platform transcendental) — not via anything
/// this file targets on purpose. It is included because the brief asks for
/// it directly and it does add one real check for free: a fixed, printed
/// positive-control count (40,585 of 40,962 cells resolve, the figure this
/// campaign has measured and recorded since Task 3) rules out the
/// vacuous-pass shape where every cell resolves to `None` in both builds
/// and the loop below trivially agrees.
#[test]
fn seed_42_resolution_is_stable_across_an_independent_rebuild() {
    let seed = Seed(42);
    let (geo_a, _features_a, index_a) = build_seed_42_from_scratch(seed);
    let (geo_b, _features_b, index_b) = build_seed_42_from_scratch(seed);

    assert_eq!(
        geo_a.vertex_count(),
        geo_b.vertex_count(),
        "two builds of the same seed produced different geospheres"
    );

    let (ph, morph) = test_phonology();
    let mut resolved = 0usize;
    let mut checked = 0usize;

    for cell in geo_a.vertices() {
        let a = resolve_at(&index_a, cell, seed, PEOPLE, &ph, &morph);
        let b = resolve_at(&index_b, cell, seed, PEOPLE, &ph, &morph);
        assert_eq!(
            a, b,
            "cell {cell:?} resolved differently across an independent rebuild: {a:?} vs {b:?}"
        );
        if a.is_some() {
            resolved += 1;
        }
        checked += 1;
    }

    println!("H2 (rebuild): seed 42, level {LEVEL}, {checked} cells checked, {resolved} resolved");

    // Positive control: the exact seed-42 shape this campaign has measured
    // and recorded (module doc, Task 3's CLAUDE.md summary). A build that
    // silently resolved nothing (e.g. an empty index) would still pass the
    // loop above trivially; this pins the count so that failure mode reads
    // as a failure.
    assert_eq!(
        checked, 40_962,
        "seed 42 at level {LEVEL} should have 40,962 cells"
    );
    assert_eq!(
        resolved, 40_585,
        "seed 42 should resolve 40,585 of 40,962 cells (measured, Task 3)"
    );
}

/// The test that actually has teeth: `VertexFeatureIndex::build` fed the same
/// feature set in three different orders — forward, fully reversed, and a
/// halves-interleaved shuffle that shares no simple positional relationship
/// with either — must produce byte-identical per-cell stacks.
///
/// Determinism does not give this away for free: a *given* call to `build`
/// is guaranteed to be internally deterministic (same input, same output,
/// every time), but nothing about that guarantees `build` is
/// *order-independent* — a tie-break that fell through to the input
/// `Vec`'s own order (e.g. `sort_by_key` on `salience` alone, relying on a
/// stable sort's original-order tie-break, or an unstable sort with no
/// secondary key at all) would still pass the rebuild test above every
/// time, because each independent rebuild happens to construct `features`
/// in the same order — while silently depending on an ordering
/// `VertexFeatureIndex::build`'s own contract never promises the caller.
/// This test varies the one input the rebuild test held fixed by accident.
#[test]
fn seed_42_index_order_is_independent_of_build_input_order() {
    let seed = Seed(42);
    let geo = Geosphere::new(LEVEL);
    let outcome = hornvale_terrain::generate(seed, &geo, &TerrainPins::default())
        .expect("default pins generate");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    let features = gazetteer_features(seed, &geo, &terrain);
    assert!(
        features.len() > 100,
        "need a real, multi-hundred feature population to make a permutation \
         test meaningful (seed 42 measures 405; got {})",
        features.len()
    );

    let forward = VertexFeatureIndex::build(&features);

    let mut reversed = features.clone();
    reversed.reverse();
    let reversed_index = VertexFeatureIndex::build(&reversed);

    // A halves-interleaved shuffle: split in two, then alternate one from
    // each half. Shares no simple relationship (identity or full reversal)
    // with the forward order, so it would not be fooled by a tie-break that
    // merely depends on first-vs-last position.
    let mid = features.len() / 2;
    let (first_half, second_half) = features.split_at(mid);
    let mut interleaved: Vec<Feature> = Vec::with_capacity(features.len());
    let mut fi = first_half.iter();
    let mut si = second_half.iter();
    loop {
        match (fi.next(), si.next()) {
            (Some(a), Some(b)) => {
                interleaved.push(a.clone());
                interleaved.push(b.clone());
            }
            (Some(a), None) => interleaved.push(a.clone()),
            (None, Some(b)) => interleaved.push(b.clone()),
            (None, None) => break,
        }
    }
    assert_eq!(interleaved.len(), features.len());
    let interleaved_index = VertexFeatureIndex::build(&interleaved);

    let mut multi_feature_cells = 0usize;
    let mut checked = 0usize;

    for cell in geo.vertices() {
        let want = forward.at(cell);
        assert_eq!(
            reversed_index.at(cell),
            want,
            "cell {cell:?}: reversing VertexFeatureIndex::build's input order changed the resolved stack"
        );
        assert_eq!(
            interleaved_index.at(cell),
            want,
            "cell {cell:?}: interleaving VertexFeatureIndex::build's input order changed the resolved stack"
        );
        if want.len() > 1 {
            multi_feature_cells += 1;
        }
        checked += 1;
    }

    println!(
        "H2 (permuted build order): seed 42, level {LEVEL}, {checked} cells checked, \
         {multi_feature_cells} multi-feature cells (where a tie-break actually fires)"
    );

    // Positive control: this assertion only has teeth where a cell's stack
    // has more than one entry (a single-feature cell's "order" is trivial
    // regardless of tie-break behaviour). Pin the count so a future change
    // that collapsed every cell to at most one feature would not silently
    // turn this into a vacuous pass.
    assert!(
        multi_feature_cells > 1_000,
        "expected thousands of multi-feature cells at seed 42 (containment is common — \
         volcanoes sit inside landmasses, landmasses inside nothing, etc.); got {multi_feature_cells}"
    );
}
