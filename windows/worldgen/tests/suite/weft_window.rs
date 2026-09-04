//! `WeftWindow` (The Weft, Task 6) must be invisible: reading through it must
//! give exactly what deriving directly gives, and dropping everything it
//! holds must change no answer. This file's two tests are the ones the plan
//! names as mattering most — byte-identity against no-window, and chaos
//! eviction — mirroring the shape `kernel/tests/suite/derived.rs`'s own
//! `Derived` battery already uses (`eviction_at_every_opportunity_changes_
//! nothing_observable`) and `windows/vessel/tests/suite/the_detent.rs`'s
//! `ground_memo_survives_chaos_eviction`.
//!
//! Test fixture (decision 0092): calls the sculpt derivation entry point
//! (`terrain_of`) directly to build its own world state, once per test.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{Facet, Geosphere, NearestVertexIndex, Seed, Vertex, VertexMap};
use hornvale_worldgen::{FieldPack, WeftFeature, WeftKind, WeftWindow};

/// Same relationship `weft_prevalence.rs` reproduces for the same reason:
/// this file cannot import `windows/locale` (it depends on
/// `hornvale-worldgen`, the crate under test), so `walk_depth`'s own
/// documented value ("the globe level plus 7") is restated rather than
/// imported.
const WALK_DEPTH_BELOW_GRID: u32 = 7;

/// Everything a derivation call needs, built once per test and kept alive
/// for its duration (`geo()`/`pack()`/`index()` all borrow from here).
struct Fixture {
    world: hornvale_kernel::World,
    terrain: hornvale_terrain::GeneratedTerrain,
    pack: FieldPack,
    index: NearestVertexIndex,
}

impl Fixture {
    fn build() -> Self {
        let world = hornvale_worldgen::seed_42_world();
        let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
        let pack = hornvale_worldgen::field_pack_from(&terrain);
        let index = NearestVertexIndex::new(terrain.geosphere());
        Self {
            world,
            terrain,
            pack,
            index,
        }
    }

    fn geo(&self) -> &hornvale_kernel::Geosphere {
        self.terrain.geosphere()
    }

    fn seed(&self) -> hornvale_kernel::Seed {
        self.world.seed
    }

    /// `steps` edge-adjacent facets starting at vertex 0
    /// (`neighbors()[0]` is always the "+a" edge step — present even at a
    /// cube corner, per `Facet::neighbor_steps`'s own doc — so this never
    /// panics), matching `weft_prevalence.rs`'s own walk exactly (same
    /// starting vertex, same walk depth, same seed): that file measured this
    /// walk non-degenerate — `occurs` fires 7 of 200 times, prevalence spans
    /// `[0.00071, 0.04641]` — so reusing it here means this file's own
    /// non-vacuity checks below rest on an already-measured walk rather than
    /// a fresh, unverified one.
    fn walk(&self, steps: usize) -> Vec<Facet> {
        let geo = self.geo();
        let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;
        let mut facet = Facet::containing(geo.position(Vertex(0)), walk_depth);
        let mut out = Vec::with_capacity(steps);
        for _ in 0..steps {
            out.push(facet.clone());
            facet = facet
                .neighbors()
                .into_iter()
                .next()
                .expect("a facet always has an edge neighbour");
        }
        out
    }

    /// The no-window oracle: exactly what `WeftWindow::features_at` derives
    /// internally (`prevalence` then, on `Some`, `occurs`), computed here
    /// independently so a divergence in either implementation shows up as a
    /// test failure rather than being definitionally impossible to observe.
    fn direct_features(&self, kind: WeftKind, facet: &Facet) -> Vec<WeftFeature> {
        self.direct_features_with_seed(kind, facet, self.seed())
    }

    /// Same oracle as [`Self::direct_features`], but against an explicit
    /// `seed` rather than this fixture's own world seed — [`prevalence`]/
    /// [`occurs`] take `seed` purely as a derivation parameter, independent
    /// of which world built `geo`/`index`/`pack`, so this lets a test probe
    /// many candidate seeds against the one real terrain this fixture
    /// already paid to build, instead of building a second world per seed.
    fn direct_features_with_seed(
        &self,
        kind: WeftKind,
        facet: &Facet,
        seed: Seed,
    ) -> Vec<WeftFeature> {
        direct_features_over(kind, facet, self.geo(), &self.index, &self.pack, seed)
    }
}

/// The no-window oracle, free-standing (not tied to a `Fixture`'s own
/// `geo`/`index`/`pack`) so the globe-level isolation test can run it against
/// a second, independently-built `Geosphere`/`NearestVertexIndex`/`FieldPack`
/// without a `Fixture` to own them.
fn direct_features_over(
    kind: WeftKind,
    facet: &Facet,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    pack: &FieldPack,
    seed: Seed,
) -> Vec<WeftFeature> {
    match hornvale_worldgen::prevalence(kind, facet, geo, index, pack, seed) {
        Some(p) if hornvale_worldgen::occurs(kind, facet, seed, p) => vec![WeftFeature {
            kind,
            prevalence: p,
        }],
        _ => Vec::new(),
    }
}

/// A window is a cache and a cache must not be observable. Running with the
/// window and without it must give byte-identical features for the same
/// facets. This is the contract `blend_at_cached` already states
/// ("`cache: None` is byte-identical to `blend_at`") and the prohibition The
/// Terrier wrote: a hidden cache in a derivation path is how derived state
/// stops being derived.
///
/// **Why this discriminates, not just agrees on an empty surface.** Both
/// sides are asserted non-vacuous below, independently of the equality
/// check: `occurs` must have fired at least once (a facet with a real,
/// non-empty `Vec<WeftFeature>`) and `prevalence` must have actually varied
/// across the walk. A test that only compared two runs of empty vectors
/// would pass unconditionally and prove nothing about the window; this one
/// cannot pass that way, because the walk is the one `weft_prevalence.rs`
/// already measured to avoid exactly that floor.
#[test]
fn the_window_is_byte_identical_to_no_window() {
    const STEPS: usize = 200;
    const ADVANCE_RADIUS: u32 = 3;

    let fx = Fixture::build();
    let walk = fx.walk(STEPS);

    // No window: derive every facet directly.
    let direct: Vec<Vec<WeftFeature>> = walk
        .iter()
        .map(|f| fx.direct_features(WeftKind::Spring, f))
        .collect();

    // Through a window advanced along the exact same walk, one step at a
    // time — the real usage shape (a walker's centre moves one facet per
    // turn) — then read back.
    let mut window = WeftWindow::new();
    let mut windowed: Vec<Vec<WeftFeature>> = Vec::with_capacity(STEPS);
    for facet in &walk {
        window.advance_to(
            facet,
            ADVANCE_RADIUS,
            fx.geo(),
            &fx.index,
            &fx.pack,
            fx.seed(),
        );
        windowed.push(
            window
                .features_at(
                    WeftKind::Spring,
                    facet,
                    fx.geo(),
                    &fx.index,
                    &fx.pack,
                    fx.seed(),
                )
                .to_vec(),
        );
    }

    assert_eq!(
        direct, windowed,
        "a window read must be byte-identical to a direct derivation at every facet"
    );

    // Non-vacuity: this walk must actually exercise both `occurs` (a
    // non-empty entry somewhere) and `prevalence`'s continuous variation
    // (not a constant field) — otherwise the equality above would hold
    // trivially over a wall of empty `Vec`s.
    let occurs_count = direct.iter().filter(|v| !v.is_empty()).count();
    assert!(
        occurs_count >= 1,
        "the walk must include at least one real occurrence, or this test compares empty vectors; got {occurs_count} of {STEPS}"
    );
    let prevalences: Vec<f64> = walk
        .iter()
        .map(|f| {
            hornvale_worldgen::prevalence(
                WeftKind::Spring,
                f,
                fx.geo(),
                &fx.index,
                &fx.pack,
                fx.seed(),
            )
            .expect("a walk-depth facet is always deeper than the geosphere's own level")
        })
        .collect();
    let min = prevalences.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = prevalences
        .iter()
        .cloned()
        .fold(f64::NEG_INFINITY, f64::max);
    assert!(
        max - min >= 0.01,
        "prevalence must actually vary over the walk (spread {:.5}), or the byte-identity \
         check above could pass on a constant field",
        max - min
    );
}

/// `Derived` already ships a chaos-eviction property battery
/// (`kernel/tests/suite/derived.rs`,
/// `eviction_at_every_opportunity_changes_nothing_observable`) proving
/// eviction cannot change an answer; this mirrors it directly over
/// `WeftWindow`, on the same non-degenerate walk `the_window_is_byte_
/// identical_to_no_window` uses, sampled with repeats so hits and misses
/// both occur (matching `derived.rs`'s own repeating-key-sequence shape —
/// no `rand` crate exists in this workspace, decision 0004, so the sequence
/// is the same deterministic multiplicative scramble that file uses).
#[test]
fn window_survives_chaos_eviction() {
    const WALK_LEN: usize = 200;
    const READS: usize = 600;

    let fx = Fixture::build();
    let walk = fx.walk(WALK_LEN);
    let sequence: Vec<usize> = (0..READS as u64)
        .map(|i| (i.wrapping_mul(2_654_435_761) % WALK_LEN as u64) as usize)
        .collect();

    let mut resident = WeftWindow::new();
    let mut chaotic = WeftWindow::new();
    let mut resident_out: Vec<Vec<WeftFeature>> = Vec::with_capacity(READS);
    let mut chaotic_out: Vec<Vec<WeftFeature>> = Vec::with_capacity(READS);

    for &idx in &sequence {
        let facet = &walk[idx];
        resident_out.push(
            resident
                .features_at(
                    WeftKind::Spring,
                    facet,
                    fx.geo(),
                    &fx.index,
                    &fx.pack,
                    fx.seed(),
                )
                .to_vec(),
        );
        chaotic_out.push(
            chaotic
                .features_at(
                    WeftKind::Spring,
                    facet,
                    fx.geo(),
                    &fx.index,
                    &fx.pack,
                    fx.seed(),
                )
                .to_vec(),
        );
        chaotic.evict_all(); // evict at EVERY legal opportunity
    }

    assert_eq!(
        resident_out, chaotic_out,
        "an evicted run must be byte-identical to a resident one"
    );

    // Non-vacuity, in two independent ways.
    //
    // 1. The comparison above must not be over a wall of empty `Vec`s: some
    //    read must have found a real feature.
    let occurs_count = resident_out.iter().filter(|v| !v.is_empty()).count();
    assert!(
        occurs_count >= 1,
        "the sampled walk must include at least one real occurrence, or the equality above \
         compares empty vectors; got {occurs_count} of {READS}"
    );
    // 2. Eviction must actually have FORCED recomputation: `chaotic` evicts
    //    before every read, so every one of its `READS` reads is a miss;
    //    `resident` never evicts, so a `READS`-length sequence over only
    //    `WALK_LEN` distinct facets (600 draws over 200 slots, so repeats
    //    are certain by pigeonhole) must produce at least one hit. If it
    //    did not, chaos eviction would be forcing no extra work at all and
    //    this test would denominate nothing.
    assert_eq!(
        chaotic.misses(),
        READS as u64,
        "chaotic evicts before every read, so every read must miss"
    );
    assert!(
        resident.hits() > 0,
        "a {READS}-read sequence over {WALK_LEN} distinct facets must produce at least one \
         repeat, or this walk is too sparse to test eviction against"
    );
    assert!(
        resident.misses() < chaotic.misses(),
        "eviction must force strictly more recomputation than letting the cache hold \
         (resident misses {}, chaotic misses {})",
        resident.misses(),
        chaotic.misses()
    );
}

/// **Fix round 1 (reviewer IMPORTANT).** The two tests above build exactly
/// one world at exactly one globe level, so `Seed` and the level never vary
/// across any read either one performs — a `WeftKey` that silently dropped
/// either component would be invisible to both, which is precisely how the
/// reviewer's `weft_key` mutation (hardcode `Seed(0)` and level `0`) passed
/// them both. This test puts two DIFFERENT seeds through one window at the
/// SAME facet, level and kind, and asserts neither read contaminates the
/// other.
///
/// The two seeds are FOUND, not assumed: `occurs` is a Bernoulli draw
/// against a per-seed noise stream, so scanning candidate seeds against the
/// one real facet is certain to turn up two whose derived features differ
/// well inside the loop bound — asserted explicitly below rather than
/// trusted.
///
/// claim: reachability(seed: 1..200, local witness search) — finds two
/// seeds whose derived features at one fixed facet differ, then uses that
/// pair as the isolation test's fixture; not a claim about seed
/// distribution or world semantics.
#[test]
fn two_seeds_do_not_contaminate_the_same_window() {
    let fx = Fixture::build();
    let facet = fx
        .walk(1)
        .into_iter()
        .next()
        .expect("a walk always has a first facet");

    let mut seed_a: Option<Seed> = None;
    let mut features_a = Vec::new();
    let mut found: Option<(Seed, Vec<WeftFeature>)> = None;
    for base in 1u64..200 {
        let candidate = Seed(base);
        let f = fx.direct_features_with_seed(WeftKind::Spring, &facet, candidate);
        match seed_a {
            None => {
                seed_a = Some(candidate);
                features_a = f;
            }
            Some(_) if f != features_a => {
                found = Some((candidate, f));
                break;
            }
            Some(_) => {}
        }
    }
    let seed_a = seed_a.expect("at least one candidate seed was tried");
    let (seed_b, features_b) = found.expect(
        "scanning 200 candidate seeds must find two whose derived features at this facet \
         differ, or this test cannot discriminate a key that drops Seed",
    );
    assert_ne!(
        features_a, features_b,
        "the two chosen seeds must actually produce different features, or the reads below \
         prove nothing"
    );

    let mut window = WeftWindow::new();
    let read_a = window
        .features_at(
            WeftKind::Spring,
            &facet,
            fx.geo(),
            &fx.index,
            &fx.pack,
            seed_a,
        )
        .to_vec();
    let read_b = window
        .features_at(
            WeftKind::Spring,
            &facet,
            fx.geo(),
            &fx.index,
            &fx.pack,
            seed_b,
        )
        .to_vec();
    // Re-read seed_a AFTER seed_b, through the SAME window: a key that
    // dropped `Seed` would return seed_b's cached entry here instead.
    let reread_a = window
        .features_at(
            WeftKind::Spring,
            &facet,
            fx.geo(),
            &fx.index,
            &fx.pack,
            seed_a,
        )
        .to_vec();

    assert_eq!(
        read_a, features_a,
        "seed_a's window read must match the direct oracle"
    );
    assert_eq!(
        read_b, features_b,
        "seed_b's window read must match the direct oracle"
    );
    assert_eq!(
        reread_a, features_a,
        "re-reading seed_a after seed_b must still return seed_a's own answer"
    );
}

/// **Fix round 1 (reviewer IMPORTANT), the globe-level half.** Same shape as
/// [`two_seeds_do_not_contaminate_the_same_window`], for the level component
/// instead of `Seed`.
///
/// **Built cheaply, not from a second full world.** A second `terrain_of`
/// build would cost this whole file's fixture again just to get a second
/// globe level; a `Geosphere`/`NearestVertexIndex` at a different depth is
/// pure kernel geometry (no tectonic/erosion sculpt) and is cheap to build
/// directly, and a synthetic `FieldPack` (every vertex saturated) answers
/// the same key-completeness question a real second sculpt would, since
/// this test asks only whether the WINDOW keeps two levels' entries apart —
/// not whether the second level's terrain is physically realistic.
#[test]
fn two_globe_levels_do_not_contaminate_the_same_window() {
    let fx = Fixture::build();
    let level_a = fx.geo().depth();
    let level_b = level_a.saturating_sub(1);
    assert_ne!(
        level_a, level_b,
        "need two distinct levels for this test to mean anything (level_a must be >= 1)"
    );

    let geo_b = Geosphere::new(level_b);
    let index_b = NearestVertexIndex::new(&geo_b);
    // Saturated on purpose: real terrain's prevalence sits near a floor
    // (measured elsewhere in this file at [0.00071, 0.04641]); a synthetic
    // pack pushed toward spring/seep's abundance ceiling makes the two
    // levels' answers easy to search apart, not a claim about real geology.
    let pack_b = FieldPack {
        carbonate: VertexMap::from_fn(&geo_b, |_| 1.0),
        induration: VertexMap::from_fn(&geo_b, |_| 1.0),
        drainage: VertexMap::from_fn(&geo_b, |_| 1000.0),
        slope: VertexMap::from_fn(&geo_b, |_| 100_000.0),
        // Land everywhere (Task 7, R1): this test's whole point is telling
        // level_a and level_b's DERIVED features apart, not exercising
        // eligibility — an all-ocean synthetic pack would make every kind
        // ineligible and collapse both sides to the same `Some(0.0)`.
        land: VertexMap::from_fn(&geo_b, |_| 1.0),
    };

    let walk = fx.walk(200);
    let seed = fx.seed();
    let mut chosen: Option<(Facet, Vec<WeftFeature>, Vec<WeftFeature>)> = None;
    for facet in &walk {
        let a = fx.direct_features_with_seed(WeftKind::Spring, facet, seed);
        let b = direct_features_over(WeftKind::Spring, facet, &geo_b, &index_b, &pack_b, seed);
        if a != b {
            chosen = Some((facet.clone(), a, b));
            break;
        }
    }
    let (facet, features_a, features_b) = chosen.expect(
        "scanning 200 walk facets must find one where level_a and level_b's derived features \
         differ, or this test cannot discriminate a key that drops the globe level",
    );
    assert_ne!(
        features_a, features_b,
        "the two levels must actually produce different features at this facet, or the reads \
         below prove nothing"
    );

    let mut window = WeftWindow::new();
    let read_a = window
        .features_at(
            WeftKind::Spring,
            &facet,
            fx.geo(),
            &fx.index,
            &fx.pack,
            seed,
        )
        .to_vec();
    let read_b = window
        .features_at(WeftKind::Spring, &facet, &geo_b, &index_b, &pack_b, seed)
        .to_vec();
    // Re-read level_a AFTER level_b, through the SAME window: a key that
    // dropped the level would return level_b's cached entry here instead.
    let reread_a = window
        .features_at(
            WeftKind::Spring,
            &facet,
            fx.geo(),
            &fx.index,
            &fx.pack,
            seed,
        )
        .to_vec();

    assert_eq!(
        read_a, features_a,
        "level_a's window read must match the direct oracle"
    );
    assert_eq!(
        read_b, features_b,
        "level_b's window read must match the direct oracle"
    );
    assert_eq!(
        reread_a, features_a,
        "re-reading level_a after level_b must still return level_a's own answer"
    );
}
