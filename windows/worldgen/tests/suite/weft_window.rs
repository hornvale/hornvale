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

use hornvale_kernel::{Facet, NearestVertexIndex, Vertex};
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
        match hornvale_worldgen::prevalence(
            kind,
            facet,
            self.geo(),
            &self.index,
            &self.pack,
            self.seed(),
        ) {
            Some(p) if hornvale_worldgen::occurs(kind, facet, self.seed(), p) => {
                vec![WeftFeature {
                    kind,
                    prevalence: p,
                }]
            }
            _ => Vec::new(),
        }
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
