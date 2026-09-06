//! The Warp, Task 3: the room sentence names the rock underfoot and how the
//! ground tilts — the two signs spring and overhang are diagnostic of —
//! through the one implementation `hornvale_worldgen::warp` holds.

use hornvale_kernel::Facet;
use hornvale_locale::{LocaleContext, dominant_corner};
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{rock_word, steepness_sign, steepness_word};

struct Oracle {
    world: hornvale_kernel::World,
    ctx: LocaleContext,
    pack: hornvale_worldgen::FieldPack,
}

impl Oracle {
    #[allow(clippy::disallowed_methods)]
    fn build() -> Self {
        let world = hornvale_worldgen::seed_42_world();
        let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
        let climate =
            hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
        let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
        let ctx = LocaleContext::build_from(&world, &terrain, &climate);
        Self { world, ctx, pack }
    }
    /// The expected clause, re-derived independently of the session.
    fn expected_clause(&self, position: &Facet) -> String {
        let geo = self.ctx.climate().geosphere();
        let weights = position
            .corner_weights(geo, self.ctx.nearest_index())
            .expect("walk band is below the globe level");
        let rock = self.ctx.terrain().rock_at(dominant_corner(&weights).0);
        let slope = hornvale_kernel::blend_corner_weights(weights, &self.pack.slope);
        hornvale_vessel::warp_prose::warp_clause(rock, steepness_sign(slope))
    }
    /// Whether `facet` renders AFLOAT — the same predicate the render gates
    /// the warp clause on (`vantage.is_none()`, the water-column test),
    /// copied from `the_weft.rs`'s own `Oracle::is_afloat`, adapted to
    /// `hornvale_locale::dominant_corner` — the shared tie-break `windows/
    /// locale` now exposes for exactly this "categorical corner" purpose.
    fn is_afloat(&self, facet: &Facet) -> bool {
        let geo = self.ctx.climate().geosphere();
        let Some(weights) = facet.corner_weights(geo, self.ctx.nearest_index()) else {
            return false;
        };
        let dominant = dominant_corner(&weights).0;
        !self.ctx.water_column_at(dominant).is_empty()
    }
}

const COMPASS: [&str; 8] = ["n", "ne", "e", "se", "s", "sw", "w", "nw"];

#[test]
fn the_room_sentence_names_the_rock_and_the_pitch_the_oracle_derives() {
    let oracle = Oracle::build();
    let (mut s, _) =
        Session::start(&oracle.world, &PossessOpts::default()).expect("seed 42 possesses");
    let mut checked = 0;
    for step in 0..40 {
        let pos = s.position();
        if !oracle.is_afloat(&pos) {
            let expected = oracle.expected_clause(&pos);
            assert!(
                !expected.is_empty(),
                "fixture check: a land facet renders a non-empty warp clause"
            );
            let reply = match s.handle("look") {
                Turn::Out(t) => t,
                Turn::Released(t) => panic!("look released: {t}"),
            };
            assert!(
                reply.contains(expected.trim()),
                "step {step}: expected {expected:?} in {reply:?}"
            );
            checked += 1;
        }
        let word = COMPASS[step % 8];
        let _ = s.handle(&format!("go {word}"));
    }
    assert!(
        checked >= 10,
        "fixture check: only {checked} land rooms were checked in 40 steps"
    );
}

#[test]
fn afloat_rooms_render_no_warp_clause() {
    // Every walked room where the vantage is over water must not name a rock —
    // a walker does not stand on the sea floor. Find one by walking a
    // straight bearing (never `COMPASS[step % 8]`-style zigzag: an 8-step
    // cycle through all eight compass words sums to zero net displacement,
    // so it orbits the start facet forever and — measured directly — never
    // reaches water within 200 steps of seed 42's start). "e" was measured
    // to reach an afloat facet at step 18; the Weft's F5 separately measured
    // 29,713 afloat facets on this grid, so the destination is common, only
    // the zigzag path to it was not.
    let oracle = Oracle::build();
    let (mut s, _) =
        Session::start(&oracle.world, &PossessOpts::default()).expect("seed 42 possesses");
    let mut found = false;
    for step in 0..200 {
        let pos = s.position();
        if oracle.is_afloat(&pos) {
            let reply = match s.handle("look") {
                Turn::Out(t) => t,
                Turn::Released(t) => panic!("{t}"),
            };
            assert!(
                !reply.contains("Underfoot,"),
                "afloat at step {step} yet the sentence names a rock: {reply:?}"
            );
            found = true;
            break;
        }
        let _ = s.handle("go e");
    }
    assert!(
        found,
        "fixture check: the walk never reached an afloat facet, so the assertion above never ran"
    );
}

#[test]
fn every_steepness_word_appears_somewhere_on_seed_42_land() {
    // Anti-vacuity for the steepness sign: a three-valued word whose middle or
    // top value never occurs is a two-valued word wearing three names.
    let oracle = Oracle::build();
    let geo = oracle.ctx.climate().geosphere();
    let depth = geo.depth() + 7;
    let mut seen = std::collections::BTreeSet::new();
    for v in (0..geo.vertex_count()).step_by(7) {
        let facet = Facet::containing(geo.position(hornvale_kernel::Vertex(v as u32)), depth);
        let Some(weights) = facet.corner_weights(geo, oracle.ctx.nearest_index()) else {
            continue;
        };
        if oracle.is_afloat(&facet) {
            continue;
        }
        let slope = hornvale_kernel::blend_corner_weights(weights, &oracle.pack.slope);
        seen.insert(steepness_word(steepness_sign(slope)));
    }
    assert_eq!(
        seen.len(),
        3,
        "steepness words seen on seed 42 land: {seen:?}"
    );
    let _ = rock_word; // the rock word's coverage is a property of the lithology, asserted in warp_signs.rs
}
