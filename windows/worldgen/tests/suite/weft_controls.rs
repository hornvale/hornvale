//! The Warp, Task 4: thicket and erratic are this campaign's non-regression
//! controls. The Warp re-parameterises the two **sign kinds** (spring and
//! overhang) — their prevalence becomes `rate · smoothstep(cause; lo, hi) +
//! floor · noise` — and leaves the other two kinds' derivation untouched,
//! character for character (spec §6.1).
//!
//! "Untouched" is a claim about BITS, not about intent. `a · (c · m + (1 −
//! c) · n)` and `(a · c) · m + (a · (1 − c)) · n` are the same number in
//! real arithmetic and can differ in the last unit in floating point, and
//! an occurrence is a comparison of a second noise sample against exactly
//! that value — so a last-unit move is a feature that appears or vanishes,
//! not a rounding curiosity. This file pins thicket's and erratic's
//! prevalence over the seed-42 grid as a committed golden.
//!
//! **The golden was recorded on the UNCHANGED tree, before any recipe code
//! moved** (Task 4, step 1; committed alone in its own commit). A control
//! recorded after the change it exists to witness witnesses nothing.
//!
//! It is a kernel golden ([`hornvale_kernel::golden::assert_golden`]), not
//! a path declared in `docs/generated-paths.txt`: that file's own comment
//! forbids listing byte-goldens under `windows/worldgen/tests/fixtures/`,
//! because the artifacts phase would silently rebaseline them. Accepting a
//! drift here is therefore an explicit `REBASELINE=1` run by a human who
//! has decided the controls may move — which, for the life of this
//! campaign, they may not.
//!
//! Test fixture (decision 0092): calls the sculpt derivation entry point
//! (`terrain_of`) directly to build its own world state, the sanctioned
//! test-fixture posture.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{Facet, NearestVertexIndex, Vertex, blend_corner_weights};
use hornvale_worldgen::WeftKind;

/// Walk depth: the globe level plus 7, the same relationship
/// `windows/locale::walk_depth` documents and `weft_prevalence.rs`
/// reproduces (this crate's test cannot import `windows/locale`, which
/// depends on the crate under test).
const WALK_DEPTH_BELOW_GRID: u32 = 7;

/// The two control kinds, in the order the golden records them.
const CONTROLS: [WeftKind; 2] = [WeftKind::Thicket, WeftKind::Erratic];

/// Thicket's and erratic's prevalence over every land-eligible walk-depth
/// facet of the seed-42 grid, in vertex order, as
/// `{vertex}\t{kind:?}\t{bits:016x}` — the raw `f64` bit pattern, so the
/// comparison is bit-for-bit and not a formatted approximation of one.
fn render_control_prevalences() -> String {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;

    let mut lines = Vec::new();
    for v in 0..geo.vertex_count() {
        let facet = Facet::containing(geo.position(Vertex(v as u32)), walk_depth);
        let Some(weights) = facet.corner_weights(geo, &index) else {
            continue;
        };
        if blend_corner_weights(weights, &pack.land) < 0.5 {
            continue;
        }
        for kind in CONTROLS {
            let p = hornvale_worldgen::prevalence_with_weights(
                kind, &facet, weights, &pack, world.seed,
            );
            lines.push(format!("{v}\t{kind:?}\t{:016x}", p.to_bits()));
        }
    }
    lines.push(String::new());
    lines.join("\n")
}

/// The controls' bit-for-bit witness. Fails the moment thicket's or
/// erratic's prevalence moves by a single unit in the last place anywhere
/// on seed 42's land — which is what "The Warp changed only the sign kinds"
/// has to mean to be worth asserting.
#[test]
fn thicket_and_erratic_prevalence_is_the_wefts_expression_bit_for_bit() {
    let text = render_control_prevalences();

    // The fixture check, derived from the rendered text itself rather than
    // a parallel counter: a rendering bug that emitted nothing would
    // otherwise leave a golden of one empty line comparing equal forever.
    let checked = text.lines().filter(|l| !l.is_empty()).count();
    assert!(
        checked > 20_000,
        "fixture check: {checked} control readings over seed 42's land-eligible grid — \
         the claim needs a population"
    );

    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/weft-controls-seed-42.txt"
        )),
        &text,
        "thicket and erratic prevalence bits over seed 42's land-eligible grid — The Warp's \
         non-regression controls",
    );
}
