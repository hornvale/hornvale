//! The Prospect, Task 8, half two: **two readouts of one site must not
//! disagree about where it is.**
//!
//! A placed exotic site is *warranted* at a canonical-grid vertex and
//! *stands* on the facet [`hornvale_worldgen::site_facet_for`] addresses.
//! Those are different places — the placement quad is deliberately wide
//! enough that a plot of every site carries no signature of the ~120 km
//! vertex lattice it came from (`site_facet_for`'s own doc) — and until this
//! task `LocaleContext::strange_site_rows` reported the VERTEX's coordinate
//! while `hornvale_vessel`'s `brief_of` would only let a walker enter the
//! PLACED FACET. So `hornvale locale --strange`, the only prose readout of
//! where a site is, pointed at open ground roughly twenty walk-band rooms
//! from the one room the site is in.
//!
//! # Why the assertion is an equality and not a tolerance
//!
//! Checking each readout independently — "the coordinate is on land", "the
//! facet is enterable" — cannot catch the two diverging again: both arms
//! stay plausible while pointing at different places, which is exactly the
//! state this task found. So the test compares the readout against
//! `site_facet_for`'s own answer for the same `(seed, vertex, reason)` and
//! demands they be the SAME NUMBER. The only slack allowed is the one the
//! emit boundary itself imposes: the row is quantized (decision 0033), so
//! the reference is quantized too, and the comparison is then exact.
//!
//! # Why the second test exists
//!
//! An equality against a reference computed the same way the implementation
//! computes it would pass just as happily if the implementation went back to
//! the vertex AND the reference did too. What makes the first test
//! discriminating is that the vertex's coordinate is a DIFFERENT number:
//! `the_placed_facet_is_not_the_vertex` measures that separation and
//! asserts it is nonzero for every site in the world, which is the same
//! non-vacuity guard `placement.rs`'s own
//! `a_placement_is_not_just_the_vertexs_own_facet` carries one level down.
//!
//! Distances here are in **radians**, never kilometres:
//! `hornvale_locale::room_edge`'s own doc records that there is no length
//! scale anywhere in this project to state a width in, and inventing one
//! would be a defect. The walk-band facet edge is the project-native unit
//! the numbers are reported in.

use hornvale_kernel::{Facet, Seed, Vertex, World, quantize};
use hornvale_locale::{LocaleContext, walk_depth};
use hornvale_worldgen::{SiteReason, site_facet_for};

/// The canonical seed every gallery artifact and calibration battery reads.
const SEED: Seed = Seed(42);

/// Great-circle separation of two unit-sphere positions, radians.
fn separation(u: [f64; 3], v: [f64; 3]) -> f64 {
    let dp: f64 = u[0] * v[0] + u[1] * v[1] + u[2] * v[2];
    hornvale_kernel::math::acos(dp.clamp(-1.0, 1.0))
}

/// The context every test here reads. `World::new` is sufficient: the
/// strangeness budget is derived from climate and terrain, never from the
/// ledger, so no genesis is paid for.
fn context() -> LocaleContext {
    let world = World::new(SEED);
    LocaleContext::build(&world).expect("seed 42 builds a locale context")
}

/// **The agreement.** Every row `--strange` prints carries the coordinate of
/// the facet `site_facet_for` places that site on — the same facet
/// `hornvale_vessel`'s enterability gate tests against — to the last digit
/// the emit boundary keeps.
#[test]
fn the_strange_readout_reports_the_placed_facets_own_centroid() {
    let ctx = context();
    let geo = ctx.climate().geosphere();
    let walk = walk_depth(&ctx);
    let rows = ctx.strange_site_rows();
    assert!(
        !rows.is_empty(),
        "seed 42 must place exotic sites for this test to say anything"
    );

    for row in &rows {
        let placed = site_facet_for(Vertex(row.vertex), SiteReason::Exotic, SEED, geo, walk);
        let coord = placed.coord();
        assert_eq!(
            (row.latitude, row.longitude),
            (quantize(coord.latitude), quantize(coord.longitude)),
            "the readout for the site at vertex {} disagrees with the facet \
             it stands on ({:?})",
            row.vertex,
            placed
        );
    }
}

/// **The non-vacuity guard.** The placed facet's centroid is a different
/// point from the warranting vertex for EVERY site in the world, so the
/// equality above is a real constraint rather than a comparison of one
/// expression with itself. Reports the separation so a reader of a failure
/// (or of the campaign report) has the magnitude and not just the verdict.
#[test]
fn the_placed_facet_is_not_the_vertex() {
    let ctx = context();
    let geo = ctx.climate().geosphere();
    let walk = walk_depth(&ctx);
    let sites = ctx.strange_sites();
    assert!(!sites.is_empty(), "seed 42 must place exotic sites");

    // The shortest edge of one walk-band facet, radians — the project-native
    // unit for "how far away is that", derived from the mesh's own geometry
    // exactly as `hornvale_locale::room_edge` derives it.
    let sample = Facet::containing(geo.position(Vertex(sites[0].vertex)), walk);
    let [a, b, c, d] = sample.corners();
    let edge = separation(a, b)
        .min(separation(b, c))
        .min(separation(c, d))
        .min(separation(d, a));

    let mut moved = 0usize;
    let mut total = 0.0f64;
    let mut worst = 0.0f64;
    for site in &sites {
        let vertex = Vertex(site.vertex);
        let placed = site_facet_for(vertex, SiteReason::Exotic, SEED, geo, walk);
        let gap = separation(geo.position(vertex), placed.centroid());
        total += gap;
        if gap > worst {
            worst = gap;
        }
        // Compared after quantization, because that is the form the readout
        // actually emits: a separation too small to survive the emit
        // boundary would leave the two readouts *reporting* the same place,
        // and this guard is about what a reader can see.
        let vc = geo.coord(vertex);
        let pc = placed.coord();
        if (quantize(vc.latitude), quantize(vc.longitude))
            != (quantize(pc.latitude), quantize(pc.longitude))
        {
            moved += 1;
        }
    }
    println!(
        "exotic placement offset over {} sites: mean {:.9} rad ({:.2} walk-facet edges), \
         max {:.9} rad ({:.2} edges)",
        sites.len(),
        total / sites.len() as f64,
        (total / sites.len() as f64) / edge,
        worst,
        worst / edge
    );
    assert_eq!(
        moved,
        sites.len(),
        "every placed site must report a coordinate distinguishable from its \
         own vertex's, or the agreement test above proves nothing"
    );
}
