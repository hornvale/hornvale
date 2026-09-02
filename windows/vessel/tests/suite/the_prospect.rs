//! H1: the rename changes nothing enterable — and, since Task 5, that a site
//! has an ADDRESS rather than a 110 km neighbourhood.

use hornvale_kernel::{Facet, Geosphere, Seed, Vertex};
use hornvale_locale::LocaleContext;
use hornvale_vessel::liveness::LocaleTerrain;
use hornvale_vessel::site::SiteKind;
use hornvale_vessel::{PossessOpts, Session, Turn, brief_of};
use hornvale_worldgen::{SiteReason, site_facet_for};

/// The seed-42 flagship is enterable before and after the gate swap. This is
/// the whole of H1 at this task: a real world, the real verb, the real answer.
///
/// Asserts a POSITIVE signal — the chamber's own `Ways on:` signature — rather
/// than the absence of the old refusal string: the old wording made the
/// negative assertion trivially true, so it could never have caught a broken
/// gate. This one goes red if `enter` stops descending at all.
#[test]
fn the_flagship_is_still_enterable_after_the_gate_swap() {
    let world = hornvale_worldgen::build_world(
        Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    let reply = match s.handle("enter") {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("enter must not release the session: {t}"),
    };
    assert!(
        reply.contains("Ways on"),
        "the flagship must stay enterable across the gate swap: {reply}"
    );
}

/// A site's facet is stable for a seed and distinct between neighbouring
/// vertices — the two properties that make it an ADDRESS rather than a
/// re-rolled guess.
///
/// **The signature is not the plan's.** The plan wrote
/// `site_facet_for(vertex: u32, seed: Seed, walk_depth: u32)`, which cannot be
/// implemented: a vertex index alone says nothing about where on the sphere the
/// vertex is, and every route from a vertex to a facet goes through
/// `Geosphere::position`. It also carries a `reason`, because caves and exotic
/// sites share this mechanism (ledger ruling #13) and a shared key would place
/// both at the identical facet wherever one vertex warrants both.
#[test]
fn a_sites_facet_is_stable_and_vertex_distinct() {
    let geo = Geosphere::new(6);
    let seed = Seed(42);
    let a1 = site_facet_for(Vertex(1953), SiteReason::Exotic, seed, &geo, 13);
    let a2 = site_facet_for(Vertex(1953), SiteReason::Exotic, seed, &geo, 13);
    let b = site_facet_for(Vertex(1954), SiteReason::Exotic, seed, &geo, 13);
    assert_eq!(
        a1, a2,
        "the same vertex must always place at the same facet"
    );
    assert_ne!(a1, b, "neighbouring vertices must not share a facet");
}

/// The point of the whole task: a placed exotic site is at ONE facet, and the
/// facet beside it holds nothing.
///
/// This is what separates an address from the defect
/// `CLIM-water-label-resolution-vs-walk-band` records — a per-vertex verdict
/// read at the walk band answers "yes" for every facet within ~55 km, which is
/// the same sentence as "no site anywhere". The neighbour assertion is the half
/// that would catch a regression to that shape; the positive assertion alone
/// would pass under it.
#[test]
fn an_exotic_site_stands_at_one_facet_and_not_at_its_neighbour() {
    let world = hornvale_worldgen::build_world(
        Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let sites = ctx.strange_sites();
    assert!(
        !sites.is_empty(),
        "seed 42 must place at least one exotic site"
    );
    let geo = ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&ctx);
    let terrain = LocaleTerrain::new(&ctx);
    let placed = site_facet_for(
        Vertex(sites[0].vertex),
        SiteReason::Exotic,
        world.seed,
        geo,
        walk,
    );

    let here = brief_of(
        &world,
        geo,
        ctx.nearest_index(),
        &placed,
        &terrain,
        walk,
        &sites,
    );
    assert_eq!(
        // `site`, not `s`: `cli/tests/suite/claim_shape.rs` reads a closure
        // parameter named `s` as a seed binding and demands a `claim:` tag on
        // what it takes for a seed sweep. This test builds ONE world, so the
        // tag would be a false declaration; the honest fix is the name, which
        // is more accurate anyway.
        here.site.as_ref().map(|site| site.kind),
        Some(SiteKind::Exotic),
        "the placed facet must carry the site"
    );

    let next: Facet = placed.neighbors()[0].clone();
    let there = brief_of(
        &world,
        geo,
        ctx.nearest_index(),
        &next,
        &terrain,
        walk,
        &sites,
    );
    assert_eq!(
        there.site, None,
        "the facet beside a site must hold nothing — a site is an address, \
         not a neighbourhood"
    );
}
