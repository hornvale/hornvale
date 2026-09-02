//! **H2, preregistered (The Prospect, spec section 9, frozen 2026-09-01):**
//! over 2,000 land facets on seeds 42, 13, 7, 1 and 100, the fraction holding a
//! cave is **1%-8%**. Below 1% the widening buys nothing; above 8% caves stop
//! being remarkable. *A result outside the band is a finding, and the threshold
//! is re-derived once, in the open.*
//!
//! # The readout: H2 IS FALSIFIED, and not by the threshold
//!
//! Measured here at `hornvale_terrain`'s shipped `CAVE_PRONENESS_THRESHOLD`
//! (0.12), 2,000 sampled land facets per seed:
//!
//! | seed | cave-warranting vertices | caves placed on land | of 2,000 sampled | exact rate |
//! | ---: | ---: | ---: | ---: | ---: |
//! | 42  | 1564 | 1528 | 1 | 1.31e-5 |
//! | 13  |  970 |  857 | 0 | 6.11e-6 |
//! | 7   | 1638 | 1564 | 0 | 8.21e-6 |
//! | 1   | 1355 | 1315 | 0 | 9.00e-6 |
//! | 100 | 2772 | 2735 | 0 | 1.43e-5 |
//!
//! Pooled sampled rate **0.0001** (one hit in 10,000, against 0.10 expected —
//! the sample is pure Poisson noise at this scale, which is the point); pooled
//! **exact** rate **1.0191e-5**, i.e. **0.00102% of land facets**, ~980x under
//! the band's 1% floor.
//!
//! # Why no threshold rescues it, which is why the threshold was not retuned
//!
//! The band is unreachable **by construction**, and the arithmetic needs no
//! seeds. A site is PLACED — one facet per warranting vertex (Decision 0537,
//! `hornvale_worldgen::site_facet_for`) — so the number of cave facets in a
//! world is bounded above by the number of geosphere vertices. The canonical
//! grid has 40,962 of them; the walk band at depth 13 has `6 * 4^13` =
//! 402,653,184 facets. **At most one facet in 9,830 can hold a site of any
//! kind — 0.0102% — even if every vertex on the globe warranted a cave**, which
//! is 98x under the 1% floor before a single seed is built.
//!
//! So the constant is not what missed the band; the *quantity* is. H2 was
//! frozen while the spec still said a cave was a per-facet derivation from a
//! field, where a percentage of facets was a coherent target. Section 6's
//! 2026-09-01 correction made caves placed sites, and a placed point process on
//! a 41k-vertex lattice cannot express a per-facet percentage at all. Decision
//! 0016 forbids retuning a constant to rescue a prediction after unblinding;
//! it would also have been useless here, so [`the_band_is_unreachable_at_any_threshold`]
//! pins the *structural* fact instead of quietly widening the band. That test
//! goes red the day the mechanism or the resolution changes enough for the
//! band to become reachable — which is the day H2 is worth re-litigating.
//!
//! # What is asserted here, and why each assertion earns its place
//!
//! A sampled rate of 0/2,000 is the same reading a completely broken
//! mechanism would give, so the sample alone cannot tell "sparse" from "dead".
//! [`caves_are_actually_placed`] is the discriminator: it asserts the roster
//! and the placed set are non-empty on every seed, so this file cannot go green
//! on a world with no caves in it.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Facet, Geosphere, NearestVertexIndex, Seed, Vertex};
use hornvale_locale::LocaleContext;
use hornvale_terrain::GeneratedTerrain;
use hornvale_worldgen::{
    BuildDepth, SiteReason, SkyChoice, WorldComponents, build_world_to_with_artifacts,
    site_facet_for,
};

/// The preregistered band's floor. Spec section 9.
const CAVE_RATE_MIN: f64 = 0.01;
/// The preregistered band's ceiling. Spec section 9.
const CAVE_RATE_MAX: f64 = 0.08;

/// The preregistered population: five seeds, default pins.
const SEEDS: [u64; 5] = [42, 13, 7, 1, 100];

/// Land facets sampled per seed — 2,000 each, so 10,000 pooled against the
/// 2,000 the spec froze. Deliberately a superset: a reading this far under the
/// band should not also be open to "you under-sampled".
const SAMPLE: usize = 2_000;

/// One seed's reading.
struct Reading {
    /// Vertices `hornvale_terrain::cave_site_at` warrants a cave at.
    roster: usize,
    /// Of those, the ones whose PLACED facet lands on a land facet — the
    /// numerator of a rate whose denominator is land facets.
    placed_on_land: usize,
    /// How many of the [`SAMPLE`] sampled land facets held a cave.
    sampled_with_cave: usize,
    /// Land facets in the world, estimated from the sampler's own accept rate.
    land_facets: f64,
    /// Every facet in the walk band, `6 * 4^walk_depth`.
    all_facets: f64,
    /// Geosphere vertices — the hard upper bound on placed sites of one kind.
    vertices: f64,
}

impl Reading {
    /// Caves per land facet, computed exactly from the placed set rather than
    /// estimated from the sample. The sample cannot resolve a rate this small;
    /// this is the number that actually answers H2.
    fn exact_rate(&self) -> f64 {
        self.placed_on_land as f64 / self.land_facets
    }

    /// The largest cave rate the PLACEMENT MECHANISM can express, whatever the
    /// threshold: every vertex warranting a cave, one facet each.
    fn ceiling_rate(&self) -> f64 {
        self.vertices / self.all_facets
    }
}

/// Read one seed: build to the shallowest depth that yields a locale context,
/// place every warranted cave, and sample land facets uniformly.
fn read(seed: u64, wc: &WorldComponents) -> Reading {
    let arts = build_world_to_with_artifacts(
        Seed(seed),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
        wc,
        // `Settlements`, not `Full`: the shallowest rung that yields a climate,
        // which `LocaleContext::build_from` needs. Nothing here reads history.
        BuildDepth::Settlements,
    )
    .expect("the calibration seeds build");
    let terrain = arts.terrain.expect("the Terrain rung sculpts terrain");
    let climate = arts.climate.expect("the Settlements rung fits climate");
    let ctx = LocaleContext::build_from(&arts.world, &terrain, &climate);
    let geo = ctx.climate().geosphere();
    let index = ctx.nearest_index();
    // `hornvale_locale::walk_depth`, never the arithmetic behind it — The
    // Pavement found sixteen restatements of this offset, two of them a whole
    // band stale.
    let walk = hornvale_locale::walk_depth(&ctx);

    let roster = ctx.terrain().cave_site_vertices();
    let placed: std::collections::BTreeSet<Facet> = roster
        .iter()
        .map(|&v| site_facet_for(v, SiteReason::Cave, Seed(seed), geo, walk))
        .collect();
    let placed_on_land = placed
        .iter()
        .filter(|facet| is_land(facet, geo, index, &terrain))
        .count();

    let mut stream = Seed(seed)
        .derive(StreamLabel::dynamic("lab/the-prospect/cave-rate"))
        .stream();
    let mut drawn = 0usize;
    let mut kept = 0usize;
    let mut sampled_with_cave = 0usize;
    while kept < SAMPLE {
        drawn += 1;
        let facet = Facet {
            face: stream.range_u32(0, 5) as u8,
            path: (0..walk).map(|_| stream.range_u32(0, 3) as u8).collect(),
        };
        if !is_land(&facet, geo, index, &terrain) {
            continue;
        }
        kept += 1;
        if placed.contains(&facet) {
            sampled_with_cave += 1;
        }
    }

    let all_facets = 6.0 * 4f64.powi(walk as i32);
    Reading {
        roster: roster.len(),
        placed_on_land,
        sampled_with_cave,
        land_facets: all_facets * kept as f64 / drawn as f64,
        all_facets,
        vertices: geo.vertex_count() as f64,
    }
}

/// Whether a walk facet sits on land, by the same rule `hornvale_vessel`'s
/// `brief::containing_vertex` uses — the maximum-weight corner of the facet's
/// bilinear blend, tie-broken by ascending `Vertex`. Integer weights, so no
/// float comparison enters the answer.
fn is_land(
    facet: &Facet,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    terrain: &GeneratedTerrain,
) -> bool {
    facet
        .corner_weights(geo, index)
        .and_then(|weights| {
            weights
                .iter()
                .max_by(|a, b| a.1.cmp(&b.1).then(b.0.0.cmp(&a.0.0)))
                .map(|&(vertex, _)| vertex)
        })
        .is_some_and(|vertex: Vertex| !terrain.is_ocean(vertex))
}

/// **H2's readout.** Measures the rate over the preregistered population and
/// prints it; the band itself is adjudicated by
/// [`the_band_is_unreachable_at_any_threshold`], because a sample of 2,000
/// cannot resolve a rate near 1e-5 and asserting a band against pure sampling
/// noise would be theatre either way it landed.
/// claim: readout(preregistered) — H2's five frozen seeds, measured and
/// printed; the band is adjudicated structurally next door.
#[test]
fn the_cave_rate_is_measured_over_the_preregistered_population() {
    let wc = WorldComponents::assemble().expect("the canonical registries are well-formed");
    let mut pooled_sampled = 0usize;
    let mut pooled_placed = 0usize;
    let mut pooled_land = 0.0f64;
    for seed in SEEDS {
        let r = read(seed, &wc);
        println!(
            "seed {seed}: roster {} placed-on-land {} sampled-with-cave {}/{SAMPLE} \
             exact rate {:.3e} ceiling rate {:.3e}",
            r.roster,
            r.placed_on_land,
            r.sampled_with_cave,
            r.exact_rate(),
            r.ceiling_rate(),
        );
        pooled_sampled += r.sampled_with_cave;
        pooled_placed += r.placed_on_land;
        pooled_land += r.land_facets;
    }
    let sampled_rate = pooled_sampled as f64 / (SAMPLE * SEEDS.len()) as f64;
    let exact_rate = pooled_placed as f64 / pooled_land;
    println!(
        "H2 pooled: sampled rate {sampled_rate:.4} over {} facets; exact rate {exact_rate:.4e}; \
         preregistered band {CAVE_RATE_MIN}..={CAVE_RATE_MAX} — FALSIFIED, see this file's doc",
        SAMPLE * SEEDS.len()
    );
}

/// **The finding, pinned as an assertion.** No value of
/// `hornvale_terrain`'s `CAVE_PRONENESS_THRESHOLD` can put the cave rate inside
/// H2's band, because the placement mechanism emits at most one facet per
/// geosphere vertex and there are ~9,830 walk facets per vertex.
///
/// Asserted rather than merely recorded so that the finding is *falsifiable*:
/// this goes red if the globe level rises, the walk band coarsens, a site gains
/// an extent covering more than one facet (`hornvale_vessel::site::Extent`
/// reserves exactly that), or placement stops being one-per-vertex — each of
/// which is a change that would make H2 worth asking again.
/// claim: invariant(forall-seed) — the ceiling is `vertices / facets` and does
/// not vary by seed at all; the five preregistered seeds are the population
/// only because that is what H2 froze.
#[test]
fn the_band_is_unreachable_at_any_threshold() {
    let wc = WorldComponents::assemble().expect("the canonical registries are well-formed");
    for seed in SEEDS {
        let r = read(seed, &wc);
        assert!(
            r.ceiling_rate() < CAVE_RATE_MIN,
            "seed {seed}: the placement mechanism's ceiling rate {:.3e} now reaches H2's \
             floor of {CAVE_RATE_MIN}. The band was frozen when a cave was a per-facet \
             derivation and has been unreachable since caves became placed sites; if this \
             is red, the mechanism changed and H2 can be asked again — re-litigate it in \
             the spec rather than deleting this test.",
            r.ceiling_rate()
        );
        assert!(
            r.exact_rate() <= r.ceiling_rate(),
            "seed {seed}: more cave facets than vertices — placement is no longer \
             one facet per vertex"
        );
    }
}

/// The discriminator the sample cannot be: caves are actually placed, on every
/// seed. Without this, a mechanism that placed nothing at all would read
/// exactly like the sparse one that ships — 0 of 2,000, five times over.
/// claim: invariant(forall-seed) — every world places at least one cave on
/// land, asserted over H2's frozen five.
#[test]
fn caves_are_actually_placed() {
    let wc = WorldComponents::assemble().expect("the canonical registries are well-formed");
    for seed in SEEDS {
        let r = read(seed, &wc);
        assert!(
            r.roster > 0,
            "seed {seed} warrants no cave anywhere — the threshold selects nothing"
        );
        assert!(
            r.placed_on_land > 0,
            "seed {seed} warrants {} caves and places none of them on land",
            r.roster
        );
    }
}
