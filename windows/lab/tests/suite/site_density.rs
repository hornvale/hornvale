//! **H3, preregistered (The Prospect, spec section 9, frozen 2026-09-01):**
//! over the same sample as H2 — 2,000 land facets on seeds 42, 13, 7, 1 and
//! 100 — the fraction of land facets holding **at least one site of any kind**.
//! **No predicted value.** This is the number the follow-on campaign needs and
//! nobody had it; recording it is the success criterion, whatever it is.
//!
//! # The readout: the density baseline
//!
//! Site facets are counted over the whole globe; the rate's denominator is the
//! land-facet count estimated from the sampler's own accept rate.
//!
//! | seed | settlement | exotic | cave | any (union) | on land | on water | of 2,000 sampled | land facets | **exact rate** |
//! | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
//! | 42  | 389 | 103 |  874 | 1366 | 1297 |  69 | 1 | 1.1932e8 | 1.0870e-5 |
//! | 13  | 259 | 143 | 1647 | 2049 | 1892 | 157 | 0 | 1.3587e8 | 1.3925e-5 |
//! | 7   | 250 | 180 | 1681 | 2111 | 2000 | 111 | 0 | 1.9471e8 | 1.0272e-5 |
//! | 1   | 301 | 136 | 1116 | 1553 | 1455 |  98 | 0 | 1.4345e8 | 1.0143e-5 |
//! | 100 |  60 | 175 | 2440 | 2675 | 2585 |  90 | 0 | 1.8382e8 | 1.4063e-5 |
//!
//! **Pooled exact rate `1.1875e-5` — 0.001188% of land facets, one site per
//! ~84,200 land facets.** That is H3, and it is the campaign's deliverable.
//! Pooled sampled rate `1.0e-4` (one hit in 10,000).
//!
//! # What "a site of any kind" means here
//!
//! Exactly what makes `hornvale_vessel::brief::Brief::site` `Some` — the
//! enterability gate (decision 0536) — so H3 is literally "how often is there
//! something to walk into". `brief_of` assembles three candidates and takes
//! the most salient, and this file reconstructs the same three sets over the
//! whole globe rather than one facet at a time:
//!
//! - **Settlement** — `hornvale_vessel::liveness::built_rooms`, the same map
//!   `Terrain::is_built` is injected with. One room per settlement today
//!   (`settlement_room`), *not* a radius of countryside.
//! - **Exotic** — `LocaleContext::strange_sites`, placed by
//!   `hornvale_worldgen::site_facet_for` under `SiteReason::Exotic`.
//! - **Cave** — `GeneratedTerrain::cave_site_vertices`, placed by the same
//!   function under `SiteReason::Cave`.
//!
//! The union is taken over FACETS, not over vertices, so a facet holding two
//! kinds counts once — which is the quantity `Brief::site` actually exposes,
//! since `max_by_key(Site::salience)` returns exactly one site per facet.
//!
//! **On these five seeds the union never collapses anything**: `any` equals
//! `settlement + exotic + cave` exactly, five times over, so no facet in any
//! of these worlds holds two kinds at once. `Site::salience`'s ordering is
//! therefore load-bearing in `brief_of` and unexercised by any generated
//! world — a tie-break with no live case. That is a consequence of the density
//! measured above, not a separate fact: at 1.2e-5 coverage, two independent
//! placements colliding on one facet of 4.0e8 is a rare event.
//!
//! # The sample cannot resolve this rate, and the one hit shows how badly
//!
//! Seed 42 returned **one** hit in 2,000 and the other four returned none. The
//! expected count at seed 42's own measured rate is 0.022 hits per 2,000, so a hit
//! anywhere in the pooled 10,000 is an ordinary ~11%-probability outcome — and
//! it drags the pooled *sampled* estimate to `1.0e-4`, **8.4x the true rate**.
//! An earlier draft of this file predicted the sampled column would be zero
//! on all five seeds; it was not, and the correction sharpens the point rather
//! than weakening it. A sampled column that is 0 four times and 8.4x-high once
//! is a clearer demonstration that 10,000 draws is the wrong instrument for a
//! rate near 1e-5 than an all-zero column would have been.
//!
//! Stated plainly, because the brief for this task asks for it: the assertion
//! in [`the_site_density_is_measured_over_the_preregistered_population`] that
//! the rate is a real number in `[0, 1]` is deliberately weak, and is **not**
//! the load-bearing one. H3 froze no predicted value, so there is no band to
//! adjudicate and none is invented here.
//! [`every_kind_of_site_is_actually_placed`] is the discriminator: it fails if
//! any of the three rosters is empty, or places nothing on land, on any seed —
//! the failure a near-zero sample would otherwise hide, since a world that
//! placed nothing at all would read almost exactly like the sparse one that
//! ships.
//!
//! # Why the number is small, and what it aims at
//!
//! It is bounded in advance and the arithmetic needs no seeds. Every site this
//! campaign emits is PLACED — at most one facet per warranting vertex
//! (decision 0537) — so the whole placed set is bounded by 2 * 40,962 geosphere
//! vertices (a cave roster and an exotic roster) plus one facet per settlement,
//! against `6 * 4^13` = 402,653,184 walk facets. The measured ceiling is
//! `2.04e-4`: **under 0.021% of the walk band can hold a site of any kind even
//! if every vertex on the globe held both kinds.** The worlds that ship sit at
//! **5.8% of that ceiling**, so raising every predicate to "always" would buy
//! ~17x and leave the rate at 2e-4.
//!
//! Nathan's goal for the world is "every square mile, something interesting".
//! A walk facet is a ~1.7 km locale (`hornvale_vessel::depth`), so "every
//! square mile" is very nearly "every facet" — a rate of 1. The measured rate
//! is **~84,200x short of that, and the ceiling is still ~4,900x short**. The
//! shortfall is therefore the placement MECHANISM's resolution and not any
//! constant, and no threshold, predicate or budget moves it:
//! `hornvale_vessel::site::Tier::Derived` — modelled, unused, decision 0539 —
//! is the axis that can. H3's job was to size the gap, and the gap is ~4.9
//! orders of magnitude at the measurement and ~3.7 at the ceiling.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Facet, Seed, Vertex};
use hornvale_locale::LocaleContext;
use hornvale_vessel::liveness::built_rooms;
use hornvale_worldgen::{
    BuildDepth, SiteReason, SkyChoice, WorldComponents, build_world_to_with_artifacts,
    site_facet_for,
};
use std::collections::BTreeSet;

use crate::cave_rate_calibration::is_land;

/// The preregistered population: five seeds, default pins. The same five H2
/// froze — H3 is defined as being measured over "the same sample".
const SEEDS: [u64; 5] = [42, 13, 7, 1, 100];

/// Land facets sampled per seed — 2,000 each, so 10,000 pooled, matching H2's
/// sample exactly so the two readouts share a denominator and H3's numerator
/// is a strict superset of H2's.
const SAMPLE: usize = 2_000;

/// One seed's reading.
struct Reading {
    /// Facets holding a settlement (`built_rooms`, one room per settlement).
    settlement: usize,
    /// Facets holding a placed exotic site.
    exotic: usize,
    /// Facets holding a placed cave.
    cave: usize,
    /// Distinct facets holding at least one site of any kind — the union, so a
    /// facet holding two kinds counts once, exactly as `Brief::site` does.
    any: usize,
    /// Of [`Reading::any`], those that sit on a LAND facet — H3's numerator,
    /// since the denominator is land facets.
    any_on_land: usize,
    /// Of [`Reading::any`], those that sit on WATER. Not a defect: a cave
    /// mouth opening under water is intended (see H2's own note), and this
    /// column is how anyone sizes what is waiting on water traversal.
    any_on_water: usize,
    /// How many of the [`SAMPLE`] sampled land facets held a site of any kind.
    sampled_with_site: usize,
    /// Land facets in the world, estimated from the sampler's own accept rate.
    land_facets: f64,
    /// Every facet in the walk band, `6 * 4^walk_depth`.
    all_facets: f64,
    /// Geosphere vertices — the bound on placed sites of ONE kind.
    vertices: f64,
}

impl Reading {
    /// Sites per land facet, computed exactly from the placed union rather
    /// than estimated from the sample. **This is H3.**
    fn exact_rate(&self) -> f64 {
        self.any_on_land as f64 / self.land_facets
    }

    /// The largest any-kind rate the PLACEMENT MECHANISM can express, whatever
    /// any predicate says: every vertex warranting both a cave and an exotic
    /// site, one facet each, plus one facet per settlement. An over-estimate by
    /// construction (the two kinds' facets can coincide), which is what makes
    /// it a ceiling.
    fn ceiling_rate(&self) -> f64 {
        (2.0 * self.vertices + self.settlement as f64) / self.all_facets
    }
}

/// Read one seed: build to the shallowest depth that yields a locale context,
/// place every site of every kind, and sample land facets uniformly.
fn read(seed: u64, wc: &WorldComponents) -> Reading {
    let arts = build_world_to_with_artifacts(
        Seed(seed),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
        wc,
        // `Settlements`, not `Full`: the shallowest rung that yields both a
        // climate (which `LocaleContext::build_from` needs) and the settlement
        // roster `built_rooms` reads. Nothing here reads history.
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

    // The three candidate sets `brief_of` assembles, reconstructed globally.
    // `built_rooms` keys are packed at the SAME walk depth (`settlement_room`
    // calls `Facet::containing(pos, walk_depth(ctx))`), so unpacking them is
    // exact rather than a re-derivation.
    let settlement: BTreeSet<Facet> = built_rooms(&arts.world, &ctx)
        .keys()
        .filter_map(|id| id.unpack().ok())
        .collect();
    let exotic: BTreeSet<Facet> = ctx
        .strange_sites()
        .iter()
        .map(|s| site_facet_for(Vertex(s.vertex), SiteReason::Exotic, Seed(seed), geo, walk))
        .collect();
    let cave: BTreeSet<Facet> = ctx
        .terrain()
        .cave_site_vertices()
        .iter()
        .map(|&v| site_facet_for(v, SiteReason::Cave, Seed(seed), geo, walk))
        .collect();
    let any: BTreeSet<Facet> = settlement
        .iter()
        .chain(exotic.iter())
        .chain(cave.iter())
        .cloned()
        .collect();
    let any_on_land = any
        .iter()
        .filter(|facet| is_land(facet, geo, index, &terrain))
        .count();

    let mut stream = Seed(seed)
        .derive(StreamLabel::dynamic("lab/the-prospect/site-density"))
        .stream();
    let mut drawn = 0usize;
    let mut kept = 0usize;
    let mut sampled_with_site = 0usize;
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
        if any.contains(&facet) {
            sampled_with_site += 1;
        }
    }

    let all_facets = 6.0 * 4f64.powi(walk as i32);
    Reading {
        settlement: settlement.len(),
        exotic: exotic.len(),
        cave: cave.len(),
        any: any.len(),
        any_on_land,
        any_on_water: any.len() - any_on_land,
        sampled_with_site,
        land_facets: all_facets * kept as f64 / drawn as f64,
        all_facets,
        vertices: geo.vertex_count() as f64,
    }
}

/// **H3's readout — the campaign's deliverable.** Measures the any-kind site
/// rate over the preregistered population and prints it. H3 froze **no
/// predicted value**, so there is no band to adjudicate and none is invented
/// here; the table in this file's module doc is the artifact.
/// claim: readout(preregistered) — H3's five frozen seeds, measured and
/// printed, with no predicted value to compare against.
#[test]
fn the_site_density_is_measured_over_the_preregistered_population() {
    let wc = WorldComponents::assemble().expect("the canonical registries are well-formed");
    let mut pooled_sampled = 0usize;
    let mut pooled_placed = 0usize;
    let mut pooled_land = 0.0f64;
    for seed in SEEDS {
        let r = read(seed, &wc);
        println!(
            "seed {seed}: settlement {} exotic {} cave {} any {} on-land {} on-water {} \
             sampled-with-site {}/{SAMPLE} land facets {:.4e} exact rate {:.4e} \
             ceiling rate {:.4e}",
            r.settlement,
            r.exotic,
            r.cave,
            r.any,
            r.any_on_land,
            r.any_on_water,
            r.sampled_with_site,
            r.land_facets,
            r.exact_rate(),
            r.ceiling_rate(),
        );
        // H3 has no predicted value; the only thing to assert about the
        // measurement itself is that it produced a real number in [0, 1]. This
        // is deliberately weak — see this file's module doc, and
        // `every_kind_of_site_is_actually_placed` for the assertion that
        // actually discriminates.
        let rate = r.exact_rate();
        assert!(
            rate.is_finite() && (0.0..=1.0).contains(&rate),
            "seed {seed}: the site rate came back {rate}, which is not a rate"
        );
        pooled_sampled += r.sampled_with_site;
        pooled_placed += r.any_on_land;
        pooled_land += r.land_facets;
    }
    let sampled_rate = pooled_sampled as f64 / (SAMPLE * SEEDS.len()) as f64;
    let exact_rate = pooled_placed as f64 / pooled_land;
    println!(
        "H3 pooled: sampled rate {sampled_rate:.4} over {} facets; \
         EXACT RATE {exact_rate:.4e} ({:.6}% of land facets) — no predicted value, \
         this is the baseline",
        SAMPLE * SEEDS.len(),
        exact_rate * 100.0,
    );
}

/// The discriminator the sample cannot be: all three kinds of site are
/// actually placed, on land, on every seed. Without this, a world that placed
/// nothing at all would read exactly like the sparse one that ships — 0 of
/// 2,000, five times over — and the readout above would still pass.
/// claim: invariant(forall-seed) — every world places at least one settlement,
/// one exotic site and one cave on land, asserted over H3's frozen five.
#[test]
fn every_kind_of_site_is_actually_placed() {
    let wc = WorldComponents::assemble().expect("the canonical registries are well-formed");
    for seed in SEEDS {
        let r = read(seed, &wc);
        assert!(
            r.settlement > 0,
            "seed {seed} places no settlement anywhere"
        );
        assert!(r.exotic > 0, "seed {seed} places no exotic site anywhere");
        assert!(r.cave > 0, "seed {seed} places no cave anywhere");
        assert!(
            r.any_on_land > 0,
            "seed {seed} holds {} sites and places none of them on land",
            r.any
        );
        // The union is a union: never smaller than its largest member, never
        // larger than their sum. A regression that dropped a kind, or that
        // double-counted a facet holding two, breaks one side or the other.
        assert!(
            r.any >= r.cave.max(r.exotic).max(r.settlement)
                && r.any <= r.cave + r.exotic + r.settlement,
            "seed {seed}: union {} is not a union of {} settlement / {} exotic / {} cave",
            r.any,
            r.settlement,
            r.exotic,
            r.cave
        );
    }
}

/// **The finding, pinned as an assertion.** The any-kind rate cannot approach
/// "something interesting every square mile" — a rate near 1 — under placement,
/// because the mechanism emits at most one facet per geosphere vertex per kind
/// and there are ~9,830 walk facets per vertex.
///
/// Asserted rather than merely recorded so the finding is *falsifiable*: this
/// goes red if the globe level rises, the walk band coarsens, a site gains an
/// extent covering more than one facet (`hornvale_vessel::site::Extent`
/// reserves exactly that), or a DERIVED tier starts emitting sites at facet
/// resolution (`hornvale_vessel::site::Tier::Derived`, decision 0539) — each of
/// which is the change that would make H3 worth measuring again, and the last
/// of which is the follow-on campaign this baseline exists to aim.
/// claim: invariant(forall-seed) — the ceiling is `(2 * vertices + settlements)
/// / facets` and barely varies by seed; the five preregistered seeds are the
/// population only because that is what H3 froze.
#[test]
fn placement_cannot_reach_a_site_every_square_mile() {
    let wc = WorldComponents::assemble().expect("the canonical registries are well-formed");
    for seed in SEEDS {
        let r = read(seed, &wc);
        assert!(
            r.ceiling_rate() < 0.001,
            "seed {seed}: the placement mechanism's ceiling rate {:.4e} now reaches one \
             site per thousand land facets. H3 measured a placed world three orders of \
             magnitude under that; if this is red, the mechanism or the resolution \
             changed and the baseline needs re-measuring rather than this test deleting.",
            r.ceiling_rate()
        );
        assert!(
            r.exact_rate() <= r.ceiling_rate(),
            "seed {seed}: more site facets than the placement ceiling allows — placement \
             is no longer one facet per vertex per kind"
        );
    }
}
