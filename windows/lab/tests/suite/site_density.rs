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
//! **Pooled exact rate `~1.19e-5` — about 0.0012% of land facets, one site per
//! ~84,200 land facets.** That is H3, and it is the campaign's deliverable.
//! Pooled sampled rate `1.0e-4` (one hit in 10,000).
//!
//! **Only three significant figures are resolved, and the table's raw
//! `1.1875e-5` is a print, not a claim to five.** The numerator is exact —
//! counted over the whole placed set — but the denominator is not:
//! `land_facets` is `all_facets * kept / drawn`, an accept-rate estimate whose
//! relative standard error is ~1.75% per seed and ~0.78% pooled at
//! `k = 2,000`, `p ~ 0.386`. So the pooled rate is `1.19e-5 +- ~1%`. The
//! 1.8-4.0% H2/H3 disagreement recorded under [`SAMPLE`] **is** that error,
//! observed rather than predicted. Quote `~1.19e-5` and `~84,200`; do not
//! carry the fourth and fifth digits into anything downstream.
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
//! ~17x and leave the rate at 2e-4 — still ~2,400x under a site per square
//! mile.
//!
//! Nathan's goal for the world is "every square mile, something interesting".
//!
//! **A walk facet is 1.126 km per side, so a square mile is 2.04 facets and
//! the target is a rate of ~0.489 — not 1.** An earlier draft of this
//! paragraph took "~1.7 km" from `hornvale_vessel::depth`, which is a
//! PRE-CUBE-SPHERE icosphere depth-12 figure, stale upstream of this campaign
//! and now corrected there. At 1.7 km a facet is 2.89 km2 and one facet really
//! would be about one square mile, which is why the substitution looked free.
//! The authoritative owner of the walk band is `hornvale_locale::walk_depth`,
//! whose own doc gives depth 13 as **1.126 km** per side — 1.267 km2, so
//! 2.04 facets to the square mile. The rate `~1.19e-5` was never affected;
//! only the target it was being compared against.
//!
//! So the gap is **~41,200x (4.61 orders of magnitude)** at the measurement
//! and **~2,400x (3.38 orders)** at the placement ceiling, against a target of
//! ~0.489 sites per facet. The earlier 84,200x / 4,900x compared against a
//! rate of 1 and are each about 2x too large.
//!
//! The shortfall is the placement MECHANISM's resolution and not any constant,
//! and no threshold, predicate or budget moves it:
//! `hornvale_vessel::site::Tier::Derived` — modelled, unused, decision 0539 —
//! is the axis that can. H3's job was to size the gap, and the gap is ~4.6
//! orders of magnitude.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Facet, Seed, Vertex};
use hornvale_locale::LocaleContext;
use hornvale_vessel::liveness::built_rooms;
use hornvale_vessel::site::{Extent, SiteKind};
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
/// sample SIZE.
///
/// **It does not match H2's SAMPLE, and this doc said it did.** It read
/// "matching H2's sample exactly so the two readouts share a denominator".
/// Only the size matches: H2 draws from `lab/the-prospect/cave-rate` and H3
/// from `lab/the-prospect/site-density`, so the two accept different facets,
/// accept them at slightly different rates, and land on `land_facets`
/// estimates that differ by **+2.47 / -3.17 / +2.15 / -1.83 / -3.96%** across
/// the five seeds. H2's `9.3411e-6` and H3's `1.1875e-5` are therefore
/// comparable to about +-4%, not exactly — enough for "H3 is a little under
/// twice H2", not enough to subtract one from the other and call the remainder
/// a non-cave rate.
///
/// The other half of that sentence **is** true and is worth keeping
/// separately, because it is what lent the false half its authority: H3's
/// numerator is a strict superset of H2's *set-wise*, since cave facets are a
/// subset of any-kind facets under the identical land filter (one shared
/// [`is_land`], not two copies). Supersets of the numerator; independent draws
/// of the denominator.
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
    /// site, plus one settlement per settlement, each covering
    /// [`facets_per_site`] facets. An over-estimate by construction (the two
    /// kinds' facets can coincide), which is what makes it a ceiling.
    fn ceiling_rate(&self) -> f64 {
        (2.0 * self.vertices + self.settlement as f64) * facets_per_site() / self.all_facets
    }

    /// Walk facets per geosphere vertex — the single structural quantity the
    /// whole ceiling reduces to, and the one this file's assertions actually
    /// pin. `402,653,184 / 40,962 = 9,830`.
    ///
    /// It is very nearly INVARIANT in the globe level, because
    /// `hornvale_locale::walk_depth` is `globe_level + 7`: vertices go as
    /// `10 * 4^L + 2` and facets as `6 * 4^(L+7)`, so the ratio tends to
    /// `6 * 4^7 / 10 = 9,830.4` and measures 9,828.9 at L=5 and 9,830.4 at
    /// L=9. It moves by a factor of 4 per band if that OFFSET changes, which
    /// is the change [`placement_cannot_reach_a_site_every_square_mile`] can
    /// see.
    fn facets_per_vertex(&self) -> f64 {
        self.all_facets / self.vertices
    }
}

/// How many walk facets one placed site covers, by an EXHAUSTIVE match on
/// [`Extent`].
///
/// The match is the point. `Extent` has one variant today, so this returns 1
/// and the ceiling is unchanged by its presence — but a variant covering more
/// than one facet **cannot be added without an arm here**, and whatever that
/// arm returns feeds [`Reading::ceiling_rate`] directly. That converts "a site
/// gains an extent covering more than one facet" from a sentence this file
/// used to claim as a trigger into one the compiler enforces.
///
/// Deliberately not `match self.extent` on a real `Site`: no `Site` value
/// enters this measurement (the union is over facets), so the exhaustiveness
/// is over the TYPE, which is what needs guarding.
fn facets_per_site() -> f64 {
    // A future multi-facet variant belongs here, and adding it will not
    // compile until it does.
    match Extent::Point {
        Extent::Point => 1.0,
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
        // `expect`, not `filter_map(…ok())`, which is what this was. A dropped
        // key would silently shrink H3's numerator and the rate would come back
        // plausible — the one failure mode a measurement must not have. It is
        // safe today only because `built_rooms` itself filters on `pack().ok()`
        // (`windows/vessel/src/liveness.rs`), so every key it emits round-trips;
        // that dependency was unstated, and stating it in a panic message costs
        // nothing and fails loudly if it ever stops holding.
        .map(|id| {
            id.unpack().unwrap_or_else(|e| {
                panic!(
                    "seed {seed}: a `built_rooms` key did not unpack ({e:?}). H3's \
                     settlement numerator would be silently short; `built_rooms` is \
                     supposed to emit only keys it packed itself."
                )
            })
        })
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

/// **The finding, pinned as an assertion — and the claim is now the one the
/// assertions can actually keep.**
///
/// Placement cannot approach "something interesting every square mile" — a
/// rate of ~0.489 per facet — because the mechanism emits at most
/// [`facets_per_site`] facets per geosphere vertex per kind and there are
/// ~9,830 walk facets per vertex.
///
/// # What this test detects, having been mutation-proved
///
/// | change | detected? | by what |
/// | --- | --- | --- |
/// | the walk band coarsens or refines by one | **yes** | the tight band on [`Reading::facets_per_vertex`] |
/// | `walk_depth` stops being `globe_level + 7` | **yes** | same |
/// | the facet branching factor or the vertex formula changes | **yes** | same |
/// | a new [`Extent`] variant covering >1 facet | **yes, at compile time** | [`facets_per_site`]'s exhaustive match, which feeds the ceiling |
/// | a new [`SiteKind`] | **yes, at compile time** | [`every_site_kind_has_a_roster_in_this_measurement`] |
/// | more site facets than placement can emit | **yes** | the count bound below |
/// | **the globe level rises** | **NO — and it never could** | see below |
/// | **a DERIVED tier emits sites at facet resolution** | **NO** | see below |
///
/// # THE FOUR-WAY CLAIM THIS DOC USED TO MAKE WAS FALSE, AND THE REVIEW PROVED IT
///
/// It read: *"this goes red if the globe level rises, the walk band coarsens, a
/// site gains an extent covering more than one facet … or a DERIVED tier starts
/// emitting sites at facet resolution."* A one-globe-level bump and a 10x
/// derived-density mutation were applied **simultaneously** and this test still
/// passed. Two of the four triggers were fiction and one needed two bands, not
/// one. Taking them one at a time:
///
/// - **The globe level is not a trigger and cannot be made one**, because the
///   ceiling is genuinely scale-invariant: `walk_depth = globe_level + 7`, so
///   vertices and facets scale together and the ceiling holds 2.03-2.07e-4
///   across L5-L9. There is nothing to detect. The claim was not merely
///   unenforced, it was describing a dependency that does not exist. What DOES
///   move the ceiling is the OFFSET, and that is now pinned tightly.
/// - **The walk band needed TWO bands to red** under the old `< 0.001`
///   threshold (one band gives 8.18e-4, still under). The bound is now a
///   two-sided band on facets-per-vertex, which reds on one band in either
///   direction.
/// - **`Extent` is now a real trigger**, via [`facets_per_site`]. It was not
///   before: `Extent` appeared nowhere in the arithmetic, and the old doc's
///   parenthetical ("`Extent` reserves exactly that") pointed at a `Region`
///   variant that **does not exist** — decision 0538 was corrected on
///   2026-09-03 for declaring it.
/// - **A derived tier is NOT a trigger, and this test cannot become one.**
///   [`read`] collects three concrete rosters — `built_rooms`,
///   `strange_sites`, `cave_site_vertices` — and a derived producer would be a
///   fourth function that nothing here is obliged to call, so derived sites
///   would not enter the union at all and neither rate would move. The honest
///   guard is elsewhere and is a fact about the tree rather than about a world:
///   `Site::derived` has no production caller (decision 0539,
///   `book/src/open-questions.md`), and the day one appears, H3 must be
///   re-measured rather than re-asserted. **This test will not tell you that
///   day has come.** Saying so is the correction; a broad false guarantee is
///   worse than a narrow true one, because only the false one stops people
///   looking.
///
/// claim: invariant(forall-seed) — the structural bounds are seed-independent
/// (only the settlement count varies, and it enters nothing asserted here);
/// the five preregistered seeds are the population only because that is what
/// H3 froze.
#[test]
fn placement_cannot_reach_a_site_every_square_mile() {
    let wc = WorldComponents::assemble().expect("the canonical registries are well-formed");
    for seed in SEEDS {
        let r = read(seed, &wc);
        // THE structural quantity. `6 * 4^(L+7) / (10 * 4^L + 2)` is 9,828.9 at
        // globe level 5 and 9,830.4 at 9, so this band is tight to ~0.7% while
        // being blind to the globe level — which is correct, because the globe
        // level genuinely does not move it. One walk band in either direction
        // moves it 4x and reds.
        assert!(
            (9_800.0..=9_900.0).contains(&r.facets_per_vertex()),
            "seed {seed}: {:.1} walk facets per geosphere vertex, outside 9,800..=9,900. \
             The walk band, the branching factor, or `walk_depth`'s +7 offset moved, so \
             H3's ceiling is no longer ~2.04e-4 and the baseline needs RE-MEASURING \
             rather than this band widening.",
            r.facets_per_vertex()
        );
        // The ceiling in rate form, checked loosely: this is downstream of the
        // band above and is here so the number a reader sees quoted is the
        // number the test saw.
        assert!(
            r.ceiling_rate() < 0.001,
            "seed {seed}: ceiling rate {:.4e} reaches one site per thousand facets",
            r.ceiling_rate()
        );
        // COUNTS, not rates — and this replaces `exact_rate() <= ceiling_rate()`,
        // which divided a per-LAND-facet rate by a per-ALL-facet one. Those
        // cancel only because roughly the land fraction of vertices is itself on
        // land, an undocumented coincidence that `--ocean-fraction` breaks: below
        // ~1% land the old form false-reds while reporting "placement is no
        // longer one facet per vertex per kind", a misdiagnosis. Comparing counts
        // has no denominator to mismatch and says exactly what placement
        // guarantees.
        let emitted = r.any as f64;
        let emittable = (2.0 * r.vertices + r.settlement as f64) * facets_per_site();
        assert!(
            emitted <= emittable,
            "seed {seed}: {emitted} site facets against a placement bound of {emittable} \
             — placement is no longer one facet per vertex per kind"
        );
        // Internal consistency of the split, which is what makes the numerator
        // trustworthy at all. `any_on_land` is H3's numerator; if it could drift
        // from the union it is drawn from, the rate would stay plausible.
        assert_eq!(
            r.any_on_land + r.any_on_water,
            r.any,
            "seed {seed}: the land/water split does not reconstitute the union"
        );
        assert!(
            r.any_on_land <= r.any,
            "seed {seed}: {} site facets on land out of a union of {}",
            r.any_on_land,
            r.any
        );
    }
}

/// **Every [`SiteKind`] has a roster in [`read`], enforced by the compiler.**
///
/// H3 measures the union of three rosters. If a fourth kind of site is added
/// and `read` is not taught to collect it, the published rate silently becomes
/// a rate for *some* of the world's sites while still reading as "a site of any
/// kind" — a plausible number that is a category error, and nothing about the
/// arithmetic would object.
///
/// The exhaustive match is the whole test: adding a variant to `SiteKind` does
/// not compile until someone comes here and says which roster supplies it. That
/// is a real trigger, unlike the two this file used to claim; it costs no world
/// build and it fires at the only moment it is useful.
#[test]
fn every_site_kind_has_a_roster_in_this_measurement() {
    for kind in [SiteKind::Cave, SiteKind::Exotic, SiteKind::Settlement] {
        // Exhaustive by construction — no wildcard arm, deliberately.
        let roster = match kind {
            SiteKind::Cave => "GeneratedTerrain::cave_site_vertices",
            SiteKind::Exotic => "LocaleContext::strange_sites",
            SiteKind::Settlement => "hornvale_vessel::liveness::built_rooms",
        };
        assert!(
            !roster.is_empty(),
            "{kind:?} has no roster in H3's `read` — teach `read` to collect it, or H3 \
             stops being a rate over sites of ANY kind"
        );
    }
}
