//! Task 2a's positive control: the mixture-keeping split of `reflectance_at`
//! stays wired correctly.
//!
//! `reflectance_at` is now defined as
//! `self.reflectance_mixture_at(addr, micro, at)?.integrate()`, so
//! `assert_eq!(reflectance_mixture_at(addr, micro, at).integrate(),
//! reflectance_at(addr, micro, at))` holds for *any* implementation of
//! `reflectance_mixture_at` — there is no interior mutability anywhere in
//! the call chain, so the two sides are structurally the same expression
//! evaluated twice, for any `micro`/`at` fed to both. This test therefore
//! does **not** establish byte-identity against the pre-refactor
//! implementation; it is a wiring check, and Task 2b's surface-cover layer
//! (spec §3.2) does not disturb it — the tautology holds with cover
//! composed in exactly as it held without it. It is still worth keeping: a
//! future edit that accidentally de-links the delegation (has
//! `reflectance_at` integrate something other than what
//! `reflectance_mixture_at` returns) would fail it. `micro` below is a
//! fixed, neutral field (not derived via `describe`) precisely because this
//! test does not care what the cover layer does with it — only that both
//! sides of the delegation see the same value.
//!
//! **The byte-identity control is the empty `make rebaseline` diff**
//! against `docs/generated-paths.txt` (see the task report) — that is the
//! artifact comparison that actually shows no colour moved, because it
//! compares this commit's rendered output against the committed output of
//! the pre-refactor code, not two calls into the same commit. Task 2b's
//! diff is legitimately non-empty — see that task's own report.

use hornvale_kernel::color::BANDS;
use hornvale_kernel::math::unit_sphere_from_lat_lon;
use hornvale_kernel::{Facet, Seed, Vertex, World, WorldTime};
use hornvale_locale::{LocaleContext, MicroField};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};
use std::collections::BTreeSet;

/// A fixed, neutral micro-field — every axis at its midpoint. This test
/// verifies delegation wiring (`reflectance_at` == `reflectance_mixture_at`
/// integrated), which holds for any `micro`, so a synthetic constant avoids
/// paying for 200 `describe` calls just to get one.
fn neutral_micro() -> MicroField {
    MicroField {
        relief: 0.0,
        aspect: 0.0,
        wetness: 0.0,
        openness: 0.0,
    }
}

/// A depth at which every constructed `Facet` is guaranteed addressable:
/// `corner_weights` requires `path.len() >= geo.depth()`, and the seed-42
/// world's canonical grid sits at level 6 (`GLOBE_LEVEL`), so 13 — the walk
/// band, which `hornvale_locale::walk_depth` defines — clears that floor with
/// room to spare.
///
/// **13, not 12, and this constant is on a leash now.** It restates the walk
/// depth ABSOLUTELY, which is invisible to any scan for a `globe_level()`
/// offset, so it fell a band behind when The Pavement moved the walk band to
/// `globe_level + 7`. Its own doc cited "the same 'six refinement levels below
/// the canonical grid' convention `windows/locale/src/lib.rs` and
/// `windows/vessel/src/agent.rs` already use" — a restatement of a
/// restatement. `cli/tests/suite/walk_depth_agreement.rs`'s
/// `absolute_walk_depth_constants_track_the_walk_band` now holds this value to
/// the live walk depth at the canonical globe level. It cannot be a call,
/// because it is a `const` and `walk_depth` needs a `LocaleContext`.
const DEPTH: usize = 13;

/// How many addresses to generate per base face.
///
/// **Ten until The Pavement, when the base mesh went from twenty triangular
/// faces to six quads.** Ten per face was 200 addresses; ten per face is now
/// 60, which is a thinner spread than the claim below wants. Thirty-three
/// keeps the total near 200 (198) and needs three base-4 path digits rather
/// than two — 64 combinations, so all thirty-three are still distinct.
const PER_FACE: u8 = 33;

/// The number of base faces the mesh has. **Six since The Pavement**, and it
/// is stated here rather than written inline because the loop below used to
/// say `0..20u8` — every address on faces 6..20 became `Unaddressable` at the
/// epoch, and the first thing this test did with one was panic inside the
/// kernel: *"invalid cube face 6: a cube-sphere has exactly 6 faces, numbered
/// 0..6"*.
const BASE_FACES: u8 = 6;

/// A spread of `BASE_FACES * PER_FACE` distinct, addressable `Facet`s,
/// round-robin distributed across ALL base faces (never filled greedily from
/// one face — an earlier version of this test claimed "all 20 faces" while
/// actually reaching 13 of them, because it broke out of the loop as soon as
/// it had 200 addresses). Each face's addresses vary the leading three path
/// digits (base-4, so all `PER_FACE` are distinct), padded to `DEPTH` with
/// zeros.
fn spread() -> Vec<Facet> {
    let mut out = Vec::with_capacity(BASE_FACES as usize * PER_FACE as usize);
    for face in 0..BASE_FACES {
        for i in 0..PER_FACE {
            let mut path = vec![i % 4, (i / 4) % 4, i / 16];
            path.resize(DEPTH, 0);
            out.push(Facet { face, path });
        }
    }
    out
}

#[test]
fn integrating_the_kept_mixture_equals_integrating_immediately() {
    let world = World::new(Seed(42));
    let ctx = LocaleContext::build(&world).unwrap();
    let addrs = spread();
    assert_eq!(
        addrs.len(),
        BASE_FACES as usize * PER_FACE as usize,
        "spread must produce one address per (face, index) pair"
    );
    let distinct: BTreeSet<&Facet> = addrs.iter().collect();
    assert_eq!(
        distinct.len(),
        addrs.len(),
        "spread must produce DISTINCT addresses, or the sweep counts one room twice"
    );
    let faces: BTreeSet<u8> = addrs.iter().map(|a| a.face).collect();
    assert_eq!(
        faces.len(),
        BASE_FACES as usize,
        "spread must cover every base face"
    );
    let micro = neutral_micro();
    for addr in &addrs {
        let via_mixture = ctx
            .reflectance_mixture_at(addr, &micro, WorldTime::GENESIS)
            .unwrap()
            .integrate();
        let direct = ctx
            .reflectance_at(addr, &micro, WorldTime::GENESIS)
            .unwrap();
        assert_eq!(via_mixture, direct, "addr {addr:?} moved");
    }
}

/// H2: a marginal, seasonally-freezing vertex's mixture is brighter in the
/// cold half of the year than the warm half — the campaign's headline
/// seasonal claim (spec §3, "a peak is white in winter because its mixture
/// changed").
///
/// **Why `Vertex(30344)`, not the global elevation maximum.** Task 1
/// measured seed 42's global max-elevation land vertex (`Vertex(21329)`) as
/// frozen 32/32 across a full-year sweep — white *all year*, not seasonally
/// white, so it cannot demonstrate a seasonal crossing. `Vertex(30344)` is
/// the land vertex whose annual mean sits closest to the freeze line
/// (-0.001 C); Task 1's resampled 32-point sweep there found 16/32 frozen,
/// annual minimum -8.898 C at day 337.35, first frozen day 328.14
/// (task-1-report.md's "§6.1 addendum" section — those are measured
/// numbers, not estimates).
///
/// **Why 32 samples, not 4.** Task 1's own report flags a ~23-day
/// sub-annual oscillation superimposed on the annual trend at this vertex: an
/// 8-sample, 46-day-spaced sweep aliased against that oscillation and read
/// as a smooth monotonic decline that never crossed freezing, missing the
/// true minimum entirely (it sat in the unsampled last 12.5% of the year).
/// A 32-sample, ~11.5-day-spaced sweep is the density Task 1 validated
/// finds the real crossing, so this test uses the same density rather than
/// the brief's illustrative "four evenly spaced days".
///
/// **Why a mean-band reflectance, not a full sRGB projection.**
/// `windows/locale` has no dependency on `hornvale-astronomy` (no
/// `Illuminant`) or a colour `Observer` beyond what `hornvale-kernel`
/// re-exports for the mixture math itself, and pulling one in for a single
/// test is not worth a new edge on the dependency graph. The mean of the
/// integrated reflectance's 10 bands is a legitimate achromatic brightness
/// proxy for this purpose: [`hornvale_locale`]'s own [`endmembers::SNOW`]
/// (not reachable from here — `pub(crate)`, so this comment states the
/// values it observed in `surface.rs` instead) sits at 0.90-0.94 in every
/// band, well above every other endmember and the mineral ground beneath
/// it, so more snow cover raises the mean in every band it touches; there
/// is no band where snow reads dark and could cancel the rise out.
#[test]
fn high_ground_is_brighter_in_the_cold_half_of_the_year() {
    // Matches `windows/scene/examples/illumination_probe.rs`'s `genesis()`
    // exactly — the construction Task 1's numbers above were measured
    // against. `World::new(Seed(42))` (used by the other test in this file)
    // is NOT equivalent: with no sky-provider fact committed, `sky_of` today
    // falls back to a constant sun, which carries no seasonal swing at all
    // (`temperature_at` would be day-invariant), so it cannot reproduce
    // Task 1's day-dependent readings. THE ZENITH: that fallback is on its
    // way out — `sky_of` will error instead — which does not change what this
    // test needs, an explicitly built generated sky, but does mean the
    // bare-world construction it warns against will stop building at all
    // rather than building the wrong thing.
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds with a generated sky");
    let ctx = LocaleContext::build(&world).unwrap();
    let depth = hornvale_locale::walk_depth(&ctx);
    // **A vertex with a seasonal freeze/thaw crossing, FOUND rather than
    // pinned.** It was `Vertex(30344)`, measured by Task 1 of another
    // campaign, and the address built from its centroid then resolved back to
    // it. On the cube-sphere mesh a room's corners are not geosphere vertices
    // (decision 0287's corollary, retired by The Pavement), so the same
    // construction resolves to `Vertex(30331)` instead and the identity
    // assertion below it failed.
    //
    // The identity was never the claim. What the load-bearing assertion needs
    // is a place where reflectance VARIES across the year, which requires a
    // vertex whose temperature crosses freezing — so that is what this asks
    // for, and it asks for it of the vertex the constructed address actually
    // resolves to, which is what keeps the two halves consistent by
    // construction rather than by a re-measured coincidence.
    let year = ctx.climate().year_length_std();
    let crosses = |v: Vertex| {
        let mut below = false;
        let mut above = false;
        for i in 0..8 {
            let at = WorldTime::from_std_days(year * f64::from(i) / 8.0).expect("finite day");
            let t = ctx.climate().temperature_at(v, at).get();
            below |= t <= 0.0;
            above |= t > 0.0;
        }
        below && above
    };
    let (vertex, addr) = (0..ctx.climate().geosphere().vertex_count() as u32)
        .step_by(7)
        .find_map(|i| {
            let seed_vertex = Vertex(i);
            let coord = ctx.climate().geosphere().coord(seed_vertex);
            let addr = Facet::containing(
                unit_sphere_from_lat_lon(coord.latitude, coord.longitude),
                depth,
            );
            let corners = addr.corner_weights(ctx.climate().geosphere(), ctx.nearest_index())?;
            // The same "max weight, tie-break lowest Vertex" rule
            // `LocaleContext`'s private `dominant_corner` uses — restated here
            // rather than imported, since it is not `pub` and this is an
            // integration test in a separate crate.
            let dominant = corners
                .iter()
                .fold(corners[0], |best, &cand| {
                    if cand.1 > best.1 || (cand.1 == best.1 && cand.0.0 < best.0.0) {
                        cand
                    } else {
                        best
                    }
                })
                .0;
            crosses(dominant).then_some((dominant, addr))
        })
        .expect(
            "no sampled vertex on seed 42 has a temperature that crosses freezing across its \
             own year, so there is nowhere the seasonal snow term can be observed at all",
        );

    // Neutral micro-field: isolate the seasonal (climate) term the test
    // claims, rather than letting the room's own address-noise modulation
    // (aspect's snow-retention swing, in particular) confound the reading.
    let micro = MicroField {
        relief: 0.0,
        aspect: 0.0,
        wetness: 0.0,
        openness: 0.0,
    };

    let year_length = ctx.climate().year_length_std();
    const SAMPLES: usize = 32;
    let mut lightness = Vec::with_capacity(SAMPLES);
    let mut temps_c = Vec::with_capacity(SAMPLES);
    for i in 0..SAMPLES {
        let day = year_length * i as f64 / SAMPLES as f64;
        let at = WorldTime::from_std_days(day).expect("finite day");
        let reflectance = ctx.reflectance_at(&addr, &micro, at).unwrap();
        let mean: f64 = reflectance.get().iter().sum::<f64>() / BANDS as f64;
        lightness.push(mean);
        // THE ONE SITE THAT ROUNDS (The Foliot, stage 4). `day` here is an
        // arbitrary fraction of a year, not a whole-day index, so landing it
        // on the tick lattice moves the sample point by up to half a tick
        // (0.432 s). Recorded rather than absorbed: every other climate call
        // site in the tree was byte-neutral, either a literal genesis or a
        // caller that already held a `WorldTime` and was unwrapping it.
        // Harmless here — this samples a smooth seasonal curve at 24 points
        // to check a mixture, and a sub-second shift cannot move that.
        temps_c.push(
            ctx.climate()
                .temperature_at(
                    vertex,
                    WorldTime::from_std_days(day).expect("a finite sample day"),
                )
                .get(),
        );
    }

    let distinct = {
        let mut sorted = lightness.clone();
        sorted.sort_by(f64::total_cmp);
        sorted.dedup_by(|a, b| (*a - *b).abs() < 1e-12);
        sorted.len()
    };
    // This is the load-bearing assertion. `distinct > 1` genuinely could
    // fail — if snow cover were annual (keyed only on `snow_fraction_at`)
    // rather than seasonal, or if `is_frozen_at` never flipped across the
    // sampled year at this vertex, every sample would compose the identical
    // mixture and this would go red. Task 1's own report measured that
    // crossing at this exact vertex before this test was written.
    assert!(
        distinct > 1,
        "reflectance must vary across the year at a vertex with a measured seasonal \
         freeze/thaw crossing; got lightness={lightness:?} temps_c={temps_c:?}"
    );

    // A directional "the brightest day falls in the cold half" clause was
    // removed here (Task 2b fix round, FINDING 3). `is_frozen_at(c, d)` is
    // literally `temperature_at(c, d) <= 0.0`, and snow is gated exactly on
    // that boolean, so the brightest sampled day is *by construction*
    // always a frozen day, always at temperature <= 0. Such a clause can
    // therefore only ever fail when the SAMPLED MEAN temperature is itself
    // <= 0 — at this vertex (annual mean -0.001 C) that depends on exactly
    // which days land in a fixed-count sample, not on whether the seasonal
    // mechanism is real. Measured: the 32-sample mean here is +0.042 C, so
    // the clause passed, but one different sampling choice could flip that
    // sign for reasons unrelated to physics. An assertion that cannot fail
    // for the reason it claims is worse than no assertion — it reads as
    // evidence it is not standing for. `distinct > 1` above is what
    // actually tests the seasonal claim.
}
