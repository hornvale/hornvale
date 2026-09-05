//! `prevalence` must be position-continuous, never address-hashed — The
//! Weft, Task 5; spec §5.1. Paired with an anti-vacuity companion (fix round
//! 1, F3) per spec §7's H2 requirement that coherence be "paired with an
//! anti-vacuity companion … so a degenerate world cannot score perfectly" —
//! the same shape The Ford pairs `channel-band-monotonicity` with
//! `channel-transect-dry-reach`. Task 7 extends both to the three new kinds,
//! and adds the eligibility regression `spring_never_occurs_off_land` (R1).
//!
//! **Fix round 1 (C1/M2/M3) replaced the single-walk calibration with a
//! broad, representative sample.** The shipped version calibrated
//! `KIND_BOUNDS` off ONE 200-step walk from vertex 14 — 18% of spring's own
//! measured global range, per the review. Worse, for erratic/scatter (short
//! correlation length, near-zero contextuality) a raw max-delta bound is a
//! genuinely weak discriminator: at the shipped bound (`0.07`, 92% of
//! erratic's own theoretical amplitude), an address-hashed mutant exceeded
//! it only 0.5% of the time — the guard could not catch what it exists to
//! catch, on the one kind least able to absorb that (Task 5's F2 shape
//! again: a bound loose enough to be decorative).
//!
//! The fix is two-part:
//!
//! 1. **[`land_eligible_walks`] samples 78 land-eligible 60-step walks**
//!    (`STRIDE = 137`-spaced starting vertices across all 40,962), not one —
//!    4,602 adjacent-pair deltas and 4,680 facet evaluations per kind. Every
//!    bound in [`KIND_BOUNDS`] is now measured against this pool, with a
//!    per-kind floor read off the pool's OWN measurement (never a shared
//!    `1`) — see that constant's own doc for the real/mutant numbers.
//! 2. **A lag-1 autocorrelation check ([`AUTOCORR_BOUNDS`],
//!    `prevalence_autocorrelation_is_not_address_hashed`) is now the
//!    PRIMARY discriminator, not the max-delta bound.** Measured on THIS
//!    tree, real vs. an address-hashed mutant (the same in-place swap Task 5
//!    used: `fbm.sample(facet.centroid())` → `facet.seed(seed)
//!    .stream().next_f64()`, both passed through `uniformize`, reverted
//!    after measuring):
//!
//!    | kind | real r | mutant r |
//!    | --- | --- | --- |
//!    | spring | 0.99821 | 0.20919 |
//!    | overhang | 0.98238 | 0.08907 |
//!    | thicket | 0.99994 | 0.89048 |
//!    | erratic | 0.86795 | **-0.02545** |
//!
//!    Erratic's separation (0.868 vs. -0.025) is the cleanest of the four —
//!    exactly the opposite of the max-delta bound's own weakest case — because
//!    erratic's tiny contextuality (`0.05`) means almost the entire mixed
//!    signal IS the noise term, so decorrelating it destroys the
//!    correlation outright. The max-delta bound stays as a SECONDARY check
//!    (a real regression that flattens the field entirely, e.g., or a gross
//!    scale error, would still trip it), but autocorrelation is what this
//!    fix round actually leans on for erratic.
//!
//! **The eligibility-cliff reasoning from the shipped version is unchanged
//! and still governs `land_eligible_walks`**: Task 7's R1 gate makes
//! ineligible ground a hard `prevalence == 0.0` cliff, a REAL geographic
//! discontinuity (a coastline), not decorrelated noise, so every walk this
//! file samples is kept only if it stays land-eligible for its entire
//! length — never mixed into a noise-smoothness measurement. The
//! eligibility cliff itself is exercised directly by
//! `spring_never_occurs_off_land` below, over the WHOLE grid.
//!
//! Test fixture (decision 0092): calls the sculpt derivation entry point
//! (`terrain_of`) directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{Facet, NearestVertexIndex, Vertex, blend_corner_weights};
use hornvale_worldgen::WeftKind;

/// Walk depth used everywhere in this file: `windows/locale::walk_depth`
/// documents its own value as "the globe level plus 7"
/// (`windows/worldgen/src/placement.rs` restates the same relationship at its
/// own call site); this file cannot import `windows/locale` at all (it
/// depends on `hornvale-worldgen`, the crate under test — the same
/// circularity Ruling 1 of this task's dispatch names), so the relationship
/// is reproduced directly rather than imported.
const WALK_DEPTH_BELOW_GRID: u32 = 7;

/// Steps per sampled walk (fix round 1). Short enough that a walk starting
/// on a modest island or coastal strip still often qualifies as fully
/// land-eligible, long enough to give each kind's own correlation length
/// (5–60 facets) room to move.
const WALK_LEN: usize = 60;

/// Spacing between candidate starting vertices, in raw vertex index (fix
/// round 1). `40,962 / 137 ≈ 299` candidates scanned; 78 qualify (stay
/// land-eligible for the whole walk) — a broad, globally-spread sample
/// rather than one hand-picked start, closing M2. Prime-ish and unrelated to
/// the geosphere's own subdivision structure, so it does not alias onto any
/// lattice regularity.
const STRIDE: u32 = 137;

/// One sampled walk: [`WALK_LEN`] steps of `(prevalence, occurs)`, kept only
/// if every step stayed land-eligible — see this file's own module doc for
/// why a coastline crossing must never enter a noise-smoothness sample.
type Walk = Vec<(f64, bool)>;

/// Every land-eligible [`WALK_LEN`]-step walk starting at a
/// [`STRIDE`]-spaced vertex, for `kind` — the representative sample fix
/// round 1 (M2) replaced the single hand-picked walk with. Builds its own
/// world/terrain/climate/pack once (not shared across kinds — this file
/// values the fixture-per-call posture decision 0092 sanctions over a
/// shared-fixture optimization).
fn land_eligible_walks(kind: WeftKind) -> Vec<Walk> {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;
    let n = geo.vertex_count() as u32;

    let mut walks = Vec::new();
    let mut start = 0u32;
    while start < n {
        let mut facet = Facet::containing(geo.position(Vertex(start)), walk_depth);
        let mut walk: Walk = Vec::with_capacity(WALK_LEN);
        let mut eligible_throughout = true;
        for _ in 0..WALK_LEN {
            let Some(weights) = facet.corner_weights(geo, &index) else {
                eligible_throughout = false;
                break;
            };
            if blend_corner_weights(weights, &pack.land) < 0.5 {
                eligible_throughout = false;
                break;
            }
            let p = hornvale_worldgen::prevalence(kind, &facet, geo, &index, &pack, world.seed)
                .expect("corner_weights just returned Some above");
            let occ = hornvale_worldgen::occurs(kind, &facet, world.seed, p);
            walk.push((p, occ));
            facet = facet
                .neighbors()
                .into_iter()
                .next()
                .expect("a facet always has an edge neighbour");
        }
        if eligible_throughout && walk.len() == WALK_LEN {
            walks.push(walk);
        }
        start += STRIDE;
    }
    walks
}

/// Per-kind `(kind, max_allowed_delta, min_pooled_spread,
/// min_total_occurs)` — every bound measured against
/// [`land_eligible_walks`]'s full 78-walk, 4,680-facet, 4,602-delta pool
/// (fix round 1, M2/M3), never a single walk or a shared floor.
///
/// **Measured (seed 42, this pool, current post-R1 mechanism) — re-measured
/// in fix round 2 (N2): the overhang row below was stale.** It was measured
/// against `OVERHANG_SLOPE_SATURATION = 8_000.0` and never re-run after the
/// SAME commit (fix round 1) changed the constant to
/// `hornvale_terrain::GORGE_SLOPE` (`40_000.0`, I2) — a real recipe change
/// (a gentler `tanh` saturation lowers overhang's typical macro-state
/// contribution), so its own calibration table drifted under it unnoticed.
///
/// **The spring pooled-min cell (lexicon: a markdown table cell, an area,
/// not the mesh sense) was ALSO stale (fix round 3), for a
/// different reason: a bug in the standalone calibration probe used to
/// produce this table, not in [`land_eligible_walks`] itself.** That probe
/// re-implemented the walk loop and updated its running pooled min/max
/// INSIDE the per-step loop, unconditionally — so a walk later rejected for
/// leaving land eligibility (`eligible_throughout = false`) still leaked its
/// partial prevalence series into the pooled bounds before the rejection
/// was known. `land_eligible_walks` itself has no such bug (it only pushes
/// a walk's samples into the returned `Vec` once `eligible_throughout &&
/// walk.len() == WALK_LEN` both hold), so calling it directly — the same
/// production path [`prevalence_is_continuous_across_adjacent_facets`] and
/// [`the_walk_is_not_degenerate`] use — gives the correct figure. Verified
/// against two independent external re-measurements before correcting:
///
/// | kind | max delta | pooled spread | total occurs |
/// | --- | --- | --- | --- |
/// | spring | 0.00576 | 0.11268 (`[0.00249, 0.11518]`) | 125 / 4,680 |
/// | overhang | 0.02982 | 0.13333 (`[0.00560, 0.13893]`) | 275 / 4,680 |
/// | thicket | 0.00764 | 0.39418 (`[0.00054, 0.39473]`) | 924 / 4,680 |
/// | erratic | 0.04920 | 0.07594 (`[0.00203, 0.07796]`) | 233 / 4,680 |
///
/// `max_allowed_delta` below leaves real headroom over its own kind's
/// measured max while staying well under that kind's own address-hashed
/// mutant max (measured the same way, see this file's module doc): spring
/// `0.010` (1.7x real / 5.4x under mutant `0.0535`), overhang `0.045` (1.5x
/// real / 2.2x under mutant `0.1009` — the mutant max is unaffected by the
/// slope-constant fix, since it comes from the fully decorrelated noise
/// term, not the macro-state recipe), thicket `0.015` (2.0x real / 4.6x
/// under mutant `0.0690`). **Erratic's `0.060` (1.2x real / only 1.3x under
/// mutant `0.0760`) is deliberately a weak, secondary check** — see this
/// file's module doc: erratic's real discrimination comes from
/// [`AUTOCORR_BOUNDS`] below, not this bound, because a short-correlation,
/// near-zero-contextuality kind's real deltas already sit close to its own
/// theoretical amplitude ceiling, the exact shape that made the shipped
/// `0.07` bound decorative.
///
/// `min_pooled_spread`/`min_total_occurs` were set with margin BELOW the
/// measured real values above (never above — a floor above the real
/// measurement would fail on real data by construction). **The Weft's
/// values, superseded for spring and overhang by the Warp paragraph below
/// and current only for thicket and erratic:** spring `0.05`/`50`, overhang
/// `0.08`/`150`, thicket `0.20`/`400`, erratic `0.04`/`100`. Overhang's and
/// spring's floors both still held against their corrected rows at the time
/// (overhang: `0.08` was 1.67x under `0.13333`, `150` was 1.83x under `275`;
/// spring: `0.05` was 2.25x under the corrected `0.11268`) — nothing broke
/// either time; the committed TABLE was false, not the bounds. Read the LIVE
/// values off the array itself, never off this paragraph.
///
/// ---
///
/// **THE WARP, Task 6 (2026-09-05) — re-measured at the frozen constants,
/// and two of the four rows moved for two different reasons.** Same pool,
/// same seed 42, same 78 walks / 4,680 facets / 4,602 adjacent pairs:
///
/// | kind | pooled spread | total occurs | lag-1 r |
/// | --- | --- | --- | --- |
/// | spring | **0.00000** (`[0.00000, 0.00000]`) | **0 / 4,680** | **undefined** |
/// | overhang | 0.46817 (`[0.00000, 0.46817]`) | 91 / 4,680 | 0.99994 |
/// | thicket | 0.39418 (`[0.00054, 0.39473]`) | 924 / 4,680 | 0.99994 |
/// | erratic | 0.07594 (`[0.00203, 0.07796]`) | 233 / 4,680 | 0.86795 |
///
/// **Thicket's and erratic's rows are byte-identical to the pre-Warp table
/// above** — they are the campaign's controls and their recipe is untouched
/// (spec §6.1), so this is the non-regression evidence, not a coincidence.
///
/// **Spring's prevalence is now IDENTICALLY ZERO over the whole sample, and
/// that is a property of the SAMPLE, not of the world.** Spring's soft step
/// opens at a cause of `0.35` (`SPRING_STEP_LO`), and the largest spring
/// cause anywhere in these 78 walks is about `0.244` — the pre-Warp figure
/// was reachable only because the Weft's recipe gave every kind an
/// unconditional noise floor, so spring's prevalence was nonzero on ground
/// with no karst at all. It is not that a walker never meets a seep: the
/// grid-band instrument (one facet per geosphere vertex, 11,218 land
/// facets) reads `weft-existence-density-spring = 0.01337`, one facet in 75.
/// This pool is 78 *locations* of 60 adjacent facets each, and a
/// regionally-clustered kind present on ~1.3% of locations is missed by 78
/// draws about a THIRD of the time (`(1 - 0.01337)^78 = 0.350`, as
/// `windows/lab/tests/suite/weft_density.rs` already computes for the same
/// pool). The grid band is where spring is
/// measured now (`windows/lab`'s `warp-*` family and
/// `weft-existence-density-spring`); this file's walk band has no power for
/// it, and the two zero floors in spring's row below say so honestly rather
/// than pretending to a coverage this instrument does not have. The WITNESS
/// in [`the_walk_is_not_degenerate`] is what keeps those zeros from being a
/// silent vacuous pass: it asserts the exact zero, so the moment spring
/// reappears in the walk band the test goes red and this row is re-derived.
///
/// **Overhang's row was re-derived at three values of `OVERHANG_RATE` in one
/// day, and the table above carries the FINAL one — take the numbers from
/// it, not from this paragraph's history.** Its floors move `0.08` → `0.25`
/// and `150` → `50`, each about 1.8x under the measured 0.46817 and 91,
/// which is the headroom philosophy the rows above use. Its pooled spread
/// more than trebled against the Weft's (0.13333 → 0.46817) while its
/// occurrence count fell by two thirds (275 → 91), and both are the soft
/// step's own signature: the response is exactly zero below `0.35` and
/// climbs to `rate` above `0.65`, so the pooled series now spans `[0, rate]`
/// rather than a lerp's narrow band, while far fewer facets carry any
/// prevalence at all.
///
/// The three readings, kept rather than overwritten, because together they
/// are the cleanest evidence available that this row tracks the RATE and
/// nothing else — the step edges, the pool, the seed and the population were
/// identical across all three, and spring's row is byte-identical across
/// them:
///
/// | rate | pooled spread | total occurs | lag-1 r |
/// | ---: | ---: | ---: | ---: |
/// | 0.16 | 0.14981 | 36 | 0.99994 |
/// | **0.50** (final) | **0.46817** | **91** | **0.99994** |
/// | 0.75 | 0.70225 | 103 | 0.99994 |
///
/// The 0.16 rung existed to satisfy an H2 between-kind clause withdrawn from
/// spec §7's gate the same day (ledger #11, 2026-09-05); 0.75 was the highest
/// rung holding overhang's own bands; 0.50 is the middle of the passing
/// range, chosen for H5 headroom and for the design reason in
/// `OVERHANG_RATE`'s own doc. Note that the occurrence count is NOT linear in
/// the rate — 36 → 91 → 103 across a 4.7x span — because the walk band's
/// facets sit mostly on the step's lower shoulder, where a higher ceiling
/// buys progressively less.
///
/// **One claim in the mutant table above no longer applies to spring or
/// overhang, and it is worth stating rather than leaving to inference.** A
/// sign kind's prevalence is `rate · smoothstep(cause) + floor · noise` with
/// `floor = 0.0`, so the position-continuous noise term is multiplied out of
/// it entirely — the address-hashing mutation the module doc describes
/// perturbs a term those two kinds no longer read, and cannot move their
/// numbers at all. What spring's and overhang's rows in [`AUTOCORR_BOUNDS`]
/// and in this table still discriminate is the continuity of the MACRO
/// STATE (the blended carbonate/drainage and induration/slope fields), which
/// is a real property and a different one. Thicket and erratic keep the
/// Weft's expression and keep the original guarantee unchanged.
const KIND_BOUNDS: [(WeftKind, f64, f64, usize); 4] = [
    // Spring's two floors are NOT READ while `SILENT_IN_THE_WALK_BAND`'s
    // exact-zero witness stands (that special case `continue`s before the
    // generic asserts). They are `0.0, 0` so that deleting the witness
    // cannot leave a live bound that is vacuously satisfied — re-derive both
    // from a fresh measurement in the same edit that removes the case.
    (WeftKind::Spring, 0.010, 0.0, 0),
    (WeftKind::Overhang, 0.045, 0.25, 50),
    (WeftKind::Thicket, 0.015, 0.20, 400),
    (WeftKind::Erratic, 0.060, 0.04, 100),
];

/// The Warp, Task 6 (2026-09-05): the kind whose two floors in
/// [`KIND_BOUNDS`] are zero, and whose lag-1 autocorrelation is therefore
/// undefined, because its prevalence is identically zero over this pool.
/// Named once here so the two tests that special-case it cannot drift apart,
/// and so that adding a second such kind is a deliberate edit rather than a
/// second copy of the same `if`. See [`KIND_BOUNDS`]'s own doc for the
/// measurement and why the walk band has no power for it.
const SILENT_IN_THE_WALK_BAND: WeftKind = WeftKind::Spring;

/// Per-kind `(kind, min_lag1_autocorrelation)` — the PRIMARY
/// decorrelated-noise discriminator fix round 1 added (C1). Pearson
/// correlation between `prevalence[i]` and `prevalence[i+1]`, pooled across
/// every pair in every walk [`land_eligible_walks`] returns for the kind
/// (4,602 pairs each). See this file's own module doc for the real-vs-mutant
/// table these thresholds sit between; each threshold below leaves
/// comparable margin on both sides of its own kind's pair (spring: real
/// `0.998` / mutant `0.209`, threshold `0.6`; overhang: real `0.982`
/// (re-measured, fix round 2, N2 — the shipped `0.983` was measured against
/// the pre-I2 slope constant and never re-run) / mutant `0.089`, threshold
/// `0.5`; thicket: real `0.99994` / mutant
/// `0.890`, threshold `0.95` — the tightest margin of the four, because
/// thicket's own high contextuality (`0.85`) means even fully decorrelated
/// noise is only 15% of the mixed signal, so the mutant's correlation stays
/// high too; erratic: real `0.868` / mutant `-0.025`, threshold `0.5` — the
/// widest margin of the four, and not a coincidence: erratic's near-zero
/// contextuality is exactly what makes this the right primary check for it).
///
/// **The Warp, Task 6 (2026-09-05).** Re-measured at the frozen constants:
/// overhang `0.98238` → `0.99994`, thicket and erratic unchanged
/// (`0.99994`, `0.86795`), and **spring is now undefined** — its prevalence
/// is a constant zero over this pool, so the Pearson denominator is zero and
/// the statistic reads `NaN`. Spring's threshold below is retained at `0.6`
/// and is NOT read while that holds; the test asserts the constant-zero
/// series directly instead, which is a stronger statement than a correlation
/// bound and goes red the moment the series stops being constant. See
/// [`KIND_BOUNDS`]'s Warp paragraph for the measurement, for why the walk
/// band has no power for spring, and for the separate fact that a zero floor
/// takes the address-hashable noise term out of both sign kinds' prevalence
/// altogether.
const AUTOCORR_BOUNDS: [(WeftKind, f64); 4] = [
    // NOT READ while `SILENT_IN_THE_WALK_BAND`'s constant-zero arm stands:
    // spring's series has no variance, so its correlation is `NaN` and this
    // threshold is never compared against. Retained at the Weft's value so
    // removing that arm restores a real bound rather than an invented one —
    // but re-measure before trusting it.
    (WeftKind::Spring, 0.6),
    (WeftKind::Overhang, 0.5),
    (WeftKind::Thicket, 0.95),
    (WeftKind::Erratic, 0.5),
];

/// A caller that already paid for a facet's corner weights must be able to
/// reuse that exact prepared value across every kind without changing any
/// prevalence bit. This is the behavioral oracle for the prepared-weight
/// seam the grid pool uses; the pool owns one `weights` value per facet,
/// while the public wrapper remains available to callers that own only
/// geometry.
///
/// The sample admits only land-eligible facets and requires at least one
/// nonzero answer, so an implementation returning the eligibility sentinel
/// for every kind cannot pass vacuously.
#[test]
fn prepared_weights_preserve_every_kinds_prevalence_bits() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;

    let mut eligible_facets = 0usize;
    let mut nonzero_answers = 0usize;
    for v in 0..geo.vertex_count() {
        let facet = Facet::containing(geo.position(Vertex(v as u32)), walk_depth);
        let Some(weights) = facet.corner_weights(geo, &index) else {
            continue;
        };
        if blend_corner_weights(weights, &pack.land) < 0.5 {
            continue;
        }

        eligible_facets += 1;
        for kind in WeftKind::ALL {
            let wrapped =
                hornvale_worldgen::prevalence(kind, &facet, geo, &index, &pack, world.seed)
                    .expect("corner_weights just returned Some above");
            let prepared = hornvale_worldgen::prevalence_with_weights(
                kind, &facet, weights, &pack, world.seed,
            );
            assert_eq!(
                prepared.to_bits(),
                wrapped.to_bits(),
                "{kind:?}: prepared weights changed prevalence at vertex {v}"
            );
            nonzero_answers += usize::from(wrapped != 0.0);
        }

        if eligible_facets == 32 {
            break;
        }
    }

    assert!(
        eligible_facets > 0,
        "the sample found no land-eligible facet, so it exercised nothing"
    );
    assert!(
        nonzero_answers > 0,
        "every prepared prevalence was the eligibility sentinel, so the comparison was vacuous"
    );
}

/// Adjacent facets mostly agree, because prevalence is position-continuous —
/// checked across every walk [`land_eligible_walks`] returns, for every kind
/// in [`KIND_BOUNDS`]. The secondary check, since fix round 1 — see
/// [`AUTOCORR_BOUNDS`] and this file's module doc for the primary one.
///
/// **Why this discriminates an address-hashed implementation, measured, not
/// assumed.** During Task 5, `prevalence`'s noise sample was temporarily
/// mutated in place — `facet.centroid()` swapped for
/// `facet.seed(seed).stream().next_f64()`, everything else untouched — and
/// this exact shape of test re-run against it; fix round 1 re-ran the same
/// mutation against the broad sample this file now uses (see
/// [`KIND_BOUNDS`]'s own doc for the resulting real/mutant max deltas).
///
/// **Round 0 measured `SPRING_CONTEXTUALITY = 0.7` against a raw (non-
/// uniformized) noise field: real max delta `~0.0025`, mutant max `~0.10`.**
/// Review found this comparison unsound: at the round-0 shipped bound
/// (`0.02`), the ORIGINAL `0.85` also discriminates (mutant: 64 of 199 steps
/// over bound) — moving `SPRING_CONTEXTUALITY` bought no additional
/// separation (42.6× at 0.85 vs 38.7× at 0.7, scale-invariant since lowering
/// contextuality scales both sides identically); the defect was a bound set
/// too loose, not the constant. `SPRING_CONTEXTUALITY` was restored to
/// `0.85` — and The Warp has since deleted the constant outright, spring
/// being a sign kind whose cause is read through a soft step rather than a
/// contextuality lerp. The lesson (decision 0016's forbidden shape: a world
/// parameter retuned to rescue a miscalibrated measurement) is what this
/// paragraph is kept for; the constant it names no longer exists.
///
/// **Round 1 also fixed F1: every noise sample is now passed through
/// [`hornvale_terrain::features::uniformize`]** before use, which widens the
/// noise term's own variance (from SD ≈0.076 raw to a genuine `[0,1]`
/// uniform).
#[test]
fn prevalence_is_continuous_across_adjacent_facets() {
    for (kind, max_allowed_delta, _, _) in KIND_BOUNDS {
        let walks = land_eligible_walks(kind);
        assert!(
            !walks.is_empty(),
            "{kind:?}: no land-eligible walk found in the sample"
        );

        let mut violations = Vec::new();
        for (w, walk) in walks.iter().enumerate() {
            let prevalences: Vec<f64> = walk.iter().map(|(p, _)| *p).collect();
            for i in 1..prevalences.len() {
                let delta = (prevalences[i] - prevalences[i - 1]).abs();
                if delta > max_allowed_delta {
                    violations.push((w, i, delta));
                }
            }
        }
        assert!(
            violations.is_empty(),
            "{kind:?}: prevalence must stay position-continuous across adjacent facets; \
             {} step(s) over {} walks exceeded {max_allowed_delta}: {violations:?}",
            violations.len(),
            walks.len(),
        );
    }
}

/// The PRIMARY decorrelated-noise discriminator since fix round 1 (C1) — see
/// this file's own module doc for the real-vs-mutant measurement table and
/// [`AUTOCORR_BOUNDS`] for the per-kind thresholds. Lag-1 Pearson
/// correlation, pooled over every adjacent pair in every walk
/// [`land_eligible_walks`] returns.
///
/// **Verified by mutation on this tree** (the same in-place swap described
/// in [`prevalence_is_continuous_across_adjacent_facets`]'s own doc): every
/// kind's mutant `r` falls below its [`AUTOCORR_BOUNDS`] threshold —
/// including erratic's, which the max-delta bound alone could not reliably
/// catch (0.5% mutant-exceedance at the shipped `0.07` bound, per the
/// review). Reverted after measuring; this assertion is what pins the
/// property going forward, not the one-off measurement.
#[test]
fn prevalence_autocorrelation_is_not_address_hashed() {
    for (kind, min_r) in AUTOCORR_BOUNDS {
        let walks = land_eligible_walks(kind);
        assert!(
            !walks.is_empty(),
            "{kind:?}: no land-eligible walk found in the sample"
        );

        let mut xs: Vec<f64> = Vec::new();
        let mut ys: Vec<f64> = Vec::new();
        for walk in &walks {
            for i in 1..walk.len() {
                xs.push(walk[i - 1].0);
                ys.push(walk[i].0);
            }
        }

        let m = xs.len() as f64;
        let mean_x = xs.iter().sum::<f64>() / m;
        let mean_y = ys.iter().sum::<f64>() / m;
        let cov: f64 = xs
            .iter()
            .zip(ys.iter())
            .map(|(x, y)| (x - mean_x) * (y - mean_y))
            .sum::<f64>()
            / m;
        let var_x: f64 = xs.iter().map(|x| (x - mean_x).powi(2)).sum::<f64>() / m;
        let var_y: f64 = ys.iter().map(|y| (y - mean_y).powi(2)).sum::<f64>() / m;
        let r = cov / (var_x.sqrt() * var_y.sqrt());

        // THE WARP, Task 6 (2026-09-05): spring's series is a constant zero
        // over this pool, so the correlation's denominator is zero and `r`
        // is `NaN`. Assert the constant directly rather than a bound that
        // cannot be evaluated — a `NaN >= 0.6` comparison is `false`, so
        // leaving the generic arm to run would report "autocorrelation too
        // low" for a series that has no autocorrelation to be low. The
        // relation that moved is named in `KIND_BOUNDS`'s own Warp
        // paragraph: spring's step opens at a cause of 0.35 and this pool's
        // largest spring cause is ~0.244.
        if kind == SILENT_IN_THE_WALK_BAND {
            assert_eq!(
                (var_x, var_y),
                (0.0, 0.0),
                "{kind:?}: the walk band's prevalence series is no longer the constant zero \
                 the Warp measured (r={r}) — re-derive this arm and KIND_BOUNDS' spring row \
                 together, and delete the special case if the band can see the kind again"
            );
            assert!(
                xs.iter().chain(ys.iter()).all(|v| *v == 0.0),
                "{kind:?}: a constant, non-zero prevalence series is not what the Warp \
                 measured — it measured an exact zero everywhere off the kind's own cause"
            );
            continue;
        }

        assert!(
            r >= min_r,
            "{kind:?}: lag-1 autocorrelation must stay high — position-continuous noise \
             correlates with its own neighbour, address-hashed noise does not; measured \
             r={r:.5} over {} pairs, wanted >= {min_r}",
            xs.len(),
        );
    }
}

/// The anti-vacuity companion spec §7's H2 requires (F3, fix round 1):
/// `prevalence_is_continuous_across_adjacent_facets` alone passes for a
/// **constant** function — `violations.is_empty()` on a flat series is
/// vacuously true. Checked for every kind in [`KIND_BOUNDS`], over the same
/// broad sample [`land_eligible_walks`] returns, pooled (spread) and summed
/// (occurs) across every walk — never a single walk's own floor, closing M3.
#[test]
fn the_walk_is_not_degenerate() {
    for (kind, _, min_pooled_spread, min_total_occurs) in KIND_BOUNDS {
        let walks = land_eligible_walks(kind);
        assert!(
            !walks.is_empty(),
            "{kind:?}: no land-eligible walk found in the sample"
        );

        let mut min = f64::INFINITY;
        let mut max = f64::NEG_INFINITY;
        let mut occurs_count = 0usize;
        let mut total = 0usize;
        for walk in &walks {
            for &(p, occ) in walk {
                min = min.min(p);
                max = max.max(p);
                total += 1;
                if occ {
                    occurs_count += 1;
                }
            }
        }
        let spread = max - min;

        // THE WARP, Task 6 (2026-09-05) — the witness that keeps spring's
        // two ZERO floors in `KIND_BOUNDS` from being a vacuous pass. A
        // floor of zero can only ever be satisfied; this asserts the exact
        // measurement the zeros stand for, so the row goes red (and is
        // re-derived, with the special case deleted) the moment the walk
        // band can see the kind again.
        if kind == SILENT_IN_THE_WALK_BAND {
            assert_eq!(
                (min, max, occurs_count),
                (0.0, 0.0, 0),
                "{kind:?}: the walk band now sees this kind ({occurs_count} occurrences over \
                 {total} facets, prevalence in [{min}, {max}]) — the Warp measured an exact \
                 zero. Re-derive KIND_BOUNDS' row from this measurement and remove the \
                 special case; see that constant's Warp paragraph."
            );
            continue;
        }

        assert!(
            spread >= min_pooled_spread,
            "{kind:?}: prevalence must actually vary over the sample, not sit near a constant \
             floor; pooled spread was {spread:.5} (min {min:.5}, max {max:.5}) over {total} \
             facets, wanted >= {min_pooled_spread}"
        );

        assert!(
            occurs_count >= min_total_occurs,
            "{kind:?}: occurs must fire at least {min_total_occurs} time(s) over the \
             {total}-facet sample of real terrain; it fired {occurs_count} times — a \
             mechanism that never fires produces an empty surface regardless of how \
             smoothly prevalence behaves"
        );
    }
}

/// The eligibility regression (Task 7, controller ruling R1) — the
/// deliverable that number is, not the code: Task 5's review measured **59%
/// of all spring occurrences landing on facets with `spring_macro_state ==
/// 0.0`** (654 of 1109, over every seed-42 walk-depth facet across all
/// 40,962 vertices — a per-kind mathematical population, "zero-macro", not
/// "ocean" or "off-land"; see [`land_eligible_walks`]'s module-doc-adjacent
/// discussion of the three populations, and fix round 1's M1 correction for
/// why the distinction matters). On THAT population, at that time, the
/// three nearly coincided: the review separately measured zero-macro
/// restricted to land at only 60 facets (0.58% of land), so the 27,645-facet
/// zero-macro population (67.5% of the sphere) was overwhelmingly ocean.
///
/// This test reproduces the identical measurement — same population, same
/// definition of "causeless" (recomputed from `blend_corner_weights` over
/// `pack.carbonate`/`pack.drainage`, mirroring spring/seep's own
/// `pub(crate)` recipe exactly, rather than reading it directly, the same
/// posture Task 5's re-review probe used) — against the CURRENT (post-fix)
/// mechanism, and ALSO checks the geographic claim directly
/// (`off_land_occurs_n`, `land < 0.5`) rather than only the mathematical
/// proxy, so a regression that reopens the ocean case wholesale fails on
/// the literal claim, not just a correlated statistic.
///
/// **Measured here (fix round 2, re-confirmed): `occurs_n=403`,
/// `causeless_occurs_n=0`, `off_land_occurs_n=0`** (seed 42, full grid) —
/// down from 654 of 1109 (58.97%) causeless before R1. `off_land_occurs_n`
/// is asserted `== 0` exactly (R1's literal requirement: spring must never
/// occur off land, full stop). `causeless_share` is asserted `<= 5%` rather
/// than `== 0` on purpose: the bound leaves headroom for a legitimately
/// LAND-based zero-macro occurrence (bare rock with no drainage still has a
/// nonzero prevalence floor via `(1 - contextuality) * noise`; Task 5's
/// review measured that land-only zero-macro slice at 0.58% of land,
/// contributing ~1 of the original 654) — a real, physically sensible case
/// the eligibility gate must NOT suppress, since R1 only forbids OCEAN
/// occurrences, not rare land ones with no drainage/carbonate signal. `5%`
/// sits two orders of magnitude above the measured `0%` and one order above
/// what a fully reopened ocean case would produce (58.97%), so it is a real
/// regression bound, not a rubber stamp.
#[test]
fn spring_never_occurs_off_land() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;
    let n = geo.vertex_count();

    /// Spring/seep's own macro-state recipe, recomputed here rather than
    /// read from the crate's `pub(crate)` `spring_macro_state` (this file is
    /// an external integration-test crate and cannot see it) — mirrors
    /// `kinds.rs`'s `spring_macro_state` exactly: `SPRING_DRAINAGE_SATURATION
    /// = 12.0`.
    fn spring_macro(weights: [(Vertex, u64); 4], pack: &hornvale_worldgen::FieldPack) -> f64 {
        let carbonate = blend_corner_weights(weights, &pack.carbonate);
        let drainage = blend_corner_weights(weights, &pack.drainage);
        (carbonate * (drainage / 12.0).tanh()).clamp(0.0, 1.0)
    }

    let mut occurs_n = 0usize;
    let mut causeless_occurs_n = 0usize;
    let mut off_land_occurs_n = 0usize;
    for v in 0..n {
        let facet = Facet::containing(geo.position(Vertex(v as u32)), walk_depth);
        let weights = facet
            .corner_weights(geo, &index)
            .expect("a level-0 walk-depth facet always has corner weights");
        let land = blend_corner_weights(weights, &pack.land);
        let macro_state = spring_macro(weights, &pack);

        let p =
            hornvale_worldgen::prevalence(WeftKind::Spring, &facet, geo, &index, &pack, world.seed)
                .expect("a level-0 walk-depth facet always has corner weights");
        let occ = hornvale_worldgen::occurs(WeftKind::Spring, &facet, world.seed, p);
        if occ {
            occurs_n += 1;
            if macro_state == 0.0 {
                causeless_occurs_n += 1;
            }
            if land < 0.5 {
                off_land_occurs_n += 1;
            }
        }
    }

    assert!(
        occurs_n > 0,
        "spring must occur somewhere over the full seed-42 grid, or this test measures nothing"
    );
    assert_eq!(
        off_land_occurs_n, 0,
        "spring must never occur on ineligible (majority-ocean) ground — R1's whole point; \
         {off_land_occurs_n} of {occurs_n} occurrences did"
    );

    let causeless_share = causeless_occurs_n as f64 / occurs_n as f64;
    assert!(
        causeless_share <= 0.05,
        "causeless-occurrence share must stay near zero (measured 0/403 at fix time); \
         got {causeless_occurs_n}/{occurs_n} = {:.4}% (bound 5%)",
        causeless_share * 100.0
    );
}

/// Per-kind non-vacuity floors for
/// [`a_sign_kind_with_a_zero_floor_is_silent_below_its_lower_step_edge`] —
/// the number of LAND-ELIGIBLE facets whose cause sits at or below the
/// kind's own lower step edge, so the kind's response there is exactly zero
/// (fix round 1).
///
/// **MEASURED on seed 42's full walk-depth grid, not assumed** (11,218
/// land-eligible facets of 40,962 vertices, this tree, the provisional
/// Task 4 step edges): **spring 10,600, overhang 6,044**. The floors below
/// sit an order of magnitude under each measurement on purpose — this is a
/// non-vacuity guard, not a ratchet on the number. Every member of the
/// population is individually asserted, so the count's only job is to prove
/// the kind HAS a population of its own; Task 6 moves the step edges, which
/// will move both measurements, and a floor set near today's value would
/// redden for a reason that has nothing to do with the property.
const ZERO_RESPONSE_FLOORS: [(WeftKind, usize); 2] =
    [(WeftKind::Spring, 1_000), (WeftKind::Overhang, 1_000)];

/// The Warp, Task 4 — a sign kind with a zero floor is EXACTLY silent
/// wherever its response is zero: the honest-silence half of spec §6,
/// asserted on the MECHANISM (floor = 0 and response = 0 ⇒ prevalence = 0 ⇒
/// occurs = false) rather than on any authored value. `floor()` and
/// `step_edges()` are read here rather than assumed, so if Task 6's
/// calibration lifts a sign kind's floor off zero this test skips that kind
/// instead of failing on a number it never named.
///
/// **The population is LAND-ELIGIBLE facets only, and counted PER KIND**
/// (fix round 1). Without the filter the guard was a count and not a
/// membership: [`hornvale_worldgen::prevalence_with_weights`] returns `0.0`
/// at its eligibility gate *before* any macro-state read, so every ocean
/// facet satisfies both assertions through a completely different
/// mechanism — the one [`spring_never_occurs_off_land`] already pins — and
/// nothing established that a single land facet was in the sample.
/// Per-kind counting closes the other half: a pooled count can be carried
/// entirely by one kind while the other's claim goes untested.
///
/// **The population is "cause at or below the lower step edge", NOT "cause
/// exactly zero", and the reason is a measurement.** The first version of
/// this test asked for `macro_state == 0.0`, and on seed 42's land that set
/// is **EMPTY for both sign kinds — 0 of 11,218 land-eligible facets, for
/// spring and for overhang alike** (measured on this tree, fix round 1).
/// Both causes are products of blended corner values
/// (`carbonate × tanh(drainage/…)`, `induration × tanh(slope/…)`), and a
/// bilinear blend of four real corners is essentially never exactly `0.0`
/// on dry ground; the exact zeros all live in the ocean, which the
/// eligibility filter above — correctly — removes. So the strict form is
/// not merely weak, it is vacuous on the real subject, and a vacuous test
/// that reads green is worse than an absent one. `cause ≤ lo` is the
/// honest statement of the same mechanism: [`smoothstep`]'s clamp makes the
/// response EXACTLY `0.0` there (not asymptotically small), which is
/// precisely what a zero floor turns into literal silence, and `cause == 0`
/// is a sub-case of it. See [`ZERO_RESPONSE_FLOORS`] for the measured
/// populations.
///
/// The land test is spelled out here rather than calling
/// `WeftKind::eligible`, which is `pub(crate)` and invisible to this
/// external test crate — the same one-line replication
/// [`land_eligible_walks`] and
/// [`prepared_weights_preserve_every_kinds_prevalence_bits`] already make,
/// chosen over widening a crate-private accessor for a test's convenience.
///
/// Walks every vertex, not every third: once ocean is excluded the sample
/// is worth having whole, and the full grid costs under half a second here
/// (the control golden walks the same one).
///
/// Builds its world inline, the same posture every other test in this file
/// takes (decision 0092's sanctioned test fixture).
#[test]
fn a_sign_kind_with_a_zero_floor_is_silent_below_its_lower_step_edge() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;

    let mut silent = [0usize; 2];
    let mut land_facets = 0usize;
    for v in 0..geo.vertex_count() {
        let facet = Facet::containing(geo.position(Vertex(v as u32)), walk_depth);
        let Some(weights) = facet.corner_weights(geo, &index) else {
            continue;
        };
        if blend_corner_weights(weights, &pack.land) < 0.5 {
            continue;
        }
        land_facets += 1;
        for (i, (kind, _)) in ZERO_RESPONSE_FLOORS.into_iter().enumerate() {
            if kind.floor() != 0.0 {
                continue;
            }
            let cause = kind.macro_state(weights, &pack);
            let (lo, _) = kind.step_edges();
            if cause > lo {
                continue;
            }
            silent[i] += 1;
            assert_eq!(
                kind.response(cause),
                0.0,
                "{kind:?} at vertex {v}: cause {cause} is at or below the lower edge {lo}, \
                 so the response must be exactly zero"
            );
            let p = hornvale_worldgen::prevalence_with_weights(
                kind, &facet, weights, &pack, world.seed,
            );
            assert_eq!(
                p, 0.0,
                "{kind:?} at vertex {v}: zero response, zero floor, nonzero prevalence {p}"
            );
            assert!(
                !hornvale_worldgen::occurs(kind, &facet, world.seed, p),
                "{kind:?} at vertex {v}: occurred against a zero prevalence"
            );
        }
    }

    for (i, (kind, floor)) in ZERO_RESPONSE_FLOORS.into_iter().enumerate() {
        assert!(
            silent[i] >= floor,
            "fixture check: {kind:?} had {} land-eligible facet(s) at or below its lower \
             step edge, over {land_facets} land facets, wanted >= {floor} — without a \
             population of its OWN this kind's honest-silence claim is untested (measured \
             at fix time: spring 10,600, overhang 6,044). Counts this run: {:?}",
            silent[i],
            ZERO_RESPONSE_FLOORS
                .iter()
                .map(|(k, _)| *k)
                .zip(silent)
                .collect::<Vec<_>>(),
        );
    }
}

/// The response is a soft step on the cause: `0` below `lo`, `1` above
/// `hi`, monotone between, and IDENTITY for the two control kinds — the
/// shape of the step, never a calibrated edge value (Task 6 sets those).
#[test]
fn the_response_is_a_step_for_sign_kinds_and_identity_for_controls() {
    for kind in [WeftKind::Spring, WeftKind::Overhang] {
        let (lo, hi) = kind.step_edges();
        assert!(
            (0.0..1.0).contains(&lo) && lo < hi && hi <= 1.0,
            "{kind:?} edges {lo} {hi} must satisfy 0 <= lo < hi <= 1"
        );
        assert_eq!(
            kind.response(lo - 0.01),
            0.0,
            "{kind:?}: the response must be exactly zero below its lower edge"
        );
        assert_eq!(
            kind.response(hi + 0.01),
            1.0,
            "{kind:?}: the response must saturate at one above its upper edge"
        );
        let mut last = 0.0;
        for i in 0..=100 {
            let r = kind.response(f64::from(i) / 100.0);
            assert!(r >= last, "{kind:?}: response fell at {i}, {r} < {last}");
            last = r;
        }
    }

    for kind in [WeftKind::Thicket, WeftKind::Erratic] {
        for i in 0..=100 {
            let x = f64::from(i) / 100.0;
            assert_eq!(
                kind.response(x),
                x,
                "{kind:?} must be identity — it is a control"
            );
        }
    }
}
