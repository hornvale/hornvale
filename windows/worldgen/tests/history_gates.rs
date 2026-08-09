//! The living-community campaign's **preregistered measurement gates** (Task 6)
//! — the measure-don't-narrate payoff check. These prove (or falsify) that
//! history-first placement delivered. Each threshold is frozen as a `const`
//! set comfortably clear of the *measured* seed-42 value: a floor that proves
//! the phenomenon fired, never a target tuned to force a pass.
//!
//! These are the LIGHT, in-`make gate` assertions on the real seed-42 world
//! built to `BuildDepth::Settlements` (~1.3 s — the same build the sibling
//! `history_placement.rs` gates already pay for). The heavy full-cascade +
//! cross-seed robustness battery, which also regenerates the committed report
//! artifact, is `cli/tests/history_battery.rs`.
//!
//! ## Two honest post-data amendments (labelled, per measure-don't-narrate)
//!
//! 1. **Displacement is MIGRATION, not raiding.** The campaign was
//!    preregistered around a raid→flee→resettle floor (`fled + resettled`).
//!    The epoch showed that on the *real* seed-42 world — ample vacant
//!    habitable land — glacially-displaced communities *migrate to empty
//!    cells* rather than crowd into raids: raids ≈ 0. So the displacement gate
//!    is re-pointed at the signal that genuinely fires, `census(bake).migrated`
//!    (read off the ledger as `migration_events`). Raid-driven displacement is
//!    deferred to campaign C3.
//!
//! 2. **Stratigraphy accretes on MARGINAL land, not prime land.** The
//!    preregistered stratigraphy sub-hypothesis was that layer depth correlates
//!    *positively* with capacity. The data FALSIFIES this: the correlation is
//!    robustly *negative* (seed-42 ≈ -0.35; negative on every sampled seed; a
//!    one-time reconstruction of the true carrying-capacity field agrees, so it
//!    is not a proxy artifact). Re-occupation stacks form on contested,
//!    climate-volatile land that is repeatedly abandoned and resettled, while
//!    prime cells are settled once and simply persist. The gate asserts the
//!    measured reality (negative coupling), and the falsification is the real
//!    finding — recorded, not buried.
//!
//! 3. **The moving sea corrected the migration count 51 → 12 (spec §7
//!    re-scope).** The Sundering rerouted the bake onto a time-varying
//!    connection graph — one per era, ocean where `elevation < sea_level` — so
//!    a community can only step across cells that era's sea level leaves as
//!    land. The pre-Sundering static count (51) was inflated by unphysical
//!    ocean-walking: the raw-mesh BFS strode straight across open ocean. The
//!    moving-sea graph removes those illegitimate strides, so seed-42 now
//!    measures 12 migrations (and 2 re-stacks, was 6). This is a correction,
//!    NOT a regression: displacement still FIRES, and 11 of the 12 migrations
//!    cross water only passable at the era they occur — they genuinely ride the
//!    glacial land-bridges and sailing lanes. The campaign's headline payoff
//!    accordingly moves to **isolation-predicts-divergence**
//!    (`history_sundering.rs`): sundered peoples drift apart. Raising the
//!    diaspora VOLUME (more settled peoples displaced) is a crowding/pressure
//!    matter deferred to campaign C3, not a Sundering deliverable. The floors
//!    below are re-scoped clear of the corrected values.
//!
//! 4. **C3 (The Tumult) arrived, so amendments 1 and 3 above are HISTORY, not
//!    current state.** They are kept as the record of why the gates point
//!    where they do; read them as of their own campaigns. What has changed:
//!    raids are no longer ≈ 0 (seed 42 measures 76 conquests, where the
//!    pre-Tumult bake had none), and climate migration is no longer 12 (it
//!    measures 58 — conquest re-seats communities onto new ground and so
//!    exposes far more of them to a later era's habitability flip). The two
//!    signals are deliberately kept apart: `migration_events` counts CLIMATE
//!    displacement only, conflict displacement is the bake's `raided`/`fled`
//!    and the cascade histogram (`history_tumult.rs`). Neither floor below
//!    moved; both were set clear of a cross-seed minimum, and both still are.
//!
//! 5. **The Tense re-based gate 1 onto a seed SPREAD.** Capacity gained a time
//!    axis, so displacement now scales with how much a given world's climate
//!    actually moves rather than firing wholesale on every era flip. The
//!    per-world spread is consequently enormous (seed 42 measures 4, seed 1234
//!    measures 311), and a floor read off one world can no longer tell a mild
//!    deep past from a dead bake. Gate 1 alone is therefore cross-seed; the
//!    others still read seed 42, and the header above applies to them.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, GOBLINOIDS, SettlementPins, SkyChoice, WorldComponents, build_world_to, census,
    goblinoid_region_overlap, history_for, migration_events, stratigraphy, territories,
};

// ---- Frozen thresholds (set BELOW the measured values) --------------------
//
// Gates 2 and 3a are read off seed 42. Gate 1 is NOT, since The Tense — see
// its comment for why a single world can no longer distinguish a mild deep
// past from an inert bake.

/// Gate 1. A run below this floor means climate-driven displacement went inert
/// (the campaign's STOP condition). Read across [`MIGRATION_SEEDS`], not off
/// seed 42.
// The Sundering (moving sea) re-scope: the static 51 was inflated by
// unphysical ocean-walking (the raw-mesh BFS strode across open ocean); the
// moving-sea graph corrects it, and seed 42 measured 12. Displacement still
// FIRES and rides the era-bridges; that campaign's headline payoff moved to
// isolation-predicts-divergence (history_sundering.rs).
//
// The Tumult (predation) re-pin: seed 42 now measures 58 climate migrations —
// UP from the Sundering's 12, because conquest re-seats communities onto new
// cells and so exposes many more of them to a later era's habitability flip.
// The floor is deliberately NOT raised to track it: 5 was set clear of the
// cross-seed minimum, not of seed 42, and this campaign's readout (Task 3) is
// what re-measures the sample. Note the raw `occ-cause = migrated` fact count
// on this world is 133, not 58 — 75 of those are conquest-relocations, which
// `migration_events` now excludes by design (see its doc comment, and the
// `migration_events_counts_climate_displacement_only` gate below).
// The Tense re-base (2026-08-05, Nathan's call): the floor is now read across a
// SPREAD, not off seed 42. Two reasons, one old and one new.
//
// The old one is written three lines up and was never acted on: "5 was set
// clear of the CROSS-SEED MINIMUM, not of seed 42". A cross-seed threshold
// applied to a single world is a category error — it was only ever survivable
// while seed 42 happened to sit well above it.
//
// The new one is that The Tense made displacement genuinely world-dependent.
// Migration now scales with how much a world's climate actually MOVES, because
// capacity varies by era instead of a binary mask displacing everyone on every
// era flip. Seed 42's deep past is mild and it measures 4; seed 1234's is harsh
// and it measures 311. Neither is a defect, and no single-seed floor can tell
// the difference between a mild world and an inert bake — which is exactly what
// a floor of 5 read off seed 42 was about to claim.
//
// So the gate asserts the two things that ARE general: the mechanism produces
// displacement in volume somewhere, and it does so on more than one world.
//
// MEASURED at the re-base (`migration_events` per seed, BuildDepth::Settlements):
//
//   42:4   7:1   999_999:3   16_244_526_067_196_353_746:7   1234:311
//   total 326, firing 5 of 5
//
// The floor is a STOP condition — "displacement went inert" — not a pin, so it
// sits an order of magnitude below the measured total rather than just beneath
// it: 25 is 13x under 326 and still an order of magnitude above the handful of
// stragglers a genuinely dead bake would leave. The distribution is extremely
// skewed (one world carries 95% of the total), which is a real property of the
// new physics and not noise — MIGRATION_MIN_FIRING_SEEDS is what stops that one
// world from satisfying the gate alone.
const MIGRATION_FLOOR: u64 = 25;

/// The spread the floor is read across — the same five worlds
/// `history_shape_probe.rs` profiles, so the two instruments sample the same
/// sample. Deliberately spans a mild deep past (42) and a harsh one (1234).
const MIGRATION_SEEDS: [u64; 5] = [42, 7, 999_999, 16_244_526_067_196_353_746, 1234];

/// How many of [`MIGRATION_SEEDS`] must show any displacement at all. Guards
/// the failure mode a bare total cannot see: one harsh world carrying the whole
/// sum while the mechanism is dead everywhere else.
const MIGRATION_MIN_FIRING_SEEDS: usize = 3;

/// Gate 2. Seed-42 measured 0.0466 region overlap under the moving sea (0.055
/// pre-Sundering; raw cell-set overlap is a structural 0). A world above this
/// ceiling has interleaved, not separated,
/// peoples — the diversity payoff would have failed.
const MAX_REGION_OVERLAP: f64 = 0.25;

/// Gate 3a. Below this floor no stratigraphy emerged at all.
// The Sundering (moving sea) re-scope: seed-42 now measures 2 (was 6). Fewer
// relocations (migration 51→12) ⇒ fewer re-stacks; the depth/capacity NEGATIVE-
// correlation finding still holds at -0.2049.
const MIN_RESTACKED_SITES: u64 = 1;

fn build(seed: Seed, depth: BuildDepth) -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        depth,
    )
    .expect("seed builds")
}

/// Gate 1 — **migration-fired-at-volume.** Climate-driven displacement must
/// genuinely fire on the real world, or the whole deep-history bake is inert.
/// (Post-data amendment: MIGRATION replaces the preregistered raid-based
/// `fled + resettled` floor — see the module docs; raids are deferred to C3.)
/// claim: rate(forall-seed, total >= MIGRATION_FLOOR AND firing >=
/// MIGRATION_MIN_FIRING_SEEDS) — over MIGRATION_SEEDS
#[test]
fn migration_fires_at_volume() {
    let counts: Vec<(u64, u64)> = MIGRATION_SEEDS
        .iter()
        .map(|&s| {
            (
                s,
                migration_events(&build(Seed(s), BuildDepth::Settlements)),
            )
        })
        .collect();
    let table = || {
        counts
            .iter()
            .map(|(s, m)| format!("{s}:{m}"))
            .collect::<Vec<_>>()
            .join(" ")
    };
    let total: u64 = counts.iter().map(|(_, m)| m).sum();
    let firing = counts.iter().filter(|(_, m)| *m > 0).count();

    assert!(
        total >= MIGRATION_FLOOR,
        "displacement went inert across the spread: {total} migration events \
         over {} seeds (floor {MIGRATION_FLOOR}) — [{}]. The paleoclimate era \
         swing is not displacing communities — re-tune the bake before \
         trusting the placement.",
        MIGRATION_SEEDS.len(),
        table()
    );
    assert!(
        firing >= MIGRATION_MIN_FIRING_SEEDS,
        "displacement fired on only {firing} of {} seeds (need \
         {MIGRATION_MIN_FIRING_SEEDS}) — [{}]. A single world carrying the \
         whole total would mean the mechanism is not general.",
        MIGRATION_SEEDS.len(),
        table()
    );
}

/// Gate 1b — **`migration_events` counts CLIMATE displacement only.** The
/// query's whole contract: it must equal `census(bake).migrated`, the bake's
/// own climate-eviction tally. It is not free to: a conquest also closes the
/// conqueror's abandoned record with cause `migrated` (`Bake::maybe_raid`
/// leaves its poor land for the prize), so the naive
/// "count every `occ-cause = migrated` fact" reading folds predation into the
/// climate signal. `migration_events` excludes those; this asserts it does.
///
/// The world under test genuinely contains BOTH kinds of displacement — the
/// `raided > 0` guard below is what makes the equality bind rather than pass
/// vacuously on a world where nothing was ever conquered. Deleting the
/// exclusion reddens this test.
#[test]
fn migration_events_counts_climate_displacement_only() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let h = history_for(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("seed-42 bakes");
    let c = census(&h);
    assert!(
        c.raided > 0,
        "seed 42 resolved no conquest at all ({c:?}) — this gate would pass \
         vacuously, since there would be no conquest-relocation for the query \
         to exclude. Re-point it at a seed that fights."
    );
    let w = build(Seed(42), BuildDepth::Settlements);
    assert_eq!(
        migration_events(&w),
        c.migrated,
        "migration_events disagrees with the bake's own climate-eviction tally \
         ({} vs {}) on a world with {} conquests — the ledger query is counting \
         conquest-relocations (which close the conqueror's record with cause \
         `migrated`) as climate migrations.",
        migration_events(&w),
        c.migrated,
        c.raided
    );
}

/// Gate 2 — **territories-separated.** THE peoples-diversity payoff: the four
/// goblinoids must occupy measurably distinct regions. Measured on their
/// regions of influence (occupied cells dilated by one neighbour ring), since
/// the raw alive-cell sets are structurally disjoint and their Jaccard is a
/// vacuous 0.
/// claim: structural(seed: 42)
#[test]
fn territories_are_separated() {
    let w = build(Seed(42), BuildDepth::Settlements);

    // Guard against a false pass: an empty people would trivially not overlap.
    let terr = territories(&w);
    for k in GOBLINOIDS {
        let n = terr.get(&k).map(|s| s.len()).unwrap_or(0);
        assert!(
            n > 0,
            "people {} holds no territory — an empty set would make the overlap \
             metric a false pass",
            k.0
        );
    }

    let overlap = goblinoid_region_overlap(&w);
    assert!(
        overlap < MAX_REGION_OVERLAP,
        "peoples did NOT separate: mean pairwise region overlap {overlap:.4} \
         exceeds the ceiling {MAX_REGION_OVERLAP} — the goblinoids are interleaved, \
         not distinct countries."
    );
}

/// Gate 3 — **stratigraphy-emerged.** Sites must re-occupy into layered stacks,
/// and depth must be *coupled* to capacity. (Post-data amendment: the coupling
/// is NEGATIVE — stratigraphy accretes on marginal, climate-contested land, not
/// prime land — falsifying the preregistered positive hypothesis. See the
/// module docs.)
#[test]
fn stratigraphy_emerged() {
    let w = build(Seed(42), BuildDepth::Settlements);
    let s = stratigraphy(&w);
    assert!(
        s.restacked_sites >= MIN_RESTACKED_SITES,
        "no stratigraphy: only {} re-occupied sites (floor {MIN_RESTACKED_SITES}) — \
         sites are settled once and never re-founded.",
        s.restacked_sites
    );
    // The measured, robust reality (falsifies the preregistered POSITIVE
    // hypothesis): depth correlates NEGATIVELY with capacity.
    assert!(
        s.depth_capacity_correlation < 0.0,
        "depth/capacity correlation {:.4} is not negative — the falsified \
         'stratigraphy accretes on marginal land' finding no longer holds; \
         re-measure before re-pinning.",
        s.depth_capacity_correlation
    );
}
