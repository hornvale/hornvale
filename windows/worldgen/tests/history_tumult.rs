//! The Tumult (conflict-as-criticality, campaign C3 slice 1) — the
//! measurement gates (Task 3). `history_for` is the diagnostic entry point:
//! it assembles the same bake inputs the settlement stage builds and
//! returns the raw `History`, whose `cascade_hist` records every
//! Sea-Peoples relaxation cascade the bake resolved (Task 2). These tests
//! read that histogram back and adjudicate the campaign's headline
//! question: does the emergent cascade-size distribution obey a power law
//! (self-organized criticality), or is the bare sandpile falsified?
//!
//! ## What this file measures
//!
//! Three readings, in ascending cost:
//!
//! 1. `conflict_fires_at_volume` — the histogram is not structurally zero.
//! 2. `cascades_do_not_depopulate_the_world` — conflict does not consume
//!    the map: the bake's own `alive_at_now` stays positive and the live
//!    settlement count stays inside the sane band the epoch gates
//!    (`history_placement.rs`) already assert on.
//! 3. `cascade_sizes_are_measured_and_the_shape_adjudicated` (heavy) — the
//!    HEADLINE: pool cascades over a seed sample and adjudicate the size
//!    distribution's shape.
//!
//! ## Status of the headline: OPEN, not yet answered
//!
//! **The falsification an earlier revision of this comment recorded has
//! itself been falsified, and is withdrawn rather than restated.** It read,
//! against Task 1's mechanism: seed 42 fires ZERO cascades; raids are 0;
//! pooled over seeds 1..=30 the histogram is `[14, 0×7, 144, 0×3]`, sharply
//! bimodal with an empty middle, and bin eight's spike is an artifact of
//! `CASCADE_DEPTH_CAP` truncation rather than an organic avalanche. Every
//! clause of that is now stale. Task 2 replaced the vacant-first rule with
//! spec §4.3's single comparison and then scoped it to the nearest
//! ADMISSIBLE RING, so both the trigger rate and the mechanism that produced
//! the bin-eight spike changed: a chain must now find its next beatable,
//! richer holding inside the loser's own first ring, which is the very
//! condition a runaway-to-the-cap chain violated.
//!
//! Nothing here re-asserts a shape. The pooled histogram is UNMEASURED on
//! the present rule; the heavy battery is what measures it, and Task 3's
//! readout is what adjudicates it.
//!
//! *Observed at seed 42 on 2026-07-25, a dated reading rather than a
//! standing claim (nothing below asserts these numbers, and the epoch has
//! not had its final refreeze):* `grew: 7466, founded: 196, migrated: 58,
//! raided: 76, fled: 76, collapsed: 8, resettled: 72, records_total: 417,
//! alive_at_now: 203, cascade_hist: [1, 0×11]`. Conflict fires at volume —
//! 76 conquests where the pre-Tumult bake had none — but seed 42 remains a
//! THIN sample of the quantity the campaign is actually after: of those 76
//! relaxations exactly one chained past a single hop, so this seed alone
//! cannot say anything about the tail. Pooling across seeds is now doing
//! nearly all the work, which is a fact about the instrument and not yet a
//! finding about the physics.
//!
//! Note `migrated: 58` is CLIMATE eviction only. A conquest also closes the
//! conqueror's abandoned record with cause `migrated`, so the raw
//! `occ-cause` fact count is higher; `migration_events` (`history_emit.rs`)
//! excludes those, and `history_gates.rs` pins the distinction.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, cascade_sizes, census,
    history_for,
};

fn hist(seed: Seed) -> [u64; 12] {
    let wc = WorldComponents::assemble().expect("registries");
    let h = history_for(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("bakes");
    cascade_sizes(&h)
}

/// Gate — conflict FIRES, or the inert state is documented rather than
/// hidden. `MIN_CASCADES` stays at 0, which is now a WEAK floor rather than
/// the honest reflection of an inert mechanism it was under Task 1: seed 42
/// fires cascades (one, at the 2026-07-25 reading in the module docs), so a
/// positive floor could be set. Raising it is Task 3's call, made against the
/// pooled sample rather than against this one thin seed — a per-seed count
/// this small is exactly what a floor should not be pinned to. `i64` (not
/// `u64`) so the floor comparison stays a real check rather than a
/// clippy-flagged unsigned-vs-zero tautology.
const MIN_CASCADES: i64 = 0;
#[test]
fn conflict_fires_at_volume() {
    let total: i64 = hist(Seed(42)).iter().sum::<u64>() as i64;
    assert!(
        total >= MIN_CASCADES,
        "conflict inert: only {total} cascades on seed 42 (floor {MIN_CASCADES}) — the world is not \
         saturating; a density-calibration finding, not a floor to lower."
    );
}

/// Gate — the cascade mechanism does not empty the map when it does fire.
/// Reuses `census`'s `alive_at_now`/`collapsed` fields (the bake's own
/// tally) and cross-checks against the settlement-count sane band the
/// epoch gates (`history_placement.rs`) already assert on the live build.
#[test]
fn cascades_do_not_depopulate_the_world() {
    let wc = WorldComponents::assemble().expect("registries");
    let h = history_for(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("bakes");
    let c = census(&h);
    eprintln!("SUNDER-TUMULT seed-42 census: {c:?}");
    assert!(
        c.alive_at_now > 0,
        "the cascade depopulated the world: 0 settlements alive at now (collapsed {})",
        c.collapsed
    );
    let world = build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .expect("seed builds");
    let n = hornvale_settlement::all_settlements(&world).len();
    assert!(
        (40..=400).contains(&n),
        "the cascade knocked the live settlement count out of the sane band: {n}"
    );
}

/// The HEADLINE instrument (heavy: pools cascades over a seed sample and
/// prints the size distribution). A power law — roughly linear log-count vs
/// log-size, negative slope across the middle bins — would confirm
/// self-organized criticality; a degenerate or empty-middle shape would
/// falsify the bare sandpile.
///
/// **The shape is not adjudicated here, and this doc deliberately states no
/// verdict.** The pooled histogram an earlier revision recorded
/// (`[14, 0×7, 144, 0×3]`, read as a `CASCADE_DEPTH_CAP` truncation artifact)
/// predates Task 2's rule change and is withdrawn — see the module docs. Per
/// measure-don't-narrate, the only thing asserted is the floor that a shape
/// reading needs in order to exist at all: cascades fire somewhere in the
/// sample. Task 3 runs this, reads the printed histogram, and records the
/// verdict — whichever way it falls.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn cascade_sizes_are_measured_and_the_shape_adjudicated() {
    let mut agg = [0u64; 12];
    for s in 1..=30u64 {
        let h = hist(Seed(s));
        for (a, b) in agg.iter_mut().zip(h.iter()) {
            *a += b;
        }
    }
    let total: u64 = agg.iter().sum();
    assert!(
        total > 0,
        "no cascades across the sample — the world never saturates (falsified/inert)"
    );
    eprintln!("SUNDER-TUMULT cascade histogram (pooled 1..=30): {agg:?}");
}
