//! The Tumult (conflict-as-criticality, campaign C3 slice 1) — the
//! measurement gates (Task 3). `history_for` is the diagnostic entry point:
//! it assembles the same bake inputs the settlement stage builds and
//! returns the raw `History`, whose `cascade_hist` records every
//! roll-downhill relaxation cascade the bake resolved (Task 2). These tests
//! read that histogram back and adjudicate the campaign's headline
//! question: does the emergent cascade-size distribution obey a power law
//! (self-organized criticality), or is predation-with-dissipation falsified?
//!
//! ## What this file measures
//!
//! Three readings, in ascending cost:
//!
//! 1. `conflict_fires_at_volume` — conquest fires on a world with land to
//!    spare, which is the proof that the driver is value × strength rather
//!    than density.
//! 2. `cascades_do_not_depopulate_the_world` — conflict does not consume the
//!    map: the bake's own `alive_at_now` stays well clear of zero and the
//!    live settlement count stays inside the sane band the epoch gates
//!    (`history_placement.rs`) already assert on.
//! 3. `cascade_sizes_are_measured_and_the_shape_adjudicated` (heavy) — the
//!    HEADLINE: pool cascades over a seed sample and adjudicate the size
//!    distribution's shape.
//!
//! ## The headline verdict: SUB-CRITICAL. The power law is FALSIFIED.
//!
//! Measured on the shipped rule (2026-07-25), pooled over `SHAPE_SAMPLE`
//! (seeds 1..=30): `hist [38, 2, 0×10]` — 38 cascades of size 1, two of size
//! 2–3, and **nothing at all above size 3**, out of 886 conquests. Replicated
//! out of band over seeds 1..=100: `hist [138, 3, 0×10]` out of 2974
//! conquests, i.e. the same shape at 3.3× the sample.
//!
//! The two readings agree on the quantity that decides the question. The
//! **branching ratio** — secondary displacements per relaxation, the σ whose
//! critical value is 1 — measures **σ ≈ 0.051** on both samples (144–147
//! secondary evictions over ~2828 relaxations at 1..=100; 42–44 over ~843 at
//! 1..=30). A relaxation chains onward about one time in twenty, and a chain
//! that has already chained once almost never chains again: the per-octave
//! drop from bin 0 to bin 1 is ~46×, where a heavy tail would fall by
//! 2^(1−τ) ≈ 2–4×. The occupied support is bins 0–1 — under half a decade,
//! against the ≥ ~1.5 decades spec §5 requires to call a power law.
//!
//! So this is a **geometric distribution with a hard exponential cutoff, deep
//! in the sub-critical regime** — spec §5's documented falsification, not its
//! payoff, and it ships as one. The diagnosis is the one spec §6 already
//! flagged: slice 1 dissipates far too fast to sustain an avalanche. Each hop
//! costs the roller `WAR_LOSS`, the victim `WAR_LOSS` plus the journey, and
//! every victim is by construction at least `RAID_MARGIN` times weaker than
//! the people that displaced it — so a chain's strength decays geometrically
//! while the strength it must beat does not, and it dies against `VIABLE_MIN`
//! within a hop or two. Nothing *accumulates* between relaxations: there is
//! no standing structure whose collapse releases stored stress at once. That
//! is precisely the deferred **dominance hierarchy + collapse-release** slice
//! (spec §6/§9), and this measurement is the evidence for promoting it.
//!
//! **Disclosure — post-observation amendments (spec §5, repeated here as that
//! section requires).** This metric's mechanism was amended twice *after*
//! unfavourable observations, and the two amendments pull in **opposite**
//! directions. Amendment 1 (the settled-land premium and the unified
//! best-value rule, §4.3) *raised* the branching ratio; amendment 2 (the
//! nearest-admissible-ring locality fix) *lowered* it, cutting seed 42 from 6
//! cascades to 1 and removing the campaign's single largest cascade. Neither
//! was made to improve the metric — amendment 1 restored what §1 and §4.3
//! already asserted, amendment 2 repaired a distance term the spec had
//! dropped by accident — but both are post-observation and are labelled as
//! such wherever this result is reported. No constant was tuned toward a
//! power law at any point, and the floors below are set clear of measured
//! values rather than at them.
//!
//! *Observed at seed 42 on 2026-07-25, a dated reading rather than a standing
//! claim (only the `const` floors below are asserted):* `grew: 7466,
//! founded: 196, migrated: 58, raided: 76, fled: 76, collapsed: 8,
//! resettled: 72, records_total: 417, alive_at_now: 203, cascade_hist:
//! [1, 0×11]`. Conflict fires at volume — 76 conquests where the pre-Tumult
//! bake had none — but seed 42 is a THIN sample of the tail: exactly one of
//! those 76 relaxations chained past a single hop. The shape verdict above
//! therefore rests on the pooled sample, never on this seed.
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

/// The seed sample the pooled shape verdict is measured over. Fixed (not a
/// range that grows with a knob) so the heavy reading is reproducible; 30
/// worlds pool ~886 conquests, enough to separate a geometric cutoff from a
/// heavy tail, and the finding replicates unchanged at 1..=100 (module docs).
const SHAPE_SAMPLE: std::ops::RangeInclusive<u64> = 1..=30;

/// The Tumult (predation) re-pin: the seed-42 conflict floor, re-pointed from
/// the cascade histogram onto `census.raided`. Seed 42 measures **76**
/// conquests where the pre-Tumult crowding bake measured zero, so the floor
/// that proves "conflict fires on VALUE, not density" is a raid count, not a
/// cascade count — the cascade histogram on this one seed is `[1, 0×11]`,
/// far too thin to floor anything against. Set well clear of 76; a run below
/// it means the raid rule went inert, which spec §8.1 makes a calibration
/// finding for the owner and never a floor to lower.
const MIN_RAIDS: u64 = 20;

/// The Tumult (predation) re-pin: the not-depopulated floor, tightened from a
/// bare `> 0` (which a world reduced to a single hamlet would still pass) to a
/// real ceiling on how much of the map lossy war may consume. Seed 42
/// measures 203 records alive at `now`; 50 leaves 4× headroom.
const MIN_ALIVE_AT_NOW: u64 = 50;

/// The Tumult (predation) re-pin: the pooled cascade floor, raised from the
/// `MIN_CASCADES = 0` that honestly recorded Task 1's inert mechanism. The
/// roll-downhill now fires: `SHAPE_SAMPLE` pools **40** cascades. Pinned at 10
/// — clear below the measurement, and deliberately pinned against the POOLED
/// sample rather than any single seed (seed 42 alone fires one). This is a
/// floor on the phenomenon EXISTING, not on its shape: the shape is
/// sub-critical (module docs), and asserting a ceiling on it would freeze the
/// falsification the deferred dominance-hierarchy slice is meant to break.
const MIN_POOLED_CASCADES: u64 = 10;

/// Gate — conflict FIRES, on value rather than on crowding (spec §8.1). Seed
/// 42 is the seed that never crowds: the pre-Tumult bake resolved zero raids
/// on it because vacant habitable land was always reachable. Under the
/// predation rule it resolves conquests anyway, which is the whole claim —
/// the trigger is a value gradient walked down a strength gradient, and
/// density is not in it. An inert run is a `RAID_MARGIN` calibration finding
/// for the owner, not a floor to lower.
#[test]
fn conflict_fires_at_volume() {
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
    assert!(
        c.raided >= MIN_RAIDS,
        "conflict inert: only {} conquests on seed 42 (floor {MIN_RAIDS}) — predation is not \
         firing on the value gradient ({c:?}); a raid-margin calibration finding, not a floor \
         to lower.",
        c.raided
    );
}

/// Gate — the cascade mechanism does not empty the map when it does fire
/// (spec §8.2). Reuses `census`'s `alive_at_now`/`collapsed` fields (the
/// bake's own tally) and cross-checks against the settlement-count sane band
/// the epoch gates (`history_placement.rs`) already assert on the live build.
/// A guard-rail, not a payoff gate, and deliberately one-sided: deleting
/// predation *raises* both numbers rather than lowering them. That is measured,
/// not assumed — a Task-3 probe that returned early from `Bake::maybe_raid`
/// (reproducing the pre-Tumult bake exactly, down to the Sundering's seed-42
/// migration count of 12) put seed 42 at `alive_at_now: 138` against the
/// predation build's 203, and pooled 1583 against 1955 over `SHAPE_SAMPLE`.
/// The six sampled worlds that do end empty (seeds 6, 9, 18, 20, 22, 29) are
/// **exactly the same six** with predation and without it: the ice empties
/// them, conquest does not. Spec §8.2 holds.
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
        c.alive_at_now >= MIN_ALIVE_AT_NOW,
        "the cascade depopulated the world: only {} records alive at now (floor \
         {MIN_ALIVE_AT_NOW}, collapsed {})",
        c.alive_at_now,
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

/// The HEADLINE instrument (heavy: pools cascades over `SHAPE_SAMPLE` and
/// prints the size distribution alongside the census counts the branching
/// ratio is computed from).
///
/// **The verdict is recorded in the module docs, and it is a falsification:**
/// the pooled distribution is geometric with a hard cutoff (`[38, 2, 0×10]`,
/// branching ratio σ ≈ 0.05), not a power law. Per measure-don't-narrate the
/// only thing asserted here is the floor that says the phenomenon exists at
/// all — `MIN_POOLED_CASCADES`. No ceiling is asserted on the shape: the
/// deferred dominance-hierarchy slice is supposed to break this shape, and a
/// pin against it would freeze the falsification instead of recording it.
/// claim: readout(off-gate, heavy:) — cascade-size distribution over
/// SHAPE_SAMPLE, with pooled revolt/flight counts, adjudicated
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn cascade_sizes_are_measured_and_the_shape_adjudicated() {
    let mut agg = [0u64; 12];
    let mut raided = 0u64;
    let mut resettled = 0u64;
    let mut collapsed = 0u64;
    // Spec §4.3d's two vassal answers, pooled beside the histogram they were
    // added to move: revolt is the campaign's first mechanism by which an
    // accumulated relation can FAIL, so how often it fires is the reading that
    // makes a still-geometric shape interpretable rather than merely
    // disappointing.
    let mut flights = 0u64;
    let mut revolts = 0u64;
    for s in SHAPE_SAMPLE {
        let wc = WorldComponents::assemble().expect("registries");
        let h = history_for(
            Seed(s),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
        )
        .expect("bakes");
        let c = census(&h);
        let hi = cascade_sizes(&h);
        eprintln!(
            "SUNDER-TUMULT seed {s}: raided {} resettled {} collapsed {} alive {} \
             flights {} revolts {} hist {hi:?}",
            c.raided, c.resettled, c.collapsed, c.alive_at_now, c.vassal_flights, c.vassal_revolts
        );
        raided += c.raided;
        resettled += c.resettled;
        collapsed += c.collapsed;
        flights += c.vassal_flights;
        revolts += c.vassal_revolts;
        for (a, b) in agg.iter_mut().zip(hi.iter()) {
            *a += b;
        }
    }
    let total: u64 = agg.iter().sum();
    eprintln!(
        "SUNDER-TUMULT pooled: hist {agg:?} cascades {total} raided {raided} resettled \
         {resettled} collapsed {collapsed} flights {flights} revolts {revolts}"
    );
    assert!(
        total >= MIN_POOLED_CASCADES,
        "the roll-downhill went inert: only {total} cascades pooled over the sample (floor \
         {MIN_POOLED_CASCADES}) across {raided} conquests — a displaced people is always \
         finding a home without evicting anybody, so the branching ratio is structurally \
         zero and the shape question cannot be asked."
    );
}
