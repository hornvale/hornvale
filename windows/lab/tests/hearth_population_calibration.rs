//! The Threshold's own preregistration (ledger #16) reads: "cold-climate
//! creatures in built rooms drawing a hearth show measurably lower thermal
//! distress." Task 5 armed `Thermal::warmth` from `None` to `Some(...)`; task
//! 5b made `Terrain::is_built` real. Task 5b's own report then found the
//! health battery's SMALL sample (10 derived NPCs: `HEALTH_NPCS` +
//! `HEALTH_WILD`) never once landed on a creature standing in a room that was
//! both built and cold, and worried aloud that the joint condition might be
//! structurally rare — settlements condense onto rivers in temperate bands
//! (The Confluence), and the coldest cells sit at the poles or altitude,
//! typically unsettled.
//!
//! This file answers that worry with a real sweep rather than a bigger
//! battery sample, and then measures the preregistered prediction on the
//! population it finds. Two things came out of it, and both are pinned
//! below rather than asserted away:
//!
//! 1. **Cold-built settlements are not rare.** Over seeds 0..15, 4 of 15
//!    carry at least one (seed 13 alone carries 61 of its 92 settlements —
//!    a COLD-DOMINATED world, not an edge case). The concern in task 5b's
//!    report was about the health battery's tiny fixed sample, not about the
//!    world model: `built_rooms` reads every settlement in a world, and cold
//!    ones are common once you look at all of them instead of a sampled ten.
//!
//! 2. **The preregistered effect is still not measurable, on a population
//!    that is emphatically not rare.** Using `hornvale_lab::health`'s own
//!    machinery (`run_simulation` / `health_report`, no parallel harness) on
//!    seed 13's full 92-settlement population, toggling the injected
//!    settlement-territory set between "real" (`Some(&built)`, the hearth
//!    arming Task 5/5b wired) and "forced inert" (`None`, every room reads
//!    unbuilt — the pre-Task-5b state) produces a BIT-IDENTICAL
//!    `HealthReport` for the cold-built population, and (as expected, since
//!    the gate never engages for them either way) for the warm-built control
//!    too.
//!
//!    Traced one level further (not asserted here, but load-bearing for
//!    reading this file): `interior_warmth_here` reads warmth at the
//!    LANDING anchor — the Threshold, since Occupancy's per-tick tracking
//!    (Task 6, "the creature crosses the room") has not landed in this
//!    worktree — which sits 3 graph-hops from the composed Hearth
//!    (`the-threshold`→`the-ground` hub→`the-alcove`→`the-fire`). At
//!    `WARMTH_DECAY = 0.5`/hop that is `HEARTH_WARMTH * 0.5^3 = 0.125`°C, and
//!    even the theoretical best case — a creature standing AT the hearth
//!    itself — is only `HEARTH_WARMTH = 1.0`°C. Every qualifying cold-built
//!    room this sweep found sits at a real temperature dozens of degrees
//!    past its resident species' niche tolerance (species widths run
//!    10–28°C; seed 13's cold-built rooms range from a hair under the 5°C
//!    gate down to −73°C), so a sub-1°C-at-best additive nudge cannot move
//!    the discrete distress read at ANY point in that range — either the
//!    room is mild enough that the niche's own tolerance already absorbs it
//!    (no baseline distress to reduce), or it is cold enough that thermal
//!    urgency is already clamped to its ceiling (a °C more warmth is a
//!    rounding error against a 40–80°C deviation). This is a real,
//!    structural finding about where this campaign's own program currently
//!    stands (Tasks 6–8 — seeking, catch-up, and the paired control —
//!    unbuilt), not a defect in Task 5/5b's arming, and NOT something the
//!    campaign's frozen constants (`FURNISHING_COLD_C`, `HEARTH_WARMTH`,
//!    `WARMTH_DECAY`, `INVENTORY`) may be adjusted to fix — this file does
//!    not touch any of them.

use hornvale_kernel::WorldTime;
use hornvale_lab::health::{AffectTrace, health_report, run_simulation};
use hornvale_locale::LocaleContext;
use hornvale_vessel::liveness::{
    AGENT_AT, DRANK, EATEN, LocaleTerrain, RESTED, Terrain, built_rooms, derive_npcs,
};

/// Builds a real world at `seed` with every pin at its default — the same
/// shape `health_calibration.rs`'s own `world` helper uses, so this file's
/// worlds are the same ones the health battery itself would derive.
fn world(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// The count of built settlement rooms reading `is_cold` (Terrain::is_cold,
/// `< FURNISHING_COLD_C` at the frozen furnishing-reference day) out of the
/// world's total built rooms — the population `interior_of` would compose a
/// hearth for.
fn cold_built_count(seed: u64) -> (usize, usize) {
    let w = world(seed);
    let Ok(ctx) = LocaleContext::build(&w) else {
        return (0, 0);
    };
    let terrain = LocaleTerrain::new(&ctx);
    let built = built_rooms(&w, &ctx);
    let cold = built
        .iter()
        .filter_map(|id| id.unpack().ok())
        .filter(|addr| terrain.is_cold(addr))
        .count();
    (cold, built.len())
}

#[test]
fn cold_built_settlements_are_common_not_rare() {
    // Pinned per-seed (deterministic world generation, seeds 0..15): the
    // real population underneath task 5b's worry. 4 of 15 seeds carry at
    // least one cold-built settlement, and seed 13 is COLD-DOMINATED (61 of
    // 92). A drift here means terrain/climate/settlement-placement moved —
    // re-derive and re-pin deliberately (ADR-0016), never loosen to match a
    // regression.
    let expected: [(u64, usize, usize); 15] = [
        (0, 8, 73),
        (1, 0, 103),
        (2, 0, 40),
        (3, 0, 94),
        (4, 2, 38),
        (5, 0, 175),
        (6, 0, 0),
        (7, 0, 87),
        (8, 4, 65),
        (9, 0, 0),
        (10, 0, 50),
        (11, 0, 48),
        (12, 0, 14),
        (13, 61, 92),
        (14, 0, 69),
    ];
    let mut seeds_with_cold = 0u32;
    for (seed, want_cold, want_built) in expected {
        let (cold, built) = cold_built_count(seed);
        assert_eq!(built, want_built, "seed {seed}: built-room count drifted");
        assert_eq!(
            cold, want_cold,
            "seed {seed}: cold-built-room count drifted"
        );
        if cold > 0 {
            seeds_with_cold += 1;
        }
    }
    assert_eq!(
        seeds_with_cold, 4,
        "4 of the 15 swept seeds must carry at least one cold-built settlement — \
         cold-built settlements are common, not the near-absent population task \
         5b's report worried they might structurally be"
    );
}

/// Derives one `Npc` per settlement in `world` (via `derive_npcs` truncated
/// to the full settlement count, so none are left out by the population-rank
/// cutoff the health battery's own small `HEALTH_NPCS` applies), and splits
/// them by whether their home room is built-and-cold (draws a hearth) or
/// built-and-warm (the campaign's own stated "warm-climate creatures"
/// control — never draws one, whatever `built` reads, since `is_cold` gates
/// it independently of `is_built`).
fn cold_and_warm_built_npcs(
    world: &hornvale_kernel::World,
    ctx: &LocaleContext,
    terrain: &LocaleTerrain,
    ledger: &mut hornvale_kernel::Ledger,
) -> (Vec<hornvale_vessel::liveness::Npc>, Vec<usize>, Vec<usize>) {
    let settlements = hornvale_settlement::all_settlements(world);
    let home = settlements[0].id;
    let npcs = derive_npcs(world, ctx, ledger, settlements.len(), home);
    let cold_idx: Vec<usize> = npcs
        .iter()
        .enumerate()
        .filter(|(_, n)| terrain.is_built(&n.home) && terrain.is_cold(&n.home))
        .map(|(i, _)| i)
        .collect();
    let warm_idx: Vec<usize> = npcs
        .iter()
        .enumerate()
        .filter(|(_, n)| terrain.is_built(&n.home) && !terrain.is_cold(&n.home))
        .map(|(i, _)| i)
        .collect();
    (npcs, cold_idx, warm_idx)
}

/// Reduce the traces at `idx` to a `HealthReport`, tagging each with its
/// derived species (the by-species reduction needs it; `AffectTrace` does
/// not borrow, so this clones the per-creature affect vector for each of the
/// two runs it is reduced under).
fn subgroup_report(
    idx: &[usize],
    npcs: &[hornvale_vessel::liveness::Npc],
    traces: &[Vec<hornvale_vessel::liveness::Affect>],
) -> hornvale_lab::health::HealthReport {
    let group: Vec<AffectTrace> = idx
        .iter()
        .map(|&i| AffectTrace {
            species: npcs[i].species.clone(),
            affects: traces[i].clone(),
        })
        .collect();
    health_report(&group)
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_hearth_shows_no_measurable_effect_on_seed_13s_cold_dominated_population() {
    // Seed 13: the richest cold-built population this campaign's sweep
    // found (61 of 92 settlements built-and-cold). If the preregistered
    // effect is measurable anywhere with the machinery as it stands today,
    // it is here.
    let w = world(13);
    let ctx = LocaleContext::build(&w).expect("seed 13 has a locale");
    let mut ledger = w.ledger.clone();
    let mut registry = w.registry.clone();
    let _ = registry.register_predicate(AGENT_AT, false, "an agent's position on a day");
    let _ = registry.register_predicate(DRANK, false, "an agent satisfied its sustenance goal");
    let _ = registry.register_predicate(RESTED, false, "an agent rested on a day");
    let _ = registry.register_predicate(EATEN, false, "an agent ate on a day");

    let built = built_rooms(&w, &ctx);
    // "Hearth live": the real arming this campaign wired (Task 5/5b).
    let terrain_live = LocaleTerrain::with_fields(&ctx, None, None, None, Some(&built));
    // "Hearth forced inert": every room reads unbuilt, so `interior_of` never
    // composes a hearth anywhere — the pre-Task-5b state, on the identical
    // world/npcs/ledger. Not a threshold or constant change (`built` is an
    // `Option` the campaign's own `with_fields` already exposes for exactly
    // this A/B, per the plan's own Task 8).
    let terrain_inert = LocaleTerrain::with_fields(&ctx, None, None, None, None);

    let (npcs, cold_idx, warm_idx) = cold_and_warm_built_npcs(&w, &ctx, &terrain_live, &mut ledger);
    assert_eq!(
        hornvale_settlement::all_settlements(&w).len(),
        92,
        "seed 13's settlement count drifted"
    );
    assert_eq!(
        cold_idx.len(),
        61,
        "seed 13's cold-built population drifted"
    );
    assert_eq!(
        warm_idx.len(),
        31,
        "seed 13's warm-built population drifted"
    );

    let traces_live = run_simulation(&ledger, &registry, &npcs, &terrain_live, 40);
    let traces_inert = run_simulation(&ledger, &registry, &npcs, &terrain_inert, 40);

    let cold_live = subgroup_report(&cold_idx, &npcs, &traces_live);
    let cold_inert = subgroup_report(&cold_idx, &npcs, &traces_inert);
    let warm_live = subgroup_report(&warm_idx, &npcs, &traces_live);
    let warm_inert = subgroup_report(&warm_idx, &npcs, &traces_inert);

    // THE PINNED NULL: the preregistered "measurably lower thermal distress"
    // does not show up here. Toggling the hearth on and off changes NOTHING
    // in the cold-built population's distress read — not prevalence, not
    // chronicity, not the thermal share of by-cause. This is the honest
    // result of task 5c's measurement, not a threshold tuned to make a
    // number move (this file touches none of `FURNISHING_COLD_C`,
    // `HEARTH_WARMTH`, `WARMTH_DECAY`, `INVENTORY`). A future task that
    // lands Occupancy tracking through this path (Task 6, "the creature
    // crosses the room") or the paired control's own re-measurement (Task 8)
    // should re-run this exact test: if it starts failing, that is the
    // signal the mechanism finally moved something, and this pin should be
    // updated to record the new, real effect rather than loosened silently.
    assert_eq!(
        cold_live, cold_inert,
        "the hearth arming currently produces NO measurable change in the cold-built \
         population's health report: {cold_live:?} vs {cold_inert:?}"
    );
    // THE SPECIFICITY CONTROL: warm-built creatures never draw a hearth
    // regardless of `is_built` (the gate is `is_built && is_cold`, and
    // `is_cold` is false for them), so toggling `built` must leave them
    // completely unaffected either way — this is expected, not a finding,
    // and its failure would mean the toggle leaked into a population it has
    // no business touching.
    assert_eq!(
        warm_live, warm_inert,
        "the warm-built control must be unaffected by the built-territory toggle: \
         {warm_live:?} vs {warm_inert:?}"
    );
}

/// Isolates the mechanism the test above only observes the absence of: the
/// `warmth_at` value a creature actually reads is capped by the graph
/// distance from its landing anchor (the Threshold, absent Occupancy
/// tracking — see this file's header) to the composed Hearth, which is
/// small relative to the temperature deviations any qualifying cold-built
/// room in this sweep actually carries. Cheap (one interior derivation, no
/// simulation), so it is not gated behind the heavy tier.
#[test]
fn the_landing_anchors_warmth_is_small_next_to_real_cold_built_deviations() {
    let w = world(13);
    let ctx = LocaleContext::build(&w).expect("seed 13 has a locale");
    let built = built_rooms(&w, &ctx);
    let terrain = LocaleTerrain::with_fields(&ctx, None, None, None, Some(&built));

    let mut ledger = w.ledger.clone();
    let settlements = hornvale_settlement::all_settlements(&w);
    let home = settlements[0].id;
    let npcs = derive_npcs(&w, &ctx, &mut ledger, settlements.len(), home);
    let target = npcs
        .iter()
        .find(|n| terrain.is_built(&n.home) && terrain.is_cold(&n.home))
        .expect("seed 13 carries a cold-built settlement");

    let interior = hornvale_vessel::interior::interior_of(&target.home, &terrain);
    let kind = hornvale_vessel::interior::seam_kind(terrain.is_built(&target.home));
    let landing = hornvale_vessel::interior::landing(&interior, kind)
        .expect("a built-cold interior always has a landing");
    let warmth = hornvale_vessel::interior::warmth_at(&interior, landing, 64);

    // The landing anchor's warmth is a small fraction of even the emitter's
    // own strength (`HEARTH_WARMTH`, read here only to state the ratio, not
    // to touch or compare against a threshold this file may not adjust).
    assert!(
        warmth <= hornvale_vessel::interior::HEARTH_WARMTH,
        "no anchor may read warmer than the hearth's own emission: {warmth}"
    );
    assert!(
        warmth > 0.0,
        "a built-cold room's landing anchor must reach SOME warmth (the hearth is \
         reachable by construction — `permits` rejects a disconnected interior)"
    );
    // The room's real ambient temperature deviates from ANY authored
    // species niche (widths run 10–28°C in `domains/species`) by far more
    // than this landing-anchor warmth could ever offset.
    let temp = terrain.temperature(&target.home, WorldTime { day: 0.0 });
    assert!(
        temp < -20.0 || warmth < 1.0,
        "either the room is mildly cold (within a °C of offsetting), or the \
         warmth read is capped well under a degree — got temp={temp}, warmth={warmth}"
    );
}
