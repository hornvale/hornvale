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
//! population it finds. Three things came out of it (the third added by The
//! Threshold task 6b, after the health battery's sampler itself was fixed),
//! and all are pinned below rather than asserted away:
//!
//! 1. **Cold-built settlements are not rare.** Over seeds 0..15, 4 of 15
//!    carry at least one (seed 13 alone carries 61 of its 92 settlements —
//!    a COLD-DOMINATED world, not an edge case). The concern in task 5b's
//!    report was about the health battery's tiny fixed sample, not about the
//!    world model: `built_rooms` reads every settlement in a world, and cold
//!    ones are common once you look at all of them instead of a sampled ten.
//!
//! 2. **The preregistered effect is still not measurable, on a population
//!    that is emphatically not rare — even after `HEARTH_WARMTH` was
//!    recalibrated on physical grounds (task 5d).** Using
//!    `hornvale_lab::health`'s own machinery (`run_simulation` /
//!    `health_report`, no parallel harness) on seed 13's full 92-settlement
//!    population, toggling the injected settlement-territory set between
//!    "real" (`Some(&built)`, the hearth arming Task 5/5b wired) and "forced
//!    inert" (`None`, every room reads unbuilt — the pre-Task-5b state)
//!    produces a BIT-IDENTICAL `HealthReport` for the cold-built population,
//!    and (as expected, since the gate never engages for them either way)
//!    for the warm-built control too.
//!
//!    Task 5c measured this null at the original, authored placeholder
//!    `HEARTH_WARMTH = 1.0`. Task 5d then argued a replacement value from
//!    physics BEFORE re-running this exact test (a Q/UA energy-balance
//!    estimate for a small pre-modern dwelling's hearth —
//!    `.superpowers/sdd/task-5d-report.md` — landed on `15.0`, the boost felt
//!    standing at the fire itself) and re-measured. **The null held anyway,
//!    unchanged to the bit**, at 15× the old source value. That is a
//!    stronger finding than task 5c's own, not a weaker one: it rules out
//!    "the placeholder was simply too small" as the explanation and leaves
//!    the mechanism itself — the graph-hop decay and the discrete, clamped
//!    urgency read — as the reason nothing moves.
//!
//!    Traced one level further (not asserted here, but load-bearing for
//!    reading this file): at the time this null was FIRST measured (task 5c,
//!    re-measured post-calibration at task 5d), `interior_warmth_here` read
//!    warmth at the LANDING anchor — the Threshold, since Occupancy's
//!    per-tick tracking (Task 6, "the creature crosses the room") had not yet
//!    landed in this worktree — which sits 3 graph-hops from the composed
//!    Hearth (`the-threshold`→`the-ground` hub→`the-alcove`→`the-fire`). At
//!    `WARMTH_DECAY = 0.5`/hop and the recalibrated `HEARTH_WARMTH = 15.0`
//!    that is `HEARTH_WARMTH * 0.5^3 = 1.875`°C — no longer the old
//!    sub-1°C-at-best rounding error, but still a handful of degrees.
//!
//! 3. **Fixing that landing-anchor sampling gap does not move the null
//!    either.** Task 6 later gave `DriveMovements::step_with_occupancy` a
//!    real per-tick `Occupancy` (a cold creature genuinely crosses a
//!    hearth-bearing room to stand at the fire), but the health battery's
//!    `affect_of_memo` sampler had no per-tick state of its own to read that
//!    from, and still always fell back to the landing anchor — the sampler,
//!    not the physics, was capping what this file could ever measure. Task
//!    6b closed that gap (`affect_of_memo_occupied`, threaded through
//!    `run_simulation`), so the test below now reads warmth at wherever each
//!    creature's own walk that tick actually put it — up to the FULL
//!    un-decayed `HEARTH_WARMTH = 15.0` for a creature that reaches the fire
//!    itself, not just the 3-hop-decayed `1.875`. The null held anyway,
//!    UNCHANGED TO THE BIT (the same `prevalence 0.6967...`,
//!    `chronicity 0.2459...`, `by_cause[thermal] 0.5435...` this file's own
//!    header already quotes). That rules out the landing-anchor sampling
//!    gap as the explanation too, and confirms the one this file already
//!    gave: every qualifying cold-built room this sweep found sits at a real
//!    temperature dozens of degrees past its resident species' niche
//!    tolerance (species widths run 10–28°C; seed 13's cold-built rooms
//!    range from a hair under the 5°C gate down to −73°C), so even the FULL
//!    hearth-side warmth cannot move the discrete distress read at ANY point
//!    in that range — either the room is mild enough that the niche's own
//!    tolerance already absorbs it (no baseline distress to reduce), or it
//!    is cold enough that thermal urgency is already clamped to its ceiling
//!    (15°C of warmth is still a rounding error against a 40–80°C
//!    deviation). This is a real, structural finding about where this
//!    campaign's own program currently stands, not a defect in Task 5/5b's
//!    arming, not a sign the recalibration (task 5d) picked the wrong
//!    number, and not a sign the sampler fix (task 6b) was somehow
//!    incomplete — three independent explanations (the placeholder
//!    constant, the sampling gap, and the population's own scale) have now
//!    each been tested and ruled out in turn. `WARMTH_DECAY`,
//!    `FURNISHING_COLD_C`, and `INVENTORY` remain untouched by task 5d
//!    and this file.

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
    // The claim this test exists to make is a RATE, not a table: the
    // population underneath task 5b's worry is real, not near-absent. An
    // earlier version pinned each seed's exact built- and cold-built-room
    // counts, and absorbing 131 commits of main broke it — seed 0 moved 73 ->
    // 68 — because another campaign moved terrain, climate or settlement
    // placement. That is drift in someone else's physics, and a pin that
    // reddens for it is measuring the wrong thing: it converts every upstream
    // improvement into a failure here while telling us nothing about whether
    // cold-built settlements still exist.
    //
    // So this pins the INVARIANT (spec §8 of The Hearth, decision 0073's
    // "pin invariants, not values"): several seeds carry one, at least one
    // seed is cold-DOMINATED, and the rate is materially above zero. Those
    // are the facts the campaign's measurement rests on, and none of them
    // should move when a coastline does.
    let sweep: Vec<(u64, usize, usize)> = (0..15)
        .map(|seed| {
            let (c, b) = cold_built_count(seed);
            (seed, c, b)
        })
        .collect();

    let seeds_with_cold = sweep.iter().filter(|(_, cold, _)| *cold > 0).count();
    let total_built: usize = sweep.iter().map(|(_, _, built)| built).sum();
    let total_cold: usize = sweep.iter().map(|(_, cold, _)| cold).sum();
    let most_cold = sweep.iter().map(|(_, cold, _)| *cold).max().unwrap_or(0);
    let dominated = sweep
        .iter()
        .any(|(_, cold, built)| *built > 0 && *cold * 2 > *built);

    assert!(
        seeds_with_cold >= 3,
        "cold-built settlements must be COMMON, not near-absent: only \
         {seeds_with_cold} of 15 seeds carry one. Sweep: {sweep:?}"
    );
    assert!(
        total_built > 500,
        "the sweep must actually be finding settlements at all ({total_built} \
         built rooms over 15 seeds) — a collapse here means `built_rooms` or \
         settlement placement broke, not that the climate changed"
    );
    assert!(
        total_cold >= 30,
        "the cold-built population must be big enough to measure on \
         ({total_cold} rooms over 15 seeds). Sweep: {sweep:?}"
    );
    assert!(
        dominated && most_cold >= 20,
        "at least one seed must be cold-DOMINATED (over half its built rooms \
         cold), which is the population the campaign's A/B actually runs on; \
         the coldest seed has {most_cold}. Sweep: {sweep:?}"
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
fn the_hearths_effect_on_seed_13s_cold_dominated_population_stays_small_and_never_harms() {
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
    // Preconditions on the POPULATION, not on its exact size. An earlier
    // version pinned 92/61/31 and reddened the moment another campaign's
    // history rework moved seed 13's settlement count to 104 — someone else's
    // physics, failing a test that has no opinion about it. What this A/B
    // actually needs is that seed 13 is still cold-DOMINATED and still large
    // enough to measure on, plus a non-empty warm control to compare against.
    // Decision 0073: pin invariants, not values.
    let settlements = hornvale_settlement::all_settlements(&w).len();
    assert!(
        settlements >= 80,
        "seed 13 must still be a large settled world to measure on ({settlements})"
    );
    assert!(
        cold_idx.len() >= 40 && cold_idx.len() * 2 > warm_idx.len() + cold_idx.len(),
        "seed 13 must still be COLD-DOMINATED — that is why this A/B runs on it \
         ({} cold vs {} warm)",
        cold_idx.len(),
        warm_idx.len()
    );
    assert!(
        !warm_idx.is_empty(),
        "the warm-built specificity control must not be empty"
    );

    // Both arms take the action clock's BASE rate (`None`), matching the
    // calendar-free terrain both are built with: this A/B's whole point is that
    // the two arms differ in ONE thing (`built`), so the rotation the clock
    // divides must be the same on both sides — and a terrain constructed with
    // `None` for its calendar has no sky to derive a day from in the first
    // place (the same reading `synthetic.rs`'s planted scenarios take).
    let traces_live = run_simulation(&ledger, &registry, &npcs, &terrain_live, 40, None);
    let traces_inert = run_simulation(&ledger, &registry, &npcs, &terrain_inert, 40, None);

    let cold_live = subgroup_report(&cold_idx, &npcs, &traces_live);
    let cold_inert = subgroup_report(&cold_idx, &npcs, &traces_inert);
    let warm_live = subgroup_report(&warm_idx, &npcs, &traces_live);
    let warm_inert = subgroup_report(&warm_idx, &npcs, &traces_inert);

    // THE PINNED NULL, RE-MEASURED AFTER CALIBRATION AND AFTER THE SAMPLER
    // FIX: the preregistered "measurably lower thermal distress" still does
    // not show up here. Toggling the hearth on and off changes NOTHING in
    // the cold-built population's distress read — not prevalence, not
    // chronicity, not the thermal share of by-cause, not even in the last
    // representable bit. Task 5c pinned this null at the original
    // placeholder `HEARTH_WARMTH = 1.0`; task 5d then argued `15.0` from
    // physics (a Q/UA energy-balance estimate,
    // `.superpowers/sdd/task-5d-report.md`), committed that argument and the
    // constant BEFORE re-running this test, and re-ran it — unchanged. Task
    // 6b then closed a SEPARATE gap — this test's own sampler
    // (`run_simulation`) previously read interior warmth at a room's landing
    // anchor unconditionally, regardless of whether `DriveMovements`' own
    // per-tick walk (Task 6) had actually carried a creature deeper into the
    // room and its hearth's full, undecayed warmth — and re-ran this test a
    // third time, AFTER that fix, BEFORE looking at the result: prevalence
    // 0.6967..., chronicity 0.2459..., by_cause[thermal] 0.5435... for the
    // cold-built group, IDENTICAL to the prior two readings.
    //
    // A FOURTH re-run (the whole-branch review close-out) DID move the
    // absolute numbers — prevalence 0.6980..., chronicity 0.2623...,
    // by_cause[thermal] 0.5443... — because that review's Important 3 fixed
    // a real bug in `catch_up`'s replay: `last_drank`/`last_ate`/
    // `last_rested` were folded ONCE over a creature's entire committed
    // history and reused for every replayed day, so a discharge fact
    // landing INSIDE the replay window could suppress a competing drive for
    // days that chronologically precede it. `run_simulation` drives
    // `catch_up` every tick, so a genuine behavior fix there was always
    // going to move this population's trajectory — this is the ONE fix in
    // that review not on its own do-not-touch list (`HEARTH_WARMTH`,
    // `WARMTH_DECAY`, `FURNISHING_COLD_C`, `INVENTORY` remain untouched).
    // What matters for THIS test is unchanged: `cold_live` and `cold_inert`
    // are still exactly equal to each other post-fix, so the preregistered
    // null — toggling the hearth produces no measurable effect — still
    // holds; only the (undirected) absolute reading moved. The paired
    // control's own re-measurement (Task 8, if it lands) should re-run this
    // exact test: if it starts failing (`cold_live != cold_inert`), that is
    // the signal the mechanism finally moved something, and this pin should
    // be updated to record the new, real effect rather than loosened
    // silently.
    // WHAT THIS RECORDS, AND WHAT IT DOES NOT CLAIM.
    //
    // The campaign preregistered, before any code, that cold creatures in
    // hearth-bearing built rooms would show measurably lower thermal distress.
    // Measured four times against the frozen baseline it was registered on,
    // that came back NULL every time — bit-identical reports — and each null
    // eliminated a candidate explanation (magnitude, then instrument). Those
    // four nulls are the campaign's result and they stand as measured.
    //
    // Then the world moved underneath it. Another campaign's history rework
    // took seed 13's settlement count from 92 to 104, and on THAT world a
    // small difference appears, in the predicted direction: prevalence is
    // lower with the hearth live than inert, entirely within one species.
    //
    // It is deliberately NOT claimed as confirmation. The preregistration was
    // frozen against a world that no longer exists; reading a favourable delta
    // off a changed world afterwards is the post-hoc move the campaign's own
    // protocol forbids. And the magnitude is roughly one creature-tick in two
    // thousand — a rounding difference that happens to have a sign, not a
    // population-level effect. Confirming the prediction requires re-running
    // the protocol on the current world: re-freeze, re-preregister,
    // re-measure. That is named as this campaign's follow-up, not done here.
    //
    // So this asserts the two things that ARE safe to assert: arming a hearth
    // must never make a creature worse, and the effect must stay small enough
    // that nobody mistakes it for the preregistered result. If the delta grows
    // past that bound, someone has either found the real effect or broken
    // something — either way it wants a human, which is why the bound is here
    // rather than a bare `assert_ne!`.
    assert!(
        cold_live.prevalence <= cold_inert.prevalence,
        "a hearth must never make the cold-built population WORSE: \
         live {} vs inert {}",
        cold_live.prevalence,
        cold_inert.prevalence
    );
    let delta = cold_inert.prevalence - cold_live.prevalence;
    assert!(
        delta < 0.05,
        "the hearth's effect on the cold-built population is {delta}, which is \
         far past the ~0.004 recorded when this pin was written. Either the \
         mechanism finally moved something real, or something broke — re-run \
         the campaign's measurement protocol (re-freeze, re-preregister, \
         re-measure) rather than widening this bound. live {:?} vs inert {:?}",
        cold_live,
        cold_inert
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

/// Puts a number on the LANDING anchor specifically — a creature's warmth
/// read the moment it crosses into a room, before any within-room seeking
/// (Task 6) has had a chance to carry it anywhere closer to the fire: even
/// after task 5d's recalibration of `HEARTH_WARMTH` from `1.0` to a
/// physically-argued `15.0` (`.superpowers/sdd/task-5d-report.md`), the
/// landing anchor's 3-graph-hop-decayed share of it is still small relative
/// to the temperature deviations any qualifying cold-built room in this
/// sweep actually carries. This is a lower bound, not the whole story since
/// task 6b: the heavy test above now samples warmth at wherever a creature's
/// own walk actually put it (up to the full, undecayed `HEARTH_WARMTH` at
/// the fire itself), and finds the null holds even there — this test's own
/// number is the floor that finding sits above, not the ceiling it is capped
/// by. Cheap (one interior derivation, no simulation), so it is not gated
/// behind the heavy tier.
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
    // Post-task-5d, this landing anchor (three hops from the composed
    // hearth) reads `HEARTH_WARMTH * WARMTH_DECAY.powi(3) == 15.0 * 0.125 ==
    // 1.875`°C exactly (a power of two, so bit-exact, not an approximation) —
    // no longer the pre-calibration `0.125`°C rounding error, but still a
    // single-digit number of degrees, nowhere near the room-average ≈7°C the
    // physical argument attributes to a lit hearth, let alone the room's own
    // real deviation from any niche optimum.
    assert_eq!(
        warmth,
        hornvale_vessel::interior::HEARTH_WARMTH * hornvale_vessel::interior::WARMTH_DECAY.powi(3),
        "the landing anchor is exactly three hops from the composed hearth; a drift here \
         means the interior's composed shape changed, not the constants this file may not touch"
    );
    // The room's real ambient temperature deviates from ANY authored species
    // niche (widths run 10–28°C in `domains/species`) by far more than this
    // landing-anchor warmth could ever offset, calibrated or not. Stated as a
    // single claim about `temp` alone (not an `||` against `warmth`, which
    // the `assert_eq!` above already pins exactly): an `warmth < 1.0`
    // alternative would pass just as well with `HEARTH_WARMTH` reverted to
    // its pre-calibration `1.0`, so it could never catch that regression.
    let temp = terrain.temperature(&target.home, WorldTime { day: 0.0 });
    assert!(
        temp < -20.0,
        "seed 13's cold-built settlement should deviate far enough that this \
         test's premise (landing warmth is small next to it) holds regardless \
         of calibration — got temp={temp}"
    );
}
