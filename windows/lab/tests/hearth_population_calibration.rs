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
//! `cold_built_settlements_are_common_not_rare` (below) answers that worry
//! with a real sweep of seed 13's own world rather than a bigger battery
//! sample: cold-built settlements are common, not an edge case. That test is
//! cheap (no simulation, just `built_rooms` + a `LocaleTerrain` read over one
//! world) and stays as it was.
//!
//! **What used to live below it does not anymore, and this is the record of
//! why (The Ember).** The preregistered claim was measured four times on
//! seed 13's REAL, full-world population — `hornvale_lab::health`'s own
//! machinery (`run_simulation`), toggling the injected settlement-territory
//! set between "real" (`Some(&built)`) and "forced inert" (`None`) on
//! otherwise-identical worlds/npcs/ledgers:
//!
//! 1. Task 5c measured a BIT-IDENTICAL null at the original placeholder
//!    `HEARTH_WARMTH = 1.0`.
//! 2. Task 5d argued a replacement value from physics BEFORE re-running the
//!    test (a Q/UA energy-balance estimate for a small pre-modern dwelling's
//!    hearth, `.superpowers/sdd/task-5d-report.md`, landing on `15.0` — the
//!    boost felt standing at the fire itself) and re-measured: the null held
//!    anyway, unchanged to the bit, at 15× the old source value.
//! 3. Task 6b closed a sampling gap (the battery previously read warmth at a
//!    room's landing anchor unconditionally, rather than wherever a
//!    creature's own within-room walk that tick actually put it) and
//!    re-measured a third time: unchanged to the bit again.
//! 4. A fourth re-run, after a real `catch_up` replay-order bug fix
//!    (unrelated to the hearth mechanism) moved the ABSOLUTE numbers but not
//!    the finding: `cold_live` and `cold_inert` were still exactly equal.
//! 5. A fifth re-run, after ANOTHER campaign's history/demography work
//!    (unrelated to the hearth mechanism) absorbed into main and moved seed
//!    13 from 92 to 104 settlements, broke the equality — **in the
//!    preregistered direction**: the hearth began lowering cold-built
//!    prevalence by ~0.004, entirely within one species. This was NOT
//!    claimed as confirmation (commit `13702ada`, "record the delta, claim
//!    nothing — the world outlived the prediction"): the preregistration was
//!    frozen against seed 13's 92-settlement population, that population no
//!    longer exists, and reading a favourable delta off a changed world after
//!    the fact is exactly the post-hoc move preregistration exists to refuse.
//!    What the commit shipped instead was a SAFETY property (`live.prevalence
//!    <= inert.prevalence` — a hearth must never make the population WORSE)
//!    plus a MAGNITUDE bound (`delta < 0.05`, a watchdog against the
//!    population moving further than the ~0.004 recorded here) — a moving
//!    target's assertion shape, not an equality's.
//!
//! The conclusion, reasoned all the way through (points 1–4; point 5 is a
//! separate, later event and is addressed on its own below): every
//! qualifying cold-built
//! room in seed 13 sits at a real temperature dozens of degrees past its
//! resident species' niche tolerance (species widths run 10–28°C; seed 13's
//! cold-built rooms range from a hair under the 5°C gate down to −73°C), so
//! even the FULL, undecayed `HEARTH_WARMTH` cannot move the discrete distress
//! read anywhere in that range — either the room is mild enough that the
//! niche's own tolerance already absorbs it (no baseline distress to reduce),
//! or it is cold enough that thermal urgency is already clamped to its
//! ceiling (15°C of warmth is a rounding error against a 40–80°C deviation).
//! Three independent candidate explanations (the placeholder constant, the
//! sampling gap, and the population's own scale) were each tested and ruled
//! out in turn. `WARMTH_DECAY`, `FURNISHING_COLD_C`, and `INVENTORY` were
//! never touched by any of this.
//!
//! **The Ember: the SAME mechanism, replanted — and a false premise
//! corrected.** The mechanism that measured this claim was `derive_npcs(..,
//! settlements.len(), ..)` — one NPC per settlement, on seed 13's own
//! settlement count. That count keeps rising as other campaigns'
//! history/demography work lands (92 → 104 → 290 across this file's own
//! lifetime), and the test's cost is linear in it: on the canonical box it
//! alone measured 7,260s+ against a heavy-tier slowest of 532s for everything
//! else COMBINED — it was, on its own, the heavy tier's entire wall clock.
//!
//! By the time of this replant, the claim being measured was **not**
//! bit-identity: point 5 above had already turned it into a SAFETY property
//! plus a MAGNITUDE bound (`13702ada`). This file's own plan text got that
//! wrong the first time — it reinstated `assert_eq!(cold_live, cold_inert)`
//! on the strength of points 1–4 alone, silently un-recording point 5's
//! falsification — and this revision corrects it.
//!
//! Only the SAFETY half is reinstated below, and it is the ROBUST half:
//! "never worse" holds regardless of population size, mechanism strength, or
//! where a moving world's settlement count lands, which is exactly why it
//! survives being replanted onto a small hand-built scenario in a way a
//! magnitude claim would not. The MAGNITUDE half — how much better, if any,
//! and by how much the delta may move before it means something changed — is
//! threshold-adjacent and fragile: per decision 0097 ("assert the robust half
//! in the gate; measure the fragile half in the census"), that half belongs
//! in the census as a measured rate with a sampling bound, not as a gate
//! assertion. **It is not reinstated here.** The real-population watchdog
//! point 5 justified (`delta < 0.05` against the ~0.004 recorded on the
//! 104-settlement world) is therefore currently ABSENT from both the gate and
//! the census, and is OWED — filed at idea registry
//! `TOOL-hearth-cost-scales-with-roster`, not built in this campaign.
//!
//! Separately, `cold_built_settlements_are_common_not_rare`'s own job — is
//! the joint "built and cold" condition rare? — is unaffected by any of this,
//! already a separate, cheap test in this same file.
//!
//! So `the_hearth_never_worsens_a_planted_cold_built_population`
//! (below) replants the SAME mechanism — real `run_simulation`, one hearth
//! toggle, a cold-built group and a warm-built specificity control — onto a
//! HAND-BUILT scenario in the shape of `windows/lab/src/synthetic.rs` (a
//! handful of rooms and creatures, hand-planted terrain, the real headless
//! drive loop; "no parallel harness") rather than a derived world. The
//! planted cold-built rooms sit at a representative deviation from seed 13's
//! own cold-built range (`COLD_BUILT_C`, tens of degrees past any authored
//! niche), so the same saturated-urgency regime that explained the null on
//! seed 13 is reproduced on purpose, not stumbled into by a sweep. This drops
//! the cost from "the entire heavy tier's wall clock" to a handful of
//! creatures over 40 ticks — cheap enough to run in the ordinary commit gate,
//! which is where it now lives (`#[ignore]` removed).
//!
//! **The mutation step this campaign's test never had.** A null this cheap
//! and this small is more vulnerable than the original to a different
//! failure: a harness too blunt to ever see a hearth effect, at ANY
//! parameters, would report the identical null for a reason that has nothing
//! to do with physics. `the_harness_detects_a_hearth_when_the_gap_is_small_
//! enough_to_close` (below) is the check for that: it plants a SEPARATE
//! scenario whose niche is WIDE enough (unlike `REPLANTED_NICHE`) that
//! thermal urgency stays unsaturated at every hop from the landing anchor to
//! the hearth, so a creature genuinely walks there over the run and ends up
//! measurably better off than the same creature with the hearth forced
//! inert. Its first draft used a NARROW niche instead (chosen so the
//! deviation exactly matched `HEARTH_WARMTH`) and found NO difference either
//! — not because the mechanism is inert, but because a narrow niche
//! saturates urgency to its ceiling at every intermediate anchor, so each
//! single within-room step's own `serviceability` reads `0.0` and the
//! creature never takes even the first step toward the fire. That is the
//! SAME masking `liveness.rs`'s own
//! `a_creature_crosses_a_hearth_bearing_room_but_not_a_hearthless_one` test
//! names as the reason real deep-cold populations (task 5c/5d) show no
//! within-room movement either — so debugging the mutation scenario
//! independently rediscovered the real null's own mechanism before finding
//! parameters that step outside it. The corrected scenario reuses that
//! test's own proven parameters verbatim rather than re-deriving them. Only
//! with this confirmation does the safety property above mean anything —
//! without it, "never worse" could be trivially true of a harness that never
//! registers a hearth effect at all.

use hornvale_kernel::ecology::ConditionResponse;
use hornvale_kernel::{
    ANIMAL_PREY, ConceptRegistry, EntityId, Ledger, PLANT_FORAGE, ResourceVector, RoomAddr,
    WorldTime,
};
use hornvale_lab::health::{AffectTrace, health_report, run_simulation};
use hornvale_locale::LocaleContext;
use hornvale_species::{ActivityCycle, MetabolicClass};
use hornvale_vessel::liveness::{
    AGENT_AT, DRANK, EATEN, LocaleTerrain, Npc, RESTED, Terrain, ThreatNiche, built_rooms,
    derive_npcs, place_agent,
};
use std::collections::{BTreeMap, BTreeSet};

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

/// claim: rate(forall-seed, [lo, hi]) — decision 0097's own worked example
/// of an existence-near-threshold row; 0097 prescribes converting this to
/// rate(census: ..., [lo, hi]) at n=1000 (not yet done by this campaign's
/// tranche)
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

/// A hand-planted terrain field for the replanted hearth scenarios below —
/// styled after `windows/lab/src/synthetic.rs`'s `SyntheticTerrain` (the
/// pub sibling of the vessel tests' `PlantedTerrain`), plus a `built` set
/// that module has no scenario needing: `interior_of` composes a hearth only
/// where `is_built && is_cold` both read true (`derive.rs`), so THIS file's
/// A/B needs a toggle `SyntheticTerrain` was never asked to carry. Elevation
/// is uniform `INFINITY` (the undescribable-room convention, never chosen
/// downhill) since every scenario here gives its creatures water and a
/// hearth right at home — the ignorant-exploration path that reads elevation
/// never fires. Unplanted temperature reads `INFINITY` too (thermally
/// silent, matching `SyntheticTerrain`'s convention); forage and hazards are
/// never overridden, so they take the `Terrain` trait's own defaults (fed,
/// safe) and hunger/danger stay quiet — these scenarios probe thermal
/// distress alone.
struct PlantedHearthTerrain {
    /// Rooms whose water is fresh (drinkable) — every planted room, so
    /// thirst is always serviceable in place and never itself distresses.
    fresh: BTreeSet<RoomAddr>,
    /// Per-room planted temperature (°C); unplanted rooms read `INFINITY`.
    temps: BTreeMap<RoomAddr, f64>,
    /// Rooms reading `is_built` true — the ONE thing the two arms of every
    /// A/B below differ in. Empty = "hearth forced inert" (every room reads
    /// unbuilt, the pre-Task-5b state); the real planted set = "hearth
    /// live" (the arming Task 5/5b wired).
    built: BTreeSet<RoomAddr>,
}

impl Terrain for PlantedHearthTerrain {
    fn elevation(&self, _room: &RoomAddr) -> f64 {
        f64::INFINITY
    }
    fn is_fresh_water(&self, room: &RoomAddr) -> bool {
        self.fresh.contains(room)
    }
    fn temperature(&self, room: &RoomAddr, _day: WorldTime) -> f64 {
        self.temps.get(room).copied().unwrap_or(f64::INFINITY)
    }
    fn is_built(&self, room: &RoomAddr) -> bool {
        self.built.contains(room)
    }
}

/// A comfortable, mid-range niche width drawn from the low end of the
/// authored species range (10–28°C, this file's own module doc) — wide
/// enough to be a realistic creature, narrow enough that `COLD_BUILT_C`
/// reads as a real, dozens-of-degrees deviation rather than an artificially
/// huge one.
const REPLANTED_NICHE: ConditionResponse = ConditionResponse {
    optimum: 15.0,
    width: 14.0,
    devotion: 0.5,
};

/// The cold-built rooms' planted temperature (°C) — representative of seed
/// 13's own cold-built range (this file's module doc: "a hair under the 5°C
/// gate down to −73°C"), chosen far enough past [`REPLANTED_NICHE`] that even
/// the FULL, undecayed `HEARTH_WARMTH` (15°C, `windows/vessel/src/interior/
/// field.rs`) cannot close the gap — the same saturated-urgency regime the
/// real seed-13 sweep found, reproduced on purpose rather than stumbled into.
const COLD_BUILT_C: f64 = -40.0;

/// The warm-built specificity control's planted temperature (°C) — inside
/// [`REPLANTED_NICHE`]'s band (`|20 − 15| = 5 < 14`), so it never engages the
/// thermal drive and reads `is_cold` false unconditionally: `interior_of`
/// gates a hearth on `is_built && is_cold` (`derive.rs`), so toggling `built`
/// cannot compose one here regardless.
const WARM_BUILT_C: f64 = 20.0;

/// Builds a creature with the scenario-relevant fields set and the
/// incidental ones (activity, drives other than thermal) at the same sane
/// defaults `synthetic.rs`'s own `creature` helper uses — duplicated rather
/// than shared because that helper is private to its module and this file's
/// niche varies per scenario, not per call site.
fn creature(entity: EntityId, home: RoomAddr, species: &str, niche: ConditionResponse) -> Npc {
    Npc {
        entity,
        home: home.clone(),
        resource: home,
        species: species.to_string(),
        activity: ActivityCycle::Diurnal,
        temperature_niche: niche,
        deliberation_latency: 0.5,
        time_horizon: 0.0,
        metabolic_class: MetabolicClass::Endotherm,
        // A balanced omnivore fed by the terrain's default productivity (The
        // Provender), so hunger stays quiet — these scenarios probe thermal
        // distress only.
        niche: ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)])
            .expect("the omnivore niche is valid"),
        boldness: 0.5,
        threat_niche: ThreatNiche {
            uncanny: 1.0,
            heat: 0.5,
            cold: 0.5,
            predator: 1.0,
        },
        // The action clock's reference mass, so tempo is exactly `1.0` and a
        // planted scenario's timings are the creature-independent baseline.
        mass_kg: hornvale_vessel::clock::REFERENCE_MASS_KG,
        label: species.to_string(),
    }
}

/// Reduce the traces at `idx` to a `HealthReport`, tagging each with its
/// derived species (the by-species reduction needs it; `AffectTrace` does
/// not borrow, so this clones the per-creature affect vector for each of the
/// two runs it is reduced under).
fn subgroup_report(
    idx: &[usize],
    npcs: &[Npc],
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

/// A registry with the two session-only predicates the drive tick commits —
/// the same pair `simulate_world`/`synthetic.rs`'s `harness_registry`
/// register.
fn planted_registry() -> ConceptRegistry {
    let mut registry = ConceptRegistry::default();
    let _ = registry.register_predicate(AGENT_AT, false, "an agent's position on a day");
    let _ = registry.register_predicate(DRANK, false, "an agent satisfied its sustenance goal");
    let _ = registry.register_predicate(RESTED, false, "an agent rested on a day");
    let _ = registry.register_predicate(EATEN, false, "an agent ate on a day");
    registry
}

/// A planted population: three cold-built creatures (draw a hearth when
/// `built` is armed) and two warm-built creatures (the specificity control —
/// never draw one, whatever `built` reads, since `is_cold` gates it
/// independently). Each creature sits on its own room, which is also its own
/// fresh-water source, so thirst stays serviceable and thermal is the only
/// active drive.
struct PlantedPopulation {
    ledger: Ledger,
    registry: ConceptRegistry,
    npcs: Vec<Npc>,
    cold_idx: Vec<usize>,
    warm_idx: Vec<usize>,
    fresh: BTreeSet<RoomAddr>,
    temps: BTreeMap<RoomAddr, f64>,
}

fn plant_population() -> PlantedPopulation {
    // Six well-separated rooms (one per axis direction, so each lands on a
    // distinct face of the room mesh and none are neighbours of another).
    let cold_rooms = [
        RoomAddr::containing([1.0, 0.0, 0.0], 6),
        RoomAddr::containing([-1.0, 0.0, 0.0], 6),
        RoomAddr::containing([0.0, 1.0, 0.0], 6),
    ];
    let warm_rooms = [
        RoomAddr::containing([0.0, -1.0, 0.0], 6),
        RoomAddr::containing([0.0, 0.0, 1.0], 6),
    ];

    let mut ledger = Ledger::default();
    let registry = planted_registry();
    let mut npcs = Vec::new();
    let mut cold_idx = Vec::new();
    let mut warm_idx = Vec::new();
    let mut fresh = BTreeSet::new();
    let mut temps = BTreeMap::new();

    for (i, room) in cold_rooms.iter().enumerate() {
        let e = ledger.mint_entity();
        ledger
            .commit(place_agent(e, room, WorldTime { day: 0.0 }), &registry)
            .expect("place cold-built creature");
        cold_idx.push(npcs.len());
        npcs.push(creature(
            e,
            room.clone(),
            &format!("kobold-cold-{i}"),
            REPLANTED_NICHE,
        ));
        fresh.insert(room.clone());
        // The room AND its neighbours are equally cold — no kinder
        // neighbour to flee to via the between-room comfort gradient
        // (`comfort_step`), the same "no kinder neighbour" idiom
        // `synthetic.rs`'s `a_heat_wave_that_passes` uses. An unplanted
        // neighbour reads `INFINITY` (thermally silent — `urgency_of`
        // returns `0.0` for a non-finite temperature), which would let the
        // creature simply walk away from the cold instead of ever engaging
        // the hearth this A/B is testing.
        temps.insert(room.clone(), COLD_BUILT_C);
        for n in room.neighbors() {
            temps.insert(n, COLD_BUILT_C);
        }
    }
    for (i, room) in warm_rooms.iter().enumerate() {
        let e = ledger.mint_entity();
        ledger
            .commit(place_agent(e, room, WorldTime { day: 0.0 }), &registry)
            .expect("place warm-built creature");
        warm_idx.push(npcs.len());
        npcs.push(creature(
            e,
            room.clone(),
            &format!("goblin-warm-{i}"),
            REPLANTED_NICHE,
        ));
        fresh.insert(room.clone());
        temps.insert(room.clone(), WARM_BUILT_C);
    }

    PlantedPopulation {
        ledger,
        registry,
        npcs,
        cold_idx,
        warm_idx,
        fresh,
        temps,
    }
}

/// Days simulated — matches `health.rs`'s private `HEALTH_TICKS` so this
/// scenario's chronic window matches a real health sweep's.
const REPLANTED_TICKS: usize = 40;

/// **Renamed from `..._stays_null` (The Ember).** The original name promised
/// a null — `cold_live == cold_inert`, bit-identical — but that was never
/// what the test this one replaces asserted at the commit it was replanted
/// from (`8a448c3f`): absorbing main had already moved seed 13 from 92 to
/// 104 settlements and found the hearth lowering cold-built prevalence by
/// ~0.004 (module doc, "And then the ground moved"), so the real assertion
/// by then was a SAFETY property (armed never worse) plus a magnitude bound
/// (the delta stays near where it was recorded), not equality. A test whose
/// name promises "null" while its body checks "never worse, and not too much
/// better" is a lie with a green tick beside it the moment anyone reads only
/// the name — so this one is named for the claim it actually makes.
///
/// Per decision 0097 ("assert the robust half in the gate; measure the
/// fragile half in the census"), only the safety half is asserted here. The
/// magnitude bound is fragile — threshold-adjacent, and the threshold moves
/// under other campaigns' history work — so it is NOT reinstated as a gate
/// assertion; relocating it as a measured census rate is filed as owed (see
/// the module doc and idea registry `TOOL-hearth-cost-scales-with-roster`),
/// not built in this campaign.
#[test]
fn the_hearth_never_worsens_a_planted_cold_built_population() {
    let pop = plant_population();
    let every_room: BTreeSet<RoomAddr> = pop.temps.keys().cloned().collect();

    // "Hearth live": every planted room reads built (the arming Task 5/5b
    // wired). "Hearth forced inert": none do — the pre-Task-5b state, on the
    // identical ledger/npcs/temperatures. The two arms differ in exactly ONE
    // thing, matching the real A/B's own discipline.
    let terrain_live = PlantedHearthTerrain {
        fresh: pop.fresh.clone(),
        temps: pop.temps.clone(),
        built: every_room,
    };
    let terrain_inert = PlantedHearthTerrain {
        fresh: pop.fresh,
        temps: pop.temps,
        built: BTreeSet::new(),
    };

    // Both arms take the action clock's BASE rate (`None`) — a planted
    // scenario has no sky to derive a day from, and the two arms must divide
    // the same rotation (`hearth_population_calibration.rs`'s own original
    // reasoning, unchanged).
    let traces_live = run_simulation(
        &pop.ledger,
        &pop.registry,
        &pop.npcs,
        &terrain_live,
        REPLANTED_TICKS,
        None,
    );
    let traces_inert = run_simulation(
        &pop.ledger,
        &pop.registry,
        &pop.npcs,
        &terrain_inert,
        REPLANTED_TICKS,
        None,
    );

    let cold_live = subgroup_report(&pop.cold_idx, &pop.npcs, &traces_live);
    let cold_inert = subgroup_report(&pop.cold_idx, &pop.npcs, &traces_inert);
    let warm_live = subgroup_report(&pop.warm_idx, &pop.npcs, &traces_live);
    let warm_inert = subgroup_report(&pop.warm_idx, &pop.npcs, &traces_inert);

    // THE SAFETY PROPERTY, replanted: an armed hearth must never make the
    // cold-built population WORSE. This is the ROBUST half of the original
    // claim (decision 0097) — true regardless of population size, mechanism
    // strength, or where a moving world's settlement count lands — so it
    // belongs in the gate. The FRAGILE half (how much better, if any) is
    // threshold-adjacent and is measured as a census rate, not asserted
    // here; that relocation is filed as owed, not built in this campaign
    // (module doc, idea registry `TOOL-hearth-cost-scales-with-roster`).
    //
    // At `COLD_BUILT_C = -40.0` this holds TRIVIALLY: thermal urgency clamps
    // to its ceiling (`1.0`) in BOTH arms — the planted scenario sits deep in
    // the same saturated-urgency regime the real seed-13 sweep found (module
    // doc) — so `cold_live.prevalence` and `cold_inert.prevalence` are
    // actually EQUAL at this point, not merely bounded. Read that as a fact
    // about where this scenario sits, not as a hard-won finding; the bound
    // below is what would catch a regression, not what proves the mechanism
    // does anything at this magnitude.
    assert!(
        cold_live.prevalence <= cold_inert.prevalence,
        "a hearth must never make the cold-built population WORSE: live \
         {cold_live:?} vs inert {cold_inert:?}"
    );

    // THE SPECIFICITY CONTROL: warm-built creatures never draw a hearth
    // regardless of `is_built` (the gate is `is_built && is_cold`, and
    // `is_cold` is false for them), so toggling `built` must leave them
    // completely unaffected either way — this is expected, not a finding,
    // and its failure would mean the toggle leaked into a population it has
    // no business touching.
    assert_eq!(
        warm_live, warm_inert,
        "the warm-built control must be unaffected by the built-territory \
         toggle: {warm_live:?} vs {warm_inert:?}"
    );
}

/// A niche WIDE enough that thermal urgency stays STRICTLY DECREASING and
/// UNSATURATED at every hop from the landing anchor to the hearth, rather
/// than clamped to `1.0` at several of them — the same parameters
/// `liveness.rs`'s own `a_creature_crosses_a_hearth_bearing_room_but_not_a_
/// hearthless_one` test proves this on directly, reused verbatim rather than
/// re-derived. This is the trap the naive first attempt at this test fell
/// into: a NARROWER niche (e.g. `width: 5.0`) saturates the felt reading at
/// EVERY intermediate anchor to the same `1.0`, so each single-hop
/// `MoveWithin`'s own `serviceability` (the reduction between the CURRENT
/// hop and the NEXT one, not current-to-hearth) reads `0.0` and the creature
/// never takes even the first step — masking the very effect this test
/// exists to demonstrate, the SAME masking `liveness.rs`'s comment there
/// says explains why real deep-cold populations (task 5c/5d) show no
/// within-room movement either. That masking is real physics this file's own
/// null test relies on (`REPLANTED_NICHE`/`COLD_BUILT_C` are deliberately IN
/// that saturated regime); this scenario exists only to step OUTSIDE it, so
/// the harness proves it CAN see movement's effect when the gradient permits
/// it.
const MUTATION_NICHE: ConditionResponse = ConditionResponse {
    optimum: 6.0,
    width: 12.0,
    devotion: 0.5,
};

/// The mutation room's planted temperature (°C) — reused verbatim from
/// `liveness.rs`'s own proof that this exact `(optimum, width, ambient)`
/// triple keeps urgency unsaturated at every hop of the landing-to-hearth
/// chain (three hops: `1.875`/`3.75`/`7.5`/`15` °C of warmth at
/// door/ground/alcove/hearth) while still reading distress at the door and
/// full comfort at the fire.
const MUTATION_COLD_C: f64 = -19.75;

/// **THE MUTATION STEP.** A safety property this cheap and this small is
/// more vulnerable than the original, expensive measurement to a different
/// failure mode: a harness too blunt to ever register a hearth effect, at ANY
/// parameters, would report the same "no difference" for a reason that has
/// nothing to do with physics. This test plants a SEPARATE,
/// deliberately-engineered scenario — one creature, one room, `MUTATION_NICHE`
/// WIDE enough (unlike the narrow first draft `MUTATION_NICHE`'s own doc
/// describes and discards above) that urgency stays unsaturated at every hop
/// from the landing anchor to the hearth — and asserts the two arms
/// genuinely DIFFER, in the preregistered direction (armed helps). The
/// shipped scenario's deviation from niche optimum is `25.75`°C
/// (`MUTATION_NICHE.optimum − MUTATION_COLD_C == 6.0 − (−19.75)`), well past
/// `HEARTH_WARMTH`'s `15.0` — it is the WIDE niche, not a matched deviation,
/// that keeps the gradient climbable. Without this test, the safety property
/// above would be unfalsifiable by construction; with it, "never worse" is a
/// real finding about the mechanism at realistic magnitudes, not an artifact
/// of an instrument that can never see anything.
#[test]
fn the_harness_detects_a_hearth_when_the_gap_is_small_enough_to_close() {
    let room = RoomAddr::containing([0.0, 0.0, -1.0], 6);
    let mut ledger = Ledger::default();
    let registry = planted_registry();
    let e = ledger.mint_entity();
    ledger
        .commit(place_agent(e, &room, WorldTime { day: 0.0 }), &registry)
        .expect("place mutation creature");
    let npc = creature(e, room.clone(), "kobold-mutation", MUTATION_NICHE);

    let fresh: BTreeSet<RoomAddr> = [room.clone()].into_iter().collect();
    let mut temps = BTreeMap::new();
    // The room AND its neighbours are equally cold (see `plant_population`'s
    // identical comment) — no kinder neighbour to flee to, so the ONLY
    // escape from the cold is the within-room hearth this test exists to
    // detect.
    temps.insert(room.clone(), MUTATION_COLD_C);
    for n in room.neighbors() {
        temps.insert(n, MUTATION_COLD_C);
    }
    let built: BTreeSet<RoomAddr> = [room.clone()].into_iter().collect();

    let terrain_live = PlantedHearthTerrain {
        fresh: fresh.clone(),
        temps: temps.clone(),
        built,
    };
    let terrain_inert = PlantedHearthTerrain {
        fresh,
        temps,
        built: BTreeSet::new(),
    };

    let npcs = [npc.clone()];
    let traces_live = run_simulation(
        &ledger,
        &registry,
        &npcs,
        &terrain_live,
        REPLANTED_TICKS,
        None,
    );
    let traces_inert = run_simulation(
        &ledger,
        &registry,
        &npcs,
        &terrain_inert,
        REPLANTED_TICKS,
        None,
    );

    let live = health_report(&[AffectTrace {
        species: npc.species.clone(),
        affects: traces_live[0].clone(),
    }]);
    let inert = health_report(&[AffectTrace {
        species: npc.species.clone(),
        affects: traces_inert[0].clone(),
    }]);

    assert_ne!(
        live, inert,
        "the harness must be able to detect A hearth effect SOMEWHERE, or \
         the safety property above proves nothing about the mechanism (only \
         about an instrument that can never see anything). The likeliest \
         cause of a future failure here is NOT the harness: the landing \
         anchor's urgency in this scenario sits at ~0.98958, about 1% below \
         the clamp ceiling of 1.0, close to the same saturated-urgency \
         regime `REPLANTED_NICHE`/`COLD_BUILT_C` sit deep in. A downward \
         recalibration of `HEARTH_WARMTH`, a higher `WARMTH_DECAY` (either \
         shrinks the felt warmth at intermediate hops), or an extra hop added \
         to the landing-to-hearth chain (more decay before the first step) \
         can each tip that last ~1% to 1.0 and stall the within-room walk \
         before it starts — look at those three constants first, not at \
         `run_simulation` or the planner: live {live:?} vs inert {inert:?}"
    );
    assert!(
        live.prevalence < inert.prevalence,
        "and the direction must be the preregistered one — an armed hearth \
         must help, not hurt: live {} vs inert {}",
        live.prevalence,
        inert.prevalence
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
/// task 6b: the replanted null above samples warmth at wherever a creature's
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
    let terrain = LocaleTerrain::with_fields(&ctx, None, None, None, Some(&built), None);

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
