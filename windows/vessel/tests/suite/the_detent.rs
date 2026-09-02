//! The Detent's witnesses (spec §4 H5/H6, §3 rules 2/4/5, M1). Counts, not
//! clocks: every number here is deterministic on every box.

use crate::common;
use hornvale_kernel::{Ledger, RoomMeshMemo, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_vessel::body::Body;
use hornvale_vessel::ground::{GroundHazards, OwnedGround};
use hornvale_vessel::liveness::{
    AGENT_AT, DRANK, DriveMovements, EATEN, HazardMemory, HomeNavCache, LocaleTerrain,
    PrimaryAfraidMemo, RESTED, SLEPT, SUSTENANCE, Terrain, derive_npcs, hazard_memory_memo,
};
use hornvale_vessel::resident::{OwnedFolds, ResidentFolds};

/// Spec §4 H5's shape: seed 42, 50 derived agents, 60 ticks.
pub const H5_SEED: u64 = 42;
pub const H5_AGENTS: usize = 50;
pub const H5_TICKS: usize = 60;

/// Everything the bench holds after `ticks` ticks, so a witness can probe it.
/// `world` and `day_ticks` have no reader yet in Task 1's own witness — later
/// tasks in this campaign read them, per the module doc's "reusable by every
/// later task" — so `dead_code` is allowed on the struct rather than trimmed.
#[allow(dead_code)]
pub struct BenchShape {
    pub world: hornvale_kernel::World,
    pub ctx: LocaleContext,
    pub ledger: Ledger,
    pub npcs: Vec<Body>,
    pub folds: OwnedFolds,
    pub mesh_memo: RoomMeshMemo,
    pub day: WorldTime,
    pub day_ticks: Option<hornvale_kernel::units::TickSpan>,
    /// The bench's own room memo (The Detent, spec §2.1): one per bench run,
    /// handed to every tick's terrain, so H5's witnesses can read its
    /// `misses()` — the field samples the memo actually took.
    pub ground: OwnedGround,
    /// Per tick, the `hazards()` calls the tick's own walk made, in order.
    pub hazards_per_tick: Vec<u64>,
    /// Per tick, the `ground` memo's `misses()` delta — the field samples the
    /// tick's own walk took, once the memo exists.
    pub samples_per_tick: Vec<u64>,
    /// Per tick, the facts committed.
    pub facts_per_tick: Vec<usize>,
    /// Per tick, the ROOMS the frightening-verdict index judged — the delta of
    /// `ReadWitness::ground_judged()` across the tick (The Detent, spec §4's
    /// H6). A room already judged is skipped inside
    /// `FrighteningGround::advance` and adds nothing here, so this IS the
    /// "new sightings" H6 claims the scan's work is proportional to.
    pub judged_per_tick: Vec<u64>,
    /// Per tick, the roster's DISTINCT visited rooms summed over its members
    /// (`LatestVisit::of(e).len()`) — H6's denominator, the quantity the
    /// pre-index scan judged in full on every tick.
    pub distinct_rooms_per_tick: Vec<usize>,
}

/// `session_length_scaling.rs`'s construction, counted: the world at `seed`,
/// `agents` derived bodies, `ticks` ticks of `DriveMovements::step_with_occupancy`
/// over one caller-owned store, mesh memo and nav cache, with every tick's
/// terrain wrapped in a `CountingTerrain`.
pub fn bench_shape(seed: u64, ticks: usize, agents: usize) -> BenchShape {
    let world = common::build(seed).expect("the seed builds a world");
    let ctx = LocaleContext::build(&world).expect("the locale context builds");
    let home_settlement = hornvale_settlement::village_info(&world)
        .expect("the flagship always exists")
        .id;
    let day_ticks = hornvale_worldgen::sky_of(&world)
        .ok()
        .and_then(|sky| sky.calendar().cloned())
        .and_then(|c| c.day_ticks());
    let mut ledger = world.ledger.clone();
    let mut registry = world.registry.clone();
    for (pred, doc) in [
        (AGENT_AT, "an agent's position on a day"),
        (DRANK, "an agent satisfied its sustenance goal"),
        (RESTED, "an agent rested on a day, for this many ticks"),
        (SLEPT, "an agent slept on a day, for this many ticks"),
        (EATEN, "an agent ate (eased its hunger) on a day"),
    ] {
        registry
            .register_predicate(pred, false, doc)
            .expect("the drive predicates register identically every run");
    }
    let npcs = derive_npcs(&world, &ctx, &mut ledger, agents, home_settlement);
    assert_eq!(
        npcs.len(),
        agents,
        "derive_npcs must yield the roster asked for"
    );
    let mut mesh_memo = RoomMeshMemo::new();
    let mut home_nav_cache = HomeNavCache::new();
    let folds = OwnedFolds::new(ResidentFolds::new());
    let ground: OwnedGround = OwnedGround::new(GroundHazards::new());
    let mut day = WorldTime::from_std_days(0.5).expect("0.5 is a finite day count");
    let mut hazards_per_tick = Vec::with_capacity(ticks);
    let mut samples_per_tick = Vec::with_capacity(ticks);
    let mut facts_per_tick = Vec::with_capacity(ticks);
    let mut judged_per_tick = Vec::with_capacity(ticks);
    let mut distinct_rooms_per_tick = Vec::with_capacity(ticks);
    for _ in 0..ticks {
        let from = day;
        day = WorldTime::from_ticks(day.ticks() + WorldTime::TICKS_PER_STD_DAY);
        let mesh_snapshot = mesh_memo.clone();
        let base = LocaleTerrain::with_fields(&ctx, None, None, None, None, Some(&mesh_snapshot))
            .with_ground(&ground);
        let terrain = common::CountingTerrain::new(&base);
        let sys = DriveMovements {
            npcs: npcs.clone(),
            from,
            to: day,
            params: SUSTENANCE,
            day_ticks,
            terrain: &terrain,
            folds: &folds,
        };
        let samples_before = ground.borrow().misses();
        let judged_before = folds.borrow().witness().ground_judged();
        let (facts, _occupancy) =
            sys.step_with_occupancy(&ledger, &mut mesh_memo, &mut home_nav_cache);
        facts_per_tick.push(facts.len());
        for fact in facts {
            ledger
                .commit(fact, &registry)
                .expect("a drive-movements fact commits");
        }
        hazards_per_tick.push(terrain.hazards_calls());
        samples_per_tick.push(ground.borrow().misses() - samples_before);
        judged_per_tick.push(folds.borrow().witness().ground_judged() - judged_before);
        // The roster's distinct visited rooms, AFTER this tick's facts are
        // committed — read under `borrow_mut` because `latest_visit_and_trail`
        // advances the store's tenants to the ledger's end first.
        distinct_rooms_per_tick.push({
            let mut store = folds.borrow_mut();
            let (visits, _) = store.latest_visit_and_trail(&ledger);
            npcs.iter().map(|b| visits.of(b.entity).len()).sum()
        });
    }
    BenchShape {
        world,
        ctx,
        ledger,
        npcs,
        folds,
        mesh_memo,
        day,
        day_ticks,
        ground,
        hazards_per_tick,
        samples_per_tick,
        facts_per_tick,
        judged_per_tick,
        distinct_rooms_per_tick,
    }
}

/// The roster member with the most `agent-at` facts — the bench's probe rule.
pub fn probe_index(shape: &BenchShape) -> usize {
    let counts: Vec<usize> = shape
        .npcs
        .iter()
        .map(|b| shape.ledger.facts_of(b.entity, AGENT_AT).count())
        .collect();
    counts
        .iter()
        .enumerate()
        .max_by_key(|(_, c)| **c)
        .map(|(i, _)| i)
        .expect("the roster is non-empty")
}

/// The counts one `hazard_memory_memo` call makes on the probe, with a FRESH
/// `PrimaryAfraidMemo` (the bench's shape), a second call on the SAME memo
/// (production's per-creature read after the tick's scan exists), and a
/// THIRD call on ANOTHER fresh `PrimaryAfraidMemo` (production's own
/// per-tick shape: a new memo every tick, over the SAME room memo). All
/// three read through `shape.ground` (The Detent, spec §2.1), so
/// `warm_samples`/`second_fresh_samples` are the FIELD samples (room-memo
/// misses) the second and third reads took — H5's first clause's subject.
pub struct ProbeCounts {
    pub fresh_hazards: u64,
    pub warm_hazards: u64,
    pub scans_delta: u64,
    pub with_emitters_delta: u64,
    pub replays_delta: u64,
    pub shunned: usize,
    /// Field samples (room-memo misses) the FIRST read took — the fresh
    /// `PrimaryAfraidMemo`, over whatever `shape.ground` already held
    /// coming in (typically warm already, from the bench's own tick loop).
    /// Kept and exposed so a reader can see the delta arithmetic
    /// `warm_samples`/`second_fresh_samples` depend on, not just their
    /// (zero) result.
    pub first_fresh_samples: u64,
    /// Field samples (room-memo misses) the SECOND read took — the same
    /// `PrimaryAfraidMemo`, already warm.
    pub warm_samples: u64,
    /// Field samples (room-memo misses) the THIRD read took — a fresh
    /// `PrimaryAfraidMemo` over the same, already-warm room memo.
    pub second_fresh_samples: u64,
}

pub fn probe_counts(shape: &BenchShape) -> ProbeCounts {
    let pi = probe_index(shape);
    let npc = &shape.npcs[pi];
    let mesh = shape.mesh_memo.clone();
    let base = LocaleTerrain::with_fields(&shape.ctx, None, None, None, None, Some(&mesh))
        .with_ground(&shape.ground);
    let terrain = common::CountingTerrain::new(&base);
    let w0 = {
        let s = shape.folds.borrow();
        let w = s.witness();
        (
            w.emitter_scans(),
            w.emitter_scans_with_emitters(),
            w.alarm_replays(),
        )
    };
    let mut memo = PrimaryAfraidMemo::new();
    let samples_before_first = shape.ground.borrow().misses();
    let first = hazard_memory_memo(
        &shape.ledger,
        &shape.folds,
        npc,
        shape.day,
        &terrain,
        &shape.npcs,
        &mut memo,
    );
    let fresh_hazards = terrain.hazards_calls();
    let samples_after_first = shape.ground.borrow().misses();
    let w1 = {
        let s = shape.folds.borrow();
        let w = s.witness();
        (
            w.emitter_scans(),
            w.emitter_scans_with_emitters(),
            w.alarm_replays(),
        )
    };
    terrain.reset();
    let second = hazard_memory_memo(
        &shape.ledger,
        &shape.folds,
        npc,
        shape.day,
        &terrain,
        &shape.npcs,
        &mut memo,
    );
    assert_eq!(
        first, second,
        "two reads of one instant over one ledger must agree"
    );
    let warm_hazards = terrain.hazards_calls();
    let samples_after_second = shape.ground.borrow().misses();

    // A THIRD read, on ANOTHER fresh `PrimaryAfraidMemo` — production's own
    // per-tick shape (a new memo every tick, over the same session-lived
    // room memo). H5's first clause is about THIS read: a second FRESH-memo
    // read in the same tick must take no field samples either, once the
    // room memo already holds everything the walk touches.
    terrain.reset();
    let mut second_memo = PrimaryAfraidMemo::new();
    let third = hazard_memory_memo(
        &shape.ledger,
        &shape.folds,
        npc,
        shape.day,
        &terrain,
        &shape.npcs,
        &mut second_memo,
    );
    assert_eq!(
        first, third,
        "a third read of the same instant, with a fresh PrimaryAfraidMemo, must agree too"
    );
    let samples_after_third = shape.ground.borrow().misses();

    ProbeCounts {
        fresh_hazards,
        warm_hazards,
        scans_delta: w1.0 - w0.0,
        with_emitters_delta: w1.1 - w0.1,
        replays_delta: w1.2 - w0.2,
        shunned: first.shunned.len(),
        first_fresh_samples: samples_after_first - samples_before_first,
        warm_samples: samples_after_second - samples_after_first,
        second_fresh_samples: samples_after_third - samples_after_second,
    }
}

/// The witness Task 1 commits: floors and prints, no threshold yet. Task 4
/// and Task 6 add H5's assertions in the task that makes each green, and
/// record the red they saw first.
#[test]
fn h5_witness_the_hazard_reads_terrain_samples_on_the_bench_shape() {
    let shape = bench_shape(H5_SEED, H5_TICKS, H5_AGENTS);
    let counts = probe_counts(&shape);
    let distinct_rooms: usize = {
        let mut store = shape.folds.borrow_mut();
        let (visits, _) = store.latest_visit_and_trail(&shape.ledger);
        shape.npcs.iter().map(|b| visits.of(b.entity).len()).sum()
    };
    let last_tick = *shape.hazards_per_tick.last().expect("ticks ran");
    println!("--- H5 witness: seed {H5_SEED}, {H5_AGENTS} agents, tick {H5_TICKS} ---");
    println!(
        "probe: FRESH memo {} hazards() calls, WARM memo {}, scans +{} (with emitters +{}), \
         alarm replays +{}, shunned {}, first-fresh samples {}, warm samples {}, \
         second-fresh samples {}",
        counts.fresh_hazards,
        counts.warm_hazards,
        counts.scans_delta,
        counts.with_emitters_delta,
        counts.replays_delta,
        counts.shunned,
        counts.first_fresh_samples,
        counts.warm_samples,
        counts.second_fresh_samples,
    );
    println!(
        "whole tick {H5_TICKS}: {last_tick} hazards() calls, {} facts committed; roster distinct rooms {distinct_rooms}",
        shape.facts_per_tick.last().copied().unwrap_or(0)
    );
    // The memo's own shape, so a reader sees it was actually exercised —
    // NOT trusted from the zero deltas above, which an unthreaded terrain
    // (`.with_ground` dropped from `bench_shape`'s or `probe_counts`'s
    // construction) would also produce, vacuously.
    let ground_misses = shape.ground.borrow().misses();
    let ground_hits = shape.ground.borrow().hits();
    let ground_len = shape.ground.borrow().len();
    println!("ground memo: {ground_misses} misses, {ground_hits} hits, {ground_len} rooms held");
    assert!(
        ground_misses > 0,
        "the memo was never filled — is the terrain built with .with_ground?"
    );
    assert!(
        ground_hits > 0,
        "the memo was never READ on a hit — the second read did not go through the memo"
    );
    assert_eq!(
        ground_misses as usize, ground_len,
        "every miss inserts exactly one room and nothing evicts here — a mismatch means a \
         second memo or a leak"
    );
    // Denominators: the path was reached, the population is the one the
    // mechanism is worst for, and the tick did real work.
    assert!(
        distinct_rooms > 0,
        "the roster visited no rooms — the walk did nothing"
    );
    assert!(
        counts.scans_delta == 1,
        "one fresh read must build exactly one emitter scan"
    );
    assert!(
        counts.fresh_hazards > 0,
        "the fresh read must sample terrain at all, or the count is zero of zero"
    );
    assert!(
        last_tick > 0,
        "the last tick must have sampled terrain, or the tick did not run the fear path"
    );
    assert!(
        shape.facts_per_tick.iter().sum::<usize>() > 0,
        "no facts committed over the run"
    );

    // H5, first clause (spec §4): field samples. A second fresh-memo read
    // in the same tick samples the field ZERO times once the memo exists.
    assert_eq!(
        counts.warm_samples, 0,
        "H5: a repeated read must take no field samples"
    );
    assert_eq!(
        counts.second_fresh_samples, 0,
        "H5: a second FRESH-memo read must take no field samples either"
    );

    // H5, whole-tick clause: the tick's own field samples against its
    // hazards() calls, on the LAST tick — the same one `last_tick` reads.
    let last_samples = *shape.samples_per_tick.last().expect("ticks ran");
    println!(
        "whole tick {H5_TICKS}: {last_samples} field samples against {last_tick} hazards() calls"
    );
    assert!(
        last_samples * 10 <= last_tick,
        "H5: the tick's field samples must be at most a tenth of its hazards() calls ({last_samples} vs {last_tick})"
    );
    assert!(
        last_samples <= 4_469,
        "H5: at most 4,469 field samples in tick 60 (from 44,694)"
    );

    // H5, SECOND READING (Task 6, spec §4): not just the field samples — the
    // `hazards()` CALLS are gone too, because the verdict index answers
    // without asking the terrain at all. Task 4 made the memo absorb the
    // samples; this task removes the questions.
    assert_eq!(
        counts.warm_hazards, 0,
        "H5: a repeated read makes no hazards() calls at all"
    );
    assert!(
        last_tick <= 4_469,
        "H5: the whole tick makes at most 4,469 hazards() calls (from 44,694), saw {last_tick}"
    );

    // H6 (spec §4): the scan's work per tick is O(new sightings). Across
    // ticks 15 -> 60 the rooms the index JUDGES per tick grow strictly slower
    // than the roster's distinct visited rooms; the margin is printed.
    //
    // **THE LATE COMPARISON POINT IS NOT TICK 60, AND THAT IS A FINDING
    // RATHER THAN A CONVENIENCE.** H6 was frozen against tick 60 and tick 60
    // judges ZERO rooms on this shape: the roster stops discovering ground it
    // has never stood on well before then, so `judged_per_tick` reaches 0 and
    // stays there. A ratio with a zero numerator is 0, which would satisfy the
    // inequality below for the wrong reason — "the scan did no work" is not
    // "the scan's work grew slowly", and an instrument that cannot tell them
    // apart is measuring nothing. So the late point is the LAST tick that
    // judged anything at all, named in the print and in the failure message,
    // and the zero-numerator guard stays as the thing that forced the choice
    // into the open. The whole profile is printed so a reader can see the
    // decay rather than take two points on trust.
    let judged_15 = shape.judged_per_tick[14];
    let rooms_15 = shape.distinct_rooms_per_tick[14];
    let late = shape
        .judged_per_tick
        .iter()
        .rposition(|j| *j > 0)
        .expect("some tick judged a room, or the index was never advanced at all");
    let judged_late = shape.judged_per_tick[late];
    let rooms_late = shape.distinct_rooms_per_tick[late];
    println!("H6: judged/tick profile {:?}", shape.judged_per_tick);
    println!(
        "H6: tick 60 judged {} rooms; the last tick that judged anything is tick {} \
         ({judged_late} rooms). Comparison: tick 15 -> tick {}: judged {judged_15} -> \
         {judged_late}; distinct rooms {rooms_15} -> {rooms_late}",
        shape.judged_per_tick[59],
        late + 1,
        late + 1,
    );
    assert!(
        late + 1 > 15,
        "H6: the last judging tick ({}) is not after tick 15, so there is no interval to \
         measure growth over",
        late + 1
    );
    assert!(
        judged_15 > 0 && judged_late > 0,
        "H6 denominator: the scan must have judged rooms on BOTH comparison ticks, saw \
         {judged_15} and {judged_late} — a zero numerator makes the ratio below undefined, \
         not favourable"
    );
    let judged_growth = judged_late as f64 / judged_15 as f64;
    let rooms_growth = rooms_late as f64 / rooms_15 as f64;
    println!(
        "H6: judged growth {judged_growth:.4}x against distinct-room growth {rooms_growth:.4}x \
         (margin {:.4}x)",
        rooms_growth / judged_growth
    );
    assert!(
        judged_growth < rooms_growth,
        "H6: judged rooms per tick must grow slower than the roster's distinct rooms \
         ({judged_growth:.4}x vs {rooms_growth:.4}x)"
    );
}

use crate::ledger_hash_witness::{EMITTER_SEED, fnv1a, run_emitter_witness, run_fixed_script};
use hornvale_vessel::{PossessOpts, Session};

/// CAMPAIGN-TIME constants (decision 0541): minted at Task 2 from the merge
/// base, re-recorded MAIN-FIRST after every absorption, retired at close.
/// They equal "the whole walk's behaviour on one seed" and redden on ANY
/// behaviour change by any campaign; that is their job for exactly as long
/// as this campaign's pre-fix code exists to diverge from.
///
/// **`DETENT_SEED_42_LEDGER` is BLIND to the fear path.** It witnesses the
/// walk's byte-identity, not the fear fold: seed 42's residents carry no
/// fear verdict that ever reaches a route, for the reason
/// `ledger_hash_witness.rs`'s own "Seed 42 is not it" note (around lines
/// 359-368) already gives in full — do not restate the mechanism here, read
/// it there. This campaign's own control (Task 2's report) confirms it
/// empirically: moving `DANGER_ACT` from 0.3 to 0.05 moved both emitter
/// hashes and left this one unchanged. A green
/// `the_detent_seed_42_walk_matches_the_campaign_time_constant` is therefore
/// NOT evidence the fear path is unchanged — `DETENT_EMITTER_LEDGER` and
/// `DETENT_EMITTER_HAZARD` are the load-bearing pair for that claim.
pub(crate) const DETENT_SEED_42_LEDGER: u64 = 0xabc4_731e_5cf1_ab21;
pub(crate) const DETENT_EMITTER_LEDGER: u64 = 0xc851_e64b_0105_38b2;
pub(crate) const DETENT_EMITTER_HAZARD: u64 = 0xa9f1_7d82_c183_2854;

#[test]
fn the_detent_seed_42_walk_matches_the_campaign_time_constant() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 starts");
    run_fixed_script(&mut session);
    let hash = fnv1a(session.session_ledger_json().as_bytes());
    println!("the-detent seed-42 ledger hash: {hash:#018x}");
    assert_eq!(
        hash, DETENT_SEED_42_LEDGER,
        "the seed-42 walk moved — a fold changed a creature's route (this witness is BLIND to the fear path; see the doc comment above and DETENT_EMITTER_LEDGER/DETENT_EMITTER_HAZARD)"
    );
}

#[test]
fn the_detent_emitter_walk_matches_the_campaign_time_constants() {
    let world = common::build(EMITTER_SEED).expect("the emitter seed builds");
    let run = run_emitter_witness(&world);
    println!(
        "the-detent emitter: ledger {:#018x} hazard {:#018x} over {} bodies, {} shunned, {} dread, {} replays",
        run.ledger_hash, run.hazard_hash, run.bodies, run.shunned, run.dread, run.replays
    );
    assert!(
        run.replays > 0,
        "the emitter seed must reach the past-day affect replay or this constant witnesses the terrain-only path"
    );
    assert!(run.shunned > 0, "the hazard digest must be non-empty");
    assert_eq!(run.ledger_hash, DETENT_EMITTER_LEDGER);
    assert_eq!(run.hazard_hash, DETENT_EMITTER_HAZARD);
}

/// Chaos eviction on the room memo (The Detent, spec §2.1's own remedy for the
/// key-completeness bug `GroundHazards::hazards_or_insert_with` cannot itself
/// rule out): drop the memo at every legal opportunity and assert the fold's
/// answer is unchanged. 30 reads of [`hazard_memory_memo`] on the bench
/// shape's probe, `evict_all` alternated on and off, each read taking a FRESH
/// `PrimaryAfraidMemo` (production's own per-tick shape) but the SAME
/// `GroundHazards` — every result must equal the first, byte for byte.
#[test]
fn ground_memo_survives_chaos_eviction() {
    let shape = bench_shape(H5_SEED, 20, 10);
    let pi = probe_index(&shape);
    let npc = &shape.npcs[pi];
    let mesh = shape.mesh_memo.clone();
    let ground: OwnedGround = OwnedGround::new(GroundHazards::new());
    let terrain = LocaleTerrain::with_fields(&shape.ctx, None, None, None, None, Some(&mesh))
        .with_ground(&ground);

    let mut first: Option<HazardMemory> = None;
    let mut last_misses = 0u64;
    let mut a_miss_grew_on_eviction = false;
    for i in 0..30 {
        let evicted = i % 2 == 0;
        if evicted {
            ground.borrow_mut().evict_all();
        }
        let mut memo = PrimaryAfraidMemo::new();
        let result = hazard_memory_memo(
            &shape.ledger,
            &shape.folds,
            npc,
            shape.day,
            &terrain,
            &shape.npcs,
            &mut memo,
        );
        match &first {
            None => first = Some(result.clone()),
            Some(f) => assert_eq!(
                &result, f,
                "read {i} (evicted={evicted}) must equal the first read — a room memo with an \
                 incomplete key would diverge exactly here"
            ),
        }
        let misses_now = ground.borrow().misses();
        if evicted && misses_now > last_misses {
            a_miss_grew_on_eviction = true;
        }
        last_misses = misses_now;
    }
    assert!(
        a_miss_grew_on_eviction,
        "an eviction must force at least one recompute, or this test denominates nothing"
    );

    // The index is fully advanced before the comparator below is stated,
    // which is the fact that makes it what it is. Asserted, not assumed: the
    // bench's own tick loop reads the hazard fold for every member, so by the
    // time the 30 reads above run, `FrighteningGround` holds a verdict for
    // every room every member has stood in.
    {
        let mut store = shape.folds.borrow_mut();
        let (visits, _) = store.latest_visit_and_trail(&shape.ledger);
        let rooms: Vec<usize> = shape
            .npcs
            .iter()
            .map(|m| visits.of(m.entity).len())
            .collect();
        drop(store);
        let store = shape.folds.borrow();
        let ground_index = store.frightening_ground();
        for (m, visited) in shape.npcs.iter().zip(&rooms) {
            assert_eq!(
                ground_index.judged(m.entity),
                *visited,
                "the verdict index must hold a verdict for every room this member has stood in, \
                 or the comparator below is measuring a half-warm index"
            );
        }
    }

    // The independent comparator: the EXACT set of rooms a fresh read
    // samples, derived from `build_emitter_scan`'s own pass (read, not
    // assumed) rather than from `GroundHazards` itself.
    //
    // **It used to be the whole visited set and its halo, and The Detent's
    // verdict index is why it is not any more.** The scan judges every roster
    // member, and each member's VISITED rooms are now answered out of
    // `FrighteningGround` — judged once, held, never re-asked — so a read
    // over a warm index asks the terrain about exactly one room per member:
    // its `home`, which is not a sighting and so is deliberately not held in
    // the index (see `build_emitter_scan`). `threat_field` samples a room and
    // `room.neighbors()` together, so the expected set is each member's home
    // plus its neighbours, and nothing else. Emptying the room memo does not
    // change that: the index is in the STORE, and `evict_all` touches only the
    // memo.
    let expected_rooms: std::collections::BTreeSet<hornvale_kernel::Facet> = {
        let mut rooms = std::collections::BTreeSet::new();
        for m in &shape.npcs {
            rooms.insert(m.home.clone());
            rooms.extend(m.home.neighbors());
        }
        rooms
    };
    // The last of the 30 reads (i = 29, odd) ran with no eviction: its
    // resident room count is what the un-evicted memo actually holds, and
    // every read samples the SAME deterministic set over this fixed ledger.
    println!(
        "ground memo len after the final un-evicted read: {} (independently computed: {})",
        ground.borrow().len(),
        expected_rooms.len()
    );
    assert_eq!(
        ground.borrow().len(),
        expected_rooms.len(),
        "the memo's resident room count must equal the independently computed sample set, \
         or GroundHazards is keyed on something other than the room `hazards()` actually reads"
    );
}

/// The room memo belongs to one `(LocaleContext, predator field)` pair, not
/// to a `LocaleTerrain` value (The Detent, spec §2.1's ownership rule): two
/// terrains over DIFFERENT fields, each with its own memo, must answer as
/// themselves; two terrains SHARING one memo alias, which is exactly the
/// shape the ownership rule forbids and this test demonstrates rather than
/// assumes.
#[test]
fn a_room_memo_belongs_to_one_predator_field_and_a_second_field_gets_its_own() {
    let world = common::build(42).expect("seed 42 builds");
    let (session, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 starts");
    let ctx = LocaleContext::build(&world).expect("ctx");
    // A room where the predator field is non-zero: search the flagship's
    // neighbourhood through the session's own predator-bearing terrain.
    let with_field = session.terrain_for_tests();
    let probe = session
        .bodies()
        .iter()
        .flat_map(|b| std::iter::once(b.home.clone()).chain(b.home.neighbors()))
        .find(|r| with_field.hazards(r).predator > 0.0)
        .expect(
            "some room near the roster carries predator pressure — the field is non-zero on \
             the flagship since The Quarry",
        );
    let without = LocaleTerrain::with_fields(&ctx, None, None, None, None, None);
    let field_only = with_field.hazards(&probe);
    let bare = without.hazards(&probe);
    assert!(
        field_only.predator > bare.predator,
        "the two terrains must disagree on this room, or the test cannot see sharing"
    );

    // Ownership: each terrain its own memo -> each answers as itself.
    let g1: OwnedGround = OwnedGround::new(GroundHazards::new());
    let g2: OwnedGround = OwnedGround::new(GroundHazards::new());
    let t1 = session.terrain_for_tests().with_ground(&g1);
    let t2 = LocaleTerrain::with_fields(&ctx, None, None, None, None, None).with_ground(&g2);
    assert_eq!(t1.hazards(&probe), field_only);
    assert_eq!(t2.hazards(&probe), bare);
    assert_eq!((g1.borrow().misses(), g2.borrow().misses()), (1, 1));

    // The refused shape, demonstrated: one memo handed to both terrains makes
    // the second terrain answer with the first's field. This is the aliasing
    // the ownership rule forbids, shown rather than assumed.
    let shared: OwnedGround = OwnedGround::new(GroundHazards::new());
    let s1 = session.terrain_for_tests().with_ground(&shared);
    let s2 = LocaleTerrain::with_fields(&ctx, None, None, None, None, None).with_ground(&shared);
    let _ = s1.hazards(&probe);
    assert_eq!(
        s2.hazards(&probe),
        field_only,
        "a shared memo aliases — which is why a memo is owned by one (context, field)"
    );
}
