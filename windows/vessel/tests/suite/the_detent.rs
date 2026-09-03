//! The Detent's witnesses (spec §4 H5/H6, §3 rules 2/4/5, M1). Counts, not
//! clocks: every number here is deterministic on every box.
//!
//! # THE CAMPAIGN-TIME CONSTANTS RETIRED AT THE CLOSE (2026-09-03)
//!
//! Decision 0541 mints a hash constant for the duration of a migration and
//! retires it at the campaign's close, because a constant here equals "the
//! whole walk's behaviour on one seed" and therefore reddens on ANY behaviour
//! change by ANY campaign — a tax on work that has nothing to do with these
//! folds, and an unwinnable race against a queue that gates main+branch rather
//! than a branch tip. This campaign minted three, used them through five
//! absorptions, and retires them here.
//!
//! **What replaces them, and what that costs.** The two witnesses below now run
//! their fixed script TWICE, on two fresh sessions of one seed, and require the
//! two runs to agree — the constant-free shape `ledger_hash_witness.rs` already
//! carries, in the same file this campaign's scripts are borrowed from. Every
//! floor the constants rode on is kept and is checked on BOTH runs: the emitter
//! witness still requires `replays > 0` and `shunned > 0`, and the seed-42
//! witness still runs through [`run_fixed_script`], which refuses to return
//! unless at least two of seed 42's bodies committed new facts over its sixty
//! ticks. Both hashes are PRINTED on every run, so a future migration has the
//! numbers without this file gating on them.
//!
//! Said plainly: **a constant-free witness guarantees DETERMINISM (two fresh
//! sessions on one seed produce the same bytes) plus its FLOORS (the path was
//! entered and the digest is not a list of empty sets). It cannot detect a
//! BEHAVIOUR CHANGE at all** — a fold that moved every creature's route would
//! move both runs together and be witnessed by neither. That is what the
//! constants were for, and it is what retiring them gives up.
//!
//! # THE DATED RECORD (constants, control, and two main-first re-measurements)
//!
//! Everything from here to the end of this doc is history, recorded between
//! 2026-09-02 and 2026-09-03. Its numbers were correct when taken and nothing
//! re-checks them; read them as a dated record, never as a current claim.
//!
//! THE THREE CONSTANTS, as they stood at the close:
//!
//! ```text
//! DETENT_SEED_42_LEDGER    0x36eb_5f31_17e8_2539   (main's value at a712371dc)
//! DETENT_EMITTER_LEDGER    0xc851_e64b_0105_38b2
//! DETENT_EMITTER_HAZARD    0xa9f1_7d82_c183_2854
//! ```
//!
//! They were minted at Task 2 from the merge base `0dccce029`, each from two
//! agreeing runs. The seed-42 value at minting was `0xabc4731e5cf1ab21` — the
//! same value main printed at The Pawl's close, so the walk had not moved
//! between the two campaigns.
//!
//! THE POSITIVE CONTROL (Task 2): `DANGER_ACT` moved from 0.3 to 0.05 with
//! `scripts/mutate.py` — the verdict predicate this campaign's whole design
//! turns on. Under it the emitter script's counts moved (shunned 186 → 428,
//! dread 6 → 177, replays 342 → 12,044) and **both emitter hashes moved**.
//! **`DETENT_SEED_42_LEDGER` did NOT move**, and that is the load-bearing half
//! of this record: **seed 42's derived residents carry no fear verdict that
//! ever reaches a route**, for the reason `ledger_hash_witness.rs`'s own "Seed
//! 42 is not it" note gives in full. So a green seed-42 witness was never
//! evidence about the fear path — it witnessed the walk's byte-identity, and
//! the emitter pair was the load-bearing pair for the path this campaign
//! rewrote. Anyone reading this record for a future migration of these folds
//! should mint the emitter pair first and treat a seed-42 constant as a
//! blast-radius check, not a fold check.
//!
//! THE TWO MAIN-FIRST RE-MEASUREMENTS (decision 0541's own discipline: take
//! main's numbers on a checkout carrying none of the campaign's code, BEFORE
//! the merge, so the merged tree is required to reproduce a number the campaign
//! did not produce):
//!
//! ```text
//! witness              campaign constant     main @4b82e544d     main @a712371dc
//! seed-42 ledger       0xabc4731e5cf1ab21    0xabc4731e5cf1ab21  0x36eb5f3117e82539
//! emitter ledger       0xc851e64b010538b2    0xc851e64b010538b2  0xc851e64b010538b2
//! emitter hazard       0xa9f17d82c1832854    0xa9f17d82c1832854  0xa9f17d82c1832854
//! ```
//!
//! At `4b82e544d` (The Reservoir) all three were equal, so the constants stood
//! unchanged and the merged tree reproduced them. At `a712371dc` (The Rack,
//! The Plumb, The Reservoir's close-out) the seed-42 value MOVED — The Rack
//! rewrote how a tick reaches the roster — and the constant was re-recorded to
//! main's `0x36eb5f3117e82539` before the merge; the emitter pair did not move,
//! which is itself a statement that The Rack did not touch the fear path. The
//! merged tree reproduced all three.

use crate::common;
use hornvale_kernel::{Ledger, RoomMeshMemo, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_vessel::body::Body;
use hornvale_vessel::ground::{GroundHazards, OwnedGround};
use hornvale_vessel::liveness::{
    AGENT_AT, DRANK, DriveMovements, EATEN, HazardMemory, HomeNavCache, LocaleTerrain, Occupancy,
    PrimaryAfraidMemo, RESTED, SLEPT, SUSTENANCE, Terrain, affect_of_memo_occupied,
    alarm_field_memo, derive_npcs, hazard_memory_memo, waking_offset,
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
    /// Per tick, the delta of `ReadWitness::emitter_timeline_copied()` — spec
    /// §3 rule 4's own numerator, the `(WorldTime, Facet)` entries
    /// `build_emitter_scan`'s pass 3 copied out of a trail that tick.
    pub copied_per_tick: Vec<u64>,
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
    let mut copied_per_tick = Vec::with_capacity(ticks);
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
        let copied_before = folds.borrow().witness().emitter_timeline_copied();
        // The third element is the roster write-back `Session::wait` needs
        // (The Rack, Task 3); this sampler owns no roster, so it is dropped.
        let (facts, _occupancy, _written) =
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
        copied_per_tick.push(folds.borrow().witness().emitter_timeline_copied() - copied_before);
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
        copied_per_tick,
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
    /// `hazards()` calls ONE `alarm_field_memo` over the whole roster makes,
    /// with a fresh `PrimaryAfraidMemo` — the tick's OTHER fear-path caller,
    /// measured so the tick's remaining calls are attributed rather than
    /// reasoned about. `DriveMovements::step_with_occupancy` builds exactly
    /// one of these per tick, before any creature moves.
    pub alarm_field_hazards: u64,
}

/// Runs the probe's three [`hazard_memory_memo`] reads and asserts they
/// agree. **This function has exactly one caller
/// (`h5_witness_the_hazard_reads_terrain_samples_on_the_bench_shape`), always
/// on `bench_shape(H5_SEED, ..)` — seed [`H5_SEED`] (42) with `predator:
/// None`, the shape the campaign measured at 0 frightening pairs.** So
/// `first`/`second`/`third` below all have an EMPTY `shunned` set, and the
/// two `assert_eq!`s pin determinism of an empty `HazardMemory`, not
/// byte-identity of a populated one — see the assertion after them, which
/// states that as a denominator rather than leaving it implicit, and points
/// at `windows/vessel/src/liveness_tests/emitter_scan.rs`'s
/// `the_indexed_scan_and_read_equal_the_pre_index_oracles` for the
/// non-empty-set proof (a haunted overlay, 497 frightening pairs, 120
/// emitter-free reads compared). This function's OWN load-bearing floor is
/// unaffected: H5's field-sample and hazards()-call counts
/// (`warm_samples`/`second_fresh_samples`/`warm_hazards` and friends), which
/// the caller asserts on, do not depend on `shunned` being non-empty.
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
        "two reads of one instant over one ledger must agree — on this shape (seed {H5_SEED}, \
         predator: None) both sides' `shunned` is empty by measurement, so this pins \
         determinism of an EMPTY result; see this function's doc comment for where the \
         non-empty case is proven"
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
        "a third read of the same instant, with a fresh PrimaryAfraidMemo, must agree too — on \
         this shape both sides' `shunned` is empty by measurement, so this too pins determinism \
         of an EMPTY result rather than proving byte-identity of a populated one"
    );
    let samples_after_third = shape.ground.borrow().misses();
    assert!(
        first.shunned.is_empty(),
        "this shape (seed {H5_SEED}, predator: None) is emitter-free by measurement — 0 \
         frightening pairs — so the two equalities above compare empty against empty; if it \
         ever gains a shunned room, they become real byte-identity evidence here and the \
         emitter_scan oracle is no longer the only proof of it — update this function's doc \
         comment"
    );

    // COMPONENT ATTRIBUTION (Task 6 fix round 1): the tick's other fear-path
    // caller, measured on the same terrain and the same instant as the probe
    // reads above. `alarm_field_memo` walks the whole roster and applies the
    // cheap gate (`threat_field` at each creature's position), which is where
    // its `hazards()` calls come from; `step_with_occupancy` builds one per
    // tick at the interval START, so this is that component's size, taken at
    // `shape.day` because that is the instant this witness instruments.
    terrain.reset();
    let mut alarm_memo = PrimaryAfraidMemo::new();
    let _alarm = alarm_field_memo(
        &shape.ledger,
        &shape.folds,
        &shape.npcs,
        &terrain,
        shape.day,
        &mut alarm_memo,
    );
    let alarm_field_hazards = terrain.hazards_calls();

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
        alarm_field_hazards,
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

    // WHAT THE SURVIVING CALLS ARE (Task 6 fix round 1). An unattributed
    // remainder is how a mechanism gets read wrong, so the tick's `hazards()`
    // calls are decomposed by MEASUREMENT rather than by argument:
    //
    //   (a) `alarm_field_memo` over the roster — one per tick, built by
    //       `step_with_occupancy` before anyone moves; its `hazards()` come
    //       from the cheap gate's `threat_field` at each creature's position.
    //   (b) one fresh-memo `hazard_memory_memo` on the probe — which is the
    //       tick's emitter scan (`build_emitter_scan` judging every member's
    //       `home` through `threat_field`) plus the probe's own read. The
    //       tick makes 50 such reads but they share one `PrimaryAfraidMemo`,
    //       so only the FIRST builds a scan and the other 49 pay the warm
    //       price this witness measures as zero.
    //   (c) the whole `step_with_occupancy`.
    //
    // The remainder (c - a - b) is the Danger drive's per-step sampling in
    // `advance_one`'s decide loop (`Danger::threat_at` / `threat_field`,
    // once per candidate room per step) and anything else the walk asks.
    let attributed = counts.alarm_field_hazards + counts.fresh_hazards;
    println!(
        "H5 attribution of tick {H5_TICKS}'s {last_tick} hazards() calls: \
         (a) alarm_field_memo over the roster {}, (b) one fresh-memo hazard read {}, \
         (a)+(b) = {attributed}, walk remainder (c-a-b) = {}",
        counts.alarm_field_hazards,
        counts.fresh_hazards,
        last_tick as i64 - attributed as i64,
    );
    assert!(
        counts.alarm_field_hazards > 0,
        "attribution denominator: the alarm field must sample terrain at all"
    );
    assert!(
        counts.fresh_hazards > 0,
        "attribution denominator: the fresh hazard read must sample terrain at all"
    );
    assert!(
        attributed <= last_tick,
        "the two measured fear-path components ({attributed}) cannot exceed the whole tick \
         ({last_tick}) — if they do, they are not components of it and the decomposition is \
         measuring different work"
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

/// One fresh session on `world`, the seed-42 fixed script, and the hash of
/// what it committed.
///
/// A FRESH session each time is the whole point of calling this twice: two
/// hashes taken off one session would agree because they are the same string.
/// [`run_fixed_script`] carries the non-vacuity floor (at least two of seed
/// 42's bodies must commit new facts over its sixty ticks), so it is checked
/// on both runs by construction rather than by a second assertion here.
fn seed_42_ledger_hash_of_a_fresh_walk(world: &hornvale_kernel::World) -> u64 {
    let (mut session, _) = Session::start(world, &PossessOpts::default()).expect("seed 42 starts");
    run_fixed_script(&mut session);
    fnv1a(session.session_ledger_json().as_bytes())
}

/// The seed-42 walk is deterministic: two fresh sessions running one fixed
/// script commit the same ledger bytes.
///
/// **There is no committed constant here any more, and its absence is the
/// campaign's ruling rather than an omission** — see the module doc's
/// "# THE CAMPAIGN-TIME CONSTANTS RETIRED AT THE CLOSE", which also records
/// the value this witness held (`0x36eb_5f31_17e8_2539`, main's at
/// `a712371dc`) and why the seed-42 half of the pair was blind to the fear
/// path this campaign rewrote. The hash is printed on every run so a future
/// migration has the number without this file asserting on it.
#[test]
fn the_detent_seed_42_walk_is_deterministic() {
    let world = common::build(42).expect("seed 42 builds");

    let first = seed_42_ledger_hash_of_a_fresh_walk(&world);
    let second = seed_42_ledger_hash_of_a_fresh_walk(&world);
    println!("--- the-detent seed-42 walk ---");
    println!("ledger hash {first:#018x} (second fresh session: {second:#018x})");

    assert_eq!(
        first, second,
        "two fresh seed-42 sessions running the same fixed script committed DIFFERENT \
         ledger bytes ({first:#018x} against {second:#018x}) — the walk is not \
         deterministic, which is a constitutional failure and not a moved golden"
    );
}

/// The emitter-bearing walk is deterministic on both halves — the committed
/// ledger AND the derived hazard digest — with every floor the retired
/// constants rode on checked on BOTH runs.
///
/// The floors are what keep two agreeing hashes from being two hashes of the
/// same empty digest: the past-day affect replay must be entered
/// (`replays > 0`), and the digest must have something in it
/// (`shunned > 0`). See the module doc for the retired constants
/// (`0xc851_e64b_0105_38b2` / `0xa9f1_7d82_c183_2854`, unmoved across both
/// main-first re-measurements) and for the `DANGER_ACT` control that showed
/// this pair — and not the seed-42 witness — to be the load-bearing one for
/// the fear path.
#[test]
fn the_detent_emitter_walk_is_deterministic() {
    let world = common::build(EMITTER_SEED).expect("the emitter seed builds");

    let first = run_emitter_witness(&world);
    let second = run_emitter_witness(&world);

    for (label, run) in [("run 1", &first), ("run 2", &second)] {
        println!(
            "the-detent emitter {label}: ledger {:#018x} hazard {:#018x} over {} bodies, \
             {} shunned, {} dread, {} replays",
            run.ledger_hash, run.hazard_hash, run.bodies, run.shunned, run.dread, run.replays
        );
        assert!(
            run.replays > 0,
            "{label} must reach the past-day affect replay on this world, or both hashes \
             witness the terrain-only path seed 42 already covers"
        );
        assert!(
            run.shunned > 0,
            "{label}'s hazard digest must be non-empty, or its hash is the hash of a list \
             of empty sets and would not move for any change to this path"
        );
    }

    // Both verdicts computed before either can panic: the retired constants'
    // own positive control (`DANGER_ACT` 0.3 -> 0.05) moved BOTH hashes, so a
    // ledger assertion that panicked first would never have printed the hazard
    // verdict — the half the seed-42 witness cannot give at all.
    let mut moved: Vec<String> = Vec::new();
    if first.ledger_hash != second.ledger_hash {
        moved.push(format!(
            "LEDGER: run 1 {:#018x}, run 2 {:#018x}",
            first.ledger_hash, second.ledger_hash
        ));
    }
    if first.hazard_hash != second.hazard_hash {
        moved.push(format!(
            "HAZARD: run 1 {:#018x}, run 2 {:#018x} — two fresh sessions on one seed \
             derived different remembered-frightening ground or different dread magnitudes",
            first.hazard_hash, second.hazard_hash
        ));
    }
    assert!(
        moved.is_empty(),
        "two fresh sessions on seed {EMITTER_SEED} running the same fixed script \
         disagreed, so the walk is not deterministic:\n  {}",
        moved.join("\n  ")
    );
}

/// Chaos eviction on the room memo (The Detent, spec §2.1's own remedy for the
/// key-completeness bug `GroundHazards::hazards_or_insert_with` cannot itself
/// rule out): drop the memo at every legal opportunity and assert the fold's
/// answer is unchanged. 30 reads of [`hazard_memory_memo`] on the bench
/// shape's probe, `evict_all` alternated on and off, each read taking a FRESH
/// `PrimaryAfraidMemo` (production's own per-tick shape) but the SAME
/// `GroundHazards` — every result must equal the first.
///
/// **What "equal" proves on THIS shape, stated rather than left to imply
/// more than it does.** `bench_shape` here is seed [`H5_SEED`] (42) with
/// `predator: None`, which the campaign measured at 0 frightening pairs: so
/// every one of these 30 `HazardMemory` results has an EMPTY `shunned` set,
/// and the equality above pins determinism of an empty result across chaos
/// eviction, not byte-identity of a populated one. This test's actual
/// load-bearing floor is its OTHER two checks: `a_miss_grew_on_eviction`
/// (an eviction must force at least one real recompute, so the 30 reads are
/// not vacuously cheap) and the `expected_rooms` equality below (the memo's
/// resident room count against a sample set computed independently of
/// `GroundHazards`, from `build_emitter_scan`'s own pass). The non-vacuous
/// byte-identity proof over a NON-empty `HazardMemory` lives in
/// `windows/vessel/src/liveness_tests/emitter_scan.rs`'s
/// `the_indexed_scan_and_read_equal_the_pre_index_oracles` (a haunted
/// overlay, 497 frightening pairs, 120 emitter-free reads compared).
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
    assert!(
        first
            .as_ref()
            .expect("30 reads ran, so first was set")
            .shunned
            .is_empty(),
        "this shape (seed {H5_SEED}, predator: None) is emitter-free by measurement — 0 \
         frightening pairs — so the 30-way equality above compares empty against empty; if it \
         ever gains a shunned room, that equality becomes real byte-identity evidence here and \
         the emitter_scan oracle is no longer the only proof of it — update the doc comment above"
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

// ---------------------------------------------------------------------------
// Task 8: rules 2, 4, 5 — the numbers the readout and stage 4 need. Each
// witness decides nothing; it prints the count spec §3 asks for and asserts
// only its own denominator (the shape reached the path at all).
// ---------------------------------------------------------------------------

/// How many `wait`s spec §3 rule 2's own witness takes, on the seed the
/// affect-replay path is reached on ([`EMITTER_SEED`]). More than the hash
/// witness's [`crate::ledger_hash_witness::EMITTER_SCRIPT_WAITS`] (2) on
/// purpose — this witness wants a per-tick profile, not a single hash.
const RULE_TWO_WAITS: usize = 4;

/// Spec §3 rule 2: the affect replay's share of the hazard read, on the
/// seed-6 possession shape. Counts `resident_alarm_replays()` per tick
/// against the room memo's own hit/miss split, then reads the whole roster's
/// hazard memory twice over an already-warm memo and index — the second
/// call's ground-memo deltas are the per-read lookup cost of the whole
/// roster, and any further `resident_alarm_replays()` increments are the
/// replay path's OWN reads inside `emitter_arousal -> affect_of`, not the
/// scan that warmed the index. No threshold: the share printed here decides
/// rule 2's branch in the ledger, not this assertion.
#[test]
fn rule_two_witness_the_affect_replay_share_of_the_hazard_read() {
    let world = common::build(EMITTER_SEED).expect("the emitter seed builds");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("the emitter seed starts a session");

    println!("--- rule 2 witness: seed {EMITTER_SEED}, {RULE_TWO_WAITS} waits ---");
    for i in 0..RULE_TWO_WAITS {
        let replays_before = session.resident_alarm_replays();
        let hits_before = session.resident_ground_hits();
        let misses_before = session.resident_ground_misses();
        session.handle("wait");
        println!(
            "tick {i}: alarm replays +{}, ground hits +{}, ground misses +{}",
            session.resident_alarm_replays() - replays_before,
            session.resident_ground_hits() - hits_before,
            session.resident_ground_misses() - misses_before,
        );
    }

    // First whole-roster read: whatever is left cold gets filled in here.
    let replays_a = session.resident_alarm_replays();
    let hits_a = session.resident_ground_hits();
    let misses_a = session.resident_ground_misses();
    let _first = session.hazard_memories();
    let replays_b = session.resident_alarm_replays();
    let hits_b = session.resident_ground_hits();
    let misses_b = session.resident_ground_misses();
    println!(
        "first whole-roster hazard read: alarm replays +{}, ground hits +{}, ground misses +{}",
        replays_b - replays_a,
        hits_b - hits_a,
        misses_b - misses_a,
    );

    // Second whole-roster read: the room memo and index are now warm, so this
    // is the per-read lookup cost of the whole roster plus whatever the
    // replay path itself asks.
    let bodies = session.bodies().len();
    let second = session.hazard_memories();
    let replays_delta = session.resident_alarm_replays() - replays_b;
    let hits_delta = session.resident_ground_hits() - hits_b;
    let misses_delta = session.resident_ground_misses() - misses_b;
    let shunned: usize = second.iter().map(|(_, m)| m.shunned.len()).sum();
    println!(
        "second whole-roster hazard read (warm): alarm replays +{replays_delta}, ground hits \
         +{hits_delta}, ground misses +{misses_delta}, over {bodies} bodies, {shunned} shunned \
         rooms total"
    );
    let replays_per_read = replays_delta as f64 / bodies as f64;
    let lookups_per_read = (hits_delta + misses_delta) as f64 / bodies as f64;
    println!(
        "rule 2 share: {replays_per_read:.4} replays per hazard read, {lookups_per_read:.4} memo \
         lookups per hazard read"
    );

    assert!(
        session.resident_alarm_replays() > 0,
        "rule 2 denominator: this shape must reach the past-day affect replay at all, or the \
         share above is zero out of zero"
    );
    assert!(
        shunned > 0,
        "rule 2 denominator: the warm read's hazard memories must hold something shunned"
    );
}

/// Spec §3 rule 4: the emitter scan's pass-3 timeline copy, on seed 6's
/// 50-agent, 60-tick roster — the same shape H5/H6 measure over. Prints the
/// entries copied per tick at 15/30/60 beside the roster's own trail sum, so
/// a reader can see whether the copy grows with history or stays flat. No
/// threshold: the branch (spec §3 rule 4) is decided in the ledger from this
/// print, not from an assertion here.
#[test]
fn rule_four_witness_the_emitter_timeline_copy() {
    let shape = bench_shape(EMITTER_SEED, 60, 50);
    println!("--- rule 4 witness: seed {EMITTER_SEED}, 50 agents, 60 ticks ---");
    println!("copied/tick profile: {:?}", shape.copied_per_tick);
    let copied_15 = shape.copied_per_tick[14];
    let copied_30 = shape.copied_per_tick[29];
    let copied_60 = shape.copied_per_tick[59];
    let trail_sum: usize = {
        let mut store = shape.folds.borrow_mut();
        let trail = store.trail(&shape.ledger);
        shape.npcs.iter().map(|b| trail.of(b.entity).len()).sum()
    };
    println!(
        "entries copied at tick 15: {copied_15}, tick 30: {copied_30}, tick 60: {copied_60}; \
         roster trail sum {trail_sum}"
    );
    let with_emitters = shape.folds.borrow().witness().emitter_scans_with_emitters();
    println!("rule 4 denominator: {with_emitters} scans found an emitter over the whole run");
    assert!(
        with_emitters > 0,
        "rule 4 denominator: this shape must build at least one scan that finds an emitter, or \
         pass 3 never copies anything"
    );
    println!(
        "rule 4 verdict input: tick 15 -> tick 60 copied {copied_15} -> {copied_60} (grows with \
         history: {})",
        copied_60 > copied_15,
    );
}

/// Spec §4 rule 5's shape: seed 42, 10 derived agents, 10 ticks — the lab's
/// `run_simulation` shape (`windows/lab/src/health.rs`'s waking-instant read),
/// reproduced over [`bench_shape`]'s pieces rather than a real
/// `windows/lab` run.
pub const RULE_FIVE_SEED: u64 = 42;
pub const RULE_FIVE_AGENTS: usize = 10;
pub const RULE_FIVE_TICKS: usize = 10;

/// Spec §3 rule 5: the lab's waking-instant reads must be served by the
/// verdict index's first-visit prefix. For each of [`bench_shape`]'s
/// [`RULE_FIVE_AGENTS`] agents, reads `hazard_memory_memo` (via
/// `affect_of_memo_occupied`, `health.rs`'s own call shape) at the same
/// waking instant `health.rs:169-186` computes — strictly before the
/// roster's last committed sighting — and asserts the read is served
/// identically whether the store is WARM (the bench's own tick loop already
/// populated it) or FRESH (rebuilt from scratch at that past instant): the
/// index's prefix machinery must not depend on which sightings after the
/// read instant happen to already be folded in.
///
/// **What the equality proves here, stated exactly.** [`RULE_FIVE_SEED`] is
/// 42 with `predator: None` (`bench_shape`'s default), the shape the
/// campaign measured at 0 frightening pairs — so `warm.shunned` and
/// `fresh.shunned` are both empty on every one of these
/// [`RULE_FIVE_AGENTS`] comparisons, and the equality pins determinism of an
/// EMPTY `HazardMemory` across a discarded-and-rebuilt store, not
/// byte-identity of a populated one. This test's own load-bearing floor is
/// its OTHER assertion: `hazards_in_the_past` must actually grow
/// (`after - before > 0`), i.e. at least one of these reads really lands
/// before a committed sighting, which is the denominator the prefix
/// machinery needs to be exercised at all. The non-vacuous byte-identity
/// proof over a NON-empty `HazardMemory` lives in
/// `windows/vessel/src/liveness_tests/emitter_scan.rs`'s
/// `the_indexed_scan_and_read_equal_the_pre_index_oracles` (a haunted
/// overlay, 497 frightening pairs, 120 emitter-free reads compared).
#[test]
fn rule_five_witness_past_instant_reads_on_the_lab_shape() {
    let shape = bench_shape(RULE_FIVE_SEED, RULE_FIVE_TICKS, RULE_FIVE_AGENTS);
    let mesh = shape.mesh_memo.clone();
    let base = LocaleTerrain::with_fields(&shape.ctx, None, None, None, None, Some(&mesh))
        .with_ground(&shape.ground);

    let before = shape.folds.borrow().witness().hazards_in_the_past();
    let mut compared = 0usize;
    for npc in &shape.npcs {
        // `health.rs`'s own instant: `(day - 1.0) + waking_offset(activity)`,
        // with `day` there the tick counter AFTER increment (so `day - 1.0`
        // is the start of the tick just simulated) — `shape.day` plays that
        // role here, as `bench_shape`'s own loop advances it the same way.
        let past_t =
            WorldTime::from_std_days(shape.day.as_std_days() - 1.0 + waking_offset(npc.activity))
                .expect("a day value derived from a finite day count is finite");

        // The read itself, through `affect_of_memo_occupied` — `health.rs`'s
        // own entry point — over the bench's WARM store.
        let mut afraid = PrimaryAfraidMemo::new();
        let mut mesh_memo = shape.mesh_memo.clone();
        let mut nav = HomeNavCache::new();
        let _ = affect_of_memo_occupied(
            &shape.ledger,
            npc,
            &shape.npcs,
            past_t,
            &base,
            &mut afraid,
            Some(&Occupancy::default()),
            &mut mesh_memo,
            &mut nav,
            &shape.folds,
        );

        // The discard check: the same `hazard_memory_memo` call, served by
        // the WARM store above against a FRESH one rebuilt from scratch at
        // this same past instant, must agree. On this shape (seed
        // RULE_FIVE_SEED = 42, predator: None) both sides' `shunned` is
        // empty by measurement, so this pins determinism of an EMPTY
        // result — see this test's own doc comment above for where the
        // non-empty case is proven.
        let mut warm_memo = PrimaryAfraidMemo::new();
        let warm = hazard_memory_memo(
            &shape.ledger,
            &shape.folds,
            npc,
            past_t,
            &base,
            &shape.npcs,
            &mut warm_memo,
        );
        let fresh_folds = OwnedFolds::new(ResidentFolds::new());
        let mut fresh_memo = PrimaryAfraidMemo::new();
        let fresh = hazard_memory_memo(
            &shape.ledger,
            &fresh_folds,
            npc,
            past_t,
            &base,
            &shape.npcs,
            &mut fresh_memo,
        );
        assert_eq!(
            warm, fresh,
            "entity {:?} at past instant {past_t:?}: a warm store and a store discarded and \
             rebuilt from scratch at this instant must agree, or the verdict index depends on \
             sightings after the instant being read — on this shape both sides' `shunned` is \
             empty by measurement, so this pins determinism of an EMPTY result rather than \
             proving byte-identity of a populated one",
            npc.entity
        );
        assert!(
            warm.shunned.is_empty(),
            "entity {:?}: this shape (seed {RULE_FIVE_SEED}, predator: None) is emitter-free by \
             measurement — 0 frightening pairs — so the equality above compares empty against \
             empty; if it ever gains a shunned room, it becomes real byte-identity evidence \
             here and the emitter_scan oracle is no longer the only proof of it — update this \
             test's doc comment",
            npc.entity
        );
        compared += 1;
    }
    let after = shape.folds.borrow().witness().hazards_in_the_past();
    println!(
        "--- rule 5 witness: seed {RULE_FIVE_SEED}, {RULE_FIVE_AGENTS} agents, \
         {RULE_FIVE_TICKS} ticks ---"
    );
    println!(
        "past-instant reads: {compared} compared (warm vs fresh), hazards_in_the_past delta {}",
        after - before
    );
    assert!(
        after - before > 0,
        "rule 5 denominator: at least one of these reads must land strictly before a committed \
         sighting, or the index's prefix machinery has no production caller on this shape"
    );
}
