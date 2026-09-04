//! The Tenon, Task 1: the rest-site baseline, taken BEFORE any sleepable
//! surface is added to the world.
//!
//! Run: `cargo run --release -p hornvale-lab --example rest_site_census`
//!
//! ## The question
//!
//! Whether any body in any world has ever taken an `Afforded` rest bout is
//! unmeasured. Decision 0697 records that the affect-trace byte-golden is
//! blind to the afforded path (seed 42's traced window is entirely open
//! ground); the idea-registry row `PSY-rest-site-is-a-tuning-indicator`
//! records the reachability test as unbuilt. The Tenon's Task 7 adds three
//! `at_locale: true` sleepable kinds to every world, so if this number is not
//! taken now, the campaign cannot say what it changed and a null result will
//! be indistinguishable from a broken instrument (spec §7.2, P3/P4/P6).
//!
//! ## Why it lives in `windows/lab` and not `windows/vessel`
//!
//! `windows/lab/Cargo.toml` declares `hornvale-vessel`; vessel declares no
//! `hornvale-lab`. An example under `windows/vessel` cannot reach the
//! world-building and population-derivation machinery this probe needs, and
//! reversing that edge would be a layering violation
//! `cli/tests/architecture.rs` refuses.
//!
//! ## What it does, and what it deliberately does not
//!
//! INFORMATIVE, NEVER A GATE — the same standing as every other example in
//! this tree. It builds a real world per seed, derives the same population
//! `hornvale_lab::health::simulate_world` does, and runs the same per-tick
//! walk `run_simulation_with_locale` runs — but keeps the LEDGER, which that
//! function does not return. Every count below is read off committed facts,
//! never off a fold.
//!
//! **It reconstructs two private things and says so at each site**:
//! `liveness::room_affords_rest` (private, `liveness.rs:3724`) and the
//! `SiteGrade` resolution inside `liveness::rest_timeline` (private,
//! `liveness.rs:3802`). A measurement must not widen the surface it measures,
//! so neither is made `pub` for this probe's benefit.

use hornvale_kernel::{Facet, FacetId, Ledger, RoomMeshMemo, Seed, Value, WorldTime, tick};
use hornvale_locale::LocaleContext;
use hornvale_vessel::affordance::{OfferedVerb, offered_to};
use hornvale_vessel::body::Body;
use hornvale_vessel::interior::interior_of;
use hornvale_vessel::liveness::{
    AGENT_AT, DRANK, DriveMovements, EATEN, HomeNavCache, LocaleTerrain, RESTED, SLEPT, SLEPT_ON,
    SUSTENANCE, Terrain, agent_position, built_rooms, derive_npcs,
};
use std::collections::{BTreeMap, BTreeSet};

/// Ticks simulated per seed — `HEALTH_TICKS`' value, so this probe's walk is
/// the same length the health battery's is and the two are comparable.
const TICKS: usize = 40;

/// Settlement-derived NPCs per world — `HEALTH_NPCS`' value, same reason.
const NPCS: usize = 6;

/// Wild NPCs appended per world — `HEALTH_WILD`' value, same reason.
const WILD: usize = 4;

/// The seed sweep. Wider than the three census seeds (42, 7, 1234) on
/// purpose: decision 0097 converted an existence claim at n=1 into a rate for
/// exactly this reason, and P3 asks for a rate with its width reported.
const SEEDS: [u64; 24] = [
    42, 7, 1234, 13, 1, 2, 3, 5, 8, 11, 17, 23, 29, 31, 37, 41, 53, 59, 61, 67, 71, 73, 79, 83,
];

/// One seed's counts.
#[derive(Default)]
struct SeedCounts {
    /// Ticks the simulation actually completed (`TICKS` unless it truncated).
    ticks_done: usize,
    /// `RESTED` bouts committed.
    rested: usize,
    /// `SLEPT` bouts committed.
    slept: usize,
    /// Bouts (either predicate) whose room afforded rest.
    afforded: usize,
    /// Bouts (either predicate) whose room did not.
    bare: usize,
    /// `SLEPT_ON` facts, by the `KindId` text in `Value::Text`.
    slept_on: BTreeMap<String, usize>,
    /// Distinct walked rooms per `(is_built, is_cold)` QUADRANT of the 2x2
    /// the two gates produce, indexed
    /// `built * 2 + cold` — the same indexing `rest_timeline`'s own grade
    /// memo uses, and the same four quadrants `interior_of`'s entire input set
    /// produces.
    quadrants: [BTreeSet<FacetId>; 4],
    /// Of those four quadrants, how many afford rest to at least one body here.
    quadrants_affording: [bool; 4],
}

fn main() {
    println!(
        "rest-site baseline (The Tenon, Task 1) -- sweep: {} seeds x {} ticks x {} bodies \
         ({} settled + {} wild) per seed",
        SEEDS.len(),
        TICKS,
        NPCS + WILD,
        NPCS,
        WILD
    );
    println!();

    let mut totals = SeedCounts::default();
    let mut truncated: Vec<u64> = Vec::new();
    let mut empty: Vec<u64> = Vec::new();

    println!(
        "{:>6}  {:>5}  {:>6}  {:>6}  {:>8}  {:>6}  {:>9}",
        "seed", "ticks", "rested", "slept", "afforded", "bare", "slept-on"
    );
    for seed in SEEDS {
        let Some(c) = census_of_seed(seed) else {
            empty.push(seed);
            println!("{seed:>6}  {:>5}", "-- no world/settlement/locale --");
            continue;
        };
        if c.ticks_done != TICKS {
            truncated.push(seed);
        }
        println!(
            "{seed:>6}  {:>5}  {:>6}  {:>6}  {:>8}  {:>6}  {:>9}",
            c.ticks_done,
            c.rested,
            c.slept,
            c.afforded,
            c.bare,
            c.slept_on.values().sum::<usize>()
        );
        totals.ticks_done += c.ticks_done;
        totals.rested += c.rested;
        totals.slept += c.slept;
        totals.afforded += c.afforded;
        totals.bare += c.bare;
        for (k, n) in c.slept_on {
            *totals.slept_on.entry(k).or_default() += n;
        }
        for i in 0..4 {
            // A `FacetId` is a packed room address and is world-independent as
            // a NUMBER only; unioning across seeds would conflate rooms of
            // different worlds. So the cross-seed total is a COUNT sum, kept
            // in a parallel accumulator below rather than in this set.
            totals.quadrants[i].extend(c.quadrants[i].iter().copied());
            totals.quadrants_affording[i] |= c.quadrants_affording[i];
        }
    }

    println!();
    println!("== totals over the sweep ==");
    let bouts = totals.rested + totals.slept;
    println!(
        "  seeds that produced a world : {}",
        SEEDS.len() - empty.len()
    );
    println!("  seeds with no world         : {empty:?}");
    println!("  TRUNCATED runs              : {truncated:?}  (must be empty)");
    println!("  RESTED bouts                : {}", totals.rested);
    println!("  SLEPT bouts                 : {}", totals.slept);
    println!("  bouts, total                : {bouts}");
    println!(
        "  graded Afforded             : {}  ({:.4} of bouts)",
        totals.afforded,
        ratio(totals.afforded, bouts)
    );
    println!(
        "  graded Bare                 : {}  ({:.4} of bouts)",
        totals.bare,
        ratio(totals.bare, bouts)
    );
    println!("  SLEPT_ON facts by kind      : {:?}", totals.slept_on);
    println!("  walked rooms by quadrant (distinct FacetIds within each seed, summed):");
    for i in 0..4 {
        println!(
            "    built={} cold={} : {:>6} rooms   affords-rest-somewhere={}",
            i / 2 == 1,
            i % 2 == 1,
            totals.quadrants[i].len(),
            totals.quadrants_affording[i]
        );
    }
}

/// `n / d`, `0.0` on an empty denominator — a rate with no denominator is not
/// a rate, and printing `NaN` would read as a broken run rather than an empty
/// one.
fn ratio(n: usize, d: usize) -> f64 {
    if d == 0 { 0.0 } else { n as f64 / d as f64 }
}

/// Build one seed's world, walk its derived population, and count.
/// `None` when the seed produces no world, no settlement or no locale — the
/// same three early returns `health::simulate_world` takes.
// Named construction site (decision 0092): this probe IS a world build, and
// mirrors `health::simulate_world`'s single derivation block exactly.
#[allow(clippy::disallowed_methods)]
fn census_of_seed(seed: u64) -> Option<SeedCounts> {
    let wc = hornvale_worldgen::WorldComponents::assemble().ok()?;
    let world = hornvale_worldgen::build_world_from_components(
        Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
        &wc,
    )
    .ok()?;
    let terrain = hornvale_worldgen::terrain_of(&world).ok()?;
    let climate = hornvale_worldgen::climate_from(&world, &terrain).ok()?;
    let ctx = LocaleContext::build_from(&world, &terrain, &climate);

    let mut ledger = world.ledger.clone();
    let mut registry = world.registry.clone();
    // The session-only predicates the drive tick commits, registered on the
    // clone exactly as `simulate_world` and `Session::start` do. SLEPT_ON is
    // the one that matters here: an unregistered predicate makes `tick`
    // return `Err`, and `health.rs`'s two `Err(_) => break` arms would
    // truncate the run silently.
    let _ = registry.register_predicate(AGENT_AT, false, "an agent's position on a day");
    let _ = registry.register_predicate(DRANK, false, "an agent satisfied its sustenance goal");
    let _ = registry.register_predicate(
        RESTED,
        false,
        "an agent rested on a day, for this many ticks",
    );
    let _ =
        registry.register_predicate(SLEPT, false, "an agent slept on a day, for this many ticks");
    let _ = registry.register_predicate(
        SLEPT_ON,
        false,
        "the kind of anchor an agent slept on, within the room it slept in",
    );
    let _ = registry.register_predicate(EATEN, false, "an agent ate on a day");

    let home = hornvale_settlement::all_settlements(&world).first()?.id;
    let mut npcs = derive_npcs(&world, &ctx, &mut ledger, NPCS, home);
    let report = hornvale_worldgen::demography_report_from(&world, &wc, &terrain, &climate).ok();
    let concentrations = match report.as_ref() {
        Some(report) => hornvale_worldgen::wild_concentrations_from(&wc, report, WILD),
        None => Vec::new(),
    };
    npcs.extend(hornvale_vessel::liveness::derive_wild_npcs(
        &world,
        &ctx,
        &mut ledger,
        concentrations,
    ));
    if npcs.is_empty() {
        return None;
    }
    let calendar = hornvale_worldgen::sky_of(&world)
        .ok()
        .and_then(|sky| sky.calendar().cloned());
    let predator = report
        .as_ref()
        .map(|r| hornvale_worldgen::predator_pressure_from(&wc, &terrain, r));
    let prey = report
        .as_ref()
        .map(|r| hornvale_worldgen::prey_pressure_from(&wc, &terrain, r));
    let built = built_rooms(&world, &ctx);
    let day_ticks = calendar.as_ref().and_then(|c| c.day_ticks());

    let ticks_done = walk(
        &mut ledger,
        &registry,
        &npcs,
        &ctx,
        calendar.as_ref(),
        predator.as_ref(),
        prey.as_ref(),
        &built,
        day_ticks,
    );

    Some(count(
        &ledger,
        &npcs,
        &ctx,
        calendar.as_ref(),
        predator.as_ref(),
        prey.as_ref(),
        &built,
        day_ticks,
        ticks_done,
    ))
}

/// The per-tick walk, mirroring `hornvale_lab::health::run_simulation_with_locale`
/// statement for statement — same per-tick geometry prefill, same fresh
/// `LocaleTerrain` per tick, same run-lived `mesh_memo` / `home_nav_cache` /
/// `folds` / `ground` scopes — with two deliberate differences:
///
/// 1. It keeps the `ledger` (that function returns only affect traces, so the
///    committed facts this probe counts are unreachable through it).
/// 2. It returns the number of ticks it COMPLETED. `health.rs` breaks out of
///    its loop on any commit error (`Err(_) => break`, lines 164 and 289) and
///    reports nothing about it; two calibration tests once passed on runs
///    stopped at day 1. A run that did not reach `TICKS` is a broken
///    instrument, not a zero, and the caller says so.
///
/// It does NOT compute affects — this probe reads committed facts only.
#[allow(clippy::too_many_arguments)]
fn walk(
    ledger: &mut Ledger,
    registry: &hornvale_kernel::ConceptRegistry,
    npcs: &[Body],
    ctx: &LocaleContext,
    calendar: Option<&hornvale_astronomy::Calendar>,
    predator: Option<&hornvale_kernel::VertexMap<f64>>,
    prey: Option<&hornvale_kernel::VertexMap<f64>>,
    built: &BTreeMap<FacetId, String>,
    day_ticks: Option<hornvale_kernel::units::TickSpan>,
) -> usize {
    let mut mesh_memo = RoomMeshMemo::new();
    let mut home_nav_cache = HomeNavCache::new();
    let folds =
        hornvale_vessel::resident::OwnedFolds::new(hornvale_vessel::resident::ResidentFolds::new());
    let ground =
        hornvale_vessel::ground::OwnedGround::new(hornvale_vessel::ground::GroundHazards::new());
    let geo = ctx.climate().geosphere();
    let index = ctx.nearest_index();
    let mut day = 0.0_f64;
    let mut done = 0usize;
    for _ in 0..TICKS {
        for npc in npcs {
            let pos = agent_position(
                ledger,
                npc,
                WorldTime::from_std_days(day).expect("a day value is finite"),
            );
            pos.corner_weights_memo(geo, index, &mut mesh_memo);
            for n in pos.neighbors_memo(&mut mesh_memo) {
                n.corner_weights_memo(geo, index, &mut mesh_memo);
            }
        }
        let mesh_snapshot = mesh_memo.clone();
        let terrain = LocaleTerrain::with_fields(
            ctx,
            calendar,
            predator,
            prey,
            Some(built),
            Some(&mesh_snapshot),
        )
        .with_ground(&ground);
        let sys = DriveMovements {
            npcs: npcs.to_vec(),
            from: WorldTime::from_std_days(day).expect("a day value is finite"),
            to: WorldTime::from_std_days(day + 1.0).expect("a day value is finite"),
            params: SUSTENANCE,
            day_ticks,
            terrain: &terrain,
            folds: &folds,
        };
        // The occupancy evaluation `Session::wait` and
        // `run_simulation_with_locale` both make before the commit, kept so
        // this walk shares their fold-store advance exactly rather than
        // differing from production in a way nothing would notice.
        let _ = sys.step_with_occupancy(ledger, &mut mesh_memo, &mut home_nav_cache);
        *ledger = match tick(ledger, &[&sys], &["drive-movements"], registry) {
            Ok(next) => next,
            // The one place this probe DIVERGES from `health.rs`: it stops
            // like that code does, but the caller learns it stopped.
            Err(e) => {
                eprintln!("  commit error at tick {done}: {e:?}");
                return done;
            }
        };
        day += 1.0;
        done += 1;
    }
    done
}

/// Read the five counts off the finished ledger.
///
/// **The terrain is rebuilt once here rather than captured per tick, and that
/// is exact, not an approximation.** The only terrain reads below are
/// `is_built` and `is_cold`: `is_built` tests membership of the `built` set,
/// computed once from the world before the walk; `is_cold` is a pure
/// temperature read against `FURNISHING_COLD_C` at a frozen reference day.
/// Neither is a function of what the walk committed, so a terrain built after
/// the walk answers both exactly as each tick's own did.
#[allow(clippy::too_many_arguments)]
fn count(
    ledger: &Ledger,
    npcs: &[Body],
    ctx: &LocaleContext,
    calendar: Option<&hornvale_astronomy::Calendar>,
    predator: Option<&hornvale_kernel::VertexMap<f64>>,
    prey: Option<&hornvale_kernel::VertexMap<f64>>,
    built: &BTreeMap<FacetId, String>,
    _day_ticks: Option<hornvale_kernel::units::TickSpan>,
    ticks_done: usize,
) -> SeedCounts {
    // A cold, empty geometry memo: `LocaleTerrain::with_fields` takes it
    // read-only and neither `is_built` nor `is_cold` consults it, so an empty
    // one costs nothing and answers identically.
    let mesh_memo = RoomMeshMemo::new();
    let terrain =
        LocaleTerrain::with_fields(ctx, calendar, predator, prey, Some(built), Some(&mesh_memo));
    let mut c = SeedCounts {
        ticks_done,
        ..Default::default()
    };

    for npc in npcs {
        // (4) THE WALKED ROOMS, sampled the way the fold resolves a bout's
        // site: `agent_position` at each simulated day. Reading the `agent-at`
        // facts directly would need `liveness::room_from_text`, which is
        // private — and `agent_position` is the pub function that decodes
        // exactly those facts (`latest_committed_position`, day <= t, else
        // `body.home`), so this is the same trail, not a second one.
        for d in 0..=ticks_done {
            let t = WorldTime::from_std_days(d as f64).expect("a day value is finite");
            let room = agent_position(ledger, npc, t);
            let Ok(id) = room.pack() else { continue };
            let slot = quadrant_of(&room, &terrain);
            c.quadrants[slot].insert(id);
            if !c.quadrants_affording[slot] && affords_rest(&room, npc, &terrain) {
                c.quadrants_affording[slot] = true;
            }
        }

        // (1)+(2) THE BOUTS AND THEIR GRADE.
        for (predicate, is_sleep) in [(RESTED, false), (SLEPT, true)] {
            for f in ledger.facts_of(npc.entity, predicate) {
                let Some(day) = f.day else { continue };
                if is_sleep {
                    c.slept += 1;
                } else {
                    c.rested += 1;
                }
                // Mirrors `rest_timeline`'s grade resolution (private,
                // `liveness.rs:3802`): the room is the last committed
                // `agent-at` at or before the bout's day, defaulting to the
                // body's home, and the grade is `room_affords_rest` of that
                // room. RECONSTRUCTION, not the same function — it can drift.
                let room = agent_position(ledger, npc, day);
                if affords_rest(&room, npc, &terrain) {
                    c.afforded += 1;
                } else {
                    c.bare += 1;
                }
            }
        }

        // (3) SLEPT_ON BY KIND.
        for f in ledger.facts_of(npc.entity, SLEPT_ON) {
            if let Value::Text(kind) = &f.object {
                *c.slept_on.entry(kind.clone()).or_default() += 1;
            }
        }
    }
    c
}

/// `built * 2 + cold` — `rest_timeline`'s own grade-memo indexing, and the
/// entire input set `interior_of` reads.
///
/// The spec and the plan name these four with a word this repository reserves
/// for a mesh vertex (`cli/tests/suite/lexicon_guard.rs`). This probe says
/// QUADRANT instead, so the two senses cannot collide in a file whose whole
/// subject is rooms.
fn quadrant_of(room: &Facet, terrain: &dyn Terrain) -> usize {
    usize::from(terrain.is_built(room)) * 2 + usize::from(terrain.is_cold(room))
}

/// **A RECONSTRUCTION of `liveness::room_affords_rest`** (private, `fn` at
/// `windows/vessel/src/liveness.rs:3724`), which this probe may not call and
/// must not widen to `pub` — a measurement that changes the surface it
/// measures is not a measurement.
///
/// It is exact TODAY and is not the same function. `room_affords_rest`
/// delegates to `sleep_site::select_sleep_site(&interior_of(room, terrain),
/// body).is_some()`; that function's whole body is
/// `interior.ids().into_iter().find(|&a| offered_to(interior.anchor(a).kind,
/// body).contains(&OfferedVerb::Sleep))`, and `.find(..).is_some()` is
/// `.any(..)`. Both are `pub(crate)`/private, so if either changes, this
/// drifts silently and nothing will say so.
///
/// Through `offered_to`, NOT `offered_to_observer`: physical restoration is
/// not gated on knowledge, and `room_affords_rest`'s own delegate uses the
/// unknowing form.
fn affords_rest(room: &Facet, body: &Body, terrain: &dyn Terrain) -> bool {
    let interior = interior_of(room, terrain);
    interior
        .ids()
        .into_iter()
        .any(|a| offered_to(interior.anchor(a).kind, body).contains(&OfferedVerb::Sleep))
}
