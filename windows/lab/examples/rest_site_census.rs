//! The Tenon rest-site census: Task 1's baseline instrument, reused by Task 8
//! after the three natural sleepable surfaces enter the world.
//!
//! Run: `cargo run --release -p hornvale-lab --example rest_site_census`
//!
//! ## The question
//!
//! Whether real bout rooms support any rest-offering anchor for the body in
//! them was unmeasured. Decision 0697 records that the affect-trace
//! byte-golden was blind to that non-`Bare` path (seed 42's traced window was
//! entirely open ground); the idea-registry row
//! `PSY-rest-site-is-a-tuning-indicator` recorded the reachability test as
//! unbuilt. The Tenon's Task 7 adds three `at_locale: true` sleepable kinds to
//! every world, so if this number is not taken now, the campaign cannot say
//! what it changed and a null result will be indistinguishable from a broken
//! instrument (spec §7.2, P3/P4/P6).
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
//! **It carries three private reconstructions and says so at each site**:
//! `liveness::room_affords_rest`; `liveness::grade_of`, with its inputs
//! resolved as `liveness::sleep_traits_of` resolves them; and
//! `liveness::FIT_FLOOR`, held here as `RECONSTRUCTED_FIT_FLOOR`. No line
//! numbers are given for those sites on purpose: the close's first attempt at
//! this very correction cited three of them and every one had already drifted.
//!
//! It does NOT reconstruct `rest_timeline`'s final `SiteGrade`: a same-day
//! `SLEPT_ON` fact makes production use `SiteGrade::On(kind)`, while this
//! probe measures only the room-level boolean that production retains as the
//! fallback when no kind fact exists. A measurement must not widen the surface
//! it measures, so none is made `pub` for this probe's benefit.

use hornvale_kernel::{
    Facet, FacetId, KindId, Ledger, RoomMeshMemo, Seed, Value, WorldTime,
    component::ComponentStore, tick,
};
use hornvale_locale::LocaleContext;
use hornvale_species::HabitatRealm;
use hornvale_vessel::affordance::{
    ObjectTraits, OfferedVerb, Substrate, object_registry, offered_to,
};
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

/// The three Task 7 additions whose real-world reach P3 measures.
const ADDED_SURFACES: [KindId; 3] = [KindId("rushes"), KindId("ledge"), KindId("bracken")];

/// The production grade's suitability floor. This is an explicit
/// reconstruction of private `liveness::FIT_FLOOR`, not a second authority.
const RECONSTRUCTED_FIT_FLOOR: f64 = 0.2;

/// One actual body standing in one world-composed room with two usable rest
/// surfaces, and the production grade arithmetic reconstructed for both.
#[derive(Clone, Debug)]
struct GradeObservation {
    seed: u64,
    room: FacetId,
    body: hornvale_kernel::EntityId,
    species: String,
    first: KindId,
    first_grade: f64,
    second: KindId,
    second_grade: f64,
}

/// One seed's counts.
#[derive(Default)]
struct SeedCounts {
    /// Bodies the seed actually derived and the probe observed.
    observed_bodies: usize,
    /// Ticks the simulation actually completed (`TICKS` unless it truncated).
    ticks_done: usize,
    /// `RESTED` bouts committed.
    rested: usize,
    /// `SLEPT` bouts committed.
    slept: usize,
    /// Bouts (either predicate) whose room supported at least one sleep offer.
    room_supported: usize,
    /// Bouts (either predicate) whose room supported none (the `Bare` path).
    room_bare: usize,
    /// `SLEPT_ON` facts, by the `KindId` text in `Value::Text`.
    slept_on: BTreeMap<String, usize>,
    /// Distinct walked rooms per `(is_built, is_cold)` QUADRANT of the 2x2
    /// the two gates produce, indexed
    /// `built * 2 + cold` — the same indexing `rest_timeline`'s own grade
    /// memo uses, and the same four quadrants `interior_of`'s entire input set
    /// produces.
    quadrants: [BTreeSet<FacetId>; 4],
    /// Cross-seed totals of the distinct-per-seed sets above. Used only by the
    /// sweep accumulator: packed addresses repeat across worlds, so these are
    /// scalar sums rather than one cross-world set.
    quadrant_counts: [usize; 4],
    /// Of those four quadrants, how many support rest for at least one body here.
    quadrants_supported: [bool; 4],
    /// Distinct walked rooms in this seed containing each added surface.
    surface_rooms: BTreeMap<KindId, BTreeSet<FacetId>>,
    /// Cross-seed scalar sums of `surface_rooms`, by kind.
    surface_room_counts: BTreeMap<KindId, usize>,
    /// How many measured worlds had at least one walked room containing a kind.
    surface_world_counts: BTreeMap<KindId, usize>,
    /// Actual body-in-room comparisons, used for P2 and P4 witnesses.
    grade_observations: Vec<GradeObservation>,
}

fn main() {
    println!(
        "rest-site census (The Tenon, Tasks 1 and 8) -- sweep: {} seeds x {} ticks; request: {} bodies \
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
    let mut incomplete_bodies: Vec<(u64, usize)> = Vec::new();

    println!(
        "{:>6}  {:>6}  {:>5}  {:>6}  {:>6}  {:>10}  {:>6}  {:>9}",
        "seed", "bodies", "ticks", "rested", "slept", "supported", "bare", "slept-on"
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
        if c.observed_bodies != NPCS + WILD {
            incomplete_bodies.push((seed, c.observed_bodies));
        }
        println!(
            "{seed:>6}  {:>6}  {:>5}  {:>6}  {:>6}  {:>10}  {:>6}  {:>9}",
            c.observed_bodies,
            c.ticks_done,
            c.rested,
            c.slept,
            c.room_supported,
            c.room_bare,
            c.slept_on.values().sum::<usize>()
        );
        accumulate_seed(&mut totals, c);
    }

    println!();
    println!("== totals over the sweep ==");
    let bouts = totals.rested + totals.slept;
    println!(
        "  seeds that produced a world : {}",
        SEEDS.len() - empty.len()
    );
    println!("  seeds with no world         : {empty:?}");
    println!("  bodies actually observed    : {}", totals.observed_bodies);
    println!("  INCOMPLETE body derivations : {incomplete_bodies:?}  (must be empty)");
    println!("  TRUNCATED runs              : {truncated:?}  (must be empty)");
    println!("  RESTED bouts                : {}", totals.rested);
    println!("  SLEPT bouts                 : {}", totals.slept);
    println!("  bouts, total                : {bouts}");
    println!(
        "  room-supported / non-Bare   : {}  ({:.4} of bouts)",
        totals.room_supported,
        ratio(totals.room_supported, bouts)
    );
    println!(
        "  room unsupported / Bare     : {}  ({:.4} of bouts)",
        totals.room_bare,
        ratio(totals.room_bare, bouts)
    );
    println!("  SLEPT_ON facts by kind      : {:?}", totals.slept_on);
    println!("  walked rooms by quadrant (distinct FacetIds within each seed, summed):");
    for i in 0..4 {
        println!(
            "    built={} cold={} : {:>6} rooms   room-supports-rest-somewhere={}",
            i / 2 == 1,
            i % 2 == 1,
            totals.quadrant_counts[i],
            totals.quadrants_supported[i]
        );
    }
    println!("  added surfaces in distinct walked rooms (worlds reached / worlds built):");
    for kind in ADDED_SURFACES {
        println!(
            "    {:>8} : {:>6} rooms   worlds={}/{}",
            kind.0,
            totals.surface_room_counts.get(&kind).copied().unwrap_or(0),
            totals.surface_world_counts.get(&kind).copied().unwrap_or(0),
            SEEDS.len() - empty.len()
        );
    }
    match reversal_witness(&totals.grade_observations) {
        Some((one, other)) => {
            println!("  P2 live reversal witness:");
            print_grade_observation(one);
            print_grade_observation(other);
        }
        None => println!("  P2 live reversal witness     : NONE"),
    }
    match totals
        .grade_observations
        .iter()
        .find(|o| o.first_grade.to_bits() != o.second_grade.to_bits())
    {
        Some(witness) => {
            println!("  P4 consequential-room witness:");
            print_grade_observation(witness);
        }
        None => println!("  P4 consequential-room witness: NONE"),
    }
}

/// Fold one world's measurement into the sweep totals.
fn accumulate_seed(totals: &mut SeedCounts, c: SeedCounts) {
    totals.observed_bodies += c.observed_bodies;
    totals.ticks_done += c.ticks_done;
    totals.rested += c.rested;
    totals.slept += c.slept;
    totals.room_supported += c.room_supported;
    totals.room_bare += c.room_bare;
    for (k, n) in c.slept_on {
        *totals.slept_on.entry(k).or_default() += n;
    }
    for i in 0..4 {
        // A `FacetId` is a packed room address and is world-independent as
        // a NUMBER only; unioning across seeds conflates rooms of different
        // worlds. Sum each world's already-deduplicated count instead.
        totals.quadrant_counts[i] += c.quadrants[i].len();
        totals.quadrants_supported[i] |= c.quadrants_supported[i];
    }
    for (kind, rooms) in c.surface_rooms {
        *totals.surface_room_counts.entry(kind).or_default() += rooms.len();
        if !rooms.is_empty() {
            *totals.surface_world_counts.entry(kind).or_default() += 1;
        }
    }
    totals.grade_observations.extend(c.grade_observations);
}

/// Print one body-in-room comparison without hiding which live world supplied
/// it. Grades use enough precision to make close non-equalities inspectable.
fn print_grade_observation(o: &GradeObservation) {
    let ordering = match o.first_grade.total_cmp(&o.second_grade) {
        std::cmp::Ordering::Less => "<",
        std::cmp::Ordering::Equal => "=",
        std::cmp::Ordering::Greater => ">",
    };
    println!(
        "    seed={} room={:?} body={:?} species={} : {}={:.9} {} {}={:.9}",
        o.seed,
        o.room,
        o.body,
        o.species,
        o.first.0,
        o.first_grade,
        ordering,
        o.second.0,
        o.second_grade
    );
}

/// Find two actual bodies that order the same pair of co-composed surfaces in
/// opposite directions. Each observation already proves that body stood in a
/// real room containing both surfaces; this joins opposite orders only.
fn reversal_witness(
    observations: &[GradeObservation],
) -> Option<(&GradeObservation, &GradeObservation)> {
    type OppositeOrders<'a> = (Option<&'a GradeObservation>, Option<&'a GradeObservation>);
    let mut orders: BTreeMap<(KindId, KindId), OppositeOrders<'_>> = BTreeMap::new();
    for observation in observations {
        let entry = orders
            .entry((observation.first, observation.second))
            .or_default();
        match observation.first_grade.total_cmp(&observation.second_grade) {
            std::cmp::Ordering::Less if entry.0.is_none() => entry.0 = Some(observation),
            std::cmp::Ordering::Greater if entry.1.is_none() => entry.1 = Some(observation),
            _ => {}
        }
    }
    orders
        .into_values()
        .find_map(|(less, greater)| Some((less?, greater?)))
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
        seed,
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

/// Read the body-integrity check and the requested counts off the finished
/// ledger.
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
    seed: u64,
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
    let objects = object_registry();
    let sleep_grades = hornvale_species::sleep_grade_registry();
    let realms = hornvale_species::habitat_realm_registry();
    let mut c = SeedCounts {
        observed_bodies: npcs.len(),
        ticks_done,
        ..Default::default()
    };

    for npc in npcs {
        let mut body_rooms = BTreeSet::new();
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
            if !c.quadrants_supported[slot] && room_supports_rest(&room, npc, &terrain) {
                c.quadrants_supported[slot] = true;
            }
            if body_rooms.insert(id) {
                observe_room(
                    seed,
                    id,
                    &room,
                    npc,
                    &terrain,
                    &objects,
                    &sleep_grades,
                    &realms,
                    &mut c,
                );
            }
        }

        // (1)+(2) THE BOUTS AND THEIR ROOM-LEVEL SUPPORT BOOLEAN.
        for (predicate, is_sleep) in [(RESTED, false), (SLEPT, true)] {
            for f in ledger.facts_of(npc.entity, predicate) {
                let Some(day) = f.day else { continue };
                if is_sleep {
                    c.slept += 1;
                } else {
                    c.rested += 1;
                }
                // Mirrors only `rest_timeline`'s private ROOM FALLBACK: the
                // room is the last committed `agent-at` at or before the
                // bout's day, defaulting to the body's home, and the boolean
                // is `room_affords_rest` of that room. This is NOT the final
                // `SiteGrade`: a same-day `SLEPT_ON` makes production use
                // `SiteGrade::On(kind)`. RECONSTRUCTION, not the same
                // function — it can drift.
                let room = agent_position(ledger, npc, day);
                if room_supports_rest(&room, npc, &terrain) {
                    c.room_supported += 1;
                } else {
                    c.room_bare += 1;
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

/// Inspect one room an actual body stood in. Physical surface reach is counted
/// independently of the body's mass; P2/P4 comparisons use only surfaces
/// `offered_to` says that body can actually use.
#[allow(clippy::too_many_arguments)]
fn observe_room(
    seed: u64,
    room_id: FacetId,
    room: &Facet,
    body: &Body,
    terrain: &dyn Terrain,
    objects: &ComponentStore<KindId, ObjectTraits>,
    sleep_grades: &ComponentStore<KindId, f64>,
    realms: &ComponentStore<KindId, HabitatRealm>,
    counts: &mut SeedCounts,
) {
    let interior = interior_of(room, terrain);
    let physical: BTreeSet<KindId> = interior
        .ids()
        .into_iter()
        .map(|id| interior.anchor(id).kind)
        .filter(|kind| {
            objects
                .get(kind)
                .is_some_and(|traits| traits.rest.is_some())
        })
        .collect();
    for kind in &physical {
        if ADDED_SURFACES.contains(kind) {
            counts
                .surface_rooms
                .entry(*kind)
                .or_default()
                .insert(room_id);
        }
    }

    let usable: Vec<KindId> = physical
        .into_iter()
        .filter(|kind| offered_to(*kind, body).contains(&OfferedVerb::Sleep))
        .collect();
    for first_index in 0..usable.len() {
        for &second in &usable[first_index + 1..] {
            let first = usable[first_index];
            counts.grade_observations.push(GradeObservation {
                seed,
                room: room_id,
                body: body.entity,
                species: body.species.clone(),
                first,
                first_grade: reconstructed_grade(body, first, objects, sleep_grades, realms),
                second,
                second_grade: reconstructed_grade(body, second, objects, sleep_grades, realms),
            });
        }
    }
}

/// **A RECONSTRUCTION of private `liveness::grade_of` and
/// `liveness::sleep_traits_of`.** The object row, species sleep-grade row,
/// realm lookup, substrate curve, formula, and `FIT_FLOOR` match those
/// functions today. Keeping it here avoids widening the production surface
/// for a one-time measurement, at the cost that a future private change can
/// drift silently.
fn reconstructed_grade(
    body: &Body,
    kind: KindId,
    objects: &ComponentStore<KindId, ObjectTraits>,
    sleep_grades: &ComponentStore<KindId, f64>,
    realms: &ComponentStore<KindId, HabitatRealm>,
) -> f64 {
    let Some(surface) = objects.get(&kind).and_then(|traits| traits.rest) else {
        return 1.0;
    };
    let afforded_gain = sleep_grades
        .get_by_label(&body.species)
        .copied()
        .unwrap_or_else(|| {
            sleep_grades
                .get_by_label("human")
                .copied()
                .expect("the sleep-grade registry has its documented human fallback")
        });
    let fit = match surface.substrate {
        Substrate::Made => 1.0,
        Substrate::Natural(hardness) => hornvale_species::substrate_response(
            realms
                .get_by_label(&body.species)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE),
        )
        .eval(hardness, RECONSTRUCTED_FIT_FLOOR),
    };
    1.0 + (afforded_gain - 1.0) * surface.offer * fit
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

/// **A RECONSTRUCTION of private `liveness::room_affords_rest`**, which this
/// probe may not call and must not widen to `pub` — a measurement that changes
/// the surface it measures is not a measurement.
///
/// Its room-level BOOLEAN is exact TODAY and is not the same function.
/// `room_affords_rest` derives the interior and delegates to
/// `sleep_site::room_offers_sleep(&interior, body, objects)`, borrowing the
/// object roster the fold built once. That delegate asks `.next().is_some()`
/// of private `sleep_candidates`, whose predicate reads each kind's traits
/// from the borrowed roster and applies private `offered_to_traits`. This
/// says only that the room supports a non-`Bare` fallback; it does not
/// reproduce `rest_timeline`'s final `SiteGrade::On(kind)` where a same-day
/// `SLEPT_ON` fact exists.
///
/// This reconstruction must instead use public [`offered_to`], which rebuilds
/// the same object registry per anchor; its cost is deliberately different but
/// its boolean answer is identical today. `room_offers_sleep`,
/// `sleep_candidates`, and `offered_to_traits` are all crate-private, so a
/// future change to that production chain can make this public reconstruction
/// drift without a compile error.
///
/// Through `offered_to`, NOT `offered_to_observer`: physical restoration is
/// not gated on knowledge, and `room_affords_rest`'s own delegate uses the
/// unknowing form.
fn room_supports_rest(room: &Facet, body: &Body, terrain: &dyn Terrain) -> bool {
    let interior = interior_of(room, terrain);
    interior
        .ids()
        .into_iter()
        .any(|a| offered_to(interior.anchor(a).kind, body).contains(&OfferedVerb::Sleep))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// MUTATION THIS MUST FAIL AGAINST: union the per-seed quadrant sets into
    /// one cross-seed set. A packed room address identifies a place only
    /// within its world, so the same number in two worlds is two rooms.
    #[test]
    fn aggregation_counts_world_independent_addresses_once_per_seed() {
        let reused_address = FacetId(32);
        let mut first = SeedCounts::default();
        first.quadrants[0].insert(reused_address);
        let mut second = SeedCounts::default();
        second.quadrants[0].insert(reused_address);

        let mut totals = SeedCounts::default();
        accumulate_seed(&mut totals, first);
        accumulate_seed(&mut totals, second);

        assert_eq!(
            totals.quadrant_counts[0], 2,
            "the same packed address in two worlds must count as two rooms"
        );
    }

    /// MUTATION THIS MUST FAIL AGAINST: omit the actual body count from the
    /// cross-seed accumulator and keep reporting the configured request.
    #[test]
    fn aggregation_sums_observed_bodies_across_seeds() {
        let first = SeedCounts {
            observed_bodies: 3,
            ..Default::default()
        };
        let second = SeedCounts {
            observed_bodies: 4,
            ..Default::default()
        };

        let mut totals = SeedCounts::default();
        accumulate_seed(&mut totals, first);
        accumulate_seed(&mut totals, second);

        assert_eq!(
            totals.observed_bodies, 7,
            "the sweep must sum bodies actually returned by each seed's derivations"
        );
    }
}
