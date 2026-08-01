//! The population health metric (The Temperament, Stage 3B): a self-scoring
//! read of creature *affect* over a simulated span — the epidemiology of a
//! world's minds, the temporal analog of the correspondence-completeness audit.
//!
//! It runs the vessel drive-simulation forward N days over a world's derived
//! creatures, reads each one's felt state per tick through `affect_of_memo_
//! occupied` (spec §7; occupancy-aware since The Threshold task 6b — see
//! `run_simulation`'s own doc for why the sampler needed a real `Occupancy`
//! rather than always reading a room's landing anchor), and reduces the
//! per-creature affect time series to a distress FAMILY (not one number):
//! prevalence, chronicity, stuck (the alarm), recovery-rate, by-cause, and
//! by-species. Searching (normal seeking) is excluded — only the negative-
//! valence regions count as distress (spec §8). Deterministic: a pure function
//! of the world, the same session-sandboxed tick run headless.

use hornvale_kernel::{Ledger, RoomMeshMemo, World, WorldTime, tick};
use hornvale_locale::LocaleContext;
use hornvale_vessel::liveness::{
    AGENT_AT, Affect, AffectLabel, DRANK, DriveKind, DriveMovements, EATEN, HomeNavCache,
    LocaleTerrain, Npc, PrimaryAfraidMemo, RESTED, SUSTENANCE, Terrain, affect_of_memo_occupied,
    agent_position, built_rooms, derive_npcs, waking_offset,
};
use std::collections::BTreeMap;

/// Days simulated per world — long enough to span several full drive cycles
/// (thirst rises over ~5–6 days and resets on a drink), so a chronic block is
/// distinguishable from a normal seek.
const HEALTH_TICKS: usize = 40;

/// Creatures derived per world (a representative sample, not the whole roster —
/// the session's own small constant, spec §4).
const HEALTH_NPCS: usize = 6;

/// Wild beast agents derived per world (The Wilding) — the fauna's contribution
/// to the population-health sample.
const HEALTH_WILD: usize = 4;

/// Consecutive distress ticks that count as CHRONIC (persistently stuck, the
/// learned-helplessness / bug-alarm signal, spec §8) rather than a transient
/// spike a healthy mind recovers from.
const CHRONIC_TICKS: usize = 8;

/// One creature's affect over the simulated span, tagged with its species.
/// type-audit: bare-ok(identifier-text: species)
pub struct AffectTrace {
    /// The creature's species (for by-species attribution).
    pub species: String,
    /// Its felt state at each simulated day.
    pub affects: Vec<Affect>,
}

/// A negative-valence (distress) region — the metric measures these, never the
/// neutral `Searching` nor the positive `Content`/`Eager` (spec §8).
fn is_distress(label: AffectLabel) -> bool {
    matches!(
        label,
        AffectLabel::Frustrated | AffectLabel::Lost | AffectLabel::Helpless
    )
}

/// Run the drive-simulation forward over `npcs` on `terrain`, reading each
/// creature's affect after every tick. The ledger evolves in a clone (the
/// session-sandbox discipline); `terrain` and `npcs` are the scenario. Pure and
/// deterministic (`DriveMovements::step` draws nothing new; the affect read is
/// a read). This is the shared core of both the real-world sweep and the
/// synthetic null-control / injected-fault scenarios.
///
/// `day_length_std` is the world's rotation period in standard days, passed
/// through to the tick so the action clock can divide the local day exactly
/// (The Action Clock, spec §4.1); `None` for a tidally-locked world and for the
/// planted-terrain synthetic scenarios, which have no sky.
///
/// **The Threshold task 6b:** this used to sample through `affect_of_memo`,
/// which has no per-tick state of its own and so always read interior warmth
/// at a room's landing anchor — the doorway a creature crossing in arrives
/// at — regardless of where its walk that tick actually carried it. That
/// made the battery structurally blind to `Thermal::warmth`'s within-room
/// seeking (a creature genuinely standing at a hearth read as cold as one
/// standing at the threshold three hops away), which is an instrument gap,
/// not a physics one: the preregistered prediction (spec §7) can only be
/// tested if the sampler reports what the creature actually experienced.
/// `DriveMovements::step_with_occupancy` already derives exactly that per
/// tick (task 6) but discards it once `step` returns; this now also calls it
/// directly — a second, PURE re-evaluation of the same frozen ledger and
/// system alongside the `tick()` call that applies its facts, not a second
/// simulation with different consequences — purely to recover the
/// `Occupancy` [`affect_of_memo_occupied`] needs to read warmth at the anchor
/// a creature actually reached.
/// type-audit: bare-ok(count: ticks), bare-ok(ratio: day_length_std)
pub fn run_simulation(
    seed_ledger: &Ledger,
    registry: &hornvale_kernel::ConceptRegistry,
    npcs: &[Npc],
    terrain: &dyn Terrain,
    ticks: usize,
    day_length_std: Option<f64>,
) -> Vec<Vec<Affect>> {
    let mut ledger = seed_ledger.clone();
    let mut traces: Vec<Vec<Affect>> = vec![Vec::new(); npcs.len()];
    let mut day = 0.0_f64;
    // The session-lived geometry memo (the-waymark fix round, Finding 2):
    // owned HERE, above the tick loop, rather than rebuilt (and discarded)
    // inside `step_with_occupancy` every one of the `ticks` iterations —
    // `run_simulation` IS the "struct/scope that owns the sim loop" for a
    // lab run (one `RoomMeshMemo` per process; `windows/lab/CLAUDE.md`:
    // nextest runs each test in its own process, so this is exactly as
    // session-lived as a lab run can be). `terrain`'s OWN prefilled
    // `corner_weights` cache (Finding 1) is a SEPARATE, independent memo —
    // see `simulate_world`'s own comment for why it can't be this same
    // instance (it must be embedded, read-only, in `terrain` for the WHOLE
    // sweep, while this one needs `&mut` access every tick for `neighbors`).
    let mut mesh_memo = RoomMeshMemo::new();
    // The session-lived, CROSS-tick home-plan cache (the-waymark, Task 4):
    // `run_simulation` IS the "struct/scope that owns the sim loop" for a lab
    // run (see `mesh_memo`'s own comment above), so this cache — unlike
    // `mesh_memo`, it must survive ACROSS calls to `step_with_occupancy`, not
    // merely across one call's own creatures/pops, to show the campaign's
    // scaling bar (a stationary, unchanged-belief creature pays zero searches
    // after its first tick) — lives here too, one per run.
    let mut home_nav_cache = HomeNavCache::new();
    for _ in 0..ticks {
        let sys = DriveMovements {
            npcs: npcs.to_vec(),
            from: WorldTime { day },
            to: WorldTime { day: day + 1.0 },
            params: SUSTENANCE,
            day_length_std,
            terrain,
        };
        // Recover this tick's within-room `Occupancy` alongside the facts
        // `tick()` (below) commits — the same walk, read twice: once here for
        // the ephemeral occupancy `step`'s `TickSystem` impl otherwise
        // discards, once inside `tick()` for the committed `Fact`s. Both
        // calls read the identical frozen `ledger`, so this changes nothing
        // about how the world evolves — only what the affect sample below
        // gets to see.
        let (_facts, occupancy) =
            sys.step_with_occupancy(&ledger, &mut mesh_memo, &mut home_nav_cache);
        // The kernel tick applies the drive-movement facts; the same headless
        // step `Session::wait` runs, minus the player. This path goes through
        // `TickSystem::step`, whose signature is kernel-fixed and so cannot
        // carry `mesh_memo` — it pays full `neighbors()` cost internally
        // (see `DriveMovements::step`'s own comment). Only the occupancy
        // recovery above, and the affect reads below, share this run's memo.
        ledger = match tick(&ledger, &[&sys], &["drive-movements"], registry) {
            Ok(next) => next,
            Err(_) => break,
        };
        day += 1.0;
        // Sample each creature at a representative WAKING moment of the day just
        // simulated — not midnight, where a diurnal creature is asleep (The
        // Slumber): distress is a waking state, so the metric reads it while up.
        // One primary-afraid memo for this tick's reads: `ledger` is fixed across
        // them, so an emitter's `(entity, day)` fear verdict — folded by EVERY
        // creature's `believed_hazard` (The Phantom) — is re-derived once, not
        // once per creature per cell. Byte-identical: a cache of a pure function
        // over a fixed ledger (see `PrimaryAfraidMemo`).
        let mut afraid_memo = PrimaryAfraidMemo::new();
        for (i, npc) in npcs.iter().enumerate() {
            let now = WorldTime {
                day: (day - 1.0) + waking_offset(npc.activity),
            };
            traces[i].push(affect_of_memo_occupied(
                &ledger,
                npc,
                npcs,
                now,
                terrain,
                &mut afraid_memo,
                Some(&occupancy),
                &mut mesh_memo,
                &mut home_nav_cache,
            ));
        }
    }
    traces
}

/// [`run_simulation`]'s twin for a REAL, `LocaleContext`-backed world (the-waymark
/// fix round, Finding 1): where `run_simulation` is handed one fixed `&dyn Terrain`
/// for its whole run (so it stays provider-agnostic — `synthetic.rs`'s planted-
/// terrain scenarios and `hearth_population_calibration.rs` both rely on that),
/// this one owns the concrete `ctx`/`calendar`/`predator`/`prey`/`built`
/// ingredients and rebuilds `LocaleTerrain` FRESH every tick, prefilling its
/// read-only geometry cache for each NPC's CURRENT position (read from the
/// evolving `ledger`, exactly the way `windows/vessel`'s `Session::wait` does
/// every real tick) rather than only the FIRST tick's starting position the
/// way a single fixed `terrain` built once could. This is
/// the ONLY caller that can do this (it alone has a real `LocaleContext` to
/// rebuild from), so it is a separate function rather than a `run_simulation`
/// parameter that every other caller would have to thread `None` through.
/// type-audit: bare-ok(count: ticks), bare-ok(ratio: day_length_std), bare-ok(ratio: predator), bare-ok(ratio: prey)
#[allow(clippy::too_many_arguments)]
pub fn run_simulation_with_locale(
    seed_ledger: &Ledger,
    registry: &hornvale_kernel::ConceptRegistry,
    npcs: &[Npc],
    ctx: &LocaleContext,
    calendar: Option<&hornvale_astronomy::Calendar>,
    predator: Option<&hornvale_kernel::CellMap<f64>>,
    prey: Option<&hornvale_kernel::CellMap<f64>>,
    built: Option<&std::collections::BTreeSet<hornvale_kernel::RoomId>>,
    ticks: usize,
    day_length_std: Option<f64>,
) -> Vec<Vec<Affect>> {
    let mut ledger = seed_ledger.clone();
    let mut traces: Vec<Vec<Affect>> = vec![Vec::new(); npcs.len()];
    let mut day = 0.0_f64;
    // Session-lived across the WHOLE run (Finding 2), same as
    // `run_simulation`'s own `mesh_memo` — but here it ALSO backs each
    // tick's `LocaleTerrain` cache (via a per-tick snapshot clone below), so
    // a room read on tick N stays warm for tick N+1 even if no creature is
    // standing there right now.
    let mut mesh_memo = RoomMeshMemo::new();
    // Cross-tick, one per run — see `run_simulation`'s identical comment.
    let mut home_nav_cache = HomeNavCache::new();
    let geo = ctx.climate().geosphere();
    let index = ctx.nearest_index();
    for _ in 0..ticks {
        // Prefill THIS tick's geometry cache for every NPC's CURRENT
        // position and its three neighbours (Finding 1) — under `&mut`,
        // strictly before the `LocaleTerrain`/drives built from it exist.
        // Mirrors `Session::wait`'s per-tick `before` prefill exactly.
        for npc in npcs {
            let pos = agent_position(&ledger, npc, WorldTime { day });
            pos.corner_weights_memo(geo, index, &mut mesh_memo);
            for n in pos.neighbors_memo(&mut mesh_memo) {
                n.corner_weights_memo(geo, index, &mut mesh_memo);
            }
        }
        // A read-only snapshot for `LocaleTerrain`, so `mesh_memo` stays
        // independently `&mut`-able below (`step_with_occupancy`'s
        // `neighbors` threading) — see `Session::wait`'s identical comment.
        let mesh_snapshot = mesh_memo.clone();
        let terrain =
            LocaleTerrain::with_fields(ctx, calendar, predator, prey, built, Some(&mesh_snapshot));
        let sys = DriveMovements {
            npcs: npcs.to_vec(),
            from: WorldTime { day },
            to: WorldTime { day: day + 1.0 },
            params: SUSTENANCE,
            day_length_std,
            terrain: &terrain,
        };
        let (_facts, occupancy) =
            sys.step_with_occupancy(&ledger, &mut mesh_memo, &mut home_nav_cache);
        ledger = match tick(&ledger, &[&sys], &["drive-movements"], registry) {
            Ok(next) => next,
            Err(_) => break,
        };
        day += 1.0;
        let mut afraid_memo = PrimaryAfraidMemo::new();
        for (i, npc) in npcs.iter().enumerate() {
            let now = WorldTime {
                day: (day - 1.0) + waking_offset(npc.activity),
            };
            traces[i].push(affect_of_memo_occupied(
                &ledger,
                npc,
                npcs,
                now,
                &terrain,
                &mut afraid_memo,
                Some(&occupancy),
                &mut mesh_memo,
                &mut home_nav_cache,
            ));
        }
    }
    traces
}

/// Simulate a real world's derived population and return each creature's affect
/// trace. `None` if the world has no locale or no settlement to derive from.
// Named construction site (decision 0092): sculpts/fits exactly ONCE per
// sweep (The Weir, Stage 2), mirroring `Session::start`.
#[allow(clippy::disallowed_methods)]
pub fn simulate_world(world: &World) -> Vec<AffectTrace> {
    // ONE derivation block (The Weir, Stage 2): terrain, climate, and the
    // locale context are each derived EXACTLY ONCE here, then threaded into
    // `LocaleContext::build_from`, the demography fit, and the predator/prey
    // pressures below — mirrors `Session::start`, so `ctx` no longer
    // quietly re-sculpts its own copy underneath the sweep's own terrain/
    // climate derivation.
    let Ok(terrain) = hornvale_worldgen::terrain_of(world) else {
        return Vec::new();
    };
    let Ok(climate) = hornvale_worldgen::climate_from(world, &terrain) else {
        return Vec::new();
    };
    let ctx = LocaleContext::build_from(world, &terrain, &climate);
    let mut ledger = world.ledger.clone();
    let mut registry = world.registry.clone();
    // The two session-only predicates the drive tick commits — registered on
    // the clone, never at genesis (spec §3; same as `Session::start`).
    let _ = registry.register_predicate(AGENT_AT, false, "an agent's position on a day");
    let _ = registry.register_predicate(DRANK, false, "an agent satisfied its sustenance goal");
    let _ = registry.register_predicate(RESTED, false, "an agent rested on a day");
    let _ = registry.register_predicate(EATEN, false, "an agent ate on a day");
    let home = match hornvale_settlement::all_settlements(world).first() {
        Some(v) => v.id,
        None => return Vec::new(),
    };
    let mut npcs = derive_npcs(world, &ctx, &mut ledger, HEALTH_NPCS, home);
    // The species roster and the demography report — assembled/fit ONCE per
    // sweep (The Weir, Stage 1b/2) and shared below by the wild-NPC
    // derivation and the predator/prey fields, instead of each
    // independently re-running the coexistence-stack fit over the same
    // `(world, wc, terrain, climate)`. `None` on any failure (the
    // dependents simply lose their demography-derived axis, same posture as
    // `calendar`/`predator`/`prey` below).
    let wc = hornvale_worldgen::WorldComponents::assemble().ok();
    // Wrapped in `Some` from here on: both derivations above already
    // succeeded (the early returns), so this `Option` is the same
    // defensive-field posture `Session::terrain`/`Session::climate` carry,
    // never a second independent derivation that could fail where this one
    // didn't.
    let terrain = Some(terrain);
    let climate = Some(climate);
    let report = match (wc.as_ref(), terrain.as_ref(), climate.as_ref()) {
        (Some(wc), Some(terrain), Some(climate)) => {
            hornvale_worldgen::demography_report_from(world, wc, terrain, climate).ok()
        }
        _ => None,
    };
    // The Wilding: the world's health includes its fauna — append a few wild
    // beast agents, so a herbivore's live predator-fear is measured too.
    let concentrations = match (wc.as_ref(), report.as_ref()) {
        (Some(wc), Some(report)) => {
            hornvale_worldgen::wild_concentrations_from(wc, report, HEALTH_WILD)
        }
        _ => Vec::new(),
    };
    npcs.extend(hornvale_vessel::liveness::derive_wild_npcs(
        world,
        &ctx,
        &mut ledger,
        concentrations,
    ));
    // The world's calendar, so the wake cycle reads the real sun (Tier-1).
    let calendar = hornvale_worldgen::sky_of(world)
        .ok()
        .and_then(|sky| sky.calendar().cloned());
    // The predator-pressure field (The Quarry), so danger senses carnivore
    // territory — from the shared `report` above (The Weir, Stage 1b)
    // rather than its own fit.
    let predator = match (wc.as_ref(), terrain.as_ref(), report.as_ref()) {
        (Some(wc), Some(terrain), Some(report)) => Some(hornvale_worldgen::predator_pressure_from(
            wc, terrain, report,
        )),
        _ => None,
    };
    // The prey-pressure field (The Teeth), so a carnivore's hunger senses
    // prey — the dual of the predator field, same shared fit.
    let prey = match (wc.as_ref(), terrain.as_ref(), report.as_ref()) {
        (Some(wc), Some(terrain), Some(report)) => {
            Some(hornvale_worldgen::prey_pressure_from(wc, terrain, report))
        }
        _ => None,
    };
    // The settlement-territory set (The Threshold, task 5b) — this sweep is a
    // real world with real settlements, so it is the other construction site
    // that has a world to read one from (`session.rs`'s live session is the
    // other); a room a settlement occupies can now draw a real hearth here
    // too, the same way it does mid-possession.
    let built = built_rooms(world, &ctx);
    // The rotation period the action clock divides (spec §4.1) — the same
    // calendar the wake cycle already reads; `None` if the world is
    // tidally-locked or has no derivable sky.
    let day_length_std = calendar
        .as_ref()
        .and_then(|c| c.day_length())
        .map(|d| d.get());
    // `run_simulation_with_locale` (the-waymark fix round, Finding 1): rebuilds
    // `LocaleTerrain` fresh EVERY tick with a per-tick geometry prefill for each
    // NPC's CURRENT position, rather than one `LocaleTerrain` fixed for the
    // whole 40-tick run whose prefill could only ever cover tick 1's starting
    // rooms. See that function's own doc for why this sweep gets its own twin
    // of `run_simulation` instead of a shared, generic path.
    let traces = run_simulation_with_locale(
        &ledger,
        &registry,
        &npcs,
        &ctx,
        calendar.as_ref(),
        predator.as_ref(),
        prey.as_ref(),
        Some(&built),
        HEALTH_TICKS,
        day_length_std,
    );
    npcs.into_iter()
        .zip(traces)
        .map(|(npc, affects)| AffectTrace {
            species: npc.species,
            affects,
        })
        .collect()
}

/// The population health family (spec §8) — distress epidemiology over a set of
/// affect traces. Every fraction is in `0.0..=1.0`; `recovery_ticks` is the
/// mean length of a distress spike that DID recover (shorter = more resilient),
/// `None` when there were no recovered spikes.
/// type-audit: bare-ok(ratio: prevalence), bare-ok(ratio: chronicity), bare-ok(ratio: stuck), bare-ok(count: recovery_ticks), bare-ok(ratio: by_cause), bare-ok(ratio: by_species)
#[derive(Clone, Debug, PartialEq)]
pub struct HealthReport {
    /// Fraction of creature-ticks in distress (instantaneous prevalence).
    pub prevalence: f64,
    /// Fraction of creatures with a distress run of at least `CHRONIC_TICKS` —
    /// a DIAGNOSTIC, not the alarm. A long run that RECOVERED is a hard patch
    /// in a varied world, which The Temperament §8 calls legitimate; see
    /// `stuck` for the bug signal.
    pub chronicity: f64,
    /// Fraction of creatures with a distress run of at least `CHRONIC_TICKS`
    /// **that never ended** — THE BUG ALARM: §8's conjunction ("elevated
    /// chronicity, no recovery"), evaluated per creature so one stuck creature
    /// among many recovering ones cannot be masked by a population aggregate.
    pub stuck: f64,
    /// Mean length (ticks) of a distress spike that recovered; `None` if none
    /// did (a healthy world with no spikes, or one where every spike persisted).
    pub recovery_ticks: Option<f64>,
    /// Distress-tick fraction attributed to each drive (by-cause).
    pub by_cause: BTreeMap<String, f64>,
    /// Distress prevalence per species (by-species).
    pub by_species: BTreeMap<String, f64>,
}

/// Reduce a set of affect traces to the health family.
pub fn health_report(traces: &[AffectTrace]) -> HealthReport {
    let mut distress_ticks = 0usize;
    let mut total_ticks = 0usize;
    let mut chronic_creatures = 0usize;
    let mut stuck_creatures = 0usize;
    let mut recovered_runs: Vec<usize> = Vec::new();
    // by-cause: distress ticks attributed to the pursued drive's kind.
    let mut cause_thirst = 0usize;
    let mut cause_thermal = 0usize;
    let mut cause_fatigue = 0usize;
    let mut cause_hunger = 0usize;
    let mut cause_danger = 0usize;
    let mut cause_social = 0usize;
    // by-species: (distress ticks, total ticks) per species.
    let mut species_stats: BTreeMap<String, (usize, usize)> = BTreeMap::new();

    for trace in traces {
        let mut run = 0usize; // current consecutive-distress run length
        let mut chronic = false;
        let stats = species_stats.entry(trace.species.clone()).or_insert((0, 0));
        for a in &trace.affects {
            total_ticks += 1;
            stats.1 += 1;
            if is_distress(a.label) {
                distress_ticks += 1;
                stats.0 += 1;
                run += 1;
                if run >= CHRONIC_TICKS {
                    chronic = true;
                }
                match a.object {
                    Some(DriveKind::Thirst) => cause_thirst += 1,
                    Some(DriveKind::Thermal) => cause_thermal += 1,
                    Some(DriveKind::Fatigue) => cause_fatigue += 1,
                    Some(DriveKind::Hunger) => cause_hunger += 1,
                    Some(DriveKind::Danger) => cause_danger += 1,
                    Some(DriveKind::Social) => cause_social += 1,
                    None => {}
                }
            } else {
                // A distress run just ended by recovering.
                if run > 0 {
                    recovered_runs.push(run);
                }
                run = 0;
            }
        }
        // A run still open at the end never recovered. If it is also LONG, this
        // creature is STUCK — the conjunction §8 names as the bug signal ("no
        // recovery, elevated chronicity"), read per creature. A run open at the
        // end is necessarily the last one, so this single check catches every
        // never-ended run.
        //
        // A SHORT open run is deliberately NOT an alarm: it might have
        // recovered one tick after the trace ended, which is right-censoring
        // and undecidable from the trace. Only long-and-open alarms; the
        // asymmetry is intentional (spec §4).
        //
        // KNOWN BLIND SPOT (decision 0080, Consequences). `stuck` reads the
        // FINAL run's fate, so it is silent on a creature in near-total
        // distress that happens to recover in the last ticks — e.g. distressed
        // for ticks 1..=38 and Content for 39..=40. That reads `stuck 0.0`
        // (the long run ended), `chronicity 1.0` and `prevalence ~0.95` (both
        // unbounded diagnostics), so EVERY surviving bound is green. The 2×2
        // this reduction implements covers one episode's length and fate, and
        // never its multiplicity or duty cycle; that is the price of choosing
        // *fate* as the discriminator, which is what §8 names. Seeing this
        // class would need a different family member (a longest-run or
        // distress-duty-cycle diagnostic) — registered as a followup, not
        // built here.
        if run >= CHRONIC_TICKS {
            stuck_creatures += 1;
        }
        if chronic {
            chronic_creatures += 1;
        }
    }

    let frac = |n: usize, d: usize| if d == 0 { 0.0 } else { n as f64 / d as f64 };
    let by_cause: BTreeMap<String, f64> = [
        ("thirst".to_string(), frac(cause_thirst, distress_ticks)),
        ("thermal".to_string(), frac(cause_thermal, distress_ticks)),
        ("fatigue".to_string(), frac(cause_fatigue, distress_ticks)),
        ("hunger".to_string(), frac(cause_hunger, distress_ticks)),
        ("danger".to_string(), frac(cause_danger, distress_ticks)),
        ("social".to_string(), frac(cause_social, distress_ticks)),
    ]
    .into_iter()
    .collect();
    let by_species: BTreeMap<String, f64> = species_stats
        .into_iter()
        .map(|(sp, (d, t))| (sp, frac(d, t)))
        .collect();
    let recovery_ticks = if recovered_runs.is_empty() {
        None
    } else {
        Some(recovered_runs.iter().sum::<usize>() as f64 / recovered_runs.len() as f64)
    };

    HealthReport {
        prevalence: frac(distress_ticks, total_ticks),
        chronicity: frac(chronic_creatures, traces.len().max(1)),
        stuck: frac(stuck_creatures, traces.len().max(1)),
        recovery_ticks,
        by_cause,
        by_species,
    }
}
