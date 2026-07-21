//! The population health metric (The Temperament, Stage 3B): a self-scoring
//! read of creature *affect* over a simulated span — the epidemiology of a
//! world's minds, the temporal analog of the correspondence-completeness audit.
//!
//! It runs the vessel drive-simulation forward N days over a world's derived
//! creatures, reads each one's felt state per tick through `affect_of` (spec
//! §7), and reduces the per-creature affect time series to a distress FAMILY
//! (not one number): prevalence, chronicity, recovery-rate, by-cause, and
//! by-species. Searching (normal seeking) is excluded — only the negative-
//! valence regions count as distress (spec §8). Deterministic: a pure function
//! of the world, the same session-sandboxed tick run headless.

use hornvale_kernel::{Ledger, World, WorldTime, tick};
use hornvale_locale::LocaleContext;
use hornvale_vessel::liveness::{
    AGENT_AT, Affect, AffectLabel, DRANK, DriveKind, DriveMovements, EATEN, LocaleTerrain, Npc,
    RESTED, SUSTENANCE, Terrain, affect_of, derive_npcs, waking_offset,
};
use std::collections::BTreeMap;

/// Days simulated per world — long enough to span several full drive cycles
/// (thirst rises over ~5–6 days and resets on a drink), so a chronic block is
/// distinguishable from a normal seek.
const HEALTH_TICKS: usize = 40;

/// Creatures derived per world (a representative sample, not the whole roster —
/// the session's own small constant, spec §4).
const HEALTH_NPCS: usize = 6;

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
/// deterministic (`DriveMovements::step` draws nothing new; `affect_of` is a
/// read). This is the shared core of both the real-world sweep and the
/// synthetic null-control / injected-fault scenarios.
/// type-audit: bare-ok(count: ticks)
pub fn run_simulation(
    seed_ledger: &Ledger,
    registry: &hornvale_kernel::ConceptRegistry,
    npcs: &[Npc],
    terrain: &dyn Terrain,
    ticks: usize,
) -> Vec<Vec<Affect>> {
    let mut ledger = seed_ledger.clone();
    let mut traces: Vec<Vec<Affect>> = vec![Vec::new(); npcs.len()];
    let mut day = 0.0_f64;
    for _ in 0..ticks {
        let sys = DriveMovements {
            npcs: npcs.to_vec(),
            from: WorldTime { day },
            to: WorldTime { day: day + 1.0 },
            params: SUSTENANCE,
            terrain,
        };
        // The kernel tick applies the drive-movement facts; the same headless
        // step `Session::wait` runs, minus the player.
        ledger = match tick(&ledger, &[&sys], &["drive-movements"], registry) {
            Ok(next) => next,
            Err(_) => break,
        };
        day += 1.0;
        // Sample each creature at a representative WAKING moment of the day just
        // simulated — not midnight, where a diurnal creature is asleep (The
        // Slumber): distress is a waking state, so the metric reads it while up.
        for (i, npc) in npcs.iter().enumerate() {
            let now = WorldTime {
                day: (day - 1.0) + waking_offset(npc.activity),
            };
            traces[i].push(affect_of(&ledger, npc, now, terrain));
        }
    }
    traces
}

/// Simulate a real world's derived population and return each creature's affect
/// trace. `None` if the world has no locale or no settlement to derive from.
pub fn simulate_world(world: &World) -> Vec<AffectTrace> {
    let Ok(ctx) = LocaleContext::build(world) else {
        return Vec::new();
    };
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
    let npcs = derive_npcs(world, &ctx, &mut ledger, HEALTH_NPCS, home);
    // The world's calendar, so the wake cycle reads the real sun (Tier-1).
    let calendar = hornvale_worldgen::sky_of(world)
        .ok()
        .and_then(|sky| sky.calendar().cloned());
    // The predator-pressure field (The Quarry), so danger senses carnivore
    // territory. A demography fit — bounded to the ~5 null-control seeds.
    let predator = hornvale_worldgen::predator_pressure(world).ok();
    let terrain =
        LocaleTerrain::with_calendar_and_predators(&ctx, calendar.as_ref(), predator.as_ref());
    let traces = run_simulation(&ledger, &registry, &npcs, &terrain, HEALTH_TICKS);
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
/// type-audit: bare-ok(ratio: prevalence), bare-ok(ratio: chronicity), bare-ok(count: recovery_ticks), bare-ok(ratio: by_cause), bare-ok(ratio: by_species)
#[derive(Clone, Debug, PartialEq)]
pub struct HealthReport {
    /// Fraction of creature-ticks in distress (instantaneous prevalence).
    pub prevalence: f64,
    /// Fraction of creatures with a distress run of at least `CHRONIC_TICKS` —
    /// the bug alarm (persistently stuck, not transiently seeking).
    pub chronicity: f64,
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
        // A run still open at the end never recovered — it is chronic, not a
        // recovered spike (already reflected in `chronic` if long enough).
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
        recovery_ticks,
        by_cause,
        by_species,
    }
}
