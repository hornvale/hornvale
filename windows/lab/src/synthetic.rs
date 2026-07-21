//! The synthetic distress harness (The Temperament followup #5): hand-built
//! terrain + creatures that drive the REAL sim (`run_simulation` → `affect_of`,
//! the same headless drive loop the health metric reads) into genuine distress.
//!
//! The health metric (`health.rs`) is armed by reading ZERO on real worlds: a
//! creature SEARCHES rather than despairs, so a natural population never trips
//! the distress family, and the calibration suite exercises the fault
//! signatures on *constructed* `Affect` traces (`trace(&[Lost, Lost, …])`). That
//! left a gap — the reduction was tested, but never the path from a world into
//! distress. This module closes it: three scenarios the drive model can be
//! *forced* into, each a real end-to-end run whose affect trace the metric then
//! scores, so `chronicity`/`recovery`/`by_cause`/`by_species` are proven on the
//! sim's own output, not on affect structs typed by hand.
//!
//! The two levers the drive model actually offers (the rest read Searching, the
//! load-bearing non-distress):
//! - **Stranded from known water** — a creature that BELIEVES in a source
//!   (stood in it once, so belief is folded from its history) but is now placed
//!   past the plan budget from it: `plan_to_water` gives up (`None`), the thirst
//!   affordance vanishes, and the creature Holds in sustained `Frustrated`. The
//!   "known-but-unreachable source" the retrospective named. Belief anchors to
//!   HOME's reachability, so home sits on the water and the creature is stranded
//!   away from both — the one construction the geometry allows.
//! - **A heat wave that passes** — a creature ON its water (thirst stays
//!   serviceable, so it never itself distresses) but gripped by a blistering
//!   thermal cell with no kinder neighbour: comfort is unservable and it Holds
//!   in `Frustrated` while the wave lasts, then the wave breaks, the cell cools
//!   into its niche, and it returns to `Content` — the spike-recover pattern.
//!   (A permanent hostile climate would read chronic; the recovery is the
//!   wave's end, sampled cleanly because the drive model's mid-cycle drinks are
//!   invisible to an after-the-tick affect read.)

use crate::health::{AffectTrace, run_simulation};
use hornvale_kernel::ecology::ConditionResponse;
use hornvale_kernel::{ConceptRegistry, EntityId, Ledger, RoomAddr, WorldTime};
use hornvale_species::{ActivityCycle, MetabolicClass};
use hornvale_vessel::liveness::{AGENT_AT, DRANK, Npc, Terrain, place_agent};
use std::collections::{BTreeMap, BTreeSet};

/// A hand-planted terrain field for the harness — the pub sibling of the
/// vessel tests' `PlantedTerrain`: a set of fresh-water rooms and a map of
/// per-room temperatures (°C). Elevation is uniform (`INFINITY`, the
/// undescribable-room convention) — the scenarios all give their creatures a
/// water belief, so the ignorant-exploration path that reads elevation never
/// fires. Unplanted temperatures read `INFINITY` → thermal urgency `0` (a cell
/// with no temperature registers no discomfort), so a scenario that plants no
/// temperatures is thermally silent and exercises thirst alone.
///
/// `calm_after` makes the planted temperatures TRANSIENT — a passing heat wave:
/// while `Some((day, calm))` and the sim day is past `day`, every planted room
/// reads `calm` (a comfortable temperature) instead of its hostile value, so a
/// creature boxed in by heat is released and recovers. `None` = a permanent
/// climate.
pub struct SyntheticTerrain {
    fresh: BTreeSet<RoomAddr>,
    temps: BTreeMap<RoomAddr, f64>,
    calm_after: Option<(f64, f64)>,
}

impl Terrain for SyntheticTerrain {
    fn elevation(&self, _room: &RoomAddr) -> f64 {
        f64::INFINITY
    }
    fn is_fresh_water(&self, room: &RoomAddr) -> bool {
        self.fresh.contains(room)
    }
    fn temperature(&self, room: &RoomAddr, day: WorldTime) -> f64 {
        match self.temps.get(room) {
            None => f64::INFINITY,
            Some(&hot) => match self.calm_after {
                Some((until, calm)) if day.day >= until => calm,
                _ => hot,
            },
        }
    }
}

/// A complete distress scenario: a seed ledger (already carrying the placed
/// agents' `agent-at` history), the two session-only predicates registered, the
/// creatures, and the terrain they inhabit. `simulate` runs the same headless
/// drive loop the real health sweep uses and returns each creature's affect
/// trace, ready for `health_report`.
pub struct Scenario {
    /// The seed ledger, with each creature's placement history committed.
    pub ledger: Ledger,
    /// The registry, with `agent-at`/`drank` registered (as a live session does).
    pub registry: ConceptRegistry,
    /// The creatures the scenario strands or boxes in.
    pub npcs: Vec<Npc>,
    /// The planted terrain field they inhabit.
    pub terrain: SyntheticTerrain,
}

impl Scenario {
    /// Run the drive simulation forward `ticks` days and reduce to per-creature
    /// affect traces (tagged with species), exactly as `simulate_world` does for
    /// a real world.
    /// type-audit: bare-ok(count: ticks)
    pub fn simulate(&self, ticks: usize) -> Vec<AffectTrace> {
        let traces = run_simulation(
            &self.ledger,
            &self.registry,
            &self.npcs,
            &self.terrain,
            ticks,
        );
        self.npcs
            .iter()
            .zip(traces)
            .map(|(npc, affects)| AffectTrace {
                species: npc.species.clone(),
                affects,
            })
            .collect()
    }
}

/// A registry with the two session-only predicates the drive tick commits —
/// the same pair `simulate_world` registers on its clone.
fn harness_registry() -> ConceptRegistry {
    let mut registry = ConceptRegistry::default();
    let _ = registry.register_predicate(AGENT_AT, false, "an agent's position on a day");
    let _ = registry.register_predicate(DRANK, false, "an agent satisfied its sustenance goal");
    registry
}

/// A comfortable temperature niche (wide tolerance) — used where the scenario
/// exercises thirst alone and thermal must stay quiet.
const MILD_NICHE: ConditionResponse = ConditionResponse {
    optimum: 15.0,
    width: 10.0,
    devotion: 0.5,
};

/// A narrow-tolerance niche for the thermally-stricken creature: a small band
/// around a cool optimum, so a hot planted cell reads far past tolerance.
const COOL_NICHE: ConditionResponse = ConditionResponse {
    optimum: 15.0,
    width: 5.0,
    devotion: 0.5,
};

/// A blistering planted cell temperature (°C) — far past `COOL_NICHE`'s
/// tolerance, so thermal urgency pins to its ceiling and the cell is
/// unlivable.
const BLISTERING_C: f64 = 60.0;

/// A heat-adapted niche: comfortable across a wide band up to ~65 °C, so a hot
/// waste registers NO thermal discomfort — isolating The Kindling's *thirst*
/// coupling (adaptation sets comfort, not evaporation: a heat-adapted creature
/// still dehydrates faster in heat).
const HEAT_TOLERANT_NICHE: ConditionResponse = ConditionResponse {
    optimum: 45.0,
    width: 20.0,
    devotion: 0.5,
};

/// A hot-but-livable waste temperature (°C) — inside `HEAT_TOLERANT_NICHE` (so
/// thermal stays quiet) yet well above thermoneutral, so an endotherm's thirst
/// couples (roughly double rate).
const HOT_WASTE_C: f64 = 45.0;

/// The day a passing heat wave breaks — chosen so the spike is unmistakable
/// (several days of distress) yet recovers BEFORE the chronic threshold, so the
/// signature is spike-recover (transient, resilient), distinct from the
/// stranded creature's chronic persist. The creature is boxed in from day 1, so
/// this bounds the run just under `CHRONIC_TICKS`.
const WAVE_BREAKS_DAY: f64 = 6.0;

/// The comfortable temperature (°C) a planted cell reads once the wave has
/// broken — inside `COOL_NICHE`'s band, so thermal urgency falls to `0`.
const AFTER_WAVE_C: f64 = 15.0;

/// Build a creature with the scenario-relevant fields set and the incidental
/// ones (activity, label) at sane defaults.
fn creature(
    entity: EntityId,
    home: RoomAddr,
    resource: RoomAddr,
    species: &str,
    niche: ConditionResponse,
) -> Npc {
    Npc {
        entity,
        home,
        resource,
        species: species.to_string(),
        activity: ActivityCycle::Diurnal,
        temperature_niche: niche,
        deliberation_latency: 0.5,
        time_horizon: 0.0,
        metabolic_class: MetabolicClass::Endotherm,
        label: species.to_string(),
    }
}

/// Two near-antipodal rooms guaranteed to be past the plan budget apart (the
/// uniform-cost search exhausts its 1000-node budget long before crossing the
/// mesh): `.0` sits on water and serves as a home a belief can anchor to, `.1`
/// is where a creature is stranded from it.
fn water_and_a_far_exile() -> (RoomAddr, RoomAddr) {
    (
        RoomAddr::containing([1.0, 0.0, 0.0], 6),
        RoomAddr::containing([-1.0, 0.05, 0.05], 6),
    )
}

/// **Stranded from known water** → sustained `Frustrated` (chronic, by-cause
/// thirst). The creature's home is a spring it has drunk from (belief anchors
/// there), but it is placed on the far side of the world: thirst rises, the
/// spring is past the plan budget, no affordance services it, and it Holds in
/// distress for the rest of the run — the bug-alarm signature, produced by the
/// sim rather than typed by hand.
pub fn stranded_from_known_water() -> Scenario {
    let (spring, exile) = water_and_a_far_exile();
    let mut ledger = Ledger::default();
    let registry = harness_registry();
    let e = ledger.mint_entity();
    // History: stood in the spring (belief), then stranded far away (position).
    let reg = &registry;
    ledger
        .commit(place_agent(e, &spring, WorldTime { day: 0.0 }), reg)
        .expect("place at spring");
    ledger
        .commit(place_agent(e, &exile, WorldTime { day: 0.5 }), reg)
        .expect("place in exile");
    let npc = creature(e, spring.clone(), spring.clone(), "kobold", MILD_NICHE);
    Scenario {
        ledger,
        registry,
        npcs: vec![npc],
        terrain: SyntheticTerrain {
            fresh: [spring].into_iter().collect(),
            temps: BTreeMap::new(),
            calm_after: None,
        },
    }
}

/// **Stranded in a hot waste** → the same stranding as
/// [`stranded_from_known_water`], but the exile cell is hot-but-livable
/// (`HOT_WASTE_C`, inside a heat-adapted niche so thermal stays quiet). The
/// Kindling's heat coupling quickens the endotherm's dehydration, so it crosses
/// into thirst-distress SOONER than the temperate stranding — the coupling,
/// end to end through the real sim, isolated from any thermal effect.
pub fn stranded_in_a_hot_waste() -> Scenario {
    let (spring, exile) = water_and_a_far_exile();
    let mut ledger = Ledger::default();
    let registry = harness_registry();
    let e = ledger.mint_entity();
    ledger
        .commit(place_agent(e, &spring, WorldTime { day: 0.0 }), &registry)
        .expect("place at spring");
    ledger
        .commit(place_agent(e, &exile, WorldTime { day: 0.5 }), &registry)
        .expect("place in exile");
    // The exile and its neighbours are hot-but-livable — the creature senses no
    // discomfort (heat-adapted), but its thirst couples to the heat.
    let mut temps = BTreeMap::new();
    temps.insert(exile.clone(), HOT_WASTE_C);
    for n in exile.neighbors() {
        temps.insert(n, HOT_WASTE_C);
    }
    let npc = creature(
        e,
        spring.clone(),
        spring.clone(),
        "kobold",
        HEAT_TOLERANT_NICHE,
    );
    Scenario {
        ledger,
        registry,
        npcs: vec![npc],
        terrain: SyntheticTerrain {
            fresh: [spring].into_iter().collect(),
            temps,
            calm_after: None,
        },
    }
}

/// **A heat wave that passes** → a `Frustrated` spike that recovers (by-cause
/// thermal, the recovery signal). The creature sits ON its spring, so thirst
/// stays serviceable (it drinks and resets) and never itself distresses; but a
/// blistering wave grips its cell and every neighbour with no kinder step, so
/// while it lasts the creature Holds in thermal `Frustrated`. When the wave
/// breaks (day `WAVE_BREAKS_DAY`) the cell cools into its niche, comfort is met,
/// and the creature returns to `Content` — a distress spike, chronic-length yet
/// recovered, produced end-to-end rather than typed by hand.
pub fn a_heat_wave_that_passes() -> Scenario {
    let spring = RoomAddr::containing([1.0, 0.0, 0.0], 6);
    let mut ledger = Ledger::default();
    let registry = harness_registry();
    let e = ledger.mint_entity();
    // Seed the water belief immediately (stood in the spring on day 0).
    ledger
        .commit(place_agent(e, &spring, WorldTime { day: 0.0 }), &registry)
        .expect("place at spring");
    // The spring and its three neighbours are equally blistering — no kinder
    // neighbour, so comfort is unservable (a local thermal pit) until the wave
    // breaks.
    let mut temps = BTreeMap::new();
    temps.insert(spring.clone(), BLISTERING_C);
    for n in spring.neighbors() {
        temps.insert(n, BLISTERING_C);
    }
    let npc = creature(e, spring.clone(), spring.clone(), "kobold", COOL_NICHE);
    Scenario {
        ledger,
        registry,
        npcs: vec![npc],
        terrain: SyntheticTerrain {
            fresh: [spring].into_iter().collect(),
            temps,
            calm_after: Some((WAVE_BREAKS_DAY, AFTER_WAVE_C)),
        },
    }
}

/// **A stricken people beside a healthy one** → by-species separation. One
/// creature is stranded from known water (chronic `Frustrated`); its neighbour
/// of another species sits on its own reachable spring in a mild climate and
/// stays well. Run together, the by-species reduction attributes distress to
/// the stricken species alone.
pub fn a_stricken_and_a_healthy_people() -> Scenario {
    let (spring, exile) = water_and_a_far_exile();
    // A second, distinct spring for the healthy creature (a mesh neighbour of
    // the first — its own reachable water).
    let healthy_spring = spring.neighbors()[0].clone();
    let mut ledger = Ledger::default();
    let registry = harness_registry();

    let stricken = ledger.mint_entity();
    ledger
        .commit(
            place_agent(stricken, &spring, WorldTime { day: 0.0 }),
            &registry,
        )
        .expect("stricken at spring");
    ledger
        .commit(
            place_agent(stricken, &exile, WorldTime { day: 0.5 }),
            &registry,
        )
        .expect("stricken in exile");

    let healthy = ledger.mint_entity();
    ledger
        .commit(
            place_agent(healthy, &healthy_spring, WorldTime { day: 0.0 }),
            &registry,
        )
        .expect("healthy at spring");

    let npcs = vec![
        creature(
            stricken,
            spring.clone(),
            spring.clone(),
            "kobold",
            MILD_NICHE,
        ),
        creature(
            healthy,
            healthy_spring.clone(),
            healthy_spring.clone(),
            "goblin",
            MILD_NICHE,
        ),
    ];
    Scenario {
        ledger,
        registry,
        npcs,
        terrain: SyntheticTerrain {
            fresh: [spring, healthy_spring].into_iter().collect(),
            temps: BTreeMap::new(),
            calm_after: None,
        },
    }
}
