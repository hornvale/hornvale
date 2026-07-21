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
use hornvale_kernel::{
    ANIMAL_PREY, ConceptRegistry, EntityId, Ledger, PLANT_FORAGE, ResourceVector, RoomAddr,
    WorldTime,
};
use hornvale_species::{ActivityCycle, MetabolicClass};
use hornvale_vessel::liveness::{AGENT_AT, DRANK, EATEN, Npc, RESTED, Terrain, place_agent};
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
    /// Per-room food productivity (The Provender); rooms without an entry read
    /// `DEFAULT_FORAGE` (fed) — so a scenario that plants none feeds its
    /// creatures and hunger stays quiet, exactly as it does for the vessel
    /// tests' `PlantedTerrain`.
    forage: BTreeMap<RoomAddr, f64>,
    /// Per-room threat (The Dread); rooms without an entry read `0.0` (safe) —
    /// so a scenario that plants none is danger-free and fear stays quiet.
    threat: BTreeMap<RoomAddr, f64>,
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
    fn forage_value(&self, room: &RoomAddr) -> f64 {
        // `DEFAULT_FORAGE` (1.0) where unplanted, matching `PlantedTerrain`.
        self.forage.get(room).copied().unwrap_or(1.0)
    }
    fn threat_value(&self, room: &RoomAddr) -> f64 {
        // Safe (0.0) where unplanted, matching `PlantedTerrain`.
        self.threat.get(room).copied().unwrap_or(0.0)
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
    let _ = registry.register_predicate(RESTED, false, "an agent rested on a day");
    let _ = registry.register_predicate(EATEN, false, "an agent ate on a day");
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
        // A balanced omnivore fed by the harness terrain's default productivity
        // (The Provender), so hunger stays quiet — these scenarios probe
        // thirst/thermal distress, not starvation.
        niche: ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)])
            .expect("the omnivore niche is valid"),
        // Steady boldness (The Mettle) — the inert baseline; a scenario probing
        // the dial overrides it via struct-update (`Npc { boldness, ..creature }`).
        boldness: 0.5,
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
            forage: BTreeMap::new(),
            threat: BTreeMap::new(),
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
            forage: BTreeMap::new(),
            threat: BTreeMap::new(),
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
            forage: BTreeMap::new(),
            threat: BTreeMap::new(),
        },
    }
}

/// **A forager in a food desert** → sustained hunger distress (by-cause
/// hunger), the hunger analogue of [`stranded_from_known_water`]. The creature
/// sits ON its home spring, so thirst stays serviceable (it drinks and resets,
/// never itself distressing — the same isolation the heat-wave scenario uses);
/// but its cell and every neighbour are barren (food-value `0`), so once hunger
/// crosses its `act` there is no cell rich enough to eat and no richer
/// neighbour to forage toward: hunger — a survival drive — Holds unserviced and
/// the creature starves, distress attributed to hunger, produced end-to-end by
/// the real sim (The Provender). Proves hunger enters the drive competition and
/// the by-cause reduction separates it.
pub fn a_forager_in_a_food_desert() -> Scenario {
    let spring = RoomAddr::containing([1.0, 0.0, 0.0], 6);
    let mut ledger = Ledger::default();
    let registry = harness_registry();
    let e = ledger.mint_entity();
    // Seed the water belief and position (stands in the spring on day 0).
    ledger
        .commit(place_agent(e, &spring, WorldTime { day: 0.0 }), &registry)
        .expect("place at spring");
    // The spring and its three neighbours are all barren — no cell feeds the
    // creature and no neighbour is richer, so hunger has no affordance and it
    // Holds (a local food pit, the hunger twin of the heat-wave's thermal pit).
    let mut forage = BTreeMap::new();
    forage.insert(spring.clone(), 0.0);
    for n in spring.neighbors() {
        forage.insert(n, 0.0);
    }
    let npc = creature(e, spring.clone(), spring.clone(), "kobold", MILD_NICHE);
    Scenario {
        ledger,
        registry,
        npcs: vec![npc],
        terrain: SyntheticTerrain {
            fresh: [spring].into_iter().collect(),
            temps: BTreeMap::new(),
            calm_after: None,
            forage,
            threat: BTreeMap::new(),
        },
    }
}

/// **A creature cornered by dread** → sustained danger distress (by-cause
/// danger), the fear analogue of the food desert and heat wave (The Dread). The
/// creature sits ON its home spring, so thirst stays serviceable (it drinks and
/// resets, never itself distressing); but its cell and every neighbour are
/// maximally uncanny (threat `1.0`), so danger — a survival drive — is engaged
/// with nowhere safer to flee (a local dread-pit, the twin of the heat wave's
/// thermal pit), and the creature Holds in danger-`Frustrated`. Proves the fifth
/// drive enters the competition and the by-cause reduction separates fear.
pub fn a_creature_cornered_by_dread() -> Scenario {
    let spring = RoomAddr::containing([1.0, 0.0, 0.0], 6);
    let mut ledger = Ledger::default();
    let registry = harness_registry();
    let e = ledger.mint_entity();
    ledger
        .commit(place_agent(e, &spring, WorldTime { day: 0.0 }), &registry)
        .expect("place at spring");
    // The spring and every neighbour are maximally threatening — no safer step,
    // so danger has no affordance and the creature Holds (cornered by dread).
    let mut threat = BTreeMap::new();
    threat.insert(spring.clone(), 1.0);
    for n in spring.neighbors() {
        threat.insert(n, 1.0);
    }
    let npc = creature(e, spring.clone(), spring.clone(), "kobold", MILD_NICHE);
    Scenario {
        ledger,
        registry,
        npcs: vec![npc],
        terrain: SyntheticTerrain {
            fresh: [spring].into_iter().collect(),
            temps: BTreeMap::new(),
            calm_after: None,
            forage: BTreeMap::new(),
            threat,
        },
    }
}

/// **A steady creature and a bold one, each in an identical dread-pit** → the
/// boldness dial's behavioural effect (The Mettle). Both sit on their own spring
/// cornered by maximal threat on every side; they differ ONLY in boldness — the
/// first steady (`0.5`, feels the full dread and Holds in distress), the second
/// bold (`0.9`, so `1.0 × 2(1−0.9) = 0.2` falls below `DANGER_ACT` and it does
/// not register the pit as actionable fear at all). Run together, the steady
/// creature reads danger distress the bold one does not — the dial, end to end.
/// Returns `(steady_trace_index 0, bold_trace_index 1)` in the scenario's npcs.
pub fn dread_pit_steady_vs_bold() -> Scenario {
    let steady_spring = RoomAddr::containing([1.0, 0.0, 0.0], 6);
    // A distinct, far spring for the bold creature (its own separate dread-pit).
    let bold_spring = RoomAddr::containing([-1.0, 0.05, 0.05], 6);
    let mut ledger = Ledger::default();
    let registry = harness_registry();
    let mut threat = BTreeMap::new();
    let mut fresh = BTreeSet::new();
    let mut mint_pit = |ledger: &mut Ledger, spring: &RoomAddr| {
        let e = ledger.mint_entity();
        ledger
            .commit(place_agent(e, spring, WorldTime { day: 0.0 }), &registry)
            .expect("place at spring");
        threat.insert(spring.clone(), 1.0);
        for n in spring.neighbors() {
            threat.insert(n, 1.0);
        }
        fresh.insert(spring.clone());
        e
    };
    let steady_e = mint_pit(&mut ledger, &steady_spring);
    let bold_e = mint_pit(&mut ledger, &bold_spring);
    let steady = creature(
        steady_e,
        steady_spring.clone(),
        steady_spring,
        "kobold",
        MILD_NICHE,
    );
    let bold = Npc {
        boldness: 0.9,
        ..creature(
            bold_e,
            bold_spring.clone(),
            bold_spring,
            "kobold",
            MILD_NICHE,
        )
    };
    Scenario {
        ledger,
        registry,
        npcs: vec![steady, bold],
        terrain: SyntheticTerrain {
            fresh,
            temps: BTreeMap::new(),
            calm_after: None,
            forage: BTreeMap::new(),
            threat,
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
            forage: BTreeMap::new(),
            threat: BTreeMap::new(),
        },
    }
}

/// A stranded pair (The Tidings; decision #8). The **stricken** creature is
/// HOMED at `spring` (so its home-anchored `believed_water` genuinely holds
/// `spring` — home is *always* the closest possible candidate to itself, so
/// this belief can never be dislodged by anything else the stricken later
/// stands in) but is marooned far away at `exile`, from which `spring` is
/// unreachable — so alone it reads chronic `Frustrated` (it KNOWS water it
/// cannot reach, and structurally can never "forget" it in favor of
/// something closer at hand — see below).
///
/// `exile` is (deliberately) ALSO marked fresh water in this scenario's
/// terrain — normally that would trivially rescue a stranded creature, but it
/// doesn't here: the stricken's OWN `believed_water` ranks candidates by
/// nearness to HOME, and home (`spring`) is unbeatable at zero hops from
/// itself, so the stricken never adopts `exile` on its own, EVEN standing on
/// it. This is exactly the review's point (decision #8): home-anchored
/// belief can strand a creature on top of water it doesn't recognize.
///
/// The **knower** is a stationary AMETABOLIC informant (no drives of its own
/// — it never acts, so it never leaves wherever it's placed, and never lets
/// its own thirst cycle wander it away mid-run, which would otherwise
/// intermittently break co-location and undo the very relief being
/// measured). When `colocated`, it stands at `exile` (home = `exile` too,
/// matching its position, so even the drive-less "return home" fallback is
/// a no-op) — since `exile` is water and IS the knower's home, the knower's
/// own belief is trivially `Some(exile)`, zero hops from itself. Pooled into
/// the stricken's shared belief and ranked by the stricken's CURRENT
/// position (the decision #8 fix), `exile` — where the stricken is ALREADY
/// standing — wins outright: the stricken drinks in place, never needing to
/// move, so co-location (and therefore relief) is stable for the entire run.
/// When apart, the knower stands at a different room entirely, is never
/// co-located with the stricken, and the stricken is never relieved.
fn a_stranded_pair(colocated: bool) -> Scenario {
    let (spring, exile) = water_and_a_far_exile();
    // A room away from exile, where the apart-case knower is safely parked —
    // never co-located with the stricken, so it can share nothing.
    let away = exile.neighbors()[0].clone();
    let mut ledger = Ledger::default();
    let registry = harness_registry();

    // The stricken: homed at spring, stood there, then marooned at exile. Its
    // home-anchored belief = spring (unreachable from exile) → chronic Frustrated.
    let stricken = ledger.mint_entity();
    ledger
        .commit(
            place_agent(stricken, &spring, WorldTime { day: 0.0 }),
            &registry,
        )
        .expect("stricken once at spring");
    ledger
        .commit(
            place_agent(stricken, &exile, WorldTime { day: 0.5 }),
            &registry,
        )
        .expect("stricken marooned at exile");

    // The knower ("informant"): AMETABOLIC — no drives, so arbitrate always
    // takes its "no active drive" branch and, since its home matches its
    // stationed position, simply Holds there forever. Stationed at exile
    // (colocated) or safely away (apart); never moves either way.
    let knower = ledger.mint_entity();
    let station = if colocated {
        exile.clone()
    } else {
        away.clone()
    };
    ledger
        .commit(
            place_agent(knower, &station, WorldTime { day: 0.0 }),
            &registry,
        )
        .expect("knower stationed");

    let npcs = vec![
        creature(
            stricken,
            spring.clone(),
            spring.clone(),
            "kobold",
            MILD_NICHE,
        ),
        Npc {
            metabolic_class: MetabolicClass::Ametabolic,
            ..creature(knower, station.clone(), station, "goblin", MILD_NICHE)
        },
    ];
    Scenario {
        ledger,
        registry,
        npcs,
        terrain: SyntheticTerrain {
            fresh: [spring, exile].into_iter().collect(),
            temps: BTreeMap::new(),
            calm_after: None,
            forage: BTreeMap::new(),
            threat: BTreeMap::new(),
        },
    }
}

/// The co-located treatment (The Tidings headline result): a band whose
/// knowledgeable member heals its stranded one by circulating a reachable water.
/// Its matched null is `a_stranded_pair(false)` — the same pair standing apart.
pub fn a_band_that_shares_water() -> Scenario {
    a_stranded_pair(true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::health::health_report;

    /// Days simulated — matches `health.rs`'s private `HEALTH_TICKS` so the
    /// scenario's chronic window matches a real health sweep's.
    const HEALTH_TICKS: usize = 40;

    #[test]
    fn a_colocated_band_is_healthier_than_the_same_band_apart() {
        // Matched pair: identical stricken + knower; the ONLY difference is
        // whether the knower stands with the stricken (sharing) or apart (no
        // sharing).
        let shared = health_report(&a_stranded_pair(true).simulate(HEALTH_TICKS)); // co-located
        let apart = health_report(&a_stranded_pair(false).simulate(HEALTH_TICKS)); // separated null
        assert!(
            shared.prevalence < apart.prevalence,
            "co-located sharing heals the stricken creature: {} < {}",
            shared.prevalence,
            apart.prevalence
        );
        assert!(
            shared.chronicity <= apart.chronicity,
            "sharing never worsens chronicity"
        );
    }

    #[test]
    fn sharing_never_increases_band_distress() {
        // v1 shares only TRUE beliefs → sharing can only help: the co-located
        // band is never worse than the same band apart, on prevalence or
        // chronicity.
        let shared = health_report(&a_stranded_pair(true).simulate(HEALTH_TICKS));
        let apart = health_report(&a_stranded_pair(false).simulate(HEALTH_TICKS));
        assert!(shared.prevalence <= apart.prevalence);
        assert!(shared.chronicity <= apart.chronicity);
    }
}
