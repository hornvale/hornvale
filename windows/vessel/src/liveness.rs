//! The Quickening: the world's first autonomous motion. NPCs derived like the
//! possessed agent walk deterministic daily routes; their position over time is
//! a pure schedule (derived, reversible — the routine). This module is the
//! pure foundation only: deriving NPCs and their daily-route schedule. No
//! ledger facts are committed here and no session/tick wiring exists yet
//! (that is later Quickening work); domains are untouched (The Walk §11).

use crate::agent::{settlement_position, walk_depth};
use hornvale_kernel::{
    ANIMAL_PREY, ConditionResponse, EntityId, Fact, Ledger, PHOTOSYNTHATE, PLANT_FORAGE,
    ResourceVector, RoomAddr, RoomId, SearchSpace, TickSystem, Value, World, WorldTime, astar,
};
use hornvale_locale::LocaleContext;
use hornvale_species::{ActivityCycle, MetabolicClass};

/// A derived non-player agent: a minted entity, a home and a resource room,
/// its species, and that species' activity-cycle. Derived from the genesis
/// world, never stored (re-derivable).
/// type-audit: bare-ok(identifier-text: label), bare-ok(identifier-text: species), bare-ok(ratio: deliberation_latency), bare-ok(ratio: time_horizon), bare-ok(ratio: boldness)
#[derive(Clone, Debug)]
pub struct Npc {
    /// The NPC's minted ledger entity (subject of its future `agent-at` facts).
    pub entity: EntityId,
    /// Where the NPC rests (its home settlement's room).
    pub home: RoomAddr,
    /// The room its sustenance drive seeks (the-wanting supersedes the old
    /// fixed-schedule destination: this IS the drive's resource anchor now).
    pub resource: RoomAddr,
    /// The NPC's species (kind label), threaded from `species_of` at derivation
    /// the same way the niche and latency are — the health metric's by-species
    /// distress attribution reads it.
    pub species: String,
    /// The species activity-cycle. Write-only this slice: the drive is the sole
    /// mover (the activity gate was dropped), retained for the deferred
    /// activity-gating followup (a diurnal NPC seeking water only while awake).
    pub activity: ActivityCycle,
    /// The species' temperature niche (`ConditionNiche.temperature`): the
    /// thermal (flow) drive's setpoint and tolerance, threaded from the
    /// species' authored `biosphere_registry` at derivation the same way
    /// `activity` is (the perception/psych pattern). A per-NPC datum because
    /// co-derived NPCs may be different species with different niches.
    pub temperature_niche: ConditionResponse,
    /// The species' `PsychVector.deliberation_latency`: slides the arbitration
    /// rule between *grab* (myopic, serve the loudest need) and *weigh* (the
    /// full weighted sum) — psychology's first runtime job (spec §6). Threaded
    /// from `psyche_registry` at derivation.
    pub deliberation_latency: f64,
    /// The species' `PsychVector.time_horizon`: how far ahead the creature
    /// plans (∈ [0,1]) — psychology's SECOND runtime dial (spec §6). A
    /// foresighted creature pre-empts a projectable stock drive, engaging it
    /// before its urgency crosses `act` (see `Drive::anticipation_lead`);
    /// `0` is myopic (acts only once the need bites). Threaded from
    /// `psyche_registry` at derivation, beside `deliberation_latency`.
    pub time_horizon: f64,
    /// The species' `MetabolicClass` (The Kindling): gates which homeostatic
    /// drives the creature has and how its thirst couples to temperature. An
    /// `Ametabolic` creature (construct/undead/elemental) has no homeostatic
    /// drives at all; a metabolizing one's thirst rate couples to ambient heat
    /// per class (`rise_at`). Threaded from `biosphere_registry` at derivation,
    /// beside the niche.
    pub metabolic_class: MetabolicClass,
    /// The species' diet niche (`Taxon.niche`, a `ResourceVector` over the
    /// resource axes): the dial the hunger drive reads to decide WHAT is food
    /// (The Provender). An omnivore weights forage+prey, an autotroph
    /// photosynthate, a lithovore mineral — read as a continuous mix, never
    /// branched on a diet type. Threaded from the species' authored
    /// `biosphere_registry` at derivation, beside the metabolic class.
    pub niche: ResourceVector,
    /// The species' `PsychVector.threat_response` (flee 0 ↔ stand 1), read at
    /// CREATURE scope as its boldness (The Mettle): scales the Danger drive's
    /// felt threat — `× 2·(1 − boldness)`, centered on `0.5` (steady/inert), so
    /// a coward (`< 0.5`) fears more and a bold creature (`> 0.5`) fears less.
    /// The banked third psychology dial, threaded from `psyche_registry` at
    /// derivation like `deliberation_latency`/`time_horizon` (default `0.5` — a
    /// steady, byte-identical baseline — for a species without a psyche entry).
    pub boldness: f64,
    /// The creature's threat niche (The Bane): how much it dreads each kind of
    /// hazard, DERIVED at derivation from its temperature niche (HEAT/COLD) and
    /// metabolic class (UNCANNY) — a cold-adapted creature fears heat, an
    /// elemental does not fear the eldritch. Read by the Danger drive against the
    /// cell's hazards for per-kind fear.
    pub threat_niche: ThreatNiche,
    /// A short human label for prose ("the herder").
    pub label: String,
}

/// A game-layer predicate: an agent's room position on a day. Non-functional
/// (position changes over sim time — c5's kind-change shape); the current
/// position is the latest committed one. Registered by the possess session,
/// NOT at genesis (spec §3).
/// type-audit: bare-ok(identifier-text)
pub const AGENT_AT: &str = "agent-at";

/// The NPC's position AS OF day `t`: the latest committed `agent-at` with day
/// ≤ `t`, ELSE its home (the drive model's pre-history state — an NPC has not
/// yet sought its resource until the drive first crosses `act`). Honouring `t`
/// is byte-identical for every LIVE caller — a tick's `frozen` ledger never
/// holds facts past its own `from`, so `day ≤ t` excludes nothing and this
/// reduces to the absolute-latest position — and is exactly what lets the
/// transient-danger memory (The Phantom, §1) re-derive a PAST alarm field:
/// re-placing each emitter where it stood on the remembered day, not where it
/// stands now (a herd's panic is recovered even after the herd has moved on).
pub fn agent_position(ledger: &Ledger, npc: &Npc, t: WorldTime) -> RoomAddr {
    latest_committed_position(ledger, npc, t).unwrap_or_else(|| npc.home.clone())
}

/// The last committed `agent-at` position for `npc` with day ≤ `t`, if any.
/// Commit order is time order, so the last matching fact is the position held
/// at `t` (the whole-history case — every fact ≤ `t` — is the absolute latest).
fn latest_committed_position(ledger: &Ledger, npc: &Npc, t: WorldTime) -> Option<RoomAddr> {
    ledger
        .find(AGENT_AT)
        .filter(|f| f.subject == npc.entity)
        .filter(|f| f.day.map(|d| d <= t.day).unwrap_or(false))
        .last()
        .and_then(|f| match &f.object {
            Value::Text(s) => Some(room_from_text(s)),
            _ => None,
        })
}

/// Encode a `RoomAddr` as save-format text: the packed `RoomId` (decision
/// 0006), rendered as a decimal `u64` string. Reuses the existing pack/unpack
/// contract rather than inventing a new encoding.
fn room_to_text(r: &RoomAddr) -> String {
    r.pack()
        .expect("a scheduled room is always within MAX_DEPTH")
        .0
        .to_string()
}

/// Decode a `RoomAddr` from its packed-`RoomId` decimal text. Panics on a
/// malformed committed value — a corrupted save is a bug, not a runtime case
/// to route around.
fn room_from_text(s: &str) -> RoomAddr {
    let id: u64 = s
        .parse()
        .unwrap_or_else(|_| panic!("agent-at text '{s}' is not a decimal RoomId"));
    RoomId(id)
        .unpack()
        .unwrap_or_else(|_| panic!("agent-at RoomId {id} does not unpack to a valid RoomAddr"))
}

/// The homeostatic-drive parameters (authored constants; §4.2/§4.3): the rise
/// rate while away from the resource, the seek threshold `act`, and the
/// `sated` narration threshold. Dimensionless; the drive lives in [0, 1].
///
/// The planned (drank-fold) model's `decide`/`drive_at` consult only `rise`
/// and `act` — the plan's own goal state (`hydrated`) replaces the old
/// hysteresis leave-condition, and a `Drink` action resets the drive to 0
/// directly rather than falling gradually. `sated` survives as a SEPARATE
/// consumer: `Session::needs`'s felt-state prose (`windows/vessel/src/
/// session.rs`) still thresholds on it to say "seems content" vs "could do
/// with a drink". The old physiological `fall` rate and pre-history
/// `initial` value have no reader anywhere in the planned model and were
/// removed (The Foresight T3 review) rather than left as dead fields with
/// misleading docs.
/// type-audit: bare-ok(ratio: rise), bare-ok(ratio: act)
#[derive(Clone, Copy, Debug)]
pub struct DriveParams {
    /// Drive gained per day while away from the resource.
    pub rise: f64,
    /// The seek threshold: drive >= act -> plan to the resource and drink.
    pub act: f64,
}

/// The one authored sustenance drive (thirst/foraging); rates chosen so a cycle
/// spans a few days. (The old `sated` felt-state threshold is retired — since
/// The Temperament, `Session::needs` renders the affect read, spec §7, not a
/// bare thirst scalar.)
pub const SUSTENANCE: DriveParams = DriveParams {
    rise: 0.15,
    act: 0.85,
};

/// How long a creature's SURVIVAL drive (thirst) may go unmet — days since it
/// last drank — before it learns helplessness and gives up seeking (§7, the
/// `Helpless` scar). Set well past the act crossing (`act/rise ≈ 5.7` days) so
/// it marks a genuinely unrelievable need, not ordinary thirst: a creature that
/// reaches water on any normal errand resets `last_drank` long before this, so
/// only one truly stuck — boxed in, or seeking water that isn't there — ever
/// despairs. One authored judgment call (spec §8).
const HELPLESS_ONSET_DAYS: f64 = 15.0;

/// The helplessness PROBE period, in days: a helpless creature abandons the
/// search, but not forever — one day in every `HELPLESS_PROBE_DAYS` it tries
/// again (a flicker of renewed effort), the seam through which relief, and so
/// recovery, remains possible. This is what makes the scar "reverse slowly"
/// (the `AffectLabel::Helpless` contract) rather than trap the creature
/// permanently. One authored judgment call.
const HELPLESS_PROBE_DAYS: f64 = 5.0;

/// Whether a creature has learned helplessness at `day` — its survival drive
/// has gone unmet since `last_drank` for at least [`HELPLESS_ONSET_DAYS`], so
/// it has given up seeking. NOT a permanent trap: it returns `false` one day in
/// every [`HELPLESS_PROBE_DAYS`] (a probe — renewed effort through which relief
/// and recovery stay reachable), so the state "reverses slowly". Pure over
/// `(last_drank, day)` — a fold, exactly like the drive it reads, so
/// `affect_of` (the read) and the drive tick (the mover) compute it identically
/// and never disagree.
fn learned_helplessness(last_drank: f64, day: f64) -> bool {
    let unmet = day - last_drank;
    if unmet < HELPLESS_ONSET_DAYS {
        return false;
    }
    // Probe on the first day of each period (`since_onset` in `[0, 1)`), give up
    // the other days — so effort is reduced, never wholly abandoned.
    let since_onset = unmet - HELPLESS_ONSET_DAYS;
    (since_onset % HELPLESS_PROBE_DAYS) >= 1.0
}

/// The maximum anticipation lead-time, in days, a fully-foresighted creature
/// (`time_horizon == 1`) pre-empts a STOCK drive by: it acts as though the
/// drive had already climbed the urgency it will gain over this many days
/// (§6, `time_horizon`). One authored judgment call, kept well under the
/// thirst cycle (`act/rise ≈ 5.7` days) so even full foresight only shifts the
/// seek a couple of days early — never to a zero-urgency creature (`rise ×
/// this < act`, so the effective threshold stays positive). The goblin
/// baseline (`time_horizon == 0.5`) thus leads by one day; a myopic species
/// (`0`) leads by none, exactly the pre-anticipation model.
const ANTICIPATION_HORIZON_DAYS: f64 = 2.0;

/// A cell's per-axis HAZARD field in `[0, 1]` (The Bane) — the raw, creature-
/// INDEPENDENT presence of each kind of hazard, the sources a creature's threat
/// niche dots against. v1 carries the three axes The Dread's scalar field already
/// sourced; reserved axes (HOLY/UNHOLY, POISON, DROWNING, PSY-10's PREDATOR) are
/// the extensible future — a general `HazardVector` over a registered
/// `HazardAxis` basis (the `ResourceVector` parallel) is the reserved
/// generalization of this concrete struct.
/// type-audit: bare-ok(ratio: uncanny), bare-ok(ratio: heat), bare-ok(ratio: cold), bare-ok(ratio: predator)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Hazards {
    /// The UNCANNY — a strange/exotic/cursed place (the strangeness magnitude).
    pub uncanny: f64,
    /// HEAT — how far the cell's temperature is above the survivable band.
    pub heat: f64,
    /// COLD — how far below.
    pub cold: f64,
    /// PREDATOR — the ambient density of carnivores (The Quarry, the first
    /// BIOTIC hazard: `worldgen::predator_pressure`, injected into
    /// `LocaleTerrain`). `0` where no predators range.
    pub predator: f64,
}

impl Hazards {
    /// A safe cell — no hazard on any axis (the `Terrain::hazards` default).
    pub const ZERO: Hazards = Hazards {
        uncanny: 0.0,
        heat: 0.0,
        cold: 0.0,
        predator: 0.0,
    };
}

/// A creature's THREAT NICHE (The Bane) — how much it dreads each kind of
/// hazard, the fear twin of the diet `ResourceVector`. Derived from what the
/// creature already is: the HEAT/COLD weights from its temperature niche (a
/// creature fears the extreme away from its comfort optimum), the UNCANNY weight
/// from its metabolic class (a mortal fears the eldritch; an Ametabolic elemental
/// IS eldritch and does not). v1 weights are `≥ 0` (differential FEAR — a
/// creature can be *fearless* of a hazard, weight `0`); NEGATIVE weights (true
/// *attraction* — drawn to the hazard) are the reserved approach shore, shared
/// with The Mettle's reckless pole.
/// type-audit: bare-ok(ratio: uncanny), bare-ok(ratio: heat), bare-ok(ratio: cold), bare-ok(ratio: predator)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ThreatNiche {
    /// Dread of the UNCANNY (`1` mortal, `0` an Ametabolic elemental).
    pub uncanny: f64,
    /// Dread of HEAT (high for the cold-adapted).
    pub heat: f64,
    /// Dread of COLD (high for the heat-adapted).
    pub cold: f64,
    /// Dread of PREDATORS (The Quarry) — how much the creature fears being
    /// EATEN: `1 − carnivory` (a herbivore fears them, an obligate apex not at
    /// all — it IS one). The eater-eaten link: carnivory sets both diet and dread.
    pub predator: f64,
}

/// The felt threat of a cell FOR a creature (The Bane / The Quarry): its threat
/// niche dotted with the cell's hazards — `Σ niche·hazard` over the axes, the
/// fear twin of `food_value = diet_niche · availability`. `≥ 0` in v1 (the
/// reserved negative-weight attraction would make this go negative — the approach
/// shore).
fn threat_value(niche: &ThreatNiche, hazards: &Hazards) -> f64 {
    niche.uncanny * hazards.uncanny
        + niche.heat * hazards.heat
        + niche.cold * hazards.cold
        + niche.predator * hazards.predator
}

/// The temperature-niche optimum (°C) below which a creature weights HEAT fully
/// and the span over which the weight falls off (The Bane): a cold-adapted
/// creature (low optimum) dreads heat, a warm one shrugs it off. Authored.
const HEAT_FEAR_REF_C: f64 = 30.0;
/// The temperature-niche optimum (°C) above which a creature weights COLD fully,
/// and the reference the weight is measured from — a heat-adapted creature (high
/// optimum) dreads cold. Authored.
const COLD_FEAR_REF_C: f64 = 0.0;
/// The optimum span (°C) over which the derived HEAT/COLD threat weights slide
/// from `0` to `1`. Authored.
const THERMAL_FEAR_SPAN_C: f64 = 40.0;

/// Derive a creature's [`ThreatNiche`] from what it already is (The Bane — no
/// fresh authoring): the HEAT/COLD weights from its temperature-niche optimum (a
/// creature dreads the extreme AWAY from its comfort — cold-adapted fears heat,
/// heat-adapted fears cold), and the UNCANNY weight from its metabolic class (a
/// metabolising mortal fears the eldritch, weight `1`; an `Ametabolic` creature —
/// a construct, an elemental like the xorn — IS eldritch and does not, weight
/// `0`). v1 weights are `≥ 0` (differential fear; the reserved negative-weight
/// attraction is the approach shore).
/// The LATENT scale on the derived PREDATOR dread (The Quarry): the predator
/// field is a *latent* ambient risk (the ideonomy visibility axis — not a
/// *visible* hunting predator, which is reserved and acute), so a creature is
/// merely WARY of predator ground, not panicked. Calibrated so the current
/// peoples — bold omnivores — shrug the ambient risk off (their predator dread
/// stays below the danger `act`, keeping worlds byte-identical), while a
/// VULNERABLE creature (a timid herbivore, `1 − carnivory` near `1` and steady/
/// coward boldness) still feels it and would flee dense predator territory the
/// moment it becomes an agent — dormant-but-correct, exactly as The Bane's exotic
/// threat niches wait for their creatures. Authored.
const PREDATOR_LATENT_SCALE: f64 = 0.5;

/// The PREDATOR dread also derives from nature — from the creature's DIET niche
/// (The Quarry, the eater-eaten link): `predator_weight = (1 − carnivory) ×
/// PREDATOR_LATENT_SCALE`, where carnivory is the `ANIMAL_PREY` diet weight. A
/// herbivore fears predators (scaled), an omnivore half, an obligate apex not at
/// all (`0` — it IS one). The defendedness (mass/potency) refinement is reserved.
fn derive_threat_niche(
    temperature_niche: &ConditionResponse,
    class: MetabolicClass,
    diet_niche: &ResourceVector,
) -> ThreatNiche {
    let optimum = temperature_niche.optimum;
    ThreatNiche {
        uncanny: if matches!(class, MetabolicClass::Ametabolic) {
            0.0
        } else {
            1.0
        },
        heat: ((HEAT_FEAR_REF_C - optimum) / THERMAL_FEAR_SPAN_C).clamp(0.0, 1.0),
        cold: ((optimum - COLD_FEAR_REF_C) / THERMAL_FEAR_SPAN_C).clamp(0.0, 1.0),
        predator: ((1.0 - diet_niche.weight(ANIMAL_PREY)) * PREDATOR_LATENT_SCALE).clamp(0.0, 1.0),
    }
}

/// The elevation field and fresh-water truth the belief/exploration logic
/// reads, abstracted so pure tests plant synthetic terrain without building a
/// world. The session backs it with a `LocaleContext` (see
/// `session.rs::LocaleTerrain`).
pub trait Terrain {
    /// The room's elevation in metres (INFINITY for an undescribable room —
    /// never chosen downhill). Still the exploration prior ("water lies
    /// low" — rivers ARE the downhill drainage channels), even though water
    /// itself is no longer classified by elevation (the-surmise T5 re-wire;
    /// see `is_fresh_water`).
    /// type-audit: waiver(elevation-convention: return)
    fn elevation(&self, room: &RoomAddr) -> f64;

    /// Whether the room's water is FRESH — drinkable — rather than salt.
    /// Reads The Freshet's own classification (`WaterKind::is_fresh`), not
    /// an elevation threshold: "below sea level" is the unreachable SALT
    /// OCEAN, not water an agent can drink or ever reach (decision-ledger
    /// #9, the T4 finding that parked this campaign). `LocaleTerrain` reads
    /// this from the locale's own `water` field; planted test terrain marks
    /// specific rooms fresh directly.
    /// type-audit: bare-ok(flag: return)
    fn is_fresh_water(&self, room: &RoomAddr) -> bool;

    /// The room's PER-DAY temperature on `day`, °C — the diurnal+seasonal
    /// signal a thermal (flow) drive senses at its own cell, distinct from
    /// the render path's annual-MEAN `temperature_c` (untouched, so the
    /// possession walk and almanac stay byte-identical). `LocaleTerrain`
    /// reads the locale's per-day `temperature_at`; planted test terrain
    /// plants specific room temperatures directly. `INFINITY` for an
    /// undescribable room — its deviation from any optimum is infinite, so it
    /// is never chosen as a comfort target (mirroring `elevation`'s
    /// never-chosen-downhill convention).
    /// type-audit: waiver(temperature-convention: return)
    fn temperature(&self, room: &RoomAddr, day: WorldTime) -> f64;

    /// The sun's altitude above the horizon at `room` on `day`, in degrees
    /// (positive = up, negative = below), or `None` on a world with NO day/night
    /// cycle (tidally locked). The wake read (`is_awake`, The Slumber Tier-1)
    /// reads it. The DEFAULT is a latitude-independent fractional-day sun — the
    /// Tier-0 coarse cycle (noon peak, dawn/dusk at the horizon, midnight
    /// trough) — which the planted/synthetic test terrains inherit; a live
    /// `LocaleTerrain` OVERRIDES it with the real astronomy altitude (latitude ×
    /// season × the terminator).
    /// type-audit: waiver(altitude-convention: return)
    fn solar_altitude(&self, _room: &RoomAddr, day: WorldTime) -> Option<f64> {
        fractional_day_sun(day)
    }

    /// The cell's material food PRODUCTIVITY in `[0, 1]` (The Provender) — the
    /// standing plant/prey biomass a forager or grazer can eat there, a
    /// net-primary-productivity proxy over the climate (a slow, annual field,
    /// so it takes no `day`). The `food_value` a specific creature reads
    /// (`food_value`) dots this against the material axes of its niche
    /// (PLANT_FORAGE + ANIMAL_PREY); the PHOTOSYNTHATE (sun-fed) axis reads
    /// `solar_altitude` instead, so an autotroph's food is light, not this.
    /// The DEFAULT is `DEFAULT_FORAGE` (a generically productive cell) — so
    /// planted/synthetic test terrains feed an omnivore in place and stay
    /// undisturbed unless a scenario plants barrenness; a live `LocaleTerrain`
    /// OVERRIDES it with the real climate's NPP proxy (`productivity_at`).
    /// type-audit: bare-ok(ratio: return)
    fn forage_value(&self, _room: &RoomAddr) -> f64 {
        DEFAULT_FORAGE
    }

    /// The cell's per-axis HAZARD field (The Dread's field, split per-axis by
    /// The Bane) — the raw, creature-independent presence of each kind of hazard
    /// (uncanny / heat / cold), which a creature's threat niche dots against. The
    /// DEFAULT is [`Hazards::ZERO`] (safe) — so planted/synthetic test terrains
    /// are hazard-free and danger stays silent unless a scenario plants one; a
    /// live `LocaleTerrain` OVERRIDES it with the real climate (`hazards_at`: the
    /// uncanny strangeness plus graded heat/cold). A slow field, so it takes no
    /// `day`.
    fn hazards(&self, _room: &RoomAddr) -> Hazards {
        Hazards::ZERO
    }

    /// The cell's PREY-PRESENCE field in `[0, 1]` (The Teeth) — the standing
    /// prey-base biomass a HUNTER can eat there, the anti-symmetric dual of the
    /// predator hazard (`worldgen::prey_pressure`). A creature's `food_value`
    /// dots this against its `ANIMAL_PREY` diet weight, so a carnivore is drawn
    /// up the prey gradient. The DEFAULT is `0.0` (a prey-empty cell) — so
    /// planted/synthetic test terrains have no prey field and a carnivore reads
    /// only the ordinary productivity unless a scenario plants prey; a live
    /// `LocaleTerrain` OVERRIDES it with the injected `prey_pressure` field. A
    /// slow field, so it takes no `day`.
    /// type-audit: bare-ok(ratio: return)
    fn prey_value(&self, _room: &RoomAddr) -> f64 {
        0.0
    }
}

/// The default cell productivity (`Terrain::forage_value`) for a terrain that
/// plants none — a generically food-rich cell, so an omnivore in a
/// planted/synthetic test world (or an undescribed live cell) can always eat
/// where it stands and hunger never spuriously drives it to wander. The live
/// `LocaleTerrain` never uses this (it reads the real NPP); it exists so pure
/// tests that don't care about food are not perturbed by the hunger drive.
const DEFAULT_FORAGE: f64 = 1.0;

/// The Tier-0 coarse solar cycle — a latitude-independent fractional-day sun:
/// 90° at noon (frac 0.5), 0° at dawn/dusk (0.25 / 0.75), −90° at midnight. The
/// `Terrain::solar_altitude` default, and `LocaleTerrain`'s fallback when a
/// world carries no calendar.
fn fractional_day_sun(day: WorldTime) -> Option<f64> {
    let frac = day.day - day.day.floor();
    Some(90.0 * hornvale_kernel::math::cos(std::f64::consts::TAU * (frac - 0.5)))
}

/// Water-truth (L0): a room is water iff its terrain reports it as FRESH
/// water (The Freshet's `WaterKind::is_fresh` — rivers only, never the salt
/// ocean or a salt basin). Pure over the terrain field; rivers scatter along
/// drainage, so sources are naturally many, not one.
/// type-audit: bare-ok(flag: return)
pub fn is_water(room: &RoomAddr, terrain: &dyn Terrain) -> bool {
    terrain.is_fresh_water(room)
}

/// The single steepest-descent neighbour ("water lies low" — the prior an
/// ignorant agent explores along). `total_cmp` with an ascending-`RoomAddr`
/// tie-break (the constitutional no-native-float-cmp rule), the same rule
/// `nearest_water`'s BFS and `lowest_unvisited_neighbor` use. Always a
/// neighbour (never `from` itself).
pub fn downhill_step(from: &RoomAddr, terrain: &dyn Terrain) -> RoomAddr {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        let elev = terrain.elevation(&n);
        let keep_existing = match &best {
            Some((ba, be)) => elev.total_cmp(be).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep_existing {
            best = Some((n, elev));
        }
    }
    best.expect("a room has three neighbors").0
}

/// The true nearest water room to `from` (ground-truth-best) — a deterministic
/// breadth-first walk over the mesh to the closest `is_water` room, frontier
/// processed in `RoomAddr` order, capped at `budget` expansions (`None` if no
/// water within it). The agent does not know this until it has PERCEIVED it.
/// type-audit: bare-ok(count: budget)
pub fn nearest_water(from: &RoomAddr, terrain: &dyn Terrain, budget: usize) -> Option<RoomAddr> {
    let mut visited: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    let mut frontier: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    frontier.insert(from.clone());
    let mut expansions = 0usize;
    while let Some(room) = frontier.iter().next().cloned() {
        frontier.remove(&room);
        if !visited.insert(room.clone()) {
            continue;
        }
        if is_water(&room, terrain) {
            return Some(room);
        }
        expansions += 1;
        if expansions >= budget {
            return None;
        }
        for n in room.neighbors() {
            if !visited.contains(&n) {
                frontier.insert(n);
            }
        }
    }
    None
}

/// A `Terrain` backed by a `LocaleContext` — the elevation and fresh-water
/// fields the belief/exploration logic reads in a live session (tests use a
/// planted terrain instead). Elevation mirrors the undescribable-room
/// fallback (INFINITY); fresh water reads The Freshet's own salt/fresh
/// classification (`LocaleFields::water`, `WaterKind::is_fresh`) rather than
/// deriving a sea-level threshold — the-surmise T5 re-wire: the prior
/// elevation-threshold model classified the unreachable SALT OCEAN as
/// "water" (decision-ledger #9), never the rivers an agent can actually
/// drink from and reach.
pub struct LocaleTerrain<'a> {
    /// The locale context whose fields are read.
    pub ctx: &'a LocaleContext,
    /// The world's calendar, for the real solar wake read (The Slumber Tier-1);
    /// `None` falls back to the fractional-day sun.
    calendar: Option<&'a hornvale_astronomy::Calendar>,
    /// The world's predator-pressure field (The Quarry — `worldgen::
    /// predator_pressure`), injected here (a domain/window can't reach up to
    /// demography); `None` → no PREDATOR hazard (throwaway reads / no field).
    predator: Option<&'a hornvale_kernel::CellMap<f64>>,
    /// The world's prey-pressure field (The Teeth — `worldgen::prey_pressure`),
    /// the dual of `predator`, injected the same way; `None` → no prey draw
    /// (throwaway reads / no field), so a carnivore reads only ordinary
    /// productivity.
    prey: Option<&'a hornvale_kernel::CellMap<f64>>,
}
impl<'a> LocaleTerrain<'a> {
    /// Build the adapter over `ctx` with the fractional-day (Tier-0) sun and no
    /// predator field — for throwaway reads (water/elevation) that never consult
    /// the wake cycle or the danger drive.
    pub fn new(ctx: &'a LocaleContext) -> Self {
        Self {
            ctx,
            calendar: None,
            predator: None,
            prey: None,
        }
    }
    /// Build with the world's `calendar` (if any), so `solar_altitude` (and thus
    /// the wake cycle) follows the REAL sun — latitude × season × the terminator
    /// (Tier-1). `None` falls back to the fractional-day sun. No predator field
    /// (use [`with_calendar_and_predators`](Self::with_calendar_and_predators) for
    /// the full drive read).
    pub fn with_calendar(
        ctx: &'a LocaleContext,
        calendar: Option<&'a hornvale_astronomy::Calendar>,
    ) -> Self {
        Self {
            ctx,
            calendar,
            predator: None,
            prey: None,
        }
    }
    /// Build with the world's `calendar` AND its predator-pressure field (The
    /// Quarry) — no prey field. Retained for callers that read danger but not the
    /// hunt; delegates to [`with_fields`](Self::with_fields).
    /// type-audit: bare-ok(ratio: predator)
    pub fn with_calendar_and_predators(
        ctx: &'a LocaleContext,
        calendar: Option<&'a hornvale_astronomy::Calendar>,
        predator: Option<&'a hornvale_kernel::CellMap<f64>>,
    ) -> Self {
        Self::with_fields(ctx, calendar, predator, None)
    }
    /// Build with the world's `calendar`, predator-pressure field (The Quarry),
    /// AND prey-pressure field (The Teeth) — the full drive read, where danger
    /// senses carnivore territory and a carnivore's hunger senses prey.
    /// type-audit: bare-ok(ratio: predator), bare-ok(ratio: prey)
    pub fn with_fields(
        ctx: &'a LocaleContext,
        calendar: Option<&'a hornvale_astronomy::Calendar>,
        predator: Option<&'a hornvale_kernel::CellMap<f64>>,
        prey: Option<&'a hornvale_kernel::CellMap<f64>>,
    ) -> Self {
        Self {
            ctx,
            calendar,
            predator,
            prey,
        }
    }
}
impl<'a> Terrain for LocaleTerrain<'a> {
    fn elevation(&self, room: &RoomAddr) -> f64 {
        self.ctx
            .describe(room, WorldTime { day: 0.0 })
            .map(|l| l.fields.elevation_m)
            .unwrap_or(f64::INFINITY)
    }
    fn is_fresh_water(&self, room: &RoomAddr) -> bool {
        self.ctx
            .describe(room, WorldTime { day: 0.0 })
            .map(|l| l.fields.water.is_fresh())
            .unwrap_or(false)
    }
    fn temperature(&self, room: &RoomAddr, day: WorldTime) -> f64 {
        // The PER-DAY field (`LocaleContext::temperature_at`), NOT `describe`'s
        // annual-mean `temperature_c` — so the drive gets a diurnal/seasonal
        // swing while the render path stays byte-identical. INFINITY for an
        // undescribable room (never chosen as a comfort target).
        self.ctx.temperature_at(room, day).unwrap_or(f64::INFINITY)
    }
    fn solar_altitude(&self, room: &RoomAddr, day: WorldTime) -> Option<f64> {
        // The real sun where the world carries a calendar (latitude from the
        // room's centroid; `None` on a locked world → no cycle); else the
        // fractional-day fallback.
        match self.calendar {
            Some(cal) => hornvale_astronomy::StdDays::new(day.day)
                .ok()
                .and_then(|t| cal.solar_altitude_at(t, room.coord().latitude)),
            None => fractional_day_sun(day),
        }
    }
    fn forage_value(&self, room: &RoomAddr) -> f64 {
        // The real climate's net-primary-productivity proxy (The Provender);
        // an undescribable/above-grid room reads 0 (no food), the never-fed
        // fallback (the dual of `temperature`'s never-chosen INFINITY).
        self.ctx.productivity_at(room).unwrap_or(0.0)
    }
    fn hazards(&self, room: &RoomAddr) -> Hazards {
        // The real climate's per-axis hazard field (The Bane: the uncanny plus
        // graded heat/cold); an undescribable/above-grid room reads all-zero
        // (safe) — the never-feared fallback, the dual of `forage_value`'s 0.
        let (uncanny, heat, cold) = self.ctx.hazards_at(room).unwrap_or((0.0, 0.0, 0.0));
        // The PREDATOR axis (The Quarry): the injected carnivore-pressure field,
        // corner-blended per room; `0` where no field is injected or the room is
        // above the grid.
        let predator = self
            .predator
            .and_then(|field| self.ctx.blend_at(room, field))
            .unwrap_or(0.0);
        Hazards {
            uncanny,
            heat,
            cold,
            predator,
        }
    }
    fn prey_value(&self, room: &RoomAddr) -> f64 {
        // The PREY field (The Teeth): the injected prey-pressure field, corner-
        // blended per room (the same read as the predator axis); `0` where no
        // field is injected or the room is above the grid — the prey-empty
        // fallback, so a carnivore there reads only ordinary productivity.
        self.prey
            .and_then(|field| self.ctx.blend_at(room, field))
            .unwrap_or(0.0)
    }
}

/// A game-layer predicate: the agent drank (satisfied its sustenance goal) on
/// this day. Registered by the session, NOT at genesis.
/// type-audit: bare-ok(identifier-text)
pub const DRANK: &str = "drank";

/// Ambient temperature (°C) at or below which no heat coupling applies — an
/// endotherm's thermoneutral zone, and the reference an ectotherm's realized
/// rate is measured from (The Kindling, spec §3). One authored judgment call.
const THERMONEUTRAL_C: f64 = 25.0;

/// The temperature span (°C) over which the heat coupling reaches full strength
/// — one `HEAT_SCALE_C` above thermoneutral applies the class's full
/// multiplier. Authored.
const HEAT_SCALE_C: f64 = 20.0;

/// Endotherm heat coupling: the extra dehydration fraction at one
/// `HEAT_SCALE_C` above thermoneutral (sweating). `1.0` → thirst rises twice as
/// fast at `THERMONEUTRAL_C + HEAT_SCALE_C` (≈45 °C). Heat-only (asymmetric):
/// an endotherm thermoregulates, so cold does not slow its water need below
/// base. Authored.
const ENDOTHERM_HEAT_K: f64 = 1.0;

/// Ectotherm coupling: the realized rate TRACKS ambient (CAP-1), symmetric
/// about thermoneutral — `1.5` makes a hot ectotherm dehydrate 2.5× at ≈45 °C
/// and a cold one torpid. Stronger than the endotherm's, because a
/// cold-blooded creature's whole metabolism follows the climate. Authored.
const ECTOTHERM_K: f64 = 1.5;

/// The floor on the ectotherm rate multiplier: a torpid (deeply cold)
/// ectotherm's metabolism slows but never stops — it still needs SOME water.
/// Authored.
const ECTOTHERM_FLOOR: f64 = 0.2;

/// The per-day thirst (dehydration) RATE at ambient temperature `temp` (°C) for
/// a creature of metabolic `class` — The Kindling's coupling of heat to the
/// survival drive (spec §3). Endotherms sweat (base below thermoneutral,
/// accelerating above — heat-only); ectotherms track ambient (CAP-1's
/// principle: symmetric, floored); autotrophs are flat (a deferred seam). An
/// unreadable cell (non-finite temperature — undescribable/unplanted) couples
/// as neutral (base rate), mirroring the thermal drive's `is_finite` guard.
fn rise_at(temp: f64, class: MetabolicClass, p: &DriveParams) -> f64 {
    let base = p.rise;
    if !temp.is_finite() {
        return base;
    }
    match class {
        MetabolicClass::Endotherm => {
            let excess = (temp - THERMONEUTRAL_C).max(0.0);
            base * (1.0 + ENDOTHERM_HEAT_K * excess / HEAT_SCALE_C)
        }
        MetabolicClass::Ectotherm => {
            let factor = 1.0 + ECTOTHERM_K * (temp - THERMONEUTRAL_C) / HEAT_SCALE_C;
            base * factor.max(ECTOTHERM_FLOOR)
        }
        // Autotroph: a deferred seam (transpiration is its own later work).
        // Ametabolic: never reaches here (no thirst drive); arm kept total.
        MetabolicClass::Autotroph | MetabolicClass::Ametabolic => base,
    }
}

/// The committed `agent-at` sightings of `entity` at or before day `upto`, as
/// `(arrival_day, room)` sorted ascending — the occupancy timeline the thirst
/// integral reads.
fn agent_sightings(ledger: &Ledger, entity: EntityId, upto: f64) -> Vec<(f64, RoomAddr)> {
    let mut v: Vec<(f64, RoomAddr)> = ledger
        .find(AGENT_AT)
        .filter(|f| f.subject == entity)
        .filter_map(|f| {
            let d = f.day?;
            if d > upto {
                return None;
            }
            match &f.object {
                Value::Text(s) => Some((d, room_from_text(s))),
                _ => None,
            }
        })
        .collect();
    v.sort_by(|a, b| a.0.total_cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
    v
}

/// The thirst drive as a PATH INTEGRAL of the dehydration rate over the
/// creature's occupancy since its last drink (The Kindling, spec §3/§4): for
/// each segment during which it stood at one cell, `rise_at(temp(cell,
/// segment_start), class) × segment_length`, summed and clamped `[0, 1]`.
/// Position at any day is the latest sighting arriving at or before it, else
/// `home`; temperature is sampled once per segment at its start (so a held cell
/// couples at a fixed rate — the Hold-jump stays closed-form). DRIVE == FOLD:
/// pure over the committed occupancy + terrain, so the tick (which folds
/// `frozen + out`) and `affect_of` (which folds the final ledger) compute it
/// identically. `sightings` must be ascending and ≤ `t`.
#[allow(clippy::too_many_arguments)]
fn integrate_thirst(
    sightings: &[(f64, RoomAddr)],
    home: &RoomAddr,
    last_drank: f64,
    t: f64,
    terrain: &dyn Terrain,
    class: MetabolicClass,
    p: &DriveParams,
) -> f64 {
    if t <= last_drank {
        return 0.0;
    }
    // Segment boundaries: last_drank, each sighting arrival strictly inside
    // (last_drank, t), then t.
    let mut bounds: Vec<f64> = vec![last_drank];
    for (d, _) in sightings {
        if *d > last_drank && *d < t {
            bounds.push(*d);
        }
    }
    bounds.push(t);
    bounds.dedup();
    let mut total = 0.0_f64;
    for w in bounds.windows(2) {
        let (s, e) = (w[0], w[1]);
        // Position governing the segment starting at `s`: the latest sighting
        // arriving at or before `s`, else home.
        let pos = sightings
            .iter()
            .rev()
            .find(|(d, _)| *d <= s)
            .map(|(_, r)| r)
            .unwrap_or(home);
        let rate = rise_at(terrain.temperature(pos, WorldTime { day: s }), class, p);
        total += rate * (e - s);
    }
    total.clamp(0.0, 1.0)
}

/// The drive at `t`: the temperature-coupled thirst path integral (The
/// Kindling) over `entity`'s committed occupancy since its last drink, at its
/// metabolic `class`. Reduces to the old flat `rise × elapsed` at a
/// thermoneutral (or unreadable) climate. DRIVE == FOLD — over `drank` (the
/// reset) and `agent-at` (the occupancy).
/// type-audit: bare-ok(ratio: return)
pub fn drive_at(
    ledger: &Ledger,
    entity: EntityId,
    home: &RoomAddr,
    t: WorldTime,
    p: &DriveParams,
    terrain: &dyn Terrain,
    class: MetabolicClass,
) -> f64 {
    let last_drank = ledger
        .find(DRANK)
        .filter(|f| f.subject == entity)
        .filter_map(|f| f.day)
        .fold(0.0_f64, f64::max);
    let sightings = agent_sightings(ledger, entity, t.day);
    integrate_thirst(&sightings, home, last_drank, t.day, terrain, class, p)
}

/// Belief (L1): the agent's nearest KNOWN water — a pure fold over its committed
/// `agent-at` history ∩ water-truth. Among the water rooms the agent has stood in
/// at or before `t`, the one nearest to `npc.home` by planned hop-distance (ties
/// by ascending `RoomAddr`), else `None` (ignorant). BELIEF == FOLD-OVER-PERCEIVED:
/// no stored belief — it re-derives from facts already committed (the matrix
/// verdict; UNI-20). Nearness anchors to home (nearest-to-current is a followup).
/// type-audit: bare-ok(count: budget)
pub fn believed_water(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    budget: usize,
) -> Option<RoomAddr> {
    let mut seen: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    for f in ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity) {
        let sighted = f.day.map(|d| d <= t.day).unwrap_or(false);
        if sighted && let Value::Text(s) = &f.object {
            let room = room_from_text(s);
            if is_water(&room, terrain) {
                seen.insert(room);
            }
        }
    }
    seen.into_iter()
        .filter_map(|r| {
            plan_to_room(&npc.home, &r, budget, &std::collections::BTreeSet::new())
                .map(|p| (p.len(), r))
        })
        .min_by(|(la, ra), (lb, rb)| la.cmp(lb).then_with(|| ra.cmp(rb)))
        .map(|(_, r)| r)
}

/// A memo of the PRIMARY-AFRAID emission `(entity, day) → arousal` (`0.0` when
/// the creature's Danger drive does NOT win — no emission). The Phantom's
/// re-derivation asks "was this emitter primary-afraid on that past day?" the
/// same way for many creatures and many cells within a single tick; each such
/// verdict is an `affect_of` (a full arbitration with an A* plan), so the
/// re-derivation without a memo re-computes the SAME verdict hundreds of times.
///
/// The memo is APPEND-ONLY and never invalidated **within a single ledger
/// snapshot**: over a fixed `frozen`, `affect_of(entity, day)` is a pure
/// function, so caching it is exactly caching that function — byte-identical to
/// the un-memoized read by construction. We therefore scope one memo to each
/// tick (where the ledger is fixed): `DriveMovements::step` builds one over its
/// `frozen`, and `run_simulation` (the lab's headless sim) builds one per tick
/// for its post-tick affect reads. This collapses the dominant within-tick
/// re-derivation to O(roster × distinct-days) while keeping the verdict provably
/// identical to a fresh `affect_of` (day quantized to its bit pattern, which
/// recurs exactly across the `agent-at` days that key it).
///
/// It also caches the per-time EMITTER SCAN — which roster members could ever
/// raise an alarm and where, plus their position timelines — since that scan is
/// identical for every creature's `believed_hazard` at a given time over the
/// same fixed ledger (built once per tick instead of once per creature).
#[derive(Default)]
pub struct PrimaryAfraidMemo {
    /// `(entity, day-bits) → emitted arousal` (`0.0` = not primary-afraid).
    afraid: std::collections::BTreeMap<(EntityId, u64), f64>,
    /// `t-day-bits → the emitter scan` over the (tick-fixed) roster and ledger.
    scans: std::collections::BTreeMap<u64, EmitterScan>,
}

impl PrimaryAfraidMemo {
    /// An empty memo — one per tick (the ledger is fixed there; see the type doc).
    pub fn new() -> Self {
        Self::default()
    }
}

/// The tick-fixed scan of a roster: the members that could EVER emit an alarm
/// (with their committed position timelines) and the union of cells any of their
/// alarms could reach. Shared across every creature's re-derivation at one time.
struct EmitterScan {
    /// The ever-terrain-afraid members and their day-sorted position timelines.
    emitters: Vec<(Npc, Vec<(f64, RoomAddr)>)>,
    /// Every cell within one hop of some emitter's frightening position.
    alarm_source_cells: std::collections::BTreeSet<RoomAddr>,
}

/// Scan `roster` for the members ever on terrain frightening to them (the only
/// possible alarm emitters), building each one's day-sorted position timeline
/// (day ≤ `t`) and the union of cells their alarms could reach. Pure over
/// `(roster, ledger, terrain, t)`; cached per `t` in [`PrimaryAfraidMemo`].
fn build_emitter_scan(
    roster: &[Npc],
    ledger: &Ledger,
    terrain: &dyn Terrain,
    t: WorldTime,
) -> EmitterScan {
    let mut emitters: Vec<(Npc, Vec<(f64, RoomAddr)>)> = Vec::new();
    let mut alarm_source_cells: std::collections::BTreeSet<RoomAddr> =
        std::collections::BTreeSet::new();
    for m in roster {
        let mettle = mettle_factor(m.boldness);
        let frightening =
            |room: &RoomAddr| threat_field(room, &m.threat_niche, terrain) * mettle >= DANGER_ACT;
        let mut timeline: Vec<(f64, RoomAddr)> = ledger
            .find(AGENT_AT)
            .filter(|f| f.subject == m.entity)
            .filter_map(|f| {
                let d = f.day.filter(|d| *d <= t.day)?;
                match &f.object {
                    Value::Text(s) => Some((d, room_from_text(s))),
                    _ => None,
                }
            })
            .collect();
        // Sort by day (stable: equal days keep commit order, matching
        // `agent_position`'s last-committed-≤-day read on the monotonic timeline).
        timeline.sort_by(|a, b| a.0.total_cmp(&b.0));
        let mut ever = false;
        let mut note_halo = |p: &RoomAddr| {
            alarm_source_cells.insert(p.clone());
            for n in p.neighbors() {
                alarm_source_cells.insert(n);
            }
        };
        if frightening(&m.home) {
            ever = true;
            note_halo(&m.home);
        }
        for (_, p) in &timeline {
            if frightening(p) {
                ever = true;
                note_halo(p);
            }
        }
        if ever {
            emitters.push((m.clone(), timeline));
        }
    }
    EmitterScan {
        emitters,
        alarm_source_cells,
    }
}

/// The arousal a roster member EMITTED as an alarm on `day` — its Danger drive's
/// arousal if it was **primary-afraid** (`affect_of`'s object is Danger with
/// arousal ≥ `DANGER_ACT`), else `0.0`. Memoized per `(entity, day)` over the
/// fixed `frozen` (see [`PrimaryAfraidMemo`]); on a miss it computes the one
/// alarm-free `affect_of` that `alarm_field` and the re-derivation share. The
/// inner `affect_of` reads an EMPTY band, so its own `believed_hazard` is
/// emitter-free and never re-enters this path (the recursion break).
fn emitter_arousal(
    afraid: &mut std::collections::BTreeMap<(EntityId, u64), f64>,
    frozen: &Ledger,
    npc: &Npc,
    day: WorldTime,
    terrain: &dyn Terrain,
) -> f64 {
    let key = (npc.entity, day.day.to_bits());
    if let Some(&v) = afraid.get(&key) {
        return v;
    }
    let affect = affect_of(frozen, npc, &[], day, terrain);
    let v = if affect.object == Some(DriveKind::Danger) && affect.arousal >= DANGER_ACT {
        affect.arousal
    } else {
        0.0
    };
    afraid.insert(key, v);
    v
}

/// Belief (L1): the ground the creature has stood on that FRIGHTENS it — a pure
/// fold over its committed `agent-at` history ∩ frightening-truth, the inverted
/// twin of [`believed_water`] (a SET it plans *around*, not a target it plans
/// *toward*). Among the rooms the creature has stood in at or before `t`, those
/// whose felt threat — terrain PLUS the re-derived transient alarm over
/// `roster` at the visited day — crosses `DANGER_ACT` ([`frightened_at`]).
/// BELIEF == FOLD-OVER-PERCEIVED: no stored state — it re-derives from committed
/// facts every read, exactly as `believed_water` re-derives `is_water`. Returns
/// the EMPTY set for a creature never frightened, so the settled peoples (never
/// frightened on their good ground) carry an empty set and every planner edge
/// stays `1` — byte-identical by construction.
///
/// STALENESS — now LIVE (spec §2, The Phantom). The rule is *a cell is
/// remembered-dangerous iff the creature's MOST RECENT visit there was
/// frightened*: a later SAFE visit CLEARS the memory (experience disproving the
/// fear). The Haunt specified this but left it inert — static terrain makes
/// every visit's verdict identical, so it reduced to *visited ∧ still-
/// frightening*. The Phantom makes it bite: a cell alarm-frightened on day t₁
/// and safely revisited on t₂ > t₁ is no longer shunned. With an EMPTY
/// `roster` the re-derived alarm is 0 (terrain is time-invariant), so the rule
/// collapses back to any-visit — The Haunt's exact set, byte-identical.
///
/// THE RECURSION BREAK: the tick passes the FULL population as `roster`;
/// [`affect_of`] passes its `band`; and the transient re-derivation's own
/// primary-fear read passes `&[]` — so an empty roster re-derives no alarm and
/// never re-enters the transient path. Deterministic: the most-recent day per
/// cell accumulates into a `BTreeMap` (max day wins), the verdict is
/// order-independent, and the shunned set is yielded sorted.
///
/// # Cost — the re-derivation is cheap on the settled worlds (spec §3)
///
/// Naively re-deriving [`alarm_field`] per visited cell is ruinous (an A* plan
/// per roster member per cell). Instead we precompute, ONCE, each roster
/// member that is EVER on terrain frightening to it (the only creatures that can
/// emit) and its committed position timeline (a `partition_point` gives its
/// position at any past day). A cell's transient alarm is then the clamped sum
/// of the arousals of just those emitters whose position on that day lies within
/// the cell's one-hop halo — the SAME quantity `alarm_field` computes, but
/// evaluated only where an emitter actually stood, so an emitter-free world
/// (seed 42) pays nothing beyond the terrain fold. `affect_of` (to confirm an
/// emitter's Danger drive WINS) runs only for a terrain-afraid member standing
/// beside the very cell being judged — rare.
pub fn believed_hazard(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
) -> std::collections::BTreeSet<RoomAddr> {
    // A lone read builds its own throwaway memo (a single re-derivation gains
    // nothing from caching); the hot sim paths thread a shared one.
    let mut memo = PrimaryAfraidMemo::new();
    believed_hazard_memo(ledger, npc, t, terrain, roster, &mut memo)
}

/// [`believed_hazard`] sharing a caller-owned [`PrimaryAfraidMemo`] across the
/// many re-derivations of a single tick (the whole cost win — see the type doc).
pub fn believed_hazard_memo(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
    memo: &mut PrimaryAfraidMemo,
) -> std::collections::BTreeSet<RoomAddr> {
    // Most-recent visit per cell (day ≤ t): the cell is judged at its LATEST
    // visit, so a later safe visit clears an earlier phantom (the staleness rule).
    let mut latest: std::collections::BTreeMap<RoomAddr, f64> = std::collections::BTreeMap::new();
    for f in ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity) {
        if let Some(fday) = f.day.filter(|d| *d <= t.day)
            && let Value::Text(s) = &f.object
        {
            latest
                .entry(room_from_text(s))
                .and_modify(|d| {
                    if fday > *d {
                        *d = fday;
                    }
                })
                .or_insert(fday);
        }
    }

    // The emitter scan (which members could ever raise an alarm, their position
    // timelines, and the cells any alarm could reach) is IDENTICAL for every
    // creature's re-derivation at this time over this ledger — build it once and
    // cache it per `t` (see [`PrimaryAfraidMemo`]).
    let tbits = t.day.to_bits();
    memo.scans
        .entry(tbits)
        .or_insert_with(|| build_emitter_scan(roster, ledger, terrain, t));
    // Disjoint field borrows: the scan (read) and the affect memo (write).
    let PrimaryAfraidMemo { afraid, scans } = memo;
    let scan = &scans[&tbits];

    // The emitter's committed position AT `day`: the latest entry with day ≤ it,
    // else its home (the pre-history fallback) — `agent_position` over the
    // precomputed timeline.
    let position_at = |m: &Npc, timeline: &[(f64, RoomAddr)], day: f64| -> RoomAddr {
        let idx = timeline.partition_point(|(d, _)| *d <= day);
        if idx == 0 {
            m.home.clone()
        } else {
            timeline[idx - 1].1.clone()
        }
    };

    let mut shunned: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    if scan.emitters.is_empty() {
        // The emitter-free common case (every settled world): no transient alarm
        // is possible, so the verdict is The Haunt's terrain-only `frightened_at`
        // (the one source of truth for the formula). Terrain is time-invariant,
        // so the most-recent-visit rule collapses to any-visit — byte-identical.
        for (cell, day) in latest {
            if frightened_at(&cell, npc, terrain, WorldTime { day }, &[], ledger) {
                shunned.insert(cell);
            }
        }
        return shunned;
    }
    for (cell, day) in latest {
        let terrain_threat = threat_field(&cell, &npc.threat_niche, terrain);
        // THE TERRAIN SHORTCUT (free win): if TERRAIN alone already frightens the
        // creature here, the cell is shunned no matter what the alarm adds (the
        // alarm is additive, ≥ 0), so skip the alarm re-derivation entirely. Only
        // a terrain-BELOW-act cell can be tipped over by a remembered alarm —
        // exactly where the phantom lives. (The most-recent-visit verdict is
        // unchanged: a terrain-frightened latest visit still shuns.)
        if feels_frightening(terrain_threat, 0.0, npc.boldness) {
            shunned.insert(cell);
            continue;
        }
        // The re-derived transient alarm at (cell, day): the clamped sum of the
        // arousals of emitters primary-afraid on `day` whose position lies in the
        // cell's one-hop halo — exactly `alarm_field(day).get(cell)`. A cell
        // outside `alarm_source_cells` can receive no alarm at ANY day (no
        // emitter is ever frightening within one hop of it), so it is judged
        // terrain-only — the byte-identity pre-filter that keeps the settled
        // worlds cheap even when a distant beast occasionally treads hazard.
        let mut alarm = 0.0_f64;
        if scan.alarm_source_cells.contains(&cell) {
            let mut sources = cell.neighbors().to_vec();
            sources.push(cell.clone());
            for (m, timeline) in &scan.emitters {
                let pos = position_at(m, timeline, day);
                if !sources.contains(&pos) {
                    continue;
                }
                // Necessary condition (cheap) before the expensive confirmation.
                if threat_field(&pos, &m.threat_niche, terrain) * mettle_factor(m.boldness)
                    < DANGER_ACT
                {
                    continue;
                }
                // Confirm the emitter's Danger drive WINS (primary-afraid) via the
                // memoized, alarm-free `affect_of` — the same read `alarm_field`
                // performs, cached per `(emitter, day)` over this fixed ledger.
                alarm += emitter_arousal(afraid, ledger, m, WorldTime { day }, terrain);
            }
        }
        if feels_frightening(terrain_threat, alarm.clamp(0.0, 1.0), npc.boldness) {
            shunned.insert(cell);
        }
    }
    shunned
}

/// The BAND's water belief for `npc` (The Tidings; anchoring split per
/// decision #8). With NO co-located peer, returns `believed_water(npc)`
/// verbatim — the home-anchored nearest water it remembers — an exact no-op
/// (this is what keeps the live one-per-settlement population byte-identical).
/// With a co-located peer, pools `npc`'s and every co-located peer's
/// `believed_water` and returns the one nearest to `npc`'s CURRENT position
/// (ties: ascending `RoomAddr`), `None` if the pool is empty. Current-position
/// anchoring is the semantics of hearsay — "water near HERE" — and is what lets
/// a stranded creature adopt a here-reachable water its home-anchored memory
/// could never admit. Order-independent by construction (`BTreeSet` union +
/// deterministic `min`); no RNG. BELIEF == FOLD (UNI-20): stores nothing.
/// type-audit: bare-ok(count: budget)
pub fn shared_believed_water(
    frozen: &Ledger,
    npc: &Npc,
    band: &[Npc],
    t: WorldTime,
    terrain: &dyn Terrain,
    budget: usize,
) -> Option<RoomAddr> {
    let own = believed_water(frozen, npc, t, terrain, budget);
    let here = agent_position(frozen, npc, t);
    let mut pool: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    let mut has_peer = false;
    // Co-located OTHERS (never npc itself) contribute what they know of water.
    for other in band {
        if other.entity != npc.entity && agent_position(frozen, other, t) == here {
            has_peer = true;
            if let Some(w) = believed_water(frozen, other, t, terrain, budget) {
                pool.insert(w);
            }
        }
    }
    // ALONE: home-anchored memory, unchanged — the byte-identical no-op.
    if !has_peer {
        return own;
    }
    // CO-LOCATED: rank the pooled beliefs (npc's + peers') by nearness to npc's
    // CURRENT position (ties: ascending RoomAddr) — act on what's reachable HERE.
    if let Some(w) = own {
        pool.insert(w);
    }
    pool.into_iter()
        .filter_map(|r| {
            plan_to_room(&here, &r, budget, &std::collections::BTreeSet::new())
                .map(|p| (p.len(), r))
        })
        .min_by(|(la, ra), (lb, rb)| la.cmp(lb).then_with(|| ra.cmp(rb)))
        .map(|(_, r)| r)
}

/// What the agent perceives of the world — the `view` the decision reads. Splits
/// SELF-knowledge (position, drive — always true) from world-BELIEF
/// (`believed_water` — a cache that may be absent/ignorant) and immediate
/// perceived affordance (`explore_step`). PSY-6's "plan over belief, not truth"
/// (UNI-16), realized: the ground-truth `water` argument `decide` once took now
/// lives here as belief.
/// type-audit: bare-ok(ratio: drive), bare-ok(ratio: fatigue)
#[derive(Clone, Debug)]
pub struct Perceived {
    /// The agent's current room (self-knowledge — always true).
    pub position: RoomAddr,
    /// The agent's perceived thirst drive level (self-knowledge — always true).
    pub drive: f64,
    /// The agent's perceived fatigue level (self-knowledge — always true, The
    /// Slumber): time since it last rested, normalized `[0, 1]`.
    pub fatigue: f64,
    /// The nearest water the agent KNOWS of (belief), or `None` (ignorant).
    pub believed_water: Option<RoomAddr>,
    /// The ground the agent remembers being FRIGHTENED on (belief, The Haunt):
    /// the set of cells its planners route AROUND — the inverted twin of
    /// `believed_water`. EMPTY ⇒ today's behaviour (every planner edge stays
    /// `1`, byte-identical). Read by the planning drives (thirst/homing) as a
    /// finite route cost; the greedy drives ignore it.
    pub believed_hazard: std::collections::BTreeSet<RoomAddr>,
    /// The next exploration move for an ignorant agent (lowest-elevation
    /// unvisited neighbour), or `None` (nowhere new to look → Hold).
    pub explore_step: Option<RoomAddr>,
}

/// The decision's output — the FIRST action of the agent's current plan, or
/// Hold. The tick depends only on this; the planner fills the body without
/// changing the seam (The Wanting decision #9).
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Intent {
    /// Perform this action next (the first step of the least-cost plan).
    Do(Action),
    /// No action (goal already met and at home, or the plan is unreachable
    /// within `budget`).
    Hold,
}

/// A DRIVE: a felt need plus how to reduce it — the two halves the decision
/// policy consults. Thirst (`Thirst`) is the single implementor this stage;
/// later temperament work adds siblings (thermal comfort, …) behind the SAME
/// seam. `urgency`/`act_threshold` are the "need" half (the drive's current
/// pressure and the level at which it acts); `affordance` is the "how to
/// reduce it" half (the next executable step, or `None` when the drive cannot
/// currently be advanced). A STOCK drive (thirst) reads only the precomputed
/// `Perceived` view (self-knowledge + belief + immediate affordance) — never
/// truth — so it is pure over the view a tick already assembled. A FLOW drive
/// (`Thermal`) additionally senses the ambient field at its OWN position
/// directly (you feel the temperature of the cell you stand in), so it carries
/// the terrain and the day it senses at; that self-perception of the current
/// cell is still pure, keyed only on the drive's own held inputs.
pub trait Drive {
    /// The drive's current urgency in [0, 1] — its felt pressure, read from
    /// the (already-folded) view. Thirst returns `view.drive` (the `drive_at`
    /// fold over `DRANK`), computed upstream by the caller.
    /// type-audit: bare-ok(ratio: return)
    fn urgency(&self, view: &Perceived) -> f64;

    /// The seek threshold: at urgency ≥ this, the drive acts (plans toward its
    /// affordance); below it, the drive yields.
    /// type-audit: bare-ok(ratio: return)
    fn act_threshold(&self) -> f64;

    /// The anticipation lead this drive grants a creature with foresight
    /// `time_horizon` (∈ [0,1]): how far its `act_threshold` is lowered so the
    /// drive engages BEFORE its urgency actually crosses `act`, pre-empting a
    /// need the creature can project (§6, `time_horizon` — the second psychology
    /// dial, beside `deliberation_latency`). A STOCK drive whose urgency climbs
    /// predictably (thirst rises `rise`/day) can be projected, so foresight buys
    /// a lead proportional to that climb; a FLOW drive (thermal), whose future
    /// urgency depends on where the creature wanders and how the weather turns,
    /// has no monotonic trajectory to anticipate and grants none (the default).
    /// Zero foresight grants zero lead — the drive engages exactly at `act`,
    /// byte-identical to the pre-anticipation model.
    /// type-audit: bare-ok(ratio: _horizon), bare-ok(ratio: return)
    fn anticipation_lead(&self, _horizon: f64) -> f64 {
        0.0
    }

    /// The next executable step that reduces this drive from the view's
    /// position, or `None` when it cannot currently be advanced (its target is
    /// unreachable within `budget`, or there is nowhere new to look). For
    /// thirst: the first step of the A* plan to believed water, else the
    /// exploration step when ignorant. Equivalently, the `argmax` over
    /// candidate actions of [`serviceability`](Drive::serviceability) — the
    /// single-drive path.
    /// type-audit: bare-ok(count: budget)
    fn affordance(&self, view: &Perceived, budget: usize) -> Option<Action>;

    /// This drive's identity — for the commitment mode and the deterministic
    /// tie-break order (a fixed `DriveKind` ordering, reload-stable).
    fn kind(&self) -> DriveKind;

    /// The soft-Maslow ceiling on this drive's urgency CONTRIBUTION to the
    /// action-utility sum (§5): survival drives reach `1.0`; comfort drives
    /// cap lower, so severe cold beats mild thirst while nothing beats dying of
    /// thirst. The hierarchy *emerges* from the ranges — no priority table.
    /// type-audit: bare-ok(ratio: return)
    fn urgency_ceiling(&self) -> f64;

    /// How well `action` serves this drive from the view's position, in
    /// `[0, 1]` — the reduction in the drive's remaining cost (the action-
    /// centric arbitration term, §5). For thirst: `1.0` for the step its
    /// [`affordance`](Drive::affordance) would take (the A*/explore first step,
    /// or `Drink` at water), else `0.0`. For thermal: the drop in thermal
    /// urgency at the neighbour (`0.0` if it doesn't improve comfort, and `0.0`
    /// for `Drink` — a flow drive has no consume).
    /// type-audit: bare-ok(ratio: return), bare-ok(count: budget)
    fn serviceability(&self, action: &Action, view: &Perceived, budget: usize) -> f64;

    /// Whether this drive is pursued WHILE ASLEEP — the off-phase (The Slumber,
    /// spec §3). The default is `false`: thirst and thermal are wake-gated (a
    /// sleeping creature does not seek water or comfort). Fatigue overrides to
    /// `true` — it is the drive that carries a creature INTO sleep, so the
    /// off-phase is exactly when it engages.
    /// type-audit: bare-ok(flag: return)
    fn seek_while_asleep(&self) -> bool {
        false
    }

    /// Whether this drive at `urgency` is severe enough to OVERRIDE the wake-gate
    /// — to keep the creature seeking even while asleep (spec §3, the survival
    /// override). The default is `false`; thirst overrides once it is close
    /// enough to killing the creature that it wakes to drink. Comfort is not
    /// lethal here, so thermal never overrides.
    /// type-audit: bare-ok(ratio: _urgency), bare-ok(flag: return)
    fn survival_override(&self, _urgency: f64) -> bool {
        false
    }
}

/// A drive's identity — the second key (beside urgency) the arbitration needs:
/// it names which drive a commitment mode is pursuing and imposes a fixed,
/// reload-stable tie-break order (`Thirst` before `Thermal`). Deliberately
/// tiny and closed; new drives extend it in their own campaigns.
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DriveKind {
    /// The sustenance (thirst) stock drive.
    Thirst,
    /// The thermal-comfort flow drive.
    Thermal,
    /// The rest (fatigue) stock drive — The Slumber. Ordered LAST so the
    /// existing thirst-before-thermal tie-break is unperturbed.
    Fatigue,
    /// The hunger (sustenance) stock drive — The Provender. A SURVIVAL drive
    /// (ceiling 1.0, like thirst), ordered after fatigue so it perturbs no
    /// existing tie-break; among the two survival drives thirst wins ties
    /// (ordered first — dying of thirst outranks dying of hunger).
    Hunger,
    /// The danger (fear) FLOW drive — The Dread. A SURVIVAL drive (ceiling 1.0),
    /// ordered LAST so it perturbs no existing tie-break; a present threat still
    /// dominates naturally through urgency × serviceability, no priority table.
    Danger,
    /// The social (affiliation) FLOW drive — The Belonging. A COMFORT drive
    /// (ceiling below survival), ordered LAST so it perturbs no existing
    /// tie-break — a lonely creature yields to every survival need and to sleep.
    Social,
}

/// The per-NPC behavioural commitment mode — the errand an NPC is on
/// (spec §5). Session-sandboxed (tick-local, never save-format): it carries
/// across the steps of one walk to give hysteresis (no boundary-dithering, no
/// mid-errand flip-flop), and is re-derived, never persisted.
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    /// Not pursuing any drive, and already home — nothing to do.
    Idle,
    /// Not pursuing any drive, but away from home — walking back.
    Homing,
    /// Committed to a drive's errand (engaged at its `act`, released below
    /// `act − h`, switched only when a challenger's utility wins by `δ`).
    Pursuing(DriveKind),
}

/// A creature's felt state — a derived read of the arbitration, a point in the
/// psychological circumplex (valence × arousal, spec §7). Immaterial and never
/// committed (matching "drive == fold"): a pure function of the decision, so
/// two identical arbitrations feel identically. Carries its **intentional
/// object** (which drive the feeling is about), which — with the decision's own
/// provenance — is what makes distress debuggable and *is* the message a
/// creature emits.
/// type-audit: bare-ok(ratio: arousal), bare-ok(ratio: valence)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Affect {
    /// How activated the mind is: the greatest urgency among ACTIVE drives
    /// (0 when none is active).
    pub arousal: f64,
    /// Whether the pursued drive is reducing (+) or its affordance is failing
    /// (−): making-progress minus blocked, in `-1.0..=1.0`.
    pub valence: f64,
    /// The circumplex region `(valence, arousal)` falls in.
    pub label: AffectLabel,
    /// What the feeling is ABOUT — the pursued drive — when one is active.
    pub object: Option<DriveKind>,
}

/// The named regions of the valence × arousal circumplex (spec §7). Positive
/// affect is first-class (`Content`/`Eager`); `Searching` is neutral seeking,
/// NOT confusion (excluded from the distress metric); `Helpless` is the sticky
/// negative scar that *persistence* upgrades `Lost`/`Frustrated` into.
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AffectLabel {
    /// Positive, low arousal: needs met, puttering — the normal state.
    Content,
    /// Positive, high arousal: chasing a satisfiable need, or a drive just met.
    Eager,
    /// Neutral, mid arousal: seeking with a gradient — normal, NOT confusion.
    Searching,
    /// Negative: blocked with a KNOWN target out of reach — "want it, can't
    /// reach it" (the loud, high-arousal distress the circumplex plots top-left).
    Frustrated,
    /// Negative: blocked with no basis to move toward — "don't know what to do"
    /// (the quiet, low-arousal distress: no known target to strain against).
    Lost,
    /// Negative, persistent: given up despite an active drive — a sticky scar
    /// that reverses slowly (persistence upgrades `Lost`/`Frustrated` here).
    Helpless,
}

/// The whole outcome of one arbitration: the `Intent` to act on, the commitment
/// `Mode` to carry to the next tick, and the derived `Affect` (spec §7). Bundled
/// so callers that narrate or measure feeling get it from the same computation
/// that chose the action — no second, drift-prone derivation.
#[derive(Clone, Debug, PartialEq)]
pub struct Resolution {
    /// What to do this tick.
    pub intent: Intent,
    /// The commitment mode to carry forward (hysteresis).
    pub mode: Mode,
    /// The felt state this decision expresses.
    pub affect: Affect,
}

/// Thirst — the one authored (sustenance) drive, Drive #1. `urgency` is the
/// `drive_at` fold surfaced on the view; `affordance` is the existing
/// belief→`plan_to_water`-first-step / `explore_step` chain. Parameterized by
/// the same `DriveParams`/`SUSTENANCE` the fold uses.
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug)]
pub struct Thirst {
    /// The homeostatic parameters (rise/act) governing this drive.
    pub params: DriveParams,
}

impl Drive for Thirst {
    fn urgency(&self, view: &Perceived) -> f64 {
        view.drive
    }
    fn act_threshold(&self) -> f64 {
        self.params.act
    }
    fn anticipation_lead(&self, horizon: f64) -> f64 {
        // Thirst climbs `rise`/day, so foresight projects that climb: the lead
        // is the urgency the drive will gain over `horizon × HORIZON_DAYS` days.
        self.params.rise * horizon * ANTICIPATION_HORIZON_DAYS
    }
    fn affordance(&self, view: &Perceived, budget: usize) -> Option<Action> {
        match &view.believed_water {
            // Knows water: the first step of the A* plan toward it (None when
            // that known water is unreachable within budget).
            Some(w) => plan_to_water(&view.position, w, budget, &view.believed_hazard)
                .and_then(|pl| pl.into_iter().next()),
            // Ignorant: the exploration step (None when nowhere new to look).
            None => view.explore_step.clone().map(Action::MoveTo),
        }
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Thirst
    }
    fn urgency_ceiling(&self) -> f64 {
        // Survival: thirst can reach full urgency (nothing beats dying of it).
        1.0
    }
    fn serviceability(&self, action: &Action, view: &Perceived, budget: usize) -> f64 {
        // A stock drive serves exactly the ONE step its affordance would take
        // (the A*/explore first move, or `Drink` at water) — an indicator, so
        // the single-drive `argmax` is precisely `affordance` and the thirst-
        // only decision is byte-identical to Stage 0.
        match self.affordance(view, budget) {
            Some(a) if &a == action => 1.0,
            _ => 0.0,
        }
    }
    fn survival_override(&self, urgency: f64) -> bool {
        // Dying of thirst wakes a creature to drink (The Slumber, spec §3).
        urgency >= SURVIVAL_OVERRIDE
    }
}

/// The urgency at which the thermal comfort drive is considered to act (its
/// `act_threshold`). Comfort is a low-stakes flow drive, so the threshold sits
/// modestly above the tolerance edge (where urgency is exactly `0.0`): a cell
/// merely a touch outside the niche band is felt but not yet acted on, while a
/// genuinely uncomfortable one (urgency past this) does. An authored Stage-1
/// placeholder; Stage 2's arbitration contextualizes it against the other
/// drives (soft-Maslow ceilings).
const THERMAL_ACT: f64 = 0.5;

/// The soft-Maslow ceiling on the thermal (comfort) drive's urgency
/// contribution (§5). Comfort caps below survival's `1.0`, so a severely cold
/// creature that is only mildly thirsty seeks warmth, while a creature dying of
/// thirst (urgency → `1.0`) ignores any cold. The ordering EMERGES from the
/// ranges — there is no priority table. Authored; contextualized against
/// future drives as they land.
const THERMAL_CEIL: f64 = 0.6;

/// The commitment-mode hysteresis band: a pursued drive engages at its `act`
/// but only RELEASES once its urgency falls below `act − h`. Prevents
/// boundary-dithering at the threshold (a drive flickering active/inactive tick
/// to tick as urgency hovers at `act`).
const HYSTERESIS_H: f64 = 0.1;

/// The challenger switch margin `δ`: while pursuing one drive, the NPC only
/// abandons it for a challenger whose best-action utility exceeds the
/// incumbent's by more than this. Prevents mid-errand flip-flop between two
/// near-equal drives (the errand is sticky, not twitchy).
const SWITCH_MARGIN: f64 = 0.1;

/// Thermal comfort — a FLOW (reactive, state-satisfied) drive, a second
/// [`Drive`] implementor beside [`Thirst`]. Where thirst is a STOCK drive
/// (urgency accrues over time and is reset by a discrete `Drink`), thermal
/// comfort reads the CURRENT cell's per-day temperature against the species'
/// temperature niche every tick: discomfort is instantaneous, and stepping to
/// a more comfortable neighbour reduces it directly (no belief cache, no A* —
/// the comfort gradient step IS the affordance, like thirst's `explore_step`).
///
/// Holds the species' temperature [`ConditionResponse`] (its thermal setpoint
/// `optimum` and tolerance `width`) plus the terrain and day it senses at — a
/// flow drive perceives the ambient temperature of the cell it occupies
/// directly (see the [`Drive`] trait's stock-vs-flow note). NOT wired into the
/// live NPC `decide` this stage (Stage 1 unit-tests it in isolation);
/// arbitration of thirst + thermal together is Stage 2.
/// type-audit: bare-ok(return)
pub struct Thermal<'a> {
    /// The species' temperature niche: `optimum` is the preferred °C, `width`
    /// the tolerance half-band. Discomfort is deviation past `width` from
    /// `optimum`.
    pub niche: ConditionResponse,
    /// The temperature field this drive senses (the cell it stands in and the
    /// three neighbours it may step to).
    pub terrain: &'a dyn Terrain,
    /// The day the temperature is sensed at (the diurnal+seasonal phase).
    pub day: WorldTime,
}

impl<'a> Thermal<'a> {
    /// The absolute temperature deviation from the niche optimum at `room`,
    /// °C — the discomfort distance the drive minimizes. `INFINITY` for an
    /// undescribable room (never chosen as a comfort target).
    fn deviation(&self, room: &RoomAddr) -> f64 {
        (self.terrain.temperature(room, self.day) - self.niche.optimum).abs()
    }

    /// The thermal urgency this drive would feel standing at `room` in `[0, 1]`
    /// — how far its temperature deviates PAST the tolerance band, normalized
    /// by the band width (one further band-width reaches full urgency). Exactly
    /// `0.0` inside the band (`|temp − optimum| ≤ width`), rising outside.
    ///
    /// An UNREADABLE cell (non-finite temperature — an undescribable room, or
    /// planted-`INFINITY` test terrain) yields `0.0`: you cannot feel the
    /// temperature of a cell that reports none, so it registers no discomfort.
    /// This is exactly what keeps the thirst-only walk byte-identical — the
    /// thirst tests plant no temperatures, so their thermal drive stays
    /// inactive (urgency `0.0`) at every cell and never enters arbitration.
    /// type-audit: bare-ok(ratio: return)
    fn urgency_at(&self, room: &RoomAddr) -> f64 {
        let temp = self.terrain.temperature(room, self.day);
        if !temp.is_finite() {
            return 0.0;
        }
        let dev = (temp - self.niche.optimum).abs();
        ((dev - self.niche.width).max(0.0) / self.niche.width).clamp(0.0, 1.0)
    }
}

impl<'a> Drive for Thermal<'a> {
    fn urgency(&self, view: &Perceived) -> f64 {
        self.urgency_at(&view.position)
    }
    fn act_threshold(&self) -> f64 {
        THERMAL_ACT
    }
    fn affordance(&self, view: &Perceived, _budget: usize) -> Option<Action> {
        // Satisfied inside the tolerance band — nothing to do (a flow drive
        // needs no plan; NO A*, so `budget` is unused).
        if self.deviation(&view.position) <= self.niche.width {
            return None;
        }
        // Otherwise, the comfort gradient step: the neighbour whose temperature
        // is CLOSEST to the optimum (too-cold → warmer, too-hot → cooler, both
        // toward the optimum), or `None` when no neighbour is strictly more
        // comfortable than here (boxed in / a local comfort optimum).
        comfort_step(&view.position, self.niche.optimum, self.terrain, self.day).map(Action::MoveTo)
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Thermal
    }
    fn urgency_ceiling(&self) -> f64 {
        THERMAL_CEIL
    }
    fn serviceability(&self, action: &Action, view: &Perceived, _budget: usize) -> f64 {
        // A flow drive is served by PRESENCE in a kinder cell: the reduction in
        // thermal urgency at the neighbour it would step to (0 if the step
        // doesn't improve comfort). No consume — `Drink` serves it not at all.
        match action {
            Action::MoveTo(n) => (self.urgency_at(&view.position) - self.urgency_at(n)).max(0.0),
            // No consume — none of `Drink`/`Rest`/`Eat` serves comfort.
            Action::Drink | Action::Rest | Action::Eat => 0.0,
        }
    }
}

/// The comfort gradient step: the neighbour whose per-day temperature is
/// CLOSEST to `optimum` (minimizing `|temp − optimum|`), or `None` when no
/// neighbour is strictly more comfortable than `from` itself. A near-copy of
/// [`downhill_step`] — the same three-neighbour scan and the same
/// `total_cmp`-then-ascending-`RoomAddr` tie-break — but the objective is the
/// minimized absolute temperature deviation rather than elevation, so a
/// too-cold cell steps toward a warmer neighbour and a too-hot one toward a
/// cooler, both toward the optimum.
fn comfort_step(
    from: &RoomAddr,
    optimum: f64,
    terrain: &dyn Terrain,
    day: WorldTime,
) -> Option<RoomAddr> {
    let deviation = |room: &RoomAddr| (terrain.temperature(room, day) - optimum).abs();
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        let dev = deviation(&n);
        let keep_existing = match &best {
            Some((ba, bd)) => dev.total_cmp(bd).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep_existing {
            best = Some((n, dev));
        }
    }
    let (best_room, best_dev) = best.expect("a room has three neighbors");
    // Only step when a neighbour is STRICTLY more comfortable than here (an
    // equal-comfort or worse neighbour is no improvement — hold).
    if best_dev.total_cmp(&deviation(from)).is_lt() {
        Some(best_room)
    } else {
        None
    }
}

/// A game-layer predicate: the agent rested (slept, resetting fatigue) on this
/// day — The Slumber's discharge event, the fatigue analogue of `drank`.
/// Registered by the session, NOT at genesis.
/// type-audit: bare-ok(identifier-text)
pub const RESTED: &str = "rested";

/// The solar-altitude band (degrees around the horizon) a CREPUSCULAR creature
/// is awake in — dawn and dusk, when the sun is near the horizon (civil
/// twilight). Diurnal wakes above it, nocturnal below (The Slumber Tier-1).
const TWILIGHT_DEG: f64 = 6.0;

/// Fatigue (Process S, sleep-debt) gained per day AWAKE since the last rest (The
/// Slumber v2). Gentle: it stays low under normal nightly sleep (Process C, the
/// wake-gate, drives the daily rest) and only crosses `FATIGUE_ACT` after days
/// of PREVENTED sleep — the exhaustion backstop. Authored.
const FATIGUE_RISE: f64 = 0.3;
/// The fatigue seek threshold: at/above this, the creature seeks rest. Mirrors
/// thirst's `act`.
const FATIGUE_ACT: f64 = 0.85;
/// The soft-Maslow ceiling on fatigue's urgency contribution — below survival
/// (like thermal comfort), so a creature dying of thirst does not sleep through
/// it, but a mildly thirsty tired one rests. Authored.
const FATIGUE_CEIL: f64 = 0.6;

/// The thirst urgency past which the wake-gate is OVERRIDDEN — a creature this
/// close to dying of thirst WAKES to drink (spec §3). Authored.
const SURVIVAL_OVERRIDE: f64 = 0.9;

/// Whether a creature of `activity` is awake at `day` — a pure function of its
/// `ActivityCycle` and the time of day (the fractional part of `day`; The
/// Slumber, spec §1). Diurnal is awake through the day window, nocturnal the
/// complement, crepuscular the twilight edges. A fractional-day approximation
/// The resolution at which the tick scans for the next wake transition (days).
/// Fine enough to catch a crepuscular creature's narrow dawn/dusk bands.
const WAKE_SCAN_STEP: f64 = 0.05;

/// A representative AWAKE fraction of the day for `activity` — where the health
/// metric samples a creature's felt state (The Slumber). Sampling at midnight
/// (`frac 0`) would find a diurnal creature asleep and miss its waking distress;
/// a sleeping creature is not distressed, so the metric must read it while it is
/// up. Midday for diurnal, deep night for nocturnal, dawn for crepuscular — each
/// verified awake by `is_awake`.
/// type-audit: bare-ok(ratio: return)
pub fn waking_offset(activity: ActivityCycle) -> f64 {
    // A representative moment EARLY in the active phase, deliberately BEFORE the
    // diurnal thermal peak (mid-afternoon), so the metric reads a creature's
    // typical waking condition rather than the noon heat spike — thirst distress
    // is time-of-day-independent, but thermal peaks midday, and a brief midday
    // heat is not chronic distress.
    match activity {
        ActivityCycle::Diurnal => 0.35,     // mid-morning
        ActivityCycle::Nocturnal => 0.9,    // deep night (coolest)
        ActivityCycle::Crepuscular => 0.25, // dawn
    }
}

/// The next day after `day` at which a creature of `activity` wakes — so a
/// sleeping creature JUMPS through its off-phase in one `Rest` rather than
/// spinning (The Slumber, spec §4). A bounded scan (at most ~1.5 days, one full
/// cycle plus margin) at [`WAKE_SCAN_STEP`]; deterministic (compute-path only).
fn next_awake_day(
    activity: ActivityCycle,
    terrain: &dyn Terrain,
    room: &RoomAddr,
    day: f64,
) -> f64 {
    let limit = day + 1.5;
    let mut t = day + WAKE_SCAN_STEP;
    while t < limit {
        if is_awake(activity, terrain, room, WorldTime { day: t }) {
            return t;
        }
        t += WAKE_SCAN_STEP;
    }
    // No waking within a cycle (e.g. polar night for a diurnal creature): sleep
    // on to the next day; the survival override still wakes a dying creature.
    day + 1.0
}

/// (true solar altitude is deferred).
fn is_awake(
    activity: ActivityCycle,
    terrain: &dyn Terrain,
    room: &RoomAddr,
    day: WorldTime,
) -> bool {
    match terrain.solar_altitude(room, day) {
        // No day/night cycle (a tidally locked world): the solar zeitgeber is
        // absent, so the wake-gate cannot fire — the creature is effectively
        // always awake and rests on fatigue alone (spec §1, the locked branch).
        None => true,
        Some(alt) => match activity {
            ActivityCycle::Diurnal => alt > 0.0,
            ActivityCycle::Nocturnal => alt < 0.0,
            ActivityCycle::Crepuscular => alt.abs() < TWILIGHT_DEG,
        },
    }
}

/// The fatigue at `t`: time since the last rest, a fold over committed `rested`
/// events (0 before any rest), clamped `[0, 1]` (The Slumber). FATIGUE == FOLD,
/// over `rested` — the structural twin of thirst's `drive_at` over `drank`.
/// type-audit: bare-ok(ratio: return)
pub fn fatigue_at(ledger: &Ledger, entity: EntityId, t: WorldTime) -> f64 {
    let last_rested = ledger
        .find(RESTED)
        .filter(|f| f.subject == entity)
        .filter_map(|f| f.day)
        .fold(0.0_f64, f64::max);
    (FATIGUE_RISE * (t.day - last_rested)).clamp(0.0, 1.0)
}

/// The rest (fatigue) drive, Drive #3 (The Slumber). A STOCK drive like thirst:
/// urgency accrues over time and is reset by a discrete `Rest`. A creature
/// sleeps **where it is** — its affordance is always `Rest` — so an explorer
/// beds down in the field at nightfall rather than trekking home, and a creature
/// stranded from home can still rest (it is never *fatigue*-blocked). `home` is
/// retained as a reserved hook for a future rest-QUALITY refinement (a safe,
/// familiar den restoring more than an exposed camp).
/// type-audit: bare-ok(return)
pub struct Fatigue {
    /// The creature's home — reserved for a future rest-quality refinement
    /// (unused by the affordance today: rest is in place).
    pub home: RoomAddr,
}

impl Drive for Fatigue {
    fn urgency(&self, view: &Perceived) -> f64 {
        view.fatigue
    }
    fn act_threshold(&self) -> f64 {
        FATIGUE_ACT
    }
    fn affordance(&self, _view: &Perceived, _budget: usize) -> Option<Action> {
        // Sleep where you are — rest is always available (The Slumber v2).
        Some(Action::Rest)
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Fatigue
    }
    fn urgency_ceiling(&self) -> f64 {
        FATIGUE_CEIL
    }
    fn serviceability(&self, action: &Action, _view: &Perceived, _budget: usize) -> f64 {
        // Served by resting in place; nothing else eases fatigue.
        match action {
            Action::Rest => 1.0,
            _ => 0.0,
        }
    }
    fn seek_while_asleep(&self) -> bool {
        // Fatigue carries the creature INTO sleep: the off-phase is when it
        // engages, not when it yields (The Slumber, spec §3).
        true
    }
}

/// A game-layer predicate: the agent ate (satisfied its hunger goal) on this
/// day — The Provender's discharge event, the hunger analogue of `drank`.
/// Registered by the session, NOT at genesis.
/// type-audit: bare-ok(identifier-text)
pub const EATEN: &str = "eaten";

/// The per-day hunger (metabolic burn) base RATE — The Provender. Slower than
/// thirst's `SUSTENANCE.rise` (0.15): a creature outlasts hunger longer than
/// thirst, so at base this is a ~8.5-day starvation cycle (`act/rise`). Like
/// thirst it couples to metabolism and cell temperature through the SAME
/// `rise_at`/path-integral machinery (The Kindling, a second consumer), so a
/// hot endotherm burns — and hungers — faster. Authored.
const HUNGER: DriveParams = DriveParams {
    rise: 0.1,
    act: 0.85,
};

/// The food-value at/above which a creature can EAT where it stands (The
/// Provender). Below it a cell is too barren to feed on and the creature must
/// forage toward a richer neighbour. Low, so any ordinarily productive cell
/// (an inhabited settlement's surroundings) feeds; only genuine barrens
/// (desert/ice, a planted wasteland) starve. Authored.
const EAT_THRESHOLD: f64 = 0.15;

/// The scale of the prey-presence term in [`food_value`] (The Teeth) — how
/// strongly a carnivore is drawn up the `prey_pressure` gradient, per unit of
/// `ANIMAL_PREY` diet weight. The prey term is ADDITIVE (it only raises
/// `food_value`), so a creature that already eats where it stands keeps doing so
/// — the current settled peoples are byte-identical regardless of this value
/// (they never forage; The Confluence sat them on productive ground). It bites
/// only for a creature that must FORAGE on prey-sparse ground: a wild carnivore
/// beast (`ANIMAL_PREY`-dominant) on barren wild land, drawn toward the herds.
/// Sized so that draw is real without swamping the ordinary productivity term.
/// Authored; the woken-hunt analog of The Quarry's `PREDATOR_LATENT_SCALE`.
const PREY_LATENT_SCALE: f64 = 1.0;

/// The food-value of a cell FOR a specific creature (The Provender, spec §1):
/// its niche dotted with the cell's resource availability. The MATERIAL axes
/// (plant forage + animal prey) read the cell's productivity
/// ([`Terrain::forage_value`], an NPP proxy); the PHOTOSYNTHATE axis reads
/// LIGHT (the sun above the horizon — an autotroph is fed by day, starved at
/// night; the wake-gated autotroph seam); DETRITUS/MINERAL are reserved (no
/// availability modelled yet, so they contribute nothing). Reading the niche
/// as a continuous mix is the whole design — no hardcoded "herbivore vs
/// carnivore" branch (spec §0). A locked world (no solar cycle) counts as lit
/// for the sun-fed (its permanently-lit hemisphere); no autotroph is an agent
/// yet, so this is a reserved seam either way.
fn food_value(
    niche: &ResourceVector,
    terrain: &dyn Terrain,
    room: &RoomAddr,
    day: WorldTime,
) -> f64 {
    let productivity = terrain.forage_value(room);
    let material = niche.weight(PLANT_FORAGE) + niche.weight(ANIMAL_PREY);
    let light = match terrain.solar_altitude(room, day) {
        Some(alt) => {
            if alt > 0.0 {
                1.0
            } else {
                0.0
            }
        }
        None => 1.0,
    };
    // The Teeth: the ANIMAL_PREY axis also reads the PREY field — a carnivore's
    // meat is other creatures, not the biome, so it is drawn up the prey gradient.
    // ADDITIVE (food_value only rises) so an eat-in-place creature is unchanged;
    // it wakes a foraging wild carnivore. `prey_value` defaults 0.0 (no prey
    // field ⇒ pre-Teeth behaviour exactly).
    let prey_draw = niche.weight(ANIMAL_PREY) * PREY_LATENT_SCALE * terrain.prey_value(room);
    material * productivity + niche.weight(PHOTOSYNTHATE) * light + prey_draw
}

/// The forage gradient step: the neighbour whose [`food_value`] is HIGHEST
/// (for this niche), or `None` when no neighbour is strictly richer than
/// `from` itself (boxed in / a local food optimum — the creature holds). The
/// hunger analogue of [`comfort_step`], maximizing food rather than minimizing
/// thermal deviation; same three-neighbour scan and the same
/// `total_cmp`-then-ascending-`RoomAddr` tie-break.
fn forage_step(
    from: &RoomAddr,
    niche: &ResourceVector,
    terrain: &dyn Terrain,
    day: WorldTime,
) -> Option<RoomAddr> {
    let value = |room: &RoomAddr| food_value(niche, terrain, room, day);
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        let v = value(&n);
        let take = match &best {
            // Higher food wins; ties break to the smaller RoomAddr (replace the
            // incumbent only when the candidate is strictly richer, or equal but
            // a smaller address).
            Some((ba, bv)) => v.total_cmp(bv).then_with(|| ba.cmp(&n)).is_gt(),
            None => true,
        };
        if take {
            best = Some((n, v));
        }
    }
    let (best_room, best_v) = best.expect("a room has three neighbors");
    // Only step when a neighbour is STRICTLY richer than here.
    if best_v.total_cmp(&value(from)).is_gt() {
        Some(best_room)
    } else {
        None
    }
}

/// The hunger at `t`: the temperature-coupled metabolic-burn path integral (The
/// Kindling machinery, reused) over `entity`'s committed occupancy since its
/// last meal, at its metabolic `class` — the structural twin of thirst's
/// [`drive_at`], folding `eaten` (the reset) and `agent-at` (the occupancy)
/// with the `HUNGER` params. HUNGER == FOLD, so the tick and `affect_of`
/// compute it identically.
/// type-audit: bare-ok(ratio: return)
pub fn hunger_at(
    ledger: &Ledger,
    entity: EntityId,
    home: &RoomAddr,
    t: WorldTime,
    terrain: &dyn Terrain,
    class: MetabolicClass,
) -> f64 {
    let last_ate = ledger
        .find(EATEN)
        .filter(|f| f.subject == entity)
        .filter_map(|f| f.day)
        .fold(0.0_f64, f64::max);
    let sightings = agent_sightings(ledger, entity, t.day);
    integrate_thirst(&sightings, home, last_ate, t.day, terrain, class, &HUNGER)
}

/// Hunger — the fourth drive (The Provender): a STOCK drive like thirst, but
/// niche-relative and spatially graded. Urgency accrues as the creature burns
/// (the `hunger_at` fold, held here rather than surfaced on the shared
/// `Perceived` view — like [`Thermal`], hunger reads inputs it carries: the
/// pre-folded urgency, the diet niche, and the food field it senses). Its
/// affordance is to EAT where the cell's [`food_value`] clears
/// [`EAT_THRESHOLD`], else to climb the food gradient toward a richer cell
/// ([`forage_step`]). Its ceiling is SURVIVAL (starving is lethal, like
/// thirst, unlike comfort/fatigue). Reads the niche as a continuous mix — no
/// hardcoded diet branch (spec §0).
/// type-audit: bare-ok(ratio: urgency)
pub struct Hunger<'a> {
    /// The pre-folded hunger urgency (`hunger_at`) in `[0, 1]` — the felt
    /// pressure, computed by the caller and carried here (see the struct doc
    /// for why it is not on the `Perceived` view).
    pub urgency: f64,
    /// The species' diet niche (the `ResourceVector` over resource axes) — the
    /// dial that decides WHAT is food (forage/prey/light/…); read as a
    /// continuous mix, never branched on a diet type.
    pub niche: ResourceVector,
    /// The food field this drive senses (the cell it stands in and the three
    /// neighbours it may step to) — like [`Thermal`]'s terrain.
    pub terrain: &'a dyn Terrain,
    /// The day the food is sensed at (for the sun-fed autotroph seam's light).
    pub day: WorldTime,
}

impl<'a> Hunger<'a> {
    /// The food-value at `room` for this creature's niche — the drive's own
    /// perception of a cell.
    fn food_value_at(&self, room: &RoomAddr) -> f64 {
        food_value(&self.niche, self.terrain, room, self.day)
    }
}

impl<'a> Drive for Hunger<'a> {
    fn urgency(&self, _view: &Perceived) -> f64 {
        self.urgency
    }
    fn act_threshold(&self) -> f64 {
        HUNGER.act
    }
    fn anticipation_lead(&self, horizon: f64) -> f64 {
        // A stock drive that climbs `rise`/day (at base), so foresight projects
        // that climb exactly as thirst's does (§6).
        HUNGER.rise * horizon * ANTICIPATION_HORIZON_DAYS
    }
    fn affordance(&self, view: &Perceived, _budget: usize) -> Option<Action> {
        // Eat in place where the cell is rich enough; else forage toward a
        // richer neighbour (None when boxed in / everywhere barren → the
        // creature holds, reading distress if hungry).
        if self.food_value_at(&view.position) >= EAT_THRESHOLD {
            Some(Action::Eat)
        } else {
            forage_step(&view.position, &self.niche, self.terrain, self.day).map(Action::MoveTo)
        }
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Hunger
    }
    fn urgency_ceiling(&self) -> f64 {
        // Survival: starving reaches full urgency (like thirst).
        1.0
    }
    fn serviceability(&self, action: &Action, view: &Perceived, budget: usize) -> f64 {
        // A stock drive serves exactly the ONE step its affordance would take
        // (Eat here, or the forage step) — an indicator, so the single-drive
        // argmax is precisely `affordance` (mirrors thirst).
        match self.affordance(view, budget) {
            Some(a) if &a == action => 1.0,
            _ => 0.0,
        }
    }
    fn survival_override(&self, urgency: f64) -> bool {
        // Starving wakes a creature to forage (mirrors thirst; The Slumber).
        urgency >= SURVIVAL_OVERRIDE
    }
}

/// The urgency at/above which a present threat WAKES a sleeping creature (The
/// Dread) — a hazard this close overrides the wake-gate, like dying of thirst.
/// Authored, matching thirst's [`SURVIVAL_OVERRIDE`] posture.
const DANGER_OVERRIDE: f64 = 0.5;

/// The threat seek threshold: at/above this the danger drive engages (flees).
/// Lower than the sustenance drives' `act` (0.85) — fear is reactive and
/// prompt, so even a moderate threat is felt and acted on, not endured. One
/// authored judgment call.
const DANGER_ACT: f64 = 0.3;

/// The LATENT scale on BORROWED alarm (The Alarm) — the fear-contagion twin of
/// [`PREDATOR_LATENT_SCALE`] / `PREY_LATENT_SCALE`: how much of a neighbour's
/// distress a creature adds to its own felt threat before the boldness scaling.
/// The additive-latent discipline — the term only ever RAISES felt threat, so a
/// creature below `DANGER_ACT` with no primary-afraid neighbours is byte-
/// identical by construction. Authored; `1.0` (the alarm field is already the
/// emitter's felt-threat magnitude, clamped `[0, 1]`, so a full-strength alarm
/// reads as a full-strength threat). Byte-identity is STRUCTURAL, not scale-
/// tuned: the settled peoples never reach primary danger distress, so the field
/// is empty on seed 42 regardless of scale.
const ALARM_SCALE: f64 = 1.0;

/// Danger — the fifth drive (The Dread), the avoidance twin of hunger: a FLOW
/// drive (like [`Thermal`]) that senses the threat at the cell it occupies and
/// FLEES down the threat gradient. Where hunger climbs *toward* a resource,
/// danger flees *from* a hazard; where thermal minimizes temperature deviation,
/// danger minimizes threat. It carries no internal stock and no discharge event
/// (fear is not "reset" — it lifts when the threat is gone), so it commits no
/// fact and adds no `Action` (fleeing is a plain `MoveTo`). Its ceiling is
/// SURVIVAL (a lethal hazard outranks comfort), and a present threat wakes a
/// sleeping creature. Its serviceability is SIGNED (unclamped) — a step into
/// worse danger scores NEGATIVE, so danger reshapes the other drives' paths
/// (a thirsty creature routes around a hazard). Its felt threat is the
/// creature's THREAT NICHE dotted with the cell's hazards (The Bane — per-kind
/// fear, so two species flee different cells), then scaled by its `boldness`
/// (The Mettle) — a bold creature fears less, so its weaker veto lets it cross
/// ground a timid one flees.
/// type-audit: bare-ok(ratio: boldness), bare-ok(ratio: alarm)
pub struct Danger<'a> {
    /// The hazard field this drive senses (the cell it stands in and the three
    /// neighbours it may flee to) — like [`Thermal`]'s terrain.
    pub terrain: &'a dyn Terrain,
    /// The creature's threat niche (The Bane): how much it dreads each kind of
    /// hazard, dotted with the cell's [`Hazards`] to give the felt threat.
    pub threat_niche: ThreatNiche,
    /// The creature's boldness (the banked `threat_response` at creature scope,
    /// The Mettle): scales the felt threat by `2·(1 − boldness)`, centered on
    /// `0.5` (steady/inert). Below `0.5` a coward fears more; above, a bold
    /// creature fears less; toward `1` it is fearless.
    pub boldness: f64,
    /// The per-tick ALARM field (The Alarm): borrowed distress from nearby
    /// primary-afraid creatures, keyed by cell. Read at the creature's OWN cell
    /// only (the field build already spread each emitter's alarm to its
    /// neighbours, so reading neighbours again would double-count) and folded
    /// ADDITIVELY into the felt threat, scaled by [`ALARM_SCALE`]. `None` ⇒ no
    /// contagion — the current (pre-Alarm) behaviour, byte-identical.
    pub alarm: Option<&'a std::collections::BTreeMap<RoomAddr, f64>>,
}

/// The boldness at which fear is felt AS IS (unscaled) — the steady baseline the
/// dial is centered on (The Mettle). `PsychVector.threat_response` uses `0.5` as
/// its flee/stand midpoint, and the goblin (and every psyche-less beast) sits
/// here, so this baseline keeps them byte-identical.
const BOLDNESS_STEADY: f64 = 0.5;

/// The boldness scaling factor `2·(1 − boldness)` — `×2` at coward `0`, `×1`
/// at steady `0.5`, `×0` at fearless `1`. Floored at `0` so v1 never inverts to
/// the reserved reckless/approach shore. The single source of the Mettle dial:
/// the Danger drive and [`believed_hazard`] both scale felt threat by it, so
/// they never disagree about how much a creature feels a hazard.
/// type-audit: bare-ok(ratio: boldness), bare-ok(ratio: return)
fn mettle_factor(boldness: f64) -> f64 {
    (2.0 * (1.0 - boldness)).max(0.0)
}

/// The terrain-sourced felt threat over `room` and its neighbours (the
/// potential-field reading the Danger drive engages on — the greatest over the
/// cell it stands in and the three it may flee to of the per-kind
/// [`threat_value`], boldness applied separately). The alarm-free terrain half
/// of the drive's urgency, factored out so the live drive and
/// [`believed_hazard`]'s memory read the SAME danger — one source of truth.
/// type-audit: bare-ok(return)
fn threat_field(room: &RoomAddr, niche: &ThreatNiche, terrain: &dyn Terrain) -> f64 {
    let here = threat_value(niche, &terrain.hazards(room));
    room.neighbors()
        .iter()
        .map(|n| threat_value(niche, &terrain.hazards(n)))
        .fold(here, f64::max)
}

/// The re-derived ALARM at `(room, day)` — the transient halo of whichever
/// creatures in `roster` were **primary-afraid** on that past day,
/// reconstructed from their committed positions (The Phantom, §1). Reuses
/// [`alarm_field`] VERBATIM (one source of truth with the live drive) and reads
/// its value at `room` (`0.0` if absent). The alarm is never committed — it is
/// re-derived, exactly as `believed_water` re-derives `is_water`.
///
/// THE RECURSION BREAK (structural, load-bearing). An EMPTY `roster`
/// short-circuits to `0.0` immediately. This is BOTH the seed-42 fast path AND
/// the base case that terminates the memory's re-derivation: `alarm_field`'s
/// internal `affect_of` passes an EMPTY band, which threads through
/// `believed_hazard` → `frightened_at` → here as an empty roster, so the field
/// build sees a terrain-only replay and never re-enters the transient path.
fn alarm_at(
    room: &RoomAddr,
    day: WorldTime,
    roster: &[Npc],
    terrain: &dyn Terrain,
    frozen: &Ledger,
) -> f64 {
    if roster.is_empty() {
        return 0.0;
    }
    alarm_field(frozen, roster, terrain, day)
        .get(room)
        .copied()
        .unwrap_or(0.0)
}

/// Whether the creature is FRIGHTENED at `room` on `day` — its felt threat
/// there (terrain PLUS the re-derived transient alarm) crosses `DANGER_ACT`,
/// exactly the Danger drive's own reading: `(threat_field + ALARM_SCALE·alarm)
/// × mettle_factor ≥ act`. The one source of truth [`believed_hazard`] folds
/// over, so the memory and the live drive never disagree about what ground is
/// frightening. The Phantom (§1): the alarm term is RE-DERIVED at the
/// remembered `day` from the frozen ledger — the danger a herd's panic left,
/// recovered long after the alarm itself has died. An EMPTY `roster` collapses
/// this to The Haunt's terrain-only verdict (the recursion base case / the
/// seed-42 path, where no primary-afraid emitter ever raises an alarm).
fn frightened_at(
    room: &RoomAddr,
    npc: &Npc,
    terrain: &dyn Terrain,
    day: WorldTime,
    roster: &[Npc],
    frozen: &Ledger,
) -> bool {
    feels_frightening(
        threat_field(room, &npc.threat_niche, terrain),
        alarm_at(room, day, roster, terrain, frozen),
        npc.boldness,
    )
}

/// The felt-threat verdict — `(terrain_threat + ALARM_SCALE·alarm) ×
/// mettle_factor ≥ DANGER_ACT`, clamped. The ONE formula [`frightened_at`] and
/// [`believed_hazard`]'s fast path share, so the memory and the live Danger
/// drive never disagree about what ground is frightening. `alarm` is the already
/// clamped alarm-field value at the cell (`0.0` for terrain-only).
/// type-audit: bare-ok(ratio: terrain_threat), bare-ok(ratio: alarm), bare-ok(ratio: boldness)
fn feels_frightening(terrain_threat: f64, alarm: f64, boldness: f64) -> bool {
    ((terrain_threat + ALARM_SCALE * alarm) * mettle_factor(boldness)).clamp(0.0, 1.0) >= DANGER_ACT
}

impl<'a> Danger<'a> {
    /// The creature's OWN felt threat at `room` (The Bane): its threat niche
    /// dotted with the cell's hazards. Per-kind — two species read the same cell
    /// differently. (Boldness is applied separately, in `urgency`.)
    fn threat_at(&self, room: &RoomAddr) -> f64 {
        threat_value(&self.threat_niche, &self.terrain.hazards(room))
    }
}

impl<'a> Drive for Danger<'a> {
    fn urgency(&self, view: &Perceived) -> f64 {
        // Fear is ANTICIPATORY: a creature dreads the dangerous ground it is on
        // AND the dangerous ground within one step (the potential-field reading —
        // the drive must be ACTIVE while adjacent to a hazard for its signed
        // serviceability to veto a step INTO it). So the base threat is the
        // greatest over the current cell and its neighbours; the creature's
        // boldness (The Mettle) then scales how much it FEELS it. Clamped [0, 1].
        let base = threat_field(&view.position, &self.threat_niche, self.terrain);
        // THE ALARM: fold the borrowed distress at the creature's OWN cell into
        // the felt threat, ADDITIVELY and BEFORE the boldness scaling — so a calm
        // creature beside genuine distress feels it, scaled by its own mettle,
        // exactly as it feels a terrain hazard. `None` (or a cell absent from the
        // sparse field) contributes `0.0`, keeping the current worlds byte-
        // identical. Read at `position` only: the field build already haloed the
        // alarm to the neighbours.
        let borrowed = self
            .alarm
            .and_then(|field| field.get(&view.position))
            .copied()
            .unwrap_or(0.0);
        let felt = base + ALARM_SCALE * borrowed;
        (felt * mettle_factor(self.boldness)).clamp(0.0, 1.0)
    }
    fn act_threshold(&self) -> f64 {
        DANGER_ACT
    }
    fn affordance(&self, view: &Perceived, _budget: usize) -> Option<Action> {
        // Flee: step to the safest neighbour (by THIS creature's threat niche),
        // or `None` when boxed in by threat on every side (cornered → Frustrated).
        // A flow drive needs no plan (no A*), so `budget` is unused.
        flee_step(&view.position, self.terrain, &self.threat_niche).map(Action::MoveTo)
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Danger
    }
    fn urgency_ceiling(&self) -> f64 {
        // Survival: a lethal hazard reaches full urgency (like thirst/hunger).
        1.0
    }
    fn serviceability(&self, action: &Action, view: &Perceived, _budget: usize) -> f64 {
        // SIGNED (unclamped, unlike thermal): the DROP in the creature's own felt
        // threat at the neighbour it would step to — positive toward safety,
        // NEGATIVE into worse danger, so a move that serves another drive but
        // raises threat is penalised and the arbitration routes around the hazard.
        // No consume — Drink/Rest/Eat do not ease fear.
        match action {
            Action::MoveTo(n) => self.threat_at(&view.position) - self.threat_at(n),
            Action::Drink | Action::Rest | Action::Eat => 0.0,
        }
    }
    fn survival_override(&self, urgency: f64) -> bool {
        // A present threat wakes a sleeping creature (The Slumber's override).
        urgency >= DANGER_OVERRIDE
    }
}

/// The flee gradient step: the neighbour of LOWEST felt threat (for this creature's
/// threat niche), or `None` when no neighbour is strictly safer than `from`
/// itself (boxed in — the creature holds, cornered). The sign-flip of
/// [`comfort_step`] / [`forage_step`]: minimize threat rather than thermal
/// deviation or maximize food; same three-neighbour scan and
/// `total_cmp`-then-ascending-`RoomAddr` tie-break.
fn flee_step(from: &RoomAddr, terrain: &dyn Terrain, niche: &ThreatNiche) -> Option<RoomAddr> {
    let threat = |room: &RoomAddr| threat_value(niche, &terrain.hazards(room));
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        let t = threat(&n);
        let keep_existing = match &best {
            // Lower threat wins; ties break to the smaller RoomAddr.
            Some((ba, bt)) => t.total_cmp(bt).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep_existing {
            best = Some((n, t));
        }
    }
    let (best_room, best_threat) = best.expect("a room has three neighbors");
    // Only flee when a neighbour is STRICTLY safer than here.
    if best_threat.total_cmp(&threat(from)).is_lt() {
        Some(best_room)
    } else {
        None
    }
}

/// The hop-distance from home at which loneliness saturates to `1.0` (The
/// Belonging) — a creature this many mesh-hops from its people (while home is
/// still REACHABLE) feels maximal isolation. Authored, modest so a creature that
/// strays a little from home already feels the homeward pull.
const LONELY_SCALE_HOPS: f64 = 20.0;

/// The loneliness seek threshold: at/above this the social drive engages (heads
/// home). Modest, like thermal's — a creature a little way from home feels the
/// pull but a comfortable range around home is untroubled. Authored.
const SOCIAL_ACT: f64 = 0.5;

/// The soft-Maslow ceiling on the social (affiliation) drive's urgency
/// contribution — COMFORT-tier (below survival, like thermal/fatigue), so a
/// thirsty/hungry/frightened creature attends to survival first and drifts home
/// only once those are met. Authored.
const SOCIAL_CEIL: f64 = 0.6;

/// The loneliness a creature feels given the A* plan home: the plan's hop-length
/// normalized by [`LONELY_SCALE_HOPS`] and clamped `[0, 1]` — `0` at home
/// (empty plan) and rising with distance, but `0` again when home is UNREACHABLE
/// within budget (`None`). Loneliness is the actionable PULL toward home: a
/// creature within homing range heads home (reading *Searching*/*Eager*), and
/// one beyond reach feels no actionable pull, so its social drive goes DORMANT
/// (comfort, unlike survival thirst — an unreachable home is not a distress but
/// a relocation). This is exactly what keeps a natural world un-lonely (a
/// reachable home is served → not distress) AND leaves a genuinely stranded
/// creature's thirst/other distress unmasked (social dormant). Computed ONCE per
/// drive construction (the plan is reused for the affordance), so the drive's
/// `urgency` stays O(1).
fn loneliness_from_plan(plan_home: &Option<Vec<Action>>) -> f64 {
    match plan_home {
        Some(p) => (p.len() as f64 / LONELY_SCALE_HOPS).clamp(0.0, 1.0),
        None => 0.0,
    }
}

/// Social affiliation — the sixth drive (The Belonging), the first drive whose
/// field is OTHER AGENTS: the pull toward one's own kind. Shaped like thermal
/// comfort (sociality has an optimum — too lonely is felt, and too crowded is
/// the reserved other pole), it reads a company field proxied in v1 by
/// PROXIMITY TO HOME (a creature's home is its people). Loneliness rises with
/// distance from home while home is REACHABLE, and lapses to `0` (dormant) once
/// home is beyond homing range — social is COMFORT, so an unreachable home is a
/// relocation, not a distress. The affordance is the first step home. Silent
/// while asleep. Like thermal/danger it commits no fact and adds no `Action`
/// (homing is a `MoveTo`). Both the loneliness urgency and the home-step are
/// precomputed once (from a single `plan_to_room`) and carried here — like
/// [`Hunger`] holds its folded urgency — so the trait methods are O(1). v1
/// gregariousness is uniform (every creature mildly gregarious); the per-kind
/// sociality niche (solitary ↔ eusocial, the sign-flip at solitary) is reserved.
/// type-audit: bare-ok(ratio: loneliness)
pub struct Social {
    /// The precomputed loneliness (`loneliness_from_plan`) in `[0, 1]` — the
    /// felt isolation, carried here rather than recomputed per call.
    pub loneliness: f64,
    /// The precomputed first step of the A* plan home (`None` at home, or when
    /// home is beyond homing range — either way the drive is dormant then).
    pub home_step: Option<Action>,
}

impl Drive for Social {
    fn urgency(&self, _view: &Perceived) -> f64 {
        self.loneliness
    }
    fn act_threshold(&self) -> f64 {
        SOCIAL_ACT
    }
    fn affordance(&self, _view: &Perceived, _budget: usize) -> Option<Action> {
        // Head home — the precomputed first step toward one's people.
        self.home_step.clone()
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Social
    }
    fn urgency_ceiling(&self) -> f64 {
        SOCIAL_CEIL
    }
    fn serviceability(&self, action: &Action, _view: &Perceived, _budget: usize) -> f64 {
        // Served by the ONE step toward home (an indicator, like thirst/fatigue).
        match &self.home_step {
            Some(a) if a == action => 1.0,
            _ => 0.0,
        }
    }
}

/// The single-drive (thirst-only) decision — the Stage-0 seam, preserved
/// byte-for-byte. It is now a thin specialization of [`arbitrate`]: the
/// action-centric arbitration over the single-element drive set `{Thirst}`
/// yields exactly the old control flow (thirsty and knows water → A* first step
/// and drink; thirsty and ignorant → the explore step, or `Hold` if nowhere
/// new; not thirsty and away → plan home; else `Hold`), because with one drive
/// the max-utility action IS its [`affordance`](Drive::affordance) and the
/// grab/weigh latency is irrelevant. A fresh `Idle` mode per call keeps it
/// stateless, as before. `arbitrate` is the multi-drive live path.
/// type-audit: bare-ok(count: budget)
pub fn decide(view: &Perceived, home: &RoomAddr, p: &DriveParams, budget: usize) -> Intent {
    let thirst = Thirst { params: *p };
    let drives: [&dyn Drive; 1] = [&thirst];
    // The Stage-0 default disposition: grab (latency 0), myopic (horizon 0),
    // not helpless, awake — exactly the literals the byte-identical seam passed.
    let disposition = Disposition {
        latency: 0.0,
        horizon: 0.0,
        helpless: false,
        awake: true,
    };
    arbitrate(view, home, &drives, &disposition, Mode::Idle, budget).intent
}

/// How a creature is disposed to decide right now — the psychology dials that
/// weight its drives and the momentary states that gate them. The same
/// perception and drive set yield DIFFERENT decisions through this: it is the
/// "how this mind decides" bundle [`arbitrate`] reads, distinct from what the
/// creature perceives (`view`/`drives`), the world frame (`home`/`budget`), and
/// the hysteresis carry (`incoming: Mode`). Bundling the two dials (endowment,
/// from the species `PsychVector`) with the two per-tick gates (`helpless`,
/// `awake`) is the tidy every drive campaign flagged.
/// type-audit: bare-ok(ratio: latency), bare-ok(ratio: horizon), bare-ok(flag: helpless), bare-ok(flag: awake)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Disposition {
    /// `deliberation_latency`: slides the arbitration weights from *grab* (0 —
    /// only the loudest drive counts) to *weigh* (1 — the full weighted sum).
    pub latency: f64,
    /// `time_horizon`: slides *myopic* (0 — acts only at `act`) to *foresighted*
    /// (1 — pre-empts a projectable stock drive by its anticipation lead).
    pub horizon: f64,
    /// Learned helplessness — the survival drive has gone unmet so long the
    /// creature has GIVEN UP (short-circuits arbitration to Hold/Helpless).
    pub helpless: bool,
    /// The wake-gate state (The Slumber): while asleep, wake-gated drives fall
    /// silent (unless survival-critical) and the sleep drive engages.
    pub awake: bool,
}

/// Action-centric, deterministic arbitration (spec §5/§6): the seam that turns
/// state into an `Intent` when SEVERAL drives may compete. It does NOT pick a
/// drive and follow its gradient — it enumerates the candidate ACTIONS (the ≤3
/// neighbour `MoveTo`s plus `Drink`) and picks the one of maximum utility, so a
/// single move can serve two needs at once (a cell both warmer AND nearer
/// water). Returns the chosen `Intent` and the NEW commitment [`Mode`] (carry
/// it into the next call for hysteresis).
///
/// - **Utility** of an action `= Σ_{d active} weight_d × capped_urgency_d ×
///   serviceability_d(action)`, where `capped_urgency_d = min(urgency_d,
///   ceiling_d)` (soft Maslow) and only drives at/above their `act` threshold
///   (hysteretically) contribute.
/// - **`deliberation_latency` (`latency ∈ [0,1]`) slides the weights** (§6):
///   the pursued (loudest / committed) drive always has `weight = 1`; every
///   OTHER active drive has `weight = latency`. So `latency = 0` is **grab**
///   (myopic — only the pursued drive counts) and `latency = 1` is **weigh**
///   (the full weighted sum), interpolating linearly between.
/// - **Commitment mode & hysteresis:** the pursued drive engages at `act`,
///   releases below `act − h`, and is switched for a challenger only when the
///   challenger's best-action utility beats the incumbent's by `δ`. With no
///   active drive the NPC falls to `Homing` (a step toward `home`) or `Idle`.
/// - **Determinism:** candidate actions are scanned in ascending-`RoomAddr`
///   order (then `Drink`), and every max is a `total_cmp` keeping the earliest
///   on ties — reload-stable.
///
/// type-audit: bare-ok(count: budget)
pub fn arbitrate(
    view: &Perceived,
    home: &RoomAddr,
    drives: &[&dyn Drive],
    disposition: &Disposition,
    incoming: Mode,
    budget: usize,
) -> Resolution {
    let Disposition {
        latency,
        horizon,
        helpless,
        awake,
    } = *disposition;
    // Learned helplessness (§7, the sticky scar): the survival drive has gone
    // unmet so long the creature has GIVEN UP — it Holds regardless of any
    // affordance (the behavioural difference: it stops trying, where a merely
    // Frustrated creature would still strain), reading `Helpless`. Computed by
    // the caller as a fold over `last_drank` (`learned_helplessness`), which
    // probes periodically so this reverses; here it simply short-circuits the
    // arbitration. Arousal stays high (the need is real and unmet); valence is
    // negative (progress abandoned). Object is thirst — the survival drive whose
    // chronic frustration this measures.
    if helpless {
        let arousal = drives
            .iter()
            .map(|d| d.urgency(view))
            .fold(0.0_f64, f64::max);
        return Resolution {
            intent: Intent::Hold,
            mode: Mode::Pursuing(DriveKind::Thirst),
            affect: Affect {
                arousal,
                valence: -1.0,
                label: AffectLabel::Helpless,
                object: Some(DriveKind::Thirst),
            },
        };
    }

    // Which drives are ACTIVE (contribute to the utility sum). A drive engages
    // at its EFFECTIVE threshold `act − anticipation_lead(horizon)` — foresight
    // (§6, `time_horizon`) lowers `act` so a projectable stock drive engages
    // early (a flow drive grants no lead, so its threshold is just `act`); the
    // incumbent pursued drive stays engaged until it falls a further `h` below
    // that (hysteresis — no boundary dithering).
    // The WAKE-GATE (The Slumber, spec §3): while ASLEEP a wake-gated drive
    // (thirst, thermal) is silent unless it is survival-critical, and the
    // sleep drive (fatigue) is engaged BECAUSE it is the off-phase; while awake
    // every drive engages normally at its threshold.
    let active: Vec<bool> = drives
        .iter()
        .map(|d| {
            let u = d.urgency(view);
            let act_eff = (d.act_threshold() - d.anticipation_lead(horizon)).max(0.0);
            let is_incumbent = matches!(incoming, Mode::Pursuing(k) if k == d.kind());
            let normally = if is_incumbent {
                u >= act_eff - HYSTERESIS_H
            } else {
                u >= act_eff
            };
            if d.seek_while_asleep() {
                !awake || normally
            } else if awake {
                normally
            } else {
                d.survival_override(u)
            }
        })
        .collect();

    // Arousal: the greatest urgency across ALL drives (spec §7) — how activated
    // the mind is, whether or not any has crossed its `act`. A creature grows
    // aroused as thirst rises before it acts: still Content, but not indifferent.
    // Computed once so every return path's affect reads the same value.
    let arousal = drives
        .iter()
        .map(|d| d.urgency(view))
        .fold(0.0_f64, f64::max);

    // No active drive → the errand is over: walk home, or rest if already home.
    // Felt state: Content (positive valence), carrying the sub-act arousal so a
    // reader can tell puttering-calm from growing-thirsty.
    if !active.iter().any(|a| *a) {
        let affect = Affect {
            arousal,
            valence: 1.0,
            label: AffectLabel::Content,
            object: None,
        };
        if view.position != *home {
            let step = plan_to_room(&view.position, home, budget, &view.believed_hazard)
                .and_then(|pl| pl.into_iter().next());
            return Resolution {
                intent: step.map(Intent::Do).unwrap_or(Intent::Hold),
                mode: Mode::Homing,
                affect,
            };
        }
        return Resolution {
            intent: Intent::Hold,
            mode: Mode::Idle,
            affect,
        };
    }

    // The capped urgency each active drive lends the sum (soft Maslow).
    let capped = |i: usize| drives[i].urgency(view).min(drives[i].urgency_ceiling());

    // Candidate actions, in a fixed deterministic order: neighbours ascending,
    // then `Drink` (only ever the winner when a drive's serviceability makes it
    // so — thirst at water; otherwise its utility is 0 and it is never chosen).
    let mut neighbors = view.position.neighbors();
    neighbors.sort();
    let mut candidates: Vec<Action> = neighbors.into_iter().map(Action::MoveTo).collect();
    candidates.push(Action::Drink);
    candidates.push(Action::Rest);
    candidates.push(Action::Eat);

    // A drive's best single-drive (grab-style) utility over the candidates —
    // the score the commitment switch compares incumbent vs challenger on.
    let grab_utility = |i: usize| -> f64 {
        candidates
            .iter()
            .map(|a| capped(i) * drives[i].serviceability(a, view, budget))
            .fold(0.0_f64, f64::max)
    };

    // The loudest active drive by grab-utility (ties broken by the fixed
    // `drives[]`/`DriveKind` order — the first such index wins).
    let loudest = (0..drives.len())
        .filter(|&i| active[i])
        .fold(None::<(usize, f64)>, |best, i| {
            let u = grab_utility(i);
            match best {
                Some((_, bu)) if u.total_cmp(&bu).is_le() => best,
                _ => Some((i, u)),
            }
        })
        .map(|(i, _)| i)
        .expect("at least one drive is active here");

    // The pursued drive, with hysteretic commitment: keep the incumbent unless
    // a challenger's grab-utility beats it by more than δ.
    let pursued = match incoming {
        Mode::Pursuing(k)
            if drives
                .iter()
                .enumerate()
                .any(|(i, d)| d.kind() == k && active[i]) =>
        {
            let inc = drives.iter().position(|d| d.kind() == k).unwrap();
            if loudest != inc
                && grab_utility(loudest)
                    .total_cmp(&(grab_utility(inc) + SWITCH_MARGIN))
                    .is_gt()
            {
                loudest
            } else {
                inc
            }
        }
        _ => loudest,
    };
    let pursued_kind = drives[pursued].kind();

    // Weight each active drive: the pursued drive at 1, every other active
    // drive at `latency` (grab 0 ↔ weigh 1). Then utility = weighted sum.
    let utility = |a: &Action| -> f64 {
        (0..drives.len())
            .filter(|&i| active[i])
            .map(|i| {
                let weight = if drives[i].kind() == pursued_kind {
                    1.0
                } else {
                    latency
                };
                weight * capped(i) * drives[i].serviceability(a, view, budget)
            })
            .sum()
    };

    // The max-utility action, earliest-on-ties (ascending RoomAddr, Drink last).
    let mut best_i = 0usize;
    let mut best_u = utility(&candidates[0]);
    for (i, a) in candidates.iter().enumerate().skip(1) {
        let u = utility(a);
        if u.total_cmp(&best_u).is_gt() {
            best_u = u;
            best_i = i;
        }
    }

    // A positive-utility action advances the errand; otherwise the pursued
    // drive is blocked (unreachable water / boxed-in comfort) — Hold, staying
    // committed (the mode is the errand, even when it cannot step this tick).
    let object = Some(pursued_kind);
    if best_u.total_cmp(&0.0).is_gt() {
        let chosen = candidates[best_i].clone();
        // A progressing decision. Relief (Eager) when the drive is directly MET
        // (a Drink) or the creature is beelining to a KNOWN source it can reach;
        // neutral Searching when it is following a gradient toward an UNKNOWN one
        // (spec §7 — searching is normal seeking, NOT confusion, the load-bearing
        // exclusion from the distress metric). Thermal, which sets no
        // `believed_water`, reads Searching while gradient-seeking comfort — and
        // once the cell is comfortable no drive is active, so it reads Content.
        let known = view.believed_water.is_some();
        let (label, valence) = match &chosen {
            // A need directly MET — a drink, a rest, or a meal.
            Action::Drink | Action::Rest | Action::Eat => (AffectLabel::Eager, 1.0),
            // Beelining to a KNOWN target it can reach: home (fatigue always
            // knows home) or a believed water source.
            Action::MoveTo(_) if pursued_kind == DriveKind::Fatigue || known => {
                (AffectLabel::Eager, 0.5)
            }
            // Following a gradient toward an UNKNOWN one (normal Searching) —
            // this is also hunger's forage-gradient step (seeking richer ground).
            Action::MoveTo(_) => (AffectLabel::Searching, 0.0),
        };
        Resolution {
            intent: Intent::Do(chosen),
            mode: Mode::Pursuing(pursued_kind),
            affect: Affect {
                arousal,
                valence,
                label,
                object,
            },
        }
    } else {
        // Blocked: no candidate reduces the drive. With a KNOWN target it cannot
        // reach (a believed source), the creature is Frustrated ("want it, can't
        // reach it"); with no basis to move toward, it is Lost ("don't know what
        // to do"). Persistence (in the caller) upgrades either to Helpless.
        let label = if view.believed_water.is_some() {
            AffectLabel::Frustrated
        } else {
            AffectLabel::Lost
        };
        Resolution {
            intent: Intent::Hold,
            mode: Mode::Pursuing(pursued_kind),
            affect: Affect {
                arousal,
                valence: -1.0,
                label,
                object,
            },
        }
    }
}

/// The felt state a derived NPC has at `day` — an instantaneous snapshot read
/// from the frozen ledger: the same arbitration a walk step runs, but stateless
/// (belief and last-drank are folded from history; exploration starts fresh, no
/// incumbent mode, so no sticky `Helpless` — persistence is the caller's, e.g.
/// the health metric's continuous loop). The narration seam
/// (`Session::needs`) reads a creature's `Affect` through this. `band` is the
/// same cohort the paired `DriveMovements` moves (The Tidings band-consistency
/// invariant) — a sampled felt state must reflect the belief the creature
/// acted on, not a poorer solo one.
pub fn affect_of(
    frozen: &Ledger,
    npc: &Npc,
    band: &[Npc],
    day: WorldTime,
    terrain: &dyn Terrain,
) -> Affect {
    let mut memo = PrimaryAfraidMemo::new();
    affect_of_memo(frozen, npc, band, day, terrain, &mut memo)
}

/// [`affect_of`] sharing a caller-owned [`PrimaryAfraidMemo`] — for the lab's
/// headless sim, whose per-tick affect reads over one post-tick ledger fold the
/// SAME emitters' primary-fear across every creature; the memo collapses those
/// re-derivations to one `affect_of` per `(emitter, day)`.
pub fn affect_of_memo(
    frozen: &Ledger,
    npc: &Npc,
    band: &[Npc],
    day: WorldTime,
    terrain: &dyn Terrain,
    memo: &mut PrimaryAfraidMemo,
) -> Affect {
    let pos = agent_position(frozen, npc, day);
    let last_drank = frozen
        .find(DRANK)
        .filter(|f| f.subject == npc.entity)
        .filter_map(|f| f.day)
        .fold(0.0_f64, f64::max);
    let believed = shared_believed_water(frozen, npc, band, day, terrain, PLAN_BUDGET);
    let drive = drive_at(
        frozen,
        npc.entity,
        &npc.home,
        day,
        &SUSTENANCE,
        terrain,
        npc.metabolic_class,
    );
    let visited = std::collections::BTreeSet::new();
    let explore_step = lowest_unvisited_neighbor(&pos, &visited, terrain);
    let fatigue = fatigue_at(frozen, npc.entity, day);
    // The Haunt + The Phantom: the ground this creature remembers being
    // frightened on — a fold over its committed history (empty for a never-
    // frightened creature ⇒ byte-identical). The roster is this call's `band`;
    // `alarm_field` invokes `affect_of` with `band = &[]`, so its replay reads
    // a terrain-only memory and the transient re-derivation never recurses.
    let believed_hazard = believed_hazard_memo(frozen, npc, day, terrain, band, memo);
    let view = Perceived {
        position: pos,
        drive,
        fatigue,
        believed_water: believed,
        believed_hazard,
        explore_step,
    };
    let thirst = Thirst { params: SUSTENANCE };
    let thermal = Thermal {
        niche: npc.temperature_niche,
        terrain,
        day,
    };
    let rest = Fatigue {
        home: npc.home.clone(),
    };
    let hunger = Hunger {
        urgency: hunger_at(
            frozen,
            npc.entity,
            &npc.home,
            day,
            terrain,
            npc.metabolic_class,
        ),
        niche: npc.niche.clone(),
        terrain,
        day,
    };
    let danger = Danger {
        terrain,
        threat_niche: npc.threat_niche,
        boldness: npc.boldness,
        // The instantaneous affect read is alarm-free (terrain-sourced only) —
        // this is the read `alarm_field` builds over, so it MUST NOT see borrowed
        // alarm (else secondary transmission, a self-sustaining stampede).
        alarm: None,
    };
    // Affiliation (The Belonging): loneliness + the home-step, precomputed once
    // from a single plan home (reused, so the drive's urgency is O(1)).
    let plan_home = plan_to_room(
        &view.position,
        &npc.home,
        PLAN_BUDGET,
        &view.believed_hazard,
    );
    let social = Social {
        loneliness: loneliness_from_plan(&plan_home),
        home_step: plan_home.and_then(|p| p.into_iter().next()),
    };
    // The metabolism gate (The Kindling): an Ametabolic creature has no
    // homeostatic drives at all — it neither thirsts, thermoregulates, tires
    // (The Slumber), hungers (The Provender), fears (The Dread — a construct
    // does not flinch), nor pines for company (The Belonging), so it reads
    // Content, never distress. The NICHE gate (The Provender, spec §2): hunger
    // is carried only by a creature whose diet niche weights SOMETHING — an
    // empty niche means "no food drive" (no axis, so no source serves it).
    let ametabolic = matches!(npc.metabolic_class, MetabolicClass::Ametabolic);
    let mut drives: Vec<&dyn Drive> = Vec::new();
    if !ametabolic {
        drives.push(&thirst);
        drives.push(&thermal);
        drives.push(&rest);
        if !npc.niche.is_zero() {
            drives.push(&hunger);
        }
        drives.push(&danger);
        drives.push(&social);
    }
    let helpless = !ametabolic && learned_helplessness(last_drank, day.day);
    let disposition = Disposition {
        latency: npc.deliberation_latency,
        horizon: npc.time_horizon,
        helpless,
        awake: is_awake(npc.activity, terrain, &view.position, day),
    };
    arbitrate(
        &view,
        &npc.home,
        &drives,
        &disposition,
        Mode::Idle,
        PLAN_BUDGET,
    )
    .affect
}

/// The per-tick ALARM field (The Alarm) — fear-contagion as a derived,
/// order-independent field over the frozen population, the vessel's dynamic
/// sibling of `worldgen::predator_pressure`. For each creature that is
/// **primary-afraid** (its own Danger drive is active — `affect_of` reads
/// `object == Some(Danger)` with `arousal ≥ DANGER_ACT`), it stamps the
/// emitter's felt-threat magnitude onto its cell and each `neighbors()` cell
/// (a one-hop halo), accumulating (`+=`) across emitters, then clamps every
/// entry to `[0, 1]`. Empty when no creature is primary-afraid.
///
/// # The termination invariant (spec §3)
///
/// The field is built by reading `affect_of` **alarm-free** — the frozen
/// ledger holds no committed alarm (affect is immaterial, never committed), and
/// `affect_of`'s own Danger drive passes `alarm: None`. So an emitter's danger
/// is necessarily **terrain-sourced**: a creature alarmed only by contagion
/// (borrowed alarm) is NOT itself an emitter, and secondary transmission (a
/// self-sustaining stampede, `R0 ≥ 1`) is impossible by construction. Only the
/// tick's Danger drive then READS the field (via `alarm: Some(&field)`) — the
/// wave is a bounded halo around genuine hazard, collapsing the next tick once
/// the hazard clears.
///
/// # Determinism
///
/// Accumulation into a `BTreeMap` with `+=` is order-independent (addition is
/// commutative), so the field is the same regardless of `npcs` order; the clamp
/// is applied once at the end over the sorted keys. The field is a compute-path
/// intermediate, never serialized.
///
/// type-audit: bare-ok(ratio: return)
pub fn alarm_field(
    frozen: &Ledger,
    npcs: &[Npc],
    terrain: &dyn Terrain,
    day: WorldTime,
) -> std::collections::BTreeMap<RoomAddr, f64> {
    let mut memo = PrimaryAfraidMemo::new();
    alarm_field_memo(frozen, npcs, terrain, day, &mut memo)
}

/// [`alarm_field`] sharing a caller-owned [`PrimaryAfraidMemo`] so its
/// primary-afraid reads coincide with the tick's re-derivation reads over the
/// same `frozen` — one `affect_of` per `(emitter, day)` for the whole tick.
/// type-audit: bare-ok(ratio: return)
pub fn alarm_field_memo(
    frozen: &Ledger,
    npcs: &[Npc],
    terrain: &dyn Terrain,
    day: WorldTime,
    memo: &mut PrimaryAfraidMemo,
) -> std::collections::BTreeMap<RoomAddr, f64> {
    let mut field: std::collections::BTreeMap<RoomAddr, f64> = std::collections::BTreeMap::new();
    for npc in npcs {
        let pos = agent_position(frozen, npc, day);
        // THE CHEAP GATE (The Phantom perf, byte-identical). A creature can be
        // primary-afraid ONLY if its OWN terrain threat there crosses act — the
        // alarm-free Danger urgency is `threat_field × mettle_factor`, and
        // `object == Danger` requires it ≥ act. So a creature on safe ground can
        // never be an emitter; skip the EXPENSIVE `affect_of` (full arbitration,
        // an A* plan-home) for it. This is a NECESSARY condition, not the
        // decision — a terrain-afraid creature still goes through `affect_of`
        // below to confirm Danger WINS. It is what keeps the re-derived transient
        // memory (`believed_hazard` folds this per visited cell) cheap on the
        // emitter-free common case: no hazard underfoot ⇒ no `affect_of` at all.
        if threat_field(&pos, &npc.threat_niche, terrain) * mettle_factor(npc.boldness) < DANGER_ACT
        {
            continue;
        }
        // The ALARM-FREE, memoized primary-afraid read (the build invariant):
        // `affect_of` senses only the terrain hazards, never borrowed alarm — so
        // emission is terrain-sourced by construction and the wave terminates.
        // The Tidings: an EMPTY band — the field reads each creature's intrinsic
        // affect (its own home-anchored belief), not band-shared belief. `&[]`
        // reproduces `affect_of`'s pre-Tidings (bandless) behaviour exactly.
        // `magnitude` is the emitter's Danger arousal, or `0.0` when it is not
        // primary-afraid (no emission).
        let magnitude = emitter_arousal(&mut memo.afraid, frozen, npc, day, terrain);
        if magnitude <= 0.0 {
            continue;
        }
        // Stamp the emitter's felt-threat magnitude on its cell and the one-hop
        // halo (its three edge-neighbours), accumulating across emitters.
        *field.entry(pos.clone()).or_insert(0.0) += magnitude;
        for n in pos.neighbors() {
            *field.entry(n).or_insert(0.0) += magnitude;
        }
    }
    // Saturation: a stampeding crowd is not infinitely scarier than a threshold
    // few. Clamp once at the emit boundary, over the sorted keys.
    for v in field.values_mut() {
        *v = v.clamp(0.0, 1.0);
    }
    field
}

/// The plan search's node-expansion budget: generous for the short local
/// journeys every derived NPC actually walks (`nearest_water` finds a real
/// world's water within a handful of mesh hops of home), but finite so a
/// pathological distance genuinely gives up (`Intent::Hold`) rather than
/// paying for a global search — the one search-budget judgment call
/// (spec §8).
const PLAN_BUDGET: usize = 1_000;

/// One action's duration: the one authored action-duration judgment call
/// (spec §8). Small relative to a drive cycle (SUSTENANCE's ~5.7-day rise) so
/// a multi-room journey still resolves within a single `wait`.
const MOVE_DURATION: f64 = 0.1;

/// The per-NPC step cap on `DriveMovements::step`'s inner loop — the
/// strict-progress guard's backstop: even if a decision loop somehow failed
/// to advance `day` on every iteration, this bounds total work per tick
/// (termination guarantee, The Foresight T3 review).
const MAX_STEPS: usize = 10_000;

/// A committed `agent-at` fact: `entity` moved to `target` on `day`, with
/// `provenance` naming why.
fn agent_at_fact(entity: EntityId, target: &RoomAddr, day: f64, provenance: &str) -> Fact {
    Fact {
        subject: entity,
        predicate: AGENT_AT.to_string(),
        object: Value::Text(room_to_text(target)),
        place: None,
        day: Some(day),
        provenance: provenance.to_string(),
    }
}

/// Plant an agent at `room` on `day` — the very `agent-at` fact the drive tick
/// commits, exposed so a scenario harness can POSITION an agent before running
/// the sim (e.g. stranding a creature far from a water source it believes in,
/// to exercise genuine distress the drive model rarely produces on its own).
/// Committing this into a seed ledger and reading `affect_of`/`run_simulation`
/// over it is the synthetic complement to the real-world health sweep — the
/// same seam, a hand-built scenario instead of a derived population. Typed
/// throughout (no primitive at the boundary), so it needs no type-audit tag.
pub fn place_agent(entity: EntityId, room: &RoomAddr, day: WorldTime) -> Fact {
    agent_at_fact(entity, room, day.day, "harness-placement")
}

/// A committed `drank` fact: `entity` satisfied its sustenance goal on `day`.
fn drank_fact(entity: EntityId, day: f64, provenance: &str) -> Fact {
    Fact {
        subject: entity,
        predicate: DRANK.to_string(),
        object: Value::Flag(true),
        place: None,
        day: Some(day),
        provenance: provenance.to_string(),
    }
}

/// A committed `rested` fact: `entity` slept (reset its fatigue) on `day` — The
/// Slumber's discharge, the fatigue twin of [`drank_fact`].
fn rested_fact(entity: EntityId, day: f64, provenance: &str) -> Fact {
    Fact {
        subject: entity,
        predicate: RESTED.to_string(),
        object: Value::Flag(true),
        place: None,
        day: Some(day),
        provenance: provenance.to_string(),
    }
}

/// A committed `eaten` fact: `entity` ate (reset its hunger) on `day` — The
/// Provender's discharge, the hunger twin of [`drank_fact`].
fn eaten_fact(entity: EntityId, day: f64, provenance: &str) -> Fact {
    Fact {
        subject: entity,
        predicate: EATEN.to_string(),
        object: Value::Flag(true),
        place: None,
        day: Some(day),
        provenance: provenance.to_string(),
    }
}

/// The drive-driven movement system (The Foresight → The Surmise): each NPC
/// steps through its belief-driven plan — exploring while ignorant, beelining
/// once it knows water — committing a dated `agent-at`/`drank` at each
/// executed step. Holds a `Terrain` to compute belief and exploration
/// mid-walk. Run through c6's `tick`.
/// type-audit: bare-ok(return)
pub struct DriveMovements<'a> {
    /// The NPCs this tick advances.
    pub npcs: Vec<Npc>,
    /// The interval start (the session's previous day).
    pub from: WorldTime,
    /// The interval end (the session's new day).
    pub to: WorldTime,
    /// The drive parameters.
    pub params: DriveParams,
    /// The elevation field belief and exploration read.
    pub terrain: &'a dyn Terrain,
}

impl<'a> TickSystem for DriveMovements<'a> {
    fn label(&self) -> &'static str {
        "drive-movements"
    }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        let mut out: Vec<Fact> = Vec::new();
        // One primary-afraid memo for the whole step: `frozen` is fixed here, so
        // every emitter's `(entity, day)` verdict — read by the alarm field AND by
        // each creature's `believed_hazard` re-derivation — is computed once (see
        // `PrimaryAfraidMemo`). Byte-identical: a cache of a pure function over a
        // fixed ledger.
        let mut afraid_memo = PrimaryAfraidMemo::new();
        // THE ALARM: build the per-tick alarm field ONCE, from the frozen
        // population, before advancing any creature — it is fixed across the whole
        // interval (the next-tick wave). Built alarm-free (via `affect_of`), so
        // emission is terrain-sourced and the wave terminates; the per-step Danger
        // drive below then reads it at each creature's cell.
        let alarm = alarm_field_memo(
            frozen,
            &self.npcs,
            self.terrain,
            self.from,
            &mut afraid_memo,
        );
        for npc in &self.npcs {
            let mut pos = agent_position(frozen, npc, self.from);
            let mut day = self.from.day;
            // A scratch ledger view isn't available; track drank locally: derive
            // the starting last-drank day from `frozen`, then simulate forward,
            // updating a local `last_drank` as we emit `DRANK` facts.
            let mut last_drank = frozen
                .find(DRANK)
                .filter(|f| f.subject == npc.entity)
                .filter_map(|f| f.day)
                .fold(0.0_f64, f64::max);
            // Likewise the last rest day (The Slumber): fatigue is time since it,
            // reset when a `rested` fact is emitted.
            let mut last_rested = frozen
                .find(RESTED)
                .filter(|f| f.subject == npc.entity)
                .filter_map(|f| f.day)
                .fold(0.0_f64, f64::max);
            // Likewise the last meal day (The Provender): hunger is a path
            // integral since it, reset when an `eaten` fact is emitted.
            let mut last_ate = frozen
                .find(EATEN)
                .filter(|f| f.subject == npc.entity)
                .filter_map(|f| f.day)
                .fold(0.0_f64, f64::max);
            // Belief and exploration state, evolved locally across the walk (the
            // fold includes this tick's own emitted moves). Seed belief from the
            // pre-tick history; grow it whenever the agent stands in water.
            // The Tidings: seed from the BAND's pooled belief (co-located
            // members share what they know), not the creature's alone.
            let mut believed = shared_believed_water(
                frozen,
                npc,
                &self.npcs,
                self.from,
                self.terrain,
                PLAN_BUDGET,
            );
            // The Haunt + The Phantom: the ground this creature remembers being
            // frightened on — a fold over its committed (pre-tick) history,
            // computed ONCE per creature. The FULL population is the roster, so
            // the memory folds the re-derived transient alarm too (the phantom);
            // the most-recent-visit staleness clears a disproven fear. Empty for
            // a never-frightened creature ⇒ every planner edge stays `1`, byte-
            // identical (no primary-afraid emitter on the settled worlds).
            let believed_hazard = believed_hazard_memo(
                frozen,
                npc,
                self.from,
                self.terrain,
                &self.npcs,
                &mut afraid_memo,
            );
            let mut visited: std::collections::BTreeSet<RoomAddr> =
                std::collections::BTreeSet::new();
            visited.insert(pos.clone());
            let mut steps = 0usize;
            // The commitment mode, carried across this walk's steps (session-
            // sandboxed hysteresis; re-derived, never persisted). Starts Idle.
            let mut mode = Mode::Idle;
            loop {
                if day > self.to.day || steps >= MAX_STEPS {
                    break;
                }
                steps += 1;
                // Standing in water forms/updates belief (nearest-to-home wins).
                if is_water(&pos, self.terrain) {
                    believed = nearer_to_home(&npc.home, believed.take(), pos.clone(), PLAN_BUDGET);
                }
                // The temperature-coupled thirst integral (The Kindling),
                // re-derived over the committed history so far (`frozen` + this
                // tick's own emitted `out` moves), so the tick's drive matches
                // `affect_of`'s exactly — one shared fold, no manual alignment.
                let mut sightings = agent_sightings(frozen, npc.entity, day);
                for f in &out {
                    if f.subject == npc.entity
                        && f.predicate == AGENT_AT
                        && let Value::Text(s) = &f.object
                        && let Some(d) = f.day
                        && d <= day
                    {
                        sightings.push((d, room_from_text(s)));
                    }
                }
                sightings.sort_by(|a, b| a.0.total_cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
                let drive = integrate_thirst(
                    &sightings,
                    &npc.home,
                    last_drank,
                    day,
                    self.terrain,
                    npc.metabolic_class,
                    &self.params,
                );
                // Hunger reuses the same Kindling integral over the same
                // occupancy (The Provender), but since the last MEAL, at the
                // HUNGER rate — the second consumer of the path-integral.
                let hunger_urgency = integrate_thirst(
                    &sightings,
                    &npc.home,
                    last_ate,
                    day,
                    self.terrain,
                    npc.metabolic_class,
                    &HUNGER,
                );
                let explore_step = lowest_unvisited_neighbor(&pos, &visited, self.terrain);
                let fatigue = (FATIGUE_RISE * (day - last_rested)).clamp(0.0, 1.0);
                let view = Perceived {
                    position: pos.clone(),
                    drive,
                    fatigue,
                    believed_water: believed.clone(),
                    believed_hazard: believed_hazard.clone(),
                    explore_step,
                };
                // BOTH drives now compete: thirst (the drank-fold, on the view)
                // and thermal (the species' niche read against the per-day
                // temperature at the cell). Where thermal is inactive (a
                // comfortable cell — planted terrain with no temperature reads
                // INFINITY → urgency 0), arbitration reduces to thirst-only and
                // the walk is byte-identical to Stage 0.
                let thirst = Thirst {
                    params: self.params,
                };
                let thermal = Thermal {
                    niche: npc.temperature_niche,
                    terrain: self.terrain,
                    day: WorldTime { day },
                };
                let rest = Fatigue {
                    home: npc.home.clone(),
                };
                let hunger = Hunger {
                    urgency: hunger_urgency,
                    niche: npc.niche.clone(),
                    terrain: self.terrain,
                    day: WorldTime { day },
                };
                let danger = Danger {
                    terrain: self.terrain,
                    threat_niche: npc.threat_niche,
                    boldness: npc.boldness,
                    // THE ALARM: the tick's mover READS the per-tick field (built
                    // alarm-free above), so a calm creature beside genuine distress
                    // catches the alarm and flees. Empty on the settled worlds
                    // (no primary distress) ⇒ byte-identical.
                    alarm: Some(&alarm),
                };
                // Affiliation (The Belonging): loneliness + home-step, from one
                // plan home (reused, so urgency is O(1)).
                let plan_home = plan_to_room(&pos, &npc.home, PLAN_BUDGET, &view.believed_hazard);
                let social = Social {
                    loneliness: loneliness_from_plan(&plan_home),
                    home_step: plan_home.and_then(|p| p.into_iter().next()),
                };
                // The metabolism gate (The Kindling): Ametabolic → no drives.
                // The niche gate (The Provender): an empty diet → no hunger.
                // Danger (The Dread) and social (The Belonging) ride the same
                // metabolic gate (a construct does not flinch or pine); danger is
                // niche-less, social's gregariousness uniform in v1.
                let ametabolic = matches!(npc.metabolic_class, MetabolicClass::Ametabolic);
                let mut drives: Vec<&dyn Drive> = Vec::new();
                if !ametabolic {
                    drives.push(&thirst);
                    drives.push(&thermal);
                    drives.push(&rest);
                    if !npc.niche.is_zero() {
                        drives.push(&hunger);
                    }
                    drives.push(&danger);
                    drives.push(&social);
                }
                let helpless = !ametabolic && learned_helplessness(last_drank, day);
                let disposition = Disposition {
                    latency: npc.deliberation_latency,
                    horizon: npc.time_horizon,
                    helpless,
                    awake: is_awake(npc.activity, self.terrain, &pos, WorldTime { day }),
                };
                let resolution =
                    arbitrate(&view, &npc.home, &drives, &disposition, mode, PLAN_BUDGET);
                mode = resolution.mode;
                match resolution.intent {
                    Intent::Do(Action::MoveTo(n)) => {
                        day += MOVE_DURATION;
                        if day > self.to.day {
                            break;
                        }
                        // Provenance follows the committed errand (the mode):
                        // thirst distinguishes BELIEVED (beelining a known
                        // source) from IGNORANT (exploring blind); thermal names
                        // the comfort-seeking; homing names the sated walk back.
                        let provenance = match mode {
                            Mode::Pursuing(DriveKind::Thermal) => "sought a kinder clime (comfort)",
                            Mode::Pursuing(DriveKind::Fatigue) => "turned home, weary, to rest",
                            Mode::Pursuing(DriveKind::Hunger) => {
                                "foraged toward richer ground (hunger)"
                            }
                            Mode::Pursuing(DriveKind::Danger) => "fled the uncanny ground (fear)",
                            Mode::Pursuing(DriveKind::Social) => {
                                "drifted homeward, missing its people (belonging)"
                            }
                            Mode::Pursuing(DriveKind::Thirst) if believed.is_some() => {
                                "went down to the river it knew (thirst)"
                            }
                            Mode::Pursuing(DriveKind::Thirst) => {
                                "wandered, having found no water yet (thirst)" // ignorant
                            }
                            Mode::Homing | Mode::Idle => "walking home (sated)",
                        };
                        out.push(agent_at_fact(npc.entity, &n, day, provenance));
                        visited.insert(n.clone());
                        pos = n;
                    }
                    Intent::Do(Action::Drink) => {
                        out.push(drank_fact(
                            npc.entity,
                            day,
                            "drank from the river (thirst sated)",
                        ));
                        last_drank = day;
                    }
                    Intent::Do(Action::Rest) => {
                        out.push(rested_fact(
                            npc.entity,
                            day,
                            "slept at home (fatigue eased)",
                        ));
                        last_rested = day;
                        // Sleep through the off-phase in one jump to the next
                        // waking, rather than re-resting every step (The Slumber).
                        day = next_awake_day(npc.activity, self.terrain, &pos, day);
                        if day > self.to.day {
                            break;
                        }
                    }
                    Intent::Do(Action::Eat) => {
                        out.push(eaten_fact(
                            npc.entity,
                            day,
                            "grazed the productive ground (hunger sated)",
                        ));
                        last_ate = day;
                    }
                    Intent::Hold => {
                        // Idle (or unreachable): jump to the next act-crossing
                        // in closed form rather than spinning day-by-day. The
                        // strict-progress guarantee: `next_act <= day` breaks
                        // (a thirsty-but-unreachable Hold recomputes the SAME
                        // next_act every iteration — without this check, that
                        // spins to `MAX_STEPS` for nothing).
                        //
                        // Held at a fixed cell, thirst rises linearly at that
                        // cell's COUPLED rate (The Kindling: temperature is
                        // sampled once per occupancy segment, so a held cell has
                        // a constant rate and the jump stays closed-form): the
                        // next crossing is `day + (act − drive) / rate_here`.
                        //
                        // A degenerate `rate == 0.0` makes `next_act` NaN or
                        // infinite. Every NaN comparison is `false`, so leaving
                        // `day` untouched (rather than assigning it) keeps
                        // `drive` well-defined next iteration. Without this guard
                        // a NaN `day` makes `drive` NaN, flipping the thirst
                        // check into the plan-home branch — a budgeted A* search
                        // every remaining iteration up to `MAX_STEPS`, an
                        // O(MAX_STEPS * PLAN_BUDGET) blowup (the-surmise T3
                        // review) instead of the intended cheap spin. `steps >=
                        // MAX_STEPS` alone still bounds the loop.
                        let rate_here = rise_at(
                            self.terrain.temperature(&pos, WorldTime { day }),
                            npc.metabolic_class,
                            &self.params,
                        );
                        let next_act = day + (self.params.act - drive) / rate_here;
                        if !next_act.is_finite() {
                            continue;
                        }
                        if next_act <= day || next_act > self.to.day {
                            break;
                        }
                        day = next_act;
                    }
                }
            }
        }
        out
    }
}

/// The nearer-to-home of an existing belief and a newly-perceived water room.
/// The tick's incremental fold — and its tie-break MUST match `believed_water`'s
/// (smaller `RoomAddr` wins on an equal hop-distance), or a mid-walk incremental
/// belief could disagree with the same belief re-derived from the committed
/// history, making the chosen source faintly sensitive to `wait` granularity
/// (the-surmise T3+T4 review). Aligned here so the two folds are identical.
fn nearer_to_home(
    home: &RoomAddr,
    current: Option<RoomAddr>,
    found: RoomAddr,
    budget: usize,
) -> Option<RoomAddr> {
    let d = |r: &RoomAddr| {
        plan_to_room(home, r, budget, &std::collections::BTreeSet::new()).map(|p| p.len())
    };
    match current {
        None => Some(found),
        Some(c) => match (d(&c), d(&found)) {
            (Some(dc), Some(df)) => Some(match df.cmp(&dc) {
                std::cmp::Ordering::Less => found,
                std::cmp::Ordering::Greater => c,
                // Tie on hop-distance: smaller RoomAddr wins (matches
                // `believed_water`'s `min_by((hop, RoomAddr))`).
                std::cmp::Ordering::Equal => std::cmp::min(c, found),
            }),
            (None, Some(_)) => Some(found),
            _ => Some(c),
        },
    }
}

/// The lowest-elevation neighbour not yet visited this walk (the directed-
/// exploration step), or `None` if every neighbour is visited. Terminating: the
/// visited set only grows.
fn lowest_unvisited_neighbor(
    from: &RoomAddr,
    visited: &std::collections::BTreeSet<RoomAddr>,
    terrain: &dyn Terrain,
) -> Option<RoomAddr> {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        if visited.contains(&n) {
            continue;
        }
        let elev = terrain.elevation(&n);
        let keep = match &best {
            Some((ba, be)) => elev.total_cmp(be).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep {
            best = Some((n, elev));
        }
    }
    best.map(|(r, _)| r)
}

/// Order settlements for NPC derivation: population descending (ties broken
/// by `EntityId`), with `home_settlement` pulled to the front regardless of
/// its rank. Pure and independently testable (no world/ledger needed) so the
/// colocation guarantee is mutation-provable on its own, not just as an
/// emergent property of a particular seed's population distribution.
fn ordered_for_derivation(
    mut settlements: Vec<hornvale_settlement::VillageInfo>,
    home_settlement: EntityId,
) -> Vec<hornvale_settlement::VillageInfo> {
    settlements.sort_by(|a, b| b.population.cmp(&a.population).then(a.id.cmp(&b.id)));
    if let Some(pos) = settlements.iter().position(|v| v.id == home_settlement) {
        let home = settlements.remove(pos);
        settlements.insert(0, home);
    }
    settlements
}

/// Derive `k` NPCs from the `k` most-populous settlements, GUARANTEEING the
/// possessed agent's own home settlement (`home_settlement`) is among them —
/// otherwise no NPC is ever co-located with the player and the observation
/// payoff (spec: "the herder has gone down to the river") can never fire
/// (the-quickening T3 review). Each NPC is minted in `ledger` (a
/// session-owned clone), homed at its settlement's cell room, with its
/// drive's resource anchor (`nearest_water` over the true terrain, The
/// Surmise) and species' activity-cycle.
/// type-audit: bare-ok(count: k)
pub fn derive_npcs(
    world: &World,
    ctx: &LocaleContext,
    ledger: &mut Ledger,
    k: usize,
    home_settlement: EntityId,
) -> Vec<Npc> {
    let settlements = hornvale_settlement::all_settlements(world);
    let mut settlements = ordered_for_derivation(settlements, home_settlement);
    settlements.truncate(k);

    // Authored per-species data, read once: the temperature niche (the thermal
    // drive's setpoint/tolerance) and the psych vector's two runtime dials —
    // deliberation latency (the arbitration tuning) and time horizon (the
    // anticipation lead). Threaded onto each NPC the same way `activity` is —
    // the perception/psych pattern.
    let biosphere = hornvale_species::biosphere_registry();
    let psyche = hornvale_species::psyche_registry();

    settlements
        .into_iter()
        .map(|village| {
            let home = settlement_room(world, ctx, village.id);
            let resource = nearest_water(&home, &LocaleTerrain::new(ctx), PLAN_BUDGET)
                .unwrap_or_else(|| home.clone());
            let species = hornvale_species::species_of(world, village.id)
                .unwrap_or_else(|| "goblin".to_string());
            let activity = species_activity(world, &species);
            let temperature_niche = biosphere
                .get_by_label(&species)
                .map(|t| t.condition_niche.temperature)
                .unwrap_or(DEFAULT_TEMPERATURE_NICHE);
            let metabolic_class = biosphere
                .get_by_label(&species)
                .map(|t| t.metabolic_class)
                .unwrap_or(MetabolicClass::Endotherm);
            let niche = biosphere
                .get_by_label(&species)
                .map(|t| t.niche.clone())
                .unwrap_or_else(default_diet_niche);
            let deliberation_latency = psyche
                .get_by_label(&species)
                .map(|p| p.deliberation_latency)
                .unwrap_or(0.5);
            let time_horizon = psyche
                .get_by_label(&species)
                .map(|p| p.time_horizon)
                .unwrap_or(0.5);
            // Boldness (The Mettle): the banked `threat_response` read at
            // creature scope. Default steady/inert for a species without a
            // psyche entry — the beasts.
            let boldness = psyche
                .get_by_label(&species)
                .map(|p| p.threat_response)
                .unwrap_or(BOLDNESS_STEADY);
            // The threat niche (The Bane): derived from the temperature niche +
            // metabolic class already on hand — no fresh authoring.
            let threat_niche = derive_threat_niche(&temperature_niche, metabolic_class, &niche);
            let entity = ledger.mint_entity();
            let label = format!("{species} of {}", village.name);
            // A NAME fact so the provenance read (`why`, backed by
            // `windows/historiography::recount`) leads with the NPC's own
            // label rather than a bare entity id — NAME is kernel-core, so
            // it is already registered in `world.registry` (never a new
            // per-session predicate the way AGENT_AT is). Committed once,
            // at derivation, to the session-owned ledger clone only — never
            // genesis (this function never runs against a world's own
            // ledger, only a session's clone; see `liveness_genesis.rs`).
            ledger
                .commit(
                    Fact {
                        subject: entity,
                        predicate: hornvale_kernel::NAME.to_string(),
                        object: Value::Text(label.clone()),
                        place: None,
                        day: None,
                        provenance: "the-quickening".to_string(),
                    },
                    &world.registry,
                )
                .expect("a freshly minted NPC entity's first NAME fact always commits");
            Npc {
                entity,
                home,
                resource,
                species,
                activity,
                temperature_niche,
                deliberation_latency,
                time_horizon,
                metabolic_class,
                niche,
                boldness,
                threat_niche,
                label,
            }
        })
        .collect()
}

/// Derive up to `k` WILD NPCs (The Wilding) — beast agents, one per distinct
/// mobile-beast concentration (`worldgen::wild_concentrations`: a herd, a lair).
/// A wild NPC is the same `Npc` a settlement produces — its home is the
/// concentration's cell, its traits its biosphere's, its psyche the DEFAULT
/// (beasts carry no `psyche_registry` entry, so the `.unwrap_or` fallbacks apply,
/// exactly as they already do for a settlement of a non-peopled species). The
/// threat niche derives (The Bane/Quarry) with LIVE predator dread, so a
/// herbivore beast finally FEARS predator ground — The Quarry, waking. Appended
/// to the peopled `derive_npcs` output; genesis untouched (the session's ledger
/// clone only, like `derive_npcs`).
/// type-audit: bare-ok(count: k)
pub fn derive_wild_npcs(
    world: &World,
    ctx: &LocaleContext,
    ledger: &mut Ledger,
    k: usize,
) -> Vec<Npc> {
    let concentrations = hornvale_worldgen::wild_concentrations(world, k).unwrap_or_default();
    let biosphere = hornvale_species::biosphere_registry();
    let psyche = hornvale_species::psyche_registry();
    concentrations
        .into_iter()
        .map(|(species, position)| {
            let home = RoomAddr::containing(position, walk_depth(ctx));
            let resource = nearest_water(&home, &LocaleTerrain::new(ctx), PLAN_BUDGET)
                .unwrap_or_else(|| home.clone());
            let activity = species_activity(world, &species);
            let temperature_niche = biosphere
                .get_by_label(&species)
                .map(|t| t.condition_niche.temperature)
                .unwrap_or(DEFAULT_TEMPERATURE_NICHE);
            let metabolic_class = biosphere
                .get_by_label(&species)
                .map(|t| t.metabolic_class)
                .unwrap_or(MetabolicClass::Endotherm);
            let niche = biosphere
                .get_by_label(&species)
                .map(|t| t.niche.clone())
                .unwrap_or_else(default_diet_niche);
            let deliberation_latency = psyche
                .get_by_label(&species)
                .map(|p| p.deliberation_latency)
                .unwrap_or(0.5);
            let time_horizon = psyche
                .get_by_label(&species)
                .map(|p| p.time_horizon)
                .unwrap_or(0.5);
            let boldness = psyche
                .get_by_label(&species)
                .map(|p| p.threat_response)
                .unwrap_or(BOLDNESS_STEADY);
            let threat_niche = derive_threat_niche(&temperature_niche, metabolic_class, &niche);
            let entity = ledger.mint_entity();
            let label = format!("a wild {species}");
            ledger
                .commit(
                    Fact {
                        subject: entity,
                        predicate: hornvale_kernel::NAME.to_string(),
                        object: Value::Text(label.clone()),
                        place: None,
                        day: None,
                        provenance: "the-wilding".to_string(),
                    },
                    &world.registry,
                )
                .expect("a freshly minted wild NPC's first NAME fact always commits");
            Npc {
                entity,
                home,
                resource,
                species,
                activity,
                temperature_niche,
                deliberation_latency,
                time_horizon,
                metabolic_class,
                niche,
                boldness,
                threat_niche,
                label,
            }
        })
        .collect()
}

/// The diet-niche fallback for a species missing from the biosphere registry
/// (defensive — `species` always resolves to at least the registered `goblin`
/// default). A balanced omnivore, so an unknown species can feed on ordinary
/// productive ground rather than starving.
fn default_diet_niche() -> ResourceVector {
    ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)])
        .expect("the default omnivore niche is valid")
}

/// The temperature-niche fallback for a species missing from the biosphere
/// registry (defensive — `species` always resolves to at least the `goblin`
/// default, which IS registered). A wide, mild, low-devotion band so the
/// thermal drive of an unknown species stays quiescent rather than flailing.
const DEFAULT_TEMPERATURE_NICHE: ConditionResponse = ConditionResponse {
    optimum: 15.0,
    width: 25.0,
    devotion: 0.5,
};

/// The room containing a settlement's cell at walk depth (mirrors
/// `mint_flagship`, via the shared `settlement_position` helper).
fn settlement_room(world: &World, ctx: &LocaleContext, settlement: EntityId) -> RoomAddr {
    let pos = settlement_position(world, settlement);
    RoomAddr::containing(pos, walk_depth(ctx))
}

/// The species' activity-cycle, from its committed `SPECIES_ACTIVITY_CYCLE`
/// fact on the species' own entity (resolved by name via `species_entity`).
/// Defaults to `Diurnal` if the species or the fact is missing.
fn species_activity(world: &World, species: &str) -> ActivityCycle {
    hornvale_species::species_entity(world, species)
        .and_then(|e| {
            match world
                .ledger
                .value_of(e, hornvale_species::SPECIES_ACTIVITY_CYCLE)
            {
                Some(Value::Text(t)) => Some(parse_activity(t)),
                _ => None,
            }
        })
        .unwrap_or(ActivityCycle::Diurnal)
}

/// Parse the committed activity-cycle text (see
/// `windows/worldgen/src/lib.rs`'s species genesis, which commits exactly
/// these three strings). Unknown text defaults to `Diurnal`.
fn parse_activity(t: &str) -> ActivityCycle {
    match t {
        "nocturnal" => ActivityCycle::Nocturnal,
        "crepuscular" => ActivityCycle::Crepuscular,
        _ => ActivityCycle::Diurnal,
    }
}

/// A GOAP action — a precondition/effect transformation over the plan state.
/// Minimal + heterogeneous (the precondition chain needs two kinds); the MAP-27
/// authored-verb DSL is a followup.
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    /// Walk to an adjacent room (precondition: adjacency; effect: position).
    MoveTo(RoomAddr),
    /// Drink (precondition: at the water room; effect: hydrated).
    Drink,
    /// Rest / sleep (precondition: at home; effect: fatigue reset) — The
    /// Slumber's discharge action, the fatigue analogue of `Drink`.
    Rest,
    /// Eat / graze (precondition: standing on a cell rich enough to feed;
    /// effect: hunger reset) — The Provender's discharge action, the hunger
    /// analogue of `Drink`.
    Eat,
}

/// The GOAP planning state A* searches: where the agent is and whether it has
/// drunk. `Ord` for the deterministic search.
/// type-audit: bare-ok(flag: hydrated)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PlanState {
    /// The agent's room.
    pub position: RoomAddr,
    /// Whether the sustenance goal is met (has drunk this plan).
    pub hydrated: bool,
}

/// The extra `MoveTo` cost the planners charge for stepping INTO a
/// remembered-dangerous cell (The Haunt): a finite detour budget over the
/// baseline edge cost of `1`, so the A* routes AROUND remembered-bad ground
/// whenever a detour is cheaper than the penalty, yet still braves it when the
/// detour would exceed the penalty (survival-override for free — the finite cost
/// IS the override, never a wall). Deliberately SMALL (decision-ledger #4): the
/// planners run Dijkstra-mode (`heuristic() == 0`, budget `PLAN_BUDGET` node
/// expansions), so a LARGE penalty makes A* exhaust its budget exploring the
/// cost-radius around a chokepoint remembered cell and return `None` — the
/// creature freezes instead of detouring (the over-avoidance failure; `20` froze
/// ~900 seed-42 fauna). `5` keeps the cost-radius within budget so avoidance is
/// graceful (the seed-42 possession `stirred` count barely moves — a handful of
/// beasts detour, none freeze). Decoupling magnitude from budget via an
/// admissible geometric heuristic (for STRONG avoidance) is reserved.
/// type-audit: bare-ok(count)
const REMEMBERED_PENALTY: u64 = 5;

/// The `MoveTo` edge cost into `n` given the remembered-danger set: the baseline
/// `1`, plus [`REMEMBERED_PENALTY`] when `n` is remembered-dangerous. For an
/// EMPTY `avoid` set every edge stays `1` — the byte-identity property both
/// planners share.
fn move_cost(n: &RoomAddr, avoid: &std::collections::BTreeSet<RoomAddr>) -> u64 {
    if avoid.contains(n) {
        1 + REMEMBERED_PENALTY
    } else {
        1
    }
}

/// The GOAP search space for the sustenance goal: reach water and drink.
/// type-audit: bare-ok(return)
pub struct GoapSpace<'a> {
    /// The water room the `Drink` action requires.
    pub water: RoomAddr,
    /// The remembered-dangerous cells to route around (The Haunt) — a `MoveTo`
    /// into one costs `1 + REMEMBERED_PENALTY`. Empty ⇒ byte-identical.
    pub avoid: &'a std::collections::BTreeSet<RoomAddr>,
}
impl<'a> SearchSpace for GoapSpace<'a> {
    type State = PlanState;
    type Action = Action;
    fn successors(&self, s: &PlanState) -> Vec<(Action, PlanState, u64)> {
        if s.hydrated {
            return Vec::new(); // goal reached; no need to expand
        }
        let mut out: Vec<(Action, PlanState, u64)> = s
            .position
            .neighbors()
            .into_iter()
            .map(|n| {
                let cost = move_cost(&n, self.avoid);
                (
                    Action::MoveTo(n.clone()),
                    PlanState {
                        position: n,
                        hydrated: false,
                    },
                    cost,
                )
            })
            .collect();
        if s.position == self.water {
            out.push((
                Action::Drink,
                PlanState {
                    position: s.position.clone(),
                    hydrated: true,
                },
                1,
            ));
        }
        out
    }
    fn goal(&self, s: &PlanState) -> bool {
        s.hydrated
    }
    fn heuristic(&self, _s: &PlanState) -> u64 {
        0 // Dijkstra-mode; a geometric heuristic is a followup
    }
}

/// Plan the `[move*, drink]` journey to satisfy the sustenance goal, or `None`
/// if water is unreachable within `budget`. `avoid` is the remembered-danger set
/// the A* routes around (The Haunt); pass an empty set for the memory-less path.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_water(
    from: &RoomAddr,
    water: &RoomAddr,
    budget: usize,
    avoid: &std::collections::BTreeSet<RoomAddr>,
) -> Option<Vec<Action>> {
    astar(
        &GoapSpace {
            water: water.clone(),
            avoid,
        },
        PlanState {
            position: from.clone(),
            hydrated: false,
        },
        budget,
    )
}

/// A navigation-only space (the home-return goal — no Drink): goal is arrival.
struct NavSpace<'a> {
    dest: RoomAddr,
    /// The remembered-dangerous cells to route around (The Haunt) — a `MoveTo`
    /// into one costs `1 + REMEMBERED_PENALTY`. Empty ⇒ byte-identical.
    avoid: &'a std::collections::BTreeSet<RoomAddr>,
}
impl<'a> SearchSpace for NavSpace<'a> {
    type State = RoomAddr;
    type Action = Action;
    fn successors(&self, s: &RoomAddr) -> Vec<(Action, RoomAddr, u64)> {
        s.neighbors()
            .into_iter()
            .map(|n| {
                let cost = move_cost(&n, self.avoid);
                (Action::MoveTo(n.clone()), n, cost)
            })
            .collect()
    }
    fn goal(&self, s: &RoomAddr) -> bool {
        *s == self.dest
    }
    fn heuristic(&self, _s: &RoomAddr) -> u64 {
        0
    }
}

/// Plan a pure navigation path to `dest` (the home-return goal), or `None`.
/// `avoid` is the remembered-danger set the A* routes around (The Haunt); pass
/// an empty set for the memory-less path.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_room(
    from: &RoomAddr,
    dest: &RoomAddr,
    budget: usize,
    avoid: &std::collections::BTreeSet<RoomAddr>,
) -> Option<Vec<Action>> {
    astar(
        &NavSpace {
            dest: dest.clone(),
            avoid,
        },
        from.clone(),
        budget,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{ConceptRegistry, Seed};

    /// A thin positional adapter over [`arbitrate`] for the tests (The
    /// Disposition): it packs the four loose disposition scalars into a
    /// [`Disposition`] so the many test call sites keep their explicit
    /// per-argument values without each rebuilding the struct. Production
    /// callers (`decide`/`affect_of`/the tick) construct `Disposition` directly;
    /// only the tests, which vary these values case by case, go through this.
    #[allow(clippy::too_many_arguments)]
    fn arb(
        view: &Perceived,
        home: &RoomAddr,
        drives: &[&dyn Drive],
        latency: f64,
        horizon: f64,
        helpless: bool,
        awake: bool,
        incoming: Mode,
        budget: usize,
    ) -> Resolution {
        arbitrate(
            view,
            home,
            drives,
            &Disposition {
                latency,
                horizon,
                helpless,
                awake,
            },
            incoming,
            budget,
        )
    }

    /// Commit an `agent-at` fact placing `entity` at `room` on `day`.
    fn commit_agent_at(
        ledger: &mut Ledger,
        reg: &ConceptRegistry,
        entity: EntityId,
        room: &RoomAddr,
        day: f64,
    ) {
        ledger
            .commit(agent_at_fact(entity, room, day, "test"), reg)
            .unwrap();
    }

    /// A neutral temperature niche for the thirst/belief tests, which never
    /// plant temperatures (so the thermal drive reads INFINITY → urgency 0 →
    /// inactive → byte-identical thirst behaviour). Its value is irrelevant to
    /// those tests; the thermal-drive tests build their own `warm`/`cold`
    /// niches directly.
    fn test_niche() -> ConditionResponse {
        ConditionResponse {
            optimum: 15.0,
            width: 10.0,
            devotion: 0.5,
        }
    }

    /// A registry with just `AGENT_AT` registered, for the belief-fold tests.
    fn agent_at_reg() -> ConceptRegistry {
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg
    }

    #[test]
    fn believed_water_is_none_until_the_agent_has_stood_in_water() {
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "h".into(),
        };
        // no agent-at yet -> ignorant
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            None
        );
        // stood in the water room on day 2 -> now believes it
        commit_agent_at(&mut ledger, &reg, e, &water, 2.0);
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            Some(water)
        );
    }

    #[test]
    fn believed_water_ignores_dry_rooms_the_agent_stood_in() {
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let dry = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only(std::iter::empty()); // `dry` is never fresh
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: home.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "h".into(),
        };
        commit_agent_at(&mut ledger, &reg, e, &dry, 2.0);
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            None
        );
    }

    #[test]
    fn believed_water_keeps_the_nearest_to_home_of_several_known_sources() {
        // THE MULTI-SOURCE FOLD: the agent has stood in a NEAR and a FAR water room;
        // belief is the near one (fewer hops from home).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let near = home.neighbors()[0].clone(); // 1 hop
        let far = near
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone(); // 2 hops
        let t = PlantedTerrain::fresh_only([near.clone(), far.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: near.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "h".into(),
        };
        commit_agent_at(&mut ledger, &reg, e, &far, 2.0); // discovered far first
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            Some(far.clone())
        );
        commit_agent_at(&mut ledger, &reg, e, &near, 3.0); // later discovers the nearer one
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            Some(near),
            "belief switches to the nearer known source"
        );
    }

    #[test]
    fn believed_water_only_counts_sightings_at_or_before_t() {
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "h".into(),
        };
        commit_agent_at(&mut ledger, &reg, e, &water, 9.0); // sighting in the future
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            None
        );
    }

    #[test]
    fn believed_water_is_deterministic_reload_stable_and_per_agent() {
        // BELIEF == FOLD: same ledger+t -> same value; reload-stable; another agent's
        // sightings never leak in (subject-scoped).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let other = ledger.mint_entity();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "h".into(),
        };
        commit_agent_at(&mut ledger, &reg, other, &water, 2.0); // OTHER stood in water, not e
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            None,
            "another agent's sighting does not become e's belief"
        );
        commit_agent_at(&mut ledger, &reg, e, &water, 3.0);
        let a = believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000);
        let json = serde_json::to_string(&ledger).unwrap();
        let reloaded: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(
            believed_water(&reloaded, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            a
        );
        assert_eq!(a, Some(water));
    }

    #[test]
    fn believed_water_breaks_equal_hop_ties_by_ascending_room_addr() {
        // DETERMINISM UNDER GENUINE TIES (the tie-break the reload/isolation test
        // can't reach — it never has two equal-distance candidates): two water
        // sources the SAME hop-distance from home (two neighbours, both 1 hop) must
        // resolve to the smaller-`RoomAddr` one, identically every run and across
        // reload. A nondeterministic (HashSet) accumulation would make this flaky;
        // the `BTreeSet` + `min_by((hop, RoomAddr))` fold makes it total.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let n = home.neighbors();
        let (first, second) = (n[0].clone(), n[1].clone()); // both exactly 1 hop from home
        let smaller = std::cmp::min(first.clone(), second.clone());
        let larger = std::cmp::max(first.clone(), second.clone());
        let t = PlantedTerrain::fresh_only([first.clone(), second.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: first.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "h".into(),
        };
        // Stand in the LARGER-addr source first, then the smaller — so a naive
        // "first sighting wins" would pick the larger; the tie-break must not.
        commit_agent_at(&mut ledger, &reg, e, &larger, 2.0);
        commit_agent_at(&mut ledger, &reg, e, &smaller, 3.0);
        let got = believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000);
        assert_eq!(
            got,
            Some(smaller.clone()),
            "an equal-hop tie resolves to the smaller RoomAddr, not sighting order"
        );
        let json = serde_json::to_string(&ledger).unwrap();
        let reloaded: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(
            believed_water(&reloaded, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            got,
            "the tie resolves identically after reload"
        );
    }

    /// A steady mortal NPC for the believed_hazard folds — the default mortal
    /// threat niche weights UNCANNY `1`, so a planted UNCANNY hazard reads as
    /// felt threat directly, and steady boldness (`0.5`) leaves it unscaled.
    fn haunt_npc(entity: EntityId, home: RoomAddr) -> Npc {
        Npc {
            entity,
            home: home.clone(),
            resource: home,
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "h".into(),
        }
    }

    /// Build an `Npc` with the same authored field values every belief test
    /// uses, varying only what these tests vary: entity, home, resource, and
    /// label. Mirrors the `Npc` literal repeated across the `believed_water`
    /// tests above — factored here only to keep the four-band-member Tidings
    /// tests below from repeating it four times over.
    fn shared_belief_npc(entity: EntityId, home: RoomAddr, resource: RoomAddr, label: &str) -> Npc {
        Npc {
            entity,
            home,
            resource,
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: label.into(),
        }
    }

    #[test]
    fn believed_hazard_is_empty_when_never_frightened() {
        // The empty-source form: a history over hazard-free ground shuns
        // nothing, so every planner edge stays `1` (byte-identical, the settled
        // peoples' set).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let elsewhere = raddr(-1.0);
        let t = PlantedTerrain::default(); // no hazard anywhere
        let npc = haunt_npc(e, home.clone());
        commit_agent_at(&mut ledger, &reg, e, &home, 1.0);
        commit_agent_at(&mut ledger, &reg, e, &elsewhere, 2.0);
        assert!(
            believed_hazard(&ledger, &npc, WorldTime { day: 5.0 }, &t, &[]).is_empty(),
            "a creature never frightened shuns nothing"
        );
    }

    #[test]
    fn believed_hazard_holds_the_visited_dangerous_cells() {
        // The fold ∩ frightening-truth: exactly the visited-and-dangerous cells.
        // A visited SAFE cell is absent, and an UNVISITED dangerous cell is
        // absent (the creature must have STOOD there to remember it).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0); // safe, visited ([1,0,0])
        let scary = raddr(-1.0); // UNCANNY 0.8 ≥ act, visited → shunned ([-1,0,0])
        let unvisited_scary = RoomAddr::containing([0.0, 1.0, 0.0], 6); // dangerous, never stood in ([0,1,0])
        let t = PlantedTerrain::hazard(
            std::iter::empty(),
            [(scary.clone(), 0.8), (unvisited_scary.clone(), 0.8)],
        );
        let npc = haunt_npc(e, home.clone());
        commit_agent_at(&mut ledger, &reg, e, &home, 1.0); // safe
        commit_agent_at(&mut ledger, &reg, e, &scary, 2.0); // frightened here
        let got = believed_hazard(&ledger, &npc, WorldTime { day: 5.0 }, &t, &[]);
        let expected: std::collections::BTreeSet<RoomAddr> = [scary].into_iter().collect();
        assert_eq!(
            got, expected,
            "shuns exactly the visited-and-dangerous cell"
        );
    }

    #[test]
    fn believed_hazard_is_terrain_only_with_empty_roster() {
        // Byte-identity guard: with an EMPTY roster the re-derived alarm is 0,
        // so the most-recent-visit rule over TIME-INVARIANT terrain collapses to
        // The Haunt's any-visit set — exactly the pre-Phantom behaviour, even
        // across a safe visit sandwiched between two frightened ones.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let safe = raddr(1.0);
        let scary = raddr(-1.0); // UNCANNY 0.8 ≥ act
        let t = PlantedTerrain::hazard(std::iter::empty(), [(scary.clone(), 0.8)]);
        let npc = haunt_npc(e, safe.clone());
        commit_agent_at(&mut ledger, &reg, e, &scary, 2.0); // frightened
        commit_agent_at(&mut ledger, &reg, e, &safe, 3.0); // safe
        commit_agent_at(&mut ledger, &reg, e, &scary, 4.0); // still frightened
        let got = believed_hazard(&ledger, &npc, WorldTime { day: 5.0 }, &t, &[]);
        let expected: std::collections::BTreeSet<RoomAddr> = [scary].into_iter().collect();
        assert_eq!(got, expected, "empty roster ⇒ The Haunt's any-visit set");
    }

    #[test]
    fn believed_hazard_clears_a_disproven_phantom() {
        // The staleness rule, now LIVE: a cell alarm-frightened on an early
        // visit and SAFELY revisited later is no longer shunned (the fear
        // disproved), while a creature that never revisits still shuns it (the
        // phantom, re-derived from the emitter's PAST cell).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone(); // E: frightens the emitter B
        let x = ns[1].clone(); // X: safe, in B's halo (the phantom cell)
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        // Emitter B: beside X on day 0.5, then far away by 9.5.
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        let far = raddr(-1.0);
        commit_agent_at(&mut ledger, &reg, b_e, &far, 9.5);
        // A (coward) stands at X while B is beside it (0.5), then SAFELY
        // revisits X after B is gone (9.5) — the disproof.
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);
        commit_agent_at(&mut ledger, &reg, a_e, &x, 9.5);
        // C (coward) stands at X only while B is beside it (0.5), never revisits.
        let c_e = ledger.mint_entity();
        let mut c = haunt_npc(c_e, x.clone());
        c.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, c_e, &x, 0.5);

        let now = WorldTime { day: 10.0 };
        let roster = [b.clone()];
        // A's most-recent visit to X was safe → the phantom is cleared.
        assert!(
            !believed_hazard(&ledger, &a, now, &terrain, &roster).contains(&x),
            "a safe revisit clears the disproven phantom"
        );
        // C never revisited → the phantom persists (re-derived from B's PAST
        // cell — requires the day-aware position lookup).
        assert!(
            believed_hazard(&ledger, &c, now, &terrain, &roster).contains(&x),
            "without a corrective revisit, the phantom is still shunned"
        );
    }

    #[test]
    fn shared_belief_fills_an_ignorant_colocated_creature() {
        // Two creatures share a room ("here"); `knower` has stood at `water`,
        // `lost` never has. Both homed at `here`.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let here = raddr(1.0);
        let water = here.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let knower_e = ledger.mint_entity();
        let knower = shared_belief_npc(knower_e, here.clone(), water.clone(), "knower");
        let lost_e = ledger.mint_entity();
        let lost = shared_belief_npc(lost_e, here.clone(), here.clone(), "lost");
        // knower's perception history: stood at water, now back at `here`.
        commit_agent_at(&mut ledger, &reg, knower_e, &water, 0.0);
        commit_agent_at(&mut ledger, &reg, knower_e, &here, 1.0);
        // lost has only ever been at `here`.
        commit_agent_at(&mut ledger, &reg, lost_e, &here, 1.0);
        let band = [knower.clone(), lost.clone()];
        let now = WorldTime { day: 1.0 };

        // Alone, `lost` is ignorant.
        assert_eq!(believed_water(&ledger, &lost, now, &t, 10_000), None);
        // Co-located with `knower`, it learns the water.
        assert_eq!(
            shared_believed_water(&ledger, &lost, &band, now, &t, 10_000),
            Some(water.clone())
        );
    }

    #[test]
    fn frightened_at_matches_the_danger_drive() {
        // ONE SOURCE OF TRUTH: `frightened_at` agrees with the Danger drive's own
        // reading (`urgency ≥ DANGER_ACT`, alarm-free) on the same cell — the
        // memory and the live drive never disagree about frightening ground.
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let scary = raddr(1.0); // UNCANNY 0.8 → frightened
        let mild = raddr(-1.0); // UNCANNY 0.1 → below act, not frightened
        let t = PlantedTerrain::hazard(
            std::iter::empty(),
            [(scary.clone(), 0.8), (mild.clone(), 0.1)],
        );
        let npc = haunt_npc(e, scary.clone());
        for cell in [&scary, &mild] {
            let drive = Danger {
                terrain: &t,
                threat_niche: npc.threat_niche,
                boldness: npc.boldness,
                alarm: None,
            };
            let drive_afraid = drive.urgency(&view_at(cell.clone())) >= DANGER_ACT;
            assert_eq!(
                frightened_at(cell, &npc, &t, WorldTime { day: 0.0 }, &[], &ledger),
                drive_afraid,
                "frightened_at agrees with the Danger drive at {cell:?}"
            );
        }
    }

    #[test]
    fn frightened_at_is_terrain_only_with_empty_roster() {
        // The recursion base case / seed-42 fast path: with an EMPTY roster the
        // re-derived alarm is 0 at EVERY day, so `frightened_at` collapses to
        // The Haunt's terrain-only verdict — the byte-identity guard.
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let scary = raddr(1.0); // UNCANNY 0.8 → frightened
        let mild = raddr(-1.0); // UNCANNY 0.1 → below act
        let t = PlantedTerrain::hazard(
            std::iter::empty(),
            [(scary.clone(), 0.8), (mild.clone(), 0.1)],
        );
        let npc = haunt_npc(e, scary.clone());
        for day in [0.0, 5.0, 100.0] {
            assert!(
                frightened_at(&scary, &npc, &t, WorldTime { day }, &[], &ledger),
                "the scary cell frightens terrain-only on day {day}"
            );
            assert!(
                !frightened_at(&mild, &npc, &t, WorldTime { day }, &[], &ledger),
                "the mild cell never frightens on day {day}"
            );
        }
    }

    #[test]
    fn frightened_at_fires_on_re_derived_past_alarm() {
        // A primary-afraid emitter B stands on ground whose hazard (E, one hop
        // from B) makes B's own Danger cross act; B's one-hop alarm halo covers
        // a SAFE cell X (two hops from the hazard, terrain-safe). The re-derived
        // alarm at (X, day) pushes a coward rememberer over act — though the
        // same cell read terrain-only (empty roster) is calm. And it re-derives
        // B's PAST position: though B later walks far off, `frightened_at` at
        // `day` still fires (agent_position honours the remembered day).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0); // where B stands (safe)
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone(); // E: the hazard that frightens B
        let x = ns[1].clone(); // X: safe, in B's halo, two hops from E
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        // B: a steady emitter, committed at D on `day`, then walks far LATER.
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        let far = raddr(-1.0);
        commit_agent_at(&mut ledger, &reg, b_e, &far, 9.5);
        // A: a coward rememberer (feels borrowed alarm strongly).
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        let day = WorldTime { day: 0.5 };
        // X read terrain-only (empty roster) is safe.
        assert!(
            !frightened_at(&x, &a, &terrain, day, &[], &ledger),
            "X read terrain-only is safe"
        );
        // With the roster, the re-derived PAST alarm at X frightens the coward.
        assert!(
            frightened_at(&x, &a, &terrain, day, std::slice::from_ref(&b), &ledger),
            "the re-derived alarm at (X, day) frightens the coward rememberer"
        );
    }

    #[test]
    fn frightened_at_is_false_after_the_alarm_passes() {
        // Same geometry; at a LATER day the emitter has walked off, so the
        // re-derived alarm at X is gone and the coward is calm there — the
        // memory's time-awareness (the alarm as it WAS on `day`, not as it
        // lingers forever).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone();
        let x = ns[1].clone();
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        let far = raddr(-1.0);
        commit_agent_at(&mut ledger, &reg, b_e, &far, 9.5);
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        // At `day` the alarm is present (guard: the setup really does frighten).
        assert!(
            frightened_at(
                &x,
                &a,
                &terrain,
                WorldTime { day: 0.5 },
                std::slice::from_ref(&b),
                &ledger
            ),
            "guard: X is alarmed while B stands beside it"
        );
        // At the later day B has moved off → no alarm at X → calm.
        assert!(
            !frightened_at(
                &x,
                &a,
                &terrain,
                WorldTime { day: 9.5 },
                std::slice::from_ref(&b),
                &ledger
            ),
            "after B leaves, X carries no re-derived alarm"
        );
    }

    #[test]
    fn shared_belief_is_order_independent() {
        // Same setup as `shared_belief_fills_an_ignorant_colocated_creature`;
        // permuting the band must not change the result.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let here = raddr(1.0);
        let water = here.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let knower_e = ledger.mint_entity();
        let knower = shared_belief_npc(knower_e, here.clone(), water.clone(), "knower");
        let lost_e = ledger.mint_entity();
        let lost = shared_belief_npc(lost_e, here.clone(), here.clone(), "lost");
        commit_agent_at(&mut ledger, &reg, knower_e, &water, 0.0);
        commit_agent_at(&mut ledger, &reg, knower_e, &here, 1.0);
        commit_agent_at(&mut ledger, &reg, lost_e, &here, 1.0);
        let now = WorldTime { day: 1.0 };
        let ab = [knower.clone(), lost.clone()];
        let ba = [lost.clone(), knower.clone()];
        let result = shared_believed_water(&ledger, &lost, &ab, now, &t, 10_000);
        assert_eq!(result, Some(water));
        assert_eq!(
            result,
            shared_believed_water(&ledger, &lost, &ba, now, &t, 10_000),
            "permuting the band must not change the pooled belief"
        );
    }

    #[test]
    fn shared_belief_is_a_noop_when_alone_or_band_empty() {
        // A lone knower's shared belief equals its own belief; an empty band,
        // and a band whose only co-located member is `knower` itself, are
        // both no-ops (the strict-generalization contract).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let here = raddr(1.0);
        let water = here.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let knower_e = ledger.mint_entity();
        let knower = shared_belief_npc(knower_e, here.clone(), water.clone(), "knower");
        commit_agent_at(&mut ledger, &reg, knower_e, &water, 0.0);
        commit_agent_at(&mut ledger, &reg, knower_e, &here, 1.0);
        let now = WorldTime { day: 1.0 };
        let solo = believed_water(&ledger, &knower, now, &t, 10_000);
        assert_eq!(solo, Some(water));

        assert_eq!(
            shared_believed_water(&ledger, &knower, &[], now, &t, 10_000),
            solo,
            "an empty band changes nothing"
        );
        assert_eq!(
            shared_believed_water(
                &ledger,
                &knower,
                std::slice::from_ref(&knower),
                now,
                &t,
                10_000
            ),
            solo,
            "a band of only itself changes nothing"
        );
    }

    #[test]
    fn shared_belief_ignores_a_knower_in_a_different_room() {
        // `knower` knows water but currently stands in a DIFFERENT room than
        // `lost` -> no share.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let here = raddr(1.0);
        let neighbors = here.neighbors();
        let water = neighbors[0].clone();
        let elsewhere = neighbors[1].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let knower_e = ledger.mint_entity();
        let knower = shared_belief_npc(knower_e, here.clone(), water.clone(), "knower");
        let lost_e = ledger.mint_entity();
        let lost = shared_belief_npc(lost_e, here.clone(), here.clone(), "lost");
        // knower has stood at water (knows it) but its LATEST position is
        // `elsewhere`, not `here`.
        commit_agent_at(&mut ledger, &reg, knower_e, &water, 0.0);
        commit_agent_at(&mut ledger, &reg, knower_e, &elsewhere, 1.0);
        // lost stands at `here`.
        commit_agent_at(&mut ledger, &reg, lost_e, &here, 1.0);
        let band = [knower.clone(), lost.clone()];
        let now = WorldTime { day: 1.0 };

        // sanity: knower does know water when consulted directly...
        assert_eq!(
            believed_water(&ledger, &knower, now, &t, 10_000),
            Some(water)
        );
        // ...but lost gains nothing, since knower is in a different room.
        assert_eq!(
            shared_believed_water(&ledger, &lost, &band, now, &t, 10_000),
            None
        );
    }

    #[test]
    fn a_colocated_lost_creature_moves_toward_shared_water() {
        // THE TIDINGS, WIRED INTO THE MOVER: a `knower` and a `lost` creature
        // share `here` (both homed there too). `knower` has genuinely stood
        // at `water` (two hops away, through `n1`); `lost` never has. Water
        // sits behind `n1`, whose OWN elevation is left unset (INFINITY);
        // `here`'s other two neighbors (`n0`, `n2`) are planted at elevation
        // 0.0 — the lowest-unvisited-neighbor explorer therefore ALWAYS
        // prefers them over `n1`, and once `home` (already visited) blocks
        // the way back, blind exploration can structurally never reach `n1`
        // (let alone `water` beyond it). So under the OLD (non-shared)
        // belief seed, `lost` — ignorant — can never drink: it wanders
        // through `n0`/`n2` and their own subtrees, never through `n1`.
        // Under the shared law, `lost` inherits `knower`'s belief the moment
        // they're co-located and A*-steps straight through `n1` to `water`,
        // drinking there. A real, mechanism-tied assertion (not just "moved
        // somewhere") — mutation-verify: reverting the seed swap reds this.
        let reg = {
            let mut r = agent_at_reg();
            r.register_predicate(DRANK, false, "drank").unwrap();
            r.register_predicate(RESTED, false, "rested").unwrap();
            r.register_predicate(EATEN, false, "eaten").unwrap();
            r
        };
        let mut ledger = Ledger::default();
        let here = raddr(1.0);
        let neighbors = here.neighbors();
        let n0 = neighbors[0].clone();
        let n1 = neighbors[1].clone();
        let n2 = neighbors[2].clone();
        let water = n1
            .neighbors()
            .into_iter()
            .find(|r| *r != here)
            .expect("n1 has a neighbor other than home");
        let t = PlantedTerrain {
            elevations: [(n0.clone(), 0.0), (n2.clone(), 0.0)].into_iter().collect(),
            fresh: [water.clone()].into_iter().collect(),
            temps: std::collections::BTreeMap::new(),
            forage: std::collections::BTreeMap::new(),
            threat: std::collections::BTreeMap::new(),
            prey: std::collections::BTreeMap::new(),
        };
        let knower_e = ledger.mint_entity();
        let knower = shared_belief_npc(knower_e, here.clone(), water.clone(), "knower");
        let lost_e = ledger.mint_entity();
        let lost = shared_belief_npc(lost_e, here.clone(), here.clone(), "lost");
        // knower's perception history: stood at water (day 0), back at `here`
        // by day 1 — same shape as the pure-`shared_believed_water` tests.
        commit_agent_at(&mut ledger, &reg, knower_e, &water, 0.0);
        commit_agent_at(&mut ledger, &reg, knower_e, &here, 1.0);
        // lost has only ever been at `here`.
        commit_agent_at(&mut ledger, &reg, lost_e, &here, 1.0);

        let sys = DriveMovements {
            npcs: vec![knower.clone(), lost.clone()],
            from: WorldTime { day: 1.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            terrain: &t,
        };
        let next =
            hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).expect("tick");
        let lost_drank = next.find(DRANK).filter(|f| f.subject == lost_e).count();
        assert!(
            lost_drank >= 1,
            "shared belief should have carried the lost creature to the water \
             its co-located band-mate knew, but it never drank"
        );
    }

    #[test]
    fn derive_npcs_are_distinct_and_placed() {
        // Use the real worldgen build for a populated world:
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap();
        let ctx = LocaleContext::build(&world).unwrap();
        let mut ledger = world.ledger.clone();
        let home = hornvale_settlement::village_info(&world).unwrap().id;
        let npcs = derive_npcs(&world, &ctx, &mut ledger, 3, home);
        assert_eq!(npcs.len(), 3);
        // distinct entities, and each has a VALID resource anchor: `derive_npcs`
        // resolves it as `nearest_water(home, ..., PLAN_BUDGET).unwrap_or(home)`
        // (against the WORLD's own derived sea level, via the same
        // `LocaleTerrain` adapter a live session uses) — so `resource` is either
        // a room that genuinely reads as water, OR the unreachable-water
        // fallback to `home` itself. `resource == home` is therefore NOT by
        // itself a bug (the old `resource_room` guaranteed a distinct
        // neighbour regardless of whether it was real water; `nearest_water`
        // makes no such promise — not every settlement has water within
        // budget, and that's a real, legitimate outcome, not a derivation
        // failure). What must hold is the disjunction below.
        let ids: std::collections::BTreeSet<_> = npcs.iter().map(|n| n.entity).collect();
        assert_eq!(ids.len(), 3);
        let terrain = LocaleTerrain::new(&ctx);
        for n in &npcs {
            assert!(
                is_water(&n.resource, &terrain) || n.resource == n.home,
                "NPC {}'s resource {:?} must be either real water or the home fallback",
                n.label,
                n.resource
            );
        }
    }

    #[test]
    fn derive_wild_npcs_mint_beast_agents_with_defaulted_psyche() {
        // THE WILDING: the wild roster is minted from the world's beast
        // concentrations, NOT its peoples. A beast is, by construction, a
        // species whose `social_form` is not `Settled` (`wild_concentrations`'s
        // `is_mobile_beast`). On today's seed-42 roster every such wild kind
        // also carries no `psyche_registry` entry, so every wild NPC takes the
        // DEFAULT psyche dials — steady boldness, mid latency/horizon — while
        // its threat niche is still derived from its biosphere nature (so a
        // herbivore fears predators). This is the peopled `derive_npcs` path's
        // mirror for fauna. NOTE: the defaulted-psyche assertion below holds
        // for these seed-42 wild kinds because they happen to carry no psyche
        // entry — it is not a claim that every wild (non-`Settled`) creature
        // lacks one; a placed dragon (Task 4) is `Solitary` yet carries an
        // authored mind.
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap();
        let ctx = LocaleContext::build(&world).unwrap();
        let mut ledger = world.ledger.clone();
        let wild = derive_wild_npcs(&world, &ctx, &mut ledger, 4);
        assert!(
            !wild.is_empty() && wild.len() <= 4,
            "seed 42 mints between 1 and 4 wild beasts, got {}",
            wild.len()
        );
        let biosphere = hornvale_species::biosphere_registry();
        for n in &wild {
            assert!(
                n.label.starts_with("a wild "),
                "a wild NPC reads as a beast: {}",
                n.label
            );
            let social_form = biosphere
                .get_by_label(&n.species)
                .unwrap_or_else(|| panic!("{} has a biosphere entry", n.species))
                .social_form;
            assert!(
                social_form != hornvale_species::SocialForm::Settled,
                "a wild species is wild (not Settled): {}",
                n.species
            );
            // Beast → defaulted psyche (no registry entry to read; see the
            // NOTE above the loop for the scope of this claim).
            assert_eq!(
                n.boldness, BOLDNESS_STEADY,
                "{} takes steady boldness",
                n.species
            );
            assert_eq!(
                n.deliberation_latency, 0.5,
                "{} takes mid latency",
                n.species
            );
            assert_eq!(n.time_horizon, 0.5, "{} takes mid horizon", n.species);
            assert!(
                (0.0..=1.0).contains(&n.threat_niche.predator),
                "{}'s predator threat weight is a valid ratio: {}",
                n.species,
                n.threat_niche.predator
            );
        }
        // At least one is a vulnerable herbivore that meaningfully fears
        // predator ground — The Quarry's threat niche, live for fauna.
        assert!(
            wild.iter().any(|n| n.threat_niche.predator > 0.3),
            "seed 42's wild roster includes a predator-fearing herbivore"
        );
        // Deterministic: the same world mints the same beast roster.
        let mut ledger2 = world.ledger.clone();
        let wild2 = derive_wild_npcs(&world, &ctx, &mut ledger2, 4);
        let species: Vec<&str> = wild.iter().map(|n| n.species.as_str()).collect();
        let species2: Vec<&str> = wild2.iter().map(|n| n.species.as_str()).collect();
        assert_eq!(species, species2, "the wild roster is deterministic");
    }

    #[test]
    fn seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding() {
        // THE CONFLUENCE'S PAYOFF, MEASURED NOT ASSUMED: the earlier pinned
        // finding here (see git history) measured that seed 42's possessed
        // home settlement was topologically stuck — the greedy-downhill,
        // never-revisit-within-a-call exploration walked 2,592 rooms over an
        // enormous 100,000-day wait and never reached fresh water, boxed in
        // by a riverless drainage basin. That was a settlement-PLACEMENT
        // fact, not a belief-mechanism bug: the settlement itself sat off
        // the river network.
        //
        // The Confluence re-points the carrying-capacity freshwater term at
        // real proximity to `WaterKind::River` cells, so settlements now
        // condense onto/adjacent-to rivers (measured: seed 42 fraction
        // within reach 0.7222, up from a pre-Confluence baseline nowhere
        // close). Re-measuring this exact settlement (same accessor,
        // `village_info`, on the post-Confluence world — the campaign moved
        // WHERE settlements land, so "home" now names a different site) with
        // the identical real mechanism:
        //
        //   seed 42 home settlement: 0 exploration move(s) over an enormous
        //   wait, 2 drink(s).
        //
        // Zero moves means the home settlement's own room now reads as
        // fresh water directly (`is_water` true at spawn) — no discovery
        // walk is even needed; the agent drinks in place. Checked against
        // `derive_npcs`'s real selection (this settlement plus its two
        // next-most-populous neighbors — the actual three NPCs a
        // `possess --seed 42` session derives): ALL THREE now read 0 moves,
        // 2 drinks — the condensation pulled every one of them onto water,
        // not just the lucky one. This is the campaign's visible payoff:
        // The Surmise's parked "can't reach water" finding is resolved by
        // fixing WHERE towns are, not by making agents smarter.
        //
        // The general exploration-policy gap this pin used to document
        // (a walker that can box itself into an unvisited-but-connected
        // basin) is not disproven by this result — it's just no longer
        // triggered by seed 42's home settlement. It remains a real,
        // out-of-scope gap for settlements condensation still leaves off a
        // river (decision-ledger followup #2), and the coarse-cell vs.
        // walk-depth resolution bridge (followup #1) is a related, separate
        // concern this measurement does not exercise (0 moves means the
        // coarse cell itself already reads as water at walk depth too).
        //
        // MEASURED, ALSO SURPRISING: only 2 drinks register over the
        // 100,000-day wait, not the thousands a ~5.667-day drive cycle would
        // suggest. Traced (debug prints, not left in): the zero-distance
        // on-water case is new — no prior settlement ever landed exactly ON
        // its own water source, so this is the first time the closed-form
        // `Hold` jump (`next_act = last_drank + act/rise`) and a drink cycle
        // of the exact same length interact at THIS boundary. By the third
        // cycle, `last_drank + act/rise` lands (floating-point rounding) a
        // hair BELOW `act` when read back as `drive`, so `decide` sees
        // "not yet thirsty" and re-derives an IDENTICAL `next_act`, which
        // trips the strict-progress guard (`next_act <= day`) and ends the
        // tick. This is not this campaign's regression (the guard predates
        // it, guarding a different case — a genuinely unreachable plan
        // recomputing the same Hold forever) and does not weaken the
        // payoff (2 confirmed drinks is already `>= 1`, and no real
        // `possess` session ever `wait`s 100,000 days) — but it is a real,
        // newly-exposed quirk in the on-water zero-distance path, captured
        // as a followup rather than silently absorbed.
        let mut world_reg = hornvale_kernel::ConceptRegistry::default();
        world_reg
            .register_predicate(AGENT_AT, false, "pos")
            .unwrap();
        world_reg.register_predicate(DRANK, false, "drank").unwrap();
        world_reg
            .register_predicate(RESTED, false, "rested")
            .unwrap();
        world_reg.register_predicate(EATEN, false, "eaten").unwrap();
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap();
        let ctx = LocaleContext::build(&world).unwrap();
        let terrain = LocaleTerrain::new(&ctx);
        let home_id = hornvale_settlement::village_info(&world).unwrap().id;
        let home = settlement_room(&world, &ctx, home_id);
        let npc = Npc {
            entity: EntityId::new(1).unwrap(),
            home: home.clone(),
            resource: home.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "measure".into(),
        };
        let ledger = Ledger::default();
        let sys = DriveMovements {
            npcs: vec![npc.clone()],
            from: WorldTime { day: 0.0 },
            // Deliberately enormous: rules out "it just needed a longer
            // wait" — a real session's `wait` would never span this.
            to: WorldTime { day: 100_000.0 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let next =
            hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &world_reg).unwrap();
        let moves = next
            .find(AGENT_AT)
            .filter(|f| f.subject == npc.entity)
            .count();
        let drinks = next.find(DRANK).filter(|f| f.subject == npc.entity).count();
        println!(
            "seed 42 home settlement: {moves} exploration move(s) over an \
             enormous wait, {drinks} drink(s)"
        );
        assert!(
            drinks >= 1,
            "THE CONFLUENCE'S PAYOFF (update this assertion AND the doc \
             comment above together if it regresses — don't just delete \
             it): the possessed agent's own home settlement's NPC must \
             reach fresh water on real seed 42 now that settlement \
             condensation pulls towns onto the river network (see the doc \
             comment above for the measured before/after); got {drinks} \
             drink(s) over the wait"
        );
        assert_eq!(
            moves, 0,
            "measured finding: the home settlement's own room now reads as \
             fresh water directly, so no discovery walk is needed — update \
             this pin (and the doc comment) if a future settlement-position \
             change makes this settlement's water a real walk rather than \
             immediate"
        );
    }

    #[test]
    fn derive_npcs_actually_includes_the_home_settlement() {
        // An end-to-end smoke check on a real world (seed 42): the possessed
        // agent's own settlement is among the derived NPCs even at k=1. (The
        // precise "regardless of population rank" guarantee is proven
        // adversarially, independent of any one seed's incidental population
        // distribution, by `ordered_for_derivation_prioritizes_home_over_population_rank`
        // below.)
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap();
        let ctx = LocaleContext::build(&world).unwrap();
        let mut ledger = world.ledger.clone();
        let home = hornvale_settlement::village_info(&world).unwrap().id;
        let npcs = derive_npcs(&world, &ctx, &mut ledger, 1, home);
        assert_eq!(npcs.len(), 1);
        let want_home_room = settlement_room(&world, &ctx, home);
        assert_eq!(
            npcs[0].home, want_home_room,
            "the possessed agent's own settlement's NPC must be derived"
        );
    }

    #[test]
    fn ordered_for_derivation_prioritizes_home_over_population_rank() {
        // THE COLOCATION GUARANTEE (T3 review), proven directly on the pure
        // selection function with adversarial data: a home settlement with
        // the LOWEST population must still land first, ahead of settlements
        // with far larger populations — otherwise, with k smaller than the
        // settlement count, no NPC could ever be co-located with the player
        // and the observation payoff would never fire.
        let home_id = EntityId::new(5).unwrap();
        let settlements = vec![
            hornvale_settlement::VillageInfo {
                id: EntityId::new(1).unwrap(),
                name: "Big".to_string(),
                population: 10_000,
            },
            hornvale_settlement::VillageInfo {
                id: EntityId::new(2).unwrap(),
                name: "Bigger".to_string(),
                population: 20_000,
            },
            hornvale_settlement::VillageInfo {
                id: home_id,
                name: "Home".to_string(),
                population: 1,
            },
        ];
        let ordered = ordered_for_derivation(settlements, home_id);
        assert_eq!(
            ordered[0].id, home_id,
            "the home settlement must be first regardless of its population rank"
        );
        // Truncating to k=1 (the adversarial case) must still keep it.
        let mut truncated = ordered;
        truncated.truncate(1);
        assert_eq!(truncated[0].id, home_id);
    }

    #[test]
    fn room_text_round_trips() {
        let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
        let dest = home.neighbors()[0].clone();
        for r in [home, dest] {
            assert_eq!(room_from_text(&room_to_text(&r)), r);
        }
    }

    #[test]
    fn drive_folds_drank_events_rising_since_the_last_drink() {
        // drive = rise * (t - last_drank_day), clamped [0,1]; last_drank = latest DRANK day.
        let p = SUSTENANCE;
        let home = raddr(1.0);
        let terrain = PlantedTerrain::thermal([]);
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();
        let e = ledger.mint_entity();
        // no drank yet: rises from day 0
        assert!(
            (drive_at(
                &ledger,
                e,
                &home,
                WorldTime { day: 2.0 },
                &p,
                &terrain,
                MetabolicClass::Endotherm
            ) - (p.rise * 2.0))
                .abs()
                < 1e-9
        );
        // drank on day 5 -> resets; by day 6 it has risen rise*1
        ledger
            .commit(
                hornvale_kernel::Fact {
                    subject: e,
                    predicate: DRANK.to_string(),
                    object: Value::Flag(true),
                    place: None,
                    day: Some(5.0),
                    provenance: "t".into(),
                },
                &reg,
            )
            .unwrap();
        assert!(
            (drive_at(
                &ledger,
                e,
                &home,
                WorldTime { day: 6.0 },
                &p,
                &terrain,
                MetabolicClass::Endotherm
            ) - (p.rise * 1.0))
                .abs()
                < 1e-9
        );
    }

    #[test]
    fn drive_at_clamps_at_one_and_ignores_other_entities_drank_events() {
        let p = SUSTENANCE;
        let home = raddr(1.0);
        let terrain = PlantedTerrain::thermal([]);
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();
        let e = ledger.mint_entity();
        let other = ledger.mint_entity();
        // Another entity's drink must not affect `e`'s drive (subject-scoped fold).
        ledger
            .commit(
                hornvale_kernel::Fact {
                    subject: other,
                    predicate: DRANK.to_string(),
                    object: Value::Flag(true),
                    place: None,
                    day: Some(1.0),
                    provenance: "t".into(),
                },
                &reg,
            )
            .unwrap();
        assert_eq!(
            drive_at(
                &ledger,
                e,
                &home,
                WorldTime { day: 1_000.0 },
                &p,
                &terrain,
                MetabolicClass::Endotherm
            ),
            1.0
        );
    }

    #[test]
    fn rise_at_couples_heat_to_thirst_per_metabolic_class() {
        use MetabolicClass::*;
        let p = SUSTENANCE;
        let base = p.rise;
        // Endotherm — heat-only (sweating): base at/below thermoneutral,
        // accelerating above.
        assert!((rise_at(THERMONEUTRAL_C, Endotherm, &p) - base).abs() < 1e-12);
        assert!(
            (rise_at(0.0, Endotherm, &p) - base).abs() < 1e-12,
            "cold does not slow an endotherm"
        );
        assert!(
            (rise_at(THERMONEUTRAL_C + HEAT_SCALE_C, Endotherm, &p)
                - base * (1.0 + ENDOTHERM_HEAT_K))
                .abs()
                < 1e-12,
            "one scale above thermoneutral applies the full multiplier"
        );
        // Ectotherm — symmetric (rate tracks ambient, CAP-1), floored.
        assert!(
            rise_at(THERMONEUTRAL_C + HEAT_SCALE_C, Ectotherm, &p)
                > rise_at(THERMONEUTRAL_C, Ectotherm, &p),
            "heat speeds an ectotherm"
        );
        assert!(
            rise_at(-100.0, Ectotherm, &p) < base,
            "deep cold slows an ectotherm below base (torpor)"
        );
        assert!(
            (rise_at(-100.0, Ectotherm, &p) - base * ECTOTHERM_FLOOR).abs() < 1e-12,
            "but never below the floor"
        );
        // Autotroph flat; an unreadable cell couples as neutral.
        assert!((rise_at(80.0, Autotroph, &p) - base).abs() < 1e-12);
        assert!((rise_at(f64::INFINITY, Endotherm, &p) - base).abs() < 1e-12);
    }

    #[test]
    fn thirst_integrates_faster_over_a_hot_occupancy() {
        // The path integral (The Kindling): the same elapsed time accrues more
        // thirst in a hot cell than a temperate one.
        let p = SUSTENANCE;
        let home = raddr(1.0);
        let hot = PlantedTerrain::thermal([(home.clone(), 45.0)]); // 2× rate (endotherm)
        let temperate = PlantedTerrain::thermal([(home.clone(), 20.0)]); // < thermoneutral → base
        let d_hot = integrate_thirst(&[], &home, 0.0, 3.0, &hot, MetabolicClass::Endotherm, &p);
        let d_temp = integrate_thirst(
            &[],
            &home,
            0.0,
            3.0,
            &temperate,
            MetabolicClass::Endotherm,
            &p,
        );
        assert!(
            d_hot > d_temp,
            "the desert dehydrates faster: {d_hot} vs {d_temp}"
        );
        // A temperate (sub-thermoneutral) cell recovers the old flat model.
        assert!((d_temp - p.rise * 3.0).abs() < 1e-9);
        // And the desert is exactly the doubled rate here.
        assert!((d_hot - p.rise * 2.0 * 3.0).abs() < 1e-9);
    }

    #[test]
    fn drive_at_is_deterministic_and_reload_stable() {
        // Fold determinism: same ledger + t -> same value; and serialize->reload of
        // the ledger yields the identical drive (the DRIVE == FOLD contract).
        let p = SUSTENANCE;
        let home = raddr(1.0);
        let terrain = PlantedTerrain::thermal([]);
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();
        let e = ledger.mint_entity();
        for day in [1.0, 4.0, 9.0] {
            ledger
                .commit(
                    hornvale_kernel::Fact {
                        subject: e,
                        predicate: DRANK.to_string(),
                        object: Value::Flag(true),
                        place: None,
                        day: Some(day),
                        provenance: "t".into(),
                    },
                    &reg,
                )
                .unwrap();
        }
        let t = WorldTime { day: 12.3 };
        let a = drive_at(
            &ledger,
            e,
            &home,
            t,
            &p,
            &terrain,
            MetabolicClass::Endotherm,
        );
        let b = drive_at(
            &ledger,
            e,
            &home,
            t,
            &p,
            &terrain,
            MetabolicClass::Endotherm,
        );
        assert_eq!(a, b);
        let json = serde_json::to_string(&ledger).unwrap();
        let reloaded: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(
            drive_at(
                &reloaded,
                e,
                &home,
                t,
                &p,
                &terrain,
                MetabolicClass::Endotherm
            ),
            a,
            "drive re-derives identically after reload"
        );
    }

    fn addr(seed: f64) -> RoomAddr {
        RoomAddr::containing([seed, 0.0, 0.0], 6)
    }

    #[test]
    fn decide_plans_to_water_when_thirsty_and_home_when_not() {
        let p = SUSTENANCE;
        let home = addr(1.0);
        let water = home.neighbors()[0].clone();
        // parched (drive >= act), at home, KNOWS water -> the plan's first
        // step, toward water
        let v = Perceived {
            position: home.clone(),
            drive: 0.9,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert_eq!(
            decide(&v, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(water.clone()))
        );
        // not thirsty, away from home (at water) -> the plan's first step home
        let v = Perceived {
            position: water.clone(),
            drive: 0.1,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert_eq!(
            decide(&v, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(home.clone()))
        );
        // not thirsty, at home -> nothing to do
        let v = Perceived {
            position: home.clone(),
            drive: 0.1,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert_eq!(decide(&v, &home, &p, 10_000), Intent::Hold);
    }

    #[test]
    fn decide_holds_when_the_plan_is_unreachable_within_budget() {
        // A zero search budget can never find even a one-step plan: both the
        // thirsty-and-knows-water (plan-to-water) and homeward (plan-to-room)
        // branches must give up rather than loop.
        let p = SUSTENANCE;
        let home = addr(1.0);
        let water = home.neighbors()[0].clone();
        let thirsty = Perceived {
            position: home.clone(),
            drive: 0.9,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert_eq!(decide(&thirsty, &home, &p, 0), Intent::Hold);
        let away_not_thirsty = Perceived {
            position: water.clone(),
            drive: 0.1,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert_eq!(decide(&away_not_thirsty, &home, &p, 0), Intent::Hold);
    }

    fn raddr(seed: f64) -> RoomAddr {
        RoomAddr::containing([seed, 0.0, 0.0], 6)
    }

    #[test]
    fn decide_plans_to_believed_water_or_explores_when_ignorant() {
        // BELIEF DRIVES THE DECISION: two views identical but for belief produce
        // different first moves — the believer A*-steps toward its known water; the
        // ignorant one takes the explore step. (Water two hops away so the A* first
        // step differs from an arbitrary explore step.)
        let p = SUSTENANCE;
        let home = raddr(1.0);
        let mid = home.neighbors()[0].clone();
        let water = mid
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone();
        let explore = home.neighbors()[2].clone(); // a different direction
        // believer, thirsty, at home -> first A* step toward water (== mid)
        let believer = Perceived {
            position: home.clone(),
            drive: 0.9,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: Some(explore.clone()),
        };
        assert_eq!(
            decide(&believer, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(mid.clone()))
        );
        // ignorant, thirsty, at home -> the explore step (not toward water)
        let ignorant = Perceived {
            position: home.clone(),
            drive: 0.9,
            fatigue: 0.0,
            believed_water: None,
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: Some(explore.clone()),
        };
        assert_eq!(
            decide(&ignorant, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(explore.clone()))
        );
        assert_ne!(
            mid, explore,
            "the two beliefs must yield different moves for this to prove anything"
        );
        // ignorant with nowhere new to explore -> Hold
        let stuck = Perceived {
            position: home.clone(),
            drive: 0.9,
            fatigue: 0.0,
            believed_water: None,
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert_eq!(decide(&stuck, &home, &p, 10_000), Intent::Hold);
        // not thirsty, away from home -> plan home (unchanged behavior)
        let sated_away = Perceived {
            position: water.clone(),
            drive: 0.1,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert!(matches!(
            decide(&sated_away, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(_))
        ));
    }

    #[test]
    fn a_thirsty_agent_plans_to_water_and_the_tick_walks_it() {
        // Over a wait long enough to grow thirsty, the tick commits a run of agent-at
        // moves ending at water plus a `drank`, and the drive resets. Under the
        // belief model the agent starts IGNORANT: its first approach to water
        // is an EXPLORE step (not a ground-truth beeline) — water is planted
        // as home's only low neighbour, so exploration discovers it on the
        // very first thirsty step; belief then persists (the fold across the
        // walk) so every later cycle A*-steps to it directly.
        let mut world_reg = hornvale_kernel::ConceptRegistry::default();
        world_reg
            .register_predicate(AGENT_AT, false, "pos")
            .unwrap();
        world_reg.register_predicate(DRANK, false, "drank").unwrap();
        world_reg
            .register_predicate(RESTED, false, "rested")
            .unwrap();
        world_reg.register_predicate(EATEN, false, "eaten").unwrap();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "herder".into(),
        };
        // Elevation still steers the exploration prior (downhill), separate
        // from fresh-water truth: `water` must be the uniquely lowest
        // neighbor for the comment above's "very first thirsty step"
        // guarantee to hold deterministically (not by RoomAddr tie-break
        // luck among equally-INFINITY neighbors).
        let t = PlantedTerrain {
            elevations: [(water.clone(), 0.0)].into_iter().collect(),
            fresh: [water.clone()].into_iter().collect(),
            ..Default::default()
        };
        let sys = DriveMovements {
            npcs: vec![npc.clone()],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            terrain: &t,
        };
        let next =
            hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &world_reg).unwrap();
        // At least one drank committed (the agent reached water and drank).
        let drank_count = next.find(DRANK).filter(|f| f.subject == e).count();
        assert!(drank_count >= 1, "the agent drank");
        // agent-at moves committed (the journey), and bounded (not one per
        // tick / not exploding — THE NO-THRASH GUARD, the campaign's
        // characteristic risk): a ~5.7-day rise cycle over 40 days is ~7
        // cycles, each a small, fixed number of moves.
        let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
        assert!(moves >= 1, "the agent walked");
        assert!(
            moves <= 60,
            "expected a bounded number of moves, not one per tick or an explosion; got {moves}"
        );
        let _ = ledger;
    }

    #[test]
    fn the_recount_surfaces_the_drives_own_provenance_for_the_full_round_trip() {
        // THE PROVENANCE READ, DECOUPLED FROM REAL-WORLD REACHABILITY
        // (the-surmise T4 review): `why`'s recount
        // (`hornvale_historiography::recount`) must surface the drive's own
        // reason for a move AND the drink that satisfied it — proven here
        // directly against `DriveMovements`'s committed facts on PLANTED
        // (guaranteed-one-hop) terrain — a deterministic, seed-independent
        // proof of the mechanism, orthogonal to
        // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`'s
        // measurement of the real seed-42 world's own settlement/water
        // placement.
        // Mutation-verify: blanking `agent_at_fact`'s "went down to the
        // river it knew (thirst)" string, or `drank_fact`'s "drank from the
        // river (thirst sated)" string, reds ONE of the two assertions
        // below without touching the other.
        // (This test predates The Freshet re-wire; kept on planted terrain
        // deliberately — the mechanism-level provenance read should not
        // depend on any one real seed's fresh-water placement.)
        let mut world = World::new(Seed(0));
        world
            .registry
            .register_predicate(AGENT_AT, false, "pos")
            .unwrap();
        world
            .registry
            .register_predicate(DRANK, false, "drank")
            .unwrap();
        world
            .registry
            .register_predicate(RESTED, false, "rested")
            .unwrap();
        world
            .registry
            .register_predicate(EATEN, false, "eaten")
            .unwrap();
        let entity = world.ledger.mint_entity();
        world
            .ledger
            .commit(
                Fact {
                    subject: entity,
                    predicate: hornvale_kernel::NAME.to_string(),
                    object: Value::Text("herder".to_string()),
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &world.registry,
            )
            .unwrap();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let npc = Npc {
            entity,
            home: home.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "herder".into(),
        };
        // Elevation still steers the exploration prior (downhill), separate
        // from fresh-water truth: `water` must be the uniquely lowest
        // neighbor so the first (ignorant) thirsty cycle explores directly
        // onto it, letting a later cycle's believer beeline actually fire
        // "went down to the river it knew (thirst)" below.
        let t = PlantedTerrain {
            elevations: [(water.clone(), 0.0)].into_iter().collect(),
            fresh: [water.clone()].into_iter().collect(),
            ..Default::default()
        };
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            terrain: &t,
        };
        world.ledger = hornvale_kernel::tick(
            &world.ledger,
            &[&sys],
            &["drive-movements"],
            &world.registry,
        )
        .unwrap();
        let recount = hornvale_historiography::recount(&world, entity).expect("facts exist");
        assert!(
            recount.contains("went down to the river it knew (thirst)"),
            "the recount names the drive's own reason for the move: {recount}"
        );
        assert!(
            recount.contains("drank from the river (thirst sated)"),
            "the recount also names the drink that satisfied the goal: {recount}"
        );
    }

    #[test]
    fn thirsty_but_unreachable_water_gives_up_quickly_not_at_max_steps() {
        // THE ANTI-HANG GUARD (The Foresight T3 review), reinterpreted for
        // belief (The Surmise): under the OLD ground-truth model, unreachable
        // water made `decide` return `Hold` immediately and the idle-jump's
        // strict-progress guard fired on that very first Hold. Under the
        // belief model an IGNORANT agent no longer "gives up" on water it has
        // never reached — it EXPLORES instead — so a genuinely water-less
        // world (all-INFINITY terrain: no water anywhere for the agent to
        // ever discover) exercises the OTHER termination guarantee: the
        // unconditional `steps >= MAX_STEPS` cap bounds the walk even though
        // it never reaches "thirsty and known-unreachable" in the old sense.
        // The load-bearing assertion is boundedness/termination, not an
        // exact fact count (an ignorant agent commits real exploration moves
        // now, where it once committed none).
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();
        let ledger = Ledger::default();
        let e = EntityId::new(1).unwrap();
        let home = raddr(1.0);
        let water = RoomAddr::containing([-1.0, 0.0, 0.0], 6); // irrelevant now: no water exists anywhere
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water,
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "herder".into(),
        };
        // No fresh water anywhere, so belief never forms and the agent
        // explores for the whole run.
        let t = PlantedTerrain::fresh_only(std::iter::empty());
        // A long wait: the MAX_STEPS cap (not the wait) must be what bounds
        // this — if it weren't a real backstop, work would scale with the
        // wait instead.
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 10_000.0 },
            params: SUSTENANCE,
            terrain: &t,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        // Never drinks (no water exists); the walk is bounded by MAX_STEPS —
        // the tick's own prompt return here (within this test's harness
        // timeout) is additional proof it didn't hang, but the real
        // assertion is the explicit bound below, not an exact fact count.
        let drank_count = next.find(DRANK).filter(|f| f.subject == e).count();
        assert_eq!(drank_count, 0, "no water exists so the agent never drinks");
        let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
        assert!(
            moves <= MAX_STEPS,
            "the MAX_STEPS cap must bound the exploring walk; got {moves} moves"
        );
    }

    #[test]
    fn a_degenerate_zero_rise_drive_terminates_via_the_max_steps_cap_not_a_hang() {
        // THE DEGENERATE-DRIVEPARAMS REGRESSION (The Foresight T3 review):
        // `rise: 0.0, act: 0.0` makes the `Hold`-idle jump compute
        // `next_act = last_drank + act / rise = 0.0 / 0.0 = NaN`. Every NaN
        // comparison (`<=`, `>`) is `false`, so BOTH strict-progress guards
        // in the `Hold` arm (`next_act <= day` and `next_act > self.to.day`)
        // fail to fire, and `day = next_act` sets `day` to NaN too (which
        // then also never exceeds `self.to.day`, since any comparison with
        // NaN is false). Only the unconditional `steps >= MAX_STEPS` cap
        // (10_000) stops the loop — this test proves that cap is the real
        // backstop, not the closed-form guard (which is a no-op here).
        //
        // Under action-centric arbitration (The Temperament): `drive == 0.0`
        // is "thirst active" by the threshold (`0.0 >= act == 0.0`), but its
        // CAPPED urgency is `0.0`, so EVERY action's utility is
        // `weight × 0.0 × serviceability == 0.0` — a zero-pressure drive drives
        // nothing. Arbitration therefore returns `Hold` on every step (rather
        // than the old model's blind explore-move, which stepped regardless of
        // urgency magnitude), and the walk terminates purely through the Hold
        // arm's non-finite idle-jump guard (`act / rise == 0.0 / 0.0 == NaN`
        // → `continue`), bounded by the unconditional `steps >= MAX_STEPS` cap.
        // This exercises the NaN-guard/Hold-spin path DIRECTLY — a stronger
        // termination proof than the old explore-move spin, and the correct
        // behaviour: no felt pressure, no motion.
        //
        // The old regression test for this class
        // (`a_misconfigured_drive_terminates_instead_of_hanging`, keyed on
        // `sated >= act`) no longer applies: the planned model's `decide`
        // never reads `sated`, so that degenerate class can't hang it. This
        // is the new degenerate class the planned model is actually exposed
        // to.
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();
        let ledger = Ledger::default();
        let e = EntityId::new(1).unwrap();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water,
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "herder".into(),
        };
        // No fresh water anywhere, so belief never forms.
        let t = PlantedTerrain::fresh_only(std::iter::empty());
        let degenerate = DriveParams {
            rise: 0.0,
            act: 0.0,
        };
        // A long interval: if MAX_STEPS were not the backstop, this would
        // spin forever (this test's own short harness timeout is additional
        // proof it didn't hang; the assertion below is the load-bearing
        // one).
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 1_000_000.0 },
            params: degenerate,
            terrain: &t,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        // The load-bearing proof is that the call above RETURNED at all — with
        // a 1_000_000-day interval, only the `steps >= MAX_STEPS` cap can bound
        // the NaN-guarded Hold-spin; a real hang would time this test out. The
        // committed-fact count is exactly ZERO: a zero-urgency drive drives
        // nothing (every action's utility is `× 0.0 == 0.0` → Hold), so no
        // `agent-at`/`drank` is ever emitted — the correct behaviour, and
        // trivially within the MAX_STEPS bound.
        let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
        let drinks = next.find(DRANK).filter(|f| f.subject == e).count();
        assert!(
            moves + drinks <= MAX_STEPS,
            "the MAX_STEPS cap must bound total committed facts even under a \
             NaN-producing degenerate DriveParams; got {moves} moves + {drinks} drinks"
        );
        assert_eq!(
            moves + drinks,
            0,
            "a zero-urgency (zero-pressure) drive drives nothing: arbitration \
             Holds every step and the walk terminates via the bounded NaN-guard \
             Hold-spin, committing no facts"
        );
    }

    #[test]
    fn moves_carry_drive_naming_provenance() {
        let p = SUSTENANCE;
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();
        let e = ledger.mint_entity();
        let home = addr(1.0);
        let resource = home.neighbors()[0].clone();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: resource.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "herder".into(),
        };
        let t = PlantedTerrain::fresh_only([resource.clone()]);
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 10.0 },
            params: p,
            terrain: &t,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        let first = next.find(AGENT_AT).find(|f| f.subject == e).unwrap();
        assert!(
            first.provenance.contains("thirst")
                || first.provenance.contains("water")
                || first.provenance.contains("sustenance"),
            "provenance names the drive: {}",
            first.provenance
        );
        let _ = ledger;
    }

    #[test]
    fn plan_to_water_is_a_precondition_chain_move_then_drink() {
        // Water is a mesh neighbor of home (one step away): plan is [MoveTo(water), Drink].
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let plan = plan_to_water(&home, &water, 10_000, &std::collections::BTreeSet::new())
            .expect("reachable");
        assert_eq!(plan.len(), 2);
        assert!(matches!(plan[0], Action::MoveTo(ref r) if *r == water));
        assert!(matches!(plan[1], Action::Drink));
    }

    #[test]
    fn plan_to_water_when_already_there_is_just_drink() {
        let water = raddr(1.0);
        let plan =
            plan_to_water(&water, &water, 10_000, &std::collections::BTreeSet::new()).unwrap();
        assert_eq!(plan, vec![Action::Drink]);
    }

    #[test]
    fn every_action_in_a_plan_has_its_precondition_satisfied_in_sequence() {
        // Execute the plan from `home`, checking each action's precondition holds in
        // order (the precondition-chain validity: Drink is only ever preceded by
        // arrival at water). Water two rooms away for a genuine multi-step chain.
        let home = raddr(1.0);
        let mid = home.neighbors()[0].clone();
        let water = mid
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone();
        let plan = plan_to_water(&home, &water, 10_000, &std::collections::BTreeSet::new())
            .expect("reachable");
        let mut pos = home.clone();
        let mut hydrated = false;
        for a in &plan {
            match a {
                Action::MoveTo(n) => {
                    assert!(
                        pos.neighbors().contains(n),
                        "MoveTo precondition: adjacency"
                    );
                    pos = n.clone();
                }
                Action::Drink => {
                    assert_eq!(pos, water, "Drink precondition: at water");
                    hydrated = true;
                }
                Action::Rest | Action::Eat => {
                    unreachable!("plan_to_water never emits Rest or Eat")
                }
            }
        }
        assert!(hydrated, "the plan achieves the goal");
        assert!(
            plan.len() >= 3,
            "multi-step: at least two moves then a drink"
        );
    }

    #[test]
    fn plan_to_room_is_pure_navigation_no_drink() {
        let home = raddr(1.0);
        let dest = home.neighbors()[0].clone();
        let plan = plan_to_room(&home, &dest, 10_000, &std::collections::BTreeSet::new()).unwrap();
        assert!(plan.iter().all(|a| matches!(a, Action::MoveTo(_))));
        assert!(!plan.is_empty());
    }

    #[test]
    fn planner_routes_around_a_remembered_cell() {
        // THE SHUN: a remembered-dangerous cell on the straight path becomes a
        // finite detour cost, so the A* routes AROUND it when a cheaper detour
        // exists — and with an EMPTY avoid set the plan is unchanged (the
        // byte-identity property, at the planner seam).
        let home = raddr(1.0);
        let mid = home.neighbors()[0].clone();
        let water = mid
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone(); // 2 hops from home, via `mid`
        let empty = std::collections::BTreeSet::new();
        let direct = plan_to_water(&home, &water, 10_000, &empty).expect("reachable");
        assert_eq!(
            direct.len(),
            3,
            "the straight path is two moves then a drink"
        );
        // The via-cell the straight plan actually steps through (not water itself).
        let via = direct
            .iter()
            .find_map(|a| match a {
                Action::MoveTo(r) if *r != water => Some(r.clone()),
                _ => None,
            })
            .expect("a via-cell on the straight path");
        let mut avoid = std::collections::BTreeSet::new();
        avoid.insert(via.clone());
        let around = plan_to_water(&home, &water, 10_000, &avoid).expect("still reachable");
        assert!(
            !around
                .iter()
                .any(|a| matches!(a, Action::MoveTo(r) if *r == via)),
            "the plan routes AROUND the remembered cell"
        );
        assert!(
            matches!(around.last(), Some(Action::Drink)),
            "the detour still reaches water and drinks"
        );
        assert!(
            around.len() >= direct.len(),
            "the detour costs at least as much as the straight path"
        );
    }

    #[test]
    fn planner_braves_it_when_the_detour_exceeds_the_penalty() {
        // SURVIVAL-OVERRIDE FOR FREE: the penalty is FINITE, so when the
        // remembered-bad cell is the ONLY route to water (no detour at all — an
        // infinite alternative), the creature still takes it. A dying-thirsty
        // creature braves the haunted ground; the flinch is a preference, not a
        // wall.
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone(); // 1 hop — MoveTo(water) is the sole route in
        let mut avoid = std::collections::BTreeSet::new();
        avoid.insert(water.clone());
        let plan =
            plan_to_water(&home, &water, 10_000, &avoid).expect("the finite penalty never traps");
        assert_eq!(
            plan,
            vec![Action::MoveTo(water.clone()), Action::Drink],
            "braves the remembered cell when it is the only route"
        );
    }

    #[test]
    fn the_shun_a_frightened_creature_detours_around_remembered_ground_a_control_goes_through() {
        // THE SHUN, end-to-end through the real DriveMovements tick (spec §e2e):
        // a creature frightened at a cell X on an early trip plans its LATER
        // journeys to water AROUND X — proactively — while an otherwise-identical
        // control that never stood at X takes the straight path THROUGH it. Both
        // reach water (the frightened one is never trapped — the finite penalty is
        // a preference, not a wall). X is only MILDLY hazardous (0.4, just over
        // DANGER_ACT): enough that the memory forms and a dying-thirsty control
        // reactively pushes through it, but not lethal (that would make the
        // control route around reactively too — the keystone
        // `danger_routes_a_thirsty_creature_around_a_hazard_to_water` case).
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();

        // Geometry: discover the straight S→W path (hazard-free planning) and pick
        // an INTERIOR cell X (distance 2 from S) as the frightening ground. X is
        // not adjacent to S or W, so standing at S/W is never itself frightening
        // (`threat_field` maxes over neighbours) — the remembered set is exactly
        // {X}.
        let start = raddr(1.0);
        // Chain neighbours to a distant water cell, then take the true shortest
        // path so an interior cell is guaranteed.
        let c1 = start.neighbors()[0].clone();
        let c2 = c1
            .neighbors()
            .iter()
            .find(|n| **n != start)
            .unwrap()
            .clone();
        let c3 = c2
            .neighbors()
            .iter()
            .find(|n| **n != c1 && **n != start)
            .unwrap()
            .clone();
        let water = c3
            .neighbors()
            .iter()
            .find(|n| **n != c2 && **n != c1 && **n != start)
            .unwrap()
            .clone();
        let empty = std::collections::BTreeSet::new();
        let straight = plan_to_room(&start, &water, PLAN_BUDGET, &empty).expect("reachable");
        assert!(
            straight.len() >= 4,
            "need a path with an interior cell not adjacent to either endpoint"
        );
        let path_cells: Vec<RoomAddr> = straight
            .iter()
            .map(|a| match a {
                Action::MoveTo(r) => r.clone(),
                _ => unreachable!("plan_to_room emits only MoveTo"),
            })
            .collect();
        let x = path_cells[1].clone(); // distance 2 from start ⇒ not adjacent to start; ≥2 from water
        assert!(
            !start.neighbors().contains(&x) && !water.neighbors().contains(&x),
            "X must be interior (not adjacent to start or water)"
        );

        // Terrain: fresh water at W, a MILD uncanny hazard at X. The planner reads
        // only the avoid-set, not the terrain hazard, so the control's straight
        // plan still runs through X; the hazard drives only the reactive Danger
        // drive during the walk.
        let terrain = PlantedTerrain::hazard([water.clone()], [(x.clone(), 0.4)]);

        // A steady mortal creature; home == start so homing does not pull it off X.
        let npc_at = |entity: EntityId| Npc {
            entity,
            home: start.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "wanderer".into(),
        };

        // Run one creature forward over a multi-cycle window and return (ledger,
        // entity). `remember_x` seeds a committed visit to X (day 0.15), so the
        // frightened creature's believed_hazard is {X}; the control never stood
        // there. Both know water (a committed visit to W) and start at S.
        let run = |remember_x: bool| -> (Ledger, EntityId) {
            let mut ledger = Ledger::default();
            let e = ledger.mint_entity();
            commit_agent_at(&mut ledger, &reg, e, &water, 0.1); // knows water
            if remember_x {
                commit_agent_at(&mut ledger, &reg, e, &x, 0.15); // frightened here → remembers X
            }
            commit_agent_at(&mut ledger, &reg, e, &start, 0.2); // now at start
            let sys = DriveMovements {
                npcs: vec![npc_at(e)],
                from: WorldTime { day: 1.0 }, // after the seeded history
                to: WorldTime { day: 60.0 },  // several thirst cycles (act/rise ≈ 5.7 days)
                params: SUSTENANCE,
                terrain: &terrain,
            };
            let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
            (next, e)
        };

        // The committed positions the tick EMITTED (day ≥ from), decoded to rooms.
        let walked = |ledger: &Ledger, e: EntityId| -> Vec<RoomAddr> {
            ledger
                .find(AGENT_AT)
                .filter(|f| f.subject == e)
                .filter(|f| f.day.map(|d| d >= 1.0).unwrap_or(false))
                .filter_map(|f| match &f.object {
                    Value::Text(s) => Some(room_from_text(s)),
                    _ => None,
                })
                .collect()
        };
        let drinks =
            |ledger: &Ledger, e: EntityId| ledger.find(DRANK).filter(|f| f.subject == e).count();

        // Confirmed sanity: believed_hazard is exactly {X} for the frightened
        // creature and empty for the control (the one-source-of-truth fold).
        {
            let mut fl = Ledger::default();
            let fe = fl.mint_entity();
            commit_agent_at(&mut fl, &reg, fe, &water, 0.1);
            commit_agent_at(&mut fl, &reg, fe, &x, 0.15);
            commit_agent_at(&mut fl, &reg, fe, &start, 0.2);
            let n = npc_at(fe);
            let hz = believed_hazard(&fl, &n, WorldTime { day: 1.0 }, &terrain, &[]);
            assert_eq!(
                hz.into_iter().collect::<Vec<_>>(),
                vec![x.clone()],
                "the frightened creature remembers exactly X"
            );
        }

        let (frightened_led, fe) = run(true);
        let (control_led, ce) = run(false);

        let frightened_path = walked(&frightened_led, fe);
        let control_path = walked(&control_led, ce);

        // (a) Both reach water and drink — the frightened one is NEVER trapped.
        assert!(
            drinks(&frightened_led, fe) >= 1,
            "the frightened creature still reaches water (the detour is finite)"
        );
        assert!(drinks(&control_led, ce) >= 1, "the control reaches water");
        // (b) The frightened creature's LATER journeys detour AROUND X — no
        // tick-emitted position is X.
        assert!(
            !frightened_path.contains(&x),
            "the frightened creature routes around remembered ground X"
        );
        // (c) The control, with no memory of X, takes the straight path THROUGH X
        // (mildly hazardous ground a dying-thirsty creature pushes across).
        assert!(
            control_path.contains(&x),
            "the never-frightened control blunders straight through X"
        );
    }

    #[test]
    fn the_phantom_detours_around_a_passed_alarm_then_relearns_the_ground_safe() {
        // THE PHANTOM, end-to-end through the real DriveMovements tick (spec §e2e):
        // a creature alarm-frightened at a now-SAFE cell X — where a herd-mate B
        // briefly panicked beside it — plans its LATER journeys to water AROUND X,
        // shunning ground that is no longer dangerous (the phobia, a fear of
        // nothing). A control that never stood at X blunders straight through. And
        // once the creature safely REVISITS X (the alarm long gone), the detour
        // ceases — the fear disproved. UNLIKE THE SHUN, X carries NO terrain
        // hazard: the danger is transient, re-derived from B's committed PAST
        // position, and gone by planning time.
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();

        // Geometry (as THE SHUN): the straight S→W path, X an interior cell not
        // adjacent to either endpoint (so standing at S/W is never frightening).
        let start = raddr(1.0);
        let c1 = start.neighbors()[0].clone();
        let c2 = c1
            .neighbors()
            .iter()
            .find(|n| **n != start)
            .unwrap()
            .clone();
        let c3 = c2
            .neighbors()
            .iter()
            .find(|n| **n != c1 && **n != start)
            .unwrap()
            .clone();
        let water = c3
            .neighbors()
            .iter()
            .find(|n| **n != c2 && **n != c1 && **n != start)
            .unwrap()
            .clone();
        let empty = std::collections::BTreeSet::new();
        let straight = plan_to_room(&start, &water, PLAN_BUDGET, &empty).expect("reachable");
        assert!(straight.len() >= 4, "need a path with an interior cell");
        let path_cells: Vec<RoomAddr> = straight
            .iter()
            .map(|a| match a {
                Action::MoveTo(r) => r.clone(),
                _ => unreachable!("plan_to_room emits only MoveTo"),
            })
            .collect();
        let x = path_cells[1].clone(); // interior, distance 2 from start
        let p0 = path_cells[0].clone(); // X's on-path predecessor (distance 1)
        let p2 = path_cells[2].clone(); // X's on-path successor (distance 3)
        assert!(
            !start.neighbors().contains(&x) && !water.neighbors().contains(&x),
            "X must be interior (not adjacent to start or water)"
        );

        // The emitter's cell D: X's OFF-path neighbour (not p0, not p2). Its own
        // neighbour E carries the hazard, so B — standing at the SAFE cell D beside
        // the hazard — is primary-afraid (anticipatory) and its one-hop alarm halo
        // covers X. E is two hops from X, so X itself stays terrain-SAFE (a pure
        // phantom, not a Haunt).
        let d_cell = x
            .neighbors()
            .iter()
            .find(|n| **n != p0 && **n != p2)
            .expect("X has a third, off-path neighbour")
            .clone();
        let hazard_e = d_cell
            .neighbors()
            .iter()
            .find(|n| **n != x && **n != p0 && **n != p2 && **n != start && **n != water)
            .expect("D has a hazard neighbour off the path")
            .clone();
        let far = raddr(-1.0);
        let terrain = PlantedTerrain::hazard([water.clone()], [(hazard_e.clone(), 0.8)]);

        // A steady mortal; home == start so homing does not pull it off course.
        let npc_at = |entity: EntityId| Npc {
            entity,
            home: start.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: BOLDNESS_STEADY,
            threat_niche: mortal_threat_niche(),
            label: "wanderer".into(),
        };
        // The herd-mate B: knows water (so it beelines and settles, bounded), and
        // is the transient alarm source when standing at D.
        let emitter_npc = |entity: EntityId| Npc {
            entity,
            home: far.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: BOLDNESS_STEADY,
            threat_niche: mortal_threat_niche(),
            label: "herd-mate".into(),
        };

        // Guard: every cell on the straight path is terrain-SAFE — the phantom is
        // a fear of nothing, never a static Haunt (empty-roster verdict is FALSE).
        let dummy = npc_at(EntityId::new(1).unwrap());
        let empty_ledger = Ledger::default();
        for cell in [&start, &p0, &x, &p2, &water] {
            assert!(
                !frightened_at(
                    cell,
                    &dummy,
                    &terrain,
                    WorldTime { day: 1.0 },
                    &[],
                    &empty_ledger
                ),
                "path cell {cell:?} must be terrain-safe (no static hazard)"
            );
        }

        // Sanity: the phantom forms exactly at X. Terrain-only memory is empty
        // (X is safe ground); with the roster, the re-derived PAST alarm — B at D
        // on day 0.35 — makes X remembered-dangerous though it is now safe.
        {
            let mut fl = Ledger::default();
            let a = fl.mint_entity();
            commit_agent_at(&mut fl, &reg, a, &water, 0.30);
            commit_agent_at(&mut fl, &reg, a, &x, 0.35);
            commit_agent_at(&mut fl, &reg, a, &start, 0.40);
            let b = fl.mint_entity();
            commit_agent_at(&mut fl, &reg, b, &d_cell, 0.35);
            commit_agent_at(&mut fl, &reg, b, &far, 0.40);
            let an = npc_at(a);
            let bn = emitter_npc(b);
            assert!(
                believed_hazard(&fl, &an, WorldTime { day: 1.0 }, &terrain, &[]).is_empty(),
                "terrain-only memory is empty — X is a fear of nothing"
            );
            let hz = believed_hazard(
                &fl,
                &an,
                WorldTime { day: 1.0 },
                &terrain,
                std::slice::from_ref(&bn),
            );
            assert_eq!(
                hz.into_iter().collect::<Vec<_>>(),
                vec![x.clone()],
                "the phantom: X is remembered-dangerous, re-derived from B's past panic"
            );
        }

        // Run one rememberer (+ the herd-mate B) forward over a multi-cycle window.
        // `remember` seeds the frightened visit to X (day 0.35, B beside it);
        // `disprove` adds a later SAFE revisit (day 2.35, B long gone).
        let run = |remember: bool, disprove: bool| -> (Ledger, EntityId, f64) {
            let from_day = if disprove { 3.0 } else { 1.0 };
            let mut ledger = Ledger::default();
            let a = ledger.mint_entity();
            commit_agent_at(&mut ledger, &reg, a, &water, 0.30); // knows water
            if remember {
                commit_agent_at(&mut ledger, &reg, a, &x, 0.35); // frightened by B's alarm
            }
            if disprove {
                commit_agent_at(&mut ledger, &reg, a, &x, 2.35); // SAFE revisit (B gone)
            }
            commit_agent_at(
                &mut ledger,
                &reg,
                a,
                &start,
                if disprove { 2.40 } else { 0.40 },
            );
            let mut npcs = vec![npc_at(a)];
            if remember {
                // B: knows water, panics beside X at 0.35, gone to far ground by
                // 0.40 — so X is SAFE at planning time. Only present when there is
                // a memory to re-derive (the control needs no alarm source).
                let b = ledger.mint_entity();
                commit_agent_at(&mut ledger, &reg, b, &water, 0.29);
                commit_agent_at(&mut ledger, &reg, b, &d_cell, 0.35);
                commit_agent_at(&mut ledger, &reg, b, &far, 0.40);
                npcs.push(emitter_npc(b));
            }
            let sys = DriveMovements {
                npcs,
                from: WorldTime { day: from_day },
                to: WorldTime { day: 60.0 }, // several thirst cycles (act/rise ≈ 5.7 days)
                params: SUSTENANCE,
                terrain: &terrain,
            };
            let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
            (next, a, from_day)
        };

        // The committed positions the tick EMITTED (day ≥ from), decoded to rooms.
        let walked = |ledger: &Ledger, e: EntityId, from_day: f64| -> Vec<RoomAddr> {
            ledger
                .find(AGENT_AT)
                .filter(|f| f.subject == e)
                .filter(|f| f.day.map(|d| d >= from_day).unwrap_or(false))
                .filter_map(|f| match &f.object {
                    Value::Text(s) => Some(room_from_text(s)),
                    _ => None,
                })
                .collect()
        };
        let drinks =
            |ledger: &Ledger, e: EntityId| ledger.find(DRANK).filter(|f| f.subject == e).count();

        let (frightened_led, fe, ffrom) = run(true, false);
        let (control_led, ce, cfrom) = run(false, false);
        let (disproved_led, de, dfrom) = run(true, true);

        let frightened_path = walked(&frightened_led, fe, ffrom);
        let control_path = walked(&control_led, ce, cfrom);
        let disproved_path = walked(&disproved_led, de, dfrom);

        // Everyone reaches water — the phantom-shunning creature is NEVER trapped
        // (the memory penalty is finite, a preference, not a wall).
        assert!(
            drinks(&frightened_led, fe) >= 1,
            "the frightened creature still reaches water (the detour is finite)"
        );
        assert!(drinks(&control_led, ce) >= 1, "the control reaches water");
        assert!(
            drinks(&disproved_led, de) >= 1,
            "after the disproof the creature still reaches water"
        );

        // (a) THE PHANTOM: later journeys DETOUR around the now-safe X, where a
        // never-alarmed control goes straight THROUGH it.
        assert!(
            !frightened_path.contains(&x),
            "the phantom: routes AROUND ground where a passed alarm once frightened it"
        );
        assert!(
            control_path.contains(&x),
            "the never-alarmed control blunders straight through the safe X"
        );
        // (b) THE DISPROOF: once X is safely revisited, the most-recent visit is
        // safe, the phantom clears, and the detour ceases — through X again.
        assert!(
            disproved_path.contains(&x),
            "the disproof: a safe revisit clears the phantom — X is braved again"
        );
    }

    /// A synthetic elevation + fresh-water field for pure tests: planted
    /// heights, INFINITY elsewhere (INFINITY = "never chosen downhill" —
    /// mirrors `LocaleTerrain`'s undescribable-room fallback), and a planted
    /// SET of fresh-water rooms (the-surmise T5 re-wire: water is no longer
    /// an elevation threshold — `Terrain::is_fresh_water` is authoritative).
    #[derive(Default)]
    struct PlantedTerrain {
        elevations: std::collections::BTreeMap<RoomAddr, f64>,
        fresh: std::collections::BTreeSet<RoomAddr>,
        /// Planted per-room temperatures (°C) for the thermal-drive tests;
        /// INFINITY elsewhere (the thirst tests never read temperature).
        temps: std::collections::BTreeMap<RoomAddr, f64>,
        /// Planted per-room food productivity for the hunger-drive tests;
        /// rooms without an entry read `DEFAULT_FORAGE` (fed) — so the thirst/
        /// thermal tests, which plant none, keep their creatures fed and
        /// hunger-inactive (byte-identical to pre-Provender behaviour).
        forage: std::collections::BTreeMap<RoomAddr, f64>,
        /// Planted per-room hazards for the danger-drive tests; rooms without an
        /// entry read `Hazards::ZERO` (safe) — so the other tests, which plant
        /// none, are danger-inactive. Named `threat` for continuity; the
        /// `hazard()` constructor plants a scalar as the UNCANNY axis (the axis a
        /// mortal niche weights `1`, so the pre-Bane danger tests are byte-
        /// identical), and thermal tests plant `Hazards` directly.
        threat: std::collections::BTreeMap<RoomAddr, Hazards>,
        /// Planted per-room prey presence (The Teeth's hunt tests); rooms without
        /// an entry read `0.0` (prey-empty) — so every other test is byte-
        /// identical (a carnivore there reads only ordinary productivity).
        prey: std::collections::BTreeMap<RoomAddr, f64>,
    }
    impl PlantedTerrain {
        /// No elevation data — just a set of fresh-water rooms (the common
        /// case for the belief-fold tests, which never exercise
        /// `downhill_step`/`nearest_water`'s elevation reads).
        fn fresh_only(rooms: impl IntoIterator<Item = RoomAddr>) -> Self {
            Self {
                fresh: rooms.into_iter().collect(),
                ..Default::default()
            }
        }
        /// No fresh water anywhere — just planted elevations (the
        /// exploration/downhill tests, which never exercise belief).
        fn dry(elevations: std::collections::BTreeMap<RoomAddr, f64>) -> Self {
            Self {
                elevations,
                ..Default::default()
            }
        }
        /// Just planted per-room temperatures (the thermal-drive tests, which
        /// never exercise elevation/water). Rooms without a planted temperature
        /// read `INFINITY` (never chosen as a comfort target).
        fn thermal(temps: impl IntoIterator<Item = (RoomAddr, f64)>) -> Self {
            Self {
                temps: temps.into_iter().collect(),
                ..Default::default()
            }
        }
        /// Just planted per-room food productivity (the hunger-drive tests).
        /// Rooms without an entry read `DEFAULT_FORAGE` (fed).
        fn forage(forage: impl IntoIterator<Item = (RoomAddr, f64)>) -> Self {
            Self {
                forage: forage.into_iter().collect(),
                ..Default::default()
            }
        }
        /// Planted per-room fresh water AND a scalar hazard mapped to the UNCANNY
        /// axis (the danger-drive tests, which pair a hazard with a water source
        /// to prove routing). Rooms without an entry read `Hazards::ZERO` (safe).
        /// A mortal threat niche weights UNCANNY `1`, so a scalar `s` reads as
        /// felt threat `s` — the pre-Bane danger tests stay byte-identical.
        fn hazard(
            fresh: impl IntoIterator<Item = RoomAddr>,
            threat: impl IntoIterator<Item = (RoomAddr, f64)>,
        ) -> Self {
            Self {
                fresh: fresh.into_iter().collect(),
                threat: threat
                    .into_iter()
                    .map(|(r, s)| {
                        (
                            r,
                            Hazards {
                                uncanny: s,
                                ..Hazards::ZERO
                            },
                        )
                    })
                    .collect(),
                ..Default::default()
            }
        }
        /// Planted per-room `Hazards` directly (the per-axis thermal-fear tests).
        fn hazards_map(hazards: impl IntoIterator<Item = (RoomAddr, Hazards)>) -> Self {
            Self {
                threat: hazards.into_iter().collect(),
                ..Default::default()
            }
        }
        /// Planted per-room food productivity AND prey presence — the hunt tests
        /// (The Teeth): a carnivore on this ground reads productivity for its
        /// forage axis and the prey field for its prey axis. Rooms without a
        /// forage entry read `DEFAULT_FORAGE`; without a prey entry, `0.0`.
        fn forage_and_prey(
            forage: impl IntoIterator<Item = (RoomAddr, f64)>,
            prey: impl IntoIterator<Item = (RoomAddr, f64)>,
        ) -> Self {
            Self {
                forage: forage.into_iter().collect(),
                prey: prey.into_iter().collect(),
                ..Default::default()
            }
        }
    }
    impl Terrain for PlantedTerrain {
        fn elevation(&self, room: &RoomAddr) -> f64 {
            self.elevations.get(room).copied().unwrap_or(f64::INFINITY)
        }
        fn is_fresh_water(&self, room: &RoomAddr) -> bool {
            self.fresh.contains(room)
        }
        fn temperature(&self, room: &RoomAddr, _day: WorldTime) -> f64 {
            self.temps.get(room).copied().unwrap_or(f64::INFINITY)
        }
        fn forage_value(&self, room: &RoomAddr) -> f64 {
            self.forage.get(room).copied().unwrap_or(DEFAULT_FORAGE)
        }
        fn hazards(&self, room: &RoomAddr) -> Hazards {
            self.threat.get(room).copied().unwrap_or(Hazards::ZERO)
        }
        fn prey_value(&self, room: &RoomAddr) -> f64 {
            self.prey.get(room).copied().unwrap_or(0.0)
        }
    }

    /// The default mortal threat niche (The Bane) — dreads the uncanny fully
    /// (weight `1`) and heat/cold at the goblinoid-neutral derived level. Used by
    /// tests that plant an UNCANNY hazard and expect the old scalar behaviour:
    /// `threat_value` reduces to the planted uncanny, so the pre-Bane danger
    /// tests stay byte-identical.
    fn mortal_threat_niche() -> ThreatNiche {
        derive_threat_niche(
            &DEFAULT_TEMPERATURE_NICHE,
            MetabolicClass::Endotherm,
            &default_diet_niche(),
        )
    }

    /// A balanced omnivore diet (forage + prey) — the common hunger-test niche.
    fn omnivore_niche() -> ResourceVector {
        ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)]).unwrap()
    }

    #[test]
    fn food_value_is_the_niche_dotted_with_availability() {
        // An omnivore reads the cell's material productivity (forage+prey);
        // a barren cell feeds it less than a rich one.
        let rich = raddr(1.0);
        let barren = rich.neighbors()[0].clone();
        let t = PlantedTerrain::forage([(rich.clone(), 1.0), (barren.clone(), 0.0)]);
        let omni = omnivore_niche();
        let day = WorldTime { day: 0.5 }; // noon (sun up) — irrelevant to an omnivore
        assert!(
            food_value(&omni, &t, &rich, day)
                .total_cmp(&food_value(&omni, &t, &barren, day))
                .is_gt(),
            "richer ground is worth more food to an omnivore"
        );
        // An EMPTY niche reads no food anywhere (the niche-gate's basis).
        let empty = ResourceVector::new(&[]).unwrap();
        assert_eq!(food_value(&empty, &t, &rich, day), 0.0);
    }

    #[test]
    fn prey_ground_feeds_a_carnivore_and_leaves_a_herbivore_flat() {
        // THE TEETH: the prey field lifts `food_value` for the ANIMAL_PREY axis,
        // so prey-dense ground is worth more to a carnivore — but a pure
        // herbivore (no prey-axis weight) reads the prey field as nothing, so its
        // food_value is flat across prey-dense and prey-empty ground.
        let preyful = raddr(1.0);
        let empty = preyful.neighbors()[0].clone();
        // Uniform productivity, prey only on `preyful`.
        let t = PlantedTerrain::forage_and_prey(
            [(preyful.clone(), 1.0), (empty.clone(), 1.0)],
            [(preyful.clone(), 1.0)],
        );
        let day = WorldTime { day: 0.5 };
        let carnivore = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap();
        let herbivore = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
        assert!(
            food_value(&carnivore, &t, &preyful, day) > food_value(&carnivore, &t, &empty, day),
            "prey-dense ground feeds a carnivore more"
        );
        assert_eq!(
            food_value(&herbivore, &t, &preyful, day),
            food_value(&herbivore, &t, &empty, day),
            "a pure herbivore reads the prey field as nothing — flat"
        );
    }

    #[test]
    fn a_carnivore_forages_toward_prey_a_herbivore_does_not() {
        // THE TEETH, end to end: on ground of UNIFORM productivity (no forage
        // gradient) with prey concentrated in one neighbour, a carnivore forages
        // toward the prey — the hunt, live — while a herbivore, blind to the prey
        // field, follows only the (flat) forage and breaks the tie elsewhere.
        let c = raddr(1.0);
        let neighbors = c.neighbors();
        // The prey cell is the LARGEST-address neighbour, so a herbivore's
        // uniform-forage tie-break (smallest address) can never land on it —
        // any pull toward it is the prey draw, not an artefact of the tie-break.
        let prey_cell = neighbors.iter().max().unwrap().clone();
        let uniform: Vec<(RoomAddr, f64)> = neighbors
            .iter()
            .cloned()
            .chain(std::iter::once(c.clone()))
            .map(|r| (r, 1.0))
            .collect();
        let t = PlantedTerrain::forage_and_prey(uniform, [(prey_cell.clone(), 1.0)]);
        let day = WorldTime { day: 0.5 };
        let carnivore = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap();
        let herbivore = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
        assert_eq!(
            forage_step(&c, &carnivore, &t, day),
            Some(prey_cell.clone()),
            "a carnivore forages toward prey-dense ground"
        );
        assert_ne!(
            forage_step(&c, &herbivore, &t, day),
            Some(prey_cell),
            "a herbivore ignores the prey field (uniform forage → tie-break, not prey)"
        );
    }

    #[test]
    fn an_autotroph_is_fed_by_light_not_forage() {
        // A pure photosynthate niche reads the SUN, not the productivity field:
        // fed by day (sun up), starved at night — even on barren ground.
        let cell = raddr(1.0);
        let t = PlantedTerrain::forage([(cell.clone(), 0.0)]); // no material food
        let autotroph = ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap();
        let noon = WorldTime { day: 0.5 }; // fractional_day_sun → +90°
        let midnight = WorldTime { day: 0.0 }; // → −90°
        assert!(
            food_value(&autotroph, &t, &cell, noon) > 0.0,
            "an autotroph eats by day"
        );
        assert_eq!(
            food_value(&autotroph, &t, &cell, midnight),
            0.0,
            "an autotroph starves at night"
        );
    }

    #[test]
    fn hunger_folds_eaten_and_resets_on_a_meal() {
        // HUNGER == FOLD over `eaten`, the twin of thirst over `drank`.
        let reg = {
            let mut r = agent_at_reg();
            r.register_predicate(EATEN, false, "eaten").unwrap();
            r
        };
        let home = raddr(1.0);
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        // No meal yet → hunger has risen by day 5 (a thermoneutral/unreadable
        // cell couples at the base HUNGER rate).
        let t = PlantedTerrain::forage(std::iter::empty());
        let before = hunger_at(
            &ledger,
            e,
            &home,
            WorldTime { day: 5.0 },
            &t,
            MetabolicClass::Endotherm,
        );
        assert!(before > 0.0, "hunger accrues without a meal");
        // Eat on day 5 → hunger is 0 right after.
        ledger.commit(eaten_fact(e, 5.0, "ate"), &reg).unwrap();
        let after = hunger_at(
            &ledger,
            e,
            &home,
            WorldTime { day: 5.0 },
            &t,
            MetabolicClass::Endotherm,
        );
        assert_eq!(after, 0.0, "a meal resets hunger");
    }

    #[test]
    fn hunger_affordance_eats_at_a_rich_cell_and_forages_from_a_barren_one() {
        let barren = raddr(1.0);
        // Plant EVERY neighbour barren except one, so the forage step is
        // unambiguous (unplanted rooms default to DEFAULT_FORAGE, which would
        // otherwise tie).
        let ns = barren.neighbors();
        let rich = ns[0].clone();
        let t = PlantedTerrain::forage([
            (barren.clone(), 0.0),
            (rich.clone(), 1.0),
            (ns[1].clone(), 0.0),
            (ns[2].clone(), 0.0),
        ]);
        let day = WorldTime { day: 0.0 };
        let view_barren = Perceived {
            position: barren.clone(),
            drive: 0.0,
            fatigue: 0.0,
            believed_water: None,
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let hunger = Hunger {
            urgency: 0.9,
            niche: omnivore_niche(),
            terrain: &t,
            day,
        };
        // Barren cell: forage toward the richer neighbour.
        assert_eq!(
            hunger.affordance(&view_barren, PLAN_BUDGET),
            Some(Action::MoveTo(rich.clone())),
            "a hungry creature on barren ground forages toward richer ground"
        );
        // Rich cell: eat in place.
        let view_rich = Perceived {
            position: rich.clone(),
            drive: 0.0,
            fatigue: 0.0,
            believed_water: None,
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert_eq!(
            hunger.affordance(&view_rich, PLAN_BUDGET),
            Some(Action::Eat),
            "a hungry creature on rich ground eats in place"
        );
    }

    #[test]
    fn hunger_integrates_faster_over_a_hot_occupancy() {
        // The Kindling coupling, reused: a hot endotherm hungers faster than a
        // thermoneutral one over the same elapsed time (mirrors the thirst test).
        let home = raddr(1.0);
        let hot = PlantedTerrain::thermal([(home.clone(), 45.0)]);
        let mild = PlantedTerrain::thermal([(home.clone(), 25.0)]);
        let mut ledger = Ledger::default(); // no eaten, no sightings → held at home
        let e = ledger.mint_entity();
        let day = WorldTime { day: 3.0 };
        let hot_h = hunger_at(&ledger, e, &home, day, &hot, MetabolicClass::Endotherm);
        let mild_h = hunger_at(&ledger, e, &home, day, &mild, MetabolicClass::Endotherm);
        assert!(
            hot_h.total_cmp(&mild_h).is_gt(),
            "heat hastens hunger for an endotherm"
        );
    }

    /// A `Perceived` view standing at `pos`, with the non-danger drives quiet
    /// (danger reads only `position` + the terrain it holds).
    fn view_at(pos: RoomAddr) -> Perceived {
        Perceived {
            position: pos,
            drive: 0.0,
            fatigue: 0.0,
            believed_water: None,
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        }
    }

    #[test]
    fn danger_urgency_reads_the_cell_threat_and_defaults_safe() {
        let scary = raddr(1.0);
        // A cell on the far side of the world — neither it nor its neighbours
        // touch the threat, so anticipatory urgency reads 0.
        let far = raddr(-1.0);
        let t = PlantedTerrain::hazard(std::iter::empty(), [(scary.clone(), 0.8)]);
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
        };
        assert_eq!(
            danger.urgency(&view_at(scary)),
            0.8,
            "feels the cell's threat"
        );
        assert_eq!(
            danger.urgency(&view_at(far)),
            0.0,
            "a cell far from any threat is safe"
        );
    }

    #[test]
    fn flee_step_picks_the_safest_neighbour_or_none_when_cornered() {
        let here = raddr(1.0);
        let ns = here.neighbors();
        // here is dangerous; one neighbour is safe(r), the others as bad as here.
        let t = PlantedTerrain::hazard(
            std::iter::empty(),
            [
                (here.clone(), 0.9),
                (ns[0].clone(), 0.1),
                (ns[1].clone(), 0.9),
                (ns[2].clone(), 0.9),
            ],
        );
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
        };
        assert_eq!(
            danger.affordance(&view_at(here.clone()), PLAN_BUDGET),
            Some(Action::MoveTo(ns[0].clone())),
            "flees to the safest neighbour"
        );
        // Cornered: here and every neighbour equally dangerous → no safer step.
        let boxed = PlantedTerrain::hazard(
            std::iter::empty(),
            here.neighbors()
                .into_iter()
                .chain(std::iter::once(here.clone()))
                .map(|r| (r, 0.9)),
        );
        let danger_boxed = Danger {
            terrain: &boxed,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
        };
        assert_eq!(
            danger_boxed.affordance(&view_at(here), PLAN_BUDGET),
            None,
            "boxed in by threat everywhere → holds (cornered)"
        );
    }

    #[test]
    fn danger_serviceability_is_signed_penalising_a_step_into_worse_danger() {
        let here = raddr(1.0);
        let ns = here.neighbors();
        let safer = ns[0].clone();
        let worse = ns[1].clone();
        let t = PlantedTerrain::hazard(
            std::iter::empty(),
            [
                (here.clone(), 0.5),
                (safer.clone(), 0.1),
                (worse.clone(), 0.9),
            ],
        );
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
        };
        let view = view_at(here);
        // Toward safety: positive (0.5 − 0.1).
        assert!(
            danger.serviceability(&Action::MoveTo(safer), &view, PLAN_BUDGET) > 0.0,
            "a step toward safety is served"
        );
        // Into worse danger: NEGATIVE (0.5 − 0.9) — the unclamped modulation.
        assert!(
            danger.serviceability(&Action::MoveTo(worse), &view, PLAN_BUDGET) < 0.0,
            "a step into worse danger is penalised (signed serviceability)"
        );
    }

    #[test]
    fn danger_routes_a_thirsty_creature_around_a_hazard_to_water() {
        // THE KEYSTONE (the potential-field modulation): water lies past a
        // dangerous cell; a safe detour neighbour exists. The creature, though
        // thirsty and knowing the water, does NOT step onto the hazard — danger's
        // negative serviceability outweighs thirst's pull on that move.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let hazard = ns[0].clone(); // the direct step toward water, but deadly
        let detour = ns[1].clone(); // a safe alternative step
        let water = hazard.clone(); // believed water sits on/at the hazard cell
        let t = PlantedTerrain::hazard([water.clone()], [(hazard.clone(), 1.0)]);
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
        };
        let thirst = Thirst { params: SUSTENANCE };
        let view = Perceived {
            position: home.clone(),
            drive: 0.9, // very thirsty
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: Some(detour.clone()),
        };
        let drives: [&dyn Drive; 2] = [&thirst, &danger];
        let res = arb(
            &view,
            &home,
            &drives,
            1.0, // weigh (both drives counted)
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        // Whatever it does, it must NOT step onto the deadly hazard cell.
        assert_ne!(
            res.intent,
            Intent::Do(Action::MoveTo(hazard.clone())),
            "a thirsty creature refuses to cross a lethal hazard even toward water"
        );
    }

    #[test]
    fn danger_urgency_is_clamped_and_a_flow_drive_carries_no_state() {
        // A flow drive: urgency is purely the cell field, no fold, clamped [0,1].
        let cell = raddr(1.0);
        let t = PlantedTerrain::hazard(std::iter::empty(), [(cell.clone(), 1.5)]);
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
        };
        assert_eq!(
            danger.urgency(&view_at(cell)),
            1.0,
            "threat urgency clamps at 1.0"
        );
    }

    #[test]
    fn boldness_scales_the_felt_threat_across_the_mettle_axis() {
        // THE METTLE: `effective = base × 2(1 − boldness)`, centered on 0.5.
        let cell = raddr(1.0);
        let t = PlantedTerrain::hazard(std::iter::empty(), [(cell.clone(), 0.4)]);
        let v = view_at(cell);
        let feel = |boldness: f64| {
            Danger {
                terrain: &t,
                threat_niche: mortal_threat_niche(),
                boldness,
                alarm: None,
            }
            .urgency(&v)
        };
        // Steady (0.5) → ×1 (unchanged, the inert baseline).
        assert_eq!(feel(0.5), 0.4, "steady feels the threat as it is");
        // Bold (0.8) → ×0.4.
        assert!(
            (feel(0.8) - 0.4 * 0.4).abs() < 1e-9,
            "a bold creature fears less"
        );
        // Fearless (1.0) → ×0.
        assert_eq!(feel(1.0), 0.0, "the fearless feel nothing");
        // Coward (0.0) → ×2, clamped at 1.0.
        assert_eq!(
            feel(0.0),
            (0.4_f64 * 2.0).min(1.0),
            "a coward fears more (clamped)"
        );
        // The monotone axis: coward > steady > bold > fearless.
        assert!(feel(0.0) > feel(0.5) && feel(0.5) > feel(0.8) && feel(0.8) > feel(1.0));
    }

    #[test]
    fn alarm_raises_a_calm_creatures_danger() {
        // THE ALARM: a creature on hazard-free ground feels nothing of its own,
        // but a borrowed alarm at its cell wakes its Danger drive additively.
        let cell = raddr(1.0);
        let t = PlantedTerrain::default(); // no hazard anywhere — nothing to fear
        let mut map: std::collections::BTreeMap<RoomAddr, f64> = std::collections::BTreeMap::new();
        map.insert(cell.clone(), 0.8);
        let feel = |alarm: Option<&std::collections::BTreeMap<RoomAddr, f64>>| {
            Danger {
                terrain: &t,
                threat_niche: mortal_threat_niche(),
                boldness: BOLDNESS_STEADY,
                alarm,
            }
            .urgency(&view_at(cell.clone()))
        };
        let felt = feel(Some(&map));
        assert!(felt > 0.0, "borrowed alarm raises felt threat above zero");
        assert!(
            felt >= DANGER_ACT,
            "a full-strength alarm crosses the danger act threshold"
        );
        assert_eq!(
            feel(None),
            0.0,
            "with no alarm field a calm creature on safe ground fears nothing"
        );
    }

    #[test]
    fn borrowed_alarm_is_scaled_by_boldness() {
        // THE ALARM reuses THE METTLE's dial: borrowed fear is scaled by the
        // reader's own `mettle_factor`, so a bold creature shrugs off the herd's
        // panic exactly as it shrugs off a hazard.
        let cell = raddr(1.0);
        let t = PlantedTerrain::default();
        let mut map: std::collections::BTreeMap<RoomAddr, f64> = std::collections::BTreeMap::new();
        map.insert(cell.clone(), 0.8);
        let feel = |boldness: f64| {
            Danger {
                terrain: &t,
                threat_niche: mortal_threat_niche(),
                boldness,
                alarm: Some(&map),
            }
            .urgency(&view_at(cell.clone()))
        };
        // Bold < steady < coward — the monotone Mettle ordering, borrowed.
        assert!(
            feel(0.9) < feel(0.5) && feel(0.5) < feel(0.1),
            "a bold creature feels less of the borrowed alarm than a coward"
        );
        // A bold omnivore shrugs the herd off — its borrowed alarm stays below act.
        assert!(
            feel(0.9) < DANGER_ACT,
            "a bold creature's borrowed alarm falls below the danger act threshold"
        );
    }

    #[test]
    fn alarm_is_additive_over_terrain_hazard() {
        // THE ALARM is ADDITIVE: on mildly hazardous ground the borrowed alarm
        // stacks on the creature's own felt threat, strictly above either alone.
        let cell = raddr(1.0);
        let t = PlantedTerrain::hazard(std::iter::empty(), [(cell.clone(), 0.2)]);
        let mut map: std::collections::BTreeMap<RoomAddr, f64> = std::collections::BTreeMap::new();
        map.insert(cell.clone(), 0.5);
        let both = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: Some(&map),
        }
        .urgency(&view_at(cell.clone()));
        let terrain_only = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
        }
        .urgency(&view_at(cell.clone()));
        // With ALARM_SCALE = 1.0 and steady boldness: 0.2 + 0.5 = 0.7.
        assert!(
            (both - 0.7).abs() < 1e-9,
            "felt threat is base + ALARM_SCALE * alarm"
        );
        assert!(
            both > terrain_only && both > 0.5,
            "the sum is strictly above either the terrain hazard or the alarm alone"
        );
    }

    /// A steady mortal NPC placed (via `commit_agent_at`) at `pos`, minted into
    /// `ledger` — the common emitter/reader for the `alarm_field` tests.
    /// `boldness` dials whether it is primary-afraid on hazard ground.
    fn alarm_npc(ledger: &mut Ledger, reg: &ConceptRegistry, pos: &RoomAddr, boldness: f64) -> Npc {
        let e = ledger.mint_entity();
        commit_agent_at(ledger, reg, e, pos, 0.0);
        Npc {
            entity: e,
            home: pos.clone(),
            resource: pos.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness,
            threat_niche: mortal_threat_niche(),
            label: "creature".into(),
        }
    }

    #[test]
    fn alarm_field_is_empty_with_no_primary_fear() {
        // THE ALARM: a settled population on hazard-free ground raises no alarm —
        // the field is empty, the byte-identical resting state.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let a = raddr(1.0);
        let b = a.neighbors()[0].clone();
        let npc_a = alarm_npc(&mut ledger, &reg, &a, BOLDNESS_STEADY);
        let npc_b = alarm_npc(&mut ledger, &reg, &b, BOLDNESS_STEADY);
        let terrain = PlantedTerrain::default(); // no hazard anywhere
        let field = alarm_field(&ledger, &[npc_a, npc_b], &terrain, WorldTime { day: 0.5 });
        assert!(
            field.is_empty(),
            "no creature is primary-afraid, so the alarm field is empty: {field:?}"
        );
    }

    #[test]
    fn alarm_field_haloes_a_primary_afraid_creature() {
        // THE ALARM: one creature on an UNCANNY-hazard cell (its Danger crosses
        // act) stamps a one-hop halo — its cell and its three neighbours carry
        // alarm in [0, 1]; a distant cell is untouched.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let cell = raddr(1.0);
        let ns = cell.neighbors();
        let far = raddr(-1.0); // the far side of the world, outside the halo
        let npc = alarm_npc(&mut ledger, &reg, &cell, BOLDNESS_STEADY);
        // A full-strength uncanny hazard ONLY on the creature's cell.
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(cell.clone(), 0.8)]);
        let field = alarm_field(&ledger, &[npc], &terrain, WorldTime { day: 0.5 });
        for room in std::iter::once(&cell).chain(ns.iter()) {
            let v = field.get(room).copied().unwrap_or(0.0);
            assert!(
                v > 0.0 && v <= 1.0,
                "the halo cell {room:?} carries alarm in (0, 1]: {v}"
            );
        }
        assert!(
            !field.contains_key(&far),
            "a cell far from the distress carries no alarm"
        );
    }

    #[test]
    fn alarm_field_does_not_re_emit() {
        // THE ALARM's termination guarantee (built alarm-free): a creature A on
        // genuine hazard ground is primary-afraid and emits; a BOLD creature B on
        // an adjacent cell shrugs the hazard off (its own terrain danger is below
        // act) and so contributes NOTHING — borrowed alarm is never re-emitted.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let h = raddr(1.0); // A's hazard cell
        let ns = h.neighbors();
        let b_cell = ns[0].clone(); // B sits one hop from A, inside A's halo
        // A is a coward (feels the hazard fully); B is bold (shrugs it off).
        let a = alarm_npc(&mut ledger, &reg, &h, BOLDNESS_STEADY);
        let b = alarm_npc(&mut ledger, &reg, &b_cell, 0.95);
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(h.clone(), 0.8)]);
        // The field over BOTH creatures.
        let both = alarm_field(
            &ledger,
            &[a.clone(), b.clone()],
            &terrain,
            WorldTime { day: 0.5 },
        );
        // The field over A ALONE — the reference: B must add nothing.
        let a_only = alarm_field(&ledger, &[a], &terrain, WorldTime { day: 0.5 });
        assert_eq!(
            both, a_only,
            "the bold neighbour B is not primary-afraid, so it re-emits no alarm"
        );
        // And B's own neighbours OUTSIDE A's halo are untouched — no secondary wave.
        for n in b_cell.neighbors() {
            if n != h && !ns.contains(&n) {
                assert!(
                    !both.contains_key(&n),
                    "B does not stamp its own halo: {n:?} must be absent"
                );
            }
        }
    }

    #[test]
    fn the_herd_bolts_borrowed_alarm_makes_a_calm_creature_flee_then_settle() {
        // THE ALARM, end-to-end (the spec's e2e criterion): drive the REAL
        // field-aware `DriveMovements` tick, not a hand-built affect. Creature A
        // is CORNERED on genuine UNCANNY hazard ground (its cell and every
        // neighbour are hazardous, so no step is strictly safer — it holds, and
        // keeps screaming every tick). Creature B stands one hop away, INSIDE
        // A's alarm halo, but dreads the uncanny only WEAKLY (a low threat-niche
        // weight), so its OWN terrain-sourced danger stays below `act`: B has NO
        // primary fear of its own. Yet the BORROWED alarm at B's cell pushes it
        // over `act`, and B flees down the local threat gradient to safe ground
        // OUTSIDE the halo — then, separated from the distress, it settles. The
        // wave is bounded and terminates (spec §3): no perpetual stampede.

        // A registry carrying every predicate the tick may commit.
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();

        // Geometry, read from the real mesh so the scenario is topology-robust.
        let x = raddr(1.0); // A's cell — the core of the hazard
        let ns = x.neighbors(); // A's three edge-neighbours
        let b_start = ns[0].clone(); // B stands here: one hop from A, in the halo
        // The hazard patch = A's cell AND its neighbours, so A is boxed in (no
        // neighbour is strictly safer → cornered, holds, keeps emitting).
        let patch: std::collections::BTreeSet<RoomAddr> = std::iter::once(x.clone())
            .chain(ns.iter().cloned())
            .collect();
        // B's escape: a neighbour of B's cell OUTSIDE the patch — and thus
        // outside A's one-hop halo. The mesh gives B such a way out; assert it.
        let escape: std::collections::BTreeSet<RoomAddr> = b_start
            .neighbors()
            .into_iter()
            .filter(|n| !patch.contains(n))
            .collect();
        assert!(
            !escape.is_empty(),
            "B must have a hop out of the halo for the wave to terminate"
        );
        // `flee_step` (and arbitration) pick the safest neighbour, ties to the
        // smallest RoomAddr — among the equally-safe escape cells that is the
        // minimum. Make it B's home, so B flees home to safety and rests there
        // (no home-ward pull back into the halo → no oscillation).
        let b_home = escape.iter().min().unwrap().clone();

        // A strong UNCANNY hazard over the whole patch.
        let terrain =
            PlantedTerrain::hazard(std::iter::empty(), patch.iter().cloned().map(|c| (c, 0.8)));

        // A — a mortal dreading the uncanny fully (weight 1), steady boldness:
        // felt threat 0.8 ≥ DANGER_ACT, cornered, emits arousal 0.8 every tick.
        let build_a = |ledger: &mut Ledger| -> Npc {
            let e = ledger.mint_entity();
            commit_agent_at(ledger, &reg, e, &x, 0.0);
            Npc {
                entity: e,
                home: x.clone(),
                resource: x.clone(),
                species: "goblin".into(),
                activity: hornvale_species::ActivityCycle::Diurnal,
                temperature_niche: test_niche(),
                deliberation_latency: 0.5,
                time_horizon: 0.0,
                metabolic_class: MetabolicClass::Endotherm,
                niche: default_diet_niche(),
                boldness: BOLDNESS_STEADY,
                threat_niche: mortal_threat_niche(),
                label: "cornered".into(),
            }
        };
        // B — dreads the uncanny only WEAKLY (0.25), so 0.8·0.25 = 0.20 <
        // DANGER_ACT (0.3): NO primary fear of its own. Its home is the safe
        // escape cell it flees to.
        let build_b = |ledger: &mut Ledger| -> Npc {
            let e = ledger.mint_entity();
            commit_agent_at(ledger, &reg, e, &b_start, 0.0);
            Npc {
                entity: e,
                home: b_home.clone(),
                resource: b_home.clone(),
                species: "goblin".into(),
                activity: hornvale_species::ActivityCycle::Diurnal,
                temperature_niche: test_niche(),
                deliberation_latency: 0.5,
                time_horizon: 0.0,
                metabolic_class: MetabolicClass::Endotherm,
                niche: default_diet_niche(),
                boldness: BOLDNESS_STEADY,
                threat_niche: ThreatNiche {
                    uncanny: 0.25,
                    heat: 0.0,
                    cold: 0.0,
                    predator: 0.0,
                },
                label: "herd-edge".into(),
            }
        };

        // --- The contagion run: A present and cornered, B one hop away. ---
        let mut ledger = Ledger::default();
        let a = build_a(&mut ledger);
        let b = build_b(&mut ledger);
        let a_entity = a.entity;
        let b_entity = b.entity;

        // TICK 1 — the daytime window (frac 0.30 → 0.40, both awake). The alarm
        // field haloes A's neighbourhood (B's cell included), so B bolts.
        let sys1 = DriveMovements {
            npcs: vec![a.clone(), b.clone()],
            from: WorldTime { day: 0.30 },
            to: WorldTime { day: 0.40 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let after1 = hornvale_kernel::tick(&ledger, &[&sys1], &["drive-movements"], &reg).unwrap();

        // B's actual tick STEPS (excluding the day-0 seed placement).
        let b_moves_1: Vec<&Fact> = after1
            .find(AGENT_AT)
            .filter(|f| f.subject == b_entity && f.provenance != "test")
            .collect();
        let b_fear_moves = b_moves_1
            .iter()
            .filter(|f| f.provenance.contains("fear"))
            .count();
        assert!(
            b_fear_moves >= 1,
            "B, with no primary fear of its own, FLEES the borrowed alarm (a fear-provenance move)"
        );
        // Bounded — no perpetual within-tick stampede (the field is fixed across
        // the interval; an oscillating B would run to MAX_STEPS, committing
        // thousands of moves). A small count proves the wave is a bounded halo.
        assert!(
            b_moves_1.len() <= 4,
            "B's flight is bounded, not a runaway stampede: got {} moves",
            b_moves_1.len()
        );
        // A is cornered — it never STEPS during the tick (only the day-0 seed
        // placement, provenance "test", is on the ledger). It holds and keeps
        // emitting, the persistent alarm source.
        assert_eq!(
            after1
                .find(AGENT_AT)
                .filter(|f| f.subject == a_entity && f.provenance != "test")
                .count(),
            0,
            "the cornered A holds its ground, the persistent alarm source"
        );

        // TICK 2 — A still cornered and screaming; B now on safe ground OUTSIDE
        // the halo. The alarm no longer reaches B, so it settles: no new move.
        // TERMINATION (spec §3): the wave dies not because the source vanished
        // (A still screams) but because B escaped the one-hop halo.
        let sys2 = DriveMovements {
            npcs: vec![a.clone(), b.clone()],
            from: WorldTime { day: 0.40 },
            to: WorldTime { day: 0.55 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let after2 = hornvale_kernel::tick(&after1, &[&sys2], &["drive-movements"], &reg).unwrap();
        let b_moves_2 = after2
            .find(AGENT_AT)
            .filter(|f| f.subject == b_entity && f.provenance != "test")
            .count();
        assert_eq!(
            b_moves_2,
            b_moves_1.len(),
            "once out of the halo B settles — the wave terminates (no perpetual stampede)"
        );

        // --- The control run: B alone (A absent). ---
        // With no primary-afraid neighbour the alarm field is EMPTY, so B feels
        // nothing borrowed and never flees. (It may amble home, but never with
        // the fear provenance — the flight was borrowed, not intrinsic.)
        let mut control = Ledger::default();
        let cb = build_b(&mut control);
        let cb_entity = cb.entity;
        let csys = DriveMovements {
            npcs: vec![cb.clone()],
            from: WorldTime { day: 0.30 },
            to: WorldTime { day: 0.40 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let cafter = hornvale_kernel::tick(&control, &[&csys], &["drive-movements"], &reg).unwrap();
        let c_fear = cafter
            .find(AGENT_AT)
            .filter(|f| f.subject == cb_entity && f.provenance.contains("fear"))
            .count();
        assert_eq!(
            c_fear, 0,
            "with no distressed neighbour present, B never flees — the flight was borrowed"
        );
    }

    #[test]
    fn the_threat_niche_is_derived_from_nature() {
        // THE BANE: HEAT/COLD derive from the temperature optimum, UNCANNY from
        // the metabolic class.
        let cold_adapted = ConditionResponse {
            optimum: -10.0,
            width: 20.0,
            devotion: 0.5,
        };
        let warm_adapted = ConditionResponse {
            optimum: 25.0,
            width: 20.0,
            devotion: 0.5,
        };
        let cold = derive_threat_niche(
            &cold_adapted,
            MetabolicClass::Endotherm,
            &default_diet_niche(),
        );
        let warm = derive_threat_niche(
            &warm_adapted,
            MetabolicClass::Endotherm,
            &default_diet_niche(),
        );
        // A cold-adapted creature dreads HEAT more than a warm one; the reverse
        // for COLD.
        assert!(cold.heat > warm.heat, "the cold-adapted fear heat more");
        assert!(warm.cold > cold.cold, "the warm-adapted fear cold more");
        // A mortal fears the uncanny; an Ametabolic elemental does not.
        assert_eq!(cold.uncanny, 1.0, "a mortal fears the eldritch");
        let elemental = derive_threat_niche(
            &cold_adapted,
            MetabolicClass::Ametabolic,
            &default_diet_niche(),
        );
        assert_eq!(elemental.uncanny, 0.0, "an elemental IS the eldritch");
    }

    #[test]
    fn two_species_read_the_same_hot_cell_differently() {
        // THE BANE, per-kind fear: a HOT cell dreaded by a cold-adapted creature,
        // shrugged off by a heat-adapted one — the niche·hazard dot.
        let cell = raddr(1.0);
        let t = PlantedTerrain::hazards_map([(
            cell.clone(),
            Hazards {
                uncanny: 0.0,
                heat: 0.8,
                cold: 0.0,
                predator: 0.0,
            },
        )]);
        let v = view_at(cell.clone());
        let cold_adapted = derive_threat_niche(
            &ConditionResponse {
                optimum: -10.0,
                width: 20.0,
                devotion: 0.5,
            },
            MetabolicClass::Endotherm,
            &default_diet_niche(),
        );
        let warm_adapted = derive_threat_niche(
            &ConditionResponse {
                optimum: 45.0,
                width: 20.0,
                devotion: 0.5,
            },
            MetabolicClass::Endotherm,
            &default_diet_niche(),
        );
        let fears = Danger {
            terrain: &t,
            threat_niche: cold_adapted,
            boldness: BOLDNESS_STEADY,
            alarm: None,
        };
        let shrugs = Danger {
            terrain: &t,
            threat_niche: warm_adapted,
            boldness: BOLDNESS_STEADY,
            alarm: None,
        };
        assert!(
            fears.urgency(&v) > shrugs.urgency(&v),
            "the cold-adapted creature dreads the heat the warm one shrugs off: \
             {} vs {}",
            fears.urgency(&v),
            shrugs.urgency(&v)
        );
        // A creature fearless of a hazard (weight 0) feels nothing there.
        assert_eq!(
            shrugs.urgency(&v),
            0.0,
            "a fully heat-adapted creature (heat weight 0) feels no heat-dread"
        );
    }

    #[test]
    fn the_predator_weight_derives_from_carnivory() {
        // THE QUARRY (the eater-eaten link): a herbivore fears predators, an
        // obligate apex does not — `(1 − carnivory) × latent scale`.
        let temp = DEFAULT_TEMPERATURE_NICHE;
        let herbivore = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
        let omnivore = ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)]).unwrap();
        let apex = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap();
        let w = |diet: &ResourceVector| {
            derive_threat_niche(&temp, MetabolicClass::Endotherm, diet).predator
        };
        assert!(
            (w(&herbivore) - PREDATOR_LATENT_SCALE).abs() < 1e-9,
            "a herbivore fears predators"
        );
        assert!(w(&omnivore) < w(&herbivore), "an omnivore fears them less");
        assert_eq!(
            w(&apex),
            0.0,
            "an obligate apex does not fear predators — it IS one"
        );
    }

    #[test]
    fn a_vulnerable_creature_dreads_predator_ground_an_apex_does_not() {
        // THE QUARRY, per-kind biotic fear: a HIGH-predator cell dreaded by a
        // (vulnerable, coward-to-amplify-the-latent) herbivore, ignored by an apex.
        let cell = raddr(1.0);
        let t = PlantedTerrain::hazards_map([(
            cell.clone(),
            Hazards {
                uncanny: 0.0,
                heat: 0.0,
                cold: 0.0,
                predator: 1.0,
            },
        )]);
        let v = view_at(cell);
        let temp = DEFAULT_TEMPERATURE_NICHE;
        let herbivore = derive_threat_niche(
            &temp,
            MetabolicClass::Endotherm,
            &ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
        );
        let apex = derive_threat_niche(
            &temp,
            MetabolicClass::Endotherm,
            &ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
        );
        // A coward (boldness 0 → ×2) to lift the latent-scaled dread above act.
        let quarry = Danger {
            terrain: &t,
            threat_niche: herbivore,
            boldness: 0.0,
            alarm: None,
        };
        let hunter = Danger {
            terrain: &t,
            threat_niche: apex,
            boldness: 0.0,
            alarm: None,
        };
        assert!(
            quarry.urgency(&v) > 0.0,
            "the herbivore dreads predator ground: {}",
            quarry.urgency(&v)
        );
        assert_eq!(
            hunter.urgency(&v),
            0.0,
            "the apex feels no dread of predators — it is one"
        );
    }

    #[test]
    fn loneliness_is_zero_at_home_rises_with_distance_and_lapses_when_unreachable() {
        // The three regimes of the social pull (The Belonging).
        assert_eq!(
            loneliness_from_plan(&Some(vec![])),
            0.0,
            "at home (empty plan) → not lonely"
        );
        let ten: Vec<Action> = std::iter::repeat_n(Action::Drink, 10).collect();
        assert_eq!(
            loneliness_from_plan(&Some(ten)),
            10.0 / LONELY_SCALE_HOPS,
            "loneliness rises with the hop-distance home"
        );
        assert_eq!(
            loneliness_from_plan(&None),
            0.0,
            "home beyond reach → DORMANT (0), not distress — social is comfort, \
             an unreachable home is a relocation"
        );
    }

    #[test]
    fn social_affordance_and_serviceability_are_the_home_step() {
        let home = raddr(1.0);
        let step = home.neighbors()[0].clone();
        let social = Social {
            loneliness: 0.9,
            home_step: Some(Action::MoveTo(step.clone())),
        };
        let view = view_at(home.neighbors()[1].clone());
        assert_eq!(
            social.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(step.clone())),
            "the affordance is the precomputed step home"
        );
        assert_eq!(
            social.serviceability(&Action::MoveTo(step), &view, PLAN_BUDGET),
            1.0,
            "the home-step is served"
        );
        assert_eq!(
            social.serviceability(&Action::Drink, &view, PLAN_BUDGET),
            0.0,
            "nothing else eases loneliness"
        );
    }

    #[test]
    fn a_lonely_creature_yields_to_thirst_but_homes_when_sated() {
        // COMFORT-tier: a thirsty AND lonely creature attends thirst first
        // (survival > comfort); sated, it heads home.
        let home = raddr(1.0);
        let pos = home.neighbors()[2].clone(); // one hop from home
        let water = home.neighbors()[1].clone();
        // The real home-step from `pos` (a genuine neighbour of `pos`).
        let plan = plan_to_room(&pos, &home, PLAN_BUDGET, &std::collections::BTreeSet::new());
        let step = plan.clone().and_then(|p| p.into_iter().next()).unwrap();
        let social = Social {
            loneliness: 0.9,
            home_step: Some(step.clone()),
        };
        let thirst = Thirst { params: SUSTENANCE };
        let drives: [&dyn Drive; 2] = [&thirst, &social];
        // Thirsty (drive past act) and knows nearby water: thirst wins.
        let thirsty = Perceived {
            position: pos.clone(),
            drive: 0.95,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let r = arb(
            &thirsty,
            &home,
            &drives,
            1.0,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(
            r.affect.object,
            Some(DriveKind::Thirst),
            "survival thirst outranks comfort loneliness: {:?}",
            r.affect
        );
        // Sated (thirst below act): only social active → heads home.
        let sated = Perceived {
            position: pos.clone(),
            drive: 0.0,
            fatigue: 0.0,
            believed_water: None,
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let r2 = arb(
            &sated,
            &home,
            &drives,
            1.0,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(
            r2.intent,
            Intent::Do(step),
            "sated, the lonely creature heads home: {:?}",
            r2.affect
        );
        assert_eq!(r2.affect.object, Some(DriveKind::Social));
    }

    #[test]
    fn an_ametabolic_creature_is_never_lonely() {
        // THE METABOLISM GATE, social edge (The Belonging): an Ametabolic
        // creature carries no social drive — placed far from home it still reads
        // Content, where a metabolizer would head home.
        let home = raddr(1.0);
        let away = raddr(-1.0); // far side of the world (home reachable within budget)
        let terrain = PlantedTerrain::fresh_only(std::iter::empty());
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let reg = {
            let mut r = agent_at_reg();
            r.register_predicate(DRANK, false, "drank").unwrap();
            r.register_predicate(RESTED, false, "rested").unwrap();
            r
        };
        commit_agent_at(&mut ledger, &reg, e, &away, 0.0);
        let base = Npc {
            entity: e,
            home: home.clone(),
            resource: home.clone(),
            species: "xorn".to_string(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Ametabolic,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "xorn".to_string(),
        };
        let a = affect_of(&ledger, &base, &[], WorldTime { day: 0.5 }, &terrain);
        assert_eq!(
            a.label,
            AffectLabel::Content,
            "a construct far from home is not lonely: {a:?}"
        );
    }

    #[test]
    fn is_water_delegates_to_terrain_is_fresh_water() {
        // `raddr(seed)` feeds `RoomAddr::containing([seed, 0.0, 0.0], 6)`, which
        // normalizes its input direction first — so `raddr(1.0)` and `raddr(2.0)`
        // collapse to the SAME room (both are the direction [1,0,0]). Use a
        // genuine mesh neighbor for `high` instead, so the two planted rooms
        // are actually distinct (deviation from the brief's literal `raddr(2.0)`;
        // see task-1-report.md). Renamed from
        // `is_water_is_the_elevation_threshold` (T5 re-wire): `is_water` no
        // longer reads elevation at all — it delegates to
        // `Terrain::is_fresh_water` (The Freshet's salt/fresh classification).
        let low = raddr(1.0);
        let high = low.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([low.clone()]);
        assert!(is_water(&low, &t));
        assert!(!is_water(&high, &t));
    }

    #[test]
    fn downhill_step_picks_the_lowest_neighbor_deterministically() {
        let home = raddr(1.0);
        let ns = home.neighbors();
        // Make ns[1] strictly lowest; others high.
        let mut m = std::collections::BTreeMap::new();
        for (i, n) in ns.iter().enumerate() {
            m.insert(n.clone(), if i == 1 { 0.0 } else { 100.0 });
        }
        let t = PlantedTerrain::dry(m);
        assert_eq!(downhill_step(&home, &t), ns[1]);
    }

    #[test]
    fn nearest_water_finds_the_closest_water_room_by_hops() {
        // home (dry) -> a neighbor that is fresh water: 1 hop.
        let home = raddr(1.0);
        let near = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([near.clone()]);
        assert_eq!(nearest_water(&home, &t, 10_000), Some(near));
    }

    #[test]
    fn nearest_water_returns_from_itself_when_already_on_water() {
        let here = raddr(1.0);
        let t = PlantedTerrain::fresh_only([here.clone()]);
        assert_eq!(nearest_water(&here, &t, 10_000), Some(here));
    }

    #[test]
    fn nearest_water_gives_up_within_budget_when_no_water() {
        let home = raddr(1.0); // no fresh water anywhere
        let t = PlantedTerrain::fresh_only(std::iter::empty());
        assert_eq!(nearest_water(&home, &t, 50), None);
    }

    #[test]
    fn two_agents_believe_different_sources_from_their_histories_and_beeline_differently() {
        // THE MULTI-SOURCE KEYSTONE (destination divergence): two NPCs, same home,
        // thirst, world — differing ONLY in a pre-seeded agent-at (perceived source).
        // A knows the near source W1; B knows the far source W2. Each beelines to its
        // OWN believed source. Belief ignored ⇒ both go to the same true-nearest ⇒
        // this fails.
        let reg = {
            let mut r = agent_at_reg();
            r.register_predicate(DRANK, false, "drank").unwrap();
            r.register_predicate(RESTED, false, "rested").unwrap();
            r.register_predicate(EATEN, false, "eaten").unwrap();
            r
        };
        let home = raddr(1.0);
        let w1 = home.neighbors()[0].clone(); // near source
        let w2 = home.neighbors()[1]
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone(); // far source
        let terrain = PlantedTerrain::fresh_only([w1.clone(), w2.clone()]);
        let run = |seed_room: &RoomAddr| -> Vec<RoomAddr> {
            let mut ledger = Ledger::default();
            let e = ledger.mint_entity();
            // The prior sighting (day 0), THEN a return-home (day 0.5): history holds
            // the sighting (→ belief) but the agent's current position is home, not
            // the water. (Position = latest agent-at; belief = the fold over history.)
            commit_agent_at(&mut ledger, &reg, e, seed_room, 0.0);
            commit_agent_at(&mut ledger, &reg, e, &home, 0.5);
            let npc = Npc {
                entity: e,
                home: home.clone(),
                resource: w1.clone(),
                species: "goblin".into(),
                activity: hornvale_species::ActivityCycle::Diurnal,
                temperature_niche: test_niche(),
                deliberation_latency: 0.5,
                time_horizon: 0.0,
                metabolic_class: MetabolicClass::Endotherm,
                niche: default_diet_niche(),
                boldness: 0.5,
                threat_niche: mortal_threat_niche(),
                label: "h".into(),
            };
            // from > both seed days so the frozen ledger holds no future facts and the
            // agent starts at home, not yet thirsty.
            let sys = DriveMovements {
                npcs: vec![npc],
                from: WorldTime { day: 1.0 },
                to: WorldTime { day: 41.0 },
                params: SUSTENANCE,
                terrain: &terrain,
            };
            let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
            // the rooms it drank at (its believed destinations)
            next.find(DRANK)
                .filter(|f| f.subject == e)
                .filter_map(|f| f.day)
                .filter_map(|d| {
                    next.find(AGENT_AT)
                        .filter(|g| g.subject == e)
                        .filter(|g| g.day.is_some_and(|gd| gd <= d))
                        .filter_map(|g| match &g.object {
                            Value::Text(s) => Some((g.day.unwrap(), room_from_text(s))),
                            _ => None,
                        })
                        .max_by(|a, b| a.0.total_cmp(&b.0))
                        .map(|(_, r)| r)
                })
                .collect()
        };
        let a_dests = run(&w1);
        let b_dests = run(&w2);
        assert!(
            a_dests.iter().all(|r| *r == w1),
            "A (knows W1) drinks at W1: {a_dests:?}"
        );
        assert!(
            b_dests.iter().all(|r| *r == w2),
            "B (knows W2) drinks at W2: {b_dests:?}"
        );
        assert_ne!(w1, w2);
    }

    #[test]
    fn an_ignorant_agent_discovers_water_then_later_beelines() {
        // DISCOVERY: a fresh NPC (no perceived water) explores downhill, finds water,
        // drinks; belief now formed, a later thirst cycle beelines. The first journey
        // (exploration) differs from the later (beeline).
        let reg = {
            let mut r = agent_at_reg();
            r.register_predicate(DRANK, false, "drank").unwrap();
            r.register_predicate(RESTED, false, "rested").unwrap();
            r.register_predicate(EATEN, false, "eaten").unwrap();
            r
        };
        // A downhill chain home(100) -> a(50) -> water(fresh); other neighbors high
        // (elevation still steers the exploration prior; fresh-water truth is
        // now a separate planted set — the-surmise T5 re-wire).
        let home = raddr(1.0);
        let a = home.neighbors()[0].clone();
        let water = a.neighbors().iter().find(|n| **n != home).unwrap().clone();
        let mut m = std::collections::BTreeMap::new();
        m.insert(home.clone(), 100.0);
        m.insert(a.clone(), 50.0);
        m.insert(water.clone(), 10.0); // still the lowest, so exploration steps onto it
        let terrain = PlantedTerrain {
            elevations: m,
            fresh: [water.clone()].into_iter().collect(),
            ..Default::default()
        };
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "h".into(),
        };
        let sys = DriveMovements {
            npcs: vec![npc.clone()],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        // It drank at least twice (multiple cycles) and reached the water room.
        let drinks = next.find(DRANK).filter(|f| f.subject == e).count();
        assert!(
            drinks >= 2,
            "discovered water and drank across cycles: {drinks}"
        );
        // Belief formed: after the run, believed_water is the discovered source.
        assert_eq!(
            believed_water(&next, &npc, WorldTime { day: 40.0 }, &terrain, PLAN_BUDGET),
            Some(water)
        );
        let _ = ledger;
    }

    // --- The thermal comfort drive (Stage 1, a flow-drive in ISOLATION). ---

    /// A warm-blooded niche (optimum ~18 °C) with a modest tolerance band —
    /// authored with a narrow width so the discrimination against the cold
    /// niche is clean (the real goblin niche's very wide width tolerates
    /// nearly everything, which can't demonstrate flight vs. tolerance).
    fn warm_niche() -> ConditionResponse {
        ConditionResponse {
            optimum: 18.0,
            width: 8.0,
            devotion: 0.45,
        }
    }
    /// A cold-adapted niche (optimum ~6 °C), same tolerance band as
    /// [`warm_niche`] so the two differ only in setpoint.
    fn cold_niche() -> ConditionResponse {
        ConditionResponse {
            optimum: 6.0,
            width: 8.0,
            devotion: 0.85,
        }
    }
    /// The zero-drive, ignorant view a flow-drive test reads (thirst state is
    /// irrelevant to the thermal drive — it senses temperature at `position`).
    fn at(position: RoomAddr) -> Perceived {
        Perceived {
            position,
            drive: 0.0,
            fatigue: 0.0,
            believed_water: None,
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        }
    }

    #[test]
    fn thermal_affordance_steps_toward_the_comfortable_neighbor_both_directions() {
        // TOO COLD → warmer neighbour; TOO HOT → cooler neighbour; both toward
        // the optimum. The comfort step is the neighbour whose temperature is
        // CLOSEST to the optimum, exactly as `downhill_step` picks the lowest.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };

        // Too COLD: home at −10 (dev 28 past optimum 18), ns[0] warmest/closest.
        let cold_here = PlantedTerrain::thermal([
            (home.clone(), -10.0),
            (ns[0].clone(), 10.0),  // dev 8  → closest to optimum
            (ns[1].clone(), -30.0), // dev 48
            (ns[2].clone(), -20.0), // dev 38
        ]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &cold_here,
            day,
        };
        let view = at(home.clone());
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(ns[0].clone())),
            "too cold: steps toward the warmer neighbour"
        );
        assert!(
            drive.urgency(&view) > drive.act_threshold(),
            "a 28 °C deviation is well past the act threshold"
        );

        // Too HOT: home at 40 (dev 22), ns[0] coolest/closest to optimum 18.
        let hot_here = PlantedTerrain::thermal([
            (home.clone(), 40.0),
            (ns[0].clone(), 20.0), // dev 2  → closest to optimum
            (ns[1].clone(), 60.0), // dev 42
            (ns[2].clone(), 50.0), // dev 32
        ]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &hot_here,
            day,
        };
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(ns[0].clone())),
            "too hot: steps toward the cooler neighbour"
        );
    }

    #[test]
    fn thermal_within_tolerance_is_satisfied_no_urgency_no_step() {
        // Inside the tolerance band: urgency is exactly 0 (< act) and there is
        // no affordance (nothing to do), even though a strictly-more-optimal
        // neighbour exists — comfort is a satisfied state, not a maximizer.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        let t = PlantedTerrain::thermal([
            (home.clone(), 20.0),  // dev 2 ≤ width 8 → comfortable
            (ns[0].clone(), 18.0), // exactly optimal, but we don't chase it
        ]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
        };
        let view = at(home.clone());
        assert_eq!(drive.urgency(&view), 0.0, "inside the band → zero urgency");
        assert!(drive.urgency(&view) < drive.act_threshold());
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            None,
            "comfortable → no comfort step"
        );
    }

    #[test]
    fn thermal_respects_the_niche_cold_tolerates_what_warm_flees() {
        // NICHE RESPECT: the SAME cell (a cold 2 °C room) is tolerated by a
        // cold-adapted niche (optimum 6, dev 4 ≤ 8) but fled by a warm one
        // (optimum 18, dev 16 > 8). Different setpoint → different verdict.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        let t = PlantedTerrain::thermal([
            (home.clone(), 2.0),
            (ns[0].clone(), 16.0), // warmer — the warm niche's comfort target
            (ns[1].clone(), -10.0),
            (ns[2].clone(), -20.0),
        ]);
        let view = at(home.clone());

        let cold = Thermal {
            niche: cold_niche(),
            terrain: &t,
            day,
        };
        assert_eq!(cold.urgency(&view), 0.0, "the cold niche tolerates 2 °C");
        assert_eq!(
            cold.affordance(&view, PLAN_BUDGET),
            None,
            "tolerated → the cold niche stays put"
        );

        let warm = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
        };
        assert!(warm.urgency(&view) > 0.0, "the warm niche flees 2 °C");
        assert_eq!(
            warm.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(ns[0].clone())),
            "the warm niche steps toward the warmer neighbour"
        );
    }

    #[test]
    fn thermal_urgency_and_affordance_are_deterministic_and_recompute_identically() {
        // Reload-stable by construction: the thermal drive reads only its held
        // niche + terrain + day (no ledger, no stored state), so recomputing
        // twice is byte-identical.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 3.5 };
        let t = PlantedTerrain::thermal([
            (home.clone(), -12.0),
            (ns[0].clone(), 4.0),
            (ns[1].clone(), -25.0),
            (ns[2].clone(), -18.0),
        ]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
        };
        let view = at(home.clone());
        assert_eq!(drive.urgency(&view), drive.urgency(&view));
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            drive.affordance(&view, PLAN_BUDGET)
        );
    }

    #[test]
    fn thermal_comfort_step_breaks_ties_by_ascending_room_addr() {
        // DETERMINISM UNDER A GENUINE TIE: two neighbours EQUIDISTANT from the
        // optimum (symmetric about it, 0 °C and 12 °C around optimum 6, both
        // dev 6) must resolve to the smaller-`RoomAddr` one — the same
        // `total_cmp` + ascending-`RoomAddr` tie-break `downhill_step` uses (cf.
        // `downhill_step_picks_the_lowest_neighbor_deterministically`).
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        let smaller = std::cmp::min(ns[0].clone(), ns[1].clone());
        let t = PlantedTerrain::thermal([
            (home.clone(), -40.0),  // dev 46 → outside the band, worse than either
            (ns[0].clone(), 0.0),   // dev 6
            (ns[1].clone(), 12.0),  // dev 6 → ties ns[0]
            (ns[2].clone(), -40.0), // dev 46 → not chosen
        ]);
        let drive = Thermal {
            niche: cold_niche(),
            terrain: &t,
            day,
        };
        let view = at(home.clone());
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(smaller)),
            "an equal-deviation tie resolves to the smaller RoomAddr"
        );
    }

    // --- Action-centric arbitration (Stage 2): thirst + thermal compete. ---

    /// A low-`act` sustenance drive so a MODERATE thirst urgency is already
    /// "active" — lets the arbitration tests put thirst and thermal in the same
    /// active window (real `SUSTENANCE.act` is `0.85`, above the thermal
    /// ceiling `0.6`, so the two rarely coexist active on real params; the
    /// mechanism is what these tests pin, per spec §5).
    fn eager_thirst() -> DriveParams {
        DriveParams {
            rise: 0.15,
            act: 0.4,
        }
    }

    #[test]
    fn affect_reads_each_circumplex_region() {
        // The affect label is a pure read of the same arbitration that chose the
        // action (spec §7). A comfortable-everywhere terrain keeps thermal
        // inactive so thirst is the sole drive under test.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let water = ns[0].clone();
        let far = raddr(9_999.0); // a believed source with no path within budget
        let day = WorldTime { day: 0.0 };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), 18.0),
            (ns[0].clone(), 18.0),
            (ns[1].clone(), 18.0),
            (ns[2].clone(), 18.0),
        ]);
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        let label = |view: &Perceived| {
            arb(
                view,
                &home,
                &drives,
                0.5,
                0.0,
                false,
                true,
                Mode::Idle,
                PLAN_BUDGET,
            )
            .affect
            .label
        };

        // CONTENT — not thirsty (drive < act): no active drive, puttering.
        assert_eq!(
            label(&Perceived {
                position: home.clone(),
                drive: 0.1,
                fatigue: 0.0,
                believed_water: Some(water.clone()),
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: None,
            }),
            AffectLabel::Content,
        );
        // EAGER — parched and standing at water: Drink satisfies (relief).
        assert_eq!(
            label(&Perceived {
                position: water.clone(),
                drive: 0.95,
                fatigue: 0.0,
                believed_water: Some(water.clone()),
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: None,
            }),
            AffectLabel::Eager,
        );
        // SEARCHING — parched, ignorant, a gradient to explore: approaching,
        // NOT confusion (the load-bearing exclusion from the distress metric).
        assert_eq!(
            label(&Perceived {
                position: home.clone(),
                drive: 0.95,
                fatigue: 0.0,
                believed_water: None,
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: Some(ns[2].clone()),
            }),
            AffectLabel::Searching,
        );
        // (FRUSTRATED — Hold while KNOWING where water is — is rare by design:
        // believed water is a cell the creature stood in, so it is almost always
        // reachable; it fires only when a known source falls beyond the plan
        // budget in a large world. The branch is `believed.is_some()` on Hold;
        // its sibling LOST below exercises the same Hold path.)
        let _ = &far;
        // LOST — parched, ignorant, nowhere new to go: no basis to move.
        assert_eq!(
            label(&Perceived {
                position: home.clone(),
                drive: 0.95,
                fatigue: 0.0,
                believed_water: None,
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: None,
            }),
            AffectLabel::Lost,
        );
    }

    #[test]
    fn arbitrate_in_a_comfortable_cell_is_byte_identical_to_thirst_only_decide() {
        // THE CRUX (thirst-only preserved): where thermal is INACTIVE (a
        // comfortable cell — every reachable cell at the niche optimum, urgency
        // 0), the two-drive arbitration must produce the EXACT `Intent` the
        // Stage-0 thirst-only `decide` does, for every state. Proven by direct
        // equality against `decide` on the same views.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let water = ns[0].clone();
        let day = WorldTime { day: 0.0 };
        // All cells at the warm niche's optimum → thermal urgency 0 everywhere.
        let terrain = PlantedTerrain::thermal([
            (home.clone(), 18.0),
            (ns[0].clone(), 18.0),
            (ns[1].clone(), 18.0),
            (ns[2].clone(), 18.0),
        ]);
        let params = SUSTENANCE;
        let thirst = Thirst { params };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        // A representative spread of thirst states; each must arbitrate ==
        // decide. Latency 0.5 (the goblin baseline) — irrelevant here since
        // only one drive is ever active, which is exactly the point.
        let views = [
            // parched, at home, knows water → plan's first step toward water
            Perceived {
                position: home.clone(),
                drive: 0.9,
                fatigue: 0.0,
                believed_water: Some(water.clone()),
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: None,
            },
            // sated, away (at water) → plan home
            Perceived {
                position: water.clone(),
                drive: 0.1,
                fatigue: 0.0,
                believed_water: Some(water.clone()),
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: None,
            },
            // sated, at home → Hold
            Perceived {
                position: home.clone(),
                drive: 0.1,
                fatigue: 0.0,
                believed_water: Some(water.clone()),
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: None,
            },
            // parched, ignorant, has an explore step → explore
            Perceived {
                position: home.clone(),
                drive: 0.9,
                fatigue: 0.0,
                believed_water: None,
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: Some(ns[2].clone()),
            },
            // parched, ignorant, nowhere new → Hold
            Perceived {
                position: home.clone(),
                drive: 0.9,
                fatigue: 0.0,
                believed_water: None,
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: None,
            },
        ];
        for v in &views {
            assert_eq!(
                arb(
                    v,
                    &home,
                    &drives,
                    0.5,
                    0.0,
                    false,
                    true,
                    Mode::Idle,
                    PLAN_BUDGET
                )
                .intent,
                decide(v, &home, &params, PLAN_BUDGET),
                "a comfortable-cell creature must decide exactly as thirst-only: {v:?}"
            );
        }
    }

    #[test]
    fn arbitrate_prefers_the_move_that_serves_both_drives() {
        // MULTI-DRIVE: a creature both thirsty AND cold. One neighbour is BOTH
        // the water step and warm; another is warm only. The both-serving
        // neighbour wins — utility sums across drives, so it beats the
        // thermal-only neighbour that serves just the (equally loud) comfort.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let both = ns[0].clone(); // water + warm
        let warm_only = ns[1].clone(); // warm, not water
        let cold = ns[2].clone(); // neither
        let day = WorldTime { day: 0.0 };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0),     // freezing → thermal urgency 1.0
            (both.clone(), 18.0),      // optimum → big comfort gain
            (warm_only.clone(), 18.0), // optimum → equal comfort gain
            (cold.clone(), -20.0),     // no gain
        ]);
        let view = Perceived {
            position: home.clone(),
            drive: 0.9,
            fatigue: 0.0,
            believed_water: Some(both.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        // Weigh (latency 1): the full weighted sum, so the both-serving move
        // wins decisively over the warm-only one.
        let intent = arb(
            &view,
            &home,
            &drives,
            1.0,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        )
        .intent;
        assert_eq!(
            intent,
            Intent::Do(Action::MoveTo(both)),
            "the move serving BOTH thirst and thermal beats the warm-only move"
        );
    }

    #[test]
    fn grab_and_weigh_resolve_the_same_conflict_differently() {
        // THE DIVERGENCE PROOF (§6): the SAME conflict, one psychology apart.
        // Thermal is the loudest single need (grab serves it: the pure-warmth
        // step X). But a third neighbour Z serves BOTH thirst and thermal
        // moderately, so the weighted SUM lifts Z above X — weigh takes Z.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let warm = ns[0].clone(); // pure warmth (loudest single relief)
        let both = ns[1].clone(); // water + moderate warmth
        let cold = ns[2].clone();
        let day = WorldTime { day: 0.0 };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0), // urgency 1.0 (capped 0.6)
            (warm.clone(), 18.0),  // thermal serv 1.0
            (both.clone(), 6.0),   // urgency 0.5 → thermal serv 0.5
            (cold.clone(), -20.0),
        ]);
        let view = Perceived {
            position: home.clone(),
            drive: 0.5, // moderate thirst (capped 0.5), active under eager_thirst
            fatigue: 0.0,
            believed_water: Some(both.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let thirst = Thirst {
            params: eager_thirst(),
        };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        let Resolution {
            intent: grab,
            mode: gm,
            ..
        } = arb(
            &view,
            &home,
            &drives,
            0.0,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        let weigh = arb(
            &view,
            &home,
            &drives,
            1.0,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        )
        .intent;
        assert_eq!(
            grab,
            Intent::Do(Action::MoveTo(warm.clone())),
            "grab (impulsive) takes the nearest relief for the loudest need — pure warmth"
        );
        assert_eq!(
            weigh,
            Intent::Do(Action::MoveTo(both.clone())),
            "weigh (deliberate) takes the move best relieving TOTAL discomfort"
        );
        assert_ne!(grab, weigh, "psychology alone changed the resolution");
        // Grab committed to the loudest drive (thermal).
        assert_eq!(gm, Mode::Pursuing(DriveKind::Thermal));
    }

    #[test]
    fn soft_maslow_severe_cold_beats_mild_thirst_but_dying_of_thirst_beats_any_cold() {
        // SOFT MASLOW via urgency ceilings (no priority table): comfort caps at
        // 0.6, survival reaches 1.0. Same freezing world, water and warmth in
        // opposite directions; the winner flips with thirst severity alone.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let warm = ns[0].clone();
        let water = ns[1].clone();
        let cold = ns[2].clone();
        let day = WorldTime { day: 0.0 };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0),  // severe cold: thermal urgency 1.0 (cap 0.6)
            (warm.clone(), 18.0),   // warmth here
            (water.clone(), -20.0), // water here, but no warmer
            (cold.clone(), -20.0),
        ]);
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        // (a) MILD thirst (0.5, active under eager_thirst, capped 0.5 < 0.6):
        //     severe cold wins → step toward WARMTH.
        let mild = Perceived {
            position: home.clone(),
            drive: 0.5,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let eager = Thirst {
            params: eager_thirst(),
        };
        let mild_drives: [&dyn Drive; 2] = [&eager, &thermal];
        assert_eq!(
            arb(
                &mild,
                &home,
                &mild_drives,
                0.5,
                0.0,
                false,
                true,
                Mode::Idle,
                PLAN_BUDGET
            )
            .intent,
            Intent::Do(Action::MoveTo(warm.clone())),
            "severe cold beats mild thirst"
        );
        // (b) DYING thirst (1.0, capped 1.0 > any comfort cap): nothing beats
        //     it → step toward WATER even while freezing.
        let dying = Perceived {
            position: home.clone(),
            drive: 1.0,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let survival = Thirst { params: SUSTENANCE };
        let dying_drives: [&dyn Drive; 2] = [&survival, &thermal];
        assert_eq!(
            arb(
                &dying,
                &home,
                &dying_drives,
                0.5,
                0.0,
                false,
                true,
                Mode::Idle,
                PLAN_BUDGET
            )
            .intent,
            Intent::Do(Action::MoveTo(water.clone())),
            "dying of thirst beats any cold"
        );
    }

    #[test]
    fn foresight_engages_a_stock_drive_before_it_crosses_act() {
        // TIME_HORIZON (§6, the second psychology dial): a foresighted creature
        // acts on a projectable stock drive BEFORE its urgency crosses `act`,
        // pre-empting a need a myopic creature would still be waiting on. Same
        // view; only `time_horizon` differs — so it, alone, decides whether the
        // creature is already seeking.
        let home = raddr(1.0);
        let water = home.neighbors()[1].clone();
        let day = WorldTime { day: 0.0 };
        // No planted temperatures → thermal reads INFINITY → urgency 0 →
        // inactive, so thirst alone decides.
        let terrain = PlantedTerrain::thermal([]);
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let thirst = Thirst { params: SUSTENANCE };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        // Thirst BELOW act (0.85) but within a full-foresight lead
        // (rise·1·HORIZON_DAYS = 0.30 → act_eff 0.55): 0.70 sits in [0.55, 0.85).
        let view = Perceived {
            position: home.clone(), // at home, so a non-seeking creature idles
            drive: 0.70,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        // Myopic (horizon 0): thirst inactive (0.70 < 0.85), thermal inactive →
        // no active drive, already home → Idle Hold, nothing engaged.
        let myopic = arb(
            &view,
            &home,
            &drives,
            0.5,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(myopic.intent, Intent::Hold, "a myopic creature waits");
        assert_eq!(myopic.affect.object, None, "no drive is engaged yet");
        // Foresighted (horizon 1): thirst active (0.70 ≥ act_eff 0.55) → already
        // beelining to known water, Eager, object Thirst.
        let foresighted = arb(
            &view,
            &home,
            &drives,
            0.5,
            1.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(
            foresighted.intent,
            Intent::Do(Action::MoveTo(water.clone())),
            "foresight pre-empts: already stepping toward water"
        );
        assert_eq!(foresighted.affect.object, Some(DriveKind::Thirst));
    }

    #[test]
    fn a_flow_drive_grants_no_anticipation_lead_but_a_stock_drive_does() {
        // The stock/flow split: thirst (a stock drive climbing `rise`/day) can be
        // projected, so foresight buys a lead; thermal (a flow drive whose future
        // depends on wandering and weather) cannot, so `time_horizon` grants it
        // none — the effective threshold stays `act` at every foresight.
        let terrain = PlantedTerrain::thermal([]);
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day: WorldTime { day: 0.0 },
        };
        let thirst = Thirst { params: SUSTENANCE };
        assert_eq!(thermal.anticipation_lead(0.0), 0.0);
        assert_eq!(
            thermal.anticipation_lead(1.0),
            0.0,
            "a flow drive anticipates nothing"
        );
        // Zero foresight is byte-identical to the pre-anticipation model (no lead).
        assert_eq!(
            thirst.anticipation_lead(0.0),
            0.0,
            "myopia is the old model"
        );
        assert!(
            thirst.anticipation_lead(1.0) > 0.0,
            "foresight leads a stock drive"
        );
    }

    #[test]
    fn learned_helplessness_onsets_after_prolonged_thirst_and_probes_periodically() {
        // The fold (§7): unmet survival drive past the onset → helpless, but with
        // a periodic probe (renewed effort) so the state reverses rather than
        // trapping the creature forever.
        assert!(
            !learned_helplessness(0.0, HELPLESS_ONSET_DAYS - 1.0),
            "ordinary thirst is not helplessness"
        );
        assert!(
            learned_helplessness(0.0, HELPLESS_ONSET_DAYS + 1.0),
            "unmet past onset → helpless"
        );
        // The probe: the first day of each period is a retry (not helpless).
        assert!(
            !learned_helplessness(0.0, HELPLESS_ONSET_DAYS),
            "the onset day itself probes"
        );
        assert!(
            !learned_helplessness(0.0, HELPLESS_ONSET_DAYS + HELPLESS_PROBE_DAYS),
            "each period opens with a probe"
        );
        assert!(
            !learned_helplessness(30.0, 31.0),
            "a fresh drink clears helplessness"
        );
    }

    #[test]
    fn a_helpless_creature_gives_up_even_with_a_reachable_affordance() {
        // THE BEHAVIOURAL DIFFERENCE (§7): a helpless creature Holds and reads
        // Helpless even when it COULD act — where a Frustrated creature strains,
        // a helpless one has stopped trying. Same view; the `helpless` flag alone
        // flips it.
        let home = raddr(1.0);
        let water = home.neighbors()[1].clone();
        let terrain = PlantedTerrain::fresh_only([water.clone()]);
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day: WorldTime { day: 0.0 },
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        // Maxed thirst, KNOWS reachable water → would normally beeline.
        let view = Perceived {
            position: home.clone(),
            drive: 1.0,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let trying = arb(
            &view,
            &home,
            &drives,
            0.5,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert!(
            matches!(trying.intent, Intent::Do(_)),
            "a creature still trying acts toward the water"
        );
        let given_up = arb(
            &view,
            &home,
            &drives,
            0.5,
            0.0,
            true,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(
            given_up.intent,
            Intent::Hold,
            "a helpless creature stops trying, even with reachable water"
        );
        assert_eq!(given_up.affect.label, AffectLabel::Helpless);
        assert_eq!(given_up.affect.object, Some(DriveKind::Thirst));
        assert!(
            given_up.affect.valence < 0.0,
            "helplessness is negative valence"
        );
    }

    #[test]
    fn an_ametabolic_creature_has_no_drives_and_never_distresses() {
        // THE METABOLISM GATE (The Kindling): an Ametabolic creature
        // (construct/undead/elemental) has no homeostatic drives, so even
        // parched-long in a blistering cell it reads Content — never thirst,
        // never distress. A metabolizer in the same spot is wrecked.
        let home = raddr(1.0);
        let terrain = PlantedTerrain::thermal([(home.clone(), 80.0)]); // blistering, no water
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let base = Npc {
            entity: e,
            home: home.clone(),
            resource: home.clone(),
            species: "xorn".to_string(),
            activity: ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Ametabolic,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "xorn".to_string(),
        };
        // Day 100: a metabolizer would be long parched and roasting.
        let a = affect_of(&ledger, &base, &[], WorldTime { day: 100.0 }, &terrain);
        assert_eq!(a.label, AffectLabel::Content, "the deathless are still");
        assert_eq!(a.object, None, "no drive is engaged");
        let meta = Npc {
            metabolic_class: MetabolicClass::Endotherm,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            ..base.clone()
        };
        let b = affect_of(&ledger, &meta, &[], WorldTime { day: 100.0 }, &terrain);
        assert_ne!(
            b.label,
            AffectLabel::Content,
            "a metabolizer is not content, parched in the heat: {b:?}"
        );
    }

    #[test]
    fn an_ametabolic_creature_does_not_flinch_at_a_hazard() {
        // THE METABOLISM GATE, danger edge (The Dread): an Ametabolic creature
        // (a construct) carries no danger drive — surrounded by lethal threat it
        // still reads Content, where a metabolizer recoils.
        let home = raddr(1.0);
        // Home and every neighbour maximally threatening (cornered by dread).
        let terrain = PlantedTerrain::hazard(
            std::iter::empty(),
            home.neighbors()
                .into_iter()
                .chain(std::iter::once(home.clone()))
                .map(|r| (r, 1.0)),
        );
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let base = Npc {
            entity: e,
            home: home.clone(),
            resource: home.clone(),
            species: "xorn".to_string(),
            activity: ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Ametabolic,
            niche: default_diet_niche(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            label: "xorn".to_string(),
        };
        let a = affect_of(&ledger, &base, &[], WorldTime { day: 0.5 }, &terrain);
        assert_eq!(a.label, AffectLabel::Content, "a construct does not flinch");
        let meta = Npc {
            metabolic_class: MetabolicClass::Endotherm,
            ..base.clone()
        };
        let b = affect_of(&ledger, &meta, &[], WorldTime { day: 0.5 }, &terrain);
        assert_eq!(
            b.object,
            Some(DriveKind::Danger),
            "a metabolizer cornered by dread fears it: {b:?}"
        );
        assert_ne!(b.label, AffectLabel::Content, "and is not content: {b:?}");
    }

    #[test]
    fn asleep_a_creature_rests_and_wakes_only_for_survival() {
        // THE WAKE-GATE (The Slumber, spec §3): while asleep, thirst and thermal
        // fall silent and the creature rests — unless thirst is survival-critical,
        // which wakes it to drink.
        let home = raddr(1.0);
        let water = home.neighbors()[1].clone();
        let terrain = PlantedTerrain::fresh_only([water.clone()]);
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day: WorldTime { day: 0.0 },
        };
        let rest = Fatigue { home: home.clone() };
        let drives: [&dyn Drive; 3] = [&thirst, &thermal, &rest];
        let view = Perceived {
            position: home.clone(),
            drive: 0.85, // thirsty (active while awake), but not yet dying
            fatigue: 0.2,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        // Awake: it seeks water.
        let awake = arb(
            &view,
            &home,
            &drives,
            0.5,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(awake.affect.object, Some(DriveKind::Thirst));
        // Asleep: the wake-gate silences thirst; it rests instead.
        let asleep = arb(
            &view,
            &home,
            &drives,
            0.5,
            0.0,
            false,
            false,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(
            asleep.affect.object,
            Some(DriveKind::Fatigue),
            "asleep it rests, not seeks: {asleep:?}"
        );
        assert_eq!(asleep.intent, Intent::Do(Action::Rest));
        // Asleep and DYING of thirst: the survival override wakes it to drink.
        let dying = Perceived {
            drive: 0.95,
            ..view.clone()
        };
        let survival = arb(
            &dying,
            &home,
            &drives,
            0.5,
            0.0,
            false,
            false,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(
            survival.affect.object,
            Some(DriveKind::Thirst),
            "dying of thirst wakes it even asleep: {survival:?}"
        );
    }

    #[test]
    fn is_awake_follows_the_sun_and_the_activity_cycle() {
        use ActivityCycle::*;
        // The default (fractional-day) solar sun: up at noon, at the horizon at
        // dawn/dusk, down at midnight — the coarse cycle planted terrain uses.
        let t = PlantedTerrain::thermal([]);
        let r = raddr(1.0);
        let at = |d: f64| WorldTime { day: d };
        // Noon (sun up): diurnal awake, nocturnal asleep. Midnight: the reverse.
        assert!(is_awake(Diurnal, &t, &r, at(3.5)));
        assert!(!is_awake(Nocturnal, &t, &r, at(3.5)));
        assert!(!is_awake(Diurnal, &t, &r, at(3.0)));
        assert!(is_awake(Nocturnal, &t, &r, at(3.0)));
        // Crepuscular: awake when the sun is near the horizon (dawn ~frac 0.25),
        // asleep at noon.
        assert!(is_awake(Crepuscular, &t, &r, at(3.25)));
        assert!(!is_awake(Crepuscular, &t, &r, at(3.5)));
    }

    #[test]
    fn fatigue_folds_rested_events() {
        // FATIGUE == FOLD over `rested`: rises since the last rest, resets on it,
        // clamps at 1 — the structural twin of thirst's `drive_at`.
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        assert!((fatigue_at(&ledger, e, WorldTime { day: 0.5 }) - FATIGUE_RISE * 0.5).abs() < 1e-9);
        ledger.commit(rested_fact(e, 2.0, "t"), &reg).unwrap();
        assert!(fatigue_at(&ledger, e, WorldTime { day: 2.0 }) < 1e-9);
        assert!((fatigue_at(&ledger, e, WorldTime { day: 3.0 }) - FATIGUE_RISE).abs() < 1e-9);
        assert_eq!(fatigue_at(&ledger, e, WorldTime { day: 100.0 }), 1.0);
    }

    #[test]
    fn the_fatigue_drive_rests_in_place_wherever_the_creature_is() {
        // A creature sleeps where it is (The Slumber v2): rest is always the
        // affordance, home or away — so an explorer beds down in the field and a
        // stranded creature is never fatigue-blocked.
        let home = raddr(1.0);
        let away = home.neighbors()[0].clone();
        let rest = Fatigue { home: home.clone() };
        for pos in [home.clone(), away.clone()] {
            let view = Perceived {
                position: pos,
                drive: 0.0,
                fatigue: 1.0,
                believed_water: None,
                believed_hazard: std::collections::BTreeSet::new(),
                explore_step: None,
            };
            assert_eq!(rest.affordance(&view, PLAN_BUDGET), Some(Action::Rest));
        }
    }

    #[test]
    fn commitment_hysteresis_prevents_flip_flop_between_near_equal_drives() {
        // HYSTERESIS: once committed to a drive, a challenger only marginally
        // louder (within the switch margin δ) does NOT steal the errand — the
        // committed mode is sticky across ticks, so no dithering.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let warm = ns[0].clone();
        let water = ns[1].clone();
        let day = WorldTime { day: 0.0 };
        // Freezing home (thermal urgency 1.0, capped 0.6); a fully-comfortable
        // warm neighbour → thermal grab-utility = capped(0.6) × drop(1.0) = 0.6.
        // Water lies in a cold direction (no thermal help).
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0),
            (warm.clone(), 18.0),
            (water.clone(), -20.0),
            (ns[2].clone(), -20.0),
        ]);
        let thirst = Thirst {
            params: eager_thirst(),
        };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        // Tick 1: thirst grab-utility 0.50 < thermal 0.60 → commit to thermal.
        let low = Perceived {
            position: home.clone(),
            drive: 0.50,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let m1 = arb(
            &low,
            &home,
            &drives,
            0.5,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        )
        .mode;
        assert_eq!(
            m1,
            Mode::Pursuing(DriveKind::Thermal),
            "commit to the louder thermal drive"
        );
        // Tick 2+: thirst climbs to 0.65 — now marginally LOUDER than thermal
        // (0.60), but within δ = 0.1. A fresh (Idle) arbitration WOULD flip to
        // thirst; carrying the committed mode must NOT.
        let high = Perceived {
            position: home.clone(),
            drive: 0.65,
            fatigue: 0.0,
            believed_water: Some(water.clone()),
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        assert_eq!(
            arb(
                &high,
                &home,
                &drives,
                0.5,
                0.0,
                false,
                true,
                Mode::Idle,
                PLAN_BUDGET
            )
            .mode,
            Mode::Pursuing(DriveKind::Thirst),
            "control: with no committed mode, the now-louder thirst is pursued (the test bites)"
        );
        let mut mode = m1;
        for _ in 0..5 {
            let m = arb(
                &high,
                &home,
                &drives,
                0.5,
                0.0,
                false,
                true,
                mode,
                PLAN_BUDGET,
            )
            .mode;
            assert_eq!(
                m,
                Mode::Pursuing(DriveKind::Thermal),
                "the committed errand is sticky: a within-δ challenger never steals it"
            );
            mode = m;
        }
    }

    #[test]
    fn arbitration_is_deterministic_reload_stable_and_breaks_ties_by_ascending_room_addr() {
        // DETERMINISM + TIE-BREAK: arbitration reads only view + terrain + niche
        // (no ledger), so it recomputes identically (reload-stable by
        // construction). And a genuine two-way utility tie resolves to the
        // smaller `RoomAddr` — the constitutional `total_cmp` + ascending-addr
        // rule, here on the arbitration's own action scan.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        let smaller = std::cmp::min(ns[0].clone(), ns[1].clone());
        // Thermal-only (thirst inactive, drive 0): home freezing, ns[0] and
        // ns[1] EQUALLY warm (both optimum) → equal thermal serviceability →
        // equal utility → the tie-break decides.
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0),
            (ns[0].clone(), 18.0),
            (ns[1].clone(), 18.0),
            (ns[2].clone(), -20.0),
        ]);
        let view = Perceived {
            position: home.clone(),
            drive: 0.0,
            fatigue: 0.0,
            believed_water: None,
            believed_hazard: std::collections::BTreeSet::new(),
            explore_step: None,
        };
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        let a = arb(
            &view,
            &home,
            &drives,
            0.5,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        let b = arb(
            &view,
            &home,
            &drives,
            0.5,
            0.0,
            false,
            true,
            Mode::Idle,
            PLAN_BUDGET,
        );
        assert_eq!(a, b, "same inputs → same (Intent, Mode); reload-stable");
        assert_eq!(
            a.intent,
            Intent::Do(Action::MoveTo(smaller)),
            "an equal-utility tie resolves to the smaller RoomAddr"
        );
    }

    #[test]
    fn a_colocated_lost_creature_feels_relief() {
        // THE TIDINGS, WIRED INTO THE SAMPLER: same co-located knower/lost
        // scenario as `shared_belief_fills_an_ignorant_colocated_creature`,
        // read through `affect_of` at a moment well past the thirst act
        // threshold (chronic, not yet at the learned-helplessness onset).
        // Alone, `lost` is ignorant: its own `believed_water` is `None`, but
        // it is never truly stuck — a mesh room always has 3 neighbours, so
        // an ignorant thirsty creature always has an exploration gradient to
        // follow (Searching: normal seeking, valence 0.0 — not yet relief).
        // With the band, `shared_believed_water` hands it
        // `knower`'s known, reachable source: the SAME arbitration now
        // beelines to a KNOWN target, reading Eager (valence 0.5) instead —
        // the measurable relief the shared belief buys it. Mutation-verify:
        // an `affect_of` that dropped `band` (or passed it through empty)
        // would read `Searching` in BOTH calls; this reds without the wiring.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let here = raddr(1.0);
        let water = here.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let knower_e = ledger.mint_entity();
        let knower = shared_belief_npc(knower_e, here.clone(), water.clone(), "knower");
        let lost_e = ledger.mint_entity();
        let lost = shared_belief_npc(lost_e, here.clone(), here.clone(), "lost");
        // knower's perception history: stood at water, now back at `here`.
        commit_agent_at(&mut ledger, &reg, knower_e, &water, 0.0);
        commit_agent_at(&mut ledger, &reg, knower_e, &here, 1.0);
        // lost has only ever been at `here`.
        commit_agent_at(&mut ledger, &reg, lost_e, &here, 1.0);
        // Day 10: well past thirst's act threshold (chronic), well before the
        // 15-day learned-helplessness onset (which, being a pure function of
        // `last_drank`/day, would be identical alone or in-band and so could
        // never distinguish them).
        let now = WorldTime { day: 10.0 };

        let alone = affect_of(&ledger, &lost, &[], now, &t);
        let in_band = affect_of(&ledger, &lost, &[knower.clone(), lost.clone()], now, &t);

        assert_eq!(
            alone.label,
            AffectLabel::Searching,
            "alone and ignorant, {lost:?} follows a gradient, unrelieved: {alone:?}"
        );
        assert_eq!(
            in_band.label,
            AffectLabel::Eager,
            "co-located with a knower, the shared belief relieves it: {in_band:?}"
        );
        assert!(
            in_band.valence > alone.valence,
            "the shared belief must make the felt state MORE positive, not just different"
        );
    }
}
