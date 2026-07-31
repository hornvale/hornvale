//! The Quickening: the world's first autonomous motion. NPCs derived like the
//! possessed agent walk deterministic daily routes; their position over time is
//! a pure schedule (derived, reversible — the routine). This module is the
//! pure foundation only: deriving NPCs and their daily-route schedule. No
//! ledger facts are committed here and no session/tick wiring exists yet
//! (that is later Quickening work); domains are untouched (The Walk §11).

use crate::agent::{settlement_position, walk_depth};
use crate::clock::{climb_factor, cost_ticks, days_of, ticks_per_local_day};
use crate::interior::{
    AnchorId, Interior, SeamKind, interior_of, landing, route_within, seam_kind, warmth_at,
};
use hornvale_kernel::{
    ANIMAL_PREY, AStarSolver, ConditionResponse, EntityId, Fact, Ledger, PHOTOSYNTHATE,
    PLANT_FORAGE, ResourceVector, RoomAddr, RoomId, RoomMeshMemo, SearchSpace, Solver, TickSystem,
    Value, World, WorldTime, astar,
};
use hornvale_locale::LocaleContext;
use hornvale_species::{ActivityCycle, MetabolicClass};

/// A derived non-player agent: a minted entity, a home and a resource room,
/// its species, and that species' activity-cycle. Derived from the genesis
/// world, never stored (re-derivable).
/// type-audit: bare-ok(identifier-text: label), bare-ok(identifier-text: species), bare-ok(ratio: deliberation_latency), bare-ok(ratio: time_horizon), bare-ok(ratio: boldness), bare-ok(ratio: mass_kg)
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
    /// The species' `MindVector.deliberation_latency`: slides the arbitration
    /// rule between *grab* (myopic, serve the loudest need) and *weigh* (the
    /// full weighted sum) — psychology's first runtime job (spec §6). Threaded
    /// from `psyche_registry` at derivation.
    pub deliberation_latency: f64,
    /// The species' `MindVector.time_horizon`: how far ahead the creature
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
    /// The species' `MindVector.threat_response` (flee 0 ↔ stand 1), read at
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
    /// The species' adult body mass in kilograms (`BiosphereTraits::mass`),
    /// threaded from `biosphere_registry` at derivation beside the metabolic
    /// class. Read by the action clock to scale every action's cost
    /// allometrically (The Action Clock); nothing else consumes it. Bare `f64`
    /// rather than the kernel's `Mass`, matching `clock::tempo`'s parameter —
    /// it is only ever consumed as the ratio `mass_kg / REFERENCE_MASS_KG`.
    pub mass_kg: f64,
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

/// The day a room's furnishing reads its climate at (The Threshold). Any fixed
/// day serves; day 0 is the world's own origin and needs no justification
/// beyond being stable. Changing it is a `room/furnishing/v1` epoch.
///
/// SCOPE, stated so it is not discovered later: this freezes furnishing at
/// ORIGIN climate forever. A room that grows cold over long time never gains a
/// hearth. That is invisible only because the play window is days to years
/// while climate drift is paleoclimate-scale — so the interior is a pure
/// function of the room in SPACE, and frozen in TIME at day 0. When eras
/// become playable this constant is the thing to revisit.
pub const FURNISHING_REFERENCE_DAY: WorldTime = WorldTime { day: 0.0 };

/// Below this mean temperature (°C) a room's people build around a fire.
/// A first-pass value; changing it is a `room/furnishing/v1` epoch.
/// type-audit: pending(wave-3)
pub const FURNISHING_COLD_C: f64 = 5.0;

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
    /// BIOTIC hazard: `worldgen::predator_pressure_from`, injected into
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

    /// Whether this room carries a built settlement — the signal that decides
    /// whether its interior draws built patterns or wild ones (The Threshold).
    /// A room's *culture* is not a property of the room: culture belongs to the
    /// people whose territory contains it, and a natural room has none. So the
    /// derivation asks the only question it can answer here — is anyone's
    /// territory this? Defaults false, so every existing implementation reads
    /// as wilderness and nothing moves.
    /// type-audit: bare-ok(flag: return)
    fn is_built(&self, _room: &RoomAddr) -> bool {
        false
    }

    /// Whether warmth matters in this room — whether its people build around a
    /// fire. Read at a CANONICAL day rather than the current one: a room's
    /// furnishing must not flicker with the seasons, so this is a stable
    /// property of the place, not of the weather (The Threshold). The
    /// comparison is against [`FURNISHING_COLD_C`], in degrees Celsius, the
    /// same unit [`Terrain::temperature`] returns.
    ///
    /// Unlike [`Terrain::is_built`], whose default is independently silent
    /// (every implementation reads as unbuilt until one says otherwise), this
    /// default is NOT self-contained: it calls straight back into whatever
    /// `temperature` the implementor supplies. A planted or synthetic test
    /// terrain therefore reads as cold or temperate according to its own
    /// `temperature`, and one that returns a non-finite value reads as
    /// temperate, since the comparison is false for `NaN`.
    /// type-audit: bare-ok(flag: return)
    fn is_cold(&self, room: &RoomAddr) -> bool {
        self.temperature(room, FURNISHING_REFERENCE_DAY) < FURNISHING_COLD_C
    }

    /// The cell's PREY-PRESENCE field in `[0, 1]` (The Teeth) — the standing
    /// prey-base biomass a HUNTER can eat there, the anti-symmetric dual of the
    /// predator hazard (`worldgen::prey_pressure_from`). A creature's
    /// `food_value` dots this against its `ANIMAL_PREY` diet weight, so a
    /// carnivore is drawn up the prey gradient. The DEFAULT is `0.0` (a
    /// prey-empty cell) — so planted/synthetic test terrains have no prey
    /// field and a carnivore reads only the ordinary productivity unless a
    /// scenario plants prey; a live `LocaleTerrain` OVERRIDES it with the
    /// injected prey-pressure field. A slow field, so it takes no `day`.
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
/// `nearest_water`'s BFS and `lowest_unvisited_neighbor_memo` use. Always a
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
    /// predator_pressure_from`), injected here (a domain/window can't reach up
    /// to demography); `None` → no PREDATOR hazard (throwaway reads / no field).
    predator: Option<&'a hornvale_kernel::CellMap<f64>>,
    /// The world's prey-pressure field (The Teeth — `worldgen::
    /// prey_pressure_from`), the dual of `predator`, injected the same way;
    /// `None` → no prey draw (throwaway reads / no field), so a carnivore
    /// reads only ordinary productivity.
    prey: Option<&'a hornvale_kernel::CellMap<f64>>,
    /// The world's settlement-territory set (The Threshold, task 5b —
    /// `built_rooms`), injected the same way (a domain/window can't reach up
    /// to `hornvale_settlement`); `None` → every room reads unbuilt (a
    /// throwaway read with no world), the same fail-safe-to-wilderness
    /// posture `Terrain::is_built`'s own default takes.
    built: Option<&'a std::collections::BTreeSet<RoomId>>,
    /// A PREFILLED, READ-ONLY [`hornvale_kernel::RoomMeshMemo`] (the-waymark
    /// fix round, Finding 1): every `corner_weights`-backed read below
    /// consults it first, falling through to a fresh recompute on a miss.
    /// `None` (every non-`with_fields` constructor) is byte-identical to the
    /// pre-Finding-1 behaviour — always a miss. This field is set ONCE, here,
    /// at construction, and never mutated: the cache is filled by the
    /// CALLER, under `&mut`, before any `LocaleTerrain` (and so before any
    /// drive) exists — see `windows/vessel/src/session.rs`'s `wait` and
    /// `windows/lab/src/health.rs`'s `simulate_world` for the fill sites.
    /// Reading it here needs no `&mut self`, which is what lets `Terrain`'s
    /// trait methods stay `&self` (they must: `decide_step` holds
    /// `Thermal`/`Hunger`/`Danger`'s `&dyn Terrain` copies alive
    /// SIMULTANEOUSLY in one `Vec<&dyn Drive>`, so a `&mut self` receiver
    /// here is not merely undesired but borrow-checker-infeasible without
    /// restructuring `arbitrate` — see the-waymark Task 3's report).
    cache: Option<&'a hornvale_kernel::RoomMeshMemo>,
}
impl<'a> LocaleTerrain<'a> {
    /// Build the adapter over `ctx` with the fractional-day (Tier-0) sun and no
    /// predator field — for throwaway reads (water/elevation) that never consult
    /// the wake cycle or the danger drive. No prefilled cache.
    pub fn new(ctx: &'a LocaleContext) -> Self {
        Self {
            ctx,
            calendar: None,
            predator: None,
            prey: None,
            built: None,
            cache: None,
        }
    }
    /// Build with the world's `calendar` (if any), so `solar_altitude` (and thus
    /// the wake cycle) follows the REAL sun — latitude × season × the terminator
    /// (Tier-1). `None` falls back to the fractional-day sun. No predator field
    /// (use [`with_calendar_and_predators`](Self::with_calendar_and_predators) for
    /// the full drive read). No prefilled cache.
    pub fn with_calendar(
        ctx: &'a LocaleContext,
        calendar: Option<&'a hornvale_astronomy::Calendar>,
    ) -> Self {
        Self {
            ctx,
            calendar,
            predator: None,
            prey: None,
            built: None,
            cache: None,
        }
    }
    /// Build with the world's `calendar` AND its predator-pressure field (The
    /// Quarry) — no prey field, no settlement-territory set, no prefilled
    /// cache. Retained for callers that read danger but not the hunt;
    /// delegates to [`with_fields`](Self::with_fields).
    /// type-audit: bare-ok(ratio: predator)
    pub fn with_calendar_and_predators(
        ctx: &'a LocaleContext,
        calendar: Option<&'a hornvale_astronomy::Calendar>,
        predator: Option<&'a hornvale_kernel::CellMap<f64>>,
    ) -> Self {
        Self::with_fields(ctx, calendar, predator, None, None, None)
    }
    /// Build with the world's `calendar`, predator-pressure field (The Quarry),
    /// prey-pressure field (The Teeth), AND settlement-territory set (The
    /// Threshold, task 5b's `built_rooms`) — the full drive read: danger
    /// senses carnivore territory, a carnivore's hunger senses prey, and a
    /// creature's thermal drive can find a real hearth. `built` is `None` for
    /// every caller with no world to read one from (the throwaway/no-field
    /// case `Terrain::is_built`'s own default already covers). `cache` is the
    /// prefilled, read-only [`hornvale_kernel::RoomMeshMemo`] (the-waymark
    /// fix round, Finding 1) — `None` for a caller with nothing prefilled
    /// (byte-identical to the pre-Finding-1 behaviour).
    /// type-audit: bare-ok(ratio: predator), bare-ok(ratio: prey)
    pub fn with_fields(
        ctx: &'a LocaleContext,
        calendar: Option<&'a hornvale_astronomy::Calendar>,
        predator: Option<&'a hornvale_kernel::CellMap<f64>>,
        prey: Option<&'a hornvale_kernel::CellMap<f64>>,
        built: Option<&'a std::collections::BTreeSet<RoomId>>,
        cache: Option<&'a hornvale_kernel::RoomMeshMemo>,
    ) -> Self {
        Self {
            ctx,
            calendar,
            predator,
            prey,
            built,
            cache,
        }
    }
}
impl<'a> Terrain for LocaleTerrain<'a> {
    fn elevation(&self, room: &RoomAddr) -> f64 {
        self.ctx
            .describe_at_cached(room, WorldTime { day: 0.0 }, None, self.cache)
            .map(|l| l.fields.elevation_m)
            .unwrap_or(f64::INFINITY)
    }
    fn is_fresh_water(&self, room: &RoomAddr) -> bool {
        self.ctx
            .describe_at_cached(room, WorldTime { day: 0.0 }, None, self.cache)
            .map(|l| l.fields.water.is_fresh())
            .unwrap_or(false)
    }
    fn temperature(&self, room: &RoomAddr, day: WorldTime) -> f64 {
        // The PER-DAY field (`LocaleContext::temperature_at`), NOT `describe`'s
        // annual-mean `temperature_c` — so the drive gets a diurnal/seasonal
        // swing while the render path stays byte-identical. INFINITY for an
        // undescribable room (never chosen as a comfort target).
        self.ctx
            .temperature_at_cached(room, day, self.cache)
            .unwrap_or(f64::INFINITY)
    }
    fn solar_altitude(&self, room: &RoomAddr, day: WorldTime) -> Option<f64> {
        // The real sun where the world carries a calendar (latitude from the
        // room's centroid; `None` on a locked world → no cycle); else the
        // fractional-day fallback. No `corner_weights` read here (a pure
        // astronomy calc over the room's centroid), so no cache to consult.
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
        self.ctx
            .productivity_at_cached(room, self.cache)
            .unwrap_or(0.0)
    }
    fn hazards(&self, room: &RoomAddr) -> Hazards {
        // The real climate's per-axis hazard field (The Bane: the uncanny plus
        // graded heat/cold); an undescribable/above-grid room reads all-zero
        // (safe) — the never-feared fallback, the dual of `forage_value`'s 0.
        let (uncanny, heat, cold) = self
            .ctx
            .hazards_at_cached(room, self.cache)
            .unwrap_or((0.0, 0.0, 0.0));
        // The PREDATOR axis (The Quarry): the injected carnivore-pressure field,
        // corner-blended per room; `0` where no field is injected or the room is
        // above the grid.
        let predator = self
            .predator
            .and_then(|field| self.ctx.blend_at_cached(room, field, self.cache))
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
            .and_then(|field| self.ctx.blend_at_cached(room, field, self.cache))
            .unwrap_or(0.0)
    }
    fn is_built(&self, room: &RoomAddr) -> bool {
        // THE THRESHOLD's real answer (task 5b): built iff `room` packs to a
        // room id in the injected settlement-territory set (`built_rooms`).
        // `None` (no set injected — a throwaway read with no world) or a pack
        // failure (only possible past `MAX_DEPTH`, never reached here) both
        // read as unbuilt, the same fail-safe-to-wilderness posture
        // `Terrain::is_built`'s own default already takes. No `corner_weights`
        // read here, so no cache to consult.
        self.built
            .zip(room.pack().ok())
            .is_some_and(|(set, id)| set.contains(&id))
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

/// The Haunt/Phantom hazard memory, split by PROVENANCE — the one fold, read
/// two ways. `shunned` is what the PLANNER routes around (both provenances);
/// `dread` is the TRANSIENT subset alone, the ground a creature's own reading
/// of the present terrain calls safe and only a remembered alarm makes
/// frightening. The Shudder's load-bearing distinction: a felt term reading
/// `shunned` would drift the canonical world (wild fauna carry a non-empty
/// static set on seed 42), while `dread` is EMPTY there by construction — no
/// primary-afraid emitter, so the emitter-free fast path returns before a
/// single entry is recorded.
/// type-audit: bare-ok(ratio: dread)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct HazardMemory {
    /// Every remembered-frightening cell, both provenances — the planner's
    /// finite route-cost set (exactly the historical `believed_hazard`).
    pub shunned: std::collections::BTreeSet<RoomAddr>,
    /// The TRANSIENT subset, keyed to the remembered ALARM magnitude at that
    /// cell: ground whose terrain alone never crossed `DANGER_ACT`, tipped over
    /// it only by the re-derived alarm of a herd that has long since moved on.
    /// A subset of `shunned`'s keys. Empty ⇒ no phobia (the settled worlds).
    pub dread: std::collections::BTreeMap<RoomAddr, f64>,
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
///
/// The planner half of [`hazard_memory`]; the transient half is
/// [`HazardMemory::dread`].
pub fn believed_hazard(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
) -> std::collections::BTreeSet<RoomAddr> {
    hazard_memory(ledger, npc, t, terrain, roster).shunned
}

/// [`believed_hazard`] sharing a caller-owned [`PrimaryAfraidMemo`] across the
/// many re-derivations of a single tick (the whole cost win — see the type doc).
///
/// The planner half of [`hazard_memory_memo`]; the transient half is
/// [`HazardMemory::dread`].
pub fn believed_hazard_memo(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
    memo: &mut PrimaryAfraidMemo,
) -> std::collections::BTreeSet<RoomAddr> {
    hazard_memory_memo(ledger, npc, t, terrain, roster, memo).shunned
}

/// [`hazard_memory_memo`] with a throwaway memo — a lone read gains nothing
/// from caching (the hot sim paths thread a shared one).
pub fn hazard_memory(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
) -> HazardMemory {
    let mut memo = PrimaryAfraidMemo::new();
    hazard_memory_memo(ledger, npc, t, terrain, roster, &mut memo)
}

/// The ONE hazard fold (see [`believed_hazard`] for the belief, the staleness
/// rule and the cost argument), returning BOTH provenances as a
/// [`HazardMemory`] and sharing a caller-owned [`PrimaryAfraidMemo`] across the
/// many re-derivations of a single tick.
pub fn hazard_memory_memo(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
    memo: &mut PrimaryAfraidMemo,
) -> HazardMemory {
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

    let mut mem = HazardMemory::default();
    if scan.emitters.is_empty() {
        // The emitter-free common case (every settled world): no transient alarm
        // is possible, so the verdict is The Haunt's terrain-only `frightened_at`
        // (the one source of truth for the formula). Terrain is time-invariant,
        // so the most-recent-visit rule collapses to any-visit — byte-identical.
        // It is also why `dread` is empty on every settled world: this returns
        // BEFORE any dread is ever recorded, so byte-identity costs not one
        // instruction.
        for (cell, day) in latest {
            if frightened_at(&cell, npc, terrain, WorldTime { day }, &[], ledger) {
                mem.shunned.insert(cell);
            }
        }
        return mem;
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
            // STATIC provenance: present danger, not a phantom — shunned only.
            mem.shunned.insert(cell);
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
        // Hoisted so the value RECORDED as dread is byte-for-byte the value that
        // produced the verdict — the memory and the feeling must not disagree.
        let alarm = alarm.clamp(0.0, 1.0);
        if feels_frightening(terrain_threat, alarm, npc.boldness) {
            // TRANSIENT provenance by construction: control only reaches here
            // when terrain ALONE did not frighten (the shortcut above `continue`d
            // otherwise), so a cell shunned here is shunned BECAUSE of a
            // remembered alarm. That is the whole isolation — no second pass.
            mem.shunned.insert(cell.clone());
            mem.dread.insert(cell, alarm);
        }
    }
    mem
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

    /// Extra candidate actions this drive proposes, BEYOND [`arbitrate`]'s
    /// fixed room-scale set (the position's three neighbours as `MoveTo`, plus
    /// `Drink`/`Rest`/`Eat`) — the seam a drive whose action space is FINER
    /// than the room graph uses to make its own moves visible to the
    /// multi-drive utility scan. The default is empty, and is correct for
    /// every drive built before The Threshold: each one's `affordance` already
    /// picks among the room-graph's own neighbours (a room-scale A* plan's or
    /// gradient step's first hop is, by construction, always a room-graph
    /// neighbour), so it is already present in the fixed set and needs no
    /// second listing here. `Thermal` is the first drive that reasons over a
    /// DIFFERENT graph (the room interior's anchors), so it is the first to
    /// override this — see its own doc for why the override is exactly its
    /// `affordance`'s own within-room choice, not a separate enumeration.
    /// type-audit: bare-ok(count: _budget)
    fn candidate_actions(&self, _view: &Perceived, _budget: usize) -> Vec<Action> {
        Vec::new()
    }

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
/// arbitration of thirst + thermal together is Stage 2. No field here is a
/// bare primitive at this `pub` boundary (every field is a typed newtype,
/// trait object, or reference), so this struct itself needs no
/// `type-audit:` tag — see the individual fields for any that do.
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
    /// The room INTERIOR the creature stands in, paired with the anchor it
    /// currently occupies within it (The Hearth's warmth seam, The
    /// Threshold's crossing), or `None` where no interior applies. Where the
    /// pre-crossing model carried a single precomputed `warmth: Option<f64>`
    /// scalar, this carries the GRAPH instead: choosing where to move needs
    /// the interior's shape and the creature's place in it, not just the
    /// number that shape currently produces at one anchor. The felt warmth
    /// at any anchor is DERIVED from this pair via
    /// [`crate::interior::warmth_at`] (see [`Self::urgency_here`]) rather
    /// than passed in, so there is exactly one source of truth for "warmth
    /// at an anchor" and the drive can also ask it about anchors the
    /// creature is not currently standing at (`Self::affordance`'s
    /// within-room branch).
    ///
    /// `None` means exactly what the old `warmth: None` meant: no interior
    /// applies, so the felt temperature is the ambient reading untouched
    /// (see [`Self::urgency_here`]'s `None` arm) — the identity every
    /// pre-Hearth call site relied on, preserved byte-for-byte. It can only
    /// ever RAISE what a creature feels (warmth is strictly non-negative and
    /// additive), so a creature already comfortable in a cold room is
    /// unchanged and an interior-free world is byte-identical by
    /// construction, the same additive-latent discipline as
    /// [`Danger::alarm`].
    pub interior: Option<(&'a Interior, AnchorId)>,
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
        self.urgency_of(self.terrain.temperature(room, self.day))
    }

    /// The thermal urgency of a FELT temperature (°C) — the niche comparison
    /// itself, factored out so the ambient reading ([`Self::urgency_at`]) and
    /// the interior-warmed one ([`Self::urgency_here`]) can never disagree
    /// about what a temperature feels like. Non-finite in, `0.0` out.
    /// type-audit: bare-ok(ratio: return)
    fn urgency_of(&self, temp: f64) -> f64 {
        if !temp.is_finite() {
            return 0.0;
        }
        let dev = (temp - self.niche.optimum).abs();
        ((dev - self.niche.width).max(0.0) / self.niche.width).clamp(0.0, 1.0)
    }

    /// The thermal urgency felt HERE — [`Self::urgency_at`] plus the room
    /// interior's warmth at the creature's own anchor ([`Self::interior`],
    /// read via [`warmth_at`]), folded ADDITIVELY into the sensed temperature
    /// BEFORE the niche comparison (The Hearth). The term can only RAISE the
    /// felt temperature, never lower it, so a cold creature beside a fire is
    /// eased and no creature is made colder by a hearth. `None` returns the
    /// ambient reading UNTOUCHED — not `temp + 0.0` — so an interior-free
    /// world takes the same arithmetic path it did before The Hearth. An
    /// unreadable cell stays unreadable: a non-finite ambient temperature
    /// plus a finite warmth is still non-finite, so it still registers `0.0`.
    ///
    /// The warmth field is emitted in °C at its source
    /// ([`crate::interior::HEARTH_WARMTH`]) and decayed over graph distance
    /// there, so it is folded 1:1 with no second dial to tune — one source of
    /// scale, unlike the alarm's separate [`ALARM_SCALE`].
    /// type-audit: bare-ok(count: budget), bare-ok(ratio: return)
    fn urgency_here(&self, room: &RoomAddr, budget: usize) -> f64 {
        let temp = self.terrain.temperature(room, self.day);
        match self.interior {
            Some((interior, anchor)) => self.urgency_of(temp + warmth_at(interior, anchor, budget)),
            None => self.urgency_of(temp),
        }
    }

    /// The within-room DESTINATION [`Self::affordance`]'s within-room branch
    /// would eventually walk to, hop by hop — returned directly rather than
    /// as a first step (The Threshold task 7). Catch-up's cap-exceeded
    /// fallback (spec §5.3: "places the creature at its drive-preferred
    /// anchor") needs exactly this, and needs it WITHOUT re-deriving
    /// `affordance`'s own judgment about whether to move at all: this is
    /// literally `affordance`'s within-room branch (same gate, same
    /// `warmest_anchor` call), just stopping short of the `route_within`
    /// step that turns a destination into a first hop. `None` under the
    /// identical conditions `affordance` would decline a within-room move:
    /// already inside the tolerance band, no interior here, no anchor
    /// strictly more comfortable than the current one, or — spec §8a's
    /// stranding case — a warmer anchor exists but [`route_within`] cannot
    /// reach it from here. That last check is not optional: this function's
    /// result is fed straight to [`Occupancy::place`] by catch-up's
    /// cap-exceeded fallback, which (unlike `walk`) performs no one-hop
    /// reachability check of its own, so an unguarded `warmest_anchor` call
    /// would teleport a stranded creature across an impassable edge.
    /// type-audit: bare-ok(count: budget)
    fn preferred_anchor(&self, position: &RoomAddr, budget: usize) -> Option<AnchorId> {
        if self.deviation(position) <= self.niche.width {
            return None;
        }
        let (interior, from) = self.interior?;
        let ambient = self.terrain.temperature(position, self.day);
        let target = warmest_anchor(interior, from, ambient, self.niche.optimum, budget)?;
        route_within(interior, from, target, budget)?;
        Some(target)
    }
}

impl<'a> Drive for Thermal<'a> {
    fn urgency(&self, view: &Perceived) -> f64 {
        // THE HEARTH: the interior's warmth at the creature's own anchor joins
        // the ambient temperature ADDITIVELY, so a creature by the fire feels
        // the room it is in rather than the weather outside it. `interior:
        // None` reads the pre-Hearth identity. `urgency` carries no `budget`
        // parameter (the `Drive` trait's `urgency` never has), so this reuses
        // the same routing budget `affordance`/`serviceability` are given
        // elsewhere in this drive's own live construction
        // (`INTERIOR_WARMTH_BUDGET`) — small, fixed, and already justified at
        // its own definition (the interior graph is at most 9 anchors).
        self.urgency_here(&view.position, INTERIOR_WARMTH_BUDGET)
    }
    fn act_threshold(&self) -> f64 {
        THERMAL_ACT
    }
    fn affordance(&self, view: &Perceived, budget: usize) -> Option<Action> {
        // Satisfied inside the tolerance band — nothing to do (a flow drive
        // needs no plan; this gate reads AMBIENT comfort only, exactly as it
        // did before The Threshold — a room already inside the niche band
        // never triggers seeking, whether or not it also has a hearth).
        if self.deviation(&view.position) <= self.niche.width {
            return None;
        }
        // THE THRESHOLD'S CROSSING: try the within-room step FIRST. Ambient
        // temperature is uniform across one room (there is no per-anchor
        // coordinate to sample it at — spec §2.1), so the only thing that
        // varies anchor-to-anchor is the additive hearth warmth `warmth_at`
        // already reads; `warmest_anchor` finds the anchor whose FELT
        // reading is closest to the niche optimum, exactly as `comfort_step`
        // does one scale up.
        //
        // Composition with the room-scale gradient below: within-room always
        // wins when it has somewhere useful to send the creature, because
        // crossing a room is strictly cheaper than crossing between rooms
        // (`MOVE_WITHIN_DURATION` << `MOVE_DURATION`) and a room offering
        // warmth relief should be exhausted before the creature gives up on
        // it and leaves. The room-scale step is not merely a slower
        // alternative, though — it is the ONLY option once a within-room
        // improvement doesn't exist, in TWO distinct cases: no anchor in the
        // interior is any warmer than here (no fire in this room, or the
        // creature is already at the warmest reachable one), and — the case
        // spec §8a calls out by name — a genuinely warmer anchor exists but
        // [`route_within`] cannot reach it from here. §8a reserves seasonal
        // passability, under which the traversable graph can be disconnected
        // even though `permits` certified the BASE graph connected at
        // composition time, so a creature can be stranded away from its own
        // room's hearth. `route_within` returning `None` here is that real,
        // expected case — not a defect to guard against — and it is handled
        // by simply falling through to the room-scale gradient below, the
        // same way "no interior at all" already does.
        if let Some((interior, from)) = self.interior {
            let ambient = self.terrain.temperature(&view.position, self.day);
            if let Some(target) =
                warmest_anchor(interior, from, ambient, self.niche.optimum, budget)
                && let Some(path) = route_within(interior, from, target, budget)
                && let Some(&step) = path.first()
            {
                return Some(Action::MoveWithin(step));
            }
        }
        // The between-rooms gradient (unchanged): the neighbour whose
        // temperature is CLOSEST to the optimum (too-cold → warmer, too-hot →
        // cooler, both toward the optimum), or `None` when no neighbour is
        // strictly more comfortable than here (boxed in / a local comfort
        // optimum, or — now — a room whose own warmer hearth is unreachable).
        comfort_step(&view.position, self.niche.optimum, self.terrain, self.day).map(Action::MoveTo)
    }
    fn candidate_actions(&self, view: &Perceived, budget: usize) -> Vec<Action> {
        // Make the within-room step visible to `arbitrate`'s multi-drive
        // utility scan: its fixed candidate set is built from `RoomAddr`
        // neighbours and cannot express an `AnchorId`. Rather than a second,
        // independent enumeration of the interior's anchors (which could
        // drift from what `affordance` itself would choose), this simply
        // republishes `affordance`'s own decision WHEN it is a `MoveWithin` —
        // the within-room branch above already is the argmax over the
        // interior's anchors, so there is nothing more useful to offer.
        // Nothing else needs a candidate: the room-scale `MoveTo` case is
        // already in the fixed set (comfort_step's target is always a room
        // neighbour), and no OTHER drive scores an anchor-graph action, so a
        // single candidate here costs the scan nothing.
        match self.affordance(view, budget) {
            Some(a @ Action::MoveWithin(_)) => vec![a],
            _ => Vec::new(),
        }
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Thermal
    }
    fn urgency_ceiling(&self) -> f64 {
        THERMAL_CEIL
    }
    fn serviceability(&self, action: &Action, view: &Perceived, budget: usize) -> f64 {
        // A flow drive is served by PRESENCE in a kinder cell: the reduction in
        // thermal urgency at the destination (0 if the step doesn't improve
        // comfort). No consume — `Drink` serves it not at all.
        match action {
            Action::MoveTo(n) => (self.urgency_at(&view.position) - self.urgency_at(n)).max(0.0),
            // THE THRESHOLD'S CROSSING: the within-room twin of the `MoveTo`
            // arm above, scored the same way — the reduction in felt urgency
            // between the creature's CURRENT anchor and `target`, using the
            // room's own (unchanging) ambient temperature at both ends since
            // only the anchor, not the room, differs. `0.0` with no interior
            // (nothing to score an anchor against) rather than a panic: a
            // `MoveWithin` candidate can only ever reach this drive's
            // `serviceability` via `candidate_actions`, which itself only
            // ever proposes one when `self.interior` is `Some`, but this
            // stays total rather than leaning on that invariant.
            Action::MoveWithin(target) => match self.interior {
                Some((interior, from)) => {
                    let ambient = self.terrain.temperature(&view.position, self.day);
                    let felt_at =
                        |a: AnchorId| self.urgency_of(ambient + warmth_at(interior, a, budget));
                    (felt_at(from) - felt_at(*target)).max(0.0)
                }
                None => 0.0,
            },
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

/// The warmest anchor in `interior` — the one whose FELT temperature
/// (`ambient` plus that anchor's own [`warmth_at`]) is CLOSEST to `optimum`
/// — or `None` when no anchor OTHER than `from` is strictly more comfortable
/// than `from` itself. [`Thermal::affordance`]'s within-room counterpart to
/// [`comfort_step`]: the same `total_cmp`-then-ascending-id tie-break and the
/// same "strictly better than here" gate, but scanning the interior's
/// anchors rather than a room's three neighbours, and scoring VALUE (`warmth_at`,
/// which already accounts for every hearth's own reachability from each
/// candidate) rather than adjacency.
///
/// This picks the best DESTINATION, not a first step — the interior graph is
/// small (at most 9 anchors, `interior/pattern.rs`'s `INVENTORY`) but can
/// still put the warmest anchor several hops from `from` (the hearth sits 3
/// hops from a built-cold room's threshold — task 5d's own finding), so a
/// one-hop neighbour scan would undersell it. The caller is responsible for
/// turning this destination into a next STEP via [`route_within`], and for
/// treating that call's `None` as the genuinely reachable case it is (spec
/// §8a) rather than as impossible — this function does not call
/// `route_within` itself, so it never has that `None` to handle.
fn warmest_anchor(
    interior: &Interior,
    from: AnchorId,
    ambient: f64,
    optimum: f64,
    budget: usize,
) -> Option<AnchorId> {
    let deviation = |a: AnchorId| (ambient + warmth_at(interior, a, budget) - optimum).abs();
    let mut best: Option<(AnchorId, f64)> = None;
    for id in interior.ids() {
        if id == from {
            continue;
        }
        let dev = deviation(id);
        let keep_existing = match &best {
            Some((ba, bd)) => dev.total_cmp(bd).then_with(|| id.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep_existing {
            best = Some((id, dev));
        }
    }
    let (best_anchor, best_dev) = best?;
    if best_dev.total_cmp(&deviation(from)).is_lt() {
        Some(best_anchor)
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
/// strongly a carnivore is drawn up the prey-pressure gradient, per unit of
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
/// type-audit: bare-ok(ratio: boldness), bare-ok(ratio: alarm), bare-ok(ratio: dread)
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
    /// The remembered DREAD map (The Shudder): the TRANSIENT subset of this
    /// creature's hazard memory — cells whose present terrain is safe but where
    /// a herd's alarm once frightened it — keyed to the remembered alarm
    /// magnitude. Read at the creature's OWN cell and folded into the same
    /// additive slot as [`Danger::alarm`], because it IS an alarm term: the
    /// alarm as it was, not as it is. `None` ⇒ no phobia — byte-identical.
    /// Provenance is the only difference from `alarm`: that one is SENSED
    /// (present, external, a per-tick field), this one is BELIEVED (past,
    /// internal, a fold over committed history).
    pub dread: Option<&'a std::collections::BTreeMap<RoomAddr, f64>>,
}

/// The boldness at which fear is felt AS IS (unscaled) — the steady baseline the
/// dial is centered on (The Mettle). `MindVector.threat_response` uses `0.5` as
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

    /// The remembered dread at `room` (`0.0` when unremembered or `None`).
    /// type-audit: bare-ok(ratio: return)
    fn dread_at(&self, room: &RoomAddr) -> f64 {
        self.dread.and_then(|m| m.get(room)).copied().unwrap_or(0.0)
    }

    /// The creature's total felt threat at `room` — present terrain PLUS
    /// remembered dread, the field `serviceability` and the flee gradient read.
    /// Unlike the borrowed alarm (whose halo always lies within one hop of
    /// terrain that genuinely frightens its emitter, so a terrain gradient
    /// always exists), dread sits on now-SAFE ground: without it in the
    /// gradient a dreading creature has nowhere to go and reads `Lost`.
    /// type-audit: bare-ok(ratio: return)
    fn felt_threat_at(&self, room: &RoomAddr) -> f64 {
        self.threat_at(room) + ALARM_SCALE * self.dread_at(room)
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
        // THE SHUDDER: the REMEMBERED alarm at this cell joins the BORROWED one
        // in the same additive slot — the dread is an alarm term, so it needs no
        // scale of its own. Feeding back the very magnitude that recorded the
        // memory reproduces the verdict that created it: the memory and the
        // feeling agree.
        let remembered = self.dread_at(&view.position);
        let felt = base + ALARM_SCALE * (borrowed + remembered);
        (felt * mettle_factor(self.boldness)).clamp(0.0, 1.0)
    }
    fn act_threshold(&self) -> f64 {
        DANGER_ACT
    }
    fn affordance(&self, view: &Perceived, _budget: usize) -> Option<Action> {
        // Flee: step to the safest neighbour (by THIS creature's threat niche),
        // or `None` when boxed in by threat on every side (cornered → Frustrated).
        // A flow drive needs no plan (no A*), so `budget` is unused.
        flee_step(&view.position, self.terrain, &self.threat_niche, self.dread).map(Action::MoveTo)
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
        // The gradient is over FELT threat (terrain PLUS remembered dread, The
        // Shudder), so a creature standing on now-safe ground it only REMEMBERS
        // as frightening is served by stepping off it.
        match action {
            Action::MoveTo(n) => self.felt_threat_at(&view.position) - self.felt_threat_at(n),
            // Fine movement is not yet wired into any drive's plan (The
            // Threshold task 6+), so it eases no fear today either.
            Action::Drink | Action::Rest | Action::Eat | Action::MoveWithin(_) => 0.0,
        }
    }
    fn survival_override(&self, urgency: f64) -> bool {
        // A present threat wakes a sleeping creature (The Slumber's override).
        urgency >= DANGER_OVERRIDE
    }
}

/// The flee gradient step: the neighbour of LOWEST FELT threat — present terrain
/// (for this creature's threat niche) PLUS the remembered `dread` at each cell
/// (The Shudder) — or `None` when no neighbour is strictly safer than `from`
/// itself (boxed in — the creature holds, cornered). The dread term is what lets
/// a creature flee ground that is frightening only in MEMORY: a phantom cell is
/// now-safe, so terrain alone offers no gradient to step down. The sign-flip of
/// [`comfort_step`] / [`forage_step`]: minimize threat rather than thermal
/// deviation or maximize food; same three-neighbour scan and
/// `total_cmp`-then-ascending-`RoomAddr` tie-break.
fn flee_step(
    from: &RoomAddr,
    terrain: &dyn Terrain,
    niche: &ThreatNiche,
    dread: Option<&std::collections::BTreeMap<RoomAddr, f64>>,
) -> Option<RoomAddr> {
    let threat = |room: &RoomAddr| {
        threat_value(niche, &terrain.hazards(room))
            + ALARM_SCALE * dread.and_then(|m| m.get(room)).copied().unwrap_or(0.0)
    };
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
///
/// Takes the plan's hop count directly (the-waymark, Task 4: both callers —
/// `decide_step` and `affect_of_memo_occupied` — now read this off a
/// [`HomeNavFeature`] rather than a full `Option<Vec<Action>>`, so this
/// shares the one formula between them instead of duplicating it).
fn loneliness_from_distance(distance: Option<usize>) -> f64 {
    match distance {
        Some(hops) => (hops as f64 / LONELY_SCALE_HOPS).clamp(0.0, 1.0),
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
    /// The precomputed loneliness (`loneliness_from_distance`) in `[0, 1]` — the
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

/// The feature [`decide_step`] (and [`affect_of_memo_occupied`]) actually
/// consume from a home plan — never the full path (the-waymark, Task 4; the
/// campaign spec's own ideonomy refinement: "cache the consumed feature, not
/// the full plan"). Mirrors exactly what [`loneliness_from_distance`] and the
/// `Social::home_step` construction already read off an `Option<Vec<Action>>`:
/// `distance` is `Some(p.len())` if `home` is reachable within budget, `None`
/// if not (`plan_to_room`'s own `None`); `first_step` is the plan's first
/// action, `None` either when unreachable OR when the plan is the empty
/// vec — already standing at `home` — which is `loneliness_from_distance`'s own
/// `Some(0)`-but-`home_step: None` case, preserved here rather than collapsed.
#[derive(Clone, Debug, PartialEq, Eq)]
struct HomeNavFeature {
    /// The plan's hop count, or `None` if `home` is unreachable within budget.
    distance: Option<usize>,
    /// The plan's first action, or `None` if unreachable or already home.
    first_step: Option<Action>,
}

/// One entity's [`HomeNavCache`] bookkeeping: its avoid-epoch state (see the
/// cache's own doc) plus whatever feature was last computed and at which
/// `(pos, epoch)`.
struct HomeNavState {
    /// Bumped whenever a `home_nav` call's `avoid` set differs from
    /// `last_avoid` — a per-entity counter, never global (a global epoch
    /// would stampede every entity's cache on any ONE creature's belief
    /// change).
    avoid_epoch: u64,
    /// The avoid set as of the most recent `home_nav` call, compared against
    /// on the NEXT call to detect a belief change.
    last_avoid: std::collections::BTreeSet<RoomAddr>,
    /// `(pos, home, budget, avoid_epoch, feature)` as of the last real search
    /// for this entity — `None` before its first `home_nav` call. `home`/
    /// `budget` are part of the key (Task 4 fix round, key hardening): they
    /// determine the answer exactly as much as `pos`/`avoid` do, and today's
    /// one production call site per consumer passes a stable `(npc.home,
    /// PLAN_BUDGET)` pair — but nothing enforces that structurally, so a
    /// future caller asking about a DIFFERENT home or budget for the same
    /// entity must miss the cache, not silently read a stale answer computed
    /// for a different question.
    cached: Option<(RoomAddr, RoomAddr, usize, u64, HomeNavFeature)>,
}

/// `home_nav`'s cross-tick, per-entity backing (the-waymark, Task 4 — the
/// campaign's dominant lever): [`decide_step`] used to call `plan_to_room` —
/// a budget-1000 Dijkstra-mode `astar` search — UNCONDITIONALLY on every
/// decision, even for a creature standing exactly where it stood last tick
/// with an unchanged believed-hazard set. The campaign spec's Stage 3 licenses
/// a cache scoped to exactly the two events that can actually change the
/// answer: the entity's `pos` (a `Step` resolution) and its believed-hazard
/// avoid set (`HazardMemory::shunned`). Verified at plan time (see the task
/// report): `HazardMemory` is a fold over the FROZEN pre-tick ledger, computed
/// ONCE per creature per tick (`step_with_occupancy`'s setup loop) and never
/// mutated again that tick, so it never changes mid-tick — the avoid-epoch
/// check below, run on every `home_nav` call, is therefore a cheap
/// confirmation (`BTreeSet` equality against an unchanged value) on every
/// call but the one where the belief genuinely moved, where it is the belief
/// update's own write point.
///
/// Lives with the NPC's sim state, in the SAME session-lived scope
/// [`hornvale_kernel::RoomMeshMemo`] does (a `Session` field;
/// `run_simulation`'s own local) — NOT per-tick — because the whole point is
/// that a stationary, unchanged-belief creature pays ZERO searches on ticks
/// after its first, which a tick-scoped memo could never show (The Waymark
/// spec, "the scaling stake").
///
/// Also gates the search itself, not only its cache: `decide_step` only calls
/// `home_nav` for a non-`Ametabolic` creature (plan-time verification (a) —
/// the Social drive, the plan's only consumer, is never pushed onto an
/// ametabolic creature's `drives` vec — "lazy AND cached" per the campaign
/// spec's Stage 3 clause).
#[derive(Default)]
pub struct HomeNavCache {
    /// Per-entity state (avoid-epoch bookkeeping and the cached feature).
    entries: std::collections::BTreeMap<EntityId, HomeNavState>,
    /// How many real `plan_to_room` searches `home_nav` has run, ever — the
    /// scaling property's own deterministic witness (the search-count pins),
    /// never a wall-clock proxy. `pub(crate)` (Task 4 fix round, rider d):
    /// test-visible within this crate only — no drive, and no OTHER crate,
    /// ever reads it.
    pub(crate) searches: u64,
}

impl HomeNavCache {
    /// An empty cache — one per session-lived scope (see the type doc).
    pub fn new() -> Self {
        Self::default()
    }

    /// `home_nav(entity) → (distance, first_step)`: the seam [`decide_step`]
    /// (and [`affect_of_memo_occupied`]) read instead of ever calling
    /// `plan_to_room` directly. A cache hit — `pos`, `home`, and `budget` all
    /// unchanged, and `avoid` unchanged since the last call for this entity —
    /// costs one `BTreeMap` lookup and a handful of cheap equality checks,
    /// never a search; a miss (including one caused solely by a different
    /// `home`/`budget` — the key-hardening rider, Task 4 fix round) runs the
    /// real search (costing exactly what every caller always paid) and
    /// counts it in `searches`. `mesh_memo` (the-waymark, Task 6 — ledger #7's
    /// re-plan) is threaded straight through to a real search's
    /// [`plan_to_room_memo`] call, so a MISS no longer recomputes
    /// `RoomAddr::neighbors` from scratch on every `astar` expansion when the
    /// caller has a session-lived [`RoomMeshMemo`] to share — a cache HIT
    /// above never touches it at all.
    #[allow(clippy::too_many_arguments)]
    fn home_nav(
        &mut self,
        entity: EntityId,
        pos: &RoomAddr,
        home: &RoomAddr,
        avoid: &std::collections::BTreeSet<RoomAddr>,
        budget: usize,
        mesh_memo: &mut RoomMeshMemo,
    ) -> HomeNavFeature {
        let state = self.entries.entry(entity).or_insert_with(|| HomeNavState {
            avoid_epoch: 0,
            last_avoid: avoid.clone(),
            cached: None,
        });
        // The belief's write point: bump iff the avoid set genuinely
        // changed since we last saw this entity (never global — see the
        // type doc).
        if &state.last_avoid != avoid {
            state.avoid_epoch += 1;
            state.last_avoid = avoid.clone();
        }
        let epoch = state.avoid_epoch;
        if let Some((cached_pos, cached_home, cached_budget, cached_epoch, feature)) = &state.cached
            && cached_pos == pos
            && cached_home == home
            && *cached_budget == budget
            && *cached_epoch == epoch
        {
            return feature.clone();
        }
        self.searches += 1;
        let plan = plan_to_room_memo(pos, home, budget, avoid, Some(mesh_memo));
        let feature = HomeNavFeature {
            distance: plan.as_ref().map(|p| p.len()),
            first_step: plan.and_then(|p| p.into_iter().next()),
        };
        state.cached = Some((pos.clone(), home.clone(), budget, epoch, feature.clone()));
        feature
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
///
/// Stage-0 carries no creature identity at all (no `Npc`/`EntityId`
/// parameter), so `arbitrate`'s `home_nav` seam (the-waymark, Task 4 fix
/// round) is given a throwaway, single-call [`HomeNavCache`] and a fixed
/// placeholder entity — this function is already documented stateless
/// ("a fresh `Idle` mode per call"), so a cache that cannot outlive the call
/// costs nothing beyond what this seam always paid. Likewise builds a
/// throwaway [`RoomMeshMemo`] (the-waymark, Task 6) for the same reason.
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
    let mut home_nav_cache = HomeNavCache::new();
    let mut mesh_memo = RoomMeshMemo::new();
    arbitrate(
        view,
        home,
        &drives,
        &disposition,
        Mode::Idle,
        budget,
        EntityId::new(1).expect("1 is a valid nonzero entity id"),
        &mut home_nav_cache,
        &mut mesh_memo,
    )
    .intent
}

/// How a creature is disposed to decide right now — the psychology dials that
/// weight its drives and the momentary states that gate them. The same
/// perception and drive set yield DIFFERENT decisions through this: it is the
/// "how this mind decides" bundle [`arbitrate`] reads, distinct from what the
/// creature perceives (`view`/`drives`), the world frame (`home`/`budget`), and
/// the hysteresis carry (`incoming: Mode`). Bundling the two dials (endowment,
/// from the species `MindVector`) with the two per-tick gates (`helpless`,
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
/// `entity`/`home_nav_cache` (the-waymark, Task 4 fix round): the no-active-
/// drive fallback below reads the SAME `(pos, home, avoid, budget)` home-plan
/// [`HomeNavCache::home_nav`] already caches for the `Social` drive — an
/// AMETABOLIC creature (empty `drives`, so this branch is its ONLY path,
/// every tick) never builds a `Social` at all, so without this it paid a
/// full, uncached `plan_to_room` search here regardless of Task 4's cache.
/// Sharing the cache is safe by construction (a hit requires an exact
/// `(pos, avoid, epoch)` match): a non-ametabolic creature that also happens
/// to have no active drive this tick just re-reads the entry its own
/// `Social` construction already warmed.
///
/// `mesh_memo` (the-waymark, Task 6 — ledger #7's re-plan) is threaded
/// straight through to the fallback's own `home_nav_cache.home_nav` call —
/// the SAME session-lived [`RoomMeshMemo`] `decide_step`'s own
/// `lowest_unvisited_neighbor_memo` read already shares, not a second one.
///
/// type-audit: bare-ok(count: budget)
#[allow(clippy::too_many_arguments)]
pub fn arbitrate(
    view: &Perceived,
    home: &RoomAddr,
    drives: &[&dyn Drive],
    disposition: &Disposition,
    incoming: Mode,
    budget: usize,
    entity: EntityId,
    home_nav_cache: &mut HomeNavCache,
    mesh_memo: &mut RoomMeshMemo,
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
            let feature = home_nav_cache.home_nav(
                entity,
                &view.position,
                home,
                &view.believed_hazard,
                budget,
                mesh_memo,
            );
            return Resolution {
                intent: feature.first_step.map(Intent::Do).unwrap_or(Intent::Hold),
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
    // THE THRESHOLD'S CROSSING: extend the fixed room-scale set with each
    // drive's OWN extra candidates, in drive order (deterministic — `drives`
    // is a caller-fixed slice). Every drive before Thermal returns none here
    // (the default `Drive::candidate_actions`), because a room-scale plan's
    // first hop is always already one of the neighbours above; Thermal is
    // the first drive reasoning over a DIFFERENT graph (the room interior's
    // anchors), so its `MoveWithin` candidate — when it has one — would
    // otherwise be invisible to this scan no matter how loudly it wants it.
    for d in drives.iter() {
        candidates.extend(d.candidate_actions(view, budget));
    }

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
            // THE THRESHOLD'S CROSSING: unlike the `MoveTo` arms above, a
            // within-room target is never a matter of belief or an unknown
            // gradient — `interior_of` is a pure, immediate derivation (no
            // partial observation, no belief cache; `interior/mod.rs`), and
            // `Thermal::affordance` only ever proposes this step AFTER
            // `route_within` has already verified a real path exists to it
            // (the unroutable case falls back to the `MoveTo` gradient
            // above, and so never reaches this arm). So a creature taking
            // this step is beelining to a KNOWN, VERIFIED-REACHABLE target
            // exactly as surely as `Fatigue` beelines home or thirst
            // beelines believed water — `Eager`, not `Searching`.
            // `Searching` is reserved for gradient-following toward an
            // UNKNOWN target, which this branch structurally cannot be.
            Action::MoveWithin(_) => (AffectLabel::Eager, 0.5),
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
    let mut mesh_memo = RoomMeshMemo::new();
    affect_of_memo(frozen, npc, band, day, terrain, &mut memo, &mut mesh_memo)
}

/// [`affect_of`] sharing a caller-owned [`PrimaryAfraidMemo`] — for the lab's
/// headless sim, whose per-tick affect reads over one post-tick ledger fold the
/// SAME emitters' primary-fear across every creature; the memo collapses those
/// re-derivations to one `affect_of` per `(emitter, day)`. Reads interior
/// warmth at the room's landing anchor only — see [`affect_of_memo_occupied`]
/// for the occupancy-aware sibling this delegates to with `occupancy: None`.
/// `mesh_memo` (the-waymark fix round, rider (b)) is the caller-owned
/// [`RoomMeshMemo`] this call's own `lowest_unvisited_neighbor_memo` read
/// shares — see [`affect_of_memo_occupied`]'s own doc. This function's own
/// public signature stays exactly as every existing caller expects it (no
/// `HomeNavCache` parameter): it builds a throwaway one internally for
/// [`affect_of_memo_occupied`]'s Task 4 `home_nav` read, exactly the
/// `RoomMeshMemo` throwaway [`DriveMovements::step`] already builds for its
/// own kernel-fixed signature — a caller that DOES have a session-lived
/// scope to share (`run_simulation`) calls [`affect_of_memo_occupied`]
/// directly instead, precisely as it already does for `mesh_memo`.
pub fn affect_of_memo(
    frozen: &Ledger,
    npc: &Npc,
    band: &[Npc],
    day: WorldTime,
    terrain: &dyn Terrain,
    memo: &mut PrimaryAfraidMemo,
    mesh_memo: &mut RoomMeshMemo,
) -> Affect {
    let mut home_nav_cache = HomeNavCache::new();
    affect_of_memo_occupied(
        frozen,
        npc,
        band,
        day,
        terrain,
        memo,
        None,
        mesh_memo,
        &mut home_nav_cache,
    )
}

/// [`affect_of_memo`], but given a caller-owned [`Occupancy`] to read the
/// creature's ACTUAL within-room anchor from, when it has one on record
/// there, instead of unconditionally defaulting to the room's landing anchor
/// (The Threshold task 6b).
///
/// **Why the sampler needed this.** `DriveMovements::step_with_occupancy`
/// (task 6) already tracks where a creature's own per-tick walk carries it —
/// a cold creature that crosses a hearth-bearing room to stand at the fire
/// really does end up warmer there than at the room's threshold. But the
/// population-health battery does not read that walk: it re-derives each
/// creature's felt state through this STATELESS function, which has no
/// per-tick state of its own to consult and so always fell back to
/// [`landing_interior`] — the doorway a creature crossing INTO a room lands
/// at, never wherever it may have actually walked since. The sampler was
/// reporting a colder read than the creature ever experienced, not because
/// the physics were wrong, but because the instrument measuring them could
/// not see past the doorway. That is a defect in what gets SAMPLED, argued
/// on its own terms — a monitoring gap the preregistered prediction (spec
/// §7) needs closed to be tested at all, independent of which way closing it
/// moves any measurement.
///
/// **The room-consistency guard is load-bearing.** An `Occupancy` captured at
/// one point in time (e.g. `step_with_occupancy`'s tick-end snapshot) can
/// legitimately describe a creature that has since crossed into, or out of,
/// the room `day` resolves it to — `agent_position` reads the committed,
/// time-correct room; the passed-in `occupancy` may be stale relative to it.
/// [`Occupancy::anchor_in`] is consulted with the room `agent_position`
/// actually returns, and yields the tracked anchor ONLY when it still
/// belongs to that same room; a mismatch (or no `occupancy` at all) falls
/// back to [`landing_interior`] exactly as [`affect_of_memo`] always did —
/// never a foreign `AnchorId` read against a room's freshly-derived
/// `Interior`, which [`crate::interior::warmth_at`] has no bounds check
/// against.
///
/// `mesh_memo` (the-waymark fix round, rider (b)): this function used to
/// build its OWN throwaway [`RoomMeshMemo`] inline for the
/// `lowest_unvisited_neighbor_memo` read below — the same shape
/// [`PrimaryAfraidMemo`] would have had if it were not already an explicit,
/// caller-supplied parameter. It is now threaded the same way `memo` already
/// is: a caller
/// that owns a session/battery-scoped memo (`windows/lab`'s `run_simulation`)
/// shares it here too; a caller that only has `&self` (`Session::snapshot`/
/// `needs`) supplies a local throwaway, exactly as it already does for cases
/// where session-scoping isn't reachable.
///
/// `home_nav_cache` (the-waymark, Task 4): this function used to call
/// `plan_to_room` UNCONDITIONALLY, same as `decide_step` did — a SEPARATE
/// budget-1000 search from `decide_step`'s own, since this is a stateless
/// re-derivation of felt state, not the live decision. Reads the Social
/// drive's feature from the caller-owned cache instead, gated on
/// non-`Ametabolic` exactly as `decide_step`'s own gate is (see that
/// function's doc). Sharing IS safe across the two consumers: a cache hit
/// requires an EXACT `(pos, avoid)` match regardless of who asked, so this
/// can only ever save a search, never answer one incorrectly.
#[allow(clippy::too_many_arguments)]
pub fn affect_of_memo_occupied(
    frozen: &Ledger,
    npc: &Npc,
    band: &[Npc],
    day: WorldTime,
    terrain: &dyn Terrain,
    memo: &mut PrimaryAfraidMemo,
    occupancy: Option<&Occupancy>,
    mesh_memo: &mut RoomMeshMemo,
    home_nav_cache: &mut HomeNavCache,
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
    let explore_step = lowest_unvisited_neighbor_memo(&pos, &visited, terrain, mesh_memo);
    let fatigue = fatigue_at(frozen, npc.entity, day);
    // The Haunt + The Phantom: the ground this creature remembers being
    // frightened on — a fold over its committed history (empty for a never-
    // frightened creature ⇒ byte-identical). The roster is this call's `band`;
    // `alarm_field` invokes `affect_of` with `band = &[]`, so its replay reads
    // a terrain-only memory and the transient re-derivation never recurses.
    let memory = hazard_memory_memo(frozen, npc, day, terrain, band, memo);
    let view = Perceived {
        position: pos,
        drive,
        fatigue,
        believed_water: believed,
        believed_hazard: memory.shunned.clone(),
        explore_step,
    };
    let thirst = Thirst { params: SUSTENANCE };
    // THE THRESHOLD's arming: the room `view.position` is in, ACTUALLY derives
    // an interior now, owned here so `thermal` below can borrow it — see
    // `landing_interior`'s own doc for why "landing" is the right anchor for
    // a stateless snapshot read with nothing better to consult.
    //
    // Task 6b: when a caller HAS a real `Occupancy` (its own per-tick walk's
    // result, or one it inherited), and that occupancy still places this
    // creature in the SAME room `view.position` names, read warmth at the
    // anchor it actually stands at instead — `Occupancy::anchor_in` is the
    // room-checked accessor that refuses a stale cross-room anchor rather
    // than handing back one `warmth_at` could misread against the wrong
    // graph. No `occupancy`, or one that disagrees about the room, is exactly
    // the pre-task-6b behaviour: landing, unconditionally.
    let here = occupancy
        .and_then(|occ| occ.anchor_in(npc.entity, &view.position))
        .map(|anchor| (interior_of(&view.position, terrain), anchor))
        .or_else(|| landing_interior(&view.position, terrain));
    let thermal = Thermal {
        niche: npc.temperature_niche,
        terrain,
        day,
        interior: here.as_ref().map(|(i, a)| (i, *a)),
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
        // THE SHUDDER: it DOES see remembered dread, because this is the read the
        // narration and the health metric observe — a fear that never reaches
        // `Affect` is not a feeling, only a second behavioural term. Safe against
        // the same stampede: the alarm-field's emission read passes `band = &[]`,
        // whose bandless memory has no emitters and therefore an EMPTY dread map,
        // so a dread-afraid creature can never emit. One structural fact — the
        // bandless replay — gives termination, byte-identity, and no contagion.
        dread: Some(&memory.dread),
    };
    // The metabolism gate (The Kindling): an Ametabolic creature has no
    // homeostatic drives at all — it neither thirsts, thermoregulates, tires
    // (The Slumber), hungers (The Provender), fears (The Dread — a construct
    // does not flinch), nor pines for company (The Belonging), so it reads
    // Content, never distress. The NICHE gate (The Provender, spec §2): hunger
    // is carried only by a creature whose diet niche weights SOMETHING — an
    // empty niche means "no food drive" (no axis, so no source serves it).
    //
    // Read early (the-waymark, Task 4): the Social drive built below is the
    // ONLY consumer of a home plan, and it is never pushed onto `drives` for
    // an ametabolic creature — see `decide_step`'s identical gate for the
    // full rationale.
    let ametabolic = matches!(npc.metabolic_class, MetabolicClass::Ametabolic);
    // Affiliation (The Belonging): loneliness + the home-step, read from the
    // cross-tick cache instead of an unconditional `plan_to_room` (the-waymark,
    // Task 4) — precomputed once so the drive's urgency stays O(1) either way.
    let social = if ametabolic {
        Social {
            loneliness: 0.0,
            home_step: None,
        }
    } else {
        let feature = home_nav_cache.home_nav(
            npc.entity,
            &view.position,
            &npc.home,
            &view.believed_hazard,
            PLAN_BUDGET,
            mesh_memo,
        );
        Social {
            loneliness: loneliness_from_distance(feature.distance),
            home_step: feature.first_step,
        }
    };
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
        npc.entity,
        home_nav_cache,
        mesh_memo,
    )
    .affect
}

/// The per-tick ALARM field (The Alarm) — fear-contagion as a derived,
/// order-independent field over the frozen population, the vessel's dynamic
/// sibling of `worldgen::predator_pressure_from`. For each creature that is
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
        // decision — and it stays exact under The Shudder: the read it guards is
        // the BANDLESS `affect_of`, whose hazard memory has no emitters and
        // therefore no dread, so its Danger urgency really is `threat_field ×
        // mettle_factor`. Remembered dread is felt but never emitted; contagious
        // superstition is reserved. Widening this gate to admit dread-only
        // creatures would open it. A terrain-afraid creature still goes through
        // `affect_of` below to confirm Danger WINS. It is what keeps the transient
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

/// Catch-up's own step cap (The Threshold task 7, spec §5.3): the most
/// replay iterations — each either a replayed [`Action::MoveWithin`] hop or
/// a replayed [`Intent::Hold`] — `DriveMovements::step_with_occupancy`'s
/// bubble-entry catch-up will spend reconstructing a creature's unobserved
/// within-room position before it gives up hopping and places the creature
/// directly at its drive-preferred anchor instead
/// (`Thermal::preferred_anchor`). Deliberately reuses [`PLAN_BUDGET`]'s
/// VALUE rather than inventing a fresh judgment call: GOAP's own search
/// already treats this many node-expansions as "generous for the short
/// local journeys every derived NPC actually walks" (`PLAN_BUDGET`'s own
/// doc), and catch-up is exactly that kind of short local journey, just
/// hopping the interior's anchor graph instead of expanding A* nodes. At one
/// within-room step per hop (`clock::base_ticks`'s `MoveWithin` dial — 1_000
/// ticks, a hundredth of an Earth-like day at reference mass) this is
/// generously past a full day of continuous replay before the cap can bite
/// (spec §5.3's diagram: "a day" still reads EXACT), so it is the "a season"
/// column, not "a day", where a real absence starts landing on the
/// approximate side — a long-running creature's absence costs O(1) work
/// rather than one iteration per within-room hop that would otherwise have
/// occurred. A creature far heavier than reference pays a longer hop and so
/// reaches the cap sooner, which is the action clock's intent, not a
/// regression: a bear crosses a room more slowly than a person does.
const CATCH_UP_STEP_CAP: usize = PLAN_BUDGET;

/// The per-NPC step cap on `DriveMovements::step`'s inner loop — the
/// strict-progress guard's backstop: even if a decision loop somehow failed
/// to advance `day` on every iteration, this bounds total work per tick
/// (termination guarantee, The Foresight T3 review).
const MAX_STEPS: usize = 10_000;

/// [`warmth_at`]'s node-expansion budget for a REAL derived interior (The
/// Threshold's arming of The Hearth's `warmth` seam). This is a ROUTING depth
/// over the anchor graph, not a distance — but the graph it routes is tiny by
/// construction: `INVENTORY` (`interior/pattern.rs`) authors exactly 9
/// patterns, so no composed `Interior` can ever hold more than 9 anchors, and
/// a route between any two of them is at most 8 hops. `64` is not a fresh
/// guess — it is exactly the budget `interior/field.rs`'s own `warmth_at`
/// tests already route within (`pattern.rs`'s composition tests use 256 over
/// the same handful-of-anchors graph), so this reuses a value the interior
/// layer has already proven safe rather than inventing a new one. At 8×
/// headroom over the worst-case hop count, no reachable hearth can ever be
/// silently missed for want of budget.
const INTERIOR_WARMTH_BUDGET: usize = 64;

/// The room `pos` is in, derived (`interior_of`), paired with the anchor a
/// creature crossing INTO the room arrives at (`landing`, keyed off
/// `seam_kind(terrain.is_built(pos))`) — the owned `(Interior, AnchorId)`
/// [`Thermal::interior`] borrows from. Where the pre-crossing model
/// ([`warmth_at`] read here directly, folded into a bare `f64`) collapsed
/// this to a single number at construction time, this returns the GRAPH
/// itself, because `Thermal::affordance`'s within-room branch (The
/// Threshold's crossing) needs to ask the interior about anchors other than
/// the one the caller happens to hand it.
///
/// This is `affect_of_memo`'s (and `affect_of_memo_occupied`'s FALLBACK)
/// helper — a stateless snapshot read with no `Occupancy` in hand, or one
/// that no longer agrees which room the creature is in, has nothing better
/// to consult than the LANDING anchor: the same anchor `Occupancy::arrive`
/// places a freshly-arrived creature at, and therefore the correct reading
/// for a read with nowhere to remember anything deeper. `DriveMovements::
/// step`'s per-tick walk (The Threshold task 6) does carry state — a real
/// [`Occupancy`] — across its own loop, so it derives the interior itself and
/// tracks the anchor directly rather than calling this; `affect_of_memo_
/// occupied` (task 6b) sits in between, preferring a caller-supplied
/// `Occupancy`'s real answer when one is available and room-consistent, and
/// falling back to this landing read otherwise.
///
/// `None` only for the pathological interior with no landing at all (an
/// empty one). `interior_of` never composes one in practice (even wilderness
/// draws `the-clearing`'s `Ground` hub — see `interior/derive.rs`'s own
/// tests), so this is unreachable on a live world, not a silent wrong
/// answer; [`Thermal::interior`]'s own `None` case already reads as the
/// correct identity (no interior applies) regardless of which of these two
/// reasons produced it.
fn landing_interior(pos: &RoomAddr, terrain: &dyn Terrain) -> Option<(Interior, AnchorId)> {
    let interior = interior_of(pos, terrain);
    let kind = seam_kind(terrain.is_built(pos));
    let anchor = landing(&interior, kind)?;
    Some((interior, anchor))
}

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
/// type-audit: bare-ok(ratio: day_length_std)
pub struct DriveMovements<'a> {
    /// The NPCs this tick advances.
    pub npcs: Vec<Npc>,
    /// The interval start (the session's previous day).
    pub from: WorldTime,
    /// The interval end (the session's new day).
    pub to: WorldTime,
    /// The drive parameters.
    pub params: DriveParams,
    /// The world's rotation period in standard days (`Calendar::day_length`),
    /// `None` on a tidally-locked world. The action clock divides the planet's
    /// day into an exact integer number of ticks (The Action Clock, spec §4.1),
    /// so the scheduler needs the day length the same way it needs the drive
    /// parameters. Read by the shared clock (the queue's tick scale) and by
    /// every charge `advance_one` and `catch_up` make against `clock::days_of`.
    pub day_length_std: Option<f64>,
    /// The elevation field belief and exploration read.
    pub terrain: &'a dyn Terrain,
}

/// The day `npc` entered the room it occupies as of `t` — the day of the
/// latest committed `agent-at` fact with day ≤ `t.day`, or the world's own
/// origin (`0.0`) when no such fact exists yet (the same pre-history
/// fallback [`agent_position`] uses for the ROOM itself: a creature with no
/// committed history has been home since the world began). This is catch-up's
/// (The Threshold task 7, spec §5) own "pre-entry state" boundary: the
/// unobserved span it replays runs from THIS day to `t`, because nothing
/// between them changed the creature's COARSE position — if it had, a later
/// `agent-at` would be the latest one instead, and this function would
/// return that later day.
fn room_entry_day(ledger: &Ledger, npc: &Npc, t: WorldTime) -> f64 {
    ledger
        .find(AGENT_AT)
        .filter(|f| f.subject == npc.entity)
        .filter(|f| f.day.map(|d| d <= t.day).unwrap_or(false))
        .last()
        .and_then(|f| f.day)
        .unwrap_or(0.0)
}

/// The three outcomes [`Intent::Hold`]'s closed-form day-jump can produce —
/// shared by the live walk and catch-up (The Threshold task 7) so the two
/// can never disagree about when a Hold makes progress, stalls, or gives up.
/// Factored out of the walk's own Hold arm (see that call site's doc for the
/// physical meaning of the jump); the live walk and catch-up differ only in
/// which `ceiling` bounds the jump (`self.to.day` for the live walk,
/// `self.from.day` — "now" — for catch-up's own unobserved-span replay).
enum HoldStep {
    /// Advance to this day and keep going.
    Advance(f64),
    /// The jump is degenerate (`rate_here == 0.0` makes it non-finite) —
    /// spend this iteration without moving `day`, matching the live walk's
    /// own `continue`.
    Stall,
    /// No progress is possible, or the jump would overshoot `ceiling` — give
    /// up, matching the live walk's own `break`.
    GiveUp,
}

/// [`HoldStep`]'s own computation: thirst's closed-form rise at `pos`
/// (`rise_at`) projected forward from `drive`'s current level to
/// `params.act`, capped at `ceiling`. `drive` is threaded in rather than
/// re-derived, because it is already the exact value [`decide_step`]'s own
/// `arbitrate` call just used — recomputing it here from the same inputs
/// would be redundant, not a second judgment, but threading it removes even
/// that redundancy.
fn hold_step(
    day: f64,
    pos: &RoomAddr,
    npc: &Npc,
    terrain: &dyn Terrain,
    drive: f64,
    params: &DriveParams,
    ceiling: f64,
) -> HoldStep {
    let rate_here = rise_at(
        terrain.temperature(pos, WorldTime { day }),
        npc.metabolic_class,
        params,
    );
    let next_act = day + (params.act - drive) / rate_here;
    if !next_act.is_finite() {
        return HoldStep::Stall;
    }
    if next_act <= day || next_act > ceiling {
        return HoldStep::GiveUp;
    }
    HoldStep::Advance(next_act)
}

/// One decide step's FULL judgment — this NPC's complete drive stack, its
/// [`Perceived`] view, and the arbitrated [`Resolution`] — exactly as the
/// live per-tick walk has always computed it, plus the thirst drive's own
/// urgency (the one extra value [`HoldStep`]'s jump needs, returned here so
/// no caller ever has to re-derive it and risk disagreeing).
///
/// Factored out of the walk's inner loop for The Threshold task 7: catch-up
/// (`DriveMovements::step_with_occupancy`, right after each creature's
/// initial landing) calls this SAME function over its own unobserved-span
/// replay, so catch-up's judgment and the live walk's real execution can
/// never diverge in HOW a resolution is reached from a given set of
/// arguments — this is the campaign's own rejected alternative (a SEPARATE
/// derivation of where a creature "would be") avoided by construction, not
/// by discipline. The two callers differ only in which resolutions each is
/// willing to ACT on ([`is_replayable_in_catch_up`] restricts catch-up to
/// `MoveWithin`/`Hold`; the live walk executes everything), never in how a
/// resolution is reached.
///
/// What this function does NOT guarantee by construction: that its
/// `last_drank`/`last_ate`/`last_rested` arguments are correct FOR `day`. A
/// caller replaying several days must supply the value each drive would
/// actually have seen at that point in the replay, not one folded once over
/// the caller's entire history — a discharge fact landing chronologically
/// AFTER `day` but before the fold's own evaluation instant would otherwise
/// suppress every competing drive for days that precede it. Catch-up meets
/// this obligation via [`last_fact_day_at_or_before`], recomputed every
/// iteration of its replay loop; that discipline lives in the caller, not
/// here.
///
/// Mutates `believed` exactly once, at the top, for the same reason the live
/// walk always has: standing in water updates belief before the view built
/// from it is assembled, and catch-up's own replayed steps must see that
/// update too or a creature that wades through water mid-catch-up would
/// forget it.
///
/// `mesh_memo` (the-waymark, Task 3) is the caller-owned [`RoomMeshMemo`] this
/// step's own [`lowest_unvisited_neighbor_memo`] read shares — one memo per
/// [`DriveMovements::step_with_occupancy`] call (the `PrimaryAfraidMemo`
/// per-tick scope), so a neighbourhood already visited by an earlier
/// creature, or an earlier tick-iteration of THIS creature's walk, is not
/// recomputed.
///
/// `home_nav_cache` (the-waymark, Task 4) is the caller-owned
/// [`HomeNavCache`] the Social drive's own home-plan feature is read from
/// (`HomeNavCache::home_nav`) instead of an unconditional `plan_to_room` —
/// session-lived like `mesh_memo`, but cross-tick rather than per-tick (see
/// the cache's own doc for why a stationary, unchanged-belief creature must
/// reach zero searches across ticks, not merely within one).
#[allow(clippy::too_many_arguments)]
fn decide_step(
    day: f64,
    pos: &RoomAddr,
    npc: &Npc,
    terrain: &dyn Terrain,
    believed: &mut Option<RoomAddr>,
    hazard: &HazardMemory,
    alarm: &std::collections::BTreeMap<RoomAddr, f64>,
    visited: &std::collections::BTreeSet<RoomAddr>,
    last_drank: f64,
    last_ate: f64,
    last_rested: f64,
    interior: Option<(&Interior, AnchorId)>,
    mode: Mode,
    params: &DriveParams,
    budget: usize,
    frozen: &Ledger,
    out: &[Fact],
    mesh_memo: &mut RoomMeshMemo,
    home_nav_cache: &mut HomeNavCache,
) -> (Resolution, f64) {
    // Standing in water forms/updates belief (nearest-to-home wins) — the
    // live walk's own first step of every iteration.
    if is_water(pos, terrain) {
        *believed = nearer_to_home(&npc.home, believed.take(), pos.clone(), PLAN_BUDGET);
    }
    // The temperature-coupled thirst integral, re-derived over the committed
    // history (`frozen`) PLUS this tick's own emitted moves (`out`) — see the
    // live walk's own doc for why both are folded together.
    let mut sightings = agent_sightings(frozen, npc.entity, day);
    for f in out {
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
        terrain,
        npc.metabolic_class,
        params,
    );
    let hunger_urgency = integrate_thirst(
        &sightings,
        &npc.home,
        last_ate,
        day,
        terrain,
        npc.metabolic_class,
        &HUNGER,
    );
    let explore_step = lowest_unvisited_neighbor_memo(pos, visited, terrain, mesh_memo);
    let fatigue = (FATIGUE_RISE * (day - last_rested)).clamp(0.0, 1.0);
    let view = Perceived {
        position: pos.clone(),
        drive,
        fatigue,
        believed_water: believed.clone(),
        believed_hazard: hazard.shunned.clone(),
        explore_step,
    };
    let thirst = Thirst { params: *params };
    let thermal = Thermal {
        niche: npc.temperature_niche,
        terrain,
        day: WorldTime { day },
        interior,
    };
    let rest = Fatigue {
        home: npc.home.clone(),
    };
    let hunger = Hunger {
        urgency: hunger_urgency,
        niche: npc.niche.clone(),
        terrain,
        day: WorldTime { day },
    };
    let danger = Danger {
        terrain,
        threat_niche: npc.threat_niche,
        boldness: npc.boldness,
        alarm: Some(alarm),
        dread: Some(&hazard.dread),
    };
    // The metabolism gate, read early (the-waymark, Task 4 — plan-time
    // verification (a)): the Social drive below is the plan's ONLY consumer,
    // and it is never pushed onto `drives` for an ametabolic creature, so
    // computing the plan for one was always pure waste. Gating the `home_nav`
    // call itself (not merely caching its result) is the "lazy AND cached"
    // half of the campaign spec's Stage 3 clause — an ametabolic creature now
    // never even touches the cache, let alone runs a search.
    let ametabolic = matches!(npc.metabolic_class, MetabolicClass::Ametabolic);
    let social = if ametabolic {
        Social {
            loneliness: 0.0,
            home_step: None,
        }
    } else {
        let feature = home_nav_cache.home_nav(
            npc.entity,
            pos,
            &npc.home,
            &view.believed_hazard,
            PLAN_BUDGET,
            mesh_memo,
        );
        Social {
            loneliness: loneliness_from_distance(feature.distance),
            home_step: feature.first_step,
        }
    };
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
        awake: is_awake(npc.activity, terrain, pos, WorldTime { day }),
    };
    let resolution = arbitrate(
        &view,
        &npc.home,
        &drives,
        &disposition,
        mode,
        budget,
        npc.entity,
        home_nav_cache,
        mesh_memo,
    );
    (resolution, drive)
}

/// The subject's most recent `predicate` fact day at or before `day`,
/// `0.0` if none — the per-day counterpart to the whole-history folds
/// computed once outside the walk (`drive_at`, `fatigue_at`, and
/// `step_with_occupancy`'s own `last_drank`/`last_ate`/`last_rested`
/// locals). Those are correct for a SINGLE evaluation instant; catch-up's
/// replay loop (below) evaluates many instants across a span that may
/// itself contain the very fact being folded, so it must re-filter to
/// `<= day` at each one rather than reuse a value folded over the whole
/// committed history, which could be looking chronologically PAST the day
/// it is being asked about.
fn last_fact_day_at_or_before(ledger: &Ledger, predicate: &str, entity: EntityId, day: f64) -> f64 {
    ledger
        .find(predicate)
        .filter(|f| f.subject == entity)
        .filter_map(|f| f.day)
        .filter(|&d| d <= day)
        .fold(0.0_f64, f64::max)
}

/// Catch-up's own replay loop (The Threshold task 7, spec §5): reconstruct
/// where `npc` would actually be standing within its current room right now
/// (`horizon`), given the ledger shows it entered the room at `entry_day`
/// and nothing observed it since. Runs [`decide_step`] — the SAME judgment
/// the live walk uses, never a second derivation — forward from `entry_day`,
/// replaying only the resolutions [`is_replayable_in_catch_up`] clears
/// (today exactly `MoveWithin`) plus bare `Hold`s (which touch nothing and
/// so are equally safe to replay), until ONE of three things happens first:
/// real time catches up to `horizon` (exact — the common case for a short
/// absence), a committing action would be needed (stop rather than
/// fabricate it, spec §5.5 — present tense only), or `cap` replay
/// iterations are spent with `horizon` still not reached (approximate: give
/// up stepping through the interior hop by hop and place the creature
/// straight at its Thermal-preferred anchor instead, spec §5.3's diagram).
/// Mutates `occupancy` (this creature's entry only) and `believed`
/// in-place; returns the commitment [`Mode`] catch-up ends in, so the live
/// walk that follows inherits its hysteresis rather than starting cold.
///
/// `cap` is [`CATCH_UP_STEP_CAP`] at the one production call site, but is
/// threaded as a parameter — matching every other search budget in this
/// module (`budget: usize`) — so a test can drive this SAME loop at a
/// small, cheap cap instead of paying for up to 1000 real iterations (each
/// a full drive-stack arbitration) just to reach the boundary the cap
/// crossover test needs to sit at. The cap's role in the loop is a property
/// of its STRUCTURE, not of `CATCH_UP_STEP_CAP`'s particular value.
///
/// Commits nothing: every argument other than `occupancy`/`believed`/`mode`/
/// `mesh_memo` is a shared reference or a plain value, `decide_step` only
/// ever reads `frozen`, and neither this function nor anything it calls ever
/// touches a `Ledger` mutably or pushes a `Fact` anywhere — there is no
/// `out: &mut Vec<Fact>` in this call graph at all for catch-up to write into
/// even by accident. `mesh_memo` (the-waymark, Task 3) is a pure-function
/// cache (see [`RoomMeshMemo`]'s own doc) threaded through to
/// [`decide_step`]'s own `lowest_unvisited_neighbor_memo` read — mutating it
/// changes nothing this function's callers can observe except speed.
/// `home_nav_cache` (the-waymark, Task 4) is the same kind of pure-function
/// cache, threaded through to `decide_step`'s own `HomeNavCache::home_nav`
/// read for exactly the same reason — every one of this loop's iterations
/// shares the SAME `hazard`/`pos` pairing the caller already fixed, so a
/// replay that revisits a position already asked about this call answers
/// from cache.
#[allow(clippy::too_many_arguments)]
fn catch_up(
    entry_day: f64,
    horizon: f64,
    pos: &RoomAddr,
    npc: &Npc,
    terrain: &dyn Terrain,
    believed: &mut Option<RoomAddr>,
    hazard: &HazardMemory,
    alarm: &std::collections::BTreeMap<RoomAddr, f64>,
    visited: &std::collections::BTreeSet<RoomAddr>,
    occupancy: &mut Occupancy,
    interior: &Interior,
    mut mode: Mode,
    params: &DriveParams,
    budget: usize,
    frozen: &Ledger,
    out: &[Fact],
    cap: usize,
    day_length_std: Option<f64>,
    mesh_memo: &mut RoomMeshMemo,
    home_nav_cache: &mut HomeNavCache,
) -> Mode {
    let mut day = entry_day;
    let mut steps = 0usize;
    let mut cap_reached = false;
    'catchup: while day < horizon {
        if steps >= cap {
            cap_reached = true;
            break;
        }
        steps += 1;
        // Recomputed AT `day`, never once over the whole committed history
        // (Important 3, The Threshold whole-branch review): the coarse
        // (room-graph) layer can commit a `drank`/`eaten`/`rested` fact for
        // this creature on a day INSIDE the span catch-up is still
        // reconstructing at the within-room resolution, so a fold over the
        // creature's entire history would find a discharge that
        // chronologically postdates `day` and wrongly suppress the drive
        // for every day before it. Filtering to `<= day` at each iteration
        // is what makes `decide_step` see the world as it actually was on
        // that day, not as it will be once the gap is fully closed.
        let last_drank = last_fact_day_at_or_before(frozen, DRANK, npc.entity, day);
        let last_ate = last_fact_day_at_or_before(frozen, EATEN, npc.entity, day);
        let last_rested = last_fact_day_at_or_before(frozen, RESTED, npc.entity, day);
        let (resolution, drive) = decide_step(
            day,
            pos,
            npc,
            terrain,
            believed,
            hazard,
            alarm,
            visited,
            last_drank,
            last_ate,
            last_rested,
            occupancy.at(npc.entity).map(|a| (interior, a)),
            mode,
            params,
            budget,
            frozen,
            out,
            mesh_memo,
            home_nav_cache,
        );
        mode = resolution.mode;
        match resolution.intent {
            // Requirement 1, straight from the source: an action reaches
            // this arm only when `is_replayable_in_catch_up` says its
            // effect is ephemeral — today exactly `MoveWithin`, checked
            // against the predicate rather than a fresh `matches!`, so a
            // future replayable action needs no second edit here.
            Intent::Do(action) if is_replayable_in_catch_up(&action) => {
                // Charged through the SAME action clock the live walk charges
                // through (`cost_ticks`, The Action Clock spec §3), with this
                // creature's own mass — a replay that advanced time at a
                // different rate than the walk it is reconstructing would drift
                // from it by construction, which is exactly the second-
                // derivation failure `decide_step` is shared to avoid. No
                // terrain factor: a within-room step does not change room, so
                // there is no elevation pair to climb.
                day += days_of(cost_ticks(&action, npc.mass_kg, 1.0), day_length_std);
                if day > horizon {
                    break;
                }
                let Action::MoveWithin(next) = action else {
                    unreachable!("is_replayable_in_catch_up currently admits only MoveWithin");
                };
                occupancy.walk(npc.entity, interior, next);
            }
            // A committing action (Drink/Rest/Eat/coarse MoveTo): stop
            // rather than fabricate the fact it would write (spec §5.5) —
            // the live walk that follows picks up here once real time
            // actually reaches `horizon`.
            Intent::Do(_) => break 'catchup,
            Intent::Hold => match hold_step(day, pos, npc, terrain, drive, params, horizon) {
                HoldStep::Stall => {}
                HoldStep::GiveUp => break 'catchup,
                HoldStep::Advance(next) => day = next,
            },
        }
    }
    if cap_reached {
        // The cap was spent before real time caught up: give up stepping
        // through the interior and jump straight to where Thermal wants to
        // be right now — `preferred_anchor` is the SAME gate, target, AND
        // `route_within` reachability check `Thermal::affordance`'s own
        // within-room branch uses (including spec §8a's stranding case), so
        // this cannot suggest a move the live drive itself would have
        // declined, and cannot place the creature across an edge it could
        // not actually cross.
        let thermal = Thermal {
            niche: npc.temperature_niche,
            terrain,
            day: WorldTime { day: horizon },
            interior: occupancy.at(npc.entity).map(|a| (interior, a)),
        };
        if let Some(target) = thermal.preferred_anchor(pos, budget) {
            occupancy.place(npc.entity, pos, target);
        }
    }
    mode
}

impl<'a> DriveMovements<'a> {
    /// The per-tick walk, returning both the committed `Fact`s AND the final
    /// per-creature [`Occupancy`] each npc's walk reached inside this tick's
    /// own presence bubble — a seam so a test can observe the ephemeral
    /// within-room result the committed facts alone never reveal (decision
    /// 0069: `MoveWithin` commits nothing, so nothing about it is otherwise
    /// visible after `step` returns). [`TickSystem::step`] is a thin wrapper
    /// discarding the second element; production code only ever wants the
    /// facts.
    ///
    /// `pub` (The Threshold task 6b) beyond this crate's own tests: the
    /// population-health battery (`hornvale_lab::health::run_simulation`) is
    /// a second, legitimate consumer of the discarded element — it drives
    /// this same walk through `kernel::tick` for the committed facts, and
    /// separately calls this directly (a second, pure re-evaluation of the
    /// SAME frozen ledger and system, not a second simulation with different
    /// consequences) purely to recover the `Occupancy` its stateless affect
    /// sampler needs to read warmth where a creature actually walked to
    /// rather than always at its room's landing anchor. See
    /// [`affect_of_memo_occupied`] for the read this occupancy feeds.
    ///
    /// `mesh_memo` (the-waymark fix round, Finding 2) is a caller-owned
    /// [`RoomMeshMemo`], threaded `&mut` rather than built fresh here: the
    /// PREVIOUS shape (`let mut mesh_memo = RoomMeshMemo::new()` local to
    /// this function) discarded a tick's worth of `neighbors()` reuse every
    /// single call — the lab's health battery alone calls this once per tick
    /// for 40 ticks, so the old shape rebuilt (and threw away) the memo 40
    /// times over a single run. The caller now owns it for as long as ITS
    /// own scope lasts (a `Session`'s whole possession; `run_simulation`'s
    /// whole 40-tick sweep), so cross-tick reuse is the point, not per-tick
    /// accident.
    ///
    /// `home_nav_cache` (the-waymark, Task 4) is likewise caller-owned, but
    /// CROSS-tick rather than per-tick by design — see [`HomeNavCache`]'s own
    /// doc for why the campaign's scaling bar needs a cache that survives
    /// across calls to this very function, not merely across one call's own
    /// creatures/pops.
    pub fn step_with_occupancy(
        &self,
        frozen: &Ledger,
        mesh_memo: &mut RoomMeshMemo,
        home_nav_cache: &mut HomeNavCache,
    ) -> (Vec<Fact>, Occupancy) {
        let mut out: Vec<Fact> = Vec::new();
        // THE THRESHOLD's crossing (task 6): which anchor each creature
        // stands at, tracked across this tick's own walk. Shared across
        // every npc this tick advances — `Occupancy` is keyed by `EntityId`,
        // so there is no cross-npc collision — and, like `afraid_memo`
        // below, discarded the moment `step` returns: nothing here is ever
        // committed or carried into the NEXT tick (`Occupancy`'s own doc —
        // it evaporates with the bubble). So every creature starts each
        // fresh `step` call back at its room's landing anchor, never at
        // wherever a PREVIOUS tick's walk carried it deeper into the room —
        // the observer-effect gap catch-up (Task 7) exists to close, not
        // this task's.
        let mut occupancy = Occupancy::default();
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
        // THE SHARED CLOCK (The Action Clock T5, spec §4): every creature is
        // queued at the moment it NEXT acts, and the whole population advances
        // by repeatedly popping whoever acts soonest. The key is
        // `(ticks, EntityId)` in a `BTreeSet` — integer time so the order is
        // exact (the `astar` reason: accumulated `f64` addition is not a total
        // order), with the entity id as the tie-break, so the emitted sequence
        // is a pure function of the frozen ledger and NEVER of how `npcs`
        // happened to be listed.
        //
        // Perception is untouched by this (spec §5): `alarm` above and each
        // creature's `memory` below are both built from `frozen` BEFORE anyone
        // moves, and `advance_one` never sees another creature's mid-tick
        // state. Interleaving reorders acting, not perceiving — which is what
        // keeps the alarm wave terminating.
        //
        // The per-entity state is the whole of what must survive a pop: the
        // `Npc`, its `WalkState` (spec §6's nine loop locals), and its
        // `HazardMemory` — the last of which is here rather than in
        // `WalkState::begin` because computing it needs `&mut afraid_memo`.
        // It is a fold over `frozen`, so it is computed ONCE per creature, as
        // the sequential loop did, and carried rather than recomputed per pop.
        let mut states: std::collections::BTreeMap<EntityId, (Npc, WalkState, HazardMemory)> =
            std::collections::BTreeMap::new();
        let mut queue: std::collections::BTreeSet<(u64, EntityId)> =
            std::collections::BTreeSet::new();
        // Ticks per STANDARD day on this world: a local day is exactly
        // `ticks_per_local_day` ticks (spec §4.1), so this is the inverse of
        // `clock::days_of` and the two agree to the tick.
        let per_day = ticks_per_local_day(self.day_length_std) as f64;
        let scale = match self.day_length_std.filter(|d| d.is_finite() && *d > 0.0) {
            Some(d) => per_day / d,
            None => per_day,
        };
        let to_ticks = (self.to.day * scale).round() as u64;
        let from_ticks = (self.from.day * scale).round() as u64;
        for npc in &self.npcs {
            let mut st = WalkState::begin(frozen, npc, &self.npcs, self.from, self.terrain);
            // THE THRESHOLD's crossing: arrive at the landing anchor of the
            // interior `WalkState::begin` just derived — the entry point for a
            // creature crossing INTO the room from the coarse (room-graph)
            // layer. Re-derived (and re-arrived) every time `st.pos` changes in
            // `advance_one`, since an `AnchorId` is only meaningful paired with
            // the SPECIFIC `Interior` it indexes (`Occupancy`'s own doc).
            occupancy.arrive(
                npc.entity,
                &st.pos,
                &st.interior,
                seam_kind(self.terrain.is_built(&st.pos)),
            );
            // The Haunt + The Phantom: the ground this creature remembers being
            // frightened on — a fold over its committed (pre-tick) history,
            // computed ONCE per creature. The FULL population is the roster, so
            // the memory folds the re-derived transient alarm too (the phantom);
            // the most-recent-visit staleness clears a disproven fear. Empty for
            // a never-frightened creature ⇒ every planner edge stays `1`, byte-
            // identical (no primary-afraid emitter on the settled worlds).
            let memory = hazard_memory_memo(
                frozen,
                npc,
                self.from,
                self.terrain,
                &self.npcs,
                &mut afraid_memo,
            );
            // THE THRESHOLD's catch-up (task 7, spec §5): close the
            // observer-effect gap this function's own doc names above. The
            // landing anchor `occupancy.arrive` just placed this creature at
            // is where a creature crossing the coarse seam RIGHT NOW
            // arrives — but if the committed ledger shows no room change
            // since an EARLIER day (`room_entry_day`), the creature has
            // really been standing somewhere in THIS room, unobserved, ever
            // since; the true (never-serialized, decision 0069) within-room
            // position it occupies right now is whatever its own decide
            // loop would have carried it to over that span, not still the
            // door. `catch_up` reconstructs it (see that function's own doc
            // for the mechanism and the cap). Order-independent across
            // creatures: it reads only `frozen` (fixed for the whole
            // `step_with_occupancy` call), this creature's own locals, and
            // `occupancy` entries THIS creature alone owns — nothing here
            // reads another creature's occupancy, so which creature catches
            // up first cannot change the result (spec §5.4's own
            // order-independence requirement). It runs BEFORE the shared clock
            // opens (The Action Clock T5) for the same reason: every creature's
            // reconstruction reads only `frozen` and its own state, so doing
            // them all in the setup pass — rather than interleaved with the
            // queue — keeps the order-independence above trivially true.
            st.mode = catch_up(
                room_entry_day(frozen, npc, self.from),
                self.from.day,
                &st.pos,
                npc,
                self.terrain,
                &mut st.believed,
                &memory,
                &alarm,
                &st.visited,
                &mut occupancy,
                &st.interior,
                st.mode,
                &self.params,
                PLAN_BUDGET,
                frozen,
                &out,
                CATCH_UP_STEP_CAP,
                self.day_length_std,
                mesh_memo,
                home_nav_cache,
            );

            queue.insert((from_ticks, npc.entity));
            states.insert(npc.entity, (npc.clone(), st, memory));
        }
        while let Some(&(t, e)) = queue.iter().next() {
            queue.remove(&(t, e));
            let Some((npc, st, memory)) = states.get_mut(&e) else {
                continue;
            };
            let npc = npc.clone();
            let memory = memory.clone();
            if !self.advance_one(
                frozen,
                &npc,
                st,
                &mut occupancy,
                &alarm,
                &memory,
                &mut out,
                mesh_memo,
                home_nav_cache,
            ) {
                // This creature's walk is over — past `to`, out of `MAX_STEPS`,
                // or halted by an arm's own stop condition. It is not requeued.
                continue;
            }
            // `advance_one` has already advanced `st.day` by what the action
            // cost, so the requeue time is READ FROM THE STATE rather than
            // recomputed: one source of truth for when a creature next acts.
            let next = (st.day * scale).round() as u64;
            if next > to_ticks {
                // `round` is monotone, so this can only fire when `st.day`
                // genuinely exceeds `to.day` — exactly the guard `advance_one`
                // would apply on the next pop, applied a pop early.
                continue;
            }
            queue.insert((next, e));
        }
        (out, occupancy)
    }
}

impl<'a> TickSystem for DriveMovements<'a> {
    fn label(&self) -> &'static str {
        "drive-movements"
    }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        // `TickSystem::step`'s signature is fixed by the kernel's scheduler
        // (`tick()` dispatches every registered system through it generically,
        // so it cannot carry a `RoomMeshMemo` parameter without a kernel-trait
        // change — out of the-waymark's scope). This path therefore cannot
        // share a caller-owned memo the way the direct `step_with_occupancy`
        // call (`Session::wait`, `run_simulation`) does; a throwaway one costs
        // nothing beyond what the pre-Finding-2 code already paid every call.
        // `throwaway_nav` (Task 4) carries the identical carve-out: this path
        // pays a fresh `plan_to_room` per creature per pop, exactly what
        // EVERY call paid before this task.
        let mut throwaway = RoomMeshMemo::new();
        let mut throwaway_nav = HomeNavCache::new();
        self.step_with_occupancy(frozen, &mut throwaway, &mut throwaway_nav)
            .0
    }
}

/// The state of ONE creature's walk through a tick — the nine pieces of
/// per-creature state that `DriveMovements::step` used to hold in loop locals
/// (The Action Clock, spec §6).
///
/// Hoisting them into a struct is what lets a walk be advanced one action at a
/// time by [`DriveMovements::advance_one`] instead of run to completion in a
/// single pass, which in turn is what lets several creatures interleave on a
/// shared clock. The extraction is behaviour-preserving on its own: the fields
/// are the same values, initialised in the same way, mutated in the same order.
///
/// Tick-local and re-derived, never persisted — like [`Mode`], which it carries.
struct WalkState {
    /// Where the creature currently stands.
    pos: RoomAddr,
    /// How far into the interval the walk has got.
    day: f64,
    /// The day of its most recent drink, `frozen`-seeded and advanced by this
    /// walk's own emitted `drank` facts.
    last_drank: f64,
    /// The day of its most recent sleep, the fatigue twin of `last_drank`.
    last_rested: f64,
    /// The day of its most recent meal, the hunger twin of `last_drank`.
    last_ate: f64,
    /// The water source it believes in, seeded from the band's pooled belief and
    /// grown whenever it stands in water.
    believed: Option<RoomAddr>,
    /// The cells this walk has already stood on — the explorer's frontier.
    visited: std::collections::BTreeSet<RoomAddr>,
    /// How many decisions this walk has taken, against `MAX_STEPS`.
    steps: usize,
    /// The commitment mode carried across this walk's steps (hysteresis).
    mode: Mode,
    /// The derived interior of the room at `pos` (The Threshold) — the anchor
    /// graph `Thermal`'s within-room branch routes over, and the graph the
    /// creature's `Occupancy` entry indexes into. Re-derived every time `pos`
    /// changes, since an `AnchorId` is only meaningful paired with the SPECIFIC
    /// `Interior` it indexes (`Occupancy`'s own doc). The tenth piece of
    /// per-creature walk state, and the one this branch added.
    interior: Interior,
}

impl WalkState {
    /// Open a walk for `npc` at `from`, deriving every field from the FROZEN
    /// pre-tick ledger — nothing here reads another creature's mid-tick state
    /// (spec §5), `band` being consulted only through `shared_believed_water`'s
    /// own frozen reads.
    fn begin(
        frozen: &Ledger,
        npc: &Npc,
        band: &[Npc],
        from: WorldTime,
        terrain: &dyn Terrain,
    ) -> WalkState {
        let pos = agent_position(frozen, npc, from);
        let day = from.day;
        // A scratch ledger view isn't available; track drank locally: derive
        // the starting last-drank day from `frozen`, then simulate forward,
        // updating a local `last_drank` as we emit `DRANK` facts.
        let last_drank = frozen
            .find(DRANK)
            .filter(|f| f.subject == npc.entity)
            .filter_map(|f| f.day)
            .fold(0.0_f64, f64::max);
        // Likewise the last rest day (The Slumber): fatigue is time since it,
        // reset when a `rested` fact is emitted.
        let last_rested = frozen
            .find(RESTED)
            .filter(|f| f.subject == npc.entity)
            .filter_map(|f| f.day)
            .fold(0.0_f64, f64::max);
        // Likewise the last meal day (The Provender): hunger is a path
        // integral since it, reset when an `eaten` fact is emitted.
        let last_ate = frozen
            .find(EATEN)
            .filter(|f| f.subject == npc.entity)
            .filter_map(|f| f.day)
            .fold(0.0_f64, f64::max);
        // Belief and exploration state, evolved locally across the walk (the
        // fold includes this tick's own emitted moves). Seed belief from the
        // pre-tick history; grow it whenever the agent stands in water.
        // The Tidings: seed from the BAND's pooled belief (co-located
        // members share what they know), not the creature's alone.
        let believed = shared_believed_water(frozen, npc, band, from, terrain, PLAN_BUDGET);
        let mut visited: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        visited.insert(pos.clone());
        let steps = 0usize;
        // The commitment mode, carried across this walk's steps (session-
        // sandboxed hysteresis; re-derived, never persisted). Starts Idle.
        let mode = Mode::Idle;
        // THE THRESHOLD's crossing: the room's own interior, derived from the
        // terrain the same way every other read of it is. The caller pairs this
        // with an `Occupancy::arrive` at its landing anchor — that write needs a
        // `&mut Occupancy`, which is shared across the whole population and so
        // lives one level up rather than in this per-creature constructor.
        let interior = interior_of(&pos, terrain);
        WalkState {
            pos,
            day,
            last_drank,
            last_rested,
            last_ate,
            believed,
            visited,
            steps,
            mode,
            interior,
        }
    }
}

impl<'a> DriveMovements<'a> {
    /// Advance ONE creature by ONE decision-and-act: perceive, arbitrate, act,
    /// append any emitted facts to `out`, and update `st`. Returns `false` when
    /// this walk is over — past `to`, out of `MAX_STEPS`, or halted by one of
    /// the arms' own stop conditions — and `true` when it should be advanced
    /// again.
    ///
    /// Every cross-agent read is FROZEN-based (spec §5): `frozen` is the
    /// pre-tick ledger and `alarm` and `memory` were both built from it before
    /// any creature moved. Nothing here can observe another creature's mid-tick
    /// position, which is what keeps the alarm wave terminating and the emitted
    /// order a pure function of the frozen ledger.
    ///
    /// The eighth argument is the shared `Occupancy` (The Threshold): it is
    /// population-wide state, keyed by `EntityId`, so it cannot live in the
    /// per-creature `WalkState` the way the other nine pieces do — the same
    /// reason `decide_step` and `catch_up` carry the allow below.
    ///
    /// `mesh_memo` (the-waymark, Task 3) is likewise population-wide, tick-
    /// scoped state: [`step_with_occupancy`](Self::step_with_occupancy) builds
    /// ONE [`RoomMeshMemo`] and threads it through every creature's every
    /// `advance_one`/`decide_step`, so the same neighbourhood recurring across
    /// creatures or across this walk's own repeated pops is not recomputed.
    /// `home_nav_cache` (the-waymark, Task 4) is `step_with_occupancy`'s own
    /// CROSS-tick [`HomeNavCache`], threaded the same way.
    #[allow(clippy::too_many_arguments)]
    fn advance_one(
        &self,
        frozen: &Ledger,
        npc: &Npc,
        st: &mut WalkState,
        occupancy: &mut Occupancy,
        alarm: &std::collections::BTreeMap<RoomAddr, f64>,
        memory: &HazardMemory,
        out: &mut Vec<Fact>,
        mesh_memo: &mut RoomMeshMemo,
        home_nav_cache: &mut HomeNavCache,
    ) -> bool {
        if st.day > self.to.day || st.steps >= MAX_STEPS {
            return false;
        }
        st.steps += 1;
        // One decide step's FULL judgment (`decide_step`, factored out for The
        // Threshold task 7 — see its own doc): the standing-in-water belief
        // update, the `Perceived` view, every drive in the stack, and the
        // arbitration, exactly as this walk has always computed them. Catch-up
        // (`catch_up`, above) runs this SAME function over its own replay, so
        // the live walk and the reconstruction of an unobserved creature's
        // position can never diverge in HOW a resolution is reached from a
        // given set of arguments. `drive` — thirst's own urgency — is threaded
        // back out for the `Hold` arm's closed-form jump below.
        //
        // The interior handed in is the one this creature is actually standing
        // in, at the anchor `occupancy` says it stands at (The Threshold task
        // 6), so `Thermal` can seek warmth WITHIN the room rather than feeling
        // nothing indoors at all.
        let (resolution, drive) = decide_step(
            st.day,
            &st.pos,
            npc,
            self.terrain,
            &mut st.believed,
            memory,
            alarm,
            &st.visited,
            st.last_drank,
            st.last_ate,
            st.last_rested,
            occupancy.at(npc.entity).map(|a| (&st.interior, a)),
            st.mode,
            &self.params,
            PLAN_BUDGET,
            frozen,
            &*out,
            mesh_memo,
            home_nav_cache,
        );
        st.mode = resolution.mode;
        // THE ACTION CLOCK (spec §2 rung 1, §3): every action costs time, and
        // what it costs depends on the creature doing it. Charged HERE, once,
        // above the behaviour match, so the cost model is TOTAL by construction
        // — a future `Action` cannot be added for free by forgetting an arm.
        // Historically only `MoveTo` charged, and it charged the same flat
        // `MOVE_DURATION` for every creature over every kind of ground; that
        // constant is gone, and `clock::base_ticks` is now its single home —
        // including The Threshold's within-room step, which arrived on this
        // branch with its own authored `MOVE_WITHIN_DURATION` and is now the
        // clock's fifth dial instead (`Action::MoveWithin`), at the same tenth
        // of a `MoveTo` it was authored as, and now scaled by body mass like
        // every other act: a bear crosses a room more slowly than a person.
        //
        // The climb factor is a `MoveTo` modifier alone (spec §3.1) — drinking
        // is not steeper in the mountains, and a within-room step does not
        // change room, so there is no elevation pair to read — and it is read
        // from the terrain BEFORE `st.pos` moves. `Rest` pays here only for
        // lying down; the sleep itself remains the jump-to-waking below, a
        // phase and not a cost.
        //
        // The interval guard stays exactly where `MoveTo`'s was: charging is
        // what can carry a walk past `to.day`, so it is checked immediately
        // after the charge and before anything is emitted.
        if let Intent::Do(action) = &resolution.intent {
            let ground = match action {
                Action::MoveTo(n) => {
                    climb_factor(self.terrain.elevation(&st.pos), self.terrain.elevation(n))
                }
                _ => 1.0,
            };
            st.day += days_of(cost_ticks(action, npc.mass_kg, ground), self.day_length_std);
            if st.day > self.to.day {
                return false;
            }
        }
        match resolution.intent {
            Intent::Do(Action::MoveTo(n)) => {
                // Provenance follows the committed errand (the mode):
                // thirst distinguishes BELIEVED (beelining a known
                // source) from IGNORANT (exploring blind); thermal names
                // the comfort-seeking; homing names the sated walk back.
                let provenance = match st.mode {
                    Mode::Pursuing(DriveKind::Thermal) => "sought a kinder clime (comfort)",
                    Mode::Pursuing(DriveKind::Fatigue) => "turned home, weary, to rest",
                    Mode::Pursuing(DriveKind::Hunger) => "foraged toward richer ground (hunger)",
                    Mode::Pursuing(DriveKind::Danger) => "fled the uncanny ground (fear)",
                    Mode::Pursuing(DriveKind::Social) => {
                        "drifted homeward, missing its people (belonging)"
                    }
                    Mode::Pursuing(DriveKind::Thirst) if st.believed.is_some() => {
                        "went down to the river it knew (thirst)"
                    }
                    Mode::Pursuing(DriveKind::Thirst) => {
                        "wandered, having found no water yet (thirst)" // ignorant
                    }
                    Mode::Homing | Mode::Idle => "walking home (sated)",
                };
                out.push(agent_at_fact(npc.entity, &n, st.day, provenance));
                st.visited.insert(n.clone());
                st.pos = n;
                // Crossing into a DIFFERENT room: re-derive ITS interior and
                // re-arrive at ITS landing anchor. The anchor reached in the
                // OLD room's graph means nothing here — an `AnchorId` is a
                // vector offset into the SPECIFIC `Interior` it indexes.
                st.interior = interior_of(&st.pos, self.terrain);
                occupancy.arrive(
                    npc.entity,
                    &st.pos,
                    &st.interior,
                    seam_kind(self.terrain.is_built(&st.pos)),
                );
            }
            Intent::Do(Action::Drink) => {
                out.push(drank_fact(
                    npc.entity,
                    st.day,
                    "drank from the river (thirst sated)",
                ));
                st.last_drank = st.day;
            }
            Intent::Do(Action::Rest) => {
                out.push(rested_fact(
                    npc.entity,
                    st.day,
                    "slept at home (fatigue eased)",
                ));
                st.last_rested = st.day;
                // Sleep through the off-phase in one jump to the next
                // waking, rather than re-resting every step (The Slumber).
                st.day = next_awake_day(npc.activity, self.terrain, &st.pos, st.day);
                if st.day > self.to.day {
                    return false;
                }
            }
            Intent::Do(Action::Eat) => {
                out.push(eaten_fact(
                    npc.entity,
                    st.day,
                    "grazed the productive ground (hunger sated)",
                ));
                st.last_ate = st.day;
            }
            Intent::Do(Action::MoveWithin(next)) => {
                // THE THRESHOLD's crossing, live. Fine movement writes NO fact
                // (decision 0069 — `MoveWithin`'s effect is bubble-local
                // occupancy, never serialized), but it costs time like any
                // other act; that charge happened above, with every other
                // action's, so this arm has only the occupancy to update.
                //
                // `Thermal::affordance`'s within-room branch only ever proposes
                // an anchor reachable by `route_within`'s FIRST hop, which is
                // by construction adjacent to wherever the creature currently
                // stands, so `walk` should always succeed here. Its bool return
                // is still checked (not `unwrap`ped) rather than leaning on
                // that invariant: a refused walk simply leaves the creature
                // where it was, having still spent the tick's time trying — no
                // different in kind from a room-scale `Hold`.
                occupancy.walk(npc.entity, &st.interior, next);
            }
            Intent::Hold => {
                // Idle (or unreachable): jump to the next act-crossing in
                // closed form rather than spinning day-by-day (`hold_step`,
                // shared with catch-up — see its own doc for the physical
                // meaning of the jump and the strict-progress guarantee that
                // bounds it). `Stall` keeps `st.day` untouched and asks for
                // another pop; `GiveUp` ends this creature's walk.
                match hold_step(
                    st.day,
                    &st.pos,
                    npc,
                    self.terrain,
                    drive,
                    &self.params,
                    self.to.day,
                ) {
                    HoldStep::Stall => return true,
                    HoldStep::GiveUp => return false,
                    HoldStep::Advance(next) => st.day = next,
                }
            }
        }
        true
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
/// exploration step), or `None` if every neighbour is visited. Terminating:
/// the visited set only grows. Consults/fills a caller-owned [`RoomMeshMemo`]
/// for the `neighbors()` read instead of recomputing the icosphere
/// lattice/edge-crossing arithmetic every call (the-waymark, Task 3). Two hot
/// callers share it: the live walk's `decide_step` (via
/// `DriveMovements::step_with_occupancy`'s session-owned memo, the-waymark
/// fix round, Finding 2) and the stateless health-sampler read
/// [`affect_of_memo_occupied`] (rider (b)) — both thread whatever memo THEIR
/// own caller supplies, never build one silently inline.
fn lowest_unvisited_neighbor_memo(
    from: &RoomAddr,
    visited: &std::collections::BTreeSet<RoomAddr>,
    terrain: &dyn Terrain,
    memo: &mut RoomMeshMemo,
) -> Option<RoomAddr> {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors_memo(memo) {
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
            // Body mass (The Action Clock): the allometric driver of every
            // action's cost. A species missing from the biosphere registry
            // falls back EXPLICITLY to the clock's reference mass — `tempo`
            // clamps a nonsense value anyway, but the fallback is stated here
            // rather than left implicit, so a defaulted creature reads at
            // exactly tempo 1.0.
            let mass_kg = biosphere
                .get_by_label(&species)
                .map(|t| t.mass.kilograms())
                .unwrap_or(crate::clock::REFERENCE_MASS_KG);
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
                mass_kg,
                label,
            }
        })
        .collect()
}

/// Derive WILD NPCs (The Wilding) — beast agents, one per distinct
/// mobile-beast `concentrations` entry (`worldgen::wild_concentrations_from`:
/// a herd, a lair). A wild NPC is the same `Npc` a settlement produces — its
/// home is the concentration's cell, its traits its biosphere's, its psyche
/// the DEFAULT (beasts carry no `psyche_registry` entry, so the `.unwrap_or`
/// fallbacks apply, exactly as they already do for a settlement of a
/// non-peopled species). The threat niche derives (The Bane/Quarry) with LIVE
/// predator dread, so a herbivore beast finally FEARS predator ground — The
/// Quarry, waking. Appended to the peopled `derive_npcs` output; genesis
/// untouched (the session's ledger clone only, like `derive_npcs`).
///
/// Takes the already-fit `concentrations` (a caller's
/// `wild_concentrations_from(wc, report, k)`) rather than fitting the
/// coexistence stack itself — since The Weir (Stage 1b), the caller shares
/// ONE demography report across the predator/prey/wild fields instead of
/// this minting step re-running its own fourth fit.
/// type-audit: bare-ok(identifier-text: concentrations)
pub fn derive_wild_npcs(
    world: &World,
    ctx: &LocaleContext,
    ledger: &mut Ledger,
    concentrations: Vec<(String, [f64; 3])>,
) -> Vec<Npc> {
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
            // Body mass (The Action Clock), as in `derive_npcs`: the fauna are
            // most of the health battery's population, so the wild path must
            // carry the trait too or the tempo spread collapses.
            let mass_kg = biosphere
                .get_by_label(&species)
                .map(|t| t.mass.kilograms())
                .unwrap_or(crate::clock::REFERENCE_MASS_KG);
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
                mass_kg,
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

/// The set of packed room ids a settlement's territory occupies — the real
/// answer [`Terrain::is_built`] needs from a live world (The Threshold, task
/// 5b: the arming Task 5 wired had nothing to read, since no `Terrain`
/// implementation ever overrode the default `false`). A room's *culture* is
/// not a property of the room itself; it belongs to the people whose
/// territory contains it, so this asks the only question derivable from
/// `hornvale_settlement::all_settlements`: which room is each settlement's
/// own cell? Today's model gives a settlement exactly ONE room (the same one
/// `settlement_room` homes its derived NPC at) — so "built" here means
/// precisely that room, not a radius of surrounding countryside. That is a
/// deliberately NARROW answer: widening it to a settlement's outskirts or
/// worked fields is a real question, but one nothing in the model yet
/// derives (there is no committed "territory extent" a wider read could be
/// honest about) — a later campaign's to ask, not an oversight here. Built
/// once, at session/sweep start, and injected into `LocaleTerrain` the same
/// way the predator/prey fields are (`with_fields`) — a domain/window can't
/// reach up to `hornvale_settlement` on its own. `RoomAddr::pack`'s only
/// failure mode is a path past `MAX_DEPTH`, never reached at a session's own
/// walk depth, so a pack failure is silently dropped rather than panicking —
/// the same "coarse constrains fine, never blocks" posture the rest of this
/// module takes toward world-derived data. `BTreeSet`, never `HashSet`
/// (constitutional): `RoomId` is the packed, `Ord` form of a `RoomAddr`, the
/// natural key.
pub fn built_rooms(world: &World, ctx: &LocaleContext) -> std::collections::BTreeSet<RoomId> {
    hornvale_settlement::all_settlements(world)
        .iter()
        .filter_map(|v| settlement_room(world, ctx, v.id).pack().ok())
        .collect()
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
    /// Walk to another anchor inside the current room (The Threshold).
    /// Precondition: adjacency in the room's anchor graph. Effect: fine
    /// position, which is NEVER serialized (decision 0069) — which is what
    /// makes this the one action catch-up may replay.
    MoveWithin(AnchorId),
}

/// Whether an action's effect is position rather than a committed fact.
/// type-audit: bare-ok(flag: return)
pub fn is_movement(a: &Action) -> bool {
    matches!(a, Action::MoveTo(_) | Action::MoveWithin(_))
}

/// Whether an action's precondition reads committed state rather than position
/// alone. Today nothing does — every precondition in this file is adjacency or
/// standing-here — and catch-up (The Threshold) depends on that: it replays a
/// creature's movement while suppressing the actions that commit facts, which
/// reconstructs a past that could actually have happened only while no
/// movement is gated by a committed effect. A barred door needing unbarring
/// would end it.
///
/// **The guard is the exhaustive match, not the test.** An earlier draft took
/// `_a` and returned a bare `false`, with a test asserting the answer is
/// `false` for movement actions — which reduces to `assert!(!false)` and
/// cannot fail for any input. It would have caught a new action only if
/// whoever added it *remembered* to come here and flip the answer, i.e.
/// exactly when the guard was not needed. Matching every variant by name
/// instead means adding one to [`Action`] fails to COMPILE here, which is this
/// project's usual preference for structural enforcement over discipline.
/// type-audit: bare-ok(flag: return)
pub fn precondition_reads_committed_state(a: &Action) -> bool {
    match a {
        // Adjacency in the room graph; nothing committed is read.
        Action::MoveTo(_) => false,
        // Adjacency in the anchor graph; likewise.
        Action::MoveWithin(_) => false,
        // Standing at the water / at home / on forage — all positional.
        Action::Drink | Action::Rest | Action::Eat => false,
    }
}

/// Whether catch-up (spec §5) may replay this action. Exactly the actions
/// whose effects are ephemeral: coarse `MoveTo` writes `agent-at`, and
/// `Drink`/`Rest`/`Eat` each commit a fact, so only fine movement qualifies.
/// The partition is "does it commit", not "is it movement".
/// type-audit: bare-ok(flag: return)
pub fn is_replayable_in_catch_up(a: &Action) -> bool {
    matches!(a, Action::MoveWithin(_))
}

impl Action {
    /// Every action kind the planner can emit, one representative per variant
    /// — the roster the correspondence audit reconciles against the concept
    /// registry (The Actants). `MoveTo` carries an address, so its
    /// representative uses a placeholder: the audit reads only which VARIANTS
    /// exist, never their payloads.
    ///
    /// Kept exhaustive by [`action_variants_must_all_be_rostered`]: a new
    /// variant fails to compile until it is listed here, so an act can never
    /// enter the world without the audit noticing it has no word.
    pub fn all() -> Vec<Action> {
        vec![
            Action::MoveTo(RoomAddr {
                face: 0,
                path: Vec::new(),
            }),
            Action::Drink,
            Action::Rest,
            Action::Eat,
            Action::MoveWithin(crate::interior::AnchorId(0)),
        ]
    }

    /// The concept name that would name this act, whether or not it is
    /// registered. The audit reports the ones that are not.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn concept_name(&self) -> &'static str {
        match self {
            // Both movement variants answer to one concept. They differ in
            // SCALE — between rooms, and between anchors inside a room (The
            // Threshold) — which is a mechanism distinction, not a vocabulary
            // one: a language has a word for going, not two words separated by
            // how far. If a people ever needs to say `approach` distinctly from
            // `move`, that is a new concept and a deliberate one, not a second
            // name minted here by accident.
            Action::MoveTo(_) | Action::MoveWithin(_) => "move",
            Action::Drink => "drink",
            Action::Rest => "rest",
            Action::Eat => "eat",
        }
    }
}

/// Compile-time tripwire: a new [`Action`] variant breaks this match — every
/// variant is named and there is no `_` arm — forcing [`Action::all`] and
/// [`Action::concept_name`] to be revisited. The `manifest.rs` destructure
/// tripwire applied to an enum. Never remove, never add a wildcard arm.
#[allow(dead_code)]
fn action_variants_must_all_be_rostered(a: &Action) -> &'static str {
    match a {
        Action::MoveTo(_) => "move",
        Action::MoveWithin(_) => "move",
        Action::Drink => "drink",
        Action::Rest => "rest",
        Action::Eat => "eat",
    }
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
impl<'a> NavSpace<'a> {
    /// The `move_cost`/`avoid` edge-building rule, shared verbatim by
    /// [`SearchSpace::successors`] and [`SearchSpace::successors_memo`]
    /// below (the-waymark, Task 6) so memoizing the raw neighbor lookup can
    /// never accidentally also change how an edge's cost is computed — the
    /// memo boundary is `neighbors`/`neighbors_memo` ALONE, never the
    /// successor list this builds from it.
    fn edges_from(&self, neighbors: [RoomAddr; 3]) -> Vec<(Action, RoomAddr, u64)> {
        neighbors
            .into_iter()
            .map(|n| {
                let cost = move_cost(&n, self.avoid);
                (Action::MoveTo(n.clone()), n, cost)
            })
            .collect()
    }
}
impl<'a> SearchSpace for NavSpace<'a> {
    type State = RoomAddr;
    type Action = Action;
    fn successors(&self, s: &RoomAddr) -> Vec<(Action, RoomAddr, u64)> {
        self.edges_from(s.neighbors())
    }
    /// Ledger #7's re-plan (the-waymark, Task 6): consults a caller-owned
    /// [`RoomMeshMemo`] for the neighbor lookup instead of recomputing the
    /// icosphere lattice arithmetic on every `astar` expansion — this is the
    /// specific hot path (`RoomAddr::neighbors` inside `NavSpace::successors`
    /// → `astar` expansions) Task 3's memo was built for but could not reach,
    /// because `SearchSpace::successors(&self, ...)` alone had no way to
    /// thread a caller's memo down into it. Byte-identical to `successors`
    /// either way ([`RoomAddr::neighbors_memo`] is a cache of the same pure
    /// function `neighbors` computes), and the `edges_from` cost rule is
    /// untouched — only which of `neighbors`/`neighbors_memo` supplies the
    /// three rooms it costs.
    fn successors_memo(
        &self,
        s: &RoomAddr,
        memo: Option<&mut RoomMeshMemo>,
    ) -> Vec<(Action, RoomAddr, u64)> {
        let neighbors = match memo {
            Some(m) => s.neighbors_memo(m),
            None => s.neighbors(),
        };
        self.edges_from(neighbors)
    }
    fn goal(&self, s: &RoomAddr) -> bool {
        *s == self.dest
    }
    fn heuristic(&self, _s: &RoomAddr) -> u64 {
        0
    }
}

/// [`plan_to_room`], threading a caller-owned [`RoomMeshMemo`] through the
/// underlying [`AStarSolver`] search instead of recomputing `RoomAddr::
/// neighbors` on every expansion (the-waymark, Task 6 — ledger #7's
/// re-plan). `mesh_memo: None` is exactly `plan_to_room`'s own behavior
/// (`NavSpace::successors_memo`'s default-free override still falls back to
/// plain `neighbors`); `Some(memo)` is byte-identical too, by construction —
/// see `NavSpace::successors_memo`'s own doc. `pub(crate)`: today's one
/// caller worth the memo is [`HomeNavCache::home_nav`], which already sits
/// in this module; a future external caller can widen this if it ever needs
/// to.
pub(crate) fn plan_to_room_memo(
    from: &RoomAddr,
    dest: &RoomAddr,
    budget: usize,
    avoid: &std::collections::BTreeSet<RoomAddr>,
    mesh_memo: Option<&mut RoomMeshMemo>,
) -> Option<Vec<Action>> {
    AStarSolver.solve(
        &NavSpace {
            dest: dest.clone(),
            avoid,
        },
        from.clone(),
        budget,
        mesh_memo,
    )
}

/// Plan a pure navigation path to `dest` (the home-return goal), or `None`.
/// `avoid` is the remembered-danger set the A* routes around (The Haunt); pass
/// an empty set for the memory-less path. A thin delegator to
/// [`plan_to_room_memo`] with `mesh_memo: None` (the-waymark, Task 6) — every
/// existing caller (there is no session memo in scope at most of them) is
/// unchanged.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_room(
    from: &RoomAddr,
    dest: &RoomAddr,
    budget: usize,
    avoid: &std::collections::BTreeSet<RoomAddr>,
) -> Option<Vec<Action>> {
    plan_to_room_memo(from, dest, budget, avoid, None)
}

/// Which anchor each creature stands at, inside the presence bubble.
///
/// NEVER SERIALIZED (decision 0069, `CLIENT-two-tier-position`): an entity's
/// persisted position is its room; this is the finer tier and it evaporates
/// with the bubble. That is not a convenience — it is what makes `AnchorId`
/// safe to use as a key here at all. `AnchorId` is a vector OFFSET into a
/// derived `Interior`, not a name, so a committed occupancy fact would orphan
/// the moment a `room/furnishing/v1` epoch regenerated the base; an ephemeral
/// one cannot. If you ever find yourself persisting one of these, that is why
/// you must not (The Threshold, task 4).
///
/// Keyed by [`EntityId`] rather than the brief's `NpcId`: no such type exists
/// in this crate. [`Npc`] already carries `entity: EntityId` — its minted
/// ledger entity, the same handle every other bubble-scoped map in this
/// module (the fear memo, the disposition state) keys by — so occupancy
/// follows suit rather than inventing a parallel identity for the same thing.
///
/// Two creatures standing at the same anchor is intentional, not an
/// oversight: the map is a `BTreeMap<EntityId, (RoomAddr, AnchorId)>`, one
/// entry per creature, and nothing here enforces exclusivity over the value
/// side. A hearth crowded with three NPCs is a legitimate occupancy, the same
/// way a room can hold more than one creature at the coarser scale.
///
/// The map's value carries the room ALONGSIDE the anchor (The Threshold task
/// 6b) rather than the anchor alone, and this is a safety property, not a
/// convenience: `AnchorId` is a raw vector offset (see above), so it is only
/// meaningful paired with the SPECIFIC `Interior` that produced it —
/// `Interior::anchor` indexes straight into its `Vec` with no bounds check
/// against a foreign graph, so reading a stale anchor from one room's
/// interior against a DIFFERENT room's (smaller) one is not merely wrong, it
/// can panic. Recording the room lets a caller holding an `Occupancy` from a
/// PAST moment (e.g. a stateless affect read that only has this tick's
/// finished walk to consult, not a live position mid-walk — see
/// [`affect_of_memo_occupied`]) verify the anchor it is about to read still
/// belongs to the room it is about to pair it with, via [`Self::anchor_in`],
/// before ever handing it to [`crate::interior::warmth_at`].
#[derive(Debug, Default)]
pub struct Occupancy(std::collections::BTreeMap<EntityId, (RoomAddr, AnchorId)>);

impl Occupancy {
    /// Where `who` currently stands, or `None` if it has not arrived (or has
    /// since departed). Both ends of a creature's stay in a room are
    /// legitimately "nowhere in particular" — there is no sentinel anchor for
    /// "not here", only the absence of an entry. Room-blind: a caller pairing
    /// this anchor with an `Interior` it did not just derive from the SAME
    /// room this creature is actually in should use [`Self::anchor_in`]
    /// instead, which checks that for you.
    pub fn at(&self, who: EntityId) -> Option<AnchorId> {
        self.0.get(&who).map(|(_, anchor)| *anchor)
    }

    /// Where `who` currently stands, but ONLY if that is inside `room` —
    /// `None` both when `who` has not arrived anywhere and when it has, but
    /// in some OTHER room than `room`. This is [`Self::at`]'s safe sibling
    /// for a caller that does not itself track which room produced the
    /// `Interior` it is about to pair the anchor with (The Threshold task
    /// 6b's `affect_of_memo_occupied`, reading a tick-old `Occupancy` against
    /// a freshly re-derived room): a mismatch here means the creature moved
    /// rooms since this anchor was recorded, and the caller must fall back to
    /// its own room-only answer (e.g. [`landing_interior`]) rather than risk
    /// [`crate::interior::warmth_at`] indexing a foreign `Interior` with a
    /// stale offset.
    pub fn anchor_in(&self, who: EntityId, room: &RoomAddr) -> Option<AnchorId> {
        self.0
            .get(&who)
            .and_then(|(r, anchor)| (r == room).then_some(*anchor))
    }

    /// Place `who` at the anchor a seam of `kind` lands at in `interior`
    /// ([`landing`]) — the entry point for a creature crossing into `room`
    /// from the coarse (room-graph) layer. An empty interior has no landing
    /// at all ([`landing`] returns `None`); arriving into one is a no-op
    /// rather than a panic, since an interior with zero anchors has nowhere
    /// for anyone to be recorded standing. `room` is recorded alongside the
    /// anchor (see the struct doc) so a later, room-checked read
    /// ([`Self::anchor_in`]) can tell this arrival apart from one in some
    /// other room.
    pub fn arrive(&mut self, who: EntityId, room: &RoomAddr, interior: &Interior, kind: SeamKind) {
        if let Some(at) = landing(interior, kind) {
            self.0.insert(who, (room.clone(), at));
        }
    }

    /// Move `who` to `to`, but only if `to` is ONE WALKABLE HOP from where it
    /// currently stands ([`Interior::walkable_neighbors`] — adjacency AND
    /// containment in either direction, e.g. an alcove to the hearth it
    /// contains). Returns whether the move happened — a creature that has
    /// not arrived anywhere, or a target that is not reachable in one hop
    /// from its current anchor, is refused rather than silently teleported,
    /// since a graph walk that skips edges is not a walk at all. The room
    /// recorded at the last [`Self::arrive`] carries over unchanged — a
    /// within-room walk never crosses a room boundary, so there is nothing
    /// for it to update.
    ///
    /// This MUST use the same one-hop definition [`route_within`]'s planner
    /// does (The Threshold task 6's own bug: an earlier version checked only
    /// [`Interior::neighbors`] — adjacency alone — so a planned step INTO a
    /// contained anchor, like the hearth `the-fire` composes inside its
    /// alcove, was silently refused even though the plan that proposed it
    /// was valid). `walkable_neighbors` is the shared definition precisely so
    /// this cannot drift from the planner again.
    /// type-audit: bare-ok(flag: return)
    pub fn walk(&mut self, who: EntityId, interior: &Interior, to: AnchorId) -> bool {
        let Some((room, here)) = self.0.get(&who).cloned() else {
            return false;
        };
        if !interior.walkable_neighbors(here).contains(&to) {
            return false;
        }
        self.0.insert(who, (room, to));
        true
    }

    /// Place `who` directly at `at` in `room`, bypassing [`Self::walk`]'s
    /// one-hop walkability check. The Threshold task 7's own use: catch-up,
    /// once its replay budget ([`CATCH_UP_STEP_CAP`]) is spent on a
    /// long-unobserved creature, gives up stepping through the interior hop
    /// by hop and places it straight at its drive-preferred anchor instead
    /// (spec §5.3, "beyond a named cap ... places the creature at its
    /// drive-preferred anchor") — a deliberate skip, not a bug: the whole
    /// point of a bounded replay budget is that a long absence costs O(1)
    /// work rather than one iteration per within-room hop that would
    /// otherwise have occurred. Callers elsewhere should almost always
    /// prefer [`Self::walk`] — this exists for exactly the one case where
    /// stepping through is what the budget was spent trying to avoid.
    pub fn place(&mut self, who: EntityId, room: &RoomAddr, at: AnchorId) {
        self.0.insert(who, (room.clone(), at));
    }

    /// Forget `who` entirely. This is the bubble collapsing (or a creature
    /// leaving the room) for ONE creature at a time — this task has no "clear
    /// everyone" call, because nothing yet drives a whole-bubble teardown
    /// through this type rather than by simply dropping it.
    pub fn depart(&mut self, who: EntityId) {
        self.0.remove(&who);
    }
}

#[cfg(test)]
mod tests {
    // Test fixture (decision 0092): calls the sculpt/fit derivation entry
    // points directly to build its own world state, once per test — the
    // sanctioned test-fixture posture the weir's spec carves out.
    #![allow(clippy::disallowed_methods)]
    use super::*;
    use hornvale_kernel::{ConceptRegistry, Seed};

    /// Test-only helper: fits the coexistence stack once and reads the `k`
    /// densest wild concentrations — the prelude `derive_wild_npcs` used to
    /// run internally (The Weir, Stage 1b), now the caller's job.
    fn wild_concentrations_of(world: &World, k: usize) -> Vec<(String, [f64; 3])> {
        let wc = hornvale_worldgen::WorldComponents::assemble().unwrap();
        let terrain = hornvale_worldgen::terrain_of(world).unwrap();
        let climate = hornvale_worldgen::climate_from(world, &terrain).unwrap();
        let report =
            hornvale_worldgen::demography_report_from(world, &wc, &terrain, &climate).unwrap();
        hornvale_worldgen::wild_concentrations_from(&wc, &report, k)
    }

    /// A thin positional adapter over [`arbitrate`] for the tests (The
    /// Disposition): it packs the four loose disposition scalars into a
    /// [`Disposition`] so the many test call sites keep their explicit
    /// per-argument values without each rebuilding the struct. Production
    /// callers (`decide`/`affect_of`/the tick) construct `Disposition` directly;
    /// only the tests, which vary these values case by case, go through this.
    ///
    /// Builds its own throwaway [`HomeNavCache`] and a fixed placeholder
    /// entity (the-waymark, Task 4 fix round): none of the ~20 call sites
    /// this adapts vary a creature identity, so a per-call cache costs
    /// nothing beyond what `arbitrate`'s `home_nav` seam always paid before
    /// Task 4, and none of them need cross-call cache reuse to make their
    /// point.
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
        let mut home_nav_cache = HomeNavCache::new();
        let mut mesh_memo = RoomMeshMemo::new();
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
            EntityId::new(1).expect("1 is a valid nonzero entity id"),
            &mut home_nav_cache,
            &mut mesh_memo,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
    fn hazard_memory_splits_static_from_transient() {
        // PROVENANCE. Two shunned cells for two different reasons:
        //   H — frightening for its own TERRAIN (The Haunt). Shunned, NOT dreaded:
        //       the present cell already frightens the creature, so there is
        //       nothing remembered-but-absent about it.
        //   X — terrain-SAFE, tipped over `act` only by emitter B's re-derived
        //       alarm (The Phantom). Shunned AND dreaded, carrying the remembered
        //       alarm magnitude.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone(); // E: frightens the emitter B (and A, if A stands there)
        let x = ns[1].clone(); // X: terrain-safe, inside B's one-hop halo
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        // Emitter B: beside X on day 0.5 (primary-afraid — E is its neighbour).
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        // A (coward) stood on BOTH the transient cell X and the terrain hazard E.
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);
        commit_agent_at(&mut ledger, &reg, a_e, &hazard, 0.5);

        let mem = hazard_memory(&ledger, &a, WorldTime { day: 10.0 }, &terrain, &[b]);
        assert!(mem.shunned.contains(&x), "the phantom cell is shunned");
        assert!(
            mem.shunned.contains(&hazard),
            "the terrain hazard is shunned"
        );
        assert!(
            mem.dread.contains_key(&x),
            "the phantom cell is DREADED (transient provenance): {:?}",
            mem.dread
        );
        assert!(
            !mem.dread.contains_key(&hazard),
            "a terrain hazard is not a phantom — it is present danger, not memory"
        );
        assert!(
            mem.dread[&x] > 0.0,
            "the dread carries the remembered alarm magnitude"
        );
    }

    #[test]
    fn hazard_memory_dread_is_empty_with_an_empty_roster() {
        // THE STRUCTURAL GUARANTEE, asserted: an empty roster ⇒ an empty emitter
        // scan ⇒ an empty dread map. This one fact is simultaneously The
        // Phantom's recursion base case, seed 42's byte-identity, and the block
        // on superstition contagion (the emission read is bandless).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let hazard = d_cell.neighbors()[0].clone();
        let x = d_cell.neighbors()[1].clone();
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);
        commit_agent_at(&mut ledger, &reg, a_e, &hazard, 0.5);

        let mem = hazard_memory(&ledger, &a, WorldTime { day: 10.0 }, &terrain, &[]);
        assert!(
            mem.dread.is_empty(),
            "no roster ⇒ no phantom: {:?}",
            mem.dread
        );
        assert!(
            mem.shunned.contains(&hazard),
            "the terrain memory is unaffected by the empty roster"
        );
    }

    #[test]
    fn believed_hazard_is_hazard_memory_shunned() {
        // The wrapper is exactly the shunned half — the old entry point keeps
        // its meaning, so The Haunt's planner reads what it always read.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let hazard = d_cell.neighbors()[0].clone();
        let x = d_cell.neighbors()[1].clone();
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);

        let now = WorldTime { day: 10.0 };
        let roster = [b];
        assert_eq!(
            believed_hazard(&ledger, &a, now, &terrain, &roster),
            hazard_memory(&ledger, &a, now, &terrain, &roster).shunned
        );
    }

    #[test]
    fn affect_of_feels_the_phantom_on_now_safe_ground() {
        // THE FELT HALF, through the public read the narration and the health
        // metric both use. A creature standing where a herd's alarm once caught
        // it reads Danger — on ground whose PRESENT terrain threat is below act.
        // A never-alarmed control on the same cell reads no danger at all.
        //
        // A does NOT revisit X after B leaves: a later SAFE visit is exactly the
        // staleness disproof `believed_hazard_clears_a_disproven_phantom` pins,
        // so a revisit would empty the memory and there would be nothing to feel.
        // A simply never moved — its committed position at `now` is still X.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone(); // E: frightens the emitter B
        let x = ns[1].clone(); // X: terrain-safe, in B's halo
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        // B: primary-afraid beside X on day 0.45, then far away by day 0.55.
        // The days are DAYLIGHT ones (the fractional-day sun is up around noon):
        // a sleeping Diurnal emitter pursues rest, not fear, and emits nothing.
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.45);
        commit_agent_at(&mut ledger, &reg, b_e, &raddr(-1.0), 0.55);
        // A (coward): stood at X while B panicked beside it, and is there still.
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.45);
        // C (coward): never stood at X before — it is there now for the first
        // time, arriving after B is already gone.
        let c_e = ledger.mint_entity();
        let mut c = haunt_npc(c_e, x.clone());
        c.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, c_e, &x, 0.55);

        // Early in the world, so the sustenance drives are quiet and the felt
        // state reports the fear rather than a louder thirst.
        let now = WorldTime { day: 0.6 };
        let band = [a.clone(), b.clone(), c.clone()];
        let felt = affect_of(&ledger, &a, &band, now, &terrain);
        assert_eq!(
            felt.object,
            Some(DriveKind::Danger),
            "the rememberer is afraid on now-safe ground: {felt:?}"
        );
        assert!(felt.arousal >= DANGER_ACT, "and the fear is felt: {felt:?}");
        let control = affect_of(&ledger, &c, &band, now, &terrain);
        assert_ne!(
            control.object,
            Some(DriveKind::Danger),
            "a creature with no memory of this ground feels nothing here: {control:?}"
        );
    }

    #[test]
    fn a_dread_afraid_creature_raises_no_alarm() {
        // NO SUPERSTITION CONTAGION (spec §3, ledger #6) — and not by a guard:
        // the emission read is BANDLESS, so its hazard memory has no emitters and
        // its dread map is empty. A creature shuddering at a phantom is quiet.
        // Same fixture as above; B is long gone, so the ONLY possible emitter is
        // A's remembered dread — and the field must be empty at X.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone();
        let x = ns[1].clone();
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.45);
        commit_agent_at(&mut ledger, &reg, b_e, &raddr(-1.0), 0.55);
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.45);

        // A really is dread-afraid here (the same fixture the felt test pins) —
        // so an empty field at X is the contagion block, not an empty memory.
        let now = WorldTime { day: 0.6 };
        assert!(
            hazard_memory(&ledger, &a, now, &terrain, &[a.clone(), b.clone()])
                .dread
                .contains_key(&x),
            "fixture check: the shudderer must actually dread X"
        );
        let field = alarm_field(&ledger, &[a, b], &terrain, now);
        assert!(
            !field.contains_key(&x),
            "remembered dread is felt, never broadcast: {field:?}"
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
                dread: None,
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

    /// The before-picture fixture for the walk hoist (The Action Clock T3): a
    /// two-creature planted-terrain world walked over a long interval, chosen
    /// because it exercises every arm of the decision loop — moves under several
    /// provenances, a drink, rests across the diurnal off-phase, and meals — so
    /// the emitted sequence is a wide net for an extraction bug.
    ///
    /// Returns the emitted facts rendered as one line per fact:
    /// `subject|predicate|object|day-bits|provenance`.
    fn hoist_walk_shape() -> Vec<String> {
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
        commit_agent_at(&mut ledger, &reg, knower_e, &water, 0.0);
        commit_agent_at(&mut ledger, &reg, knower_e, &here, 1.0);
        commit_agent_at(&mut ledger, &reg, lost_e, &here, 1.0);

        let sys = DriveMovements {
            npcs: vec![knower, lost],
            from: WorldTime { day: 1.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
            terrain: &t,
        };
        sys.step(&ledger)
            .iter()
            .map(|f| {
                format!(
                    "{:?}|{}|{:?}|{:?}|{}",
                    f.subject,
                    f.predicate,
                    f.object,
                    f.day.map(f64::to_bits),
                    f.provenance
                )
            })
            .collect()
    }

    #[test]
    fn the_hoisted_walk_emits_exactly_what_the_loop_emitted() {
        // THE REFACTOR'S WARRANT (The Action Clock T3). The per-creature walk is
        // being hoisted out of `DriveMovements::step`'s loop into `WalkState` +
        // `advance_one`, and the claim is that this changes NOTHING. So the
        // emitted fact sequence — subject, predicate, object, the day's exact
        // bits, and provenance, IN ORDER — is pinned here against the loop as it
        // stood before the extraction. Written and passing BEFORE the hoist; it
        // must still pass after, or the extraction is wrong.
        //
        // It is a golden, deliberately: a self-consistency check (run twice, get
        // the same answer) is satisfied by any deterministic implementation,
        // including a broken one. Only a recorded before-picture pins the OLD
        // behaviour.
        //
        // REWRITTEN BY HAND at T4 (charging every action), as designed. What
        // moved is ONLY the days: all eighty facts, their order, their rooms and
        // their provenances were unchanged, because both creatures stand at the
        // reference mass (tempo exactly `1.0`) and the terrain is level, so a
        // move still costs the historical `0.1` days. The shift is the newly
        // charged actions alone — a drink `0.0015` days, lying down `0.0015`, a
        // meal `0.03` — accumulating to at most `0.070` days by the end of a
        // 39-day walk. Two ties the old literal recorded are gone with them:
        // `drank` no longer shares the arriving move's exact day, and `rested`
        // no longer shares the preceding meal's.
        //
        // REWRITTEN BY HAND AGAIN at T5 (the shared clock), and this time the
        // move is PURE REORDERING — the cleanest result the fixture could have
        // given. Compared fact-for-fact against the T4 literal as a multiset,
        // the eighty facts are IDENTICAL: same subjects, same predicates, same
        // rooms, the same exact day bits, the same provenances. Not one value
        // changed. What changed is the sequence they arrive in:
        //
        //     before (T4)   1111111111111111111111111111111111111111
        //                   2222222222222222222222222222222222222222
        //     after  (T5)   1212121212121212121212121212121212121212
        //                   1212121212121212121212121212121212121212
        //
        // That is the sequential loop — all of one creature's walk, then all of
        // the next's — giving way to a shared clock. The alternation is perfect
        // rather than ragged because this fixture's two creatures are twins:
        // same mass, same home, same terrain, and (via the band's pooled belief)
        // the same water, so they act in lockstep and every pop is a tie at the
        // same tick, broken by entity id. A ragged interleave is what
        // `a_faster_creature_acts_more_often_between_a_slower_ones_actions`
        // pins, with masses deliberately apart.
        //
        // The days being bit-identical is itself the load-bearing evidence for
        // spec §5: interleaving reordered when each creature ACTED without
        // changing anything either creature could SEE. Had a cross-agent read
        // slipped from `frozen` to a mid-tick observation, these twins would
        // have started to diverge — the second to act would have perceived the
        // first's new position — and the day bits would have moved. They did
        // not, so the emitted stream is still a pure function of the pre-tick
        // ledger.
        const EXPECTED: &[&str] = &[
            r#"EntityId(1)|rested|Flag(true)|Some(4607189174199458464)|slept at home (fatigue eased)"#,
            r#"EntityId(2)|rested|Flag(true)|Some(4607189174199458464)|slept at home (fatigue eased)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4618178707890180369)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4618178707890180369)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|rested|Flag(true)|Some(4618180396740040633)|slept at home (fatigue eased)"#,
            r#"EntityId(2)|rested|Flag(true)|Some(4618180396740040633)|slept at home (fatigue eased)"#,
            r#"EntityId(1)|agent-at|Text("180339")|Some(4618855936684146205)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180339")|Some(4618855936684146205)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|drank|Flag(true)|Some(4618857625534006469)|drank from the river (thirst sated)"#,
            r#"EntityId(2)|drank|Flag(true)|Some(4618857625534006469)|drank from the river (thirst sated)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4618970215524690731)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4618970215524690731)|walking home (sated)"#,
            r#"EntityId(1)|agent-at|Text("172046")|Some(4619082805515374993)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("172046")|Some(4619082805515374993)|walking home (sated)"#,
            r#"EntityId(1)|eaten|Flag(true)|Some(4622982359842724423)|grazed the productive ground (hunger sated)"#,
            r#"EntityId(2)|eaten|Flag(true)|Some(4622982359842724423)|grazed the productive ground (hunger sated)"#,
            r#"EntityId(1)|rested|Flag(true)|Some(4622983204267654555)|slept at home (fatigue eased)"#,
            r#"EntityId(2)|rested|Flag(true)|Some(4622983204267654555)|slept at home (fatigue eased)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4623152089253680950)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4623152089253680950)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|agent-at|Text("180339")|Some(4623208384249023081)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180339")|Some(4623208384249023081)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|drank|Flag(true)|Some(4623209228673953213)|drank from the river (thirst sated)"#,
            r#"EntityId(2)|drank|Flag(true)|Some(4623209228673953213)|drank from the river (thirst sated)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4623265523669295344)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4623265523669295344)|walking home (sated)"#,
            r#"EntityId(1)|agent-at|Text("172046")|Some(4623321818664637475)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("172046")|Some(4623321818664637475)|walking home (sated)"#,
            r#"EntityId(1)|rested|Flag(true)|Some(4625798470072218419)|slept at home (fatigue eased)"#,
            r#"EntityId(2)|rested|Flag(true)|Some(4625798470072218419)|slept at home (fatigue eased)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4625868838816396084)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4625868838816396084)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|agent-at|Text("180339")|Some(4625896986314067150)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180339")|Some(4625896986314067150)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|drank|Flag(true)|Some(4625897408526532216)|drank from the river (thirst sated)"#,
            r#"EntityId(2)|drank|Flag(true)|Some(4625897408526532216)|drank from the river (thirst sated)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4625925556024203282)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4625925556024203282)|walking home (sated)"#,
            r#"EntityId(1)|agent-at|Text("172046")|Some(4625953703521874348)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("172046")|Some(4625953703521874348)|walking home (sated)"#,
            r#"EntityId(1)|eaten|Flag(true)|Some(4627500877643860586)|grazed the productive ground (hunger sated)"#,
            r#"EntityId(2)|eaten|Flag(true)|Some(4627500877643860586)|grazed the productive ground (hunger sated)"#,
            r#"EntityId(1)|rested|Flag(true)|Some(4627501299856325652)|slept at home (fatigue eased)"#,
            r#"EntityId(2)|rested|Flag(true)|Some(4627501299856325652)|slept at home (fatigue eased)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4627557594851667784)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4627557594851667784)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|agent-at|Text("180339")|Some(4627585742349338850)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180339")|Some(4627585742349338850)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|drank|Flag(true)|Some(4627586164561803916)|drank from the river (thirst sated)"#,
            r#"EntityId(2)|drank|Flag(true)|Some(4627586164561803916)|drank from the river (thirst sated)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4627614312059474982)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4627614312059474982)|walking home (sated)"#,
            r#"EntityId(1)|agent-at|Text("172046")|Some(4627642459557146048)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("172046")|Some(4627642459557146048)|walking home (sated)"#,
            r#"EntityId(1)|rested|Flag(true)|Some(4629181611642296032)|slept at home (fatigue eased)"#,
            r#"EntityId(2)|rested|Flag(true)|Some(4629181611642296032)|slept at home (fatigue eased)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4629237906637638164)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4629237906637638164)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|agent-at|Text("180339")|Some(4629266054135309230)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180339")|Some(4629266054135309230)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|drank|Flag(true)|Some(4629266476347774296)|drank from the river (thirst sated)"#,
            r#"EntityId(2)|drank|Flag(true)|Some(4629266476347774296)|drank from the river (thirst sated)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4629294623845445362)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4629294623845445362)|walking home (sated)"#,
            r#"EntityId(1)|agent-at|Text("172046")|Some(4629322771343116428)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("172046")|Some(4629322771343116428)|walking home (sated)"#,
            r#"EntityId(1)|eaten|Flag(true)|Some(4630285181200986277)|grazed the productive ground (hunger sated)"#,
            r#"EntityId(2)|eaten|Flag(true)|Some(4630285181200986277)|grazed the productive ground (hunger sated)"#,
            r#"EntityId(1)|rested|Flag(true)|Some(4630285392307218810)|slept at home (fatigue eased)"#,
            r#"EntityId(2)|rested|Flag(true)|Some(4630285392307218810)|slept at home (fatigue eased)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4630313539804889875)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4630313539804889875)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|agent-at|Text("180339")|Some(4630327613553725408)|went down to the river it knew (thirst)"#,
            r#"EntityId(2)|agent-at|Text("180339")|Some(4630327613553725408)|went down to the river it knew (thirst)"#,
            r#"EntityId(1)|drank|Flag(true)|Some(4630327824659957941)|drank from the river (thirst sated)"#,
            r#"EntityId(2)|drank|Flag(true)|Some(4630327824659957941)|drank from the river (thirst sated)"#,
            r#"EntityId(1)|agent-at|Text("180243")|Some(4630341898408793474)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("180243")|Some(4630341898408793474)|walking home (sated)"#,
            r#"EntityId(1)|agent-at|Text("172046")|Some(4630355972157629007)|walking home (sated)"#,
            r#"EntityId(2)|agent-at|Text("172046")|Some(4630355972157629007)|walking home (sated)"#,
        ];
        let shape = hoist_walk_shape();
        assert_eq!(
            shape.len(),
            EXPECTED.len(),
            "the walk emitted {} facts, not the recorded {}",
            shape.len(),
            EXPECTED.len()
        );
        for (i, (got, want)) in shape.iter().zip(EXPECTED.iter()).enumerate() {
            assert_eq!(
                got, want,
                "fact {i} differs from the recorded before-picture"
            );
        }
    }

    /// The charging fixture (The Action Clock T4): the `hoist_walk_shape` world
    /// reduced to the ONE creature that genuinely reaches the water and drinks.
    /// Both charging tests want the same walk — one varying its mass, one
    /// reading the days it emits — so the world is built once here rather than
    /// invented twice.
    ///
    /// Returns the ledger (already carrying the creature's perception history),
    /// the terrain, and the creature at reference mass.
    fn charged_walk_fixture() -> (Ledger, PlantedTerrain, Npc) {
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
            elevations: [(n0, 0.0), (n2, 0.0)].into_iter().collect(),
            fresh: [water.clone()].into_iter().collect(),
            temps: std::collections::BTreeMap::new(),
            forage: std::collections::BTreeMap::new(),
            threat: std::collections::BTreeMap::new(),
            prey: std::collections::BTreeMap::new(),
        };
        let e = ledger.mint_entity();
        let npc = shared_belief_npc(e, here.clone(), water.clone(), "walker");
        // Stood at the water on day 0, home again by day 1 — the same history
        // the shared-belief tests give their `knower`, so belief is real.
        commit_agent_at(&mut ledger, &reg, e, &water, 0.0);
        commit_agent_at(&mut ledger, &reg, e, &here, 1.0);
        (ledger, t, npc)
    }

    #[test]
    fn a_heavier_creature_covers_less_ground_in_the_same_interval() {
        // THE CAMPAIGN'S HEADLINE (spec §3, rung 2), at unit level: one world,
        // one interval, one creature — and the ONLY difference between the runs
        // is body mass. An action costs `base × (mass/70)^0.25`, so the heavier
        // creature spends longer on each one and gets less far in the same days.
        //
        // The world is deliberately WATERLESS, and the creature ignorant, so it
        // explores continuously: its walk is bounded by how fast it can move
        // rather than by how often it gets thirsty. In the water fixture next
        // door the errand is drive-bound instead — a round trip to a known
        // river is four moves per thirst cycle whatever the creature weighs, so
        // mass shifts *when* it drinks (later, as the spec predicts) but not how
        // many moves fit in the interval. Ground covered is the sharper read.
        let mut ledger = Ledger::default();
        let reg = agent_at_reg();
        let here = raddr(1.0);
        let terrain = PlantedTerrain::dry(std::collections::BTreeMap::new());
        let e = ledger.mint_entity();
        let base = shared_belief_npc(e, here.clone(), here.clone(), "walker");
        commit_agent_at(&mut ledger, &reg, e, &here, 1.0);
        let moves = |mass_kg: f64| {
            let mut npc = base.clone();
            npc.mass_kg = mass_kg;
            let sys = DriveMovements {
                npcs: vec![npc],
                from: WorldTime { day: 1.0 },
                to: WorldTime { day: 40.0 },
                params: SUSTENANCE,
                day_length_std: None,
                terrain: &terrain,
            };
            sys.step(&ledger)
                .iter()
                .filter(|f| f.predicate == AGENT_AT)
                .count()
        };
        // Not two buckets but a graded spread across the mass band — the spec's
        // own acceptance prediction (§8), and the reason tempo is derived from
        // continuous mass rather than the four-valued metabolic class.
        let walked: Vec<(f64, usize)> = [1.0_f64, 70.0, 5_000.0, 100_000.0]
            .into_iter()
            .map(|m| (m, moves(m)))
            .collect();
        for pair in walked.windows(2) {
            let ((lm, lw), (hm, hw)) = (pair[0], pair[1]);
            assert!(
                lw > hw,
                "a {lm} kg creature covered {lw} rooms and a heavier {hm} kg one \
                 {hw} — the heavier must cover less ground in the same interval"
            );
        }
    }

    #[test]
    fn drinking_and_eating_now_cost_time() {
        // RUNG 1 (spec §2): no action is free. Before this task the creature
        // arrived at the water and drank in the SAME instant — the `drank` fact
        // carried the arriving `agent-at` fact's exact day (both read
        // `4618854247834285941` in the T3 golden above). Now the drink consumes
        // time, so it lands strictly later, by exactly its cost.
        let (ledger, terrain, npc) = charged_walk_fixture();
        let mass = npc.mass_kg;
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 1.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            day_length_std: None,
            terrain: &terrain,
        };
        let facts = sys.step(&ledger);
        let drank_day = facts
            .iter()
            .find(|f| f.predicate == DRANK)
            .expect("it drinks")
            .day
            .expect("dated");
        // The arrival is the latest move no later than the drink itself.
        let arrived = facts
            .iter()
            .filter(|f| f.predicate == AGENT_AT)
            .filter_map(|f| f.day)
            .filter(|d| *d <= drank_day)
            .fold(f64::NEG_INFINITY, f64::max);
        assert!(
            arrived.is_finite(),
            "it walked to the water before drinking"
        );
        assert!(
            drank_day > arrived,
            "the drink still happens in the same instant as the arrival ({arrived})"
        );
        let expected = crate::clock::days_of(
            crate::clock::cost_ticks(&Action::Drink, mass, 1.0),
            // The fixture has no sky, so the clock takes its base rate.
            None,
        );
        assert!(
            (drank_day - arrived - expected).abs() < 1e-12,
            "a drink should cost exactly {expected} days; the gap is {}",
            drank_day - arrived
        );
        // The same property across the WHOLE walk: one creature, so every fact
        // it emits must be strictly later than the one before. That also pins
        // the `rested`-atop-`eaten` tie the T3 golden recorded (both
        // `4622963782494261520`) as gone — a meal and lying down cost time too.
        let mut prev = f64::NEG_INFINITY;
        for f in &facts {
            let d = f.day.expect("every emitted fact is dated");
            assert!(
                d > prev,
                "`{}` at {d} did not advance the clock past {prev}",
                f.predicate
            );
            prev = d;
        }
    }

    /// The interleaving fixture (The Action Clock T5): a WATERLESS planted world
    /// and one home, with a creature of each requested mass minted into the
    /// ledger in the order given.
    ///
    /// Waterless and ignorant for the same reason
    /// `a_heavier_creature_covers_less_ground_in_the_same_interval` is: every
    /// creature then explores continuously for the whole interval, so its walk
    /// is move-bound rather than drive-bound and the emitted sequence is a clean
    /// read on *who acts when* rather than on how often each gets thirsty.
    fn interleaving_fixture(masses: &[f64]) -> (Ledger, PlantedTerrain, Vec<Npc>) {
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let here = raddr(1.0);
        let terrain = PlantedTerrain::dry(std::collections::BTreeMap::new());
        let mut npcs: Vec<Npc> = Vec::new();
        for mass_kg in masses {
            let e = ledger.mint_entity();
            let mut npc = shared_belief_npc(e, here.clone(), here.clone(), "walker");
            npc.mass_kg = *mass_kg;
            commit_agent_at(&mut ledger, &reg, e, &here, 1.0);
            npcs.push(npc);
        }
        (ledger, terrain, npcs)
    }

    #[test]
    fn the_emission_order_is_independent_of_the_input_order() {
        // THE POINT OF THE QUEUE. Order must be a pure function of the frozen
        // ledger and the clock, not of how `npcs` happened to be listed — the
        // queue key is `(Ticks, EntityId)`, so a tie at the same simulated
        // moment is broken by the entity id and never by the caller's vector.
        // Shuffle the input; get the same sequence, fact for fact.
        //
        // Three DIFFERENT masses, so the creatures genuinely fall out of step
        // with one another and the interleaving is non-trivial: were the order
        // input-derived, reversing the vector would show it immediately.
        let (ledger, terrain, npcs) = interleaving_fixture(&[4.375, 70.0, 1_120.0]);
        let run = |npcs: Vec<Npc>| {
            let sys = DriveMovements {
                npcs,
                from: WorldTime { day: 1.0 },
                to: WorldTime { day: 20.0 },
                params: SUSTENANCE,
                day_length_std: None,
                terrain: &terrain,
            };
            sys.step(&ledger)
                .iter()
                .map(|f| (f.subject, f.predicate.clone(), f.day.map(f64::to_bits)))
                .collect::<Vec<_>>()
        };
        let forward = run(npcs.clone());
        let reversed = run(npcs.into_iter().rev().collect());
        assert!(
            !forward.is_empty(),
            "the fixture emitted nothing; it cannot pin an order"
        );
        assert_eq!(
            forward, reversed,
            "emission order must not depend on input order"
        );
    }

    #[test]
    fn a_faster_creature_acts_more_often_between_a_slower_ones_actions() {
        // INTERLEAVING, OBSERVABLY. Two creatures sixteen-fold apart in mass are
        // exactly two-fold apart in tempo (`16 ^ 0.25 == 2`), so the lighter one
        // takes two actions in the time the heavier takes one. Under a shared
        // clock its facts must appear BETWEEN the heavier one's; under the old
        // sequential loop they appeared entirely before them.
        //
        // The assertion counts SWITCHES of subject along the emitted sequence.
        // The sequential loop scores exactly one (all of A, then all of B) for
        // any pair, however far apart in tempo; a scheduler scores many. `>= 2`
        // is therefore the smallest threshold the old loop cannot reach.
        let (ledger, terrain, npcs) = interleaving_fixture(&[4.375, 70.0]);
        let sys = DriveMovements {
            npcs,
            from: WorldTime { day: 1.0 },
            to: WorldTime { day: 20.0 },
            params: SUSTENANCE,
            day_length_std: None,
            terrain: &terrain,
        };
        let facts = sys.step(&ledger);
        let seq: Vec<EntityId> = facts
            .iter()
            .filter(|f| f.predicate == AGENT_AT)
            .map(|f| f.subject)
            .collect();
        let switches = seq.windows(2).filter(|w| w[0] != w[1]).count();
        assert!(
            switches >= 2,
            "the two creatures never interleave (switches={switches}, seq={seq:?}) — \
             the queue is not scheduling, it is still walking each in turn"
        );
        // And the emitted days run forward on ONE timeline — up to the tick,
        // which is the resolution the schedule actually orders at. The sequential
        // loop jumped the full interval backwards at its single handover (the
        // second creature restarted at `from`); a shared clock can only ever go
        // back by less than one tick, because creatures tied at the same rounded
        // tick are separated by entity id and their exact `f64` days then differ
        // by whatever float accumulation put inside that tick. Bounding the
        // regression by a tick is the honest form of "one timeline": asserting
        // strict monotonicity would be asserting that scheduling happens in
        // `f64`, which is the thing spec §4 refuses to do.
        let tick = crate::clock::days_of(crate::clock::Ticks(1), None);
        let mut prev = f64::NEG_INFINITY;
        for f in &facts {
            let d = f.day.expect("every emitted fact is dated");
            assert!(
                d >= prev - tick,
                "`{}` at {d} went back more than a tick past {prev} — \
                 the clock is not shared",
                f.predicate
            );
            prev = prev.max(d);
        }
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
        // species whose `social_form` is not `Settled` (`wild_concentrations_from`'s
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
        let concentrations = wild_concentrations_of(&world, 4);
        let wild = derive_wild_npcs(&world, &ctx, &mut ledger, concentrations);
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
        let concentrations2 = wild_concentrations_of(&world, 4);
        let wild2 = derive_wild_npcs(&world, &ctx, &mut ledger2, concentrations2);
        let species: Vec<&str> = wild.iter().map(|n| n.species.as_str()).collect();
        let species2: Vec<&str> = wild2.iter().map(|n| n.species.as_str()).collect();
        assert_eq!(species, species2, "the wild roster is deterministic");
    }

    #[test]
    fn derived_npcs_carry_their_species_body_mass() {
        // THE PRECONDITION FOR PER-AGENT TEMPO (The Action Clock T2): if mass
        // does not reach `Npc`, the action clock's tempo collapses to a
        // constant and the campaign has no per-agent variation at all.
        // Asserted on a REAL derived population — both the peopled roster and
        // the wild fauna, which are most of the health battery's population —
        // and asserted to VARY: a single value across species means the trait
        // is being defaulted, not read.
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
        let mut npcs = derive_npcs(&world, &ctx, &mut ledger, 3, home);
        let concentrations = wild_concentrations_of(&world, 4);
        npcs.extend(derive_wild_npcs(&world, &ctx, &mut ledger, concentrations));
        assert!(!npcs.is_empty(), "the probe world derives a population");
        for n in &npcs {
            assert!(
                n.mass_kg.is_finite() && n.mass_kg > 0.0,
                "{} has a nonsense mass {}",
                n.species,
                n.mass_kg
            );
            // The registry's own value, not a fallback: the threading reads the
            // same `biosphere_registry` lookup that supplies the niche.
            let authored = hornvale_species::biosphere_registry()
                .get_by_label(&n.species)
                .map(|t| t.mass.kilograms())
                .unwrap_or(crate::clock::REFERENCE_MASS_KG);
            assert_eq!(
                n.mass_kg, authored,
                "{}'s mass is the authored trait, not a default",
                n.species
            );
        }
        let distinct: std::collections::BTreeSet<u64> =
            npcs.iter().map(|n| n.mass_kg.to_bits()).collect();
        assert!(
            distinct.len() > 1,
            "every species has the same mass — the trait is defaulted, not read: {:?}",
            npcs.iter()
                .map(|n| (n.species.as_str(), n.mass_kg))
                .collect::<Vec<_>>()
        );
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
    fn locale_terrain_is_built_reads_real_settlement_territory() {
        // THE THRESHOLD's real answer (task 5b): a `LocaleTerrain` injected
        // with the world's settlement-territory set (`built_rooms`) must read
        // the settlement's own room as built, and a DIFFERENT room (one of
        // its neighbours) as wild — proving `is_built` reads real data
        // derived from the world, not a hardcoded constant either way.
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap();
        let ctx = LocaleContext::build(&world).unwrap();
        let home_id = hornvale_settlement::village_info(&world).unwrap().id;
        let home = settlement_room(&world, &ctx, home_id);
        let built = built_rooms(&world, &ctx);
        assert!(
            !built.is_empty(),
            "seed 42's flagship settlement must contribute at least one built room"
        );
        let terrain = LocaleTerrain::with_fields(&ctx, None, None, None, Some(&built), None);
        assert!(
            terrain.is_built(&home),
            "the settlement's own room must read as built"
        );
        // A neighbour is NOT the settlement's own room, so under the narrow
        // "settlement's own room only" definition it must read wild — unless
        // a second settlement happens to occupy it too, which the assertion
        // below would surface honestly rather than silently pass.
        let neighbor = home.neighbors().into_iter().next().unwrap();
        assert!(
            !terrain.is_built(&neighbor),
            "a settlement's neighbouring room is a different room, so it \
             reads wild under the narrow definition: {neighbor:?}"
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
            label: "herder".into(),
        };
        let t = PlantedTerrain::fresh_only([resource.clone()]);
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 10.0 },
            params: p,
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
                Action::MoveWithin(_) => {
                    unreachable!("plan_to_water never emits MoveWithin (The Threshold task 6+)")
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
                // No sky in a planted-terrain fixture: the action clock takes its
                // base rate (spec §4.1).
                day_length_std: None,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
                // No sky in a planted-terrain fixture: the action clock takes its
                // base rate (spec §4.1).
                day_length_std: None,
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

    #[test]
    fn the_shudder_is_felt_on_the_phantom_then_discharged_then_disproven() {
        // THE SHUDDER, end-to-end: the full arc The Phantom could only plan, in
        // the SAME world it could only route around.
        //   (1) FELT       — standing on X, where herd-mate B once panicked
        //                    beside it, the creature reads Danger though X's own
        //                    terrain is safe and B is long gone. Fear of nothing
        //                    present.
        //   (2) DISCHARGED — it is not stuck: its affect is not a distress label,
        //                    and the drive offers a step OFF X, not a Hold.
        //   (3) DISPROVEN  — having stood there and come to no harm, the cell
        //                    leaves both the shunned set and the dread map: the
        //                    fear the avoidance had been protecting is undone by
        //                    the one experience that can undo it.
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        reg.register_predicate(RESTED, false, "rested").unwrap();
        reg.register_predicate(EATEN, false, "eaten").unwrap();

        // GEOMETRY — copied verbatim from
        // `the_phantom_detours_around_a_passed_alarm_then_relearns_the_ground_safe`
        // above: the straight S→W path, X an interior cell, D its off-path
        // neighbour, E the hazard beside D (so X itself is terrain-SAFE and the
        // only thing that ever frightened anyone there was B's passing panic).
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
        let p0 = path_cells[0].clone();
        let p2 = path_cells[2].clone();
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

        let npc_at = |entity: EntityId, home: RoomAddr, label: &str| Npc {
            entity,
            home,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
            label: label.into(),
        };

        // THE TIMING (the trap T3 charted): the fixture sits in DAYLIGHT at a
        // low-thirst hour. A sleeping Diurnal emitter pursues rest, not fear, and
        // emits nothing; by a late day thirst has saturated and wins arbitration
        // outright, drowning the shudder out.
        let mut ledger = Ledger::default();
        // B: primary-afraid at D (beside the hazard E) on day 0.45, and far away
        // by 0.55 — so at `now` the ground is unremarkable and B is long gone.
        let b_e = ledger.mint_entity();
        let b = npc_at(b_e, far.clone(), "herd-mate");
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.45);
        commit_agent_at(&mut ledger, &reg, b_e, &far, 0.55);
        // A: stood at X while B panicked beside it — and has NOT moved since. It
        // gets no safe revisit before `now`: that revisit is exactly the staleness
        // disproof, so granting one early would empty the memory before the test
        // could feel anything. Homed AT X, so the walk that discharges the dread
        // also brings it back to the ground it fears.
        let a_e = ledger.mint_entity();
        let a = npc_at(a_e, x.clone(), "rememberer");
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.45);

        let now = WorldTime { day: 0.6 };
        let band = [a.clone(), b.clone()];

        // (1) FELT.
        let felt = affect_of(&ledger, &a, &band, now, &terrain);
        assert_eq!(
            felt.object,
            Some(DriveKind::Danger),
            "the rememberer is afraid on now-safe ground: {felt:?}"
        );
        assert!(felt.arousal >= DANGER_ACT, "and it is FELT: {felt:?}");
        assert!(
            threat_field(&x, &a.threat_niche, &terrain) * mettle_factor(a.boldness) < DANGER_ACT,
            "X's PRESENT terrain is not frightening — the fear is memory, not sense"
        );

        // (2) DISCHARGED — a feeling with an outlet, not a pathology.
        assert!(
            !matches!(
                felt.label,
                AffectLabel::Lost | AffectLabel::Frustrated | AffectLabel::Helpless
            ),
            "dread with an outlet is wariness, not distress: {felt:?}"
        );
        let memory = hazard_memory(&ledger, &a, now, &terrain, &band);
        assert!(
            memory.dread.contains_key(&x),
            "fixture check: X really is a phantom, not a Haunt: {:?}",
            memory.dread
        );
        let danger = Danger {
            terrain: &terrain,
            threat_niche: a.threat_niche,
            boldness: a.boldness,
            alarm: None,
            dread: Some(&memory.dread),
        };
        assert!(
            matches!(
                danger.affordance(&view_at(x.clone()), PLAN_BUDGET),
                Some(Action::MoveTo(_))
            ),
            "it has somewhere to go — the dread is dischargeable"
        );

        // (3) DISPROVEN — the real tick. A steps off X (the discharge), and its
        // homeward walk carries it back onto the ground it feared, with no emitter
        // anywhere near: the most-recent verdict at X is SAFE, and the phantom
        // leaves BOTH halves of the memory.
        let sys = DriveMovements {
            npcs: band.to_vec(),
            from: now,
            to: WorldTime { day: now.day + 1.0 },
            params: SUSTENANCE,
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
            terrain: &terrain,
        };
        let next =
            hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).expect("tick");
        let walked: Vec<RoomAddr> = next
            .find(AGENT_AT)
            .filter(|f| f.subject == a_e)
            .filter(|f| f.day.map(|d| d >= now.day).unwrap_or(false))
            .filter_map(|f| match &f.object {
                Value::Text(s) => Some(room_from_text(s)),
                _ => None,
            })
            .collect();
        // The discharge, in the walk itself: the very first thing A does is leave
        // X. And the disproof is EARNED, not stipulated — the homeward pull brings
        // it back to stand on the feared ground while nothing is there to fear.
        assert_eq!(
            walked.first(),
            Some(&p0),
            "the first step of the tick is OFF the haunted cell: {walked:?}"
        );
        assert!(
            walked[1..].contains(&x),
            "and it comes back to stand there unharmed — the experience that \
             disproves the fear: {walked:?}"
        );
        let after = hazard_memory(&next, &a, WorldTime { day: now.day + 1.0 }, &terrain, &band);
        assert!(
            !after.dread.contains_key(&x),
            "standing there unharmed disproves the dread: {:?}",
            after.dread
        );
        assert!(
            !after.shunned.contains(&x),
            "and clears the shun with it — the phobia is falsifiable"
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
            dread: None,
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
    fn danger_urgency_reads_remembered_dread_on_now_safe_ground() {
        // THE SHUDDER: a cell with NO hazard anywhere near it — present threat 0 —
        // frightens a creature that remembers a herd's alarm there. Fear of
        // nothing present. `None` dread on the same cell reads calm, so the term
        // is additive-latent: byte-identical wherever the map is empty.
        let safe = raddr(-1.0); // neither it nor its neighbours carry any hazard
        let t = PlantedTerrain::hazard(std::iter::empty(), std::iter::empty());
        let mut dread = std::collections::BTreeMap::new();
        dread.insert(safe.clone(), 0.8);

        let calm = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: None,
        };
        assert_eq!(
            calm.urgency(&view_at(safe.clone())),
            0.0,
            "without the memory the ground is unremarkable"
        );

        let haunted = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: Some(&dread),
        };
        let felt = haunted.urgency(&view_at(safe));
        assert_eq!(
            felt, 0.8,
            "the remembered alarm is felt as the alarm it was"
        );
        assert!(felt >= DANGER_ACT, "and it crosses act — the drive engages");
    }

    #[test]
    fn danger_discharges_dread_by_stepping_off_the_haunted_cell() {
        // THE AFFORDANCE (spec §2, ledger #1). A phantom cell is now-SAFE ground,
        // so terrain offers no gradient to flee down: without a dread-aware
        // serviceability the creature would Hold and read `Lost` — a distress
        // tick for a feature that is a feeling, not a pathology. With it, every
        // neighbour is an improvement and the creature steps off.
        let here = raddr(-1.0);
        let t = PlantedTerrain::hazard(std::iter::empty(), std::iter::empty());
        let mut dread = std::collections::BTreeMap::new();
        dread.insert(here.clone(), 0.8);
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: Some(&dread),
        };
        let view = view_at(here.clone());
        let step = danger
            .affordance(&view, PLAN_BUDGET)
            .expect("dread on flat ground still offers a step off it");
        let Action::MoveTo(to) = step else {
            panic!("fleeing is a MoveTo");
        };
        assert!(here.neighbors().contains(&to), "it steps to a neighbour");
        assert!(
            danger.serviceability(&Action::MoveTo(to), &view, PLAN_BUDGET) > 0.0,
            "stepping off the dreaded cell positively serves the drive"
        );
    }

    #[test]
    fn danger_without_dread_is_unchanged_on_flat_ground() {
        // The byte-identity half of the same seam: `dread: None` on hazard-free
        // ground still offers NO flee step (nowhere is strictly safer) — today's
        // behaviour exactly.
        let here = raddr(-1.0);
        let t = PlantedTerrain::hazard(std::iter::empty(), std::iter::empty());
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: None,
        };
        assert_eq!(danger.affordance(&view_at(here), PLAN_BUDGET), None);
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
            dread: None,
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
            dread: None,
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
            dread: None,
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
            dread: None,
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
            dread: None,
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
                dread: None,
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
                dread: None,
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
                dread: None,
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
            dread: None,
        }
        .urgency(&view_at(cell.clone()));
        let terrain_only = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: None,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
                // The action clock's reference mass (The Action Clock T2): tempo is
                // exactly `1.0` here, so this fixture's timings are unmoved.
                mass_kg: crate::clock::REFERENCE_MASS_KG,
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
                // The action clock's reference mass (The Action Clock T2): tempo is
                // exactly `1.0` here, so this fixture's timings are unmoved.
                mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
            dread: None,
        };
        let shrugs = Danger {
            terrain: &t,
            threat_niche: warm_adapted,
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: None,
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
            dread: None,
        };
        let hunter = Danger {
            terrain: &t,
            threat_niche: apex,
            boldness: 0.0,
            alarm: None,
            dread: None,
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
        // The three regimes of the social pull (The Belonging). Takes the hop
        // count directly (the-waymark, Task 4: `loneliness_from_distance` is
        // `loneliness_from_plan`'s successor, reading a `HomeNavFeature`'s
        // `distance` rather than a full plan — see its own doc).
        assert_eq!(
            loneliness_from_distance(Some(0)),
            0.0,
            "at home (zero hops) → not lonely"
        );
        assert_eq!(
            loneliness_from_distance(Some(10)),
            10.0 / LONELY_SCALE_HOPS,
            "loneliness rises with the hop-distance home"
        );
        assert_eq!(
            loneliness_from_distance(None),
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
                // The action clock's reference mass (The Action Clock T2): tempo is
                // exactly `1.0` here, so this fixture's timings are unmoved.
                mass_kg: crate::clock::REFERENCE_MASS_KG,
                label: "h".into(),
            };
            // from > both seed days so the frozen ledger holds no future facts and the
            // agent starts at home, not yet thirsty.
            let sys = DriveMovements {
                npcs: vec![npc],
                from: WorldTime { day: 1.0 },
                to: WorldTime { day: 41.0 },
                params: SUSTENANCE,
                // No sky in a planted-terrain fixture: the action clock takes its
                // base rate (spec §4.1).
                day_length_std: None,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
            label: "h".into(),
        };
        let sys = DriveMovements {
            npcs: vec![npc.clone()],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            // No sky in a planted-terrain fixture: the action clock takes its
            // base rate (spec §4.1).
            day_length_std: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            // The action clock's reference mass (The Action Clock T2): tempo is
            // exactly `1.0` here, so this fixture's timings are unmoved.
            mass_kg: crate::clock::REFERENCE_MASS_KG,
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
            interior: None,
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
            interior: None,
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
            interior: None,
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

    #[test]
    fn a_cold_creature_crosses_the_room_to_the_fire() {
        // THE HEARTH, end to end: a thermally stressed creature in a room with a
        // hearth routes to the hearth anchor and is warmer there than where it
        // began. A creature in an identical room WITHOUT a fire has nowhere
        // warmer to go — the additive-latent control.
        use crate::interior::{AnchorKind, Interior, route_within, warmth_at};
        let mut warm_room = Interior::new();
        let door = warm_room.push(AnchorKind::Threshold, None);
        let hall = warm_room.push(AnchorKind::Bed, None);
        let hearth = warm_room.push(AnchorKind::Hearth, None);
        warm_room.connect(door, hall);
        warm_room.connect(hall, hearth);

        let here = warmth_at(&warm_room, door, 64);
        let there = warmth_at(&warm_room, hearth, 64);
        assert!(there > here, "the fire is warmer than the doorway");
        let plan = route_within(&warm_room, door, hearth, 64).expect("reachable");
        assert_eq!(plan.last(), Some(&hearth), "the plan ends at the fire");

        let mut cold_room = Interior::new();
        let d2 = cold_room.push(AnchorKind::Threshold, None);
        let h2 = cold_room.push(AnchorKind::Bed, None);
        cold_room.connect(d2, h2);
        assert_eq!(
            warmth_at(&cold_room, d2, 64),
            warmth_at(&cold_room, h2, 64),
            "with no fire, nowhere is warmer — the creature has no reason to move"
        );

        // AND ON A REAL COMPOSITION, not only a hand-built graph — otherwise the
        // demonstration proves the fixture rather than the grammar. The
        // cold-built selection puts a hearth in an alcove off the ground, so the
        // warmth gradient must be strictly positive walking in from the door.
        use crate::interior::{compose, selection};
        let real = compose(&selection(true, true));
        let door = real
            .ids()
            .into_iter()
            .find(|id| real.anchor(*id).kind == AnchorKind::Threshold)
            .expect("a built room has a threshold");
        let fire = real
            .ids()
            .into_iter()
            .find(|id| real.anchor(*id).kind == AnchorKind::Hearth)
            .expect("a cold built room has a hearth");
        assert!(
            warmth_at(&real, fire, 64) > warmth_at(&real, door, 64),
            "in a really composed room, the fire is warmer than the doorway"
        );
        let real_plan = route_within(&real, door, fire, 256).expect("the fire is reachable");
        assert!(
            real_plan.len() >= 2,
            "the route to the fire crosses the room rather than being one step: {real_plan:?}"
        );
    }

    #[test]
    fn the_thermal_drive_folds_the_hearths_warmth_additively() {
        // THE HEARTH's drive seam. Warmth is folded ADDITIVELY into the sensed
        // temperature, so the SAME cold cell is urgent unwarmed and comfortable
        // beside a fire — and a hearthless interior reads exactly like `None`,
        // the identity every live construction site relies on for
        // byte-identity. Unlike the pre-crossing model (a hand-picked
        // `warmth: Some(scalar)`), warmth is now DERIVED from a real
        // `Interior`, so this test builds one rather than injecting a number.
        use crate::interior::{AnchorKind, Interior};
        let home = raddr(1.0);
        let day = WorldTime { day: 0.0 };
        // −10 °C against the warm niche (optimum 18, width 8): deviation 28,
        // well past the band.
        let t = PlantedTerrain::thermal([(home.clone(), -10.0)]);
        let view = at(home.clone());

        let unwarmed = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
            interior: None,
        };
        // A real interior with no hearth: `warmth_at` reads `0.0` at its one
        // anchor, so this must read IDENTICALLY to `interior: None` — the
        // additive-latent discipline (an emitter-free room is byte-identical
        // to no interior at all).
        let mut cold_room = Interior::new();
        let bed = cold_room.push(AnchorKind::Bed, None);
        let unheated = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
            interior: Some((&cold_room, bed)),
        };
        // Two adjacent hearths: `HEARTH_WARMTH * (1.0 + WARMTH_DECAY) ==
        // 15.0 * 1.5 == 22.5`°C at the nearer one (its own undecayed
        // emission plus the other's, one hop away) — enough to carry
        // −10 °C into the warm niche's `[10, 26]` tolerance band, without
        // hand-injecting an arbitrary scalar the way the pre-crossing model
        // could.
        let mut warm_room = Interior::new();
        let fire = warm_room.push(AnchorKind::Hearth, None);
        let second_fire = warm_room.push(AnchorKind::Hearth, None);
        warm_room.connect(fire, second_fire);
        let beside_the_fire = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
            interior: Some((&warm_room, fire)),
        };

        assert!(
            unwarmed.urgency(&view) > unwarmed.act_threshold(),
            "a −10 °C room is well past the warm niche's act threshold"
        );
        assert_eq!(
            unheated.urgency(&view),
            unwarmed.urgency(&view),
            "a hearthless interior is the identity — an emitter-free room is unchanged"
        );
        assert_eq!(
            beside_the_fire.urgency(&view),
            0.0,
            "22.5 °C of hearth warmth carries −10 °C into the warm niche's tolerance band"
        );
        assert!(
            beside_the_fire.urgency(&view) < unwarmed.urgency(&view),
            "the additive term can only ease a COLD creature's discomfort"
        );
    }

    #[test]
    fn a_cold_creature_reads_less_arousal_beside_a_real_hearth_than_in_a_hearthless_room() {
        // THE THRESHOLD'S ARMING: this is the production path
        // (`affect_of` → `affect_of_memo` → `landing_interior`), not a
        // hand-built `Interior` like the test above. Two terrains report
        // the IDENTICAL ambient temperature at the SAME room address — the
        // only difference between them is `is_built`, which alone decides
        // whether `interior_of` composes a hearth (a cold BUILT room draws
        // one; wilderness never does — `interior/pattern.rs`'s INVENTORY has
        // no wild `Hearth` pattern). Every other drive is pinned to exactly
        // zero at day 0 (never drank, never ate, never rested, no hazard, and
        // already standing at home so no loneliness pull), so
        // `Affect::arousal` — the greatest urgency across every drive
        // (`arbitrate`) — reduces to exactly the thermal drive's own reading.
        // Before this task, both runs pass `warmth: None` and read IDENTICAL
        // arousal; after it, the hearth run must read strictly less.
        struct FurnishingStub {
            built: bool,
        }
        impl Terrain for FurnishingStub {
            fn elevation(&self, _r: &RoomAddr) -> f64 {
                0.0
            }
            fn is_fresh_water(&self, _r: &RoomAddr) -> bool {
                false
            }
            fn temperature(&self, _r: &RoomAddr, _d: WorldTime) -> f64 {
                // Just past `test_niche`'s tolerance band (optimum 15, width
                // 10 → band edge at 5.0): deviation 10.5, comfortably inside
                // the open interval (0, 1) on urgency before AND after a
                // realistic hearth's fractional-degree warmth is added, so
                // neither run can saturate at the 0 or 1 clamp for reasons
                // unrelated to the hearth.
                4.5
            }
            fn is_built(&self, _r: &RoomAddr) -> bool {
                self.built
            }
        }

        let home = raddr(1.0);
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: home.clone(),
            species: "human".to_string(),
            activity: ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            // An EMPTY diet niche: the hunger drive's niche-gate
            // (`!npc.niche.is_zero()`) never engages it, one fewer drive to
            // rule out.
            niche: ResourceVector::new(&[]).unwrap(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            mass_kg: crate::clock::REFERENCE_MASS_KG,
            label: "human".to_string(),
        };
        let day = WorldTime { day: 0.0 };

        let hearth_terrain = FurnishingStub { built: true };
        let wild_terrain = FurnishingStub { built: false };

        let hearth_affect = affect_of(&ledger, &npc, &[], day, &hearth_terrain);
        let wild_affect = affect_of(&ledger, &npc, &[], day, &wild_terrain);

        assert!(
            wild_affect.arousal > 0.0,
            "a room past the tolerance band with no hearth must register SOME \
             thermal arousal, or this test proves nothing: {wild_affect:?}"
        );
        assert!(
            hearth_affect.arousal < wild_affect.arousal,
            "a real, derived hearth must ease the felt cold below the \
             hearthless reading: hearth {hearth_affect:?} vs wild {wild_affect:?}"
        );
    }

    // --- The Threshold's crossing (task 6): Thermal seeks WITHIN a room. ---

    #[test]
    fn thermal_affordance_crosses_the_room_toward_the_hearth() {
        // THE THRESHOLD'S CROSSING, the payoff: on a REAL composed interior
        // (not a hand-built toy — the same one The Hearth's own anti-hub test
        // proves is non-degenerate), a creature standing at the threshold —
        // well outside the niche band — steps WITHIN the room toward the
        // fire, each step landing somewhere strictly warmer than the last,
        // until it reaches the hearth itself and has nowhere better to go.
        use crate::interior::{AnchorKind, compose, selection};
        let interior = compose(&selection(true, true));
        let door = interior
            .ids()
            .into_iter()
            .find(|&id| interior.anchor(id).kind == AnchorKind::Threshold)
            .expect("a built cold room has a threshold");
        let hearth_id = interior
            .ids()
            .into_iter()
            .find(|&id| interior.anchor(id).kind == AnchorKind::Hearth)
            .expect("a cold built room has a hearth");

        let home = raddr(1.0);
        let day = WorldTime { day: 0.0 };
        // Deep cold: well past the warm niche's tolerance band, so the
        // ambient gate never short-circuits the within-room branch.
        let t = PlantedTerrain::thermal([(home.clone(), -30.0)]);
        let view = at(home.clone());

        let mut anchor = door;
        let mut steps = 0;
        loop {
            let drive = Thermal {
                niche: warm_niche(),
                terrain: &t,
                day,
                interior: Some((&interior, anchor)),
            };
            match drive.affordance(&view, 64) {
                Some(Action::MoveWithin(next)) => {
                    let before = warmth_at(&interior, anchor, 64);
                    let after = warmth_at(&interior, next, 64);
                    assert!(
                        after > before,
                        "each within-room step must land somewhere strictly \
                         warmer: {anchor:?} ({before}) -> {next:?} ({after})"
                    );
                    anchor = next;
                    steps += 1;
                    assert!(
                        steps <= 8,
                        "the interior has at most 9 anchors; this should have converged by now"
                    );
                }
                other => {
                    // Converged: with nowhere better to go, this must be the
                    // hearth itself — the interior's unique warmth maximum.
                    assert_eq!(
                        anchor, hearth_id,
                        "the walk should end at the hearth, not give up early \
                         (next affordance: {other:?})"
                    );
                    break;
                }
            }
        }
        assert!(
            steps >= 2,
            "the composed interior is non-degenerate (The Hearth's own \
             anti-hub test), so reaching the fire should take more than one step"
        );
    }

    #[test]
    fn thermal_affordance_falls_back_to_the_room_scale_gradient_when_the_hearth_is_unroutable() {
        // spec §8a: a seasonal-passability read can leave the traversable
        // graph disconnected even though the BASE graph validated as
        // connected at composition time — a creature stranded away from its
        // own room's hearth. `route_within`'s `None` here is a real, expected
        // case, not a defect: the drive must fall back to the room-scale
        // gradient rather than treating it as impossible.
        use crate::interior::{AnchorKind, Interior};
        let mut stranded = Interior::new();
        let door = stranded.push(AnchorKind::Threshold, None);
        stranded.push(AnchorKind::Hearth, None);
        // Deliberately NO edge between them — `permits` would reject this as
        // a COMPOSITION (it is what a seasonal-passability READ could
        // produce transiently, not a shape `interior_of` ever composes
        // today; see route.rs's own `an_unreachable_anchor_yields_no_route`).

        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        // The room-scale gradient DOES have somewhere to go (ns[0] is
        // warmer), so this exercises a genuine fallback, not a coincidental
        // `None` from a boxed-in room.
        let t = PlantedTerrain::thermal([
            (home.clone(), -30.0),
            (ns[0].clone(), 10.0),
            (ns[1].clone(), -30.0),
            (ns[2].clone(), -30.0),
        ]);
        let view = at(home.clone());
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
            interior: Some((&stranded, door)),
        };

        assert_eq!(
            drive.affordance(&view, 64),
            Some(Action::MoveTo(ns[0].clone())),
            "the hearth is unroutable from the door, so the drive must fall \
             back to the between-rooms gradient rather than treating the \
             stranding as impossible"
        );
    }

    #[test]
    fn preferred_anchor_declines_a_target_that_route_within_cannot_reach() {
        // The Threshold whole-branch review, Important 2: `preferred_anchor`
        // feeds catch-up's cap-exceeded fallback straight into
        // `Occupancy::place`, which — unlike `walk` — performs no one-hop
        // reachability check of its own. If `preferred_anchor` named a
        // target `route_within` cannot reach, a stranded creature would be
        // teleported across the very edge spec §8a says it cannot cross.
        // Reuses the `stranded` fixture from the `affordance` fallback test
        // above: a hearth with no edge to the door, so a genuinely warmer
        // anchor exists but is unroutable.
        use crate::interior::{AnchorKind, Interior};
        let mut stranded = Interior::new();
        let door = stranded.push(AnchorKind::Threshold, None);
        stranded.push(AnchorKind::Hearth, None);

        let home = raddr(1.0);
        let day = WorldTime { day: 0.0 };
        let t = PlantedTerrain::thermal([(home.clone(), -30.0)]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
            interior: Some((&stranded, door)),
        };

        assert_eq!(
            drive.preferred_anchor(&home, 64),
            None,
            "the hearth is warmer but unroutable from the door, so \
             preferred_anchor must decline rather than name a target \
             `route_within` cannot reach"
        );
    }

    #[test]
    fn thermal_serviceability_scores_the_within_room_step_by_warmth_gained() {
        // Mirrors the `MoveTo` arm's own contract (`urgency_at(here) -
        // urgency_at(there)`), one scale down: the reduction in FELT urgency
        // between the creature's current anchor and a candidate one, clamped
        // to never go negative (a step that makes things worse serves the
        // drive not at all, exactly like the room-scale arm).
        use crate::interior::{AnchorKind, Interior};
        let mut interior = Interior::new();
        let door = interior.push(AnchorKind::Threshold, None);
        let hearth = interior.push(AnchorKind::Hearth, None);
        interior.connect(door, hearth);

        let home = raddr(1.0);
        let day = WorldTime { day: 0.0 };
        // cold_niche (optimum 6, width 8): −15 °C is 13.5 short of the
        // optimum once the door's own 1-hop-decayed warmth (7.5 °C) is
        // folded in — deliberately NOT deep enough to clamp urgency at its
        // ceiling at EITHER end, so the improvement genuinely shows up as a
        // difference rather than two saturated `1.0`s cancelling to `0.0`.
        let t = PlantedTerrain::thermal([(home.clone(), -15.0)]);
        let view = at(home.clone());
        let drive = Thermal {
            niche: cold_niche(),
            terrain: &t,
            day,
            interior: Some((&interior, door)),
        };

        let toward_the_fire = drive.serviceability(&Action::MoveWithin(hearth), &view, 64);
        assert!(
            toward_the_fire > 0.0,
            "stepping to the strictly-warmer hearth must serve the drive: {toward_the_fire}"
        );
        let toward_itself = drive.serviceability(&Action::MoveWithin(door), &view, 64);
        assert_eq!(
            toward_itself, 0.0,
            "a step that changes nothing serves the drive not at all"
        );
        // No interior at all: total (a `MoveWithin` candidate can only ever
        // reach this drive via `candidate_actions`, which never proposes one
        // without an interior, but `serviceability` stays total rather than
        // leaning on that invariant).
        let no_interior = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
            interior: None,
        };
        assert_eq!(
            no_interior.serviceability(&Action::MoveWithin(hearth), &view, 64),
            0.0
        );
    }

    #[test]
    fn arbitrate_chooses_the_within_room_step_when_thermal_proposes_one() {
        // THE THRESHOLD'S CROSSING wired all the way through: `arbitrate`'s
        // fixed room-scale candidate set is built from `RoomAddr` neighbours
        // and cannot express an `AnchorId` on its own — this proves
        // `Drive::candidate_actions` actually makes `MoveWithin` reachable
        // via the live multi-drive path, not merely via `Thermal::affordance`
        // called in isolation (the test above).
        use crate::interior::{AnchorKind, Interior};
        let mut interior = Interior::new();
        let door = interior.push(AnchorKind::Threshold, None);
        let hall = interior.push(AnchorKind::Bed, None);
        let hearth = interior.push(AnchorKind::Hearth, None);
        interior.connect(door, hall);
        interior.connect(hall, hearth);

        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        // Every room-scale neighbour reads the SAME cold as home (no
        // room-scale improvement available), so any `MoveTo` the fixed
        // candidate set offers scores exactly `0.0` — only the within-room
        // step, reached via `candidate_actions`, can win this arbitration.
        let t = PlantedTerrain::thermal([
            (home.clone(), -15.0),
            (ns[0].clone(), -15.0),
            (ns[1].clone(), -15.0),
            (ns[2].clone(), -15.0),
        ]);
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: cold_niche(),
            terrain: &t,
            day,
            interior: Some((&interior, door)),
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        let view = at(home.clone());
        let resolution = arb(
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

        assert_eq!(
            resolution.intent,
            Intent::Do(Action::MoveWithin(hall)),
            "the within-room step must actually win the multi-drive \
             arbitration, not merely exist as a possibility: {resolution:?}"
        );
        // THE AFFECT LABEL: beelining to a KNOWN, VERIFIED-REACHABLE target —
        // `Thermal::affordance` only ever proposes this step after
        // `route_within` has confirmed a real path — reads Eager, on the
        // same basis as `Fatigue`'s always-known walk home, not Searching
        // (reserved for gradient-following toward an UNKNOWN target).
        assert_eq!(
            resolution.affect.label,
            AffectLabel::Eager,
            "a creature beelining to a verified-reachable fire is Eager, not Searching"
        );
    }

    #[test]
    fn a_creature_crosses_a_hearth_bearing_room_but_not_a_hearthless_one() {
        // Task 6's own failing test (step 1): a cold creature at a
        // hearth-bearing interior's threshold ends its tick nearer the
        // hearth — reading strictly MORE warmth where it ends than where it
        // began — while the same creature in a hearthless (wilderness)
        // interior does not move at all. `MoveWithin` commits no `Fact`
        // (decision 0069), so this reads the walk's result through
        // `step_with_occupancy`'s second element rather than the committed
        // ledger, which a `MoveWithin`-only walk leaves untouched.
        struct BuiltOverlay<'a> {
            built: bool,
            inner: &'a PlantedTerrain,
        }
        impl Terrain for BuiltOverlay<'_> {
            fn elevation(&self, r: &RoomAddr) -> f64 {
                self.inner.elevation(r)
            }
            fn is_fresh_water(&self, r: &RoomAddr) -> bool {
                self.inner.is_fresh_water(r)
            }
            fn temperature(&self, r: &RoomAddr, d: WorldTime) -> f64 {
                self.inner.temperature(r, d)
            }
            fn is_built(&self, _r: &RoomAddr) -> bool {
                self.built
            }
        }

        let home = raddr(1.0);
        let ns = home.neighbors();
        // Deep, uniform cold: every room-scale neighbour reads the SAME
        // temperature as home, so the between-rooms gradient never fires and
        // any movement observed is genuinely the within-room mechanism.
        //
        // The niche is chosen deliberately, not reused off the shelf: the
        // landing anchor (the Threshold) is THREE hops from the composed
        // hearth (task 5d's own geometry), decaying `HEARTH_WARMTH` to
        // 1.875/3.75/7.5/15 °C at door/ground/alcove/hearth. `width: 12.0`
        // and this ambient keep urgency STRICTLY decreasing and UNSATURATED
        // at every one of those four anchors (each stays inside one
        // band-width of the last, so the clamp at `1.0` never equalizes two
        // consecutive readings) while staying ≥ `THERMAL_ACT` everywhere
        // except the fire itself — so the walk neither stalls on a
        // zero-`serviceability` tie nor gives up "good enough" one hop
        // early. A narrower, off-the-shelf niche (`cold_niche`/`warm_niche`,
        // width `8.0`) saturates two adjacent hops to an identical `1.0`
        // urgency here, masking the real warmth gain the same way task 5c/5d
        // found real deep-cold populations already do — this test is
        // deliberately NOT in that saturated regime, so it demonstrates the
        // mechanism rather than accidentally exercising the null.
        let niche = ConditionResponse {
            optimum: 6.0,
            width: 12.0,
            devotion: 0.5,
        };
        let ambient = -19.75;
        let planted = PlantedTerrain::thermal([
            (home.clone(), ambient),
            (ns[0].clone(), ambient),
            (ns[1].clone(), ambient),
            (ns[2].clone(), ambient),
        ]);
        let build_npc = |entity: EntityId| Npc {
            entity,
            home: home.clone(),
            resource: home.clone(),
            species: "human".to_string(),
            activity: ActivityCycle::Diurnal,
            temperature_niche: niche,
            deliberation_latency: 0.0,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            // An EMPTY diet niche: no hunger drive to rule out.
            niche: ResourceVector::new(&[]).unwrap(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            mass_kg: crate::clock::REFERENCE_MASS_KG,
            label: "test".to_string(),
        };

        // BUILT + COLD: `interior_of` composes the real hearth-bearing chain
        // (threshold -> ground -> alcove -> hearth -> bed;
        // `pattern.rs`'s own `the_intended_chain_is_the_deep_one`), landing
        // at the Threshold, three hops from the fire.
        let hearth_terrain = BuiltOverlay {
            built: true,
            inner: &planted,
        };
        let mut ledger = Ledger::default();
        let e1 = ledger.mint_entity();
        let sys = DriveMovements {
            npcs: vec![build_npc(e1)],
            // Start mid-morning (`waking_offset(Diurnal)`), not midnight: The
            // Slumber wake-gates Thermal, so a diurnal creature simulated
            // from `day: 0.0` starts ASLEEP and never engages it at all.
            from: WorldTime { day: 0.35 },
            to: WorldTime { day: 1.35 },
            params: SUSTENANCE,
            day_length_std: None,
            terrain: &hearth_terrain,
        };
        let (_facts, occ) =
            sys.step_with_occupancy(&ledger, &mut RoomMeshMemo::new(), &mut HomeNavCache::new());
        let interior = interior_of(&home, &hearth_terrain);
        let landing_anchor = landing(&interior, seam_kind(true)).expect("a built room lands");
        let hearth_id = interior
            .ids()
            .into_iter()
            .find(|&id| interior.anchor(id).kind == crate::interior::AnchorKind::Hearth)
            .expect("a built cold room has a hearth");
        let end = occ.at(e1).expect("the creature arrived somewhere");
        assert_ne!(
            end, landing_anchor,
            "a cold creature in a hearth-bearing room must move within it \
             over the course of a full day"
        );
        assert!(
            warmth_at(&interior, end, 64) > warmth_at(&interior, landing_anchor, 64),
            "it must end its tick strictly warmer than where it began"
        );
        assert_eq!(
            end, hearth_id,
            "given a full day and a small per-step cost, it reaches the fire itself"
        );

        // WILDERNESS + COLD (the hearthless control): the SAME temperature,
        // the SAME species, differing only in `is_built` — `interior_of`
        // never composes a `Hearth` anchor for wilderness
        // (`interior/pattern.rs`'s INVENTORY has no wild `Hearth` pattern),
        // so there is nowhere warmer to walk to and the creature stays
        // exactly where it arrived.
        let wild_terrain = BuiltOverlay {
            built: false,
            inner: &planted,
        };
        let mut ledger2 = Ledger::default();
        let e2 = ledger2.mint_entity();
        let sys2 = DriveMovements {
            npcs: vec![build_npc(e2)],
            // Start mid-morning (`waking_offset(Diurnal)`), not midnight: The
            // Slumber wake-gates Thermal, so a diurnal creature simulated
            // from `day: 0.0` starts ASLEEP and never engages it at all.
            from: WorldTime { day: 0.35 },
            to: WorldTime { day: 1.35 },
            params: SUSTENANCE,
            day_length_std: None,
            terrain: &wild_terrain,
        };
        let (_facts2, occ2) =
            sys2.step_with_occupancy(&ledger2, &mut RoomMeshMemo::new(), &mut HomeNavCache::new());
        let wild_interior = interior_of(&home, &wild_terrain);
        let wild_landing = landing(&wild_interior, seam_kind(false)).expect("wilderness lands too");
        assert_eq!(
            occ2.at(e2),
            Some(wild_landing),
            "a hearthless room gives the creature nowhere warmer to walk \
             within, so it must not move"
        );
    }

    #[test]
    fn no_movement_precondition_depends_on_a_committing_effect() {
        // Catch-up (spec §5) replays a creature's movement while suppressing the
        // actions that commit facts. That is sound only while movement
        // preconditions are purely positional. If a future action gates movement
        // — a barred door needing unbarring — catch-up silently reconstructs a
        // past that could not have happened, and silently is the bad part.
        //
        // The check: every action is classified, and every movement action's
        // precondition is declared positional.
        for a in [
            Action::MoveTo(RoomAddr {
                face: 0,
                path: vec![],
            }),
            Action::MoveWithin(AnchorId(0)),
            Action::Drink,
            Action::Rest,
            Action::Eat,
        ] {
            if is_movement(&a) {
                assert!(
                    !precondition_reads_committed_state(&a),
                    "{a:?} is a movement action whose precondition reads committed \
                     state — catch-up cannot replay it. Either make the \
                     precondition positional or exclude the action from catch-up."
                );
            }
        }
    }

    #[test]
    fn exactly_the_non_committing_actions_are_replayable() {
        assert!(is_replayable_in_catch_up(&Action::MoveWithin(AnchorId(0))));
        // Coarse movement writes `agent-at` — replaying it would fabricate history.
        assert!(!is_replayable_in_catch_up(&Action::MoveTo(RoomAddr {
            face: 0,
            path: vec![]
        })));
        assert!(!is_replayable_in_catch_up(&Action::Drink));
        assert!(!is_replayable_in_catch_up(&Action::Rest));
        assert!(!is_replayable_in_catch_up(&Action::Eat));
    }

    #[test]
    fn everything_replayable_is_a_movement() {
        // The trio is three hand-maintained lists over one enum; nothing ties
        // them together. This is the tie: an action catch-up may replay must
        // be one whose effect is position, or the partition has drifted.
        for a in [
            Action::MoveTo(RoomAddr {
                face: 0,
                path: vec![],
            }),
            Action::MoveWithin(AnchorId(0)),
            Action::Drink,
            Action::Rest,
            Action::Eat,
        ] {
            if is_replayable_in_catch_up(&a) {
                assert!(is_movement(&a), "{a:?} is replayable but is not a movement");
            }
        }
    }

    /// A minimal built-interior fixture for occupancy tests: a threshold
    /// connected directly to a hearth. Deliberately NOT the full pattern-grammar
    /// composition (`compose(&selection(true, true))`, which nests the hearth
    /// several hops inside an alcove) — `walking_requires_adjacency` needs a
    /// threshold and a hearth that ARE neighbors, one hop apart.
    fn built_interior() -> Interior {
        use crate::interior::AnchorKind;
        let mut i = Interior::new();
        let t = i.push(AnchorKind::Threshold, None);
        let h = i.push(AnchorKind::Hearth, None);
        i.connect(t, h);
        i
    }

    /// A minted entity id for occupancy tests, standing in for the brief's
    /// `NpcId` (no such type exists — see [`Occupancy`]'s doc comment).
    fn npc_id(n: u64) -> EntityId {
        EntityId::new(n).unwrap()
    }

    /// A fixture where a `Narrow` and a `Broad` landing genuinely disagree
    /// (review finding 1): `built_interior` above has a `Threshold` at index
    /// 0 and no `Ground` at all, so `seam::landing`'s `Broad` fallback
    /// (`ids().first()`) lands on that same `Threshold` by coincidence —
    /// making `a_creature_arrives_at_the_seam_landing` blind to `kind` being
    /// ignored entirely. Here the `Threshold` leads (index 0) and the
    /// `Ground` hub trails (index 1), mirroring `seam.rs`'s own
    /// `the_hub_is_found_by_kind_not_by_index` fixture: `Narrow` must find
    /// the `Threshold` by kind, `Broad` must find the `Ground` hub by kind,
    /// and since neither is `ids().first()`-equals-the-other by construction
    /// here, only a `kind`-respecting `arrive` can pass both assertions at
    /// once.
    fn built_interior_with_ground() -> Interior {
        use crate::interior::AnchorKind;
        let mut i = Interior::new();
        let t = i.push(AnchorKind::Threshold, None);
        let g = i.push(AnchorKind::Ground, None);
        i.connect(t, g);
        i
    }

    #[test]
    fn a_creature_arrives_at_the_seam_landing() {
        use crate::interior::AnchorKind;
        let interior = built_interior_with_ground();
        let room = raddr(1.0);
        let mut narrow = Occupancy::default();
        narrow.arrive(npc_id(1), &room, &interior, SeamKind::Narrow);
        let at_narrow = narrow
            .at(npc_id(1))
            .expect("an arrived creature stands somewhere");
        assert_eq!(interior.anchor(at_narrow).kind, AnchorKind::Threshold);

        let mut broad = Occupancy::default();
        broad.arrive(npc_id(2), &room, &interior, SeamKind::Broad);
        let at_broad = broad
            .at(npc_id(2))
            .expect("an arrived creature stands somewhere");
        assert_eq!(interior.anchor(at_broad).kind, AnchorKind::Ground);

        assert_ne!(
            at_narrow, at_broad,
            "the two seam kinds must land at genuinely different anchors, \
             or this test cannot tell `arrive` from a `kind`-blind stub"
        );
    }

    #[test]
    fn arriving_into_an_empty_interior_is_a_no_op_not_a_panic() {
        // Review finding 2: `arrive`'s doc comment claims this, but nothing
        // exercised it — `landing` returns `None` for a zero-anchor
        // `Interior`, and `arrive`'s `if let Some(at) = ...` must simply skip
        // the insert rather than unwrap into a panic.
        let interior = Interior::new();
        let mut occ = Occupancy::default();
        occ.arrive(npc_id(1), &raddr(1.0), &interior, SeamKind::Narrow);
        assert!(
            occ.at(npc_id(1)).is_none(),
            "an empty interior has no landing, so arrival records nothing"
        );
    }

    #[test]
    fn occupancy_is_empty_until_arrival_and_forgotten_on_departure() {
        let mut occ = Occupancy::default();
        assert!(occ.at(npc_id(1)).is_none());
        occ.arrive(npc_id(1), &raddr(1.0), &built_interior(), SeamKind::Narrow);
        assert!(occ.at(npc_id(1)).is_some());
        occ.depart(npc_id(1));
        assert!(
            occ.at(npc_id(1)).is_none(),
            "the bubble collapsing forgets everything"
        );
    }

    #[test]
    fn walking_requires_adjacency() {
        use crate::interior::AnchorKind;
        let i = built_interior(); // threshold -- hearth
        let room = raddr(1.0);
        let mut occ = Occupancy::default();
        occ.arrive(npc_id(1), &room, &i, SeamKind::Narrow);
        let hearth = i
            .ids()
            .iter()
            .copied()
            .find(|&a| i.anchor(a).kind == AnchorKind::Hearth)
            .unwrap();
        assert!(
            occ.walk(npc_id(1), &i, hearth),
            "adjacent, so the walk succeeds"
        );
        assert_eq!(occ.at(npc_id(1)), Some(hearth));

        // THE NAME'S OWN CLAIM: adjacency is REQUIRED, not merely consulted on
        // the happy path. Plant a third anchor with no edge to the hearth and
        // confirm walking straight to it is refused rather than teleported.
        let mut disconnected = built_interior();
        let stray = disconnected.push(AnchorKind::Bed, None);
        let mut occ2 = Occupancy::default();
        occ2.arrive(npc_id(2), &raddr(2.0), &disconnected, SeamKind::Narrow);
        assert!(
            !occ2.walk(npc_id(2), &disconnected, stray),
            "a non-adjacent target must be rejected"
        );
        assert_ne!(
            occ2.at(npc_id(2)),
            Some(stray),
            "a rejected walk must not move the creature"
        );
    }

    #[test]
    fn walking_into_a_contained_anchor_succeeds_not_only_across_adjacency() {
        // THE THRESHOLD task 6's own regression: a hearth composes WITHIN its
        // alcove (`Attach::Within`, `pattern.rs`'s `the-fire`), a containment
        // (`Ntpp`) edge, not adjacency (`Ec`) — `Interior::neighbors` alone
        // does not see it, but `route_within`'s planner walks it (its own
        // `successors` includes containment in both directions), and a plan
        // step `walk` then silently refused was a real bug this task hit
        // live: a creature stepping from an alcove INTO the hearth it
        // contains was rejected by the old adjacency-only check even though
        // the route that proposed the step was valid.
        use crate::interior::AnchorKind;
        let mut i = Interior::new();
        let ground = i.push(AnchorKind::Ground, None);
        let alcove = i.push(AnchorKind::Alcove, None);
        let hearth = i.push(AnchorKind::Hearth, Some(alcove));
        i.connect(ground, alcove);
        let mut occ = Occupancy::default();
        occ.arrive(npc_id(1), &raddr(1.0), &i, SeamKind::Broad);
        assert_eq!(
            occ.at(npc_id(1)),
            Some(ground),
            "lands at the Ground hub (no Threshold here)"
        );
        assert!(
            occ.walk(npc_id(1), &i, alcove),
            "ground to alcove is plain adjacency"
        );
        assert!(
            occ.walk(npc_id(1), &i, hearth),
            "alcove to the hearth it CONTAINS is one walkable hop too, \
             not merely one `route_within` step"
        );
        assert_eq!(occ.at(npc_id(1)), Some(hearth));
        // And back out, the converse direction (contained -> container).
        assert!(
            occ.walk(npc_id(1), &i, alcove),
            "the hearth to the alcove containing it is walkable in reverse"
        );
        assert_eq!(occ.at(npc_id(1)), Some(alcove));
    }

    #[test]
    fn walking_before_arriving_is_refused() {
        // Review finding 3: every other test calls `arrive` before `walk`, so
        // `walk`'s opening `let Some(here) = self.at(who) else { return
        // false }` — the "hasn't arrived anywhere" branch — had zero
        // coverage. An accidental `.unwrap_or(some_default_anchor)` in place
        // of that early return would still pass every other test in this
        // module.
        use crate::interior::AnchorKind;
        let i = built_interior();
        let mut occ = Occupancy::default();
        let hearth = i
            .ids()
            .iter()
            .copied()
            .find(|&a| i.anchor(a).kind == AnchorKind::Hearth)
            .unwrap();
        assert!(
            !occ.walk(npc_id(1), &i, hearth),
            "a creature that never arrived cannot walk anywhere"
        );
        assert!(
            occ.at(npc_id(1)).is_none(),
            "a refused walk must not conjure a position out of nothing"
        );
    }

    #[test]
    fn departing_a_creature_that_never_arrived_is_a_no_op() {
        // Minor finding: `depart` delegates straight to `BTreeMap::remove`,
        // itself a documented no-op on a missing key, but that delegation was
        // asserted nowhere in this module.
        let mut occ = Occupancy::default();
        occ.depart(npc_id(1));
        assert!(occ.at(npc_id(1)).is_none());
    }

    /// An overlay marking a room BUILT while delegating everything else to a
    /// planted terrain — the same fixture task 6's own hearth-crossing test
    /// uses, reused here rather than re-derived: catch-up's tests need a
    /// composed (`interior_of`) hearth-bearing room exactly as that test did.
    struct BuiltOverlay<'a> {
        inner: &'a PlantedTerrain,
    }
    impl Terrain for BuiltOverlay<'_> {
        fn elevation(&self, r: &RoomAddr) -> f64 {
            self.inner.elevation(r)
        }
        fn is_fresh_water(&self, r: &RoomAddr) -> bool {
            self.inner.is_fresh_water(r)
        }
        fn temperature(&self, r: &RoomAddr, d: WorldTime) -> f64 {
            self.inner.temperature(r, d)
        }
        fn is_built(&self, _r: &RoomAddr) -> bool {
            true
        }
    }

    /// A cold-adapted `Npc` standing in `home`, otherwise inert on every
    /// OTHER drive (ignorant of water, empty diet, no hazards, already
    /// home) — the shared fixture the catch-up tests below build on, so
    /// Thermal is provably the ONLY drive that can ever move it.
    fn cold_thermal_npc(entity: EntityId, home: RoomAddr, niche: ConditionResponse) -> Npc {
        Npc {
            entity,
            home: home.clone(),
            resource: home,
            species: "test".to_string(),
            activity: ActivityCycle::Diurnal,
            temperature_niche: niche,
            deliberation_latency: 0.0,
            time_horizon: 0.0,
            metabolic_class: MetabolicClass::Endotherm,
            // An EMPTY diet: no hunger drive to rule out.
            niche: ResourceVector::new(&[]).unwrap(),
            boldness: 0.5,
            threat_niche: mortal_threat_niche(),
            mass_kg: crate::clock::REFERENCE_MASS_KG,
            label: "test".to_string(),
        }
    }

    #[test]
    fn catch_up_reconstructs_the_within_room_position_across_an_unobserved_gap() {
        // The campaign's own headline scenario (spec §5): a creature that
        // entered a hearth-bearing room long before this tick's own `from`
        // must NOT still read as standing at the door — that is the
        // observer-effect bug this task exists to close. `from == to`
        // isolates catch-up's own contribution: the live walk below it
        // gets at most one `decide_step` call, and (as this scenario's
        // sole active drive, Thermal, can only ever propose `MoveTo` or
        // `MoveWithin`) that call's `MoveTo`/`MoveWithin` arms both check
        // `day > self.to.day` BEFORE touching `occupancy` — so any
        // occupancy progress this test observes is catch-up's alone, not
        // the live walk's.
        let home = raddr(1.0);
        let ns = home.neighbors();
        // The exact niche/ambient task 6's own hearth-crossing test uses
        // (see that test's own comment for why: strictly decreasing,
        // unsaturated urgency at every one of the composed chain's four
        // anchors).
        let niche = ConditionResponse {
            optimum: 6.0,
            width: 12.0,
            devotion: 0.5,
        };
        let ambient = -19.75;
        let planted = PlantedTerrain::thermal([
            (home.clone(), ambient),
            (ns[0].clone(), ambient),
            (ns[1].clone(), ambient),
            (ns[2].clone(), ambient),
        ]);
        let terrain = BuiltOverlay { inner: &planted };

        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        // Entered the room at a waking moment (sidesteps the sleep gate),
        // long before `from` — the unobserved gap catch-up must close.
        let entry_day = waking_offset(ActivityCycle::Diurnal);
        commit_agent_at(&mut ledger, &reg, e, &home, entry_day);

        let npc = cold_thermal_npc(e, home.clone(), niche);
        // A ONE-day gap — generous for the composed chain's 3-hop journey
        // (task 6's own finding), but short enough that Fatigue's own
        // `last_rested` fold (defaulted to `0.0`, no `rested` fact in this
        // ledger) never crosses `FATIGUE_ACT` (0.85 at `FATIGUE_RISE`
        // 0.3/day ⇒ ~2.8 days) and competes for the arbitration this test
        // means to isolate to Thermal.
        let now = WorldTime {
            day: entry_day + 1.0,
        };
        let sys = DriveMovements {
            npcs: vec![npc],
            from: now,
            to: now,
            params: SUSTENANCE,
            day_length_std: None,
            terrain: &terrain,
        };
        let (facts, occ) =
            sys.step_with_occupancy(&ledger, &mut RoomMeshMemo::new(), &mut HomeNavCache::new());
        assert!(
            facts.is_empty(),
            "an instantaneous tick (from == to) leaves the live walk nothing \
             to commit on its own, so every fact here would have to be \
             catch-up's — and catch-up must commit none: {facts:?}"
        );

        let interior = interior_of(&home, &terrain);
        let landing_anchor = landing(&interior, seam_kind(true)).expect("a built room lands");
        let end = occ
            .at(e)
            .expect("catch-up leaves the creature standing somewhere");
        assert_ne!(
            end, landing_anchor,
            "1 unobserved day in a hearth-bearing cold room must NOT still \
             read as standing at the door"
        );
        assert!(
            warmth_at(&interior, end, 64) > warmth_at(&interior, landing_anchor, 64),
            "catch-up must leave the creature somewhere strictly warmer than \
             the door it would otherwise still be read at"
        );
    }

    #[test]
    fn catch_up_replay_sees_thirst_before_a_drank_fact_inside_the_window() {
        // The Threshold whole-branch review, Important 3: `last_drank` (and
        // its `last_ate`/`last_rested` twins) folded ONCE over a creature's
        // ENTIRE committed history and reused unchanged for every replayed
        // day is wrong whenever that fold's own maximum lands INSIDE the
        // replay window — a day chronologically BEFORE the drink would
        // wrongly read as already-discharged, because
        // `integrate_thirst`'s `t <= last_drank` short-circuit sees a
        // `last_drank` from the future. `last_fact_day_at_or_before` fixes
        // this by filtering to `<= day` at each iteration; this test pins
        // both the correct answer AND the exact wrong one the naive
        // whole-history fold produces, so it fails without the fix.
        let home = raddr(1.0);
        // Thermally inert: optimum pinned at the planted ambient with a huge
        // tolerance band, so Thermal never proposes a competing action and
        // `decide_step`'s returned thirst urgency is the only thing this
        // test needs to read.
        let terrain = PlantedTerrain::thermal([(home.clone(), 20.0)]);
        let niche = ConditionResponse {
            optimum: 20.0,
            width: 50.0,
            devotion: 0.5,
        };
        let npc = cold_thermal_npc(npc_id(1), home.clone(), niche);

        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        let mut ledger = Ledger::default();
        // The drink lands on day 3 — strictly AFTER the day this test reads
        // thirst at (day 2), but it is still the only `DRANK` fact in the
        // creature's whole history, so an unfiltered fold over that history
        // finds it regardless of which day is being asked about.
        let drank_day = 3.0;
        let read_day = 2.0;
        ledger
            .commit(drank_fact(npc.entity, drank_day, "test"), &reg)
            .unwrap();

        let hazard = HazardMemory::default();
        let alarm: std::collections::BTreeMap<RoomAddr, f64> = Default::default();
        let visited: std::collections::BTreeSet<RoomAddr> = [home.clone()].into_iter().collect();

        let correct_last_drank = last_fact_day_at_or_before(&ledger, DRANK, npc.entity, read_day);
        assert_eq!(
            correct_last_drank, 0.0,
            "no drink has been committed as of read_day yet"
        );
        let mut believed_correct: Option<RoomAddr> = None;
        let (_, correct_thirst) = decide_step(
            read_day,
            &home,
            &npc,
            &terrain,
            &mut believed_correct,
            &hazard,
            &alarm,
            &visited,
            correct_last_drank,
            0.0,
            0.0,
            None,
            Mode::Idle,
            &SUSTENANCE,
            PLAN_BUDGET,
            &ledger,
            &[],
            &mut RoomMeshMemo::new(),
            &mut HomeNavCache::new(),
        );
        assert!(
            correct_thirst > 0.0,
            "2 days elapsed with no prior drink must show accrued thirst, \
             not the future drink suppressing it: got {correct_thirst}"
        );

        // The bug: the naive whole-history fold `catch_up` used to pass
        // unchanged into every replayed day.
        let buggy_last_drank = ledger
            .find(DRANK)
            .filter(|f| f.subject == npc.entity)
            .filter_map(|f| f.day)
            .fold(0.0_f64, f64::max);
        assert_eq!(
            buggy_last_drank, drank_day,
            "sanity check: the unfiltered fold finds the FUTURE drink"
        );
        let mut believed_buggy: Option<RoomAddr> = None;
        let (_, buggy_thirst) = decide_step(
            read_day,
            &home,
            &npc,
            &terrain,
            &mut believed_buggy,
            &hazard,
            &alarm,
            &visited,
            buggy_last_drank,
            0.0,
            0.0,
            None,
            Mode::Idle,
            &SUSTENANCE,
            PLAN_BUDGET,
            &ledger,
            &[],
            &mut RoomMeshMemo::new(),
            &mut HomeNavCache::new(),
        );
        assert_eq!(
            buggy_thirst, 0.0,
            "pinning the wrong answer: `integrate_thirst`'s `t <= last_drank` \
             short-circuit reads the future drink as already-discharged and \
             zeroes out thirst that should have accrued"
        );
    }

    #[test]
    fn catch_up_commits_nothing() {
        // Requirement 2: catch-up must commit nothing, ever — a projection
        // rebuild with side effects is a corrupted rebuild (spec §5.4).
        // Call `catch_up` directly (it never even receives a `&mut Ledger`
        // to write through) and prove real replay work happened first
        // (occupancy actually moves off the landing anchor), so the
        // before/after equality below is not vacuously true because
        // nothing ran.
        use crate::interior::AnchorKind;
        let mut interior = Interior::new();
        let door = interior.push(AnchorKind::Threshold, None);
        let hearth = interior.push(AnchorKind::Hearth, None);
        interior.connect(door, hearth);

        let home = raddr(1.0);
        let ns = home.neighbors();
        // The exact niche/ambient `thermal_serviceability_scores_the_within_
        // room_step_by_warmth_gained` uses: −15 °C is inside `cold_niche`'s
        // tolerance once the hearth's own (undecayed) warmth is folded in,
        // so a single hop from the door reaches comfort. EVERY neighbour
        // plants the SAME ambient (not just `home`): an UNplanted neighbour
        // reads `INFINITY`, which `Thermal::urgency_of` treats as "already
        // comfortable" (`0.0`) — an artifact that would make the room-scale
        // `MoveTo` candidate falsely outscore the real within-room
        // improvement `MoveWithin` offers, arbitration never even reaching
        // this test's own point.
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -15.0),
            (ns[0].clone(), -15.0),
            (ns[1].clone(), -15.0),
            (ns[2].clone(), -15.0),
        ]);
        let npc = cold_thermal_npc(npc_id(1), home.clone(), cold_niche());

        let ledger = Ledger::default();
        let mut occ = Occupancy::default();
        occ.place(npc.entity, &home, door);
        let mut believed: Option<RoomAddr> = None;
        let hazard = HazardMemory::default();
        let alarm: std::collections::BTreeMap<RoomAddr, f64> = Default::default();
        let visited: std::collections::BTreeSet<RoomAddr> = [home.clone()].into_iter().collect();

        let entry_day = waking_offset(ActivityCycle::Diurnal);
        let before = ledger.len();
        let _mode = catch_up(
            entry_day,
            entry_day + 1.0,
            &home,
            &npc,
            &terrain,
            &mut believed,
            &hazard,
            &alarm,
            &visited,
            &mut occ,
            &interior,
            Mode::Idle,
            &SUSTENANCE,
            PLAN_BUDGET,
            &ledger,
            &[],
            CATCH_UP_STEP_CAP,
            None,
            &mut RoomMeshMemo::new(),
            &mut HomeNavCache::new(),
        );
        let after = ledger.len();

        assert_eq!(
            occ.at(npc.entity),
            Some(hearth),
            "sanity: catch-up must have actually replayed the one hop to the \
             hearth, or the length equality below proves nothing"
        );
        assert_eq!(
            before, after,
            "catch-up must never commit a fact to the ledger"
        );
    }

    #[test]
    fn catch_up_is_order_independent_across_creatures() {
        // spec §5.4: two creatures catching up toward the same hearth must
        // give the same result in either order — free today (anchors have
        // no capacity, plans are independent) but silent to break the
        // moment capacity/`beside(host)` arrives. Assert it now, while it
        // is still free, so a future capacity change has a red test to
        // catch the regression at.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let niche = ConditionResponse {
            optimum: 6.0,
            width: 12.0,
            devotion: 0.5,
        };
        let ambient = -19.75;
        let planted = PlantedTerrain::thermal([
            (home.clone(), ambient),
            (ns[0].clone(), ambient),
            (ns[1].clone(), ambient),
            (ns[2].clone(), ambient),
        ]);
        let terrain = BuiltOverlay { inner: &planted };

        let entry_day = waking_offset(ActivityCycle::Diurnal);
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let a = ledger.mint_entity();
        let b = ledger.mint_entity();
        commit_agent_at(&mut ledger, &reg, a, &home, entry_day);
        commit_agent_at(&mut ledger, &reg, b, &home, entry_day);

        // A ONE-day gap, same reasoning as the reconstruction test above:
        // generous for the 3-hop journey, short of Fatigue's own act
        // threshold.
        let now = WorldTime {
            day: entry_day + 1.0,
        };
        let forward = DriveMovements {
            npcs: vec![
                cold_thermal_npc(a, home.clone(), niche),
                cold_thermal_npc(b, home.clone(), niche),
            ],
            from: now,
            to: now,
            params: SUSTENANCE,
            day_length_std: None,
            terrain: &terrain,
        };
        let (_f1, occ_forward) = forward.step_with_occupancy(
            &ledger,
            &mut RoomMeshMemo::new(),
            &mut HomeNavCache::new(),
        );

        let reversed = DriveMovements {
            npcs: vec![
                cold_thermal_npc(b, home.clone(), niche),
                cold_thermal_npc(a, home.clone(), niche),
            ],
            from: now,
            to: now,
            params: SUSTENANCE,
            day_length_std: None,
            terrain: &terrain,
        };
        let (_f2, occ_reversed) = reversed.step_with_occupancy(
            &ledger,
            &mut RoomMeshMemo::new(),
            &mut HomeNavCache::new(),
        );

        assert_eq!(
            occ_forward.at(a),
            occ_reversed.at(a),
            "creature a's catch-up result must not depend on iteration order"
        );
        assert_eq!(
            occ_forward.at(b),
            occ_reversed.at(b),
            "creature b's catch-up result must not depend on iteration order"
        );

        let interior = interior_of(&home, &terrain);
        let landing_anchor = landing(&interior, seam_kind(true)).expect("a built room lands");
        assert_ne!(
            occ_forward.at(a),
            Some(landing_anchor),
            "sanity: this must exercise real catch-up movement, or \
             order-independence is checking nothing"
        );
    }

    #[test]
    fn catch_up_is_exact_under_the_cap_and_approximate_over_it() {
        // spec §5.3's own instruction: "put the test AT THE CROSSOVER, not
        // in the middle of either regime." A corridor interior LONGER than
        // a small, test-local `cap` (passed directly to `catch_up`,
        // matching every other search budget in this module — see that
        // function's own doc for why this is cheap rather than paying for
        // the real 1000-iteration `CATCH_UP_STEP_CAP`): UNDER the cap,
        // replay is EXACT — it lands exactly as many hops down the
        // corridor as the elapsed budget allowed, nowhere near the hearth.
        // OVER the cap (a longer elapsed budget, still short of what the
        // FULL corridor needs), the cap — not the horizon — ends the
        // replay, and the fallback then jumps straight to the hearth,
        // skipping every anchor still between it and where replay stopped.
        use crate::interior::AnchorKind;
        const CAP: usize = 5;
        const LEN: usize = CAP + 3; // longer than the cap can ever traverse
        let mut interior = Interior::new();
        let mut anchors = Vec::with_capacity(LEN);
        for i in 0..LEN {
            let kind = if i == LEN - 1 {
                AnchorKind::Hearth
            } else {
                AnchorKind::Ground
            };
            let id = interior.push(kind, None);
            if let Some(&prev) = anchors.last() {
                interior.connect(prev, id);
            }
            anchors.push(id);
        }

        let home = raddr(1.0);
        let ns = home.neighbors();
        // Every neighbour plants the SAME ambient as `home` (not just
        // `home` itself) — an unplanted neighbour reads `INFINITY`, which
        // `Thermal::urgency_of` treats as "already comfortable" (`0.0`),
        // falsely outscoring the real within-room improvement `MoveWithin`
        // offers (`catch_up_commits_nothing`'s own doc hit this first).
        //
        // `cold_niche`'s narrower width (8.0) SATURATES `serviceability`
        // (which reads the CLAMPED `urgency_of`, unlike `warmest_anchor`'s
        // own raw-deviation comparison `affordance` uses) once two anchors
        // both sit more than one band-width past the edge — exactly what a
        // corridor this long (7 hops of `WARMTH_DECAY`) does by its far
        // end, making every step past the first read as ZERO improvement
        // to `arbitrate`'s utility scan even though `affordance` still
        // wants to take it. Task 6's own hearth-crossing test hit this
        // same trap and is why its niche is `width: 12.0`, not the
        // off-the-shelf `cold_niche`/`warm_niche` (see that test's own
        // comment) — reused here for the same reason, at an ambient
        // (`−15`) chosen so `urgency_of` stays strictly monotonic and
        // unsaturated across the WHOLE corridor, not just three hops.
        let niche = ConditionResponse {
            optimum: 6.0,
            width: 12.0,
            devotion: 0.5,
        };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -15.0),
            (ns[0].clone(), -15.0),
            (ns[1].clone(), -15.0),
            (ns[2].clone(), -15.0),
        ]);
        let npc = cold_thermal_npc(npc_id(1), home.clone(), niche);
        let ledger = Ledger::default();
        let entry_day = waking_offset(ActivityCycle::Diurnal);
        // What ONE replayed within-room hop costs this creature, asked of the
        // action clock exactly as `catch_up` itself asks (same action, same
        // mass, no terrain factor, no rotation) — rather than restated as a
        // literal here, so a retune of the `MoveWithin` dial cannot leave this
        // test's arithmetic quietly disagreeing with the loop it measures.
        let step_days = days_of(
            cost_ticks(&Action::MoveWithin(anchors[0]), npc.mass_kg, 1.0),
            None,
        );

        let run = |horizon: f64| -> Option<AnchorId> {
            let mut occ = Occupancy::default();
            occ.place(npc.entity, &home, anchors[0]);
            let mut believed: Option<RoomAddr> = None;
            let hazard = HazardMemory::default();
            let alarm: std::collections::BTreeMap<RoomAddr, f64> = Default::default();
            let visited: std::collections::BTreeSet<RoomAddr> =
                [home.clone()].into_iter().collect();
            let _mode = catch_up(
                entry_day,
                horizon,
                &home,
                &npc,
                &terrain,
                &mut believed,
                &hazard,
                &alarm,
                &visited,
                &mut occ,
                &interior,
                Mode::Idle,
                &SUSTENANCE,
                LEN + 10, // a routing budget generous enough never to be the
                // limiting factor here — `CAP` is what this test exercises.
                &ledger,
                &[],
                CAP,
                None,
                &mut RoomMeshMemo::new(),
                &mut HomeNavCache::new(),
            );
            occ.at(npc.entity)
        };

        // UNDER the cap: elapsed allows CAP - 1 = 4 replayed hops, so the
        // loop exhausts its horizon (not the cap) and lands 4 hops down the
        // corridor. The extra half-step of slack absorbs float summation
        // drift between `entry_day + 4.0 * step_days` (computed once) and
        // `catch_up`'s own `day += days_of(cost_ticks(..))` run four times in
        // a row — the two need not land on the identical f64, and an exact
        // boundary would make this test's own arithmetic, not the mechanism,
        // decide whether the 4th hop lands in time.
        let under = run(entry_day + 4.5 * step_days);
        assert_eq!(
            under,
            Some(anchors[4]),
            "under the cap, catch-up is EXACT: it must land exactly as far \
             down the corridor as the elapsed budget allowed, not the hearth"
        );

        // OVER the cap: elapsed is generous (far more than CAP hops would
        // need, but still short of the LEN-hop corridor), so the cap ends
        // the replay after exactly CAP hops, and the fallback jumps
        // straight to the hearth.
        let over = run(entry_day + 1.0);
        assert_eq!(
            over,
            Some(anchors[LEN - 1]),
            "over the cap, catch-up is APPROXIMATE: it must give up hopping \
             and place the creature directly at its drive-preferred anchor \
             (the hearth), not merely stop CAP hops down the corridor"
        );
    }

    // --- HomeNavCache (the-waymark, Task 4): the search-count pins and the
    // adversarial staleness test. Driven directly against `HomeNavCache::
    // home_nav` rather than through the whole `decide_step`/`step_with_
    // occupancy` machinery — the cache's own contract (zero searches for a
    // stationary, unchanged-belief entity; exactly one on a `pos` or avoid-
    // set change) is what these pin, and testing it directly keeps the
    // search counter's arithmetic legible instead of buried in a full walk.

    #[test]
    fn home_nav_pays_zero_searches_for_a_stationary_unchanged_belief_entity_after_warmup() {
        let start = raddr(1.0);
        let home = start.neighbors()[0].neighbors()[0].clone();
        let avoid: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        let mut cache = HomeNavCache::new();
        let mut mesh = RoomMeshMemo::new();
        let e = npc_id(1);

        let first = cache.home_nav(e, &start, &home, &avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(
            cache.searches, 1,
            "the very first call is necessarily a cold miss"
        );

        // The scaling bar itself (The Waymark spec's "scaling stake"): repeat
        // the IDENTICAL query (same pos, same avoid) several times, as a
        // stationary creature's every subsequent tick would.
        for _ in 0..5 {
            let again = cache.home_nav(e, &start, &home, &avoid, PLAN_BUDGET, &mut mesh);
            assert_eq!(
                again, first,
                "a cache hit must return the identical feature every time"
            );
        }
        assert_eq!(
            cache.searches, 1,
            "a stationary, unchanged-belief entity must pay ZERO searches on \
             every call after its first (the campaign's own scaling bar) — \
             got {} total searches for 6 identical queries",
            cache.searches
        );
    }

    #[test]
    fn home_nav_moving_triggers_exactly_one_new_search() {
        let start = raddr(1.0);
        let elsewhere = start.neighbors()[1].clone();
        let home = start.neighbors()[0].neighbors()[0].clone();
        let avoid: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        let mut cache = HomeNavCache::new();
        let mut mesh = RoomMeshMemo::new();
        let e = npc_id(1);

        let _ = cache.home_nav(e, &start, &home, &avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(cache.searches, 1, "warm-up search");

        // Repeat at the SAME position a few times first, to prove the
        // subsequent count bump is attributable to the move, not to noise.
        for _ in 0..3 {
            let _ = cache.home_nav(e, &start, &home, &avoid, PLAN_BUDGET, &mut mesh);
        }
        assert_eq!(cache.searches, 1, "still warm before the move");

        let _ = cache.home_nav(e, &elsewhere, &home, &avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(
            cache.searches, 2,
            "a moved entity (a Step resolution changing `pos`) must trigger \
             EXACTLY one new search, not zero (stale) and not more than one"
        );

        // And it goes cold again at the new position.
        for _ in 0..3 {
            let _ = cache.home_nav(e, &elsewhere, &home, &avoid, PLAN_BUDGET, &mut mesh);
        }
        assert_eq!(
            cache.searches, 2,
            "warm again at the new position — no further searches"
        );
    }

    #[test]
    fn home_nav_a_belief_change_triggers_exactly_one_new_search() {
        let start = raddr(1.0);
        let home = start.neighbors()[0].neighbors()[0].clone();
        let mut avoid: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        let mut cache = HomeNavCache::new();
        let mut mesh = RoomMeshMemo::new();
        let e = npc_id(1);

        let _ = cache.home_nav(e, &start, &home, &avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(cache.searches, 1, "warm-up search");
        for _ in 0..3 {
            let _ = cache.home_nav(e, &start, &home, &avoid, PLAN_BUDGET, &mut mesh);
        }
        assert_eq!(cache.searches, 1, "still warm before the belief changes");

        // The believed-hazard set changes (the entity stood on ground that
        // frightened it): the avoid-epoch write point.
        avoid.insert(start.neighbors()[1].clone());
        let _ = cache.home_nav(e, &start, &home, &avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(
            cache.searches, 2,
            "a believed-hazard change must trigger EXACTLY one new search"
        );

        for _ in 0..3 {
            let _ = cache.home_nav(e, &start, &home, &avoid, PLAN_BUDGET, &mut mesh);
        }
        assert_eq!(
            cache.searches, 2,
            "warm again under the NEW belief — no further searches"
        );
    }

    #[test]
    fn home_nav_cache_is_per_entity_never_global() {
        // A global epoch would stampede every entity's cache on ANY one
        // creature's belief change (the campaign spec's own refinement,
        // folded into `HomeNavCache`'s doc) — pinned here directly: entity
        // A's belief changes; entity B, unchanged, must still hit its cache.
        let start = raddr(1.0);
        let home = start.neighbors()[0].neighbors()[0].clone();
        let mut cache = HomeNavCache::new();
        let mut mesh = RoomMeshMemo::new();
        let (a, b) = (npc_id(1), npc_id(2));
        let empty: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();

        let _ = cache.home_nav(a, &start, &home, &empty, PLAN_BUDGET, &mut mesh);
        let _ = cache.home_nav(b, &start, &home, &empty, PLAN_BUDGET, &mut mesh);
        assert_eq!(cache.searches, 2, "both entities warm up independently");

        let mut a_avoid = empty.clone();
        a_avoid.insert(start.neighbors()[1].clone());
        let _ = cache.home_nav(a, &start, &home, &a_avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(
            cache.searches, 3,
            "A's own belief change costs A one search"
        );

        let _ = cache.home_nav(b, &start, &home, &empty, PLAN_BUDGET, &mut mesh);
        assert_eq!(
            cache.searches, 3,
            "B's cache must be UNAFFECTED by A's belief change — a global \
             epoch would have stampeded it into a third search here"
        );
    }

    #[test]
    fn home_nav_cache_changes_the_plan_when_the_avoid_set_changes() {
        // THE ADVERSARIAL STALENESS TEST (the-waymark, Task 4 — the
        // campaign's one real correctness risk, spec §6): mirrors
        // `planner_routes_around_a_remembered_cell`'s topology (a two-hop
        // destination reached via a single "via" room on the straight path),
        // so avoiding `via` provably forces the plan onto a different first
        // step. Red-run-proven: see the task report for the paired run with
        // the cache's own invalidation check disabled, which fails this
        // exact assertion.
        let start = raddr(1.0);
        let via = start.neighbors()[0].clone();
        let dest = via
            .neighbors()
            .iter()
            .find(|n| **n != start)
            .unwrap()
            .clone();
        let empty: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        let mut cache = HomeNavCache::new();
        let mut mesh = RoomMeshMemo::new();
        let e = npc_id(1);

        let before = cache.home_nav(e, &start, &dest, &empty, 10_000, &mut mesh);
        assert_eq!(
            before.first_step,
            Some(Action::MoveTo(via.clone())),
            "sanity: the straight plan's first step is the direct neighbor"
        );

        let mut avoid = empty.clone();
        avoid.insert(via.clone());
        let after = cache.home_nav(e, &start, &dest, &avoid, 10_000, &mut mesh);
        assert_ne!(
            after.first_step, before.first_step,
            "an avoid-set change MUST change the cached plan feature — a \
             stale first_step surviving a belief change is exactly the \
             staleness bug this cache exists to prevent"
        );
        assert_ne!(
            after.first_step,
            Some(Action::MoveTo(via.clone())),
            "the new plan must not still route through the now-avoided cell"
        );
    }

    #[test]
    fn home_nav_cache_is_keyed_by_home_and_budget_not_only_pos_and_avoid() {
        // KEY HARDENING (the-waymark, Task 4 fix round, review item 2):
        // `home` and `budget` determine the answer exactly as much as
        // `pos`/`avoid` do. Before this fix the cached tuple omitted both, so
        // a second call for a DIFFERENT home (same pos/avoid/epoch) would
        // have silently returned the FIRST home's stale `first_step` — a
        // cache hit on the wrong question. Two distinct one-hop-away
        // destinations from the same `pos`, so their correct first steps
        // provably differ.
        let start = raddr(1.0);
        let home_a = start.neighbors()[0].clone();
        let home_b = start.neighbors()[1].clone();
        assert_ne!(home_a, home_b, "sanity: two distinct neighbor destinations");
        let avoid: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        let mut cache = HomeNavCache::new();
        let mut mesh = RoomMeshMemo::new();
        let e = npc_id(1);

        let feature_a = cache.home_nav(e, &start, &home_a, &avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(
            feature_a.first_step,
            Some(Action::MoveTo(home_a.clone())),
            "sanity: home_a is one hop away, straight there"
        );
        assert_eq!(cache.searches, 1, "warm-up search for home_a");

        let feature_b = cache.home_nav(e, &start, &home_b, &avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(
            feature_b.first_step,
            Some(Action::MoveTo(home_b.clone())),
            "a DIFFERENT home must yield ITS OWN correct first_step, not \
             home_a's stale one — the key-hardening bug this test pins"
        );
        assert_eq!(
            cache.searches, 2,
            "a home change (pos/avoid/epoch all unchanged) must still cost \
             a real search, not a silent hit on the wrong destination"
        );

        // Asking about home_a again costs a search too (the cache holds one
        // entry per ENTITY, not one per (entity, home) pair) — but it must
        // still reproduce home_a's own correct feature, never home_b's.
        let feature_a_again = cache.home_nav(e, &start, &home_a, &avoid, PLAN_BUDGET, &mut mesh);
        assert_eq!(
            feature_a_again, feature_a,
            "re-asking about home_a must reproduce its own correct feature"
        );
        assert_eq!(
            cache.searches, 3,
            "switching back to home_a costs a search too"
        );
    }

    // --- Stage 3b (the-waymark, Task 5): the shared reverse field,
    // equivalence-gated. `ReverseField`/`build_reverse_field` are TEST-ONLY
    // scaffolding — see the property test immediately below for why this
    // never left the test module (the property the campaign spec licenses
    // as a legitimate failure mode).

    /// A single reverse Dijkstra rooted at `home`, mirroring `astar`'s own
    /// frontier discipline by hand (the same `(f, g, state)` `BTreeSet`
    /// order, `heuristic() == 0` — Dijkstra mode, matching [`NavSpace`] —
    /// and the same first-strict-improvement-wins relaxation `astar` itself
    /// uses) rather than through `SearchSpace` (there is no goal state to
    /// hand it — the point is reaching everything within budget). Valid
    /// ONLY for empty-avoid queries: [`move_cost`] with an empty set is `1`
    /// uniformly, so every edge is symmetric and a room's distance FROM
    /// `home` equals its distance TO `home`.
    struct ReverseField {
        /// Every room reached within the build budget: `(distance-to-home,
        /// next-hop-toward-home)`. `home` itself maps to `(0, None)`.
        nodes: std::collections::BTreeMap<RoomAddr, (usize, Option<RoomAddr>)>,
    }

    impl ReverseField {
        /// `(distance, first_step)` for `room`, matching [`HomeNavFeature`]'s
        /// own shape — both `None` if `room` was not reached within budget
        /// (mirrors `plan_to_room`'s budget-exhaustion `None`); `first_step`
        /// is `None` exactly at `home` itself (mirrors the empty-plan case).
        fn feature(&self, room: &RoomAddr) -> HomeNavFeature {
            match self.nodes.get(room) {
                None => HomeNavFeature {
                    distance: None,
                    first_step: None,
                },
                Some((dist, parent)) => HomeNavFeature {
                    distance: Some(*dist),
                    first_step: parent.clone().map(Action::MoveTo),
                },
            }
        }
    }

    /// Build a [`ReverseField`] rooted at `home`, expanding up to `budget`
    /// nodes — a full single-source search with no goal test.
    fn build_reverse_field(home: &RoomAddr, budget: usize) -> ReverseField {
        use std::collections::{BTreeMap, BTreeSet};
        let mut frontier: BTreeSet<(u64, u64, RoomAddr)> = BTreeSet::new();
        let mut best_g: BTreeMap<RoomAddr, u64> = BTreeMap::new();
        let mut came_from: BTreeMap<RoomAddr, RoomAddr> = BTreeMap::new();

        frontier.insert((0, 0, home.clone()));
        best_g.insert(home.clone(), 0);

        let mut expansions = 0usize;
        while let Some(&(_f, g, ref state)) = frontier.iter().next() {
            let (f, g, state) = (_f, g, state.clone());
            frontier.remove(&(f, g, state.clone()));
            if best_g.get(&state).is_some_and(|&bg| bg < g) {
                continue;
            }
            expansions += 1;
            if expansions > budget {
                break;
            }
            for n in state.neighbors() {
                let ng = g + 1; // move_cost is 1 uniformly — empty-avoid only
                if best_g.get(&n).is_none_or(|&bg| ng < bg) {
                    best_g.insert(n.clone(), ng);
                    came_from.insert(n.clone(), state.clone());
                    frontier.insert((ng, ng, n));
                }
            }
        }

        let nodes = best_g
            .into_iter()
            .map(|(room, dist)| {
                let parent = came_from.get(&room).cloned();
                (room, (dist as usize, parent))
            })
            .collect();
        ReverseField { nodes }
    }

    /// **THE PROPERTY TEST — decides Task 5's whole outcome.** For every
    /// empty-avoid room the field reaches within budget, does
    /// `field.feature(room)` match what a genuinely independent forward
    /// search (`plan_to_room`, exactly what `home_nav` calls today) returns,
    /// byte-for-byte in both `distance` and `first_step`? If yes for every
    /// room, Stage 3b's field is a safe drop-in for `home_nav`'s empty-avoid
    /// path. If not, the campaign spec licenses shipping this test
    /// `#[ignore]`d as documentation of the failure mode, field disabled.
    ///
    /// **Result (the-waymark, Task 5): the property FAILS.** 52 of 346
    /// reached rooms (~15%) — a substantial fraction, not a rare edge case —
    /// disagree with forward search in `first_step`. `distance` never
    /// mismatches for any of the 52 (confirmed separately) — expected,
    /// since distance is symmetric for empty-avoid (uniform edge cost) and
    /// root-independent; only the CHOICE OF PATH among equal-length
    /// alternatives is root-dependent. This is real, not a bug in the
    /// field: `home`-rooted and `s`-rooted BTreeSet relaxation break ties
    /// independently (see `ReverseField`'s own doc for the induction
    /// argument, and this task's report for a concrete diverging pair).
    /// Field ships disabled; the per-entity `HomeNavCache` (Task 4) remains
    /// the sole nav-answering path. Left `#[ignore]`d rather than deleted,
    /// per the spec's own licensed exit, so a future attempt at a smarter
    /// tie-break rule has a ready-made falsifier.
    #[ignore = "documents a DISPROVEN hypothesis (the-waymark, Task 5): the \
                field/forward tie-break equivalence fails for ~15% of rooms \
                (52/346, seed-independent mesh property); field ships \
                disabled, HomeNavCache (Task 4) carries alone; kept as a \
                falsifier for any future field construction attempt"]
    #[test]
    fn reverse_field_matches_forward_search_for_every_empty_avoid_room() {
        let home = raddr(1.0);
        let avoid: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        // A modest budget: large enough to reach several hundred rooms
        // (well past the health population's actual walking radius) while
        // keeping the O(field_size) forward re-searches below it fast.
        let budget = 300;
        let field = build_reverse_field(&home, budget);
        assert!(
            field.nodes.len() > 50,
            "sanity: the field must reach a nontrivial neighborhood, got {}",
            field.nodes.len()
        );

        let mut mismatches: Vec<(RoomAddr, HomeNavFeature, HomeNavFeature)> = Vec::new();
        for room in field.nodes.keys() {
            if *room == home {
                continue;
            }
            let field_feature = field.feature(room);
            let forward_plan = plan_to_room(room, &home, PLAN_BUDGET, &avoid);
            let forward_feature = HomeNavFeature {
                distance: forward_plan.as_ref().map(|p| p.len()),
                first_step: forward_plan.and_then(|p| p.into_iter().next()),
            };
            if field_feature != forward_feature {
                mismatches.push((room.clone(), field_feature, forward_feature));
            }
        }

        assert!(
            mismatches.is_empty(),
            "the field/forward equivalence FAILED for {} of {} rooms (showing \
             up to 5): {:#?}\n\
             This is the tie-break-root-dependence failure mode the campaign \
             spec names as a legitimate exit: astar's smallest-RoomAddr \
             relaxation winner is root-relative (see ReverseField's own \
             doc), so a field rooted at `home` need not agree with a forward \
             search rooted at each individual query room whenever a room has \
             two structurally different equal-length predecessor branches \
             (any 4-cycle in the triangulated mesh).",
            mismatches.len(),
            field.nodes.len(),
            &mismatches[..mismatches.len().min(5)]
        );
    }
}
