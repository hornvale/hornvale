//! The body type: what a creature is, independent of whether anything is
//! driving it. Moved out of `liveness` (The Hand, Task 7) once the possessed
//! agent and the derived NPC merged into one type (Tasks 1-6) — a possessed
//! body is one `Body` among the roster, not a separate kind.

use crate::liveness::ThreatNiche;
use hornvale_kernel::{ConditionResponse, EntityId, ResourceVector, RoomAddr};
use hornvale_species::{ActivityCycle, MetabolicClass};

/// A derived non-player agent: a minted entity, a home and a resource room,
/// its species, and that species' activity-cycle. Derived from the genesis
/// world, never stored (re-derivable).
/// type-audit: bare-ok(identifier-text: label), bare-ok(identifier-text: species), bare-ok(ratio: deliberation_latency), bare-ok(ratio: time_horizon), bare-ok(ratio: boldness), bare-ok(ratio: mass_kg)
#[derive(Clone, Debug)]
pub struct Body {
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
    /// The species' authored perception vector (the dragon-test slot) — the
    /// same value a possessed body carried before The Hand merged the types.
    pub perception: hornvale_species::PerceptionVector,
    /// The settlement this body was derived from, or `None` for a body not
    /// derived from a settlement — a wild creature (`derive_wild_npcs`), or a
    /// harness fabrication in a test/lab fixture. Session prose reads its
    /// name and population when present; the creature layer does not.
    pub village: Option<hornvale_settlement::VillageInfo>,
}
