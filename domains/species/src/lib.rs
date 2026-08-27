//! Species, tier 1: the authored body/mind/taxonomy component registries —
//! the universal biosphere component (mass, metabolic class, resource + climate
//! niche, potency), the closed three-dimension mind and society vectors (*The
//! Cloister* split the former six-dimension psychology vector), a closed
//! three-dimension perception vector, and each kind's family label. Kinds are
//! keyed by `KindId`; each component authors its own rows directly (the former
//! authored god-struct was dissolved in ECS c3). Species are data; the
//! social grammar stays code (spec §2). The MANIKIN is the reference vector:
//! scalars at the 0.5 midpoint, designated default enum variants; every
//! downstream modulation is the identity function at this vector. It is
//! nobody's — no `KindId`, no registry row — and a kind sitting on it does so
//! by authorship. The peopled speech data (articulation vector, lexicon,
//! family proto) is language-owned and lives in `hornvale-language`.
#![warn(missing_docs)]

use std::collections::BTreeMap;

use hornvale_kernel::{
    ANIMAL_PREY, AxisValence, Component, ComponentStore, ConceptDef, ConceptKind, ConceptRegistry,
    ConditionResponse, Correspondent, DETRITUS, EntityId, EnvironmentAxis, EnvironmentVector, Fact,
    Ledger, LedgerError, MARINE_FORAGE, MINERAL, Manifest, Mass, PHOTOSYNTHATE, PLANT_FORAGE,
    RegistryError, ResourceVector, UnitError, Value, Void, World, WorldTime,
};
// `perception_registry()` is keyed by `KindId`, so a caller resolving a
// species by name (worldgen's `observer_named`, campaign "The Beholding")
// needs the type nameable as `hornvale_species::KindId`, not just usable
// internally.
pub use hornvale_kernel::KindId;

mod allometry;
pub use allometry::{
    LifeHistory, age_at_maturity, basal_metabolic_rate_w, life_history, lifespan,
    reproductive_tempo,
};

/// Predicate: a species entity's name (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_NAME: &str = "species-name";
/// Body mass in kilograms — a level-agnostic trait predicate: the subject
/// may be a kind-representative entity or an instance (the instance fact is
/// the prototype-inheritance override). Non-functional: sim-mutable, the
/// latest fact wins (`Ledger::latest_value_of`).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_MASS_KG: &str = "species-mass-kg";
/// Magical potency override — level-agnostic, non-functional, latest-wins
/// (see `SPECIES_MASS_KG`).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_POTENCY: &str = "species-potency";
/// Predicate: how a species answers threat, flee 0 ↔ stand 1 (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const THREAT_RESPONSE: &str = "species-threat-response";
/// Predicate: how slowly a species decides (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const DELIBERATION_LATENCY: &str = "species-deliberation-latency";
/// Predicate: how wide a species draws "us" (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const IN_GROUP_RADIUS: &str = "species-in-group-radius";
/// Predicate: how far ahead a species plans (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const TIME_HORIZON: &str = "species-time-horizon";
/// Predicate: hierarchic or communal sociality (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const SOCIALITY_MODE: &str = "species-sociality-mode";
/// Predicate: what earns standing — rank, knowledge, generosity (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const STATUS_BASIS: &str = "species-status-basis";
/// Predicate: the species that peoples a settlement (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const PEOPLED_BY: &str = "peopled-by";
/// Predicate: a species' activity cycle — diurnal, nocturnal, crepuscular (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_ACTIVITY_CYCLE: &str = "species-activity-cycle";
/// Predicate: how well a species sees at night, 0-1 (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_NIGHT_VISION: &str = "species-night-vision";
/// Predicate: how much of a species' attention the sky claims, 0-1 (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_SKY_ATTENTION: &str = "species-sky-attention";
/// Predicate: a species' lip-rounding and jaw-closure degree, 0-1 (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_LABIALITY: &str = "species-labiality";
/// Predicate: a species' vowel-space size, 0-1 (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_VOWEL_SPACE: &str = "species-vowel-space";
/// Predicate: a species' voicing (voiced vs. voiceless), 0-1 (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_VOICING: &str = "species-voicing";
/// Predicate: a species' sibilance emphasis, 0-1 (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_SIBILANCE: &str = "species-sibilance";
/// Predicate: a species' voice-loudness range, 0-1 (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_VOICE_LOUDNESS: &str = "species-voice-loudness";
/// Predicate: a species' exotic manner — none, trill, click, ejective (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_EXOTIC_MANNER: &str = "species-exotic-manner";
/// Predicate: a species' tonal propensity, 0 atonal ↔ 1 fully tonal (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_TONALITY: &str = "species-tonality";

/// How a species organizes authority.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sociality {
    /// Ranked authority under a single head.
    Hierarchic,
    /// Collective authority, consensus-run.
    Communal,
}

/// What earns standing in a species' societies.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StatusBasis {
    /// Dominance and position.
    Rank,
    /// Craft, lore, and cunning.
    Knowledge,
    /// Provision and largesse.
    Generosity,
}

/// When a species is awake and watching.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ActivityCycle {
    /// Awake by day (the manikin's designated default schedule).
    Diurnal,
    /// Awake by night.
    Nocturnal,
    /// Awake at the boundaries (idle this campaign; authored now so a
    /// future species is a data change).
    Crepuscular,
}

/// How a creature organizes with its own kind — the universal social axis,
/// distinct from [`Sociality`] (a peopled society's *authority* shape).
/// Ordered by permanence of association. Only `Settled` builds settlements;
/// re-keying a "has a mind ⇒ is a people" proxy onto `Settled` is what lets a
/// solitary creature carry a mind without being a settling people.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SocialForm {
    /// Rooted; placed on the map, never agentified (autotrophs).
    Sessile,
    /// Lives and ranges alone (a dragon, a xorn).
    Solitary,
    /// Moves in herds or packs, forming no fixed place (an elk herd).
    Gregarious,
    /// Forms sedentary communities — the settling peoples.
    Settled,
}

impl SocialForm {
    /// Whether this form lives *socially* — in a group with its own kind, so a
    /// minded member has a society-mind (authority, status, an in-group). True
    /// for `Gregarious` (packs/herds) and `Settled` (communities); false for
    /// `Solitary` and `Sessile`. This is the sociality axis, deliberately
    /// distinct from settlement-forming (`Settled` alone): a nomadic band is
    /// social without being sedentary (decision 0068 refines 0067).
    /// type-audit: bare-ok(flag: return)
    pub const fn is_social(self) -> bool {
        matches!(self, Self::Gregarious | Self::Settled)
    }
}

/// The individual-mind vector (spec: The Cloister): the psychology every
/// minded kind carries, whether or not it belongs to a society. Scalars are
/// bare ratios in `[0, 1]` with 0.5 ≡ the manikin's neutral midpoint;
/// widening requires its own campaign.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MindVector {
    /// How this creature answers threat: flee 0 ↔ stand 1.
    pub threat_response: f64,
    /// How slowly decisions are made (banked; read by the vessel).
    pub deliberation_latency: f64,
    /// How far ahead works are planned: immediate 0 ↔ generational 1.
    pub time_horizon: f64,
}

impl MindVector {
    /// The manikin's mind: the neutral midpoint on every dimension.
    ///
    /// This is the model's reference vector, not any creature's psychology —
    /// no kind is obliged to sit here, and a kind that does, does so by
    /// authorship. See `SocietyVector::MANIKIN` for the full account.
    /// type-audit: bare-ok(ratio)
    pub const MANIKIN: Self = Self {
        threat_response: 0.5,
        deliberation_latency: 0.5,
        time_horizon: 0.5,
    };
}

/// The community-mind vector (spec: The Cloister): the psychology only a
/// society has, carried solely by `Settled` kinds. A `Solitary` creature
/// carries none; consumers needing a society reading for one resolve
/// [`SocietyVector::MANIKIN`]. `in_group_radius` is a bare ratio in `[0, 1]`.
///
/// **This is a grid/group instrument** (Douglas), adopted deliberately at the
/// owner's direction (The Tolerance, spec D6): `sociality` is *grid* (how
/// rule-bound a life is) and `in_group_radius` is *group* (how bounded "us"
/// is). The four biases — hierarchy, egalitarian/sect, individualist,
/// fatalist — each carry published predictions about cosmology, risk, and
/// stance toward outsiders, so those are DERIVED from the quadrant rather
/// than authored per people. Adding a people means placing it on two axes,
/// not inventing its culture.
///
/// **The adoption is documentary; no consumer reads a quadrant yet.** The
/// Tolerance names the frame and stops there. Wiring the quadrant into
/// behaviour — the obvious candidate being the raid gate, spec D5's third
/// term — was deliberately deferred: both axes are per-*people* constants, so
/// a quadrant term adds nothing to the between-settlement variance that
/// campaign was measuring, and shipping it would have been an unpreregistered
/// behavioural change with no measurement attached. The frame is recorded here
/// so the next campaign that wants it inherits the reading rather than
/// reinventing one.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SocietyVector {
    /// Authority shape.
    pub sociality: Sociality,
    /// What earns standing.
    pub status_basis: StatusBasis,
    /// How wide "us" is drawn: insular 0 ↔ expansive 1.
    pub in_group_radius: f64,
}

impl SocietyVector {
    /// The manikin's society: the reference reading a mixed consumer resolves
    /// for a `Solitary` kind that carries no society vector of its own.
    ///
    /// The manikin is a body that is nobody — the model's reference figure, in
    /// the lineage of the CIE standard observer and ICRP's "standard man". It
    /// is deliberately *not* a species: it has no `KindId`, no entry in any
    /// registry, no mass and no niche, so it can never be placed in a world
    /// and can never be a ghost.
    ///
    /// Note the asymmetry, which is real and not papered over: `0.5` is a
    /// principled **neutral midpoint** on a scalar, but `Sociality` and
    /// `StatusBasis` have no middle, so `Hierarchic` and `Rank` are a
    /// designated **default** rather than a neutral value.
    /// type-audit: bare-ok(ratio)
    pub const MANIKIN: Self = Self {
        sociality: Sociality::Hierarchic,
        status_basis: StatusBasis::Rank,
        in_group_radius: 0.5,
    };
}

/// The closed three-dimension perception vector (spec §4). Scalars are bare
/// ratios in `[0, 1]` with 0.5 ≡ the manikin's neutral midpoint; widening the
/// vector requires its own campaign. Every dimension is authored — nothing
/// drawn.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PerceptionVector {
    /// When this species observes.
    pub activity: ActivityCycle,
    /// Night-sky acuity: blind 0 ↔ owl-eyed 1.
    pub night_vision: f64,
    /// Celestial vs. terrestrial attention: earthbound 0 ↔ sky-rapt 1.
    pub sky_attention: f64,
}

impl PerceptionVector {
    /// The manikin's perception: the neutral midpoint on both scalars, and
    /// `Diurnal` as the designated default schedule.
    ///
    /// As with `SocietyVector::MANIKIN`, `activity` is a default rather than a
    /// neutral value — a schedule has no midpoint.
    /// type-audit: bare-ok(ratio)
    pub const MANIKIN: Self = Self {
        activity: ActivityCycle::Diurnal,
        night_vision: 0.5,
        sky_attention: 0.5,
    };
}

/// How widely a species spreads around its authored vectors.
///
/// **The authored vector is the MEAN, and this is the standard deviation of a
/// population around it.** That choice is a fiat, not a discovery, and it is
/// stated because leaving it unstated is precisely the frame bug The Manikin
/// removed one level up: a datum whose frame is implicit drifts in meaning as
/// the model grows.
///
/// One dispersion per vector, not per dimension. A per-dimension spread is a
/// refinement that should be argued from a measured need (spec §8).
///
/// `0.0` means every member is identical — the model's behaviour before this
/// campaign, and the value that must collapse H2's variance to zero.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Dispersion {
    /// Spread around [`MindVector`].
    pub mind: f64,
    /// Spread around [`SocietyVector`].
    pub society: f64,
    /// Spread around [`PerceptionVector`].
    pub perception: f64,
}

/// The draconic clade's night-sky acuity. Authored once for the whole clade
/// rather than per kind: `night_vision` is the only perception dimension that
/// reaches language (it alone drives `pack_depths`' hue ladder), so a
/// per-dragon value would give each dragon its own hue inventory and fragment
/// the shared frozen Draconic tongue — the cognates section admits only
/// concepts rooted in *every* daughter. A future dragon inherits this by
/// construction; a deliberately divergent-eyed dragon must override it, which
/// is exactly when someone should have to decide whether the shared tongue
/// still holds. At this value the hue ladder yields depth 2, so Draconic's
/// HUE vocabulary is exactly `dark`, `light`, and `red` — nothing else on
/// that ladder. The same value separately opens the full luminance ladder
/// (`gloom`/`shadow`/`starlit`, `pack_depths`' other output), so this is not
/// a claim about Draconic's color/dark vocabulary as a whole (spec: The Vigil).
/// type-audit: bare-ok(ratio)
pub const DRACONIC_NIGHT_VISION: f64 = 0.9;

/// A species' condition-tolerance profile: one response curve per v1
/// environmental axis. v1 fixes the four axes; a later campaign generalizes
/// to an open axis registry.
///
/// **The elevation frame (The Tumult's re-datum).** The elevation axis is
/// **height above the world's sea level, in metres** — the value worldgen's
/// `substrate_field` computes as `elevation_at(vertex) − sea_level`. It was
/// previously the raw `hornvale_kernel::ReferenceElevation`, whose datum is
/// isostatic (0 m = a reference-thickness crust at equilibrium) and whose
/// sea level is a *drawn* value differing by ~1.8 km between worlds — so an
/// authored optimum meant a different altitude on every seed, and the
/// kobold's 2600 sat ≈ 5200–5900 m above a typical world's sea level, at or
/// above its highest land. The optima below are authored against the
/// corrected frame, on named percentiles of the measured distribution of
/// **settleable land** (land above sea level with non-zero carrying
/// capacity), pooled over seeds 1..=30, n = 142 595 vertices:
///
/// | percentile | p15 | p25 | p35 | p50 | p65 | p75 | p85 | p95 |
/// |---|---:|---:|---:|---:|---:|---:|---:|---:|
/// | metres above sea level | 142 | 621 | 1004 | 1561 | 2166 | 2651 | 3251 | 4148 |
///
/// (All land, ignoring capacity, runs higher — median 2188 m, and a world's
/// single highest land vertex has a median of ≈ 6970 m.)
///
/// Three of the four peoples' elevation optima and every fauna kind's were
/// re-checked against that table and kept: they had always been *written* as
/// metres above sea level (the otyugh's "50, near sea level", the black
/// dragon's lowland swamp, the rust monster's sub-sea-level cave), so the
/// re-datum makes them mean what they say for the first time. Only the two
/// the table showed misplaced moved — the kobold's stronghold and the
/// goblin's generalist centre — plus the giant goat, which the kobold's move
/// displaced. The older "authored within the measured seed-42 land ranges"
/// note on each helper below refers to the other three axes, whose frames the
/// re-datum did not touch.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConditionNiche {
    /// Response curve over temperature, axis value in °C.
    pub temperature: ConditionResponse,
    /// Response curve over moisture, axis value in the climate moisture unit.
    pub moisture: ConditionResponse,
    /// Response curve over insolation, axis value in the annual-mean
    /// insolation unit.
    pub insolation: ConditionResponse,
    /// Response curve over elevation, axis value in **metres above the
    /// world's sea level** (see the struct doc for the frame and the
    /// measured land distribution the optima are authored against).
    pub elevation: ConditionResponse,
}

/// Kobold condition niche: cool HIGHLANDER — dark-adapted (consistent with
/// cool/polar), wide/indifferent on moisture, and staked to high elevation as
/// its exclusive, hard-excluding stronghold axis (Task B2b re-authoring: the
/// original B2 optima wanted cold+low-light vertices that are also food-poor on
/// this world; elevation is a geographically independent axis the lowland
/// species can't contest). See the species chapter's model card for the
/// ecological rationale.
///
/// **Elevation re-authored by The Tumult's re-datum** (see [`ConditionNiche`]
/// for the frame). The old 2600 was in reference-datum units, ≈ 5200–5900 m
/// above a typical world's sea level — above the highest land on most seeds,
/// so the stronghold was unoccupiable rather than uncontested and kobold fit
/// ran ~25× below every other people's *everywhere*. The new 3000 m above sea
/// level is p79 of settleable land: a genuine top-fifth stake. Measured over
/// seeds 1..=30, it is the exclusive stronghold the prose claims — kobold is
/// the best-fit people on every settleable vertex above 3000 m (mean fit 0.130
/// against hobgoblin 0.041, goblin 0.049, bugbear 0.004) while its own fit on
/// land below 500 m collapses to 0.0065, i.e. hard-excluded from the lowlands
/// the other three hold.
fn kobold_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 6.0,
            width: 14.0,
            devotion: 0.85,
        },
        // wide/indifferent
        moisture: ConditionResponse {
            optimum: 0.45,
            width: 0.60,
            devotion: 0.40,
        },
        // low light — consistent with cold/polar
        insolation: ConditionResponse {
            optimum: 0.04,
            width: 0.12,
            devotion: 0.80,
        },
        // HIGHLANDS — its exclusive niche. p79 of settleable land.
        elevation: ConditionResponse {
            optimum: 3000.0,
            width: 1100.0,
            devotion: 0.95,
        },
    }
}

/// Goblin condition niche: a warm-marginal GENERALIST with wide tolerance on
/// every axis (the cosmopolitan weed that fills margins/ecotones between the
/// three specialists). See the species chapter's model card for the
/// ecological rationale.
///
/// **Elevation re-authored by The Tumult's re-datum** (see [`ConditionNiche`]
/// for the frame). A wide, low-devotion curve only reads as *indifferent* if
/// it is centred on the land it scores: 500 m above sea level is p22 of
/// settleable land, so the old value made the generalist quietly lowland-
/// leaning. Recentred on the land median (1500 m ≈ p49), the same width and
/// devotion now sit flat across the range — measured mean fit on highland
/// (≥3000 m) rises 0.036 → 0.049 and on lowland (≤500 m) falls 0.066 → 0.060,
/// i.e. the two ends converge, which is what indifference looks like.
fn goblin_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 18.0,
            width: 28.0,
            devotion: 0.45,
        },
        moisture: ConditionResponse {
            optimum: 0.50,
            width: 0.60,
            devotion: 0.35,
        },
        insolation: ConditionResponse {
            optimum: 0.13,
            width: 0.30,
            devotion: 0.35,
        },
        // wide/indifferent, centred on the settleable-land median (p49).
        elevation: ConditionResponse {
            optimum: 1500.0,
            width: 3000.0,
            devotion: 0.35,
        },
    }
}

/// Hobgoblin condition niche: temperate, DRIER, low-to-mid-elevation open
/// plains — moisture and elevation separate it from bugbear's wet lowlands
/// and kobold's highlands (Task B2b re-authoring). See the species chapter's
/// model card for the ecological rationale.
///
/// **Elevation re-checked, not re-authored, under The Tumult's re-datum**
/// (see [`ConditionNiche`] for the frame). 600 m above sea level is p24 of
/// settleable land and the ±1400 m band spans p10–p60 — the plains band
/// between bugbear's lowland (p15) and kobold's highland (p79), which is
/// exactly what the value was always meant to say. Only the frame it is said
/// in was wrong; the number survives it unchanged.
fn hobgoblin_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 13.0,
            width: 10.0,
            devotion: 0.90,
        },
        // drier/open plains
        moisture: ConditionResponse {
            optimum: 0.35,
            width: 0.30,
            devotion: 0.80,
        },
        // open sun
        insolation: ConditionResponse {
            optimum: 0.19,
            width: 0.13,
            devotion: 0.85,
        },
        // low-mid: p24 of settleable land, band p10–p60.
        elevation: ConditionResponse {
            optimum: 600.0,
            width: 1400.0,
            devotion: 0.70,
        },
    }
}

/// Bugbear condition niche: warm-WET LOWLAND forest (rainforest); moisture is
/// its stronghold axis, insolation stays wide/neutral so it does not fight
/// the world's warm↔sun coupling the way the original B2 shaded-forest
/// framing did (Task B2b re-authoring). See the species chapter's model card
/// for the ecological rationale.
///
/// **Elevation re-checked, not re-authored, under The Tumult's re-datum**
/// (see [`ConditionNiche`] for the frame). 150 m above sea level is p15 of
/// settleable land — the lowland stake the prose claims, and the value needed
/// only the corrected frame to mean it. Measured: bugbear's mean fit on land
/// below 500 m is 0.264, against 0.0038 above 3000 m — the sharpest
/// lowland/highland split of the four, as a rainforest species should have.
///
/// **Contradicted by a later measurement, undiagnosed (The Generalist, Task
/// 6):** a re-run read bugbear's mean fit below 500 m as 0.017563, ~15x below
/// the figure above, with the whole kobold-highland comparison also an order
/// of magnitude down and goblin/hobgoblin rank-swapped. Whether the
/// populations, mesh, or frame differ between the two runs has not been
/// investigated; neither number has been corrected. See
/// `BIO-generalist-remeasure` in the idea registry.
fn bugbear_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 21.0,
            width: 11.0,
            devotion: 0.85,
        },
        // WETTEST vertices — its stronghold
        moisture: ConditionResponse {
            optimum: 0.82,
            width: 0.20,
            devotion: 0.95,
        },
        // wide/neutral
        insolation: ConditionResponse {
            optimum: 0.15,
            width: 0.40,
            devotion: 0.30,
        },
        // lowland: p15 of settleable land.
        elevation: ConditionResponse {
            optimum: 150.0,
            width: 1200.0,
            devotion: 0.70,
        },
    }
}

/// Treant condition niche: temperate FOREST autotroph; moderate warmth and
/// moisture, shaded (low-mid insolation), lowland-to-mid elevation. Mighty
/// (potency > 0), so its sovereignty floor already buffers climate — the
/// curve stays a genuine preference (moderate devotion), not a hard fence.
/// Authored within the measured seed-42 land ranges.
fn treant_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 14.0,
            width: 12.0,
            devotion: 0.75,
        },
        moisture: ConditionResponse {
            optimum: 0.55,
            width: 0.25,
            devotion: 0.70,
        },
        insolation: ConditionResponse {
            optimum: 0.12,
            width: 0.10,
            devotion: 0.50,
        },
        elevation: ConditionResponse {
            optimum: 400.0,
            width: 1500.0,
            devotion: 0.50,
        },
    }
}

/// Twig blight condition niche: the same temperate-forest tile as its
/// treant kin, at understory scale — no potency, so it is more tightly
/// environment-placed (higher devotion on the axes that matter). Authored
/// within the measured seed-42 land ranges.
fn twig_blight_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 15.0,
            width: 10.0,
            devotion: 0.70,
        },
        moisture: ConditionResponse {
            optimum: 0.55,
            width: 0.20,
            devotion: 0.75,
        },
        insolation: ConditionResponse {
            optimum: 0.10,
            width: 0.08,
            devotion: 0.55,
        },
        elevation: ConditionResponse {
            optimum: 350.0,
            width: 1200.0,
            devotion: 0.55,
        },
    }
}

/// Giant elk condition niche: temperate open woodland/plains grazer —
/// cooler and drier than the treant's shaded forest, wide-open sun.
/// Authored within the measured seed-42 land ranges.
fn giant_elk_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 10.0,
            width: 15.0,
            devotion: 0.60,
        },
        moisture: ConditionResponse {
            optimum: 0.40,
            width: 0.35,
            devotion: 0.50,
        },
        insolation: ConditionResponse {
            optimum: 0.16,
            width: 0.15,
            devotion: 0.50,
        },
        elevation: ConditionResponse {
            optimum: 600.0,
            width: 1600.0,
            devotion: 0.40,
        },
    }
}

/// Woolly mammoth condition niche: COLD tundra plains — a deep-cold
/// specialist (near the measured floor) at LOW elevation, distinguishing it
/// from the giant goat's high-mountain cold. Authored within the measured
/// seed-42 land ranges.
fn woolly_mammoth_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: -25.0,
            width: 20.0,
            devotion: 0.85,
        },
        moisture: ConditionResponse {
            optimum: 0.30,
            width: 0.30,
            devotion: 0.40,
        },
        insolation: ConditionResponse {
            optimum: 0.05,
            width: 0.08,
            devotion: 0.60,
        },
        elevation: ConditionResponse {
            optimum: 200.0,
            width: 1000.0,
            devotion: 0.50,
        },
    }
}

/// Giant goat condition niche: ALPINE/HIGHLAND — its exclusive, hard-
/// excluding stronghold axis is elevation (mirrors the kobold's highlander
/// shape, one band above it), cool mountain temperature rather than arctic
/// cold.
///
/// **Elevation re-authored by The Tumult's re-datum** (see [`ConditionNiche`]
/// for the frame). The goat is documented as the alpine ceiling *above* the
/// kobold highlander; the re-datum moved kobold to 3000 m above sea level, so
/// leaving the goat at 3000 would have collapsed the two onto one optimum.
/// 3800 m is p91 of settleable land, against a p95 of 4148 — "near the
/// measured ceiling" restored as a true claim in the corrected frame. The
/// cost is the honest one for a ceiling specialist: mean fit over settleable
/// land falls 0.089 → 0.059.
fn giant_goat_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: -5.0,
            width: 15.0,
            devotion: 0.70,
        },
        moisture: ConditionResponse {
            optimum: 0.35,
            width: 0.30,
            devotion: 0.40,
        },
        insolation: ConditionResponse {
            optimum: 0.10,
            width: 0.12,
            devotion: 0.50,
        },
        // HIGH MOUNTAIN — its exclusive niche, near the measured ceiling
        // (p91 of settleable land; kobold's highland stake sits at p79).
        elevation: ConditionResponse {
            optimum: 3800.0,
            width: 900.0,
            devotion: 0.90,
        },
    }
}

/// Otyugh condition niche: warm, WET LOWLAND (swamp/refuse) detritivore —
/// the wettest, lowest-elevation tile, warmer than the bugbear's rainforest.
/// Authored within the measured seed-42 land ranges.
fn otyugh_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 23.0,
            width: 10.0,
            devotion: 0.80,
        },
        // WETTEST vertices — its stronghold, near the measured ceiling.
        moisture: ConditionResponse {
            optimum: 0.83,
            width: 0.15,
            devotion: 0.90,
        },
        insolation: ConditionResponse {
            optimum: 0.10,
            width: 0.30,
            devotion: 0.30,
        },
        // near sea level — its exclusive lowland niche.
        elevation: ConditionResponse {
            optimum: 50.0,
            width: 800.0,
            devotion: 0.70,
        },
    }
}

/// Xorn condition niche: subterranean/mineral — an elemental that burrows
/// through solid earth, so it reads as nearly climate-indifferent on the
/// surface axes (low devotion everywhere); mighty (potency > 0) already
/// buys most of its sovereignty floor. Temperature/moisture/elevation are
/// authored within the measured seed-42 land ranges, unchanged.
///
/// **Insolation re-authored (The Deep Realm, Task 6).** The old curve
/// (`optimum: 0.05, width: 0.20`) approximated cave-dark by biasing toward
/// the darkest *surface* vertices — a proxy authored back when no subterranean
/// substrate existed to score against directly. Now that one does
/// (`hornvale_worldgen::subterranean_substrate` reads insolation as `0.0`
/// exactly, always), the proxy is no longer needed to make a xorn read as
/// dwelling in the dark, so this widens past the entire plausible surface
/// insolation range (`~[0, 0.35]`) instead of narrowing further: at
/// `width: 1.0` the response barely varies across that whole range, which
/// is the genuinely-indifferent claim this niche has always made, now
/// implemented on every axis rather than only some of them. Devotion stays
/// at its old low value — this is a widening, not a strengthening.
fn xorn_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 0.0,
            width: 40.0,
            devotion: 0.10,
        },
        moisture: ConditionResponse {
            optimum: 0.40,
            width: 0.60,
            devotion: 0.10,
        },
        // Widened past the plausible surface range so the response is flat
        // rather than dark-biased — see the frame note above.
        insolation: ConditionResponse {
            optimum: 0.15,
            width: 1.0,
            devotion: 0.10,
        },
        elevation: ConditionResponse {
            optimum: 0.0,
            width: 3500.0,
            devotion: 0.10,
        },
    }
}

/// Rust monster condition niche: subterranean/cave mineral-eater — no
/// potency, so unlike the xorn it is genuinely environment-placed. Cool
/// stable rock temperature is an unchanged, real preference (mass alone
/// still buys some sovereignty floor, so this is a soft lean, not a hard
/// fence) authored within the measured seed-42 land ranges.
///
/// **Moisture, insolation and elevation re-authored (The Deep Realm, Task
/// 6).** All three were originally proxies scored against the surface,
/// authored back when no subterranean substrate existed to place a cave
/// creature against directly:
///
/// - **moisture** moves from a mild `0.45` "somewhat wet" lean to `0.90` —
///   which was, when authored, exactly
///   [`hornvale_worldgen::subterranean_substrate`]'s fixed
///   `SUBTERRANEAN_MOISTURE`, so a real chamber was a genuine match rather
///   than an approximation of one — with devotion raised to `0.60`: this is
///   meant as a real preference now that a real reading exists to have one
///   about.
///
///   **That constant is gone (The Underworld, spec §4.3).** A chamber's
///   moisture is now derived from its distance above the water table and the
///   rock's porosity: saturated where the chamber is flooded (68–84% of cave
///   columns at their reach depth, on the campaign's three seeds) and spread
///   over roughly `[0.3, 0.8]` where it is not. So `0.90` is no longer the
///   value every chamber reports; it is an authored preference that a wet
///   chamber meets and a dry one does not, which is what a preference is
///   supposed to be. **It is deliberately not re-authored here**: moving a
///   species' curve at the same moment the reading beneath it moves would
///   make the two changes unattributable, and nothing measures this curve
///   today anyway — `warren_readout`'s P1 tripwire shows the Liebig minimum
///   is bound by the unfloored elevation axis on every cave-bearing vertex, so
///   moisture does not reach the result at all.
/// - **insolation** moves from `0.03` (the darkest available *surface*
///   vertices, a proxy for "inside a cave") to `0.0` exactly — the true
///   subterranean reading — with devotion raised to `0.70`, the strongest
///   axis in this niche: darkness is this creature's defining trait.
/// - **elevation** widens from `-500` (sub-sea-level, a second proxy for
///   "underground" authored the same way) toward indifference. A chamber's
///   elevation is simply the land above it, unchanged by depth
///   (`subterranean_substrate` does not invent a metres-below-surface
///   coordinate — see its own docs for why), so a fixed sub-sea-level
///   optimum would now systematically under-score genuine caves sitting on
///   ordinary high land. Once darkness and dampness are measured directly,
///   altitude is not what actually distinguishes this creature's habitat,
///   so the axis widens rather than relocating to a new fixed point.
fn rust_monster_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 8.0,
            width: 20.0,
            devotion: 0.50,
        },
        // Mirrored `SUBTERRANEAN_MOISTURE` when authored; that constant was
        // retired by The Underworld and this is now an ordinary authored
        // preference — see the frame note above.
        moisture: ConditionResponse {
            optimum: 0.90,
            width: 0.22,
            devotion: 0.60,
        },
        // TRUE darkness, not a proxy — see the frame note above.
        insolation: ConditionResponse {
            optimum: 0.0,
            width: 0.06,
            devotion: 0.70,
        },
        // Widened toward indifference — see the frame note above.
        elevation: ConditionResponse {
            optimum: 800.0,
            width: 3200.0,
            devotion: 0.25,
        },
    }
}

/// White dragon condition niche: an obligate apex that owns the cold;
/// mighty (buffers climate). The worked example from the task brief.
fn white_dragon_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: -20.0,
            width: 25.0,
            devotion: 0.9,
        },
        moisture: ConditionResponse {
            optimum: 0.4,
            width: 0.5,
            devotion: 0.3,
        },
        insolation: ConditionResponse {
            optimum: 0.05,
            width: 0.15,
            devotion: 0.2,
        },
        elevation: ConditionResponse {
            optimum: 1500.0,
            width: 3000.0,
            devotion: 0.4,
        },
    }
}

/// Red dragon condition niche: warm/volcanic apex — arid, high-sun, high
/// mountain terrain; the mightiest of the three chromatics, so its floor
/// dominates and this curve stays a soft preference. Authored within the
/// measured seed-42 land ranges.
fn red_dragon_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 20.0,
            width: 20.0,
            devotion: 0.85,
        },
        // arid/volcanic — dry.
        moisture: ConditionResponse {
            optimum: 0.10,
            width: 0.25,
            devotion: 0.60,
        },
        // open volcanic terrain — high sun.
        insolation: ConditionResponse {
            optimum: 0.20,
            width: 0.10,
            devotion: 0.60,
        },
        // volcanic peaks.
        elevation: ConditionResponse {
            optimum: 2200.0,
            width: 2500.0,
            devotion: 0.50,
        },
    }
}

/// Black dragon condition niche: swamp/wet apex — the wettest, lowest-
/// elevation chromatic, mighty like its white kin. Authored within the
/// measured seed-42 land ranges.
fn black_dragon_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 22.0,
            width: 12.0,
            devotion: 0.80,
        },
        // wettest vertices — its stronghold, like the otyugh's swamp.
        moisture: ConditionResponse {
            optimum: 0.80,
            width: 0.18,
            devotion: 0.85,
        },
        insolation: ConditionResponse {
            optimum: 0.10,
            width: 0.25,
            devotion: 0.30,
        },
        // lowland swamp.
        elevation: ConditionResponse {
            optimum: 50.0,
            width: 700.0,
            devotion: 0.60,
        },
    }
}

/// Owlbear condition niche: temperate forest predator — the treant's forest
/// tile, read from a predator's (not an autotroph's) tolerance shape; no
/// potency, so it is genuinely environment-placed. Authored within the
/// measured seed-42 land ranges.
fn owlbear_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 12.0,
            width: 14.0,
            devotion: 0.60,
        },
        moisture: ConditionResponse {
            optimum: 0.55,
            width: 0.30,
            devotion: 0.50,
        },
        insolation: ConditionResponse {
            optimum: 0.12,
            width: 0.15,
            devotion: 0.40,
        },
        elevation: ConditionResponse {
            optimum: 500.0,
            width: 1800.0,
            devotion: 0.40,
        },
    }
}

// The Vacancy (T7): seven terrestrial fauna, each authored against
// `windows/worldgen/tests/fixtures/occupancy.csv` (the committed occupancy
// readout) as it stood before this task's regen. Two structural facts read
// off `per_species_suitability` (worldgen) shaped every niche below: (1) the
// `ANIMAL_PREY`/`PLANT_FORAGE` supply terms both derive from
// `forage_supply_field`, itself a fraction of the NPP-based `base_carrying`
// field, which collapses toward 0 wherever `carrying_capacity`'s aridity term
// pushes hostility high (desert-band vertices, moisture < 0.2) — this is why
// EVERY existing NPP-fed kind (giant-elk, giant-goat, woolly-mammoth, the
// four peoples, the three dragons, owlbear) carries no desert row at all in
// the readout; (2) `DETRITUS`'s supply is `DETRITUS_AMBIENT`, a flat land-mask
// constant untouched by aridity, which is why otyugh/rust-monster/xorn are the
// only kinds that reach desert and ice today. Elevation optima below cite the
// settleable-land percentile table on [`ConditionNiche`]'s doc (p15=142,
// p25=621, p35=1004, p50=1561, p65=2166, p75=2651, p85=3251, p95=4148 m above
// sea level).

/// Giant scorpion condition niche: the hot-arid DESERT specialist — the
/// largest land gap in the pre-T7 readout. Every existing `ANIMAL_PREY`/
/// `PLANT_FORAGE` consumer carries zero desert rows (see the block comment
/// above); the niche below weights `DETRITUS` **over** `ANIMAL_PREY` — the
/// shipped vector is `ANIMAL_PREY 0.3, DETRITUS 0.7`, an opportunistic
/// scavenger reading rather than a pure predator — specifically so the supply
/// term is not dominated by the NPP-linked `ANIMAL_PREY` axis, which collapses
/// in desert the way any predator's would. **Measured, not fully achieved**:
/// this raised the scorpion's desert `mean_k` from 0.0081 under the
/// prey-dominant vector (`ANIMAL_PREY 0.7, DETRITUS 0.3`) to 0.0176 under the
/// shipped detritus-dominant one, and desert is now the scorpion's
/// #2 biome by `mean_k` (behind only tropical-rainforest, 0.0198) — a real,
/// competitive desert presence, clearly ahead of every prior desert
/// occupant (otyugh/rust-monster/xorn all sit at or below 0.014 there) —
/// but not the outright #1 biome. See this crate's T7 task report for why:
/// `insolation` is a pure function of latitude (`annual_mean_insolation`),
/// uncorrelated with canopy/shade, so it barely differentiates hot biomes
/// from one another, and `DETRITUS`'s flat land-mask supply still leaves
/// wetter hot biomes with a small residual `ANIMAL_PREY` edge. Ectotherm
/// (the third, after kobold and rust-monster). Large beast, Challenge 3
/// (5E Monster Manual, verified at authoring time; `potency` stays 0.0 —
/// mundane, not the dragon/plant/elemental supernatural set). Mass is an
/// author's estimate for the MM's Large size category (no weight is
/// printed in the stat block); ~300 kg, a horse-scale armored predator.
fn giant_scorpion_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // hottest optimum in the roster — the desert's hot band (>=20C).
        temperature: ConditionResponse {
            optimum: 32.0,
            width: 10.0,
            devotion: 0.80,
        },
        // deep in the desert moisture band (<0.20); the DETRITUS share on
        // the niche (below) is what keeps this survivable rather than void.
        moisture: ConditionResponse {
            optimum: 0.10,
            width: 0.12,
            devotion: 0.75,
        },
        // the highest-sun optimum in the roster — open desert exposure.
        insolation: ConditionResponse {
            optimum: 0.24,
            width: 0.10,
            devotion: 0.65,
        },
        // desert basin lowland, below p25 (621 m).
        elevation: ConditionResponse {
            optimum: 400.0,
            width: 1000.0,
            devotion: 0.45,
        },
    }
}

/// Giant hyena condition niche: the SAVANNA witness of `Gregarious ×
/// ANIMAL_PREY` — before this task every herder in the roster was a pure
/// forager (`the_dark_trait_combinations_are_named`, pre-T7: empty). Savanna
/// carries real NPP-fed supply today (giant-elk's savanna mean_k = 0.0214,
/// pre-regen readout), so a pure `ANIMAL_PREY` predator is safe here, unlike
/// the desert case above. Large beast, Challenge 1 (5E MM, verified). Mass
/// is an author's estimate for the MM's Large size category; ~160 kg, above
/// a real spotted hyena's scale to match "giant."
fn giant_hyena_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 25.0,
            width: 9.0,
            devotion: 0.75,
        },
        // mid-savanna band (0.20-0.45).
        moisture: ConditionResponse {
            optimum: 0.32,
            width: 0.13,
            devotion: 0.70,
        },
        // open savanna sun.
        insolation: ConditionResponse {
            optimum: 0.19,
            width: 0.10,
            devotion: 0.55,
        },
        // savanna lowland, below p25 (621 m).
        elevation: ConditionResponse {
            optimum: 500.0,
            width: 1400.0,
            devotion: 0.40,
        },
    }
}

/// Dire wolf condition niche: the BOREAL witness of `Gregarious ×
/// ANIMAL_PREY` — the same dark combination as the giant hyena, a second
/// climate. Taiga carries real NPP-fed supply (giant-elk's taiga mean_k =
/// 0.0096, pre-regen readout), so predation is viable here too. Cold but
/// held well clear of the `Ice` cutoff (-20 C) and centred above taiga's
/// moisture split (>=0.30-0.35, vs. the drier tundra split) so the pack
/// reads as taiga, not tundra. Elevation kept low — 300 m, which sits between
/// p15 (142 m) and p25 (621 m) of settleable land, i.e. the low fifth without
/// being at the floor — because taiga's tree line falls toward ~400-1600 m at
/// the high latitudes taiga occupies (`tree_line_m`); a higher optimum here
/// would bleed into `Alpine`.
/// Large beast, Challenge 1 (5E MM, verified). Mass is an author's estimate
/// for the MM's Large size category; ~150 kg, matching the giant hyena's
/// scale for the shared vertex.
fn dire_wolf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: -3.0,
            width: 8.0,
            devotion: 0.75,
        },
        // above taiga's moisture split, distinguishing it from tundra.
        moisture: ConditionResponse {
            optimum: 0.45,
            width: 0.18,
            devotion: 0.60,
        },
        // low boreal sun.
        insolation: ConditionResponse {
            optimum: 0.06,
            width: 0.07,
            devotion: 0.55,
        },
        // low, well clear of the high-latitude tree line.
        elevation: ConditionResponse {
            optimum: 300.0,
            width: 1100.0,
            devotion: 0.40,
        },
    }
}

/// Rhinoceros condition niche: the herbivore prey base for the giant hyena
/// (savanna) — the hot-arid/savanna vertex. A pure `PLANT_FORAGE` grazer inherits
/// the same desert-NPP collapse the giant elk/goat/mammoth already show (see
/// the block comment above), so this is authored savanna-dominant with an
/// arid lean toward the desert margin, not as a true desert occupant — the
/// honest placement for an NPP-fed herbivore. Large beast, Challenge 2 (5E
/// MM, verified). Unlike the fantastical menagerie, "Rhinoceros" in the MM
/// **is** the real animal, so its mass is sourced from the real species
/// rather than estimated: ~2300 kg, a white rhinoceros adult male average.
/// Solitary (real rhinos are not herd animals), distinguishing its
/// `SocialForm` from the roster's other grazers.
fn rhinoceros_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 25.0,
            width: 10.0,
            devotion: 0.65,
        },
        // savanna-leaning, arid edge — spans toward the desert margin
        // without centring on it (see doc: a pure grazer cannot stake true
        // desert the way the scorpion's DETRITUS-blended niche can).
        moisture: ConditionResponse {
            optimum: 0.24,
            width: 0.16,
            devotion: 0.55,
        },
        insolation: ConditionResponse {
            optimum: 0.20,
            width: 0.11,
            devotion: 0.50,
        },
        // savanna lowland.
        elevation: ConditionResponse {
            optimum: 450.0,
            width: 1500.0,
            devotion: 0.40,
        },
    }
}

/// Giant constrictor snake condition niche: the TROPICAL `Ectotherm` apex —
/// "hot-wet is covered by peoples and a dragon, not by an ectotherm" (spec
/// §5.1). Shares the black dragon's wet-lowland climate tile (moisture
/// optimum 0.80, elevation 50 m) but as a mundane, non-buffered predator: no
/// `potency`, so devotion is tighter here than the dragon's soft preference.
/// Huge beast, Challenge 2 (5E MM, verified). Mass is an author's estimate
/// for the MM's Huge size category; ~500 kg, scaled up from a real large
/// anaconda for a "giant" fantasy constrictor.
fn giant_constrictor_snake_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 27.0,
            width: 7.0,
            devotion: 0.80,
        },
        // tropical-rainforest wet.
        moisture: ConditionResponse {
            optimum: 0.78,
            width: 0.16,
            devotion: 0.75,
        },
        // shaded canopy floor, like the black dragon's ambush and the
        // otyugh's swamp.
        insolation: ConditionResponse {
            optimum: 0.11,
            width: 0.09,
            devotion: 0.45,
        },
        // lowland, p15 (142 m).
        elevation: ConditionResponse {
            optimum: 150.0,
            width: 900.0,
            devotion: 0.50,
        },
    }
}

/// Carrion crawler condition niche: the second `DETRITUS` witness (otyugh is
/// the first). `DETRITUS`'s supply (`DETRITUS_AMBIENT`) is a flat land-mask
/// constant with no spatial variation, so this niche is deliberately placed
/// AWAY from the otyugh's warm/wet swamp stronghold (temperature 23,
/// moisture 0.83, elevation 50) — a cool, shaded, moderate-moisture tile —
/// so the two `DETRITUS` witnesses are genuinely differentiated, not a near-
/// duplicate pair. Large monstrosity, Challenge 2 (5E MM, verified;
/// `potency` stays 0.0 — monstrosity is not in this campaign's supernatural
/// set, matching the owlbear precedent). Mass is an author's estimate for
/// the MM's Large size category; ~200 kg.
fn carrion_crawler_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // cool, unlike the otyugh's warm 23 C.
        temperature: ConditionResponse {
            optimum: 3.0,
            width: 9.0,
            devotion: 0.55,
        },
        // moist forest litter, below the otyugh's wettest-vertex stake.
        moisture: ConditionResponse {
            optimum: 0.55,
            width: 0.22,
            devotion: 0.55,
        },
        // shaded/cave-adjacent, tighter than the otyugh's wide-neutral 0.30.
        insolation: ConditionResponse {
            optimum: 0.05,
            width: 0.07,
            devotion: 0.55,
        },
        elevation: ConditionResponse {
            optimum: 350.0,
            width: 1200.0,
            devotion: 0.40,
        },
    }
}

/// Shrieker condition niche: `Sessile × DETRITUS` — a genuinely new vertex
/// (both existing `Sessile` kinds, treant/twig-blight, are `PHOTOSYNTHATE`
/// autotrophs), a decomposer that cannot move. Medium plant, Challenge 0
/// (5E MM, verified) — `potency` is `0.0` either way (`CR/30 = 0`), so this
/// kind does not have to resolve whether "plant" belongs to the supernatural
/// set for it. Mass is an author's estimate for the MM's Medium size
/// category, consistent with the MM's own "human-sized mushroom" framing;
/// ~35 kg. The deepest-shade insolation optimum in the roster (tighter than
/// even the rust monster's cave preference) is this kind's signature: a
/// stationary decomposer that cannot walk toward better light has to be
/// authored INTO the darkness it needs, not merely tolerant of it.
fn shrieker_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // temperate, between the goblin's warm-marginal and the dire wolf's
        // cold.
        temperature: ConditionResponse {
            optimum: 11.0,
            width: 9.0,
            devotion: 0.55,
        },
        // moist forest floor.
        moisture: ConditionResponse {
            optimum: 0.58,
            width: 0.18,
            devotion: 0.55,
        },
        // deepest shade in the roster — a sessile decomposer's hard stake.
        insolation: ConditionResponse {
            optimum: 0.03,
            width: 0.05,
            devotion: 0.70,
        },
        elevation: ConditionResponse {
            optimum: 400.0,
            width: 1300.0,
            devotion: 0.35,
        },
    }
}

// The Vacancy (T8): four marine kinds plus one amphibious kind, the first
// roster members to weight `MARINE_FORAGE` (The Vacancy T6). Elevation below
// is `elevation_at(vertex) - sea_level`
// ([`ConditionNiche`]'s struct doc), so a marine optimum is NEGATIVE — its
// magnitude is depth. The percentiles cited per kind below come from a
// throwaway probe (deleted before commit, not part of the suite) that
// measured `substrate_field`'s elevation reading over every OCEAN vertex across
// seeds 1..=30 — the same sweep `occupancy_readout.rs` uses — bucketed by the
// vertex's `Biome`:
//
// | biome | n | min | p5 | p25 | p50 | p75 | p95 | max |
// |---|---:|---:|---:|---:|---:|---:|---:|---:|
// | coral-reef | 19586 | -200.0 | -173.4 | -56.7 | -40.0 | -40.0 | -40.0 | -0.0 |
// | kelp-forest | 13081 | -200.0 | -173.3 | -48.8 | -40.0 | -40.0 | -40.0 | -0.0 |
// | epipelagic | 5539 | -199.9 | -181.2 | -77.4 | -40.0 | -40.0 | -40.0 | -0.0 |
// | mesopelagic | 181106 | -1000.0 | -986.4 | -930.5 | -804.7 | -620.5 | -329.9 | -200.0 |
// | bathypelagic | 264736 | -3991.6 | -2022.1 | -1532.7 | -1262.6 | -1117.9 | -1026.3 | -1000.0 |
// | abyssal | 4 | -4010.6 | -4010.1 | -4008.1 | -4006.1 | -4004.1 | -4002.4 | -4001.9 |
//
// Two things this table settles: (1) the shelf biomes (coral-reef/kelp-forest/
// epipelagic, all `depth_m < 200` in `classify_marine`) sit almost entirely at
// a single dominant depth (-40 m — p50 through p95 tie exactly, a shelf-break
// artifact of the sculpting pipeline, not a modelling choice made here); (2)
// `Abyssal` is vanishingly rare (4 vertices total across the whole 30-seed sweep,
// right at its 4000 m floor) and `HadalTrench` never occurred at all, so a
// kind "for" the abyssal is honestly a bathypelagic kind whose tail can reach
// the boundary, not a kind with a real abyssal stronghold to measure against.
//
// `marine_forage_supply_field` (worldgen) keys `MARINE_FORAGE` productivity
// directly to the vertex's biome class (coral-reef/kelp-forest 0.85, epipelagic
// 0.45, mesopelagic 0.15, bathypelagic 0.05, abyssal/hadal-trench 0.02,
// upwelling 1.0) rather than to a continuous NPP field the way the land's
// `PHOTOSYNTHATE`/`PLANT_FORAGE` supply is — so, unlike The Vacancy T7's
// land kinds (whose `mean_k` ranking was dominated by NPP magnitude,
// independent of the kind's own target biome — BIO-supply-drowns-niche), a marine kind's own
// elevation+temperature optimum is what SELECTS its supply tier, because it
// selects which biome class the vertex classifies as in the first place.
// Measured per-kind below; `upwelling`'s productivity (1.0) is the one
// remaining confound, since it can outrank a shelf/deep-water kind's own
// target biome on vertices the kind's wide condition tolerance also reaches.
//
// Temperature at every vertex (including ocean) is `climate.mean_temperature_at`,
// a pure function of latitude and elevation-above-sea-level lapse (elevation
// below sea level applies NO lapse term) — i.e. sea-surface temperature only,
// uncorrelated with depth (`domains/climate/src/temperature.rs`). Insolation
// is likewise a pure function of latitude/obliquity (Finding 2, The Vacancy
// T7 report) — also uncorrelated with depth. Neither axis can therefore
// distinguish "sunlit shallows" from "aphotic deep water" the way real ocean
// physics would; a deep-water kind's low insolation/cool temperature
// optimum below is a thematic placement, not a claim the model enforces
// depth-linked light or cold. Moisture at every ocean vertex is the banded
// circulation model's base wetness plus a flat +0.3 ocean-proximity bonus
// (`domains/climate/src/moisture.rs::ocean_bonus`), landing at 0.55 (a
// sinking/dry band) or 0.90 (a rising/wet band) on spinning worlds — a
// circulation-band artifact with no marine ecological meaning, so every
// kind below keeps it wide and low-devotion rather than staking anything on
// it.
//
// CR/mass source: 5E Monster Manual, verified at authoring time (via the SRD
// mirrors `5esrd.com`/`5thsrd.org`, which reproduce the MM stat blocks under
// the OGL). As Task 7 found, the MM prints no weight for any beast; masses
// below are either an author's estimate for the kind's MM size category
// (marked as such) or, for a kind whose MM name names a real species outright
// (no "giant" prefix), the real animal's cited mass (the rhinoceros
// precedent) — both honestly labelled per kind, never presented as read off
// the stat block the way CR is.

/// Reef shark condition niche: the `CoralReef` witness — a warm, shallow-
/// shelf specialist. `classify_marine` requires `sst_c > 20` and
/// `depth_m < 200` for `CoralReef` (checked before the kelp/upwelling
/// branches), so a reliably tropical, reliably shallow niche lands on the
/// biome by construction, not by luck. Medium beast, Challenge 1/2 (5E MM,
/// verified). Mass is the real animal's (no "giant" prefix — the MM's "Reef
/// Shark" names the real species): a grey reef shark (*Carcharhinus
/// amblyrhynchos*) averages ~18.5 kg.
fn reef_shark_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // reliably above the CoralReef sst floor (20C).
        temperature: ConditionResponse {
            optimum: 26.0,
            width: 6.0,
            devotion: 0.70,
        },
        // wide/low-devotion: ocean moisture is a circulation-band artifact
        // (see block comment), not ecologically meaningful here.
        moisture: ConditionResponse {
            optimum: 0.75,
            width: 0.35,
            devotion: 0.25,
        },
        // open, sunlit shallows.
        insolation: ConditionResponse {
            optimum: 0.20,
            width: 0.12,
            devotion: 0.45,
        },
        // shelf depth: the dominant coral-reef depth is -40 m (see table).
        elevation: ConditionResponse {
            optimum: -40.0,
            width: 120.0,
            devotion: 0.50,
        },
    }
}

/// Giant octopus condition niche: the `KelpForest` witness — a cool,
/// shallow-shelf specialist, `classify_marine`'s mirror image of the reef
/// shark (`sst_c < 12`, same `depth_m < 200` shelf band). Large beast,
/// Challenge 1 (5E MM, verified). Mass is an author's estimate for the MM's
/// Large size category (the "giant" prefix marks this as the fantastical
/// scale-up, not the real Pacific giant octopus, whose adults top out
/// around 50 kg): ~180 kg.
fn giant_octopus_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // reliably below the KelpForest sst ceiling (12C).
        temperature: ConditionResponse {
            optimum: 8.0,
            width: 5.0,
            devotion: 0.70,
        },
        moisture: ConditionResponse {
            optimum: 0.75,
            width: 0.35,
            devotion: 0.25,
        },
        // cooler, higher-latitude sun than the reef shark's tropics.
        insolation: ConditionResponse {
            optimum: 0.12,
            width: 0.10,
            devotion: 0.45,
        },
        // same shelf band as the reef shark (-40 m dominant depth).
        elevation: ConditionResponse {
            optimum: -40.0,
            width: 120.0,
            devotion: 0.50,
        },
    }
}

/// Killer whale condition niche: the `Epipelagic` witness and the roster's
/// first MARINE `Gregarious x ANIMAL_PREY`-class predator (pod-hunting).
/// `classify_marine` reaches `Epipelagic` only on the SAME `depth_m < 200`
/// shelf band as the reef shark/giant octopus, at a MID sst (neither the
/// reef's `> 20` nor the kelp's `< 12`) — real killer whales are cosmopolitan
/// (all latitudes, all depths), but this model's `Epipelagic` class is
/// deliberately narrower than that, so the niche below is authored to the
/// classifier's actual band rather than the animal's full real range. Huge
/// beast, Challenge 3 (5E MM, verified). Mass is the real animal's (no
/// "giant" prefix): commonly cited adult male range 3,600-5,400 kg; ~5,400 kg
/// used here (the top of that commonly cited range, matching the roster's
/// other apex-scale masses).
fn killer_whale_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // the mid band between CoralReef's >20C and KelpForest's <12C.
        temperature: ConditionResponse {
            optimum: 16.0,
            width: 4.0,
            devotion: 0.55,
        },
        moisture: ConditionResponse {
            optimum: 0.75,
            width: 0.35,
            devotion: 0.25,
        },
        insolation: ConditionResponse {
            optimum: 0.15,
            width: 0.12,
            devotion: 0.40,
        },
        // same shelf band (-40 m dominant depth); real orcas range far
        // deeper, but Epipelagic itself is shelf-bound in this model.
        elevation: ConditionResponse {
            optimum: -40.0,
            width: 140.0,
            devotion: 0.45,
        },
    }
}

/// Giant squid condition niche: the `Bathypelagic`/`Abyssal` witness — a
/// deep, cold-and-dark-themed specialist. `Abyssal` is nearly unoccupiable
/// territory in this model (4 vertices total across the 30-seed probe sweep,
/// right at its 4000 m floor — see block comment), so this niche targets
/// `Bathypelagic`'s bulk (p50 depth 1263 m, p95 1026 m) with a tail reaching
/// toward the abyssal floor, rather than staking on the abyssal itself. Huge
/// beast, Challenge 7 (5E MM, verified). Mass is the real animal's (no
/// "giant" prefix — the MM's "Giant Squid" names the real species,
/// *Architeuthis dux*): commonly cited large-adult estimates run ~200-275 kg;
/// ~250 kg used here.
fn giant_squid_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // cool/dark theming (see block comment: sst is latitude-only, not
        // depth-linked, so this is thematic, not model-enforced).
        temperature: ConditionResponse {
            optimum: 8.0,
            width: 18.0,
            devotion: 0.30,
        },
        moisture: ConditionResponse {
            optimum: 0.75,
            width: 0.35,
            devotion: 0.25,
        },
        // aphotic-dark theming, mirroring the rust monster's cave stake.
        insolation: ConditionResponse {
            optimum: 0.05,
            width: 0.10,
            devotion: 0.45,
        },
        // bathypelagic's bulk (p50 -1262.6 m), width wide enough to reach
        // toward the abyssal floor (-4000 m) without centring on the
        // near-void abyssal band itself.
        elevation: ConditionResponse {
            optimum: -1500.0,
            width: 900.0,
            devotion: 0.55,
        },
    }
}

/// Giant crocodile condition niche: the AMPHIBIOUS proof case (spec §3.4) —
/// a coastal/estuarine ambush predator whose elevation optimum sits AT sea
/// level with a wide tolerance, so it scores well on both low-lying coastal
/// LAND (where its `ANIMAL_PREY` weight draws supply) and shallow marine
/// shelf vertices (where its `MARINE_FORAGE` weight draws supply) — the same
/// single condition-niche curve read against whichever supply field is
/// nonzero at that vertex, no special case anywhere. Huge beast, Challenge 5
/// (5E MM, verified). Mass is an author's estimate for the MM's Huge size
/// category (the "giant" prefix marks the fantastical scale-up; real
/// saltwater crocodiles top out near 1,000 kg): ~1,000 kg.
fn giant_crocodile_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // tropical estuarine warmth, like the otyugh/black-dragon swamp tile.
        temperature: ConditionResponse {
            optimum: 26.0,
            width: 8.0,
            devotion: 0.65,
        },
        // moist coastal/estuarine, between the marine axis's ~0.55-0.90 and
        // the swamp kinds' ~0.80-0.83 stronghold.
        moisture: ConditionResponse {
            optimum: 0.65,
            width: 0.30,
            devotion: 0.35,
        },
        // open coastal sun, like the hyena/rhinoceros savanna tile.
        insolation: ConditionResponse {
            optimum: 0.18,
            width: 0.12,
            devotion: 0.45,
        },
        // AT sea level, wide: reaches both low coastal land (e.g. the
        // otyugh's 50 m, the black dragon's 50 m) and the shelf's -40 m to
        // -200 m band.
        elevation: ConditionResponse {
            optimum: -20.0,
            width: 250.0,
            devotion: 0.45,
        },
    }
}

// The Vacancy (T9): the fifth people — the gnoll. Hot-arid DESERT
// specialist, the same climate tile `giant_scorpion_condition_niche` claims
// (`classify_land`, `domains/climate/src/biome.rs`: `Desert` requires
// `temp_c >= 20` and `moisture < 0.2`), but read as a pack-hunting Settled
// people rather than a solitary scavenger. Elevation cites the same
// settleable-land percentile table as every other people
// ([`ConditionNiche`]'s doc: p15=142, p25=621, p35=1004, p50=1561, p65=2166,
// p75=2651, p85=3251, p95=4148 m above sea level). The pre-T9 desert
// occupants in the committed `windows/worldgen/tests/fixtures/occupancy.csv`
// give the competitive landscape this niche is authored into: giant-scorpion
// mean_k 0.0176 (desert's current best occupant), otyugh 0.0138,
// rust-monster 0.0042, shrieker 0.0031, xorn 0.0012 (all `desert`,
// `cells_occupied` 8020, the 30-seed sweep). **BIO-supply-drowns-niche applies unmodified**:
// `mean_k` is dominated by the NPP-linked `ANIMAL_PREY`/`PLANT_FORAGE`
// supply term, which collapses in desert exactly as it does for every other
// omnivore/predator in the roster (the block comment above
// `giant_scorpion_condition_niche` walks the mechanism); a genuinely
// hot-arid-authored gnoll is therefore not expected to out-rank the
// DETRITUS-fed desert incumbents on raw `mean_k`, and this niche was not
// re-weighted chasing that rank (see the measured ranking in this crate's
// T9 task report).
fn gnoll_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // hot desert band, clear of the >=20C floor with margin (compare
        // giant-scorpion's 32.0, the roster's hottest optimum).
        temperature: ConditionResponse {
            optimum: 29.0,
            width: 9.0,
            devotion: 0.80,
        },
        // deep in the desert moisture band (<0.20), mirroring the
        // giant-scorpion's stake on the same climate tile.
        moisture: ConditionResponse {
            optimum: 0.12,
            width: 0.12,
            devotion: 0.75,
        },
        // LOW, shade-seeking — the ecological choice behind this kind's
        // Crepuscular activity (see `perception_registry`): a desert pack
        // hunter that forages at the cooler margins of the day and shelters
        // through the peak heat, the same real-world strategy spotted
        // hyenas use. Insolation is a pure function of latitude
        // (BIO-insolation-is-latitude) — this is theming, not a claim the model enforces
        // canopy/shade, the same caveat every "shaded" niche in this file
        // already carries (rust-monster's cave stake, black-dragon's swamp
        // ambush).
        insolation: ConditionResponse {
            optimum: 0.08,
            width: 0.10,
            devotion: 0.45,
        },
        // desert basin lowland, below p25 (621 m) — the same band
        // giant-scorpion (400 m) and giant-hyena (500 m, savanna) stake.
        elevation: ConditionResponse {
            optimum: 500.0,
            width: 1300.0,
            devotion: 0.40,
        },
    }
}

/// Human condition niche: the roster's first true GENERALIST — a settler
/// that leans on none of its four axes and is authored to be
/// simultaneously the LEAST-DEVOTED and the WIDEST curve on every axis
/// among the peoples, so "no refuge" is true in both senses that could
/// otherwise pull apart.
///
/// **Task 5b re-authoring (2026-08-04): a re-derivation, not the original
/// authoring.** The niche shipped by Task 2 stated its contrast with goblin
/// as devotion alone ("Width is a mixed comparison... Devotion does [carry
/// the contrast]") while its widths were, in fact, unargued: narrower than
/// goblin's on temperature (22.0 vs. 28.0) and elevation (2000.0 vs.
/// 3000.0). A shape-attribution reading in
/// `windows/worldgen/tests/generalist_distinctness.rs` then measured that
/// the vacuity gate's real-case dispersion gap was WIDTH-dominated, not
/// devotion-dominated, and pointed the opposite direction from what the
/// doc claimed. The owner directed a re-authoring so the claim and the
/// numbers agree: human's widths are now derived from a stated,
/// measurement-grounded rule rather than chosen by eye, and every axis is
/// verified wider than every other people's.
///
/// **The rule.** On each axis, human's response must vary by no more than
/// 20% of its peak across the measured p5–p95 span of settleable land
/// (`windows/worldgen/tests/generalist_baseline.rs`'s Task 5b extension,
/// seeds 1..=30, 142593 settleable vertices — the same population, same
/// [`hornvale_worldgen::Substrate`] frame,
/// [`crate::ConditionResponse::eval`] scores). Since
/// `bump = exp(-0.5 z²)`, `bump >= 0.80` requires `|z| <= 0.6680`, so a
/// FLOOR on width follows directly from the optimum's distance to the
/// farther of p5/p95:
///
/// ```text
/// width_floor = max(|optimum - p5|, |optimum - p95|) / 0.6680
/// ```
///
/// This is a lower bound, not a target — a wider curve is still
/// "indifferent," only more so. Where an axis's already-authored width
/// already cleared both this floor and every other people's width on that
/// axis, it is left unchanged (moisture); where it did not, it is raised —
/// to the floor where the floor itself is the binding constraint
/// (temperature, elevation), or modestly above the floor where the
/// binding constraint is instead being strictly wider than the roster
/// (insolation, whose floor is tiny because the settleable insolation band
/// is narrow, but which tied goblin's width before this pass).
///
/// **Measured p5/p50/p95 (settleable land, same population as
/// `human_condition_niche`'s elevation frame below) and the resulting
/// floors:**
///
/// | axis | optimum | p5 | p50 | p95 | width floor | authored width | why |
/// |---|---|---|---|---|---|---|---|
/// | temperature (°C) | 14.0 (kept, 4% off p50) | 3.27 | 14.59 | 31.59 | 26.33 | **29.0** | floor-bound, rounded above goblin's 28.0 |
/// | moisture | 0.50 (kept, 3% off p50) | 0.24 | 0.49 | 0.70 | 0.39 | **0.70 (unchanged)** | already clears the floor and every people's width |
/// | insolation | **0.25 (recentred; was 0.14, 43% off p50)** | 0.19 | 0.25 | 0.31 | 0.09 | **0.45** | floor is tiny (narrow settleable band); raised past bugbear's 0.40 to stay widest |
/// | elevation (m) | 1500.0 (fixed — see below) | 0.0 | 1561.2 | 4148.1 | 3964.1 | **4000.0** | floor-bound, comfortably above goblin's 3000.0 |
///
/// Optima: kept where within 10% of the measured p50 (temperature,
/// moisture); recentred on p50 where not (insolation, 0.14 → 0.25 — the
/// original value was authored before this measurement existed and landed
/// well off the land the axis is actually scored against). Elevation's
/// optimum is a deliberate exception, held at 1500.0 rather than the
/// measured p50 (1561.2) — see the paragraph below.
///
/// **Sanity check, verified rather than assumed: human is now the widest
/// curve of the SIX peoples on all four axes** (temperature 29.0 > goblin's
/// 28.0; moisture 0.70 > goblin/kobold's 0.60; insolation 0.45 > bugbear's
/// 0.40; elevation 4000.0 > goblin's 3000.0) — the property this
/// re-authoring exists to restore. Devotion is UNCHANGED (0.20/0.20/0.25/
/// 0.30 — see below) and remains the lowest of the six peoples on every
/// axis, so "widest and least devoted" is now true in both senses on every
/// axis, not mixed axis by axis as the original authoring left it.
///
/// Goblin's own elevation optimum was already re-centred by The Tumult's
/// re-datum to 1500.0 m, the settleable-land median at the time. Human's
/// elevation optimum sits at that SAME 1500.0 m — deliberately, not by
/// coincidence, and held there through this re-authoring even though the
/// freshly measured p50 (1561.2 m) has drifted slightly since — the terrain
/// mesh's land distribution is not perfectly stable seed-family to
/// seed-family, and re-chasing a ~40 m drift would decouple human's
/// optimum from goblin's shared-optimum argument for no ecological gain. A
/// wide, low-devotion curve only reads as genuine *indifference* if it is
/// centred on the land it scores (goblin's own re-datum argument); a
/// displaced optimum would instead hand human its own lowland or highland
/// refuge, which contradicts the no-refuge premise the whole campaign's
/// Gause probe rests on — this kind exists to test a competitor that
/// out-competes nobody and holds no stronghold of its own. So the two kinds
/// share an optimum on purpose: what makes human lose the specialists'
/// strongholds (kobold's mountain, bugbear's rainforest) is its LOWER
/// devotion against their high devotion, reinforced (not contradicted) by
/// this pass's wider curves.
///
/// Devotion is unchanged by this re-authoring — it is the lowest of the six
/// PEOPLES on every axis (temperature 0.20 vs. goblin's 0.45, moisture 0.20
/// vs. 0.35, insolation 0.25 vs. 0.35, elevation 0.30 vs. 0.35), the argued
/// contrast from Task 2 that measurement never called into question. Scoped
/// to the peoples, not the whole roster: xorn is lower on all four
/// (0.10/0.10/0.20/0.10), being the indifferent elemental this niche is
/// deliberately a peoples-appropriate echo of.
///
/// **History: the vacuity/distinctness check and its width-attribution
/// finding.** `windows/worldgen/tests/generalist_distinctness.rs`'s
/// coefficient-of-variation statistic is scale-invariant under a positive
/// constant multiplier, so elevation's devotion is algebraically invisible
/// to it (elevation's hard `floor(0.0)` makes `eval` a pure multiplier
/// there); the other three axes' devotions are visible because their
/// nonzero sovereignty floor is additive, not multiplicative. Before this
/// re-authoring, the real-case gap (`cv_ratio = 1.0462`) was measured
/// WIDTH-dominated and pointed opposite to a devotion-only attribution
/// reading (`cv_ratio = 0.9766`) — full numbers and reasoning in that
/// file's module doc comment, which predates this pass and is retained as
/// the record of the finding that motivated it. That file's own doc comment
/// and the design spec's §4 amendment carry the post-re-authoring numbers;
/// see them for the current reading rather than re-deriving it here.
///
/// Frame: elevation is metres above the world's sea level (see
/// [`ConditionNiche`]).
fn human_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 14.0,
            width: 29.0,
            devotion: 0.20,
        },
        moisture: ConditionResponse {
            optimum: 0.50,
            width: 0.70,
            devotion: 0.20,
        },
        // recentred on the measured settleable-land p50 (0.2468, rounded to
        // 0.25) - the original 0.14 sat 43% off it, outside the 10% keep
        // band (see the doc comment above).
        insolation: ConditionResponse {
            optimum: 0.25,
            width: 0.45,
            devotion: 0.25,
        },
        // wide/indifferent, held at 1500.0 m rather than re-chased to the
        // freshly measured p50 (1561.2 m) - the same value as goblin's
        // optimum, deliberately (see the doc comment above).
        elevation: ConditionResponse {
            optimum: 1500.0,
            width: 4000.0,
            devotion: 0.30,
        },
    }
}

// ---------------------------------------------------------------------------
// THE DELVERS (C2c): the dwarf family — three kinds on one measured rule.
//
// **Roster cut to three (spec §11).** Mountain and Duergar were authored and
// then withdrawn: both are defined by DEPTH, and the model's elevation axis
// is metres above sea level, so authoring "deep" as "low ASL" was the same
// class of fake The Warren spent itself removing. They return in a successor
// campaign that gives the underworld biomes (`BIO-kinds-declare-biomes`).
//
// The three niches below are authored against a MEASURED theorem rather than
// against taste, and the theorem decides which axis each kind's identity may
// live on. `tolerance_liebig` (`windows/worldgen/src/lib.rs`) floors
// temperature/moisture/insolation by `sovereignty_floor(mass, potency)` and
// passes elevation a literal `0.0`, while
// `hornvale_kernel::ConditionResponse::eval` is
// `floor + (1 - floor) * devotion * exp(-z²/2)`. So elevation's value never
// exceeds its own `devotion`, and the other three never fall below the floor:
//
//     elevation is the Liebig minimum on EVERY vertex
//         iff  devotion_elev < sovereignty_floor(mass, potency)
//
// `windows/worldgen/tests/delver_bind_audit.rs` measured that closed form on
// the shipped roster over seeds 42 / 7 / 1234 and confirmed it EXACTLY — every
// kind below its floor is elevation-bound on 100.00% of land, and the three
// authored above their floor are not (kobold 43-51%, hobgoblin 69-77%,
// bugbear 71-78%). Mass sets the floor; the AUTHORED devotion decides the
// bind. This roster uses both modes deliberately:
//
//   kind             mass   sov. floor   dev_el   mode     identity carried by
//   desert-dwarf     66.0     0.443252     0.70   ABOVE    climate (arid)
//   gully-dwarf      62.0     0.438477     0.30   below    elevation (low)
//   hill-dwarf       70.0     0.447705     0.30   below    elevation (mid)
//
// (Floors computed live from `hornvale_kernel::sovereignty_floor(Mass, 0.0)`,
// not copied from a plan — the plan's own table was wrong in the fourth
// decimal for two of them.)
//
// Two of the three are BELOW their floor, so their temperature, moisture and
// insolation curves are *prepared* in the organ-builder's sense — engraved,
// installed, connected to no rank. Each of those two says so in its own doc
// comment rather than implying a climate preference the model will never
// read. Desert-dwarf is the exception and the campaign's deliberate one: at
// devotion 0.70 against a floor of 0.443252 (a margin of 0.256748) its arid
// curves genuinely bind, making it the first people in the roster whose
// CLIMATE niche selects. Spec §10.2.
//
// Elevation optima are cited against the same settleable-land percentile
// table every other people uses ([`ConditionNiche`]'s doc: p15=142, p25=621,
// p35=1004, p50=1561, p65=2166, p75=2651, p85=3251, p95=4148 m above sea
// level).
// ---------------------------------------------------------------------------

/// Desert dwarf condition niche: the roster's first CLIMATE-selected people.
///
/// **Authored ABOVE its sovereignty floor, deliberately.** At 66.0 kg and
/// potency 0.0 the floor is `0.443252`; this niche's `devotion_elev` is
/// `0.70`, a margin of `0.256748` ABOVE it. By the measured theorem in the
/// block comment above, that means elevation is NOT the automatic Liebig
/// minimum — the temperature and moisture curves below actually bind, and
/// they are authored to be genuinely arid rather than decorative. Elevation
/// only takes the minimum where `0.70 * exp(-z²/2)` falls under the floor,
/// i.e. more than ~2880 m from the 700 m optimum — on land above ~3.6 km,
/// which is above p85 (3251 m). Everywhere below that, climate decides.
///
/// This is a shipped style, not a new one: kobold (0.95) and hobgoblin and
/// bugbear (0.70) already sit above their floors and are measurably not
/// elevation-bound. The cost is the mirror of the benefit — a kind authored
/// far above its floor is sharply excluded away from its optimum, which is
/// why `windows/worldgen/tests/non_void_roster.rs` is this authoring's gate.
///
/// The desert climate tile is `domains/climate`'s `Desert`
/// (`temp_c >= 20` and `moisture < 0.2`), the same tile gnoll and
/// giant-scorpion stake. Gnoll is NOT re-authored here even though the bind
/// audit diagnosed its defect — its `devotion_elev` of 0.40 sits BELOW its
/// floor of 0.4954, so its authored moisture curve has never once bound.
/// Moving an existing people's capacity inside a roster epoch would destroy
/// this campaign's attribution (spec §10.2).
fn desert_dwarf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // hot, clear of the Desert tile's >= 20 C floor with margin. Narrow
        // (8.0) because this is the axis that must actually select: at
        // devotion 0.70 the response spans [floor, 0.833], so a vertex 12 C off
        // the optimum reads at the floor and a vertex on it reads nearly twice
        // that.
        temperature: ConditionResponse {
            optimum: 28.0,
            width: 8.0,
            devotion: 0.70,
        },
        // deep inside the Desert tile's < 0.20 moisture band, and the second
        // axis authored to bind. Settleable-land moisture runs p5 0.24 /
        // p50 0.49 / p95 0.70, so an optimum of 0.12 puts the great majority
        // of land well off this curve — which is the aridity claim, stated as
        // a number.
        moisture: ConditionResponse {
            optimum: 0.12,
            width: 0.15,
            devotion: 0.65,
        },
        // Insolation is a pure function of latitude (BIO-insolation-is-latitude), and the
        // settleable band is narrow (p5 0.19 / p50 0.25 / p95 0.31). The
        // subtropical desert belt sits at its upper end, so the optimum leans
        // high, but the curve is wide and only moderately devoted: this is a
        // latitude-honest reading, not a second aridity claim. Inside the
        // kind's own arid stronghold — where temperature and moisture are
        // both satisfied — this becomes the binding axis, which is the
        // correct reading of "nothing in the climate constrains a desert
        // dwarf in the desert".
        insolation: ConditionResponse {
            optimum: 0.28,
            width: 0.35,
            devotion: 0.45,
        },
        // Wide and shallow-centred near p25 (621 m): desert basins and their
        // margins. The width (3000.0) is what keeps elevation OUT of the
        // minimum across ordinary land, so the climate axes can be the ones
        // that speak.
        elevation: ConditionResponse {
            optimum: 700.0,
            width: 3000.0,
            devotion: 0.70,
        },
    }
}

/// Gully dwarf condition niche: the lowland scavenger, and the roster's
/// lowest-sitting dwarf.
///
/// **Elevation is the sole binding axis, by construction.** `devotion_elev`
/// is `0.30` against a sovereignty floor of `0.438477` at 62.0 kg — the
/// lowest floor of the three, and still comfortably above 0.30 — so this kind
/// is elevation-bound on 100% of land. The three climate curves below are
/// honest but PREPARED: floored at `0.438477`, they can never fall under
/// `0.30 * bump`, so nothing consumes them. Stated rather than implied.
fn gully_dwarf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // temperate-to-warm, wide and unfussy — a scavenger eats what the
        // weather leaves. PREPARED: never binds.
        temperature: ConditionResponse {
            optimum: 17.0,
            width: 26.0,
            devotion: 0.25,
        },
        // damp lowland margins: floodplain, marsh edge, the wet ground where
        // detritus accumulates. PREPARED: never binds.
        moisture: ConditionResponse {
            optimum: 0.62,
            width: 0.40,
            devotion: 0.25,
        },
        // centred on the settleable-land insolation median (0.25), wide —
        // this kind makes no claim about light. PREPARED: never binds.
        insolation: ConditionResponse {
            optimum: 0.25,
            width: 0.40,
            devotion: 0.20,
        },
        // THE axis that binds: 150 m, at p15 (142 m) — coastal plain, river
        // bottom and the bottom of every gully. The narrowest width of the
        // five (900.0), because "low" is this kind's whole ecological
        // statement and a wide curve would erase it.
        elevation: ConditionResponse {
            optimum: 150.0,
            width: 900.0,
            devotion: 0.30,
        },
    }
}

/// Hill dwarf condition niche: the surface farmer-herder, the family's
/// middle kind, and the one every other dwarf is read against.
///
/// **Elevation is the sole binding axis, by construction.** `devotion_elev`
/// is `0.30` against a sovereignty floor of `0.447705` at 70.0 kg — the same
/// mass and the same devotion as human, which the bind audit measured
/// elevation-bound on 100.00% of land on every seed. The three climate curves
/// below are PREPARED: floored at `0.447705`, they never reach under
/// `0.30 * bump` and so are never the Liebig minimum. They are authored
/// honestly and they do not work; that is the model, not an oversight.
fn hill_dwarf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // temperate uplands: cooler than human's 14.0, on the cold side of
        // the settleable band (p5 3.27 / p50 14.59 / p95 31.59 C).
        // PREPARED: never binds.
        temperature: ConditionResponse {
            optimum: 11.0,
            width: 20.0,
            devotion: 0.35,
        },
        // moist enough to farm, just above the settleable median (0.49) —
        // terraced grain and pasture, not irrigation. PREPARED: never binds.
        moisture: ConditionResponse {
            optimum: 0.55,
            width: 0.35,
            devotion: 0.35,
        },
        // at the settleable-land insolation median. PREPARED: never binds.
        insolation: ConditionResponse {
            optimum: 0.25,
            width: 0.32,
            devotion: 0.30,
        },
        // THE axis that binds: 900 m, between p25 (621 m) and p35 (1004 m) —
        // genuine hill country, well above gully-dwarf's 150 m. The three
        // dwarves' optima are spaced so that no two overlap inside one
        // width.
        elevation: ConditionResponse {
            optimum: 900.0,
            width: 1400.0,
            devotion: 0.30,
        },
    }
}

// ---------------------------------------------------------------------------
// THE RADIATION (C2d): the elf family — six kinds on ONE route.
//
// The theorem is The Delvers' and is restated here rather than cited, because
// it is what decides every number below. `tolerance_liebig`
// (`windows/worldgen/src/lib.rs`) floors temperature/moisture/insolation by
// `sovereignty_floor(mass, potency)` and passes elevation a literal `0.0`,
// while `hornvale_kernel::ConditionResponse::eval` is
// `floor + (1 - floor) * devotion * exp(-z²/2)`. So elevation's value never
// exceeds its own `devotion`, and the other three never fall below the floor:
//
//     elevation is the Liebig minimum on EVERY vertex
//         iff  devotion_elev < sovereignty_floor(mass, potency)
//
// **Every elf is authored BELOW its floor, and that is the whole strategy.**
// The Delvers' roster used both modes; this one uses one, deliberately, because
// a kind whose climate curves are discarded everywhere is the only kind that
// may honestly carry a `BiomeAffinity` — an affinity on top of a binding
// climate curve applies the same preference twice, and the campaign would
// measure the sum while attributing it to the affinity (spec §3.1). The
// climate-curve arm already exists in merged work (desert-dwarf, measured to
// bind on 67-91% of land and to buy almost no separation); the control is on
// the shelf, and authoring a people badly to manufacture another one is not
// this campaign's business.
//
//   kind          mass   sov. floor   dev_el   mode     identity carried by
//   desert-elf    50.0     0.421703     0.30   below    biome affinity
//   drow          52.0     0.424802     0.30   below    the realm gate
//   high-elf      55.0     0.429202     0.30   below    psyche/society/language
//   sea-elf       58.0     0.433335     0.30   below    biome affinity (shelf)
//   snow-elf      60.0     0.435955     0.30   below    biome affinity
//   wood-elf      55.0     0.429202     0.30   below    biome affinity
//
// (Floors computed LIVE from `hornvale_kernel::sovereignty_floor(Mass, 0.0)`,
// never copied from a plan — The Delvers' plan table was wrong in the fourth
// decimal for two of three. The live per-elf check is
// `windows/worldgen/tests/radiation_admission.rs`'s
// `every_elf_clears_the_affinity_precondition`, which prints this table on a
// green run and reddens on the value if a mass or a devotion ever flips the
// inequality.)
//
// Consequently **every temperature, moisture and insolation curve below is
// PREPARED**, in the organ-builder's sense the two below-floor dwarves already
// use: engraved, installed, connected to no rank. Each says so on its own line.
// Authoring an honest curve the model discards is the model; implying a
// preference it will never read is not.
//
// Elevation optima are cited against the same settleable-land percentile table
// every other people uses ([`ConditionNiche`]'s doc: p15=142, p25=621, p35=1004,
// p50=1561, p65=2166, p75=2651, p85=3251, p95=4148 m above sea level).
// ---------------------------------------------------------------------------

/// Desert elf condition niche: the arid-margin kind.
///
/// **PREPARED climate, affinity-carried identity.** `devotion_elev` is `0.30`
/// against a sovereignty floor of `0.421703` at 50.0 kg, so elevation is the
/// Liebig minimum on every vertex and the three curves below are never read. The
/// aridity is stated here because it is true of the kind, and it is stated as
/// PREPARED because the model will not act on it — the acting is the biome
/// affinity's job (Task 3), which is the one channel outside the minimum.
fn desert_elf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // hot, clear of the Desert tile's >= 20 C floor. PREPARED: never binds.
        temperature: ConditionResponse {
            optimum: 28.0,
            width: 10.0,
            devotion: 0.35,
        },
        // inside the Desert tile's < 0.20 moisture band, well below the
        // settleable median (0.49). PREPARED: never binds.
        moisture: ConditionResponse {
            optimum: 0.14,
            width: 0.18,
            devotion: 0.35,
        },
        // the upper end of the narrow settleable band (p5 0.19 / p95 0.31):
        // the subtropical desert belt's latitude, read honestly rather than
        // as a second aridity claim. PREPARED: never binds.
        insolation: ConditionResponse {
            optimum: 0.28,
            width: 0.30,
            devotion: 0.25,
        },
        // THE axis that binds: basin and basin-margin country just above p25
        // (621 m), wide, so elevation excludes almost nothing on its own and
        // the affinity is what shapes the field.
        elevation: ConditionResponse {
            optimum: 700.0,
            width: 2600.0,
            devotion: 0.30,
        },
    }
}

/// Drow condition niche: **Wood's elevation curve, and a dark half that is
/// dormant by measurement rather than by omission.**
///
/// **No depth is encoded here, and that is the point.** Depth below the surface
/// and height above sea level are different quantities: a deep chamber under a
/// mountain sits high above the sea, a shallow cave in a marsh sits low. The
/// Delvers committed exactly that fake and caught it — duergar authored at a
/// 300 m optimum to mean *deep* selected lowland marshes, and its toponymy came
/// back as an emergent finding until one question dissolved it. The toponymy was
/// reporting the authoring. So this kind's `elevation` **is** wood-elf's, taken
/// from that function rather than retyped, and
/// `radiation_admission.rs::drows_elevation_curve_is_woods_and_says_nothing_about_depth`
/// pins the equality. Drow's only authored separation from the surface elves is
/// the realm gate in [`habitat_realm_registry`].
///
/// **The insolation curve is authored for cave-dark and is DORMANT.** Not
/// omitted — dormant, and known to be. The Warren measured that going
/// underground improves a kind's moisture (.585 → .787) and insolation
/// (.467 → .840) readings and that the Liebig minimum never sees the
/// improvement, because the unfloored elevation axis is scarcer. Generalised: a
/// non-lethal preference cannot matter while an unfloored axis is scarcer. So
/// the dark preference below contributes nothing to placement today. It is
/// authored anyway, because it is true of the kind, and the two-tier
/// gate/modifier tolerance that would make it bind already exists in shadow
/// mode — `windows/worldgen/tests/warren_readout.rs` carries the tripwire that
/// reddens on purpose the day it starts to bind.
fn drow_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // a cave is thermally stable and cool: narrow, but PREPARED — never
        // binds.
        temperature: ConditionResponse {
            optimum: 13.0,
            width: 12.0,
            devotion: 0.30,
        },
        // near-saturated air, the reading The Warren measured underground.
        // PREPARED: never binds.
        moisture: ConditionResponse {
            optimum: 0.85,
            width: 0.25,
            devotion: 0.35,
        },
        // CAVE-DARK, and the family's only authored-but-dormant trait: an
        // optimum below the settleable-land p5 (0.19), narrow and the most
        // devoted curve in the family. PREPARED like the rest — and dormant
        // for the additional, measured reason in this function's doc comment,
        // not merely because it sits under the floor.
        insolation: ConditionResponse {
            optimum: 0.02,
            width: 0.10,
            devotion: 0.55,
        },
        // WOOD'S CURVE, byte for byte. Read from the function rather than
        // copied, so the two cannot drift apart in a later edit.
        elevation: wood_elf_condition_niche().elevation,
    }
}

/// High elf condition niche: **wood-elf's, unchanged and undecorated.**
///
/// High is the family's deliberate null control (spec §3.6). It diverges from
/// Wood in mind, society and language and in **nothing else** — same mass, same
/// potency, same resource vector, same curves — so that `Wood vs High` isolates
/// MIND exactly as `Wood vs Drow` isolates REALM. A roster of six kinds each
/// differing on several axes at once measures nothing.
///
/// Delegating to [`wood_elf_condition_niche`] rather than restating its numbers
/// is deliberate: an equality a later editor has to maintain by hand is an
/// equality that will eventually stop holding, and P3(a)'s primary arm is that
/// Wood's and High's capacity fields are **bit-identical**.
fn high_elf_condition_niche() -> ConditionNiche {
    wood_elf_condition_niche()
}

/// Sea elf condition niche: the productive shallow band.
///
/// **The one elf whose elevation curve may honestly say "shallow".** In the
/// ocean, depth *is* −(height above sea level), so the two quantities coincide
/// and a negative optimum is a literal reading rather than a stand-in for a
/// concept the axis cannot express — which is precisely what separates this
/// kind from Drow's case. Killer whale (−40 m) and giant squid (~−1263 m) are
/// the authoring precedent; this niche sits on the shelf with the former, not
/// in the abyss with the latter, because a *settled* people needs shallow
/// productive water. The affinity (Task 3) sharpens the ranking
/// `marine_forage_supply_field` already grades off the biome class
/// (`Upwelling => 1.0`, `CoralReef | KelpForest => 0.85`, `Epipelagic => 0.45`);
/// it does not contradict it.
///
/// **PREPARED climate**, like every elf: `devotion_elev` `0.30` against a floor
/// of `0.433335` at 58.0 kg.
fn sea_elf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // the temperate-to-warm shelf, spanning the reef/kelp span the marine
        // classifier splits at 20 C and 12 C. PREPARED: never binds.
        temperature: ConditionResponse {
            optimum: 16.0,
            width: 12.0,
            devotion: 0.30,
        },
        // ocean moisture is the banded circulation model's base wetness plus a
        // flat ocean-proximity bonus — a circulation-band artifact with no
        // marine ecological meaning (see the block comment above
        // `reef_shark_condition_niche`), so this is wide and barely devoted, as
        // every other marine kind's is. PREPARED: never binds.
        moisture: ConditionResponse {
            optimum: 0.75,
            width: 0.40,
            devotion: 0.20,
        },
        // insolation is a pure function of latitude and says nothing about
        // depth-linked light in this model; centred on the settleable median
        // and left wide. PREPARED: never binds.
        insolation: ConditionResponse {
            optimum: 0.25,
            width: 0.30,
            devotion: 0.25,
        },
        // THE axis that binds, and the family's only negative optimum: the
        // continental shelf. Narrow enough (300 m) that the abyss reads at the
        // floor, wide enough that the whole shelf is available.
        elevation: ConditionResponse {
            optimum: -60.0,
            width: 300.0,
            devotion: 0.30,
        },
    }
}

/// Snow elf condition niche: the cold-margin kind.
///
/// **PREPARED climate, affinity-carried identity** — `devotion_elev` `0.30`
/// against a floor of `0.435955` at 60.0 kg, the family's highest floor and
/// therefore its widest margin. The cold below is authored and discarded; the
/// tundra/ice preference is the affinity's to state (Task 3).
fn snow_elf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // below the settleable-land p5 (3.27 C): the cold tail, and the coldest
        // optimum any people carries. PREPARED: never binds.
        temperature: ConditionResponse {
            optimum: 0.0,
            width: 14.0,
            devotion: 0.35,
        },
        // dry-cold rather than wet-cold: below the settleable median (0.49),
        // because cold air holds little. PREPARED: never binds.
        moisture: ConditionResponse {
            optimum: 0.38,
            width: 0.30,
            devotion: 0.30,
        },
        // the low end of the settleable band (p5 0.19) — insolation is a pure
        // function of latitude, so this is the high-latitude reading and
        // nothing more. PREPARED: never binds.
        insolation: ConditionResponse {
            optimum: 0.19,
            width: 0.25,
            devotion: 0.25,
        },
        // THE axis that binds: low-to-middling ground between p15 (142 m) and
        // p25 (621 m) — the polar coast and the tundra plain, not the peak.
        elevation: ConditionResponse {
            optimum: 400.0,
            width: 2200.0,
            devotion: 0.30,
        },
    }
}

/// Wood elf condition niche: **the family's ancestral reading, and the row
/// three of the six are defined against.**
///
/// High elf takes this niche entire ([`high_elf_condition_niche`]); drow takes
/// its `elevation` and nothing else ([`drow_condition_niche`]). Editing a number
/// here therefore moves three kinds, which is the intended coupling and is why
/// both of those read it from this function rather than restating it.
///
/// **PREPARED climate**: `devotion_elev` `0.30` against a floor of `0.429202`
/// at 55.0 kg, so the three curves below are computed and discarded on every
/// vertex. Temperate forest is the affinity's claim to make (Task 3), not this
/// function's.
fn wood_elf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        // temperate, a little below the settleable median (14.59 C): the
        // deciduous and mixed-forest band. PREPARED: never binds.
        temperature: ConditionResponse {
            optimum: 12.0,
            width: 18.0,
            devotion: 0.35,
        },
        // wetter than the settleable median (0.49), which is what a closed
        // canopy needs. PREPARED: never binds.
        moisture: ConditionResponse {
            optimum: 0.62,
            width: 0.30,
            devotion: 0.35,
        },
        // at the settleable-land insolation median: this kind makes no claim
        // about latitude. PREPARED: never binds.
        insolation: ConditionResponse {
            optimum: 0.25,
            width: 0.30,
            devotion: 0.25,
        },
        // THE axis that binds: forested low country between p15 (142 m) and
        // p25 (621 m), wide, because the affinity — not the elevation — is
        // what is supposed to shape this kind's field.
        elevation: ConditionResponse {
            optimum: 500.0,
            width: 2200.0,
            devotion: 0.30,
        },
    }
}

/// How a species regulates body temperature — the **demand** axis, and the
/// only one allometry reads.
///
/// Split out of `MetabolicClass` by THE GOSSAN. That enum conflated this
/// with the supply axis ([`TrophicMode`]): its own doc says its job is to
/// select B₀ and the pace multiplier, and `Autotroph` — a supply value —
/// ended up grouped with `Endotherm` in `basal_metabolic_rate_w` because
/// allometry had nothing else to do with it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ThermalStrategy {
    /// Warm-blooded (mammal/bird analogue): high, temperature-stable basal rate.
    Endothermic,
    /// Cold-blooded (reptile/amphibian analogue): ~1/8 the basal rate; longer
    /// life per kg. Realized rate couples to ambient temperature.
    Ectothermic,
    /// Has a metabolism; its thermal behaviour is **not modelled**.
    ///
    /// Not a placeholder — it names a distinction shipped code already made
    /// and had no word for. `basal_metabolic_rate_w` groups the old
    /// `Autotroph` with `Endotherm`; `rise_at` groups it with `Ametabolic`.
    /// No single existing value preserves both, so the honest answer is a
    /// value that says the modelling call was never made. That call is
    /// tracked as BIO-autotroph-physics and is deliberately not this
    /// campaign's.
    ///
    /// **What the deleted `MetabolicClass::Autotroph` doc held, kept here
    /// because nothing else does.** A phototroph's basal rate is physically
    /// SURFACE/area-limited, so §4's universal ¾ mass exponent should not
    /// apply to it. It nonetheless does: [`crate::allometry`] gives this
    /// value `B0_ENDOTHERM` and a pace multiplier of 1.0, so the three
    /// shipped autotrophs (treant, twig-blight, shrieker) are computed
    /// exactly as endotherms of the same mass. `shrieker` is a fungus and so
    /// not a phototroph at all — a corpus error left standing on purpose,
    /// because a data fix inside a structural rename hides both. Making the
    /// physics real needs an area-scaling exponent and an autotroph `B0`
    /// calibrated against a photosynthetic-productivity anchor. The current
    /// divergence is pinned by `autotroph_is_computed_as_an_endotherm_today`
    /// in `tests/suite/coverage.rs`, so the fix will present as a visible
    /// diff.
    Unmodelled,
    /// No metabolism at all (construct/undead analogue): no life-history.
    ///
    /// Named `Absent` rather than `None` because
    /// `rise_at_couples_heat_to_thirst_per_metabolic_class` glob-imports this
    /// enum's variants, where a `None` would collide with `Option::None`.
    Absent,
}

/// Where a species gets its energy — the **supply** axis.
///
/// Split out of `MetabolicClass` by THE GOSSAN. Making a chemotroph
/// expressible is the whole of that campaign, and giving this axis a
/// SECOND consumer is rung 2 of the Underworld Larder.
///
/// **It has one production reader already**, acquired the moment the axis
/// existed: `hornvale_worldgen::prey_pressure_from` excludes phototrophs from
/// the prey base ("a plant is not a carnivore's prey") and asks
/// `trophic_mode == Phototrophic` to do it. That question was being asked of
/// the metabolic enum, and briefly of `ThermalStrategy::Unmodelled`, for want
/// of anywhere better to ask it — which is precisely the conflation this
/// split removes.
///
/// An axis nobody reads is how `MetabolicClass` rotted, so the guard in
/// `tests/suite/metabolic_pairs.rs` is additionally a genuine reader of every
/// kind's value, not merely a widening check. It runs in the WORKSPACE SUITE
/// today; it joins the commit gate once a green chamber run records its
/// baseline duration into `docs/timings/subfloor-roster.tsv`, which selects the
/// sub-floor tier by exact test name and excludes a test it has never timed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TrophicMode {
    /// Eats other organisms — prey, detritus, or their remains.
    Heterotrophic,
    /// Energy from light (plant-folk/fungal analogue).
    Phototrophic,
    /// Energy from chemical gradients in rock or water — a hydrothermal vent
    /// community, and the underworld's only possible productive base.
    ///
    /// **Witnessed by `xorn`** (rung 2 of the Underworld Larder,
    /// `tests/suite/metabolic_pairs.rs`): a thing that burrows through stone
    /// and eats only mineral is a chemolithotroph, and `Absent`/`Absent` was
    /// only ever the honest encoding available before this variant existed.
    /// The `niche` weight that actually feeds it (`CHEMOSYNTHATE`) is a
    /// separate, later change — see the `xorn` row's own comment.
    Chemotrophic,
    /// No metabolism at all. See [`ThermalStrategy::Absent`] for the naming.
    Absent,
}

/// Whether this describes something with **no metabolism at all** — the
/// question four separate sites ask, in two crates, with no shared name
/// (spec §4.3). That is the same drift that produced the mixed enum this
/// campaign split.
///
/// **THE DIRECTION THIS RELIES ON, STATED.** It reads the thermal axis alone,
/// so it is correct only while `ThermalStrategy::Absent` and
/// `TrophicMode::Absent` occur together and never apart. The type admits **six**
/// pairs where that is false — three `(Absent, <live trophic mode>)` and three
/// `(<live thermal strategy>, Absent)` (spec §4.4). Keep that number distinct
/// from the **twelve** UNSANCTIONED pairs (16 combinations less the 4
/// sanctioned rows): twelve is what the pair table refuses, six is what would
/// break *this function*, and only the second is the direction stated here.
/// What enforces it is `tests/suite/metabolic_pairs.rs`'s sanctioned-pair
/// table, which consults every kind's pair in the workspace suite — and in the
/// commit gate once a green chamber run records its baseline duration into
/// `docs/timings/subfloor-roster.tsv`. If that table is ever relaxed
/// to admit a `(Absent, …)` pair with a live trophic mode, this function is
/// the first place that goes wrong.
///
/// A two-axis signature was specified and is not available: every one of the
/// four call sites holds a `ThermalStrategy` and nothing else, because `Body`
/// carries only the axis the vessel layer reads.
/// type-audit: bare-ok(flag: return)
pub fn is_ametabolic(thermal: ThermalStrategy) -> bool {
    thermal == ThermalStrategy::Absent
}

/// How a kind's time-law quantities are scheduled against its mass (The Long
/// Age, spec §3). Mass and [`ThermalStrategy`] are the other two inputs to the
/// same law; this is the third, and it is the only one that is a free
/// authoring choice rather than a physical measurement.
///
/// An enum rather than a bare `f64` so that a *staged* schedule — a
/// metamorphic kind whose larval phase runs on its own curve before merging
/// into the adult one — arrives later as a new variant rather than a new
/// axis, changing no consumer's signature.
/// type-audit: bare-ok(ratio: Paced.factor)
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LifeSchedule {
    /// Pure allometry: every time-law quantity is a function of mass and
    /// metabolic class alone. Every kind in the roster today.
    Allometric,
    /// Allometry with an authored dimensionless pace factor: lifespan,
    /// maturity and reproductive tempo all stretch by `factor` at unchanged
    /// mass, and the basal metabolic rate does not move. Construct through
    /// [`LifeSchedule::paced`].
    Paced {
        /// The dimensionless stretch. `1.0` is [`LifeSchedule::Allometric`];
        /// above 1.0 is longer-lived and later-maturing, below 1.0 shorter.
        factor: f64,
    },
}

impl LifeSchedule {
    /// The default every kind carries unless authored otherwise.
    pub const ALLOMETRIC: LifeSchedule = LifeSchedule::Allometric;

    /// A paced schedule, or `None` if `factor` is not finite and strictly
    /// positive. A zero or negative factor is not a fast-living creature; it
    /// is a creature with no lifespan, which the time laws cannot express.
    /// type-audit: bare-ok(ratio: factor)
    pub fn paced(factor: f64) -> Option<LifeSchedule> {
        if factor.is_finite() && factor > 0.0 {
            Some(LifeSchedule::Paced { factor })
        } else {
            None
        }
    }

    /// The multiplier this schedule contributes to the time laws — `1.0` for
    /// [`LifeSchedule::Allometric`], so the default path is an IEEE-754
    /// no-op and every pre-campaign value is preserved bit-for-bit.
    /// type-audit: bare-ok(ratio: return)
    pub fn factor(self) -> f64 {
        match self {
            LifeSchedule::Allometric => 1.0,
            LifeSchedule::Paced { factor } => factor,
        }
    }
}

/// Which environmental frame a kind's carrying capacity is scored in (The
/// Warren). `domains/climate` owns the richer `Realm { medium, access }`;
/// this is deliberately NOT that type — a domain crate may not depend on a
/// sibling domain, and what the placement layer needs is a two-valued
/// question, not a realm vocabulary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HabitatRealm {
    /// Scored against the surface substrate — every kind not in the store.
    Surface,
    /// Scored against the subterranean substrate, and gated by whether the
    /// vertex holds a cave at all. A void that does not exist is not habitat.
    Subterranean,
}

impl HabitatRealm {
    /// The realm a kind absent from [`habitat_realm_registry`] carries.
    pub const SURFACE: HabitatRealm = HabitatRealm::Surface;
}

impl Component for HabitatRealm {}

/// The sparse habitat-realm component: **only** kinds that are not
/// `Surface` appear. Two rows today, both re-homed by The Deep Realm, whose
/// niches have been authored for darkness and near-saturation since that
/// campaign and scored against sunlit surface vertices until this one.
///
/// Sparse rather than a `BiosphereTraits` field because this has a single
/// consumer (`per_species_suitability`) which holds a slice, not a row —
/// the consumer-count rule The Long Age established, which gave the
/// opposite answer there because the life schedule had six consumers each
/// already holding the row.
pub fn habitat_realm_registry() -> ComponentStore<KindId, HabitatRealm> {
    [
        // A cave-dark, damp mineral-eater: C2a measured its subterranean fit
        // at ~2.5x its surface fit once the low-insolation proxy came out.
        (KindId("rust-monster"), HabitatRealm::Subterranean),
        // Climate-indifferent by potency rather than by curve — C2a measured
        // its ratio at 1.02, flat within noise. Listed because it LIVES
        // underground, not because scoring it there will move it.
        (KindId("xorn"), HabitatRealm::Subterranean),
        // The Delvers (C2c) briefly added two subterranean PEOPLES here and
        // withdrew them: a kind whose identity is DEPTH cannot be expressed
        // by an axis measured in metres above sea level (spec §11). They
        // return when the underworld has biomes.
        //
        // THE RADIATION (C2d): the drow — the store's first PEOPLED occupant,
        // and the first row whose consumer is settlement placement rather than
        // a readout (The Range carried the gate to `per_species_capacity_at`).
        // This is Drow's ONLY authored separation from the surface elves, and
        // it is deliberately its only one: the trap is not authoring a
        // subterranean kind, it is distinguishing two kinds by DEPTH, which
        // nothing in the model can say. One cave kind needs only to differ from
        // the surface, and the gate does that measurably. Drow's `elevation`
        // response is wood-elf's byte for byte, for exactly that reason.
        (KindId("drow"), HabitatRealm::Subterranean),
    ]
    .into_iter()
    .collect()
}

/// A kind's declared affinity across biomes (The Range). `domains/climate`
/// owns the richer `Biome` enum; this is deliberately NOT keyed by it — a
/// domain crate may not depend on a sibling domain. The store is keyed by
/// the biome's stable name string (`Biome::name()`, already used for concept
/// registration), and resolution against the live `Biome` value happens at
/// the composition root, which sits above both domains.
/// type-audit: bare-ok(ratio: default), bare-ok(ratio: by_biome)
#[derive(Clone, Debug, PartialEq)]
pub struct BiomeAffinity {
    /// The factor an unlisted biome takes. `1.0` is unrestricted; the value
    /// every kind absent from [`biome_affinity_registry`] carries for every
    /// biome.
    pub default: f64,
    /// Per-biome overrides, keyed by the biome's stable name string. A biome
    /// not listed here takes `default`.
    pub by_biome: Vec<(&'static str, f64)>,
}

impl BiomeAffinity {
    /// The affinity factor for `biome_name`: the listed override if present,
    /// else `default`.
    /// type-audit: bare-ok(identifier-text: biome_name), bare-ok(ratio: return)
    pub fn factor(&self, biome_name: &str) -> f64 {
        self.by_biome
            .iter()
            .find(|(name, _)| *name == biome_name)
            .map(|(_, factor)| *factor)
            .unwrap_or(self.default)
    }

    /// Build a row from a **preference** ladder and the kind's sovereignty
    /// floor, mapping each preference `p` in `[0, 1]` to the factor
    /// `floor + (1 - floor) * p` and setting `default` to `floor` itself
    /// (the `p == 0.0` case, which is why the elsewhere rung is never listed).
    ///
    /// This is the only constructor [`biome_affinity_registry`] uses, and the
    /// separation it enforces is the point: a row's author states a *shape*
    /// — [`AFFINITY_STRONGHOLD`] / [`AFFINITY_NEAR`] / [`AFFINITY_MARGINAL`],
    /// derived from the classifier at the kind's own authored climate — and
    /// never states a *level*. The level is
    /// [`hornvale_kernel::sovereignty_floor`], the model's existing statement
    /// of how much environmental unsuitability a creature's mass and potency
    /// buy it off; the registry's doc carries the derivation and the
    /// double-count argument in full.
    ///
    /// `floor` must be in `[0, 1)` — every value `sovereignty_floor` can
    /// return, whose ceiling is `0.95`. Outside that the mapping is not
    /// monotone and the caller has already lost the ladder, so this is a
    /// caller's contract in the same style as
    /// [`hornvale_kernel::ConditionResponse::eval`]'s, not a validated one.
    /// type-audit: bare-ok(ratio: floor), bare-ok(ratio: preferences), waiver(constructor-return)
    pub fn from_preferences(floor: f64, preferences: Vec<(&'static str, f64)>) -> Self {
        Self {
            default: floor,
            by_biome: preferences
                .into_iter()
                .map(|(name, p)| (name, floor + (1.0 - floor) * p))
                .collect(),
        }
    }
}

/// The affinity ladder's top rung, as a **preference** in `[0, 1]`: the biome
/// the classifier returns for the kind's own authored reading. Maps to a factor
/// of exactly `1.00` for every kind, whatever its sovereignty floor.
/// type-audit: bare-ok(ratio)
pub const AFFINITY_STRONGHOLD: f64 = 1.00;

/// The affinity ladder's second rung, as a **preference** in `[0, 1]`: one band
/// out in the classifier's lookup table, still recognisably the kind's country.
/// type-audit: bare-ok(ratio)
pub const AFFINITY_NEAR: f64 = 0.70;

/// The affinity ladder's third rung, as a **preference** in `[0, 1]`: two bands
/// out, or the right climate in the wrong form.
/// type-audit: bare-ok(ratio)
pub const AFFINITY_MARGINAL: f64 = 0.45;

impl Component for BiomeAffinity {}

/// The sparse biome-affinity component: **only** kinds with an authored,
/// non-uniform affinity across biomes appear. Eight occupants as of The
/// Radiation task 3 — The Range's two (gnoll, woolly-mammoth) plus the six
/// elves — and every other kind is unrestricted across every biome.
///
/// Sparse rather than a `BiosphereTraits` field because this has two
/// consumers (genesis placement and `best_home`), each of which holds a
/// slice, not a row — the consumer-count rule The Long Age established
/// (a `BiosphereTraits` field is for a component every consumer already
/// holds the row for; a sparse store is for a component only a few
/// slice-holding consumers read).
///
/// # The admission test: a row may only add a preference that is ABSENT
///
/// A biome affinity that merely restates a climate preference the model
/// already applies would double-count it, and the resulting movement would be
/// unattributable — it could be read as evidence for the mechanism when it was
/// only the old curve counted twice. So a kind is admitted here only when its
/// authored climate curves are **currently discarded**, and the discipline is
/// mechanical rather than a judgement call.
///
/// `tolerance_liebig` (`windows/worldgen/src/lib.rs`) evaluates temperature,
/// moisture and insolation floored by `sovereignty_floor(mass, potency)` and
/// evaluates **elevation floored by `0.0`**, then takes the minimum. A floored
/// axis can never read below its floor; an unfloored one peaks at its own
/// `devotion`. So whenever
///
/// ```text
///   elevation.devotion  <  sovereignty_floor(mass, potency)
/// ```
///
/// the elevation term is below the other three at **every** vertex of every
/// world, the minimum is elevation everywhere, and the temperature, moisture
/// and insolation curves contribute exactly nothing. Every occupant below
/// clears that bar, so each affinity restores a preference the model was
/// throwing away rather than duplicating one it already honours:
///
/// ```text
///   kind              mass kg   potency   floor      elev devotion   below?
///   gnoll               136.1      0.00   0.495384        0.40        YES
///   woolly-mammoth     6000.0      0.00   0.692367        0.50        YES
///   desert-elf           50.0      0.00   0.421703        0.30        YES
///   drow                 52.0      0.00   0.424802        0.30        YES
///   high-elf             55.0      0.00   0.429202        0.30        YES
///   sea-elf              58.0      0.00   0.433335        0.30        YES
///   snow-elf             60.0      0.00   0.435955        0.30        YES
///   wood-elf             55.0      0.00   0.429202        0.30        YES
/// ```
///
/// `windows/worldgen/tests/range_readout.rs` asserts this inequality for every
/// row in this registry, so a later edit to a mass or an elevation devotion
/// cannot quietly turn one of these rows into a double count.
/// `windows/worldgen/tests/radiation_admission.rs` asserts it a second time for
/// the six elves specifically, as a task-1 precondition.
///
/// # The ladder every row is authored on, and where its LEVEL comes from
///
/// A row carries two separable things. Its **shape** says which biomes are the
/// kind's country and how far out each one sits; that is derived per kind from
/// the classifier read at the kind's own authored climate (see below, and every
/// row says its own working). Its **level** says how much of a vertex the kind
/// still takes where the biome is not its country at all; that is derived from
/// the model and is not authored at any row.
///
/// The four steps are a **preference in `[0, 1]`**, not a factor:
///
/// ```text
///   1.00  stronghold  the biome the classifier returns for the kind's OWN
///                     authored reading
///   0.70  near        one band out, still recognisably the kind's country
///   0.45  marginal    two bands out, or the right climate in the wrong form
///   0.00  elsewhere   everything else
/// ```
///
/// and the factor the world actually sees is that preference mapped through the
/// kind's own **sovereignty floor**:
///
/// ```text
///   factor(biome) = floor + (1 - floor) * preference(biome)
///   floor         = hornvale_kernel::sovereignty_floor(mass, potency)
/// ```
///
/// so [`BiomeAffinity::default`] is the floor exactly, a stronghold is exactly
/// `1.00` for every kind however heavy, and the rows stay comparable rung for
/// rung. [`BiomeAffinity::from_preferences`] is the only constructor these rows
/// use, so the mapping cannot be bypassed by hand.
///
/// ## The derivation, and why this quantity
///
/// This is the model's own algebra rather than a new one.
/// [`hornvale_kernel::ConditionResponse::eval`] is
/// `floor + (1 - floor) * devotion * bump` — the same `sovereignty_floor`, in
/// the same position, mapping a preference in `[0, 1]` into `[floor, 1]`. And
/// `sovereignty_floor` is the model's single existing statement of *how much
/// environmental unsuitability a creature's mass and potency buy it off*:
/// "preference is the luxury of the unconstrained", a tiny material creature
/// environment-placed and a dragon self-determined. A biome affinity asks that
/// same question one level coarser — over classes rather than along an axis —
/// so it takes the same answer instead of an unrelated second one.
///
/// The consequence reads correctly as biology: a woolly mammoth is far less
/// diminished by being off its ground (`0.692`) than a wood elf is (`0.429`),
/// which is what six tonnes of homeostatic buffering ought to buy.
///
/// That is a plausibility reading and it is offered as nothing more — a sanity
/// check on the *sign and ordering* of the result, not evidence that the
/// quantity is the right one. Nothing measured here distinguishes
/// `sovereignty_floor` from any other function increasing in mass. An earlier
/// version of this paragraph called it "the check that it is the right quantity
/// and not merely an available one"; it is not a check at all, and the whole
/// reason this ladder needed re-deriving is that a number nobody had checked
/// looked settled. The argument for the quantity is the paragraph above: the
/// model already answers this exact question in
/// [`hornvale_kernel::ConditionResponse::eval`], and a second answer would be a
/// second model.
///
/// ## Why a PREFERENCE remap, and not simply "default = floor"
///
/// Because replacing the fourth step alone inverts the ladder for the two
/// heaviest occupants. Gnoll's floor is `0.495` and the mammoth's is `0.692`,
/// both above the old `0.45` marginal step — so a biome listed as *marginal*
/// would have scored **below** an unlisted one, and declaring a preference
/// would have been a penalty for holding it. Expressing the steps as a
/// preference and mapping the whole ladder keeps `stronghold > near > marginal
/// > default` true by construction for any floor in `[0, 1)`.
///
/// ## Why this is not the double count the admission test forbids
///
/// For any kind *not* in this registry it would be. For these eight it is not,
/// and the reason is the admission test above: every occupant has
/// `elevation.devotion < sovereignty_floor`, so `tolerance_liebig`'s minimum is
/// the **unfloored** elevation term at every vertex of every world and the floor
/// never reaches the product at all. The quantity is computed and discarded for
/// exactly the kinds this registry admits — which is the same sentence that
/// admitted them.
///
/// **Read "computed and discarded" narrowly, because a fix round of The
/// Radiation did not.** What is discarded is the floor computed *inside*
/// `hornvale_worldgen`'s `per_species_suitability`. The floor computed *here*,
/// to set a row's LEVEL, is applied **outside** `tolerance_liebig`'s minimum and
/// is not discarded at all — that is the entire point of the derivation above.
/// Collapsing the two produced the claim "mass does not reach this path's
/// output", which reached the campaign spec as a general rule and is refuted by
/// mutation: `desert-elf` at 500 kg instead of 50 reddens
/// `occupancy_readout_is_current` on twelve of its own rows, and `wood-elf` at
/// 550 kg instead of 55 moves thirty-six — its own twelve plus `drow`'s and
/// `high-elf`'s, which clone its row. **Mass reaches the field through the
/// affinity level for every kind whose row is self-derived**; only the two
/// clone-takers are exempt, and they are exempt because the row is not theirs.
///
/// That stays true by enforcement rather than by memory:
/// `range_readout.rs::every_occupant_has_climate_curves_the_minimum_currently_discards`
/// reddens the moment an edit to a mass, a potency or an elevation devotion
/// would let the floor bind, and that is precisely the edit that would turn
/// this reuse into a double count.
///
/// ## The number this replaced
///
/// The fourth step read `0.25` from The Range through The Radiation task 3, and
/// **it was never derived**: it appears in The Range's plan only inside
/// illustrative test-fixture code, its spec never names it, and it was then
/// adopted as house style for six more kinds. What exposed it was a
/// measurement, not a review — at `0.25` the six elf rows took seed 42's tithe
/// census from 552 occupation records to **193** (alive at now 192 → 100,
/// subordinations formed 232 → 41, tribute relations standing 83 → 17),
/// breaching four deliberate fidelity floors in `history_tithe.rs` and
/// `history_sundering.rs`. No single row caused it and the dose was not linear
/// (wood+high+drow alone gave 366, desert+sea+snow alone gave 660, all six gave
/// 193): the ladder had been calibrated on a store holding one settling people
/// in nine, and it now holds seven of fifteen.
///
/// "Level is gauge" is what made a bare constant look safe, and the sentence
/// that states its exemption has to be written carefully, because the obvious
/// version of it is false. **A UNIFORM rescale of a whole row cannot reorder
/// that kind's own ranking** — genesis and `best_home` rank vertices in the kind's
/// own units, so a constant factor reorders nothing for it. That is true, and it
/// is what The Radiation's chronicle says.
///
/// **A change to the LEVEL is not a uniform rescale, so it does not inherit
/// that exemption.** [`BiomeAffinity::from_preferences`] maps each preference to
/// `floor + (1 - floor) * p`, which holds a stronghold at exactly `1.00` while
/// pulling every lower rung down: it changes the ladder's CONTRAST, not its
/// scale. The factor then multiplies the capacity field per vertex, keyed on that
/// vertex's biome, so it reweights biome against every other condition in the
/// product — and vertices reorder. Measured, seed 42, the seven authored rows moved
/// from their shipped level to `0.6 x` their gap to `1.0`, with every shape held
/// fixed: **all seven row-carrying kinds have their own vertex ranking changed**,
/// and gnoll's argmax — the vertex `best_home` would pick — moves from 30312 to
/// 2276 with only 5 of its top 50 vertices surviving. All eleven row-LESS kinds are
/// bit-identical, which is the control: for them the factor is `1.0` at every
/// level and the level genuinely is gauge.
///
/// So the exemption is narrow and it is about the *shape*, not the consumer:
/// within-kind ranking is invariant under a uniform rescale of a whole row, and
/// under any level change for a kind carrying no row at all — and under nothing
/// else. For a kind with a shaped row, **the level is load-bearing in all four
/// consumers**, and here they are with the evidence for each.
///
/// 1. **Within-kind vertex ranking** (genesis's founding pool, `best_home`'s
///    choice of ground) — the consumer this paragraph used to exempt.
///    Evidence: the seven-of-seven reordering measured above.
/// 2. **`per_species_capacity`** — the factor multiplies the headcount that
///    becomes a settlement's POPULATION, and the history bake's volume is a
///    function of population. Evidence, immediately above: at the abandoned
///    `0.25` the six elf rows took seed 42's tithe census from 552 occupation
///    records to 193 and breached four deliberate fidelity floors.
/// 3. **`coexist::pack`, the per-kind share** — a vertex's share is `K^β`
///    normalized **across** kinds (`hornvale_worldgen`'s
///    `demography_report_with_beta_from` hands `per_species_k` to
///    `hornvale_demography::coexist::pack`), so rescaling ONE kind's level
///    moves EVERY kind's share in that vertex, not only its own. Evidence: The
///    Muster's positive control, recorded in full in the module doc of
///    `windows/worldgen/tests/beta_calibration_freeze.rs` — a level-only
///    change, every authored shape carried through unchanged, takes the mean
///    per-claimed-vertex diversity from 2.5789 to 1.4155 and reddens a
///    preregistered band. It takes a roster where every kind carries a row to
///    do it; at today's seven rows in eighteen kinds the level moves that
///    quantity by 0.9 without ever crossing an edge, which is a statement
///    about the guard's sensitivity and not about the level's reach. Full
///    reach is necessary and not sufficient: whether the crossing happens
///    also depends on which kind holds which ground, and the test file's
///    record gives three arrangements that move the mean by −0.94 to −1.22
///    of which only one crosses. Cite it as an existence proof. (Re-measured
///    after The Ell, 2026-08-12: every figure in that record — the 2.5789 →
///    1.4155 pair and all three arrangements among them — reproduces
///    bit-identically. Retyping `Fact.day` and moving the bake from years to
///    days does not reach this path; the test file's module doc says why.)
/// 4. **`coexist::pack`, the vertex's capacity** — that same vertex's total is a
///    plain **sum** of the present kinds' `K`, so the level moves the total,
///    and with it the wilderness fraction and the emigration pressure derived
///    from it, even where it moves no ordering at all.
///
/// For a kind carrying a shaped row, then, the level is load-bearing in all
/// four and gauge in none. That is why the phrase survived two campaigns: it
/// was a true statement about a UNIFORM rescale, restated as a statement about
/// the level and then applied to every consumer — and the one consumer it was
/// still believed to exempt turns out not to be exempt either. When a quantity
/// is described as gauge, name the transformation it is gauge under; "level is
/// gauge" names none, which is exactly how it stayed unfalsified for two
/// campaigns while being wrong about four consumers out of four.
///
/// What the level is **not** is two quantities. The Muster asked exactly that,
/// preregistered, and the sweep answered it: `level_k = λ · floor_k` satisfies
/// every band simultaneously at λ ∈ {0.25, 0.50, 1.00, 1.20} with the shipped
/// configuration interior to the grid, and λ = 1.0 reproduces the shipped world
/// byte-identically across five seeds. One quantity, in force — the freeze and
/// both result sets are in `book/src/chronicle/the-muster.md`.
///
/// Note what the derivation is **not**: it is not `0.50`, the value measured to
/// restore those floors. Restoring them was not the criterion, and whether the
/// derived level restores them is a reading, not a requirement.
///
/// "The classifier" is `classify_land` for a terrestrial kind, read at its
/// authored `(temperature.optimum, moisture.optimum)`. For the one marine
/// occupant (sea-elf) it is `classify_marine`, read at its authored
/// `(elevation.optimum, temperature.optimum)` — a different lookup, the same
/// discipline: name the class the code returns, do not name the class the
/// theme suggests.
///
/// **"One band out" is a step in the lookup TABLE, not a step in sigma.** The
/// classifier is a grid of thermal bands crossed with moisture bands, and the
/// ladder walks that grid. Which neighbour counts as *near* rather than
/// *marginal* is then settled by the clause the step already carries — "still
/// recognisably the kind's country" — read against whichever axis the kind's
/// own curve is committed to, with its authored sigma as the evidence. Gnoll's
/// row is the worked example: its moisture devotion (0.75) and its narrow
/// moisture sigma (0.12) are what put the two *dry* temperate bands at `near`
/// and the *wetter* savanna at `marginal`, even though savanna is the closer
/// step in sigma. Desert-elf, two rows down, makes the opposite call from the
/// same stronghold and says why.
///
/// The ceiling is `1.00` deliberately: an affinity here is a **penalty
/// relative to the unrestricted 1.0 every other kind carries**, never a boost,
/// so declaring one can only lower a kind's capacity. Permitting a factor above
/// `1.0` is the spec's pre-committed repair path if binding proves too weak; it
/// is not the shipped design, and taking it is a decision to record rather than
/// a knob to turn.
///
/// The floor is the kind's sovereignty floor and never `0.0` — and it cannot
/// reach `0.0`, since `sovereignty_floor` is strictly positive for any mass
/// above 1 kg. Zero is a **hard exclusion**, not a strong preference: genesis
/// filters its founding pool on `caps_now()[pidx].at(c) > 0.0` ("a proto-site a
/// people cannot feed is not a founding, it is a death two epochs later"). No
/// kind here means *never*: a gnoll war-band in a temperate forest is a rarity,
/// not an impossibility, and neither is a snow-elf outpost in a savanna. The
/// deep ocean is the case that most tempts a zero — sea-elf has no business in
/// the abyss — and it still takes the floor, because "no settlement has ever
/// been founded there" is a result the placement layer should produce, not an
/// input the registry should assert.
pub fn biome_affinity_registry() -> ComponentStore<KindId, BiomeAffinity> {
    // Every row's LEVEL, read live from the same biosphere row the capacity
    // path reads rather than transcribed as a literal. A kind's mass moving is
    // therefore a kind's affinity default moving, in the same commit, with no
    // second copy to forget — the failure mode the admission table's own
    // "the inputs live three places apart" note describes.
    let biosphere = biosphere_registry();
    let floor_of = |kind: &'static str| -> f64 {
        let bio = biosphere.get(&KindId(kind)).unwrap_or_else(|| {
            panic!("{kind} carries a biome affinity but has no biosphere row to derive its floor")
        });
        hornvale_kernel::sovereignty_floor(bio.mass, bio.potency)
    };
    // Wood's row is built ONCE and cloned into Drow and High, which is stronger
    // than three calls to one helper: the three are the same value, not three
    // values a helper currently happens to agree on. See the two rows below for
    // why each takes it, and note that this hands Drow WOOD's floor rather than
    // its own (52.0 kg vs 55.0 kg, 0.424802 vs 0.429202) — deliberately. Drow
    // is the REALM control; a control that differs environmentally, in level or
    // in shape, controls nothing.
    let wood = wood_elf_biome_affinity(floor_of("wood-elf"));
    [
        // THE RANGE (task 4), occupant one: the gnoll, the roster's strongest
        // DESERT authoring and — until this row — a people that selected no
        // arid vertex at all. Its niche states temperature optimum 29.0 °C at
        // devotion 0.80 and moisture optimum 0.12 at devotion 0.75, the most
        // committed hot-arid pair in `biosphere_registry`, and the admission
        // table above shows every bit of it discarded on every land vertex of
        // every world. Measured on seed 42 before this row existed: 20 gnoll
        // settlements, **zero** of them on an arid biome.
        //
        // `desert` is the stronghold by derivation, not by theme:
        // `classify_land` at gnoll's own authored reading (29.0 °C, moisture
        // 0.12) returns `Desert` exactly (hot band, moisture < 0.20).
        (
            KindId("gnoll"),
            BiomeAffinity::from_preferences(
                floor_of("gnoll"),
                vec![
                    // Stronghold — gnoll's own authored climate, classified.
                    ("desert", AFFINITY_STRONGHOLD),
                    // Near: the two temperate dry bands (moisture < 0.25 and
                    // 0.25-0.40). Right dryness, wrong thermal band — a gnoll
                    // steppe is a real place, a gnoll rainforest is not.
                    ("temperate-grassland", AFFINITY_NEAR),
                    ("shrubland", AFFINITY_NEAR),
                    // Marginal: the hot band's next step out (moisture
                    // 0.20-0.45) — the ground the savanna-authored giant-hyena
                    // is documented onto, which a desert pack raids and does
                    // not hold.
                    ("savanna", AFFINITY_MARGINAL),
                ],
            ),
        ),
        // THE RANGE (task 4), occupant two: the woolly mammoth — gnoll's defect
        // in the opposite climate. Temperature optimum **-25.0 °C at devotion
        // 0.85** is the roster's strongest COLD authoring, and the admission
        // table shows it discarded exactly as gnoll's desert authoring is: at
        // 6000 kg its sovereignty floor is 0.692367 and its elevation devotion
        // is 0.50, so elevation is the minimum on every vertex and the deep-cold
        // curve never binds.
        //
        // FAUNA, and that is the point of choosing it. `SocialForm::Gregarious`
        // never enters the bake's `SocialForm::Settled` roster, so this row
        // moves capacity and occupancy but places no settlement. Exactly ONE
        // peopled kind's placement moves in this campaign, which is what makes
        // the P1" readout attributable to gnoll's row alone.
        //
        // Two strongholds rather than one, because the kind's authored
        // temperature curve straddles a classification boundary rather than
        // sitting inside a band: `classify_land` returns `Ice` below -20 °C and
        // `Tundra` from there to freezing at moisture < 0.35, and the mammoth's
        // optimum (-25.0) and its one-sigma shoulder (-5.0) at its authored
        // moisture (0.30) land one in each. Splitting them would be an artifact
        // of where the lookup cuts, not a claim about the animal.
        (
            KindId("woolly-mammoth"),
            BiomeAffinity::from_preferences(
                floor_of("woolly-mammoth"),
                vec![
                    // Stronghold — the two cold-dry classes its own authored
                    // curve covers.
                    ("ice", AFFINITY_STRONGHOLD),
                    ("tundra", AFFINITY_STRONGHOLD),
                    // Near: the same cold band, wetter than its 0.30 optimum
                    // (moisture >= 0.35) — forest rather than open plain.
                    ("taiga", AFFINITY_NEAR),
                    // Marginal: cold, but reached by ALTITUDE rather than by
                    // latitude, and this is a 200 m lowland grazer. The right
                    // climate in the wrong form — the giant goat's country.
                    //
                    // This is the row that forced the ladder to be expressed as
                    // a preference: at 6000 kg the floor is 0.692367, so a
                    // literal `0.45` here would have scored alpine BELOW the
                    // eighteen biomes this row never mentions.
                    ("alpine", AFFINITY_MARGINAL),
                ],
            ),
        ),
        // ---------------------------------------------------------------
        // THE RADIATION (C2d, task 3): the six elves.
        //
        // The family is authored on the affinity route rather than the
        // condition-curve route, and the admission table above is why: every
        // elf's elevation devotion is 0.30 against a sovereignty floor of
        // 0.4217-0.4360, so the Liebig minimum is elevation on every vertex of
        // every world and each kind's temperature/moisture/insolation curves
        // are computed and discarded. Those curves are still authored — they
        // are true of the kind, and they are what the strongholds below are
        // DERIVED from — but the row is what reaches the world.
        //
        // THREE OF THE SIX CARRY THE SAME ROW, deliberately. Wood is the
        // ancestral reading; High is the family's MIND control and Drow its
        // REALM control, and a control that also differs environmentally
        // controls nothing. `radiation_affinity.rs` pins both equalities.
        // ---------------------------------------------------------------
        //
        // Stronghold by derivation: `classify_land` at desert-elf's own
        // authored reading (28.0 °C, moisture 0.14) returns `Desert` — hot
        // band (>= 20 °C), moisture < 0.20.
        //
        // THIS ROW DISAGREES WITH GNOLL'S FROM THE SAME STRONGHOLD, and the
        // disagreement is the ladder working rather than a slip. Gnoll holds
        // savanna at `marginal` because its moisture curve is a spike —
        // devotion 0.75, sigma 0.12, so its +1 sigma reading (0.24) is 16% of
        // the way into savanna's 0.20-0.45 band. Desert-elf's moisture curve
        // is devotion 0.35, sigma 0.18: its +1 sigma reading (0.32) is 48% of
        // the way in, squarely inside. A people this much less committed to
        // extreme aridity than a gnoll pack genuinely does hold the savanna
        // margin, so savanna is `near` here and `marginal` there.
        (
            KindId("desert-elf"),
            BiomeAffinity::from_preferences(
                floor_of("desert-elf"),
                vec![
                    // Stronghold — desert-elf's own authored climate,
                    // classified.
                    ("desert", AFFINITY_STRONGHOLD),
                    // Near: one moisture band wetter in the same hot tier,
                    // reached at +1 sigma of this kind's own moisture curve.
                    ("savanna", AFFINITY_NEAR),
                    // Near: one thermal band cooler at the same dryness,
                    // reached at -1 sigma of its temperature curve (18.0 °C).
                    // Right dryness, wrong thermal band — the same reading
                    // gnoll's row makes of the same biome.
                    ("temperate-grassland", AFFINITY_NEAR),
                    // Marginal: two bands out — cooler AND wetter. Scrub, not
                    // sand.
                    ("shrubland", AFFINITY_MARGINAL),
                ],
            ),
        ),
        // Drow: WOOD'S ROW — the same VALUE the surface elves carry, cloned,
        // level and shape together.
        //
        // Its own authored reading would NOT produce this row. `classify_land`
        // at (13.0 °C, moisture 0.85) returns `TemperateRainforest` — the
        // near-saturated cave air The Warren measured underground, classified
        // as the wettest temperate class. That class appears below at `near`,
        // one moisture band off wood's stronghold, so the derivation and the
        // authored row agree on direction and disagree on rank.
        //
        // The row is wood's anyway, because Drow is the family's REALM control
        // (spec §3.5): its only authored separation from the surface elves is
        // the `habitat_realm_registry` gate, so that P4's mutation — remove the
        // realm row, watch the separation collapse — isolates the gate. A row
        // derived from Drow's own cave-air reading would make that mutation
        // measure the gate plus a biome difference, and P4 would have no clean
        // reading. `radiation_affinity::drows_affinity_is_wood_elfs` pins it.
        //
        // Since The Radiation's derivation task the row's LEVEL is wood's too,
        // not drow's — see `wood_elf_biome_affinity`'s `floor` parameter.
        (KindId("drow"), wood.clone()),
        // High elf: WOOD'S ROW, and for the reason High exists at all. It is
        // the MIND control (spec §3.6) — same mass, same potency, same
        // resource vector, same curves, same affinity — so that Wood vs High
        // isolates psyche. P3(a) predicts the two capacity fields are
        // BIT-IDENTICAL, which is only possible if this row is wood's, not
        // merely similar to it. Cloned from one constructed value for the same
        // reason `high_elf_condition_niche` delegates: an equality maintained by
        // hand is an equality that eventually stops holding. (High and Wood are
        // both 55.0 kg, so this row's level would have been wood's either way —
        // drow's, one row up, would not.)
        (KindId("high-elf"), wood.clone()),
        // Sea elf: the ONE marine occupant, and therefore the one row derived
        // through `classify_marine` rather than `classify_land`.
        //
        // Stronghold by derivation: at this kind's own authored elevation
        // optimum (-60 m, i.e. 60 m of water) and its authored SST optimum
        // (16.0 °C), `classify_marine` falls THROUGH the reef arm (which wants
        // > 20 °C) and through the kelp arm (which wants < 12 °C) — 12-20 °C is
        // the documented gap in that precedence chain — and lands on
        // `Upwelling` where the upwelling flag is set, `Epipelagic` where it is
        // not. So upwelling is the stronghold and epipelagic is the same
        // reading with the flag off.
        //
        // The rank is monotone with `marine_forage_supply_field`, which already
        // grades the water Upwelling 1.0, reef/kelp 0.85, epipelagic 0.45,
        // mesopelagic 0.15, bathypelagic 0.05. The affinity SHARPENS that
        // ranking; it must not contradict it. Note what that costs: two rows
        // that both derive and both sharpen an existing grading is the closest
        // any row in this registry comes to the double-count the admission test
        // exists to prevent — it is admissible only because the supply field is
        // a RESOURCE axis and the affinity is a preference over classes, and
        // because the admission table above shows this kind's climate curves
        // discarded like every other occupant's.
        //
        // THE MARINE SUPPLY FIELD'S OWN GRADING WAS THE ALTERNATIVE ANCHOR
        // CONSIDERED FOR THE LADDER'S LEVEL, and this row is why it was
        // rejected. It is a marine PRODUCTIVITY gradient — the water's yield,
        // not a people's preference — it exists only for the ocean, so it can
        // anchor nothing on land, and here it would be a literal squaring:
        // sea-elf's resource axis already reads it, so grading the affinity by
        // it too would apply the identical curve twice to the one kind the
        // admission test flags as closest to a double count.
        //
        // THE DEEP CLASSES ARE UNLISTED AND THEREFORE AT THE DEFAULT — not
        // zero, and since the derivation task that default is 0.433335, sea-
        // elf's own sovereignty floor.
        // `radiation_affinity::the_sea_elf_is_confined_to_the_shelf_band` pins
        // both halves: the four shelf classes strictly above the default, the
        // five deep ones at or below it. Authored to the whole ocean this kind
        // would hold ~27,000 vertices against wood's ~800; on the shelf band it
        // holds ~1,425 (three-seed mean, 42/7/1234), which is the same order as
        // the rest of the family.
        (
            KindId("sea-elf"),
            BiomeAffinity::from_preferences(
                floor_of("sea-elf"),
                vec![
                    // Stronghold — sea-elf's own authored (depth, SST)
                    // reading, classified, on an upwelling vertex.
                    ("upwelling", AFFINITY_STRONGHOLD),
                    // Near: the same 0-200 m shelf, one SST step either side of
                    // the 12-20 °C gap this kind sits in — reef above, kelp
                    // below. Both are the shelf's productive communities and
                    // both grade 0.85 on the supply field.
                    ("coral-reef", AFFINITY_NEAR),
                    ("kelp-forest", AFFINITY_NEAR),
                    // Marginal: the identical depth and temperature with no
                    // upwelling — open shelf water. Right place, thin table.
                    ("epipelagic", AFFINITY_MARGINAL),
                ],
            ),
        ),
        // Snow elf: TWO strongholds, for the woolly mammoth's reason exactly.
        //
        // `classify_land` at this kind's own authored reading (0.0 °C, moisture
        // 0.38) returns `Taiga`: the 0-7 °C branch, moisture >= 0.30. Its
        // authored moisture optimum sits **0.03** above the sub-freezing
        // branch's tundra/taiga cut (0.35) and 0.08 above the 0-7 °C branch's
        // (0.30), against a sigma of 0.30 — so -1 sigma (0.08) is deep in
        // tundra and the optimum itself is barely inside taiga. The kind
        // straddles the cut rather than sitting in a band, and splitting the
        // pair would be an artifact of where the lookup cuts rather than a
        // claim about the people.
        //
        // NOTE FOR ANYONE RE-READING THE CAMPAIGN BRIEF: this is taiga+tundra,
        // not tundra+ice. Ice is `classify_land`'s < -20 °C class, which this
        // kind reaches only at -1.43 sigma; "snow elf therefore ice" is theme,
        // and the ladder's first line is the standing instruction against it.
        (
            KindId("snow-elf"),
            BiomeAffinity::from_preferences(
                floor_of("snow-elf"),
                vec![
                    // Stronghold — the two cold classes its own authored
                    // moisture curve straddles.
                    ("taiga", AFFINITY_STRONGHOLD),
                    ("tundra", AFFINITY_STRONGHOLD),
                    // Near: one thermal band colder — the permanent ice, at
                    // -1.43 sigma of its temperature curve. The margin this
                    // people is named for, and not the ground it holds.
                    ("ice", AFFINITY_NEAR),
                    // Marginal: cold reached by ALTITUDE rather than by
                    // latitude, above the tree line. The right climate in the
                    // wrong form — the same call the woolly mammoth's row
                    // makes of the same biome, three rows up.
                    ("alpine", AFFINITY_MARGINAL),
                ],
            ),
        ),
        // Wood elf: the family's ancestral row, and the row High and Drow are
        // defined against. The values live in `wood_elf_biome_affinity` so that
        // all three read one authored source; editing it moves three kinds,
        // which is the intended coupling.
        (KindId("wood-elf"), wood),
    ]
    .into_iter()
    .collect()
}

/// Wood elf's biome affinity — **and high elf's and drow's, byte for byte.**
///
/// Factored out of [`biome_affinity_registry`] for the same reason
/// [`high_elf_condition_niche`] delegates to [`wood_elf_condition_niche`]: P3(a)
/// predicts wood's and high's capacity fields are *bit-identical*, and an
/// equality a later editor has to maintain by hand is an equality that will
/// eventually stop holding.
///
/// Stronghold by derivation: `classify_land` at wood-elf's own authored reading
/// (12.0 °C, moisture 0.62) returns `TemperateForest` — the temperate band
/// (7-20 °C), moisture 0.40-0.75.
///
/// The near/marginal split reads the "still recognisably the kind's country"
/// clause against **closed canopy**, which is this kind's committed axis the way
/// dryness is gnoll's. Four classes sit one band out from temperate forest, and
/// they divide two and two:
///
/// ```text
///   temperate-rainforest   one moisture band wetter   forest   -> near
///   taiga                  one thermal band cooler    forest   -> near
///   tropical-seasonal-fst  one thermal band warmer    forest   -> marginal
///   shrubland              one moisture band drier    open     -> marginal
/// ```
///
/// The two `near` classes keep both the canopy and the temperate reading. The
/// two `marginal` ones each break exactly one of those: tropical seasonal
/// forest is the right form in the wrong thermal band, shrubland the right
/// thermal band in the wrong form. That is the ladder's own "two bands out, or
/// the right climate in the wrong form" line, read on the second clause.
///
/// Wood's curves are wide (sigma 18.0 °C, 0.30 moisture) and would put all four
/// within one sigma, which is exactly why the ladder walks the lookup table and
/// not the sigma — a sigma reading of this kind would rank nothing.
///
/// `floor` is **wood-elf's** sovereignty floor, and the caller hands it the same
/// value for all three kinds. Drow's own mass (52.0 kg) would give 0.424802
/// against wood's 0.429202; taking wood's is the same call the shape already
/// makes, for the same reason — a REALM control that also differs in level is
/// not a control. `radiation_affinity::drows_affinity_is_wood_elfs` pins the
/// whole row, level included.
fn wood_elf_biome_affinity(floor: f64) -> BiomeAffinity {
    BiomeAffinity::from_preferences(
        floor,
        vec![
            // Stronghold — wood-elf's own authored climate, classified.
            ("temperate-forest", AFFINITY_STRONGHOLD),
            // Near: one moisture band wetter, still closed canopy.
            ("temperate-rainforest", AFFINITY_NEAR),
            // Near: one thermal band cooler, still closed canopy — the boreal
            // forest.
            ("taiga", AFFINITY_NEAR),
            // Marginal: a forest in the wrong thermal band.
            ("tropical-seasonal-forest", AFFINITY_MARGINAL),
            // Marginal: the right thermal band with the canopy gone.
            ("shrubland", AFFINITY_MARGINAL),
        ],
    )
}

/// The biosphere component: every entity has one. The packer and the
/// habitat/niche-K layer read only these traits.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct BiosphereTraits {
    /// Adult individual body mass — the BIO-2 down-payment the coexistence
    /// packer reads to convert a settlement population into a standing
    /// biomass demand.
    pub mass: Mass,
    /// How this species regulates body temperature — the axis life-history
    /// allometry reads (spec BIO-2).
    pub thermal_strategy: ThermalStrategy,
    /// Where this species gets its energy (THE GOSSAN). Read in production by
    /// `hornvale_worldgen::prey_pressure_from`, which excludes phototrophs
    /// from the prey base; `tests/suite/metabolic_pairs.rs` reads every kind's
    /// value in the workspace suite — and in the commit gate once a green
    /// chamber run records its baseline duration — so the axis cannot rot the
    /// way `MetabolicClass` did.
    pub trophic_mode: TrophicMode,
    /// The species' ecological niche: a sparse utilization profile over the
    /// resource-axis basis (`hornvale_kernel::ecology`). Feeds the packer's
    /// Pianka overlap between coexisting species.
    pub niche: ResourceVector,
    /// The species' condition-tolerance profile over the v1 environmental
    /// axes (temperature/moisture/insolation/elevation). Coupled to the
    /// world's shipped fields by the worldgen K layer to place the species
    /// in space. See [`ConditionNiche`].
    pub condition_niche: ConditionNiche,
    /// Magical potency (0 = a purely material creature). Raises the species'
    /// sovereignty floor (`hornvale_kernel::sovereignty_floor`) so mighty
    /// creatures buffer environmental constraint. Authored as the creature's
    /// 5E adult Challenge Rating over 30 (`CR/30`), nonzero only for the
    /// supernatural set (dragon/plant/elemental — treant is 5E plant-typed);
    /// mundane beasts and the peoples carry 0 (count-free deliberately, like
    /// [`biosphere_registry`]'s doc 35 lines below — this line read "the four
    /// peoples" for four campaigns after there were nine, and was fixed only
    /// when its sibling was).
    /// type-audit: bare-ok(ratio: potency)
    pub potency: f64,
    /// How this creature organizes socially (universal; every kind carries
    /// one). `Settled` is the sole settlement-forming value and the successor
    /// to the old "has a psyche entry" proxy for peoplehood. (An enum, not a
    /// bare primitive — no type-audit verdict needed.)
    pub social_form: SocialForm,
    /// How this kind's time-law quantities are scheduled against its mass.
    /// [`LifeSchedule::Allometric`] for every kind authored so far — longevity
    /// is an authoring choice this roster has not yet made. (An enum, not a
    /// bare primitive — no type-audit verdict needed.)
    pub schedule: LifeSchedule,
}

// The biosphere / psyche / perception / family authoring lives in the four
// component registries below (`biosphere_registry` / `psyche_registry` /
// `perception_registry` / `family_of`). The former authored god-struct and
// its `registry()` are gone (ECS c3): kinds are keyed by `KindId`, and each
// component authors its own rows directly. The peopled speech data
// (articulation, lexicon, family proto) lives in `hornvale_language`.

impl Component for BiosphereTraits {}
impl Component for MindVector {}
impl Component for SocietyVector {}
impl Component for PerceptionVector {}
impl Component for Dispersion {}

/// The universal biosphere component, authored directly (one row per kind).
/// Every kind that competes for space has a biosphere row; this is the
/// canonical entity set. Mass is D&D 5E canon (kg); niche is a sparse
/// utilization profile over the resource-axis basis; each kind's climate-tile
/// rationale lives in its `*_condition_niche` helper above. Potency is the
/// creature's 5E adult Challenge Rating over 30 (`CR/30`), nonzero only for the
/// supernatural set (dragons, treant, xorn); mundane beasts and the peoples
/// carry 0 (count-free deliberately — it read "the six peoples" for three
/// campaigns after there were nine). `social_form` is the universal
/// social-organization axis
/// (spec §3.1, The Eremite): `Settled` for the fifteen peoples, `Sessile` for
/// the rooted autotrophs, `Gregarious` for the herding beasts, `Solitary`
/// for everything else (including the three dragons).
/// type-audit: bare-ok(identifier-text)
pub fn biosphere_registry() -> ComponentStore<KindId, BiosphereTraits> {
    [
        (
            KindId("goblin"),
            BiosphereTraits {
                mass: Mass::new(18.1).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.50), (ANIMAL_PREY, 0.50)]).unwrap(),
                condition_niche: goblin_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("kobold"),
            BiosphereTraits {
                mass: Mass::new(13.6).unwrap(),
                thermal_strategy: ThermalStrategy::Ectothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.55), (ANIMAL_PREY, 0.45)]).unwrap(),
                condition_niche: kobold_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("hobgoblin"),
            BiosphereTraits {
                mass: Mass::new(74.8).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.65), (ANIMAL_PREY, 0.35)]).unwrap(),
                condition_niche: hobgoblin_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("bugbear"),
            BiosphereTraits {
                mass: Mass::new(132.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.15), (ANIMAL_PREY, 0.85)]).unwrap(),
                condition_niche: bugbear_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("treant"),
            BiosphereTraits {
                mass: Mass::new(1800.0).unwrap(),
                thermal_strategy: ThermalStrategy::Unmodelled,
                trophic_mode: TrophicMode::Phototrophic,
                niche: ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap(),
                condition_niche: treant_condition_niche(),
                potency: 9.0 / 30.0, // treant — CR 9 (5E MM); potency = CR/30
                social_form: SocialForm::Sessile,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("twig-blight"),
            BiosphereTraits {
                mass: Mass::new(5.0).unwrap(),
                thermal_strategy: ThermalStrategy::Unmodelled,
                trophic_mode: TrophicMode::Phototrophic,
                niche: ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap(),
                condition_niche: twig_blight_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Sessile,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("giant-elk"),
            BiosphereTraits {
                mass: Mass::new(450.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_elk_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Gregarious,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("woolly-mammoth"),
            BiosphereTraits {
                mass: Mass::new(6000.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: woolly_mammoth_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Gregarious,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("giant-goat"),
            BiosphereTraits {
                mass: Mass::new(140.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_goat_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Gregarious,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("otyugh"),
            BiosphereTraits {
                mass: Mass::new(260.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap(),
                condition_niche: otyugh_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("xorn"),
            BiosphereTraits {
                mass: Mass::new(55.0).unwrap(),
                thermal_strategy: ThermalStrategy::Absent,
                trophic_mode: TrophicMode::Chemotrophic,
                niche: ResourceVector::new(&[(MINERAL, 1.0)]).unwrap(),
                condition_niche: xorn_condition_niche(),
                potency: 5.0 / 30.0, // xorn — CR 5 (5E MM); potency = CR/30
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
                // Chemotrophic (rung 2 of the Underworld Larder): burrows
                // through stone and lives IN the substrate, not on it, eating
                // only mineral — a chemolithotroph, not merely ametabolic.
                // `thermal_strategy` stays `Absent` (unchanged; ametabolism is
                // a thermal-axis fact and `is_ametabolic` reads that axis
                // only), so xorn's BMR and the life-history golden do not
                // move. Its `niche` still carries no `CHEMOSYNTHATE` weight —
                // that supply is wired in a later task, in the same commit as
                // the field that feeds it — so this row is witnessed but not
                // yet fed; a witnessed-but-unfed niche is the deliberate,
                // temporary gap that task's brief owns. rust-monster shares
                // the pure-MINERAL niche but stays Terrestrial/Heterotrophic —
                // it walks the surface eating metal, not gaining energy from
                // a chemical gradient.
            },
        ),
        (
            KindId("rust-monster"),
            BiosphereTraits {
                mass: Mass::new(90.0).unwrap(),
                thermal_strategy: ThermalStrategy::Ectothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(MINERAL, 1.0)]).unwrap(),
                condition_niche: rust_monster_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("white-dragon"),
            BiosphereTraits {
                mass: Mass::new(2200.0).unwrap(), // 5E adult white dragon
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(), // obligate apex
                condition_niche: white_dragon_condition_niche(),
                potency: 13.0 / 30.0, // adult white dragon — CR 13 (5E MM); potency = CR/30
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("red-dragon"),
            BiosphereTraits {
                mass: Mass::new(2700.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: red_dragon_condition_niche(),
                potency: 17.0 / 30.0, // adult red dragon — CR 17 (5E MM); potency = CR/30
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("black-dragon"),
            BiosphereTraits {
                mass: Mass::new(2200.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: black_dragon_condition_niche(),
                potency: 14.0 / 30.0, // adult black dragon — CR 14 (5E MM); potency = CR/30
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("owlbear"),
            BiosphereTraits {
                mass: Mass::new(450.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: owlbear_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        // The Vacancy (T7): seven terrestrial fauna. See the block comment
        // above `giant_scorpion_condition_niche` for the shared design notes
        // (the NPP/desert supply trap and the elevation percentile table).
        (
            KindId("giant-scorpion"),
            BiosphereTraits {
                mass: Mass::new(300.0).unwrap(),
                thermal_strategy: ThermalStrategy::Ectothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 0.3), (DETRITUS, 0.7)]).unwrap(),
                condition_niche: giant_scorpion_condition_niche(),
                potency: 0.0, // giant scorpion — CR 3 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("giant-hyena"),
            BiosphereTraits {
                mass: Mass::new(160.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: giant_hyena_condition_niche(),
                potency: 0.0, // giant hyena — CR 1 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Gregarious,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("dire-wolf"),
            BiosphereTraits {
                mass: Mass::new(150.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: dire_wolf_condition_niche(),
                potency: 0.0, // dire wolf — CR 1 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Gregarious,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("rhinoceros"),
            BiosphereTraits {
                mass: Mass::new(2300.0).unwrap(), // real white rhinoceros adult male average
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: rhinoceros_condition_niche(),
                potency: 0.0, // rhinoceros — CR 2 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("giant-constrictor-snake"),
            BiosphereTraits {
                mass: Mass::new(500.0).unwrap(),
                thermal_strategy: ThermalStrategy::Ectothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: giant_constrictor_snake_condition_niche(),
                potency: 0.0, // giant constrictor snake — CR 2 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("carrion-crawler"),
            BiosphereTraits {
                mass: Mass::new(200.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap(),
                condition_niche: carrion_crawler_condition_niche(),
                potency: 0.0, // carrion crawler — CR 2 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("shrieker"),
            BiosphereTraits {
                mass: Mass::new(35.0).unwrap(),
                thermal_strategy: ThermalStrategy::Unmodelled,
                trophic_mode: TrophicMode::Phototrophic,
                niche: ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap(),
                condition_niche: shrieker_condition_niche(),
                potency: 0.0, // shrieker — CR 0 (5E MM); CR/30 = 0 regardless of set
                social_form: SocialForm::Sessile,
                schedule: LifeSchedule::Allometric,
            },
        ),
        // The Vacancy (T8): four marine kinds plus the amphibious proof case.
        // See the block comment above `reef_shark_condition_niche` for the
        // shared design notes (the measured ocean-depth-by-biome table and
        // the marine supply/temperature/insolation/moisture caveats).
        (
            KindId("reef-shark"),
            BiosphereTraits {
                mass: Mass::new(18.5).unwrap(), // real grey reef shark average
                thermal_strategy: ThermalStrategy::Ectothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(MARINE_FORAGE, 1.0)]).unwrap(),
                condition_niche: reef_shark_condition_niche(),
                potency: 0.0, // reef shark — CR 1/2 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("giant-octopus"),
            BiosphereTraits {
                mass: Mass::new(180.0).unwrap(),
                thermal_strategy: ThermalStrategy::Ectothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(MARINE_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_octopus_condition_niche(),
                potency: 0.0, // giant octopus — CR 1 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("killer-whale"),
            BiosphereTraits {
                mass: Mass::new(5400.0).unwrap(), // real adult male average (upper of range)
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(MARINE_FORAGE, 1.0)]).unwrap(),
                condition_niche: killer_whale_condition_niche(),
                potency: 0.0, // killer whale — CR 3 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Gregarious,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("giant-squid"),
            BiosphereTraits {
                mass: Mass::new(250.0).unwrap(), // real Architeuthis dux, large-adult estimate
                thermal_strategy: ThermalStrategy::Ectothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(MARINE_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_squid_condition_niche(),
                potency: 0.0, // giant squid — CR 7 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        (
            KindId("giant-crocodile"),
            BiosphereTraits {
                mass: Mass::new(1000.0).unwrap(),
                thermal_strategy: ThermalStrategy::Ectothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // the amphibious proof case: MARINE_FORAGE (sea) plus
                // ANIMAL_PREY (land) — no special case, see the condition
                // niche's doc comment.
                niche: ResourceVector::new(&[(MARINE_FORAGE, 0.4), (ANIMAL_PREY, 0.6)]).unwrap(),
                condition_niche: giant_crocodile_condition_niche(),
                potency: 0.0, // giant crocodile — CR 5 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
                schedule: LifeSchedule::Allometric,
            },
        ),
        // The Vacancy (T9): the fifth people. `family_of` follows kobold's
        // shape — a singleton family, no `family_proto` entry (see the
        // `family_of` doc below).
        (
            KindId("gnoll"),
            BiosphereTraits {
                // 5E MM prints no weight (the same finding Task 7 made for
                // every beast, generalizing here to a humanoid): the MM's own
                // stat block and flavor text give CR 1/2 and "stands well
                // over six feet tall" but no number in pounds. The Midgard
                // Heroes Handbook's gnoll entry (an OGL 5E sourcebook, via
                // the 5esrd.com SRD mirror) is the closest sourced figure —
                // "females range 7 to 8 feet and weigh more than 250 pounds;
                // males average 6 inches and 30 pounds less" — consistent
                // with the ~300 lb figure repeated across independent
                // secondary D&D compilations for the species' adult average.
                // 300 lb = 136.1 kg used here: sourced from the best
                // available published numbers, not authored from scratch.
                mass: Mass::new(136.1).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // mixed omnivore weighted toward ANIMAL_PREY — a pack
                // hunter that also forages, not a pure predator (contrast
                // bugbear's 0.85 ANIMAL_PREY lean).
                niche: ResourceVector::new(&[(ANIMAL_PREY, 0.65), (PLANT_FORAGE, 0.35)]).unwrap(),
                condition_niche: gnoll_condition_niche(),
                potency: 0.0, // gnoll — CR 1/2 (5E MM); mundane like the other five peoples
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::Allometric,
            },
        ),
        // The Generalist (C2-0): the sixth people, and the roster's first
        // competitor with no refuge. Mass is 5E canon for a Medium humanoid.
        // The trophic split is deliberately close to goblin's 0.50/0.50 —
        // humans are not trophically novel, and the generalism this kind
        // exists to test lives on the CONDITION axes, not the resource axes.
        (
            KindId("human"),
            BiosphereTraits {
                mass: Mass::new(70.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.55), (ANIMAL_PREY, 0.45)]).unwrap(),
                condition_niche: human_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::Allometric,
            },
        ),
        // THE DELVERS (C2c): the dwarf family, three kinds at once — the
        // roster's seventh through ninth peoples and its first
        // multi-member family since goblinoid. (Mountain and Duergar were
        // authored alongside them and withdrawn before merge: both are
        // DEPTH kinds and the model has no depth axis to seat them on —
        // spec §11.)
        //
        // **`ResourceVector` is where the differentiation actually lands.**
        // BIO-supply-drowns-niche records that supply magnitude spans orders
        // of magnitude while tolerance is bounded in `[0, 1]`; read as a
        // defect that is the row's complaint, read as a lever it is this
        // family's loudest channel. A miner, a farmer and a scavenger differ
        // by orders of magnitude where a hot dwarf and a cold dwarf differ by
        // nothing (spec §3.1).
        //
        // **All three are `LifeSchedule::paced(4.0)`, and the schedule is a
        // FAMILY trait.** Long life is a dwarf trait, not a cave trait —
        // the withdrawn cave kinds were authored on the same factor for
        // exactly that reason, so their departure moves nothing here. This is
        // `LifeSchedule::Paced`'s first occupant — The Long Age shipped the
        // variant with an empty witness list and named C2c as the campaign
        // that must fill it. Measured through `hornvale_species::life_history`
        // at `ThermalStrategy::Endothermic`:
        //
        //   kind             mass   allometric   paced(4.0)   maturity   generation
        //   gully-dwarf      62.0      66.95 y     267.79 y     53.56 y     117.83 y
        //   desert-dwarf     66.0      68.00 y     272.01 y     54.40 y     119.68 y
        //   hill-dwarf       70.0      69.01 y     276.04 y     55.21 y     121.46 y
        //
        // The factor has to CLEAR something to be read: `cascade_regime_of`
        // (`windows/worldgen/src/lib.rs`) switches a Settled people onto the
        // slow language-drift regime at `LIFESPAN_THRESHOLD_YEARS = 120.0`, and
        // a 70 kg endotherm reads 69.01 y under pure allometry. 4.0 clears the
        // threshold on all three with a wide margin, so the schedule is
        // observable rather than decorative.
        //
        // **`pace_of_life` and `reproductive_tempo` SATURATE at exactly 1.0**
        // at this factor, because `factor × raw × pace_multiplier` passes
        // `MAX_PACE_MULTIPLIER = 1.5` (`allometry.rs`). Measured, not
        // predicted: all three read 1.0000 on both. That is deliberate and
        // stated (The Long Age §3.5) — saturating there is preferred to
        // rescaling every kind in the roster — and it means those two channels
        // are uninformative for a dwarf. `lifespan`, `age_at_maturity` and
        // `generation_length` stay linear and unbounded, which is where the
        // longevity is actually legible.
        (
            KindId("desert-dwarf"),
            BiosphereTraits {
                mass: Mass::new(66.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // Sums to 1.00, like every other kind in the roster. A
                // forager leaning on plants over game, in the same
                // proportion a sparse-ground people would.
                //
                // An earlier draft made this sum to 0.60 to express "takes
                // less from a vertex than a farmer does". Reverted, for two
                // reasons. **Ecologically it puts the scarcity in the wrong
                // object**: a desert is poor because the *vertex* supplies
                // little, which the supply field already says, not because
                // the people are worse at extraction — that is a claim about
                // the creature, and a different one. **And it would confound
                // this kind's whole purpose.** Desert-dwarf is the campaign's
                // demonstrator that an authored climate niche can bind
                // (spec §10.2); supply is the dominant channel, spanning
                // orders of magnitude against tolerance's bounded [0,1]
                // (`BIO-supply-drowns-niche`), so a 40% uniform supply cut
                // would leave its difference from hill-dwarf partly
                // attributable to supply rather than to climate. Correlation
                // is scale-invariant so the readout's number would have
                // survived; the *interpretation* would not.
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.58), (ANIMAL_PREY, 0.42)]).unwrap(),
                condition_niche: desert_dwarf_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(4.0).unwrap(),
            },
        ),
        (
            KindId("gully-dwarf"),
            BiosphereTraits {
                mass: Mass::new(62.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // A SURFACE scavenger, on the axis otyugh, carrion-crawler
                // and shrieker hold. It shares `DETRITUS` with the two cave
                // dwarves but arrives at it from the opposite direction —
                // they farm fungus in the dark, this one works refuse in the
                // light — so it keeps the largest `PLANT_FORAGE` share of
                // the three and the smallest `DETRITUS` one. What actually
                // separates it from them is the realm gate and an elevation
                // optimum eight hundred metres below hill-dwarf's; the diet
                // axis is not doing that work.
                niche: ResourceVector::new(&[
                    (DETRITUS, 0.50),
                    (PLANT_FORAGE, 0.35),
                    (ANIMAL_PREY, 0.15),
                ])
                .unwrap(),
                condition_niche: gully_dwarf_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(4.0).unwrap(),
            },
        ),
        (
            KindId("hill-dwarf"),
            BiosphereTraits {
                mass: Mass::new(70.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // PLANT_FORAGE-dominant: a farmer and herder, leaning harder
                // on plants than human's 0.55/0.45 and much harder than
                // bugbear's predatory 0.15/0.85.
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.70), (ANIMAL_PREY, 0.30)]).unwrap(),
                condition_niche: hill_dwarf_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(4.0).unwrap(),
            },
        ),
        // THE RADIATION (C2d): the six elves, the roster's largest family and
        // the last of the peoples programme. Every row below is `Settled`,
        // `Endotherm`, potency 0.0, and paced at 5.0 — the family's shared
        // body reading, with the differences carried by the niche vector, the
        // condition niche and (Task 3) the biome affinity.
        //
        // **Masses sit in the 45-70 kg band** the spec's floor table covers,
        // and every one of them is deliberately lighter than the dwarves'
        // 62-70: an elf is the roster's slight people. The band matters
        // because mass sets `sovereignty_floor`, and the family's entire
        // authoring strategy is to sit BELOW that floor on elevation devotion
        // (see the C2d block comment above `desert_elf_condition_niche`).
        //
        // **`paced(5.0)`, harder than the dwarves' 4.0, and the consequence is
        // asserted rather than assumed.** These are the longest-lived people
        // in the roster. But `cascade_regime_of`
        // (`windows/worldgen/src/lib.rs`) is BINARY at
        // `LIFESPAN_THRESHOLD_YEARS = 120.0`, and the dwarves already clear it
        // at 4.0 (268-276 y), so the extra pacing buys **nothing** in language
        // drift. That is a stated null, not an oversight: N1 in
        // `windows/worldgen/src/lib.rs`'s test module
        // (`pacing_elves_harder_than_dwarves_changes_no_drift_regime`) asserts
        // all three clauses — the elves are on the slow regime, doubling the
        // factor moves nothing, and dropping to pure allometry DOES move it,
        // which is what keeps the first two from being satisfied by a branch
        // that never fires. `pace_of_life` and `reproductive_tempo` saturate
        // at exactly 1.0 here as they do for a dwarf (`MAX_PACE_MULTIPLIER`);
        // `lifespan`, `age_at_maturity` and `generation_length` stay linear,
        // and that is where the longevity is legible.
        (
            KindId("desert-elf"),
            BiosphereTraits {
                mass: Mass::new(50.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // an arid-margin forager, near the roster's midpoint between
                // plants and game — a sparse ground supports neither
                // exclusively.
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.55), (ANIMAL_PREY, 0.45)]).unwrap(),
                condition_niche: desert_elf_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(5.0).unwrap(),
            },
        ),
        (
            KindId("drow"),
            BiosphereTraits {
                mass: Mass::new(52.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // `DETRITUS`-dominant: the fungus axis gully-dwarf, otyugh,
                // carrion-crawler and shrieker share. This kind arrives at it
                // from the direction the withdrawn cave dwarves would have —
                // it farms in the dark rather than working refuse in the
                // light — so it keeps the largest `DETRITUS` share of any
                // people and the smallest `PLANT_FORAGE` one.
                niche: ResourceVector::new(&[
                    (DETRITUS, 0.50),
                    (ANIMAL_PREY, 0.30),
                    (PLANT_FORAGE, 0.20),
                ])
                .unwrap(),
                condition_niche: drow_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(5.0).unwrap(),
            },
        ),
        (
            KindId("high-elf"),
            BiosphereTraits {
                // WOOD'S BODY, EXACTLY — mass, class, niche, curves, potency
                // and schedule all identical. High is the family's null
                // control (spec §3.6): it diverges in mind, society and
                // language and in nothing else, so that `Wood vs High`
                // isolates MIND the way `Wood vs Drow` isolates REALM. Mass
                // sets the sovereignty floor and the resource vector sets
                // supply; a difference in either would silently make P3(a) a
                // two-variable comparison.
                // `radiation_admission.rs::wood_and_high_differ_in_mind_and_not_in_body`
                // asserts the equality in one direction and the psyche/society
                // divergence in the other.
                mass: Mass::new(55.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.65), (ANIMAL_PREY, 0.35)]).unwrap(),
                condition_niche: high_elf_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(5.0).unwrap(),
            },
        ),
        (
            KindId("sea-elf"),
            BiosphereTraits {
                mass: Mass::new(58.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // THE ONLY ELF ON `MARINE_FORAGE`, and it must be: that axis
                // is what `marine_forage_supply_field` pays out on a water
                // vertex, so a sea people without a weight on it would draw zero
                // supply everywhere it lives and its shelf affinity would
                // multiply zero — authored, admitted and void. The small
                // terrestrial residue is the shore: a settled coastal people
                // does not live entirely in the water.
                // `radiation_admission.rs::the_sea_elf_draws_on_the_marine_supply_axis`
                // pins both halves — that this kind has the weight, and that
                // no other elf does.
                niche: ResourceVector::new(&[
                    (MARINE_FORAGE, 0.75),
                    (ANIMAL_PREY, 0.15),
                    (PLANT_FORAGE, 0.10),
                ])
                .unwrap(),
                condition_niche: sea_elf_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(5.0).unwrap(),
            },
        ),
        (
            KindId("snow-elf"),
            BiosphereTraits {
                mass: Mass::new(60.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // the family's only `ANIMAL_PREY`-dominant row: a cold
                // people's calories come from animals, because a tundra grows
                // very little a person can eat.
                niche: ResourceVector::new(&[(ANIMAL_PREY, 0.65), (PLANT_FORAGE, 0.35)]).unwrap(),
                condition_niche: snow_elf_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(5.0).unwrap(),
            },
        ),
        (
            KindId("wood-elf"),
            BiosphereTraits {
                mass: Mass::new(55.0).unwrap(),
                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                // `PLANT_FORAGE`-dominant, at human's temper rather than
                // hill-dwarf's 0.70: a forest people gathers more than it
                // farms, and hunts the rest. Shared with high-elf, exactly.
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.65), (ANIMAL_PREY, 0.35)]).unwrap(),
                condition_niche: wood_elf_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
                schedule: LifeSchedule::paced(5.0).unwrap(),
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// The individual-mind component — authored directly, present for every
/// minded kind (the fifteen settling peoples and the three solitary dragons).
/// Goblin's row happens to sit at [`MindVector::MANIKIN`] — a fact about
/// goblin's authorship, not about what the manikin is.
/// type-audit: bare-ok(identifier-text)
pub fn psyche_registry() -> ComponentStore<KindId, MindVector> {
    [
        (
            KindId("goblin"),
            MindVector {
                threat_response: 0.5,
                deliberation_latency: 0.5,
                time_horizon: 0.5,
            },
        ),
        (
            KindId("kobold"),
            MindVector {
                threat_response: 0.8,
                deliberation_latency: 0.7,
                time_horizon: 0.8,
            },
        ),
        (
            KindId("hobgoblin"),
            MindVector {
                threat_response: 0.7,
                deliberation_latency: 0.6,
                time_horizon: 0.5,
            },
        ),
        (
            KindId("bugbear"),
            MindVector {
                threat_response: 0.8,
                deliberation_latency: 0.4,
                time_horizon: 0.3,
            },
        ),
        // The Eremite: the three chromatic dragons carry a mind though they
        // never settle or speak — a solitary temperament, one shared chromatic
        // profile (per-chromatic differentiation is a deferred refinement).
        (
            KindId("white-dragon"),
            MindVector {
                threat_response: 0.95,     // an apex — stands, never flees
                deliberation_latency: 0.5, // banked dial, at the midpoint
                time_horizon: 0.90,        // a centuries-long hoarder
            },
        ),
        (
            KindId("red-dragon"),
            MindVector {
                threat_response: 0.95,
                deliberation_latency: 0.5,
                time_horizon: 0.90,
            },
        ),
        (
            KindId("black-dragon"),
            MindVector {
                threat_response: 0.95,
                deliberation_latency: 0.5,
                time_horizon: 0.90,
            },
        ),
        // The Vacancy (T9): the fifth people.
        (
            KindId("gnoll"),
            MindVector {
                // stands and fights rather than fleeing — a frenzied,
                // reckless pack predator (5E's Rampage trait reads the same
                // temperament from the mechanics side).
                threat_response: 0.85,
                // impulsive, not deliberate: decisions arrive fast, driven
                // by opportunity rather than careful weighing.
                deliberation_latency: 0.2,
                // short-horizon: a high-variance forager cannot plan far
                // past the next windfall, so it does not try to.
                time_horizon: 0.2,
            },
        ),
        // The Generalist (C2-0). `threat_response` sits AT the manikin by
        // authorship, not by default: humans genuinely both flee and stand,
        // and The Manikin moved the model to the rung where a kind may
        // coincide with the reference vector. Stated explicitly because a
        // people welded to the identity element is the bug that campaign
        // removed.
        (
            KindId("human"),
            MindVector {
                threat_response: 0.5,
                deliberation_latency: 0.6,
                time_horizon: 0.75,
            },
        ),
        // THE DELVERS (C2c): the three dwarves. The family's shared reading is
        // a LONG `time_horizon` — every one of them sits above human's 0.75 —
        // and that is not decoration: `paced(4.0)` gives them 268-276 years
        // and a 118-121 year generation length, so a dwarf genuinely plans
        // past the span a human can. The differences among the three are the
        // ecology each lives in, not the family they belong to.
        (
            KindId("desert-dwarf"),
            MindVector {
                // patient rather than pugnacious: a sparse-forage people
                // cannot afford to lose members to a fight it could walk
                // away from.
                threat_response: 0.45,
                // slow and considered — the family reading, and the correct
                // one for a people whose next water is a day's march away.
                deliberation_latency: 0.7,
                // the longest of the three: route, well and season knowledge
                // is the capital a desert people actually holds.
                time_horizon: 0.9,
            },
        ),
        (
            KindId("gully-dwarf"),
            MindVector {
                // flees: a scavenger that stands its ground against anything
                // larger stops being a scavenger.
                threat_response: 0.2,
                // the fastest of the three, and the only one below the
                // manikin's midpoint — opportunism is a decision made before
                // the opportunity leaves.
                deliberation_latency: 0.4,
                // long by family, short by dwarf: enough to keep a midden and
                // a lineage, not enough to keep a chronicle.
                time_horizon: 0.8,
            },
        ),
        (
            KindId("hill-dwarf"),
            MindVector {
                // stands, moderately: a settled farmer defends its steading
                // but is not looking for the fight.
                threat_response: 0.6,
                // slow and considered, the family's characteristic reading.
                deliberation_latency: 0.75,
                // a farmer plans in seasons and a long-lived farmer plans in
                // generations of orchard and terrace.
                time_horizon: 0.85,
            },
        ),
        // THE RADIATION (C2d): the six elves. The family's shared reading is
        // the roster's longest `time_horizon` — every one of them sits at or
        // above the dwarves' 0.80-0.90 — and it is not decoration:
        // `paced(5.0)` puts an elf past three centuries, so it genuinely plans
        // past the span a dwarf can. As with the dwarves, the differences
        // among the six are the ecology and the institution each lives in, not
        // the family they belong to.
        //
        // **Wood's and High's rows must differ, and this is where §3.6's whole
        // claim lives.** High shares Wood's body exactly; if it shared Wood's
        // mind it would be a duplicate rather than a control, which
        // `radiation_admission.rs::wood_and_high_differ_in_mind_and_not_in_body`
        // asserts in both directions.
        (
            KindId("desert-elf"),
            MindVector {
                // patient, like every arid people in the roster: a sparse
                // country cannot afford a fight it could walk away from.
                threat_response: 0.4,
                deliberation_latency: 0.7,
                // route, well and season knowledge is a desert people's real
                // capital, and a three-century memory of it is a large one.
                time_horizon: 0.9,
            },
        ),
        (
            KindId("drow"),
            MindVector {
                // the family's most forward: a confined realm has nowhere to
                // withdraw to, so a threat is met where it is found.
                threat_response: 0.75,
                // fast for an elf. Sightlines underground are short and a
                // decision deferred is a decision made for you.
                deliberation_latency: 0.55,
                time_horizon: 0.85,
            },
        ),
        (
            KindId("high-elf"),
            MindVector {
                // MIND IS HALF OF HIGH'S ENTIRE IDENTITY (the other half is
                // society and language). Every scalar here differs from
                // wood-elf's below: more willing to stand, markedly slower to
                // decide, and the longest horizon in the roster — the reading
                // of a people whose institutions outlive its members and know
                // it.
                threat_response: 0.6,
                deliberation_latency: 0.85,
                time_horizon: 0.95,
            },
        ),
        (
            KindId("sea-elf"),
            MindVector {
                // a shore people reads weather and tide and gets off the water
                // rather than arguing with it.
                threat_response: 0.35,
                deliberation_latency: 0.6,
                time_horizon: 0.85,
            },
        ),
        (
            KindId("snow-elf"),
            MindVector {
                threat_response: 0.5,
                // the family's fastest. A cold margin punishes deliberation:
                // the weather does not wait, and neither does the herd.
                deliberation_latency: 0.5,
                // long even so — a people that must plan a whole winter in
                // autumn plans in years by habit.
                time_horizon: 0.88,
            },
        ),
        (
            KindId("wood-elf"),
            MindVector {
                // the family's ancestral reading and its middle: withdraws
                // rather than meets, considers rather than reacts.
                threat_response: 0.45,
                deliberation_latency: 0.7,
                time_horizon: 0.9,
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// Per-kind dispersion. **Variability is itself a species trait** (spec §2's
/// keystone): a species is a distribution, and how wide that distribution is
/// says as much about the kind as where it is centred. Every minded kind
/// carries a row — a kind with no society (a solitary dragon) is not exempt
/// from a `society` field, it is authored near-zero because there is almost
/// no society to vary.
/// type-audit: bare-ok(identifier-text)
pub fn dispersion_registry() -> ComponentStore<KindId, Dispersion> {
    [
        // GENERALIST-LITE: the cosmopolitan weed's widest goblinoid spread,
        // still a notch below human's true psychological breadth.
        (
            KindId("goblin"),
            Dispersion {
                mind: 0.25,
                society: 0.20,
                perception: 0.15,
            },
        ),
        // a disciplined knowledge-caste narrows temperament the way training
        // narrows any specialist.
        (
            KindId("kobold"),
            Dispersion {
                mind: 0.12,
                society: 0.08,
                perception: 0.08,
            },
        ),
        // a drilled military hierarchy is precisely a machine for
        // suppressing individual variance.
        (
            KindId("hobgoblin"),
            Dispersion {
                mind: 0.10,
                society: 0.06,
                perception: 0.08,
            },
        ),
        // solitary ambush hunters folded into a loose communal band: the
        // variance a rigid hierarchy would drill out survives here.
        (
            KindId("bugbear"),
            Dispersion {
                mind: 0.20,
                society: 0.15,
                perception: 0.12,
            },
        ),
        // The three chromatic dragons, one shared reading (mirrors
        // psyche_registry's shared chromatic profile): solitary apex
        // predators carry near-uniform temperament, and almost no society
        // exists to vary — the model's narrowest kind on every axis.
        (
            KindId("white-dragon"),
            Dispersion {
                mind: 0.08,
                society: 0.02,
                perception: 0.05,
            },
        ),
        (
            KindId("red-dragon"),
            Dispersion {
                mind: 0.08,
                society: 0.02,
                perception: 0.05,
            },
        ),
        (
            KindId("black-dragon"),
            Dispersion {
                mind: 0.08,
                society: 0.02,
                perception: 0.05,
            },
        ),
        // a frenzied, opportunistic forager's temperament swings by
        // disposition, not doctrine — wider than the disciplined goblinoids.
        (
            KindId("gnoll"),
            Dispersion {
                mind: 0.22,
                society: 0.15,
                perception: 0.12,
            },
        ),
        // GENERALIST: widest on every axis — this campaign's own argument
        // that psychological breadth, not ecological breadth, is what
        // "generalist" means.
        (
            KindId("human"),
            Dispersion {
                mind: 0.35,
                society: 0.30,
                perception: 0.20,
            },
        ),
        // THE DELVERS (C2c): the three dwarves, spread across much of the
        // range the roster uses. The ordering principle is the one the
        // goblinoid rows already established — an institution that drills is
        // an institution that narrows — so the settled farmer with its halls
        // and terraces is the narrowest of the three and the institution-free
        // scavenger band the widest. Human stays the widest overall on
        // every axis, which is its own campaign's claim and is not disturbed.
        (
            KindId("desert-dwarf"),
            Dispersion {
                mind: 0.20,
                society: 0.16,
                perception: 0.14,
            },
        ),
        (
            KindId("gully-dwarf"),
            Dispersion {
                // second only to human: no caste, no guild, no hall, nothing
                // that would make two gully dwarves resemble each other.
                mind: 0.30,
                society: 0.25,
                perception: 0.18,
            },
        ),
        (
            KindId("hill-dwarf"),
            Dispersion {
                mind: 0.15,
                society: 0.12,
                perception: 0.10,
            },
        ),
        // THE RADIATION (C2d): the six elves, ordered on the principle the
        // goblinoid and dwarf rows already established — an institution that
        // drills is an institution that narrows. High, whose identity IS its
        // institutions, is the narrowest; drow, held by a caste order, is next;
        // the three affinity-carried surface kinds sit in the middle; and the
        // scattered desert people is the family's widest. **Human stays the
        // widest overall on every axis** (0.35 / 0.30 / 0.20), which is its own
        // campaign's claim and is not disturbed here.
        (
            KindId("desert-elf"),
            Dispersion {
                // the family's widest: scattered kin who meet rarely have
                // nothing holding them to a common temperament.
                mind: 0.24,
                society: 0.20,
                perception: 0.16,
            },
        ),
        (
            KindId("drow"),
            Dispersion {
                mind: 0.12,
                society: 0.10,
                perception: 0.09,
            },
        ),
        (
            KindId("high-elf"),
            Dispersion {
                // the roster's narrowest people, below hobgoblin's drilled
                // 0.10 / 0.06 / 0.08 only on mind: a people whose institutions
                // outlive its members reproduces those members' outlook.
                mind: 0.09,
                society: 0.07,
                perception: 0.07,
            },
        ),
        (
            KindId("sea-elf"),
            Dispersion {
                mind: 0.18,
                society: 0.15,
                perception: 0.13,
            },
        ),
        (
            KindId("snow-elf"),
            Dispersion {
                mind: 0.20,
                society: 0.17,
                perception: 0.14,
            },
        ),
        (
            KindId("wood-elf"),
            Dispersion {
                // the family's middle, and the value High is read against: a
                // dispersed forest people with custom but no bureaucracy.
                mind: 0.18,
                society: 0.16,
                perception: 0.13,
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// The community-mind component — authored directly, present only for the
/// fifteen settling peoples. A Solitary minded kind (a dragon) carries a
/// MindVector but no SocietyVector; a mixed consumer resolves
/// [`SocietyVector::MANIKIN`] for one. Goblin's row happens to sit at those
/// same values — again authorship, not definition.
/// type-audit: bare-ok(identifier-text)
pub fn society_registry() -> ComponentStore<KindId, SocietyVector> {
    [
        (
            KindId("goblin"),
            SocietyVector {
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
                in_group_radius: 0.5,
            },
        ),
        (
            KindId("kobold"),
            SocietyVector {
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Knowledge,
                in_group_radius: 0.2,
            },
        ),
        (
            KindId("hobgoblin"),
            SocietyVector {
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
                in_group_radius: 0.3,
            },
        ),
        (
            KindId("bugbear"),
            SocietyVector {
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Rank,
                in_group_radius: 0.3,
            },
        ),
        // The Vacancy (T9): the fifth people, and `StatusBasis::Generosity`'s
        // first witness — the campaign's headline promotion (see
        // `tests/coverage.rs`'s `status_basis_coverage_matches_the_table`).
        //
        // Justified from the ECOLOGY, not from lore (decision 0021: no 5E
        // moral canon rides along — 5E supplies mass and CR only, nothing
        // else). `gnoll_condition_niche` stakes the desert climate tile:
        // hot, deep in the `< 0.20` moisture band. A forager there faces
        // resource windfalls that are both SCARCE and HIGH-VARIANCE — a kill
        // or a find feeds many mouths at once, then nothing for a stretch.
        // Human forager ethnography shows the standard adaptive response to
        // exactly this variance profile is a WIDER reciprocal food-sharing
        // network, not a narrower one: pooling risk across more partners
        // smooths the individual variance each forager alone cannot smooth
        // (the same risk-pooling logic behind !Kung/Ache-style meat-sharing
        // norms). A pack that shares a windfall widely, rather than hoarding
        // it, is the one whose members survive the droughts between finds —
        // so what earns standing is provisioning the group, not winning it
        // by force or hoarding lore. `in_group_radius` is authored wide
        // (0.7, above the manikin's midpoint) for the same reason: an
        // expansive "us" is the risk-pooling network's natural shape.
        (
            KindId("gnoll"),
            SocietyVector {
                // packs follow a leader (authority shape); Generosity below
                // is what a leader must DO to hold that standing, not how
                // the pack is organized.
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Generosity,
                // wide: windfall-sharing risk-pooling networks extend "us"
                // broadly, the adaptive response to a scarce, high-variance
                // forage base.
                in_group_radius: 0.7,
            },
        ),
        // The Generalist (C2-0). `in_group_radius` 0.8 is the widest in the
        // roster, above gnoll's 0.7: an expansive "us" is the social twin of
        // a broad niche, and is what a no-refuge generalist looks like from
        // the inside.
        (
            KindId("human"),
            SocietyVector {
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Knowledge,
                in_group_radius: 0.8,
            },
        ),
        // THE DELVERS (C2c): the three dwarves. Each row is argued from the
        // kind's ECOLOGY, per decision 0021 — 5E supplies mass and CR and
        // nothing else, so no moral canon rides along with the names. The
        // three deliberately do not share a social reading: a family is a
        // shared descent and a shared tongue, not a shared constitution.
        (
            KindId("desert-dwarf"),
            SocietyVector {
                // consensus, not command. A dispersed people whose members
                // are separated for long stretches by the distances between
                // water cannot enforce a standing authority, and does not
                // need one.
                sociality: Sociality::Communal,
                // what earns standing is knowing where the water, the route
                // and the season are — the same reasoning that puts
                // `starreader` at the top of its lexicon.
                status_basis: StatusBasis::Knowledge,
                // wide: scattered kin who meet rarely must count distant
                // relations as "us" or lose them entirely.
                in_group_radius: 0.65,
            },
        ),
        (
            KindId("gully-dwarf"),
            SocietyVector {
                // no authority worth the name: a scavenger band has nothing
                // to command and nothing to command it with.
                sociality: Sociality::Communal,
                // what is found is shared — the same risk-pooling logic that
                // earned gnoll `Generosity`, arrived at from the opposite
                // direction: not a windfall too large to keep, but a find too
                // small to be worth fighting over.
                status_basis: StatusBasis::Generosity,
                in_group_radius: 0.5,
            },
        ),
        (
            KindId("hill-dwarf"),
            SocietyVector {
                sociality: Sociality::Hierarchic,
                // a settled surplus people: standing comes from what the hall
                // sets out, which is what a good harvest is FOR.
                status_basis: StatusBasis::Generosity,
                in_group_radius: 0.6,
            },
        ),
        // THE RADIATION (C2d): the six elves. Each row is argued from the
        // kind's ECOLOGY, per decision 0021 — 5E supplies mass and CR and
        // nothing else, so no moral canon rides along with the names, and in
        // particular nothing here reads "drow" as wicked. The six deliberately
        // do not share a social reading: a family is a shared descent and a
        // shared tongue, not a shared constitution.
        //
        // **Wood's and High's rows differ, and must.** Society is the second
        // of High's three identity channels (psyche, society, language); if
        // this row matched Wood's, High would be a duplicate rather than the
        // family's null control.
        (
            KindId("desert-elf"),
            SocietyVector {
                // consensus: a people separated for long stretches by the
                // distance between water cannot enforce a standing authority
                // and does not need one.
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Knowledge,
                // wide — scattered kin who meet rarely must count distant
                // relations as "us" or lose them entirely.
                in_group_radius: 0.7,
            },
        ),
        (
            KindId("drow"),
            SocietyVector {
                // a confined realm rations space, water and light, and a
                // society that must ration runs on ranked authority.
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
                // the family's narrowest: the "us" of a single hold, because
                // the next hold is a different country however near it is in
                // metres.
                in_group_radius: 0.35,
            },
        ),
        (
            KindId("high-elf"),
            SocietyVector {
                // HIGH'S SECOND IDENTITY CHANNEL. Every field here differs
                // from wood-elf's below — ranked where Wood is consensual, and
                // standing earned by lore rather than by what is given.
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Knowledge,
                in_group_radius: 0.5,
            },
        ),
        (
            KindId("sea-elf"),
            SocietyVector {
                // a boat crew is not a court: consensus, and standing to
                // whoever fed the others through the lean season.
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Generosity,
                in_group_radius: 0.6,
            },
        ),
        (
            KindId("snow-elf"),
            SocietyVector {
                sociality: Sociality::Communal,
                // the risk-pooling reading gnoll and gully-dwarf already
                // carry, arrived at from the cold: a winter is survived by
                // what the band shares, not by what one member holds.
                status_basis: StatusBasis::Generosity,
                in_group_radius: 0.55,
            },
        ),
        (
            KindId("wood-elf"),
            SocietyVector {
                // custom without bureaucracy: a dispersed forest people that
                // decides in common and has no hall to decide in.
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Generosity,
                in_group_radius: 0.65,
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// The perception component — authored directly, present for every minded
/// SPEAKING kind: the fifteen peoples and the three chromatic dragons (The
/// Vigil). Goblin's row happens to sit at [`PerceptionVector::MANIKIN`]
/// (`Diurnal`, 0.5/0.5) — authorship, not definition. Since The Vigil the
/// enforced lattice is `speech ⊆ perception ⊆ mind`, so a speaking kind added
/// without a row here fails `check_integrity` at load rather than silently
/// falling back on goblin's row, as the pre-Vigil stopgap did.
/// type-audit: bare-ok(identifier-text)
pub fn perception_registry() -> ComponentStore<KindId, PerceptionVector> {
    [
        (
            KindId("goblin"),
            PerceptionVector {
                activity: ActivityCycle::Diurnal,
                night_vision: 0.5,
                sky_attention: 0.5,
            },
        ),
        (
            KindId("kobold"),
            PerceptionVector {
                activity: ActivityCycle::Nocturnal,
                night_vision: 0.9,
                sky_attention: 0.8,
            },
        ),
        (
            KindId("hobgoblin"),
            PerceptionVector {
                activity: ActivityCycle::Diurnal,
                night_vision: 0.6,
                sky_attention: 0.5,
            },
        ),
        (
            KindId("bugbear"),
            PerceptionVector {
                activity: ActivityCycle::Nocturnal,
                night_vision: 0.7,
                sky_attention: 0.3,
            },
        ),
        // The Vigil: the three chromatic dragons perceive. One clade eye
        // (`DRACONIC_NIGHT_VISION`), three ecological schedules — `activity`
        // read off each kind's already-authored `ConditionNiche.insolation`
        // optimum, and `sky_attention` low across the clade because the
        // dimension means CELESTIAL vs terrestrial attention, not aerialness:
        // `perception_lens.ambient = 1.5 - sky_attention`, and a hunting
        // dragon on the wing looks DOWN.
        (
            KindId("white-dragon"),
            PerceptionVector {
                // polar, insolation optimum 0.05 — twilight-dominated light
                activity: ActivityCycle::Crepuscular,
                night_vision: DRACONIC_NIGHT_VISION,
                // the open polar sky, the most of the three
                sky_attention: 0.3,
            },
        ),
        (
            KindId("red-dragon"),
            PerceptionVector {
                // open volcanic terrain, insolation optimum 0.20 — high sun
                activity: ActivityCycle::Diurnal,
                night_vision: DRACONIC_NIGHT_VISION,
                sky_attention: 0.25,
            },
        ),
        (
            KindId("black-dragon"),
            PerceptionVector {
                // shaded lowland swamp, insolation optimum 0.10 — ambush
                activity: ActivityCycle::Nocturnal,
                night_vision: DRACONIC_NIGHT_VISION,
                // canopy, no sky: the most ground-attentive kind in the roster
                sky_attention: 0.15,
            },
        ),
        // The Vacancy (T9): the fifth people. `activity` is read off the
        // gnoll's own authored `gnoll_condition_niche().insolation` optimum
        // (0.08, LOW), the way The Vigil derived the dragons' schedules: a
        // desert forager that shelters through the day's peak heat and
        // hunts at the cooler margins is `Crepuscular`, not `Diurnal` — the
        // real strategy spotted hyenas use, and the ecological reason
        // behind the low insolation optimum in the first place (see that
        // niche's doc comment). This gives `ActivityCycle::Crepuscular` its
        // second witness, alongside white-dragon
        // (`tests/coverage.rs`'s `activity_cycle_coverage_matches_the_table`).
        (
            KindId("gnoll"),
            PerceptionVector {
                activity: ActivityCycle::Crepuscular,
                // hunts at dusk/dawn/night: above the manikin's midpoint.
                night_vision: 0.75,
                // ground-focused pack predator tracking prey and scent, not
                // sky-rapt.
                sky_attention: 0.3,
            },
        ),
        // The Generalist (C2-0). Night vision sits BELOW the manikin, and
        // below every other people (goblin 0.5 .. kobold 0.9) — the call The
        // Manikin identified and deferred to this campaign: human scotopic
        // vision is genuinely poor, so authoring it at 0.5 would have made
        // "typical" mean "weak" and silently rescaled kobold's 0.9.
        //
        // 0.15 rather than 0.25 is deliberate and visible. `pack_depths` is a
        // step function, `hue = 2 + ((1 - night_vision) * 3).round()`: 0.25
        // yields depth 4, TIED with goblin, while <= 0.166 yields depth 5 and
        // makes human the only kind at the ladder's deepest rung. The hue
        // ladder is Berlin & Kay's, derived from human languages; a model
        // whose colour hierarchy is human-derived and then denies humans its
        // deepest rung is incoherent. Luminance is 1 either way — the shallow
        // dark-vocabulary is the cost side of the same trade.
        (
            KindId("human"),
            PerceptionVector {
                activity: ActivityCycle::Diurnal,
                night_vision: 0.15,
                sky_attention: 0.65,
            },
        ),
        // THE DELVERS (C2c): the three dwarves. `sky_attention` is CELESTIAL
        // vs terrestrial attention, not aerialness (`perception_lens.ambient
        // = 1.5 - sky_attention`), so the scavenger who reads the ground is
        // authored low on it (0.2) and the desert navigator who reads the
        // stars high (0.75), while all three carry a raised `night_vision`.
        // That authoring is
        // HONEST rather than mechanical: it reaches the perception consumers —
        // the hue ladder in `pack_depths`, the exposure lens — even though it
        // reaches nothing in the capacity model, which reads only mass,
        // potency, the resource vector and the condition niche.
        (
            KindId("desert-dwarf"),
            PerceptionVector {
                // the same strategy gnoll's row argues for on the same
                // climate tile: shelter through the peak heat, move at the
                // cooler margins.
                activity: ActivityCycle::Crepuscular,
                night_vision: 0.65,
                // THE HIGHEST IN THE ROSTER (above human's 0.65). An open-
                // country people that crosses trackless ground at night
                // navigates by the sky, and that is a claim about where its
                // attention goes, not about where its body is.
                sky_attention: 0.75,
            },
        ),
        (
            KindId("gully-dwarf"),
            PerceptionVector {
                // works the margins of the day, where what it scavenges is
                // least contested.
                activity: ActivityCycle::Crepuscular,
                night_vision: 0.7,
                // eyes on the ground — that is where the food is.
                sky_attention: 0.2,
            },
        ),
        (
            KindId("hill-dwarf"),
            PerceptionVector {
                // a surface farmer keeps the sun's hours.
                activity: ActivityCycle::Diurnal,
                // better than a human's by a wide margin, worse than any
                // dwarf that lives in the dark.
                night_vision: 0.6,
                // reads the sky for weather and season, which is a farmer's
                // reason to look up and a moderate one.
                sky_attention: 0.5,
            },
        ),
        // THE RADIATION (C2d): the six elves. `sky_attention` is CELESTIAL vs
        // terrestrial attention, not aerialness (`perception_lens.ambient =
        // 1.5 - sky_attention`), so a canopy people that never sees the sky is
        // authored LOW on it and an open-country people high. Every elf
        // carries a raised `night_vision`; the family reading is a long
        // twilight eye.
        //
        // **Drow's cave adaptation is legible HERE and only here today.** Its
        // `activity` and `night_vision` reach the perception consumers — the
        // hue ladder in `pack_depths`, the exposure lens — even though the
        // capacity model reads only mass, potency, the resource vector and the
        // condition niche, and even though its authored cave-dark insolation
        // curve is dormant (see `drow_condition_niche`'s doc comment).
        (
            KindId("desert-elf"),
            PerceptionVector {
                // the strategy gnoll and desert-dwarf already argue for on the
                // same climate tile: shelter through the peak heat, move at
                // the cooler margins.
                activity: ActivityCycle::Crepuscular,
                night_vision: 0.7,
                // high, for the reason desert-dwarf's 0.75 is high: an open-
                // country people crossing trackless ground at night navigates
                // by the sky. Held just below desert-dwarf's so that the
                // roster's maximum stays where its own campaign put it.
                sky_attention: 0.7,
            },
        ),
        (
            KindId("drow"),
            PerceptionVector {
                // no sun to keep hours by. The roster's least ambiguous
                // `Nocturnal`.
                activity: ActivityCycle::Nocturnal,
                // THE HIGHEST IN THE ROSTER, above kobold's 0.9: this people
                // sees where there is nothing to see by.
                night_vision: 0.95,
                // THE LOWEST IN THE ROSTER, below black-dragon's 0.15: a kind
                // that has never seen a sky does not attend to one. The
                // consequence is a raised `perception_lens.ambient`, which is
                // the correct reading for an eye adapted to the dark.
                sky_attention: 0.05,
            },
        ),
        (
            KindId("high-elf"),
            PerceptionVector {
                // High's identity is mind, society and language — NOT
                // perception. This row differs from wood-elf's only in
                // `sky_attention`, and that one difference is a claim about
                // where an institution points its attention (record, season,
                // reckoning), not about the eye it points.
                activity: ActivityCycle::Diurnal,
                night_vision: 0.75,
                sky_attention: 0.6,
            },
        ),
        (
            KindId("sea-elf"),
            PerceptionVector {
                activity: ActivityCycle::Diurnal,
                night_vision: 0.7,
                // a coastal people navigates and keeps a tide-reckoning: the
                // family's second-highest.
                sky_attention: 0.65,
            },
        ),
        (
            KindId("snow-elf"),
            PerceptionVector {
                // a high-latitude people's day is not the sun's: it works the
                // long margins, which at that latitude are most of the year.
                activity: ActivityCycle::Crepuscular,
                // the highest of the five surface elves — a polar winter is a
                // months-long twilight.
                night_vision: 0.8,
                sky_attention: 0.55,
            },
        ),
        (
            KindId("wood-elf"),
            PerceptionVector {
                activity: ActivityCycle::Crepuscular,
                night_vision: 0.75,
                // low: under a closed canopy there is no sky to read, and this
                // people reads the ground and the trunks instead.
                sky_attention: 0.25,
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// The universal taxonomy lookup: a kind's family label, authored directly
/// (one row per kind). Read by worldgen to resolve a kind's proto vector
/// against language's `family_proto`. A singleton family's name equals its
/// lone member's name.
/// type-audit: bare-ok(identifier-text)
pub fn family_of() -> ComponentStore<KindId, &'static str> {
    [
        (KindId("goblin"), "goblinoid"),
        (KindId("kobold"), "kobold"),
        (KindId("hobgoblin"), "goblinoid"),
        (KindId("bugbear"), "goblinoid"),
        (KindId("treant"), "plant"),
        (KindId("twig-blight"), "plant"),
        (KindId("giant-elk"), "giant-elk"),
        (KindId("woolly-mammoth"), "woolly-mammoth"),
        (KindId("giant-goat"), "giant-goat"),
        (KindId("otyugh"), "otyugh"),
        (KindId("xorn"), "xorn"),
        (KindId("rust-monster"), "rust-monster"),
        (KindId("white-dragon"), "draconic"),
        (KindId("red-dragon"), "draconic"),
        (KindId("black-dragon"), "draconic"),
        (KindId("owlbear"), "owlbear"),
        // The Vacancy (T7): seven singleton families — none shares a family
        // label with another kind, so none needs a `family_proto` entry in
        // `hornvale_language` (a proto is only required once a label is
        // carried by >= 2 kinds).
        (KindId("giant-scorpion"), "giant-scorpion"),
        (KindId("giant-hyena"), "giant-hyena"),
        (KindId("dire-wolf"), "dire-wolf"),
        (KindId("rhinoceros"), "rhinoceros"),
        (KindId("giant-constrictor-snake"), "giant-constrictor-snake"),
        (KindId("carrion-crawler"), "carrion-crawler"),
        (KindId("shrieker"), "shrieker"),
        // The Vacancy (T8): five more singleton families — same rule as T7's
        // (no label shared by >= 2 kinds, so no `family_proto` entry needed).
        (KindId("reef-shark"), "reef-shark"),
        (KindId("giant-octopus"), "giant-octopus"),
        (KindId("killer-whale"), "killer-whale"),
        (KindId("giant-squid"), "giant-squid"),
        (KindId("giant-crocodile"), "giant-crocodile"),
        // The Vacancy (T9): the fifth people. Follows kobold's shape, not
        // the goblinoids' — `family_of` maps a singleton-family people to
        // its own name, and `hornvale_language`'s `family_proto` carries no
        // "gnoll" entry, because `check_integrity` requires a proto only for
        // a label held by >= 2 kinds (goblinoid/draconic/plant, the roster's
        // only multi-member families).
        (KindId("gnoll"), "gnoll"),
        // The Generalist (C2-0): a singleton family, following kobold's and
        // gnoll's shape — `family_proto` in `hornvale_language` carries no
        // "human" entry, because `check_integrity` requires a proto only for
        // a label held by >= 2 kinds. The dwarf and elf families of C2c/C2d
        // will be the roster's first new multi-member families.
        (KindId("human"), "human"),
        // THE DELVERS (C2c): three kinds, ONE label — the roster's first new
        // multi-member family since goblinoid, and the first ever added as a
        // family rather than grown into one. The moment the second of these
        // rows exists, `check_integrity`
        // (`windows/worldgen/src/components.rs`) requires a matching
        // `family_proto` entry keyed `KindId("dwarf")` in `hornvale_language`;
        // that row lands in the same commit, because it must.
        (KindId("desert-dwarf"), "dwarf"),
        (KindId("gully-dwarf"), "dwarf"),
        (KindId("hill-dwarf"), "dwarf"),
        // THE RADIATION (C2d): six kinds, ONE label — the roster's largest
        // family, and the programme's last. `family_proto` in
        // `hornvale_language` carries the matching `KindId("elf")` row in this
        // same commit, because `check_integrity` requires one the moment a
        // label is carried by >= 2 kinds.
        (KindId("desert-elf"), "elf"),
        (KindId("drow"), "elf"),
        (KindId("high-elf"), "elf"),
        (KindId("sea-elf"), "elf"),
        (KindId("snow-elf"), "elf"),
        (KindId("wood-elf"), "elf"),
    ]
    .into_iter()
    .collect()
}

/// Every seed-derivation label this crate uses (none — species are authored).
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register species' contribution to the concept registry.
///
/// The `*-kind` concepts register through their correspondence [`Manifest`].
/// Like climate's biome classes, these are taxonomic class labels no language
/// pack names yet, so each lexeme edge is a `Gap`; species emits no phenomenon
/// kind for them, so the percept edge is a `Gap`; and cognition voids to the
/// Every `*-kind` concept the registry holds, as `(concept id, gloss)`.
///
/// A `const` rather than literals inside the registration loop so that
/// [`kind_concept`] and [`register_concepts`] read the SAME roster — a lookup
/// built from its own copy of this list would silently answer for a kind the
/// registry never registered, which is the one failure an authored table has
/// (`cli/tests/accession.rs` makes the same argument for `EPOCH_COHORTS`).
///
/// Glosses are authored, not derived from the id, so `giant-elk` reads as
/// "a giant elk" and not as its own key.
/// type-audit: bare-ok(identifier-text)
pub const KIND_CONCEPTS: &[(&str, &str)] = &[
    ("goblin-kind", "a goblin"),
    ("kobold-kind", "a kobold"),
    ("hobgoblin-kind", "a hobgoblin"),
    ("bugbear-kind", "a bugbear"),
    ("treant-kind", "a treant"),
    ("twig-blight-kind", "a twig blight"),
    ("giant-elk-kind", "a giant elk"),
    ("woolly-mammoth-kind", "a woolly mammoth"),
    ("giant-goat-kind", "a giant goat"),
    ("otyugh-kind", "an otyugh"),
    ("xorn-kind", "a xorn"),
    ("rust-monster-kind", "a rust monster"),
    ("white-dragon-kind", "a white dragon"),
    ("red-dragon-kind", "a red dragon"),
    ("black-dragon-kind", "a black dragon"),
    ("owlbear-kind", "an owlbear"),
    // The Vacancy's thirteen. The Actants' rule — every kind the biosphere
    // registry holds owes a name, not only the speaking peoples — is what
    // makes these mandatory rather than optional, and the two campaigns
    // arrived at the same seam from opposite directions within a week.
    // Glosses are authored, not derived from the id, so `giant-scorpion`
    // reads as "a giant scorpion" and not as its own key.
    ("gnoll-kind", "a gnoll"),
    ("giant-scorpion-kind", "a giant scorpion"),
    ("giant-hyena-kind", "a giant hyena"),
    ("dire-wolf-kind", "a dire wolf"),
    ("rhinoceros-kind", "a rhinoceros"),
    ("giant-constrictor-snake-kind", "a giant constrictor snake"),
    ("carrion-crawler-kind", "a carrion crawler"),
    ("shrieker-kind", "a shrieker"),
    ("reef-shark-kind", "a reef shark"),
    ("giant-octopus-kind", "a giant octopus"),
    ("killer-whale-kind", "a killer whale"),
    ("giant-squid-kind", "a giant squid"),
    ("giant-crocodile-kind", "a giant crocodile"),
    // The Generalist (C2-0): the sixth people.
    ("human-kind", "a human"),
    // The Delvers (C2c): the dwarf family's three. These three ids are what
    // `domains/language/src/accession.rs`'s epoch-9 cohort lists;
    // `cli/tests/accession.rs` checks the two agree in BOTH directions, and
    // commit `ee4e6a00` records that omitting the cohort also changes which
    // proto-root each concept draws.
    ("desert-dwarf-kind", "a desert dwarf"),
    ("gully-dwarf-kind", "a gully dwarf"),
    ("hill-dwarf-kind", "a hill dwarf"),
    // THE RADIATION (C2d): the elf family's six — the roster's largest family,
    // and the programme's last. These six ids are what
    // `domains/language/src/accession.rs`'s epoch-10 cohort lists;
    // `cli/tests/accession.rs` checks the two agree in BOTH directions, and
    // commit `ee4e6a00` records that omitting the cohort also changes which
    // proto-root each concept draws. Glosses are authored, not derived from
    // the id, so `drow` reads as "a drow" and not as its own key.
    ("desert-elf-kind", "a desert elf"),
    ("drow-kind", "a drow"),
    ("high-elf-kind", "a high elf"),
    ("sea-elf-kind", "a sea elf"),
    ("snow-elf-kind", "a snow elf"),
    ("wood-elf-kind", "a wood elf"),
];

/// The `*-kind` concept naming `species`, or `None` when the species has no
/// registered kind concept.
///
/// The Watershed, Item 5: a settlement raised on another people's ruin is
/// named for THEM, so the namer needs a people's concept as a
/// `&'static str` — `settlement_site_concepts` returns `Vec<&'static str>`
/// and cannot mint one. Reads [`KIND_CONCEPTS`], so it can only ever return a
/// concept the registry actually registered.
/// type-audit: bare-ok(identifier-text: species), bare-ok(identifier-text: return)
pub fn kind_concept(species: &str) -> Option<&'static str> {
    KIND_CONCEPTS
        .iter()
        .find(|(id, _)| id.strip_suffix("-kind") == Some(species))
        .map(|(id, _)| *id)
}

/// future cognition wave.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(SPECIES_NAME, true, "a species entity's name")?;
    registry.register_predicate(THREAT_RESPONSE, true, "flee 0 ↔ stand 1")?;
    registry.register_predicate(DELIBERATION_LATENCY, true, "decision slowness, 0-1")?;
    registry.register_predicate(IN_GROUP_RADIUS, true, "how wide 'us' is drawn, 0-1")?;
    registry.register_predicate(TIME_HORIZON, true, "planning depth, 0-1")?;
    registry.register_predicate(SOCIALITY_MODE, true, "hierarchic or communal")?;
    registry.register_predicate(STATUS_BASIS, true, "rank, knowledge, or generosity")?;
    registry.register_predicate(PEOPLED_BY, true, "the species that peoples a settlement")?;
    registry.register_predicate(
        SPECIES_MASS_KG,
        false,
        "body mass in kilograms (latest wins)",
    )?;
    registry.register_predicate(SPECIES_POTENCY, false, "magical potency (latest wins)")?;
    registry.register_predicate(
        SPECIES_ACTIVITY_CYCLE,
        true,
        "when a species is awake: diurnal, nocturnal, crepuscular",
    )?;
    registry.register_predicate(SPECIES_NIGHT_VISION, true, "night-sky acuity, 0-1")?;
    registry.register_predicate(SPECIES_SKY_ATTENTION, true, "sky vs. ground attention, 0-1")?;
    registry.register_predicate(SPECIES_LABIALITY, true, "lip-rounding and jaw-closure, 0-1")?;
    registry.register_predicate(SPECIES_VOWEL_SPACE, true, "vowel-space size, 0-1")?;
    registry.register_predicate(SPECIES_VOICING, true, "voicing emphasis, 0-1")?;
    registry.register_predicate(SPECIES_SIBILANCE, true, "sibilance emphasis, 0-1")?;
    registry.register_predicate(SPECIES_VOICE_LOUDNESS, true, "voice-loudness range, 0-1")?;
    registry.register_predicate(
        SPECIES_EXOTIC_MANNER,
        true,
        "exotic manner: none, trill, click, ejective",
    )?;
    registry.register_predicate(
        SPECIES_TONALITY,
        true,
        "tonal propensity, 0 atonal ↔ 1 tonal",
    )?;

    // Every kind the biosphere registry holds, not only the speaking peoples:
    // a creature the world simulates, places, and narrates is a thing the
    // vocabulary owes a name, whether or not anyone has a word for it yet. The
    // roster was peoples-only from The Words until The Actants, which is how
    // The Menagerie's twelve fauna went four campaigns unnamed. Glosses are
    // authored rather than derived from the id, so a `giant-elk` reads as "a
    // giant elk" and not as its own key.
    for (name, doc) in KIND_CONCEPTS {
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: name.to_string(),
                domain: "species".to_string(),
                kind: ConceptKind::Living,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap("no language pack names species kinds yet")),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }
    Ok(())
}

/// Species as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Species;

impl hornvale_kernel::Domain for Species {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        crate::stream_labels()
    }
}

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(WorldTime::GENESIS),
        provenance: "species".to_string(),
    }
}

/// Commit the `peopled-by` fact linking a settlement to its species.
/// type-audit: bare-ok(identifier-text)
pub fn people(world: &mut World, settlement: EntityId, species: &str) -> Result<(), LedgerError> {
    world.ledger.commit(
        fact(settlement, PEOPLED_BY, Value::Text(species.to_string())),
        &world.registry,
    )?;
    Ok(())
}

/// The species a settlement is peopled by, if committed.
/// type-audit: bare-ok(identifier-text)
pub fn species_of(world: &World, settlement: EntityId) -> Option<String> {
    match world.ledger.value_of(settlement, PEOPLED_BY) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

/// The species entity carrying `name`'s authored vector, if genesis ran.
/// type-audit: bare-ok(identifier-text)
pub fn species_entity(world: &World, name: &str) -> Option<EntityId> {
    world
        .ledger
        .find(SPECIES_NAME)
        .find(|f| matches!(&f.object, Value::Text(t) if t == name))
        .map(|f| f.subject)
}

/// The instance-component lens (spec §4.3): the effective `BiosphereTraits`
/// of an instance — its (latest) numeric override facts applied over its
/// current kind's authored registry default. Materialized per call; derived,
/// never serialized, never cached (the tick cache is c6). Total: `None` for
/// a kindless entity, a dangling label, or a physically invalid override.
pub fn instance_biosphere(
    ledger: &Ledger,
    e: EntityId,
    biosphere: &ComponentStore<KindId, BiosphereTraits>,
) -> Option<BiosphereTraits> {
    let label = ledger.kind_of(e)?;
    let mut traits = biosphere.get_by_label(label)?.clone();
    if let Some(Value::Number(m)) = ledger.latest_value_of(e, SPECIES_MASS_KG) {
        traits.mass = Mass::new(*m).ok()?;
    }
    if let Some(Value::Number(p)) = ledger.latest_value_of(e, SPECIES_POTENCY) {
        if !p.is_finite() || *p < 0.0 {
            return None;
        }
        traits.potency = *p;
    }
    Some(traits)
}

/// A kind's preference on **one** environment axis: the species-side atom of
/// an [`EnvironmentNiche`].
///
/// Two variants rather than one struct, and the reason is constitutional to
/// the basis rather than stylistic. `hornvale_kernel::AxisValence` declares
/// the axes "deliberately **not** homogeneous, and a consumer must not assume
/// otherwise": `PHYSIOGNOMY` is `Ordinal`, `ENERGY`/`WATER`/`LIGHT` are
/// `Scalar`, `SUBSTRATE` is `Nominal`, `DISTURBANCE` is a `Rate`. A magnitude
/// reading of a nominal axis is meaningless — soil at `0.0` is not "closer to"
/// sand at `0.2` than to organic at `1.0`, they are simply different classes —
/// and [`EnvironmentNiche::new`] refuses the combinations that would assume
/// otherwise, so the variant a preference carries always matches its axis's
/// valence. That is why [`environment_fit`] can dispatch on the variant alone
/// and never needs to look an axis's valence up again.
///
/// Deliberately **not** `hornvale_kernel::ConditionResponse`, which
/// [`ConditionNiche`] uses for the four climate axes. That type is a Gaussian
/// over an unbounded field with a sovereignty floor — the right shape for
/// °C and metres, and the wrong shape here twice over: it would read
/// `SUBSTRATE`'s class index as a magnitude, and its floor belongs to the
/// Liebig fast path (`tolerance_liebig`'s single unfloored undercutter) whose
/// invariant The Axes pinned and this campaign must not disturb.
/// type-audit: bare-ok(ratio: Graded.preferred), bare-ok(ratio: Graded.tolerance), bare-ok(index: Class.accepted)
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AxisPreference {
    /// A preferred magnitude on an ordered axis (`Ordinal`, `Scalar`, `Rate`),
    /// with the breadth of tolerance around it. Fit falls linearly from `1.0`
    /// at `preferred` to `0.0` a full `tolerance` away, and stays at `0.0`
    /// beyond — a tent, not a Gaussian, because the axis is bounded and a
    /// bounded axis does not need an asymptote.
    Graded {
        /// The axis value the kind prefers, in the axis's own `[0, 1]` range.
        preferred: f64,
        /// How far from `preferred` the kind tolerates before this axis scores
        /// zero, as a fraction of the axis's `[0, 1]` range. Must lie in
        /// `(0, 1]`. A kind wanting more breadth than the whole range should
        /// **decline the axis** instead (see [`EnvironmentNiche`]'s genus
        /// rule) — that, not a wider number, is how indifference is said.
        tolerance: f64,
    },
    /// The one class a kind accepts on a `Nominal` axis. The value indexes an
    /// unordered set and is never a magnitude, so the fit is `1.0` on the
    /// class and `0.0` off it, with no gradient in between.
    Class {
        /// The accepted class index, in the axis's own `[0, 1]` range.
        accepted: f64,
    },
}

impl AxisPreference {
    /// A graded preference tolerant across the **whole** axis range.
    ///
    /// The `1.0` is AUTHORED as the neutral default, and it is neutral in an
    /// exact sense rather than a vague one: at this tolerance the per-axis
    /// dissimilarity is exactly `|value − preferred|`, so [`environment_fit`]
    /// reduces bit-for-bit to `1 − d` for Gower's `d` — the instrument Task 6
    /// measured the underworld corpus with. A narrower tolerance makes a kind
    /// fussier than that reference; there is no wider one.
    /// type-audit: bare-ok(ratio: preferred)
    pub fn graded(preferred: f64) -> AxisPreference {
        AxisPreference::Graded {
            preferred,
            tolerance: 1.0,
        }
    }

    /// A nominal-class preference. See [`AxisPreference::Class`].
    /// type-audit: bare-ok(index: accepted)
    pub fn class(accepted: f64) -> AxisPreference {
        AxisPreference::Class { accepted }
    }

    /// This preference's **dissimilarity** against one place reading, in
    /// `[0, 1]`: `0.0` is a perfect match. Dissimilarity rather than fit
    /// because that is what makes [`environment_fit`] the exact complement of
    /// Gower's distance rather than an approximation of it — the mean is taken
    /// over dissimilarities and subtracted from one **once**, which is the same
    /// expression Gower's is, not merely the same value to within rounding.
    fn dissimilarity(&self, value: f64) -> f64 {
        match self {
            AxisPreference::Graded {
                preferred,
                tolerance,
            } => ((value - preferred).abs() / tolerance).min(1.0),
            // `==` rather than `to_bits()`: two class indices are the same
            // class or they are not, and `-0.0` is the same class as `0.0`
            // while its bits are not. Both operands are validated finite, so
            // there is no NaN case for the comparison to mishandle. This is a
            // stated, deliberate difference from `distance` in
            // `domains/climate/tests/underworld.rs`, which compares bits; on
            // the shipped corpora the two agree, because no authored class
            // value is a negative zero.
            AxisPreference::Class { accepted } => {
                if *accepted == value {
                    0.0
                } else {
                    1.0
                }
            }
        }
    }
}

/// A kind's preference stated in the **same basis a place's character is
/// stated in** — the species-side counterpart to
/// `hornvale_kernel::EnvironmentVector`, which The Axes specified and deferred
/// for want of a consumer (its retrospective A-1; this campaign's spec §4.5).
///
/// Six axes are declared and five are occupied: `DISTURBANCE` is a `Rate` no
/// whole-community name can carry a value on (The Axes' A-4), so a niche in
/// practice speaks to at most the other five. Nothing here special-cases it —
/// it is simply an axis every corpus declines today.
///
/// Sparse, keyed by `EnvironmentAxis::id`, and **the silence is meaningful**:
///
/// > **The genus rule.** A niche silent on an axis is INDIFFERENT to it, never
/// > zero on it.
///
/// That is The Axes' own rule, carried across from the place side, where a
/// formation is a genus and declines any axis its variants disagree on. It is
/// not a nicety: score a declined axis as a zero and every partially-specified
/// niche reads as a poor fit everywhere, which fails quietly rather than
/// loudly. [`environment_fit`] enforces it by construction — it iterates the
/// niche's own recorded axes and drops any the place does not carry, so a
/// declined axis is never a term in the mean.
///
/// **What this type deliberately does not touch.** [`ConditionNiche`] stays
/// exactly as it is. This is an additive second vocabulary alongside it, not a
/// replacement: the Liebig fast path's single unfloored undercutter is
/// elevation and remains so, which is the invariant The Axes pinned and proved
/// before deferring this type.
///
/// **Why the axis is stored, not just its id.** The axis is kept alongside its
/// preference rather than reconstructed from the key, so that [`environment_fit`] can look a place's reading up without
/// consulting `environment_v1_basis()` — the property that makes the basis's
/// append-only rule sufficient here. The resulting duplication of
/// `EnvironmentAxis::id` in the key is the same shape decision 0015 ratified
/// for `PredicateDef.name`, and is kept honest the same way: only
/// [`EnvironmentNiche::new`] ever inserts, and it always keys by the axis it
/// stores.
#[derive(Clone, Debug, PartialEq)]
pub struct EnvironmentNiche(BTreeMap<u16, (EnvironmentAxis, AxisPreference)>);

impl EnvironmentNiche {
    /// Validating constructor. Rejects, with the physical reason:
    ///
    /// - a `Graded` preference on a `Nominal` axis, or a `Class` preference on
    ///   any other — the valence mismatch that would read a class index as a
    ///   magnitude, or throw a magnitude away;
    /// - a `preferred` or `accepted` outside the axis's `[0, 1]` range, or
    ///   non-finite;
    /// - a `tolerance` outside `(0, 1]`, or non-finite — zero would divide by
    ///   zero and a wider-than-the-range value is spelled by declining the
    ///   axis instead.
    ///
    /// An empty slice is legal and produces the wholly indifferent niche, the
    /// mirror of `EnvironmentVector`'s legal zero vector. Repeated axis ids
    /// overwrite rather than combine (last write wins), matching the map-like
    /// semantics of the sparse representation and `EnvironmentVector::new`.
    ///
    /// No `type-audit:` tag: `preferences` carries no bare primitive (it is a
    /// slice of `(EnvironmentAxis, AxisPreference)`) and neither does the
    /// return. The kernel's `EnvironmentVector::new` is tagged
    /// `constructor-edge` because its slice's second element **is** an `f64`;
    /// here the primitives are tagged one level down, on
    /// [`AxisPreference`]'s own variants.
    pub fn new(preferences: &[(EnvironmentAxis, AxisPreference)]) -> Result<Self, UnitError> {
        let mut map = BTreeMap::new();
        for (axis, preference) in preferences {
            let nominal = axis.valence == AxisValence::Nominal;
            match preference {
                AxisPreference::Graded {
                    preferred,
                    tolerance,
                } => {
                    if nominal {
                        return Err(UnitError {
                            unit: "environment axis preference",
                            value: *preferred,
                            reason: "a nominal axis carries a class, not a magnitude",
                        });
                    }
                    check_unit_range(*preferred, "environment axis preference")?;
                    if !tolerance.is_finite() || *tolerance <= 0.0 || *tolerance > 1.0 {
                        return Err(UnitError {
                            unit: "environment axis tolerance",
                            value: *tolerance,
                            reason: "must be finite and within (0, 1]",
                        });
                    }
                }
                AxisPreference::Class { accepted } => {
                    if !nominal {
                        return Err(UnitError {
                            unit: "environment axis preference",
                            value: *accepted,
                            reason: "only a nominal axis carries a class",
                        });
                    }
                    check_unit_range(*accepted, "environment axis preference")?;
                }
            }
            map.insert(axis.id, (*axis, *preference));
        }
        Ok(Self(map))
    }

    /// This niche's preference on `axis`, or `None` if it declines the axis —
    /// which, by the genus rule, means indifference rather than exclusion.
    pub fn get(&self, axis: EnvironmentAxis) -> Option<AxisPreference> {
        self.0.get(&axis.id).map(|(_, p)| *p)
    }

    /// The axis ids this niche states a preference on, ascending.
    /// type-audit: bare-ok(index: return)
    pub fn axis_ids(&self) -> Vec<u16> {
        self.0.keys().copied().collect()
    }

    /// Whether this niche states no preference at all. Such a niche scores a
    /// constant `0.0` everywhere (see [`environment_fit`]), which is uniform
    /// across places and therefore never mis-ranks one against another.
    /// type-audit: bare-ok(flag: return)
    pub fn is_indifferent(&self) -> bool {
        self.0.is_empty()
    }
}

/// Shared range check for the two preference payloads.
fn check_unit_range(value: f64, unit: &'static str) -> Result<(), UnitError> {
    if !value.is_finite() || !(0.0..=1.0).contains(&value) {
        return Err(UnitError {
            unit,
            value,
            reason: "must be finite and within [0, 1]",
        });
    }
    Ok(())
}

/// How well a place's character suits a kind's niche, in `[0, 1]`: `1.0` is an
/// exact match on every axis the two have in common.
///
/// **The measure is Gower's similarity over the shared axes**, valence-aware
/// by construction: an ordered axis contributes `|value − preferred| /
/// tolerance` capped at one, a nominal axis contributes zero or one, and the
/// mean of those dissimilarities is subtracted from one. With every tolerance
/// at its `1.0` default this is bit-for-bit `1 − d` for the `d` that Task 6
/// measured the underworld corpus with (`domains/climate/tests/underworld.rs`,
/// five values hand-reproduced by a reviewer). Euclidean distance was not
/// available: `SUBSTRATE` is `AxisValence::Nominal`, so squaring a difference
/// of class indices computes a number that means nothing.
///
/// **The genus rule.** An axis either side declines is dropped from the mean
/// rather than scored as a zero — see [`EnvironmentNiche`]. The mean is
/// therefore over the INTERSECTION, and this function never reads
/// `environment_v1_basis()` at all: it walks the niche's own recorded axes,
/// the way `ResourceVector::overlap` walks its two vectors' own keys. That is
/// what makes the basis's append-only rule (`AxisPreference`'s sibling pin,
/// `the_environment_basis_ids_are_append_only`) sufficient here — appending an
/// axis cannot perturb any existing fit, because no existing fit ever looked
/// at the basis.
///
/// **That pin is load-bearing a SECOND time, for the bitwise Gower tie above,
/// and this is the only place it is written down.** The sum here runs in
/// ascending **axis-id** order, because the niche is a `BTreeMap<u16, _>`;
/// Task 6's `distance` runs in **basis** order, because it iterates
/// `environment_v1_basis()`. Float addition is not associative, so two orders
/// give bit-identical sums only when they are the *same* order — and they are
/// the same order only because the basis is dense and ascending from zero,
/// which is precisely what
/// `the_environment_basis_ids_are_append_only` asserts. A basis that ever
/// stopped being dense-ascending would leave both instruments individually
/// correct and no longer bit-comparable.
///
/// **An unassigned place scores `0.0`, and so does any place sharing no axis
/// with the niche.** The zero vector is legal and means *unassigned* — how a
/// name the axes cannot place is represented, a finding to be counted rather
/// than an error — so this must be a defined number rather than a panic (the
/// Task 6 reference `distance` asserts instead, which is right for an
/// instrument and wrong for a consumer). `0.0` follows
/// `ResourceVector::overlap`'s precedent on the sibling basis: an empty
/// intersection is **no evidence of fit**, not perfect indifference. `1.0`
/// would be actively wrong — it would make a place the axes could not describe
/// outrank every place they could.
///
/// # The score is only comparable across places of equal arity
///
/// A mean over the **intersection** carries a sparsity bias, and it runs the
/// same direction every time: a place that states two of the niche's axes is
/// scored on two terms and can reach `1.0` on both, while a place that states
/// five is scored on all five and is penalised by every one it misses. **The
/// sparser vector is systematically advantaged.** Nothing here corrects for
/// that, and nothing should — the correction would have to invent a value for
/// an axis the place declined, which is exactly the fabrication the genus rule
/// exists to refuse.
///
/// So: **ranking places against one niche is sound only where the candidates
/// share an arity**, and a caller comparing across arities is comparing
/// different denominators. This is measured rather than feared, in
/// `domains/climate/tests/underworld.rs`'s
/// `the_underworld_corpus_has_constant_arity_and_the_surface_corpus_does_not`
/// — the species crate cannot import a sibling domain to assert it here, so
/// the ratchet lives with the corpus and this paragraph points at it. The
/// state of that measurement, as a fact about the corpora rather than about
/// this function: the **underworld** corpus is constant-arity, so a
/// single-realm chamber ranking is unaffected, while the **surface** corpus is
/// not, so any cross-corpus comparison carries the bias. Read the current
/// numbers off that test, never off this comment.
///
/// # This function cannot separate realms, and must not be asked to
///
/// The basis carries **no realm coordinate**, and Task 6 measured the
/// consequence and committed it as a ratchet: three underworld communities are
/// vector-identical to surface ones — `mud-sump` ≡ `lightless-water`,
/// `deep-karst-void` ≡ `smoker-field`, `tube-ice-trap` ≡ `ice`. A high
/// `environment_fit` therefore says "these conditions suit this kind", never
/// "this kind belongs in this realm". The realm gate is a separate mechanism
/// and stays so: [`HabitatRealm::Subterranean`] gives a kind `availability =
/// 0.0` on any vertex whose terrain holds no cave, in worldgen's
/// `per_species_suitability_masked`. A reader who assumes this function
/// discriminates realm will be wrong.
/// type-audit: bare-ok(ratio: return)
pub fn environment_fit(niche: &EnvironmentNiche, place: &EnvironmentVector) -> f64 {
    let mut total = 0.0;
    let mut shared = 0usize;
    for (axis, preference) in niche.0.values() {
        // The place's reading on this axis, or nothing — and nothing means the
        // axis leaves the mean entirely. No valence lookup is needed: the
        // preference's own variant already encodes the valence treatment, and
        // `EnvironmentNiche::new` guarantees the two agree.
        let Some(value) = place.get(*axis) else {
            continue;
        };
        shared += 1;
        total += preference.dissimilarity(value);
    }
    if shared == 0 {
        return 0.0;
    }
    // Each term is in [0, 1], so the mean is too and the difference lands in
    // [0, 1]. The clamp is a guard against float rounding at the ends, not a
    // correction for an out-of-range term — there is no such term.
    (1.0 - total / shared as f64).clamp(0.0, 1.0)
}

impl Component for EnvironmentNiche {}

/// The sparse environment-niche component: **only** kinds whose habitat is
/// stated in the environment basis appear (The Underworld, Task 8).
///
/// Sparse for the same reason [`habitat_realm_registry`] is — one consumer,
/// which holds a slice rather than a row — and it is the same consumer one
/// step further on: worldgen's realm-aware capacity, which scores a
/// subterranean people's chambers with [`environment_fit`] before deciding
/// which delve rung it seats at.
///
/// **Absence is load-bearing, and it is the campaign's positive control.** A
/// kind with no row here cannot score a chamber, so it cannot choose a rung,
/// so it stays at the surface exactly as it did before this campaign. Emptying
/// this registry therefore reverts the seating without touching capacity,
/// which is what lets the re-key's surface invariance be measured rather than
/// argued.
///
/// One row today. `rust-monster` and `xorn` are `HabitatRealm::Subterranean`
/// and are deliberately **absent**: they are fauna, they settle nothing, and
/// authoring a niche for a kind that places no community would be a value no
/// consumer reads.
pub fn environment_niche_registry() -> ComponentStore<KindId, EnvironmentNiche> {
    [(KindId("drow"), drow_niche())].into_iter().collect()
}

/// Drow's niche in the environment basis — **every value authored**, on the
/// same five axes and the same value grid `hornvale_climate`'s underworld
/// corpus uses, so a fit against one of its 22 communities is a comparison of
/// two points on one ruler rather than of two vocabularies.
///
/// Drow is the store's first and only occupant for the reason
/// [`habitat_realm_registry`] gives for its own drow row: it is the peopled
/// subterranean kind that is already shipped, so it is the producer that does
/// not wait on §4.7's two dwarves being authored.
///
/// Per axis, and each is a claim about a people rather than about a cave:
///
/// - `PHYSIOGNOMY` **0.6** — standing structure. A city needs something to
///   build in and on: speleothem stands, gypsum curtains, fungal thickets. A
///   smooth lava pipe (0.0) offers nothing to hold a settlement, and a fully
///   decorated gallery (0.8) is a place to walk through rather than to live
///   in. AUTHORED.
/// - `ENERGY` **0.5** — a working base. Drow farm; a system with a stream's
///   organic load or a modest chemical one is what a farmed underworld looks
///   like. Neither inert rock (0.0) nor a whole channel's load at one point
///   (1.0), which is a hot spring rather than a country. AUTHORED.
/// - `WATER` **0.4** — fracture-borne seepage: enough to drink, not enough to
///   drown in. This is the axis that says drow live in the dry part of a wet
///   world; `1.0` is below the water table. AUTHORED.
/// - `SUBSTRATE` **0.6** (rock) — a `Class`, never a magnitude, because the
///   axis is `AxisValence::Nominal`. A people builds on rock; mud, sand, ice
///   and buried carbon are floors you cross. AUTHORED.
/// - `LIGHT` **0.0** — aphotic, which is the one axis drow's existing
///   authoring already stated in another vocabulary (its cave-dark insolation
///   response). AUTHORED.
///
/// **Every tolerance is the neutral default** ([`AxisPreference::graded`]'s
/// `1.0`), and that is a refusal rather than an omission: a narrower tolerance
/// on any axis would be a second, unmeasured calibration authored at the same
/// moment as the preference it modifies, and nothing in this campaign measures
/// how fussy a drow is. At the default the fit reduces bit-for-bit to `1 − d`
/// for the Gower distance Task 6 measured the corpus with, so the number this
/// niche produces is comparable with that instrument's own.
///
/// Stated in the same arity as every row of the underworld corpus — five axes,
/// all five occupied — so `environment_fit`'s sparsity bias cannot reach a
/// chamber ranking. `DISTURBANCE` is declined for the reason no corpus can
/// occupy it: it is the basis's only `Rate`, and a people is a state.
fn drow_niche() -> EnvironmentNiche {
    EnvironmentNiche::new(&[
        (
            hornvale_kernel::PHYSIOGNOMY,
            AxisPreference::graded(DROW_PHYSIOGNOMY),
        ),
        (hornvale_kernel::ENERGY, AxisPreference::graded(DROW_ENERGY)),
        (hornvale_kernel::WATER, AxisPreference::graded(DROW_WATER)),
        (
            hornvale_kernel::SUBSTRATE,
            AxisPreference::class(DROW_SUBSTRATE),
        ),
        (hornvale_kernel::LIGHT, AxisPreference::graded(DROW_LIGHT)),
    ])
    .expect("drow's authored niche is valid: five in-range values, class on the nominal axis")
}

/// Drow's preferred void form: standing structure. AUTHORED — see
/// [`drow_niche`].
/// type-audit: bare-ok(ratio)
const DROW_PHYSIOGNOMY: f64 = 0.6;
/// Drow's preferred energy base: a working one. AUTHORED — see [`drow_niche`].
/// type-audit: bare-ok(ratio)
const DROW_ENERGY: f64 = 0.5;
/// Drow's preferred moisture: fracture-borne seepage. AUTHORED — see
/// [`drow_niche`].
/// type-audit: bare-ok(ratio)
const DROW_WATER: f64 = 0.4;
/// Drow's accepted substrate class: bare rock. A class index, never a
/// magnitude. AUTHORED — see [`drow_niche`].
/// type-audit: bare-ok(index)
const DROW_SUBSTRATE: f64 = 0.6;
/// Drow's preferred light level: aphotic. AUTHORED — see [`drow_niche`].
/// type-audit: bare-ok(ratio)
const DROW_LIGHT: f64 = 0.0;

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::test_lineage;
    use hornvale_kernel::{Fact, Seed};

    /// The predicate reads the THERMAL axis alone, because no caller holds
    /// the other one (spec §4.3, corrected after Task 4). What makes that
    /// safe is the sanctioned-pair table, not this function — so assert the
    /// discrimination it DOES provide, and do not pretend to a check it
    /// cannot make.
    #[test]
    fn is_ametabolic_is_true_only_for_the_absent_thermal_strategy() {
        use super::{ThermalStrategy as T, is_ametabolic};
        assert!(is_ametabolic(T::Absent));
        assert!(!is_ametabolic(T::Endothermic));
        assert!(!is_ametabolic(T::Ectothermic));
        assert!(
            !is_ametabolic(T::Unmodelled),
            "Unmodelled means a metabolism nobody has modelled, NOT the \
             absence of one — collapsing the two is the exact conflation this \
             campaign split the enum to remove"
        );
    }

    #[test]
    fn bio2_adds_no_stream_label() {
        // The life-history layer is authored constants + pure derivations: it
        // must introduce NO new seed-derivation stream. Species streams stay
        // empty (species are authored, not drawn); guard against a future
        // BIO-2-adjacent change quietly adding a life/allometry/metabolic draw.
        let labels = stream_labels();
        assert!(
            labels.is_empty(),
            "species crate must register no streams at all: {labels:?}"
        );
        assert!(
            !labels.iter().any(|(k, _)| k.contains("life")
                || k.contains("allometry")
                || k.contains("metabolic")),
            "BIO-2 must not register a stream: {labels:?}"
        );
    }

    #[test]
    fn component_registries_are_consistent() {
        // With the god-struct gone, the four registries author independently.
        // The cross-registry invariants the world relies on: biosphere and
        // family cover the SAME full kind set, and psyche/perception share
        // exactly one key-set — the fifteen peoples plus the three minded
        // dragons — every one of which also carries a biosphere row.
        let bio = biosphere_registry();
        let fam = family_of();
        let psy = psyche_registry();
        let per = perception_registry();

        assert_eq!(
            bio.len(),
            39,
            "thirty-nine kinds compete for space (The Vacancy T7 added seven, T8 added five, T9 added the gnoll, The Generalist added the human, The Delvers added the three dwarves, The Radiation added the six elves)"
        );
        let bio_ids: Vec<_> = bio.ids().collect();
        let fam_ids: Vec<_> = fam.ids().collect();
        assert_eq!(bio_ids, fam_ids, "family covers exactly the biosphere set");

        // Capacities nest (The Eremite, tightened by The Vigil): perception ⊆
        // psyche, and since The Vigil every minded SPEAKER also perceives, so
        // the two stores again share one key-set — eighteen kinds, not the
        // fifteen peoples alone.
        for kind in per.ids() {
            assert!(
                psy.contains(kind),
                "perceiver {kind:?} carries a mind (perception ⊆ psyche)"
            );
        }
        assert_eq!(psy.len(), 18, "fifteen peoples + three minded dragons");
        assert_eq!(
            per.len(),
            18,
            "perception is the fifteen peoples + the three dragons (The Vigil)"
        );
        for kind in psy.ids() {
            assert!(bio.contains(kind), "minded {kind:?} has a biosphere row");
        }
    }

    #[test]
    fn concepts_registered() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        for name in [
            "goblin-kind",
            "kobold-kind",
            "hobgoblin-kind",
            "bugbear-kind",
            "gnoll-kind",
        ] {
            let c = r
                .concept(name)
                .unwrap_or_else(|| panic!("missing concept {name}"));
            assert_eq!(c.domain, "species");
            assert_eq!(c.kind, ConceptKind::Living);
        }
    }

    /// CHARACTERIZATION, NOT CONTRACT.
    ///
    /// Goblin is currently authored at exactly the manikin's values. That is
    /// authorship, not definition: goblin was the first people written down,
    /// and nobody ever decided that goblins are unremarkable. Nothing in the
    /// model requires a kind to sit on the manikin, and this test does not
    /// make it a requirement.
    ///
    /// It exists so that characterising goblin — giving it the impulsive,
    /// short-horizon profile it has never actually been given — arrives as a
    /// visible diff on this test rather than as a silent shift in every
    /// goblin-bearing world's language envelope, culture rungs and demography
    /// weights. When that campaign comes, DELETE this test; do not "fix" it.
    ///
    /// The pattern is The Vacancy's, applied in this same registry to the
    /// `Autotroph`/Kleiber divergence.
    #[test]
    fn goblin_is_currently_authored_at_the_manikin() {
        let mind = *psyche_registry().get(&KindId("goblin")).unwrap();
        assert_eq!(
            mind,
            MindVector::MANIKIN,
            "goblin's mind is authored at the manikin (characterization)"
        );

        let society = *society_registry().get(&KindId("goblin")).unwrap();
        assert_eq!(
            society,
            SocietyVector::MANIKIN,
            "goblin's society is authored at the manikin (characterization)"
        );
    }

    /// The manikin is the model's reference vector: neutral on every scalar,
    /// and a designated default on the enums (which have no midpoint to be
    /// neutral at — see the spec's flagged item 5). It belongs to no creature.
    #[test]
    fn the_manikin_is_neutral_on_scalars_and_default_on_enums() {
        let mind = MindVector::MANIKIN;
        for v in [
            mind.threat_response,
            mind.deliberation_latency,
            mind.time_horizon,
        ] {
            assert_eq!(v, 0.5, "every manikin mind scalar is the neutral midpoint");
        }

        let society = SocietyVector::MANIKIN;
        assert_eq!(society.in_group_radius, 0.5);
        assert_eq!(society.sociality, Sociality::Hierarchic);
        assert_eq!(society.status_basis, StatusBasis::Rank);

        let perception = PerceptionVector::MANIKIN;
        for v in [perception.night_vision, perception.sky_attention] {
            assert_eq!(v, 0.5, "every manikin perception scalar is the midpoint");
        }
        assert_eq!(perception.activity, ActivityCycle::Diurnal);
    }

    #[test]
    fn registry_is_ordered_alphabetically_and_kobold_contrasts() {
        let bio = biosphere_registry();
        let names: Vec<&str> = bio.ids().map(|k| k.0).collect();
        // The roster grew with the Task 4 menagerie (12 biosphere-only fauna
        // alongside the four peoples), then with The Vacancy's T7 (seven more
        // biosphere-only fauna), T8 (five more, four marine plus the
        // amphibious giant crocodile), T9 (the gnoll, the fifth people), The
        // Generalist (the human, the sixth people), The Delvers (the three
        // dwarves, peoples seven through nine), and The Radiation (the six
        // elves, peoples ten through fifteen); ComponentStore key order is
        // lexicographic, so the family scatters rather than clustering — the
        // elves land in six separate places, `high-elf` between `gully-dwarf`
        // and `hill-dwarf`, `wood-elf` between `white-dragon` and
        // `woolly-mammoth`.
        assert_eq!(
            names,
            vec![
                "black-dragon",
                "bugbear",
                "carrion-crawler",
                "desert-dwarf",
                "desert-elf",
                "dire-wolf",
                "drow",
                "giant-constrictor-snake",
                "giant-crocodile",
                "giant-elk",
                "giant-goat",
                "giant-hyena",
                "giant-octopus",
                "giant-scorpion",
                "giant-squid",
                "gnoll",
                "goblin",
                "gully-dwarf",
                "high-elf",
                "hill-dwarf",
                "hobgoblin",
                "human",
                "killer-whale",
                "kobold",
                "otyugh",
                "owlbear",
                "red-dragon",
                "reef-shark",
                "rhinoceros",
                "rust-monster",
                "sea-elf",
                "shrieker",
                "snow-elf",
                "treant",
                "twig-blight",
                "white-dragon",
                "wood-elf",
                "woolly-mammoth",
                "xorn",
            ]
        );
        let psy = psyche_registry();
        let k = psy.get(&KindId("kobold")).unwrap();
        assert!(k.time_horizon > 0.5 && k.threat_response > 0.5);
        let soc = society_registry();
        let k_soc = soc.get(&KindId("kobold")).unwrap();
        assert_eq!(k_soc.sociality, Sociality::Communal);
        assert_eq!(k_soc.status_basis, StatusBasis::Knowledge);
        assert!(k_soc.in_group_radius < 0.5);
    }

    #[test]
    fn people_links_a_settlement_to_its_species() {
        // The ledger helpers `people` / `species_of` / `species_entity` stay in
        // species (they don't touch the deleted god-struct). Genesis moved to
        // `worldgen::species_genesis`; its byte-identity is proven by the
        // seed-42 world fixture and worldgen's own tests, so it is not re-tested
        // here.
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let settlement = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        people(&mut w, settlement, "kobold").unwrap();
        assert_eq!(species_of(&w, settlement).as_deref(), Some("kobold"));

        // `species_entity` resolves a committed SPECIES_NAME fact back to its
        // entity (the fact worldgen's genesis commits at world build).
        let kobold = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        w.ledger
            .commit(
                Fact {
                    subject: kobold,
                    predicate: SPECIES_NAME.to_string(),
                    object: Value::Text("kobold".to_string()),
                    place: None,
                    day: Some(WorldTime::GENESIS),
                    provenance: "species".to_string(),
                },
                &w.registry,
            )
            .unwrap();
        assert_eq!(species_entity(&w, "kobold"), Some(kobold));
    }

    /// CHARACTERIZATION, NOT CONTRACT — the perception half of
    /// `goblin_is_currently_authored_at_the_manikin`. Goblin's authored
    /// perception coincides with [`PerceptionVector::MANIKIN`]; nothing in the
    /// model requires that, and kobold is here to show the vector genuinely
    /// varies across the roster.
    ///
    /// Unlike its sibling, this test cannot simply be deleted the day goblin
    /// is characterised on its own merits: it welds one characterization
    /// assertion (goblin's row) to one real contract (the kobold contrast,
    /// which pins that the vector genuinely varies across the roster and
    /// must survive). When that day comes, split this test — delete the
    /// goblin assertions, keep the kobold ones — rather than deleting the
    /// whole function or leaving the stale goblin assertions in place.
    #[test]
    fn goblin_perception_is_authored_at_the_manikin_and_kobold_contrasts() {
        let per = perception_registry();
        let g = per.get(&KindId("goblin")).unwrap();
        assert_eq!(g.activity, ActivityCycle::Diurnal);
        assert_eq!(g.night_vision, 0.5);
        assert_eq!(g.sky_attention, 0.5);
        let k = per.get(&KindId("kobold")).unwrap();
        assert_eq!(k.activity, ActivityCycle::Nocturnal);
        assert!(k.night_vision > 0.5 && k.sky_attention > 0.5);
    }

    #[test]
    fn draconic_perception_is_one_clade_eye_and_three_schedules() {
        let per = perception_registry();
        // The clade eye: night_vision is the ONLY perception dimension that
        // reaches language (sole input to `pack_depths`), so every dragon
        // shares one value — a per-dragon value would give each dragon its own
        // hue inventory and fragment the shared Draconic tongue.
        for name in ["white-dragon", "red-dragon", "black-dragon"] {
            let d = per
                .get(&KindId(name))
                .unwrap_or_else(|| panic!("{name} carries a perception row"));
            assert_eq!(
                d.night_vision, DRACONIC_NIGHT_VISION,
                "{name} shares the clade eye"
            );
            assert!(
                d.sky_attention < 0.6,
                "{name} is a ground-scanning predator, not sky-rapt"
            );
        }
        // The ecological schedule: activity is read off each kind's own
        // authored insolation optimum, so the three differ.
        assert_eq!(
            per.get(&KindId("red-dragon")).unwrap().activity,
            ActivityCycle::Diurnal,
            "red-dragon: insolation optimum 0.20, open volcanic high sun"
        );
        assert_eq!(
            per.get(&KindId("black-dragon")).unwrap().activity,
            ActivityCycle::Nocturnal,
            "black-dragon: insolation optimum 0.10, shaded swamp ambush"
        );
        assert_eq!(
            per.get(&KindId("white-dragon")).unwrap().activity,
            ActivityCycle::Crepuscular,
            "white-dragon: insolation optimum 0.05, polar twilight"
        );
    }

    #[test]
    fn registry_has_the_goblinoid_triad_and_kobold() {
        let bio = biosphere_registry();
        let fam = family_of();
        for name in ["goblin", "hobgoblin", "bugbear", "kobold"] {
            assert!(bio.contains(&KindId(name)), "{name} missing");
        }
        assert_eq!(fam.get(&KindId("hobgoblin")), Some(&"goblinoid"));
        assert_eq!(fam.get(&KindId("bugbear")), Some(&"goblinoid"));
        assert_eq!(fam.get(&KindId("kobold")), Some(&"kobold"));
    }

    #[test]
    fn goblinoids_carry_mass_and_a_nonzero_omnivore_niche() {
        let bio = biosphere_registry();
        for name in ["goblin", "kobold", "hobgoblin", "bugbear"] {
            let s = bio.get(&KindId(name)).unwrap();
            assert!(s.mass.kilograms() > 0.0, "{name} has mass");
            assert!(!s.niche.is_zero(), "{name} eats something");
            // omnivores: both plant-forage and animal-prey present
            assert!(s.niche.weight(hornvale_kernel::PLANT_FORAGE) > 0.0);
            assert!(s.niche.weight(hornvale_kernel::ANIMAL_PREY) > 0.0);
        }
        // strict, modest, monotone mass band: kobold < goblin < hobgoblin < bugbear
        let kg = |n: &'static str| bio.get(&KindId(n)).unwrap().mass.kilograms();
        assert!(kg("kobold") < kg("goblin"));
        assert!(kg("goblin") < kg("hobgoblin"));
        assert!(kg("hobgoblin") < kg("bugbear"));
    }

    #[test]
    fn every_species_has_a_finite_condition_niche() {
        for (kind, def) in biosphere_registry_pairs() {
            let name = kind.0;
            for r in [
                def.condition_niche.temperature,
                def.condition_niche.moisture,
                def.condition_niche.insolation,
                def.condition_niche.elevation,
            ] {
                assert!(r.optimum.is_finite(), "{name} optimum finite");
                assert!(
                    r.width.is_finite() && r.width > 0.0,
                    "{name} width positive"
                );
                assert!(r.devotion.is_finite(), "{name} devotion finite");
            }
            assert!(
                def.potency >= 0.0 && def.potency.is_finite(),
                "{name} potency >= 0"
            );
        }
    }

    #[test]
    fn the_five_peoples_have_distinct_temperature_optima() {
        let bio = biosphere_registry();
        let opts: Vec<f64> = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll"]
            .iter()
            .map(|n| {
                bio.get(&KindId(n))
                    .unwrap()
                    .condition_niche
                    .temperature
                    .optimum
            })
            .collect();
        // the anti-uniformity guard: all four temperature optima pairwise distinct
        for i in 0..opts.len() {
            for j in (i + 1)..opts.len() {
                assert!(
                    (opts[i] - opts[j]).abs() > 1e-6,
                    "temperature optima {i} and {j} must differ (broke the oatmeal)"
                );
            }
        }
    }

    #[test]
    fn every_species_has_a_metabolic_class() {
        use ThermalStrategy::*;
        let bio = biosphere_registry();
        let mc = |n: &'static str| bio.get(&KindId(n)).unwrap().thermal_strategy;
        assert_eq!(mc("goblin"), Endothermic);
        assert_eq!(mc("hobgoblin"), Endothermic);
        assert_eq!(mc("bugbear"), Endothermic);
        assert_eq!(mc("kobold"), Ectothermic); // reptilian/draconic SRD lineage
    }

    #[test]
    fn split_preserves_biosphere_and_peopled_presence() {
        let bio = biosphere_registry();
        let psy = psyche_registry();
        // biosphere authored intact
        let goblin = bio.get(&KindId("goblin")).unwrap();
        assert_eq!(goblin.mass, Mass::new(18.1).unwrap());
        assert_eq!(goblin.potency, 0.0);
        // these four (of today's six) peoples all speak/settle (carry a psyche row)
        for name in ["goblin", "kobold", "hobgoblin", "bugbear"] {
            assert!(psy.contains(&KindId(name)), "{name} must carry a psyche");
        }
    }

    #[test]
    fn menagerie_is_biosphere_only_and_spans_axes() {
        let bio = biosphere_registry();
        let psy = psyche_registry();
        let per = perception_registry();
        for name in [
            "treant",
            "twig-blight",
            "giant-elk",
            "woolly-mammoth",
            "giant-goat",
            "otyugh",
            "xorn",
            "rust-monster",
            "white-dragon",
            "red-dragon",
            "black-dragon",
            "owlbear",
        ] {
            let d = bio.get(&KindId(name)).unwrap();
            // The Eremite: the three dragons are MINDED fauna — a solitary
            // psyche. Since The Vigil they also perceive (one clade eye,
            // three schedules — see `draconic_perception_is_one_clade_eye_
            // and_three_schedules`). Every other menagerie kind carries
            // neither capacity.
            let is_dragon = matches!(name, "white-dragon" | "red-dragon" | "black-dragon");
            assert_eq!(
                psy.contains(&KindId(name)),
                is_dragon,
                "{name}: only the dragons among the menagerie carry a mind"
            );
            assert_eq!(
                per.contains(&KindId(name)),
                is_dragon,
                "{name}: only the dragons among the menagerie perceive (The Vigil)"
            );
            // `Mass` has no PartialOrd, so read the raw kilograms rather
            // than comparing against `Mass::new(0.0)`.
            assert!(d.mass.kilograms() > 0.0, "{name} has mass");
        }
        // mighty creatures carry potency
        assert!(bio.get(&KindId("red-dragon")).unwrap().potency > 0.0);
        assert!(bio.get(&KindId("treant")).unwrap().potency > 0.0);
        assert!(bio.get(&KindId("xorn")).unwrap().potency > 0.0);
        // the material, non-mighty fauna carry none
        assert_eq!(bio.get(&KindId("owlbear")).unwrap().potency, 0.0);
        assert_eq!(bio.get(&KindId("rust-monster")).unwrap().potency, 0.0);

        // resource niches are partitioned, not four omnivores: the distinct
        // dominant axis differs across creatures. `ResourceVector::overlap`
        // is the packer's Pianka overlap; disjoint axes overlap 0.
        let overlap = bio
            .get(&KindId("treant"))
            .unwrap()
            .niche
            .overlap(&bio.get(&KindId("white-dragon")).unwrap().niche);
        assert!(
            overlap < 0.5,
            "photosynthate vs apex niches must barely overlap"
        );

        // Directly assert the basis-constant partition the brief calls for.
        let w = |n: &'static str, axis| bio.get(&KindId(n)).unwrap().niche.weight(axis);
        assert_eq!(w("treant", PHOTOSYNTHATE), 1.0);
        assert_eq!(w("twig-blight", PHOTOSYNTHATE), 1.0);
        for name in ["giant-elk", "woolly-mammoth", "giant-goat"] {
            assert_eq!(w(name, PLANT_FORAGE), 1.0);
        }
        for name in ["white-dragon", "red-dragon", "black-dragon", "owlbear"] {
            assert_eq!(w(name, ANIMAL_PREY), 1.0);
        }
        assert_eq!(w("otyugh", DETRITUS), 1.0);
        for name in ["xorn", "rust-monster"] {
            assert_eq!(w(name, MINERAL), 1.0);
        }
    }

    #[test]
    fn menagerie_families_and_climate_tiles_distinct() {
        // The three chromatics and the two plant kinds are multi-member
        // families (their proto vectors live in `hornvale_language`, tested
        // there); here we pin the family labels and the distinct climate tiles.
        let fam = family_of();
        for name in ["white-dragon", "red-dragon", "black-dragon"] {
            assert_eq!(fam.get(&KindId(name)), Some(&"draconic"));
        }
        for name in ["treant", "twig-blight"] {
            assert_eq!(fam.get(&KindId(name)), Some(&"plant"));
        }

        // The three chromatics claim distinct climate tiles even though they
        // share the animal-prey axis: white owns the cold, and red/black —
        // both warm — separate on moisture (volcanic-arid vs. swamp-wet).
        let bio = biosphere_registry();
        let temp = |n: &'static str| {
            bio.get(&KindId(n))
                .unwrap()
                .condition_niche
                .temperature
                .optimum
        };
        let moisture = |n: &'static str| {
            bio.get(&KindId(n))
                .unwrap()
                .condition_niche
                .moisture
                .optimum
        };
        assert!(temp("white-dragon") < temp("red-dragon"));
        assert!(temp("white-dragon") < temp("black-dragon"));
        assert!(
            moisture("red-dragon") < moisture("black-dragon") - 0.3,
            "volcanic-arid red must sit well below swamp-wet black on moisture"
        );
    }

    // A test-only pairing of the biosphere store as (KindId, &BiosphereTraits)
    // so the condition-niche sweep can name each kind in its assertions.
    fn biosphere_registry_pairs() -> Vec<(KindId, BiosphereTraits)> {
        let bio = biosphere_registry();
        bio.ids()
            .map(|k| (*k, bio.get(k).unwrap().clone()))
            .collect()
    }

    /// The fallback a mixed consumer resolves is the manikin — stated without
    /// reference to any people. Before The Manikin this test asserted the
    /// fallback equalled *goblin's* authored society, which welded the model's
    /// identity element to one inhabitant of the world.
    #[test]
    fn the_society_fallback_is_the_manikin() {
        assert_eq!(
            SocietyVector::MANIKIN,
            SocietyVector {
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
                in_group_radius: 0.5,
            },
            "the fallback is the manikin, and the manikin is nobody's"
        );
    }

    #[test]
    fn society_registry_holds_exactly_the_settled_peoples() {
        let society: Vec<_> = society_registry().ids().map(|k| k.0).collect();
        assert_eq!(
            society,
            vec![
                "bugbear",
                "desert-dwarf",
                "desert-elf",
                "drow",
                "gnoll",
                "goblin",
                "gully-dwarf",
                "high-elf",
                "hill-dwarf",
                "hobgoblin",
                "human",
                "kobold",
                "sea-elf",
                "snow-elf",
                "wood-elf"
            ]
        );
        // dragons are minded (psyche) but not Settled — no society vector
        assert!(society_registry().get(&KindId("red-dragon")).is_none());
        assert!(psyche_registry().get(&KindId("red-dragon")).is_some());
    }

    /// The Underworld, Task 7: `EnvironmentNiche` and `environment_fit`.
    ///
    /// A nested module so that every test below carries `environment` in its
    /// path and `cargo test -p hornvale-species environment` selects them. The
    /// plan's own step-2 command was that filter, and against flat names none
    /// of these carries the word — it reported `0 passed` and would have read
    /// as a green run.
    mod environment_niche {
        use super::*;
        use hornvale_kernel::{
            DISTURBANCE, ENERGY, LIGHT, PHYSIOGNOMY, SUBSTRATE, WATER, environment_v1_basis,
        };

        /// A place vector, panicking on a value the kernel would reject — a test
        /// that authors an invalid vector has a bug in the test, not a finding.
        fn place(values: &[(EnvironmentAxis, f64)]) -> EnvironmentVector {
            EnvironmentVector::new(values).expect("the test's place vector must be valid")
        }

        /// A niche, panicking on a preference the constructor would reject.
        fn a_niche(prefs: &[(EnvironmentAxis, AxisPreference)]) -> EnvironmentNiche {
            EnvironmentNiche::new(prefs).expect("the test's niche must be valid")
        }

        #[test]
        fn a_niche_matching_a_place_exactly_scores_one() {
            let p = place(&[
                (PHYSIOGNOMY, 0.2),
                (ENERGY, 0.8),
                (WATER, 0.4),
                (SUBSTRATE, 0.6),
            ]);
            let n = a_niche(&[
                (PHYSIOGNOMY, AxisPreference::graded(0.2)),
                (ENERGY, AxisPreference::graded(0.8)),
                (WATER, AxisPreference::graded(0.4)),
                // SUBSTRATE is Nominal: a class, never a magnitude.
                (SUBSTRATE, AxisPreference::class(0.6)),
            ]);
            assert_eq!(
                environment_fit(&n, &p),
                1.0,
                "a niche sitting exactly on a place's reading is a perfect fit"
            );

            // The positive control the `1.0` needs: the score is not simply
            // constant. One axis moved off the preference must score strictly
            // below one, on the graded axis and on the nominal one alike.
            let off_graded = place(&[
                (PHYSIOGNOMY, 0.2),
                (ENERGY, 0.3),
                (WATER, 0.4),
                (SUBSTRATE, 0.6),
            ]);
            let off_nominal = place(&[
                (PHYSIOGNOMY, 0.2),
                (ENERGY, 0.8),
                (WATER, 0.4),
                (SUBSTRATE, 0.0),
            ]);
            assert!(environment_fit(&n, &off_graded) < 1.0);
            assert!(environment_fit(&n, &off_nominal) < 1.0);
        }

        #[test]
        fn an_unassigned_place_vector_scores_a_defined_value_not_a_panic() {
            // The zero vector is LEGAL and means unassigned (kernel docs). A niche
            // scored against it must return a defined number.
            let unassigned = EnvironmentVector::new(&[]).expect("the zero vector is legal");
            assert!(unassigned.is_unassigned());
            let n = a_niche(&[(ENERGY, AxisPreference::graded(0.9))]);

            let fit = environment_fit(&n, &unassigned);
            assert!(fit.is_finite(), "an unassigned place must score a number");
            assert_eq!(
                fit, 0.0,
                "no shared axis is no evidence of fit — `ResourceVector::overlap`'s \
             precedent on the sibling basis, and the value that keeps an \
             unplaceable place from outranking a described one"
            );

            // The mechanism is the EMPTY INTERSECTION, not the empty vector: a
            // place that carries axes, none of which the niche speaks to, takes
            // the same value.
            let disjoint = place(&[(WATER, 0.9)]);
            assert_eq!(environment_fit(&n, &disjoint), 0.0);

            // And the value is the right way round: a described place the niche
            // matches outranks the unassigned one.
            let described = place(&[(ENERGY, 0.9)]);
            assert!(environment_fit(&n, &described) > environment_fit(&n, &unassigned));
        }

        #[test]
        fn fit_is_bounded_in_zero_one_over_the_whole_basis() {
            let levels = [0.0, 0.2, 0.4, 0.5, 0.6, 0.8, 1.0];
            let tolerances = [0.05, 0.2, 0.5, 1.0];
            let mut saw_zero = false;
            let mut saw_one = false;

            for axis in environment_v1_basis() {
                for preferred in levels {
                    for value in levels {
                        for tolerance in tolerances {
                            let pref = if axis.valence == AxisValence::Nominal {
                                AxisPreference::Class {
                                    accepted: preferred,
                                }
                            } else {
                                AxisPreference::Graded {
                                    preferred,
                                    tolerance,
                                }
                            };
                            let fit = environment_fit(
                                &a_niche(&[(*axis, pref)]),
                                &place(&[(*axis, value)]),
                            );
                            assert!(
                                (0.0..=1.0).contains(&fit),
                                "fit out of [0, 1] on {}: preferred {preferred}, value {value}, \
                             tolerance {tolerance} -> {fit}",
                                axis.label
                            );
                            saw_zero |= fit == 0.0;
                            saw_one |= fit == 1.0;
                        }
                    }
                }
            }
            assert!(
                saw_zero && saw_one,
                "the sweep must reach both ends of the interval, or the bound is \
             being asserted against a constant"
            );

            // The whole basis at once, since a per-axis sweep cannot see an
            // averaging bug across axes.
            let prefs: Vec<(EnvironmentAxis, AxisPreference)> = environment_v1_basis()
                .iter()
                .map(|a| {
                    let p = if a.valence == AxisValence::Nominal {
                        AxisPreference::class(0.4)
                    } else {
                        AxisPreference::graded(0.4)
                    };
                    (*a, p)
                })
                .collect();
            let full = a_niche(&prefs);
            let same: Vec<(EnvironmentAxis, f64)> =
                environment_v1_basis().iter().map(|a| (*a, 0.4)).collect();
            assert_eq!(environment_fit(&full, &place(&same)), 1.0);
            let far: Vec<(EnvironmentAxis, f64)> =
                environment_v1_basis().iter().map(|a| (*a, 1.0)).collect();
            let f = environment_fit(&full, &place(&far));
            assert!((0.0..=1.0).contains(&f), "six-axis fit out of range: {f}");
            assert!(
                f > 0.0 && f < 1.0,
                "the six-axis case must be interior: {f}"
            );
        }

        #[test]
        fn an_axis_the_niche_declines_does_not_constrain_the_fit() {
            // Mirrors the genus rule The Axes established: a niche silent on an
            // axis is indifferent to it, never zero on it.
            let n = a_niche(&[
                (ENERGY, AxisPreference::graded(0.9)),
                (WATER, AxisPreference::graded(0.5)),
            ]);
            let quiet = place(&[(ENERGY, 0.9), (WATER, 0.5)]);
            let agreeing = place(&[
                (ENERGY, 0.9),
                (WATER, 0.5),
                (LIGHT, 0.0),
                (PHYSIOGNOMY, 1.0),
            ]);
            let disagreeing = place(&[
                (ENERGY, 0.9),
                (WATER, 0.5),
                (LIGHT, 1.0),
                (PHYSIOGNOMY, 0.0),
            ]);

            assert_eq!(
                environment_fit(&n, &quiet).to_bits(),
                environment_fit(&n, &agreeing).to_bits(),
                "a declined axis is dropped from the mean, so its value cannot move the fit"
            );
            assert_eq!(
                environment_fit(&n, &quiet).to_bits(),
                environment_fit(&n, &disagreeing).to_bits(),
                "and that holds whichever way the declined axis reads"
            );
            assert_eq!(environment_fit(&n, &quiet), 1.0);

            // THE FAILURE MODE THIS RULES OUT, stated as a number. Were a declined
            // axis scored as a zero rather than dropped, both `agreeing` and
            // `disagreeing` would score 2/4 = 0.5 and every partially-specified
            // niche would read as a poor fit everywhere — failing quietly rather
            // than loudly.
            assert!(
                environment_fit(&n, &disagreeing) > 0.5,
                "a silent axis is being scored as a zero, not as indifference"
            );
        }

        #[test]
        fn an_appended_basis_axis_cannot_perturb_an_existing_fit() {
            // The Axes' A-1 invariant, honoured structurally: the basis is
            // append-only (`the_environment_basis_ids_are_append_only`), and
            // `environment_fit` never reads the basis at all — it iterates the
            // niche's own recorded axes and looks each one up in the place, the
            // way `ResourceVector::overlap` iterates its two vectors' own keys.
            // `DISTURBANCE` is the stand-in for any axis a later campaign appends:
            // nothing occupies it today.
            let n = a_niche(&[(ENERGY, AxisPreference::graded(0.3))]);
            let p = place(&[(ENERGY, 0.5)]);
            let base = environment_fit(&n, &p);

            let place_plus = place(&[(ENERGY, 0.5), (DISTURBANCE, 1.0)]);
            assert_eq!(
                base.to_bits(),
                environment_fit(&n, &place_plus).to_bits(),
                "a place carrying the appended axis must score bit-identically"
            );

            let niche_plus = a_niche(&[
                (ENERGY, AxisPreference::graded(0.3)),
                (DISTURBANCE, AxisPreference::graded(0.0)),
            ]);
            assert_eq!(
                base.to_bits(),
                environment_fit(&niche_plus, &p).to_bits(),
                "a niche carrying the appended axis must score bit-identically \
             against a place that declines it"
            );

            // The positive control: `base` is not a constant this test would
            // match no matter what.
            assert_ne!(
                base.to_bits(),
                environment_fit(&n, &place(&[(ENERGY, 0.9)])).to_bits()
            );
        }

        #[test]
        fn a_nominal_axis_is_scored_as_a_class_never_as_a_magnitude() {
            // `SUBSTRATE` is `AxisValence::Nominal` — its six classes are soil /
            // sand / evaporite / rock / ice / organic, and the numeric value
            // indexes them. |0.0 - 0.2| is NOT a smaller difference than
            // |0.0 - 1.0|: they are both "a different class".
            let n = a_niche(&[(SUBSTRATE, AxisPreference::class(0.0))]);
            assert_eq!(environment_fit(&n, &place(&[(SUBSTRATE, 0.2)])), 0.0);
            assert_eq!(environment_fit(&n, &place(&[(SUBSTRATE, 1.0)])), 0.0);
            assert_eq!(environment_fit(&n, &place(&[(SUBSTRATE, 0.0)])), 1.0);
            // Negative zero is the same class as zero — the one place a bitwise
            // comparison would silently disagree with the meaning.
            assert_eq!(environment_fit(&n, &place(&[(SUBSTRATE, -0.0)])), 1.0);

            // The constructor refuses both ways of assuming the axes are
            // homogeneous, which is what keeps the scoring above true by
            // construction rather than by care.
            assert!(
                EnvironmentNiche::new(&[(SUBSTRATE, AxisPreference::graded(0.0))]).is_err(),
                "a graded preference on a nominal axis reads an index as a magnitude"
            );
            assert!(
                EnvironmentNiche::new(&[(ENERGY, AxisPreference::class(0.0))]).is_err(),
                "a class preference on a scalar axis throws away the magnitude"
            );
        }

        #[test]
        fn a_repeated_axis_id_overwrites_rather_than_combining() {
            // `EnvironmentNiche::new`'s doc claims last-write-wins, matching
            // `EnvironmentVector::new`. A doc claim no test holds is this
            // campaign's most-repeated defect, so this holds it.
            let n = a_niche(&[
                (ENERGY, AxisPreference::graded(0.0)),
                (ENERGY, AxisPreference::graded(1.0)),
            ]);
            assert_eq!(n.axis_ids(), vec![ENERGY.id], "one entry, not two");
            assert_eq!(
                n.get(ENERGY),
                Some(AxisPreference::graded(1.0)),
                "the LAST write wins"
            );

            // Observable through the fit, not only through the accessor —
            // otherwise this would pin the storage and not the behaviour.
            assert_eq!(environment_fit(&n, &place(&[(ENERGY, 1.0)])), 1.0);
            assert_eq!(environment_fit(&n, &place(&[(ENERGY, 0.0)])), 0.0);

            // And the overwriting write is validated like any other: it cannot
            // smuggle a valence mismatch in behind a legal first write.
            assert!(
                EnvironmentNiche::new(&[
                    (SUBSTRATE, AxisPreference::class(0.0)),
                    (SUBSTRATE, AxisPreference::graded(0.5)),
                ])
                .is_err()
            );
        }

        #[test]
        fn a_full_range_tolerance_reproduces_gowers_similarity() {
            // The justification for the whole shape: at `tolerance == 1.0` — the
            // whole of an axis's [0, 1] range — `environment_fit` is exactly
            // `1 - d`, where `d` is Gower's distance over the shared axes as
            // Task 6 measured it (`domains/climate/tests/underworld.rs::distance`,
            // whose five hand-reproduced values a reviewer verified). BITWISE, not
            // approximately: the implementation accumulates dissimilarity and
            // subtracts once, so the two arithmetics are the same expression.
            //
            // READ THIS BEFORE TREATING A GREEN HERE AS AGREEMENT WITH TASK 6.
            // The reference below is a RE-IMPLEMENTATION, not the instrument.
            // `domains/species` may not depend on a sibling domain, so the real
            // `distance` cannot be imported and this is a copy — one that
            // already differs from the original on purpose (`==` against its
            // `to_bits()`, which disagree only on `-0.0`). A green here says
            // "the fit agrees with a faithful transcription", never "the fit
            // agrees with the shipped instrument".
            //
            // SUMMATION ORDER IS PART OF THE TRANSCRIPTION, and getting it
            // wrong is invisible when it happens to cancel. The original
            // iterates `environment_v1_basis()`; an earlier draft of this
            // helper iterated the AUTHORED slice order instead, and one of the
            // three cases below lists LIGHT (id 4) before SUBSTRATE (id 3). It
            // passed anyway, because those particular terms summed identically
            // — but float addition is not associative, so a case with two
            // non-zero out-of-order terms could have reddened without either
            // instrument being wrong. Fixed by iterating the basis exactly as
            // the original does. See `environment_fit`'s doc for why the
            // implementation's own ascending-id order agrees with basis order
            // at all: that is a second, separate dependency on The Axes'
            // append-only pin.
            fn gower(niche: &[(EnvironmentAxis, f64)], p: &EnvironmentVector) -> f64 {
                let mut total = 0.0;
                let mut shared = 0usize;
                for axis in environment_v1_basis() {
                    let Some((_, x)) = niche.iter().find(|(a, _)| a.id == axis.id) else {
                        continue;
                    };
                    let Some(y) = p.get(*axis) else { continue };
                    shared += 1;
                    total += match axis.valence {
                        AxisValence::Nominal => {
                            if *x == y {
                                0.0
                            } else {
                                1.0
                            }
                        }
                        _ => (x - y).abs(),
                    };
                }
                assert!(shared > 0, "the reference needs a shared axis");
                total / shared as f64
            }

            let cases: [&[(EnvironmentAxis, f64)]; 3] = [
                &[(PHYSIOGNOMY, 0.2), (ENERGY, 0.8), (SUBSTRATE, 0.6)],
                &[(ENERGY, 0.0), (WATER, 1.0), (LIGHT, 0.0), (SUBSTRATE, 0.0)],
                &[(PHYSIOGNOMY, 1.0), (WATER, 0.35)],
            ];
            let places = [
                place(&[
                    (PHYSIOGNOMY, 0.6),
                    (ENERGY, 0.1),
                    (WATER, 0.5),
                    (SUBSTRATE, 0.0),
                    (LIGHT, 1.0),
                ]),
                place(&[(ENERGY, 0.4), (WATER, 0.2), (SUBSTRATE, 0.0)]),
                place(&[(PHYSIOGNOMY, 0.0), (WATER, 0.35), (LIGHT, 0.5)]),
            ];

            let mut saw_a_difference = false;
            for spec in cases {
                let prefs: Vec<(EnvironmentAxis, AxisPreference)> = spec
                    .iter()
                    .map(|(a, v)| {
                        let p = if a.valence == AxisValence::Nominal {
                            AxisPreference::class(*v)
                        } else {
                            AxisPreference::graded(*v)
                        };
                        (*a, p)
                    })
                    .collect();
                let n = a_niche(&prefs);
                for p in &places {
                    let d = gower(spec, p);
                    let fit = environment_fit(&n, p);
                    assert_eq!(
                        fit.to_bits(),
                        (1.0 - d).to_bits(),
                        "fit {fit} is not 1 - Gower {d} for {spec:?}"
                    );
                    saw_a_difference |= d > 0.0;
                }
            }
            assert!(
                saw_a_difference,
                "every case scored a zero distance, so the comparison was vacuous"
            );
        }

        #[test]
        fn a_niche_rejects_a_preference_the_axis_cannot_carry() {
            // Every rejection below is a value that would produce a fit outside
            // [0, 1] or a division by zero if it were let through.
            for bad in [
                AxisPreference::Graded {
                    preferred: 1.5,
                    tolerance: 1.0,
                },
                AxisPreference::Graded {
                    preferred: -0.5,
                    tolerance: 1.0,
                },
                AxisPreference::Graded {
                    preferred: f64::NAN,
                    tolerance: 1.0,
                },
                AxisPreference::Graded {
                    preferred: 0.5,
                    tolerance: 0.0,
                },
                AxisPreference::Graded {
                    preferred: 0.5,
                    tolerance: 1.5,
                },
                AxisPreference::Graded {
                    preferred: 0.5,
                    tolerance: f64::INFINITY,
                },
            ] {
                assert!(
                    EnvironmentNiche::new(&[(ENERGY, bad)]).is_err(),
                    "ENERGY must reject {bad:?}"
                );
            }
            for bad in [
                AxisPreference::Class { accepted: -0.5 },
                AxisPreference::Class { accepted: 1.5 },
                AxisPreference::Class { accepted: f64::NAN },
            ] {
                assert!(
                    EnvironmentNiche::new(&[(SUBSTRATE, bad)]).is_err(),
                    "SUBSTRATE must reject {bad:?}"
                );
            }
            // The positive control: the valid forms are accepted.
            assert!(EnvironmentNiche::new(&[(ENERGY, AxisPreference::graded(0.5))]).is_ok());
            assert!(EnvironmentNiche::new(&[(SUBSTRATE, AxisPreference::class(0.5))]).is_ok());
            // An empty niche is legal — a kind that states nothing — and scores a
            // constant 0.0, so it never mis-ranks one place against another.
            let empty = EnvironmentNiche::new(&[]).expect("an empty niche is legal");
            assert!(empty.is_indifferent());
            assert_eq!(environment_fit(&empty, &place(&[(ENERGY, 0.5)])), 0.0);
            assert_eq!(environment_fit(&empty, &place(&[(ENERGY, 1.0)])), 0.0);
        }
    }
}
