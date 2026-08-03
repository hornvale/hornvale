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

use hornvale_kernel::{
    ANIMAL_PREY, Component, ComponentStore, ConceptDef, ConceptKind, ConceptRegistry,
    ConditionResponse, Correspondent, DETRITUS, EntityId, Fact, KindId, Ledger, LedgerError,
    MARINE_FORAGE, MINERAL, Manifest, Mass, PHOTOSYNTHATE, PLANT_FORAGE, RegistryError,
    ResourceVector, Value, Void, World,
};

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
/// `substrate_field` computes as `elevation_at(cell) − sea_level`. It was
/// previously the raw `hornvale_kernel::ReferenceElevation`, whose datum is
/// isostatic (0 m = a reference-thickness crust at equilibrium) and whose
/// sea level is a *drawn* value differing by ~1.8 km between worlds — so an
/// authored optimum meant a different altitude on every seed, and the
/// kobold's 2600 sat ≈ 5200–5900 m above a typical world's sea level, at or
/// above its highest land. The optima below are authored against the
/// corrected frame, on named percentiles of the measured distribution of
/// **settleable land** (land above sea level with non-zero carrying
/// capacity), pooled over seeds 1..=30, n = 142 595 cells:
///
/// | percentile | p15 | p25 | p35 | p50 | p65 | p75 | p85 | p95 |
/// |---|---:|---:|---:|---:|---:|---:|---:|---:|
/// | metres above sea level | 142 | 621 | 1004 | 1561 | 2166 | 2651 | 3251 | 4148 |
///
/// (All land, ignoring capacity, runs higher — median 2188 m, and a world's
/// single highest land cell has a median of ≈ 6970 m.)
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
/// original B2 optima wanted cold+low-light cells that are also food-poor on
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
/// the best-fit people on every settleable cell above 3000 m (mean fit 0.130
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
fn bugbear_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 21.0,
            width: 11.0,
            devotion: 0.85,
        },
        // WETTEST cells — its stronghold
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
        // WETTEST cells — its stronghold, near the measured ceiling.
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
/// buys most of its sovereignty floor. Authored within the measured seed-42
/// land ranges.
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
        insolation: ConditionResponse {
            optimum: 0.05,
            width: 0.20,
            devotion: 0.20,
        },
        elevation: ConditionResponse {
            optimum: 0.0,
            width: 3500.0,
            devotion: 0.10,
        },
    }
}

/// Rust monster condition niche: subterranean/cave mineral-eater — no
/// potency, so unlike the xorn it is genuinely environment-placed, with a
/// strong low-insolation (cave-dark) preference and a low-elevation lean.
/// Authored within the measured seed-42 land ranges.
fn rust_monster_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 8.0,
            width: 20.0,
            devotion: 0.50,
        },
        moisture: ConditionResponse {
            optimum: 0.45,
            width: 0.40,
            devotion: 0.30,
        },
        // avoids surface light — cave-dark preference.
        insolation: ConditionResponse {
            optimum: 0.03,
            width: 0.06,
            devotion: 0.60,
        },
        elevation: ConditionResponse {
            optimum: -500.0,
            width: 1500.0,
            devotion: 0.60,
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
        // wettest cells — its stronghold, like the otyugh's swamp.
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
// off `niche_per_species_k` (worldgen) shaped every niche below: (1) the
// `ANIMAL_PREY`/`PLANT_FORAGE` supply terms both derive from
// `forage_supply_field`, itself a fraction of the NPP-based `base_carrying`
// field, which collapses toward 0 wherever `carrying_capacity`'s aridity term
// pushes hostility high (desert-band cells, moisture < 0.2) — this is why
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
/// scale for the shared cell.
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
/// (savanna) — the hot-arid/savanna cell. A pure `PLANT_FORAGE` grazer inherits
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
        // moist forest litter, below the otyugh's wettest-cell stake.
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

/// Shrieker condition niche: `Sessile × DETRITUS` — a genuinely new cell
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
// is `elevation_at(cell) - sea_level`
// ([`ConditionNiche`]'s struct doc), so a marine optimum is NEGATIVE — its
// magnitude is depth. The percentiles cited per kind below come from a
// throwaway probe (deleted before commit, not part of the suite) that
// measured `substrate_field`'s elevation reading over every OCEAN cell across
// seeds 1..=30 — the same sweep `occupancy_readout.rs` uses — bucketed by the
// cell's `Biome`:
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
// `Abyssal` is vanishingly rare (4 cells total across the whole 30-seed sweep,
// right at its 4000 m floor) and `HadalTrench` never occurred at all, so a
// kind "for" the abyssal is honestly a bathypelagic kind whose tail can reach
// the boundary, not a kind with a real abyssal stronghold to measure against.
//
// `marine_forage_supply_field` (worldgen) keys `MARINE_FORAGE` productivity
// directly to the cell's biome class (coral-reef/kelp-forest 0.85, epipelagic
// 0.45, mesopelagic 0.15, bathypelagic 0.05, abyssal/hadal-trench 0.02,
// upwelling 1.0) rather than to a continuous NPP field the way the land's
// `PHOTOSYNTHATE`/`PLANT_FORAGE` supply is — so, unlike The Vacancy T7's
// land kinds (whose `mean_k` ranking was dominated by NPP magnitude,
// independent of the kind's own target biome — BIO-supply-drowns-niche), a marine kind's own
// elevation+temperature optimum is what SELECTS its supply tier, because it
// selects which biome class the cell classifies as in the first place.
// Measured per-kind below; `upwelling`'s productivity (1.0) is the one
// remaining confound, since it can outrank a shelf/deep-water kind's own
// target biome on cells the kind's wide condition tolerance also reaches.
//
// Temperature at every cell (including ocean) is `climate.mean_temperature_at`,
// a pure function of latitude and elevation-above-sea-level lapse (elevation
// below sea level applies NO lapse term) — i.e. sea-surface temperature only,
// uncorrelated with depth (`domains/climate/src/temperature.rs`). Insolation
// is likewise a pure function of latitude/obliquity (Finding 2, The Vacancy
// T7 report) — also uncorrelated with depth. Neither axis can therefore
// distinguish "sunlit shallows" from "aphotic deep water" the way real ocean
// physics would; a deep-water kind's low insolation/cool temperature
// optimum below is a thematic placement, not a claim the model enforces
// depth-linked light or cold. Moisture at every ocean cell is the banded
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
/// territory in this model (4 cells total across the 30-seed probe sweep,
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
/// shelf cells (where its `MARINE_FORAGE` weight draws supply) — the same
/// single condition-niche curve read against whichever supply field is
/// nonzero at that cell, no special case anywhere. Huge beast, Challenge 5
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

/// A species' metabolic strategy. Selects the allometric normalization
/// coefficient (B₀) and the per-class pace multiplier; the scaling
/// *exponents* are universal across classes (spec §4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MetabolicClass {
    /// Warm-blooded (mammal/bird analogue): high, temperature-stable basal rate.
    Endotherm,
    /// Cold-blooded (reptile/amphibian analogue): ~1/8 the basal rate; longer
    /// life per kg. Realized rate couples to ambient temperature (deferred,
    /// spec §10 CAP-1).
    Ectotherm,
    /// Phototroph (plant-folk/fungal analogue). Energy from light.
    ///
    /// **Documented intent, not shipped behaviour.** A phototroph's basal rate
    /// is physically SURFACE/area-limited, so §4's universal ¾ mass exponent
    /// should not apply to it. It nonetheless does: [`crate::allometry`] gives
    /// this class `B0_ENDOTHERM` and a pace multiplier of 1.0, so the two
    /// shipped autotrophs (treant, twig-blight) are computed exactly as
    /// endotherms of the same mass. The class was witnessed by The Menagerie
    /// without the modelling decision ever being made, and this doc claimed
    /// "unused seam" for three campaigns after it stopped being one.
    ///
    /// Making it real needs an area-scaling exponent and an autotroph `B0`
    /// calibrated against a photosynthetic-productivity anchor — a genuine
    /// modelling call that moves both kinds' life-history and every golden
    /// they touch, tracked as BIO-autotroph-physics and deliberately NOT bundled with the
    /// roster expansion that would destroy its attribution. The current
    /// divergence is pinned by `autotroph_is_computed_as_an_endotherm_today`
    /// in `tests/coverage.rs`, so the fix will present as a visible diff.
    Autotroph,
    /// No metabolism (construct/undead analogue). Has no life-history: the
    /// biological traits are `None`. Unused seam.
    Ametabolic,
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
    /// Metabolic strategy — drives life-history allometry (spec BIO-2).
    pub metabolic_class: MetabolicClass,
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
    /// mundane beasts and the four
    /// peoples carry 0.
    /// type-audit: bare-ok(ratio: potency)
    pub potency: f64,
    /// How this creature organizes socially (universal; every kind carries
    /// one). `Settled` is the sole settlement-forming value and the successor
    /// to the old "has a psyche entry" proxy for peoplehood. (An enum, not a
    /// bare primitive — no type-audit verdict needed.)
    pub social_form: SocialForm,
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

/// The universal biosphere component, authored directly (one row per kind).
/// Every kind that competes for space has a biosphere row; this is the
/// canonical entity set. Mass is D&D 5E canon (kg); niche is a sparse
/// utilization profile over the resource-axis basis; each kind's climate-tile
/// rationale lives in its `*_condition_niche` helper above. Potency is the
/// creature's 5E adult Challenge Rating over 30 (`CR/30`), nonzero only for the
/// supernatural set (dragons, treant, xorn); mundane beasts and the four
/// peoples carry 0. `social_form` is the universal social-organization axis
/// (spec §3.1, The Eremite): `Settled` for the four peoples, `Sessile` for
/// the rooted autotrophs, `Gregarious` for the herding beasts, `Solitary`
/// for everything else (including the three dragons).
/// type-audit: bare-ok(identifier-text)
pub fn biosphere_registry() -> ComponentStore<KindId, BiosphereTraits> {
    [
        (
            KindId("goblin"),
            BiosphereTraits {
                mass: Mass::new(18.1).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.50), (ANIMAL_PREY, 0.50)]).unwrap(),
                condition_niche: goblin_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
            },
        ),
        (
            KindId("kobold"),
            BiosphereTraits {
                mass: Mass::new(13.6).unwrap(),
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.55), (ANIMAL_PREY, 0.45)]).unwrap(),
                condition_niche: kobold_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
            },
        ),
        (
            KindId("hobgoblin"),
            BiosphereTraits {
                mass: Mass::new(74.8).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.65), (ANIMAL_PREY, 0.35)]).unwrap(),
                condition_niche: hobgoblin_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
            },
        ),
        (
            KindId("bugbear"),
            BiosphereTraits {
                mass: Mass::new(132.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.15), (ANIMAL_PREY, 0.85)]).unwrap(),
                condition_niche: bugbear_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
            },
        ),
        (
            KindId("treant"),
            BiosphereTraits {
                mass: Mass::new(1800.0).unwrap(),
                metabolic_class: MetabolicClass::Autotroph,
                niche: ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap(),
                condition_niche: treant_condition_niche(),
                potency: 9.0 / 30.0, // treant — CR 9 (5E MM); potency = CR/30
                social_form: SocialForm::Sessile,
            },
        ),
        (
            KindId("twig-blight"),
            BiosphereTraits {
                mass: Mass::new(5.0).unwrap(),
                metabolic_class: MetabolicClass::Autotroph,
                niche: ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap(),
                condition_niche: twig_blight_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Sessile,
            },
        ),
        (
            KindId("giant-elk"),
            BiosphereTraits {
                mass: Mass::new(450.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_elk_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Gregarious,
            },
        ),
        (
            KindId("woolly-mammoth"),
            BiosphereTraits {
                mass: Mass::new(6000.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: woolly_mammoth_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Gregarious,
            },
        ),
        (
            KindId("giant-goat"),
            BiosphereTraits {
                mass: Mass::new(140.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_goat_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Gregarious,
            },
        ),
        (
            KindId("otyugh"),
            BiosphereTraits {
                mass: Mass::new(260.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap(),
                condition_niche: otyugh_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("xorn"),
            BiosphereTraits {
                mass: Mass::new(55.0).unwrap(),
                metabolic_class: MetabolicClass::Ametabolic,
                niche: ResourceVector::new(&[(MINERAL, 1.0)]).unwrap(),
                condition_niche: xorn_condition_niche(),
                potency: 5.0 / 30.0, // xorn — CR 5 (5E MM); potency = CR/30
                social_form: SocialForm::Solitary,
                // Ametabolic, burrows through stone: lives IN the substrate,
                // not on it. rust-monster shares the pure-MINERAL niche but
                // stays Terrestrial — it walks the surface eating metal.
            },
        ),
        (
            KindId("rust-monster"),
            BiosphereTraits {
                mass: Mass::new(90.0).unwrap(),
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(MINERAL, 1.0)]).unwrap(),
                condition_niche: rust_monster_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("white-dragon"),
            BiosphereTraits {
                mass: Mass::new(2200.0).unwrap(), // 5E adult white dragon
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(), // obligate apex
                condition_niche: white_dragon_condition_niche(),
                potency: 13.0 / 30.0, // adult white dragon — CR 13 (5E MM); potency = CR/30
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("red-dragon"),
            BiosphereTraits {
                mass: Mass::new(2700.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: red_dragon_condition_niche(),
                potency: 17.0 / 30.0, // adult red dragon — CR 17 (5E MM); potency = CR/30
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("black-dragon"),
            BiosphereTraits {
                mass: Mass::new(2200.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: black_dragon_condition_niche(),
                potency: 14.0 / 30.0, // adult black dragon — CR 14 (5E MM); potency = CR/30
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("owlbear"),
            BiosphereTraits {
                mass: Mass::new(450.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: owlbear_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Solitary,
            },
        ),
        // The Vacancy (T7): seven terrestrial fauna. See the block comment
        // above `giant_scorpion_condition_niche` for the shared design notes
        // (the NPP/desert supply trap and the elevation percentile table).
        (
            KindId("giant-scorpion"),
            BiosphereTraits {
                mass: Mass::new(300.0).unwrap(),
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 0.3), (DETRITUS, 0.7)]).unwrap(),
                condition_niche: giant_scorpion_condition_niche(),
                potency: 0.0, // giant scorpion — CR 3 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("giant-hyena"),
            BiosphereTraits {
                mass: Mass::new(160.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: giant_hyena_condition_niche(),
                potency: 0.0, // giant hyena — CR 1 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Gregarious,
            },
        ),
        (
            KindId("dire-wolf"),
            BiosphereTraits {
                mass: Mass::new(150.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: dire_wolf_condition_niche(),
                potency: 0.0, // dire wolf — CR 1 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Gregarious,
            },
        ),
        (
            KindId("rhinoceros"),
            BiosphereTraits {
                mass: Mass::new(2300.0).unwrap(), // real white rhinoceros adult male average
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: rhinoceros_condition_niche(),
                potency: 0.0, // rhinoceros — CR 2 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("giant-constrictor-snake"),
            BiosphereTraits {
                mass: Mass::new(500.0).unwrap(),
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: giant_constrictor_snake_condition_niche(),
                potency: 0.0, // giant constrictor snake — CR 2 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("carrion-crawler"),
            BiosphereTraits {
                mass: Mass::new(200.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap(),
                condition_niche: carrion_crawler_condition_niche(),
                potency: 0.0, // carrion crawler — CR 2 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("shrieker"),
            BiosphereTraits {
                mass: Mass::new(35.0).unwrap(),
                metabolic_class: MetabolicClass::Autotroph,
                niche: ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap(),
                condition_niche: shrieker_condition_niche(),
                potency: 0.0, // shrieker — CR 0 (5E MM); CR/30 = 0 regardless of set
                social_form: SocialForm::Sessile,
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
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(MARINE_FORAGE, 1.0)]).unwrap(),
                condition_niche: reef_shark_condition_niche(),
                potency: 0.0, // reef shark — CR 1/2 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("giant-octopus"),
            BiosphereTraits {
                mass: Mass::new(180.0).unwrap(),
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(MARINE_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_octopus_condition_niche(),
                potency: 0.0, // giant octopus — CR 1 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("killer-whale"),
            BiosphereTraits {
                mass: Mass::new(5400.0).unwrap(), // real adult male average (upper of range)
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(MARINE_FORAGE, 1.0)]).unwrap(),
                condition_niche: killer_whale_condition_niche(),
                potency: 0.0, // killer whale — CR 3 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Gregarious,
            },
        ),
        (
            KindId("giant-squid"),
            BiosphereTraits {
                mass: Mass::new(250.0).unwrap(), // real Architeuthis dux, large-adult estimate
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(MARINE_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_squid_condition_niche(),
                potency: 0.0, // giant squid — CR 7 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
            },
        ),
        (
            KindId("giant-crocodile"),
            BiosphereTraits {
                mass: Mass::new(1000.0).unwrap(),
                metabolic_class: MetabolicClass::Ectotherm,
                // the amphibious proof case: MARINE_FORAGE (sea) plus
                // ANIMAL_PREY (land) — no special case, see the condition
                // niche's doc comment.
                niche: ResourceVector::new(&[(MARINE_FORAGE, 0.4), (ANIMAL_PREY, 0.6)]).unwrap(),
                condition_niche: giant_crocodile_condition_niche(),
                potency: 0.0, // giant crocodile — CR 5 (5E MM); mundane, potency stays 0
                social_form: SocialForm::Solitary,
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
                metabolic_class: MetabolicClass::Endotherm,
                // mixed omnivore weighted toward ANIMAL_PREY — a pack
                // hunter that also forages, not a pure predator (contrast
                // bugbear's 0.85 ANIMAL_PREY lean).
                niche: ResourceVector::new(&[(ANIMAL_PREY, 0.65), (PLANT_FORAGE, 0.35)]).unwrap(),
                condition_niche: gnoll_condition_niche(),
                potency: 0.0, // gnoll — CR 1/2 (5E MM); mundane like the other four peoples
                social_form: SocialForm::Settled,
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// The individual-mind component — authored directly, present for every
/// minded kind (the four settling peoples and the three solitary dragons).
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
    ]
    .into_iter()
    .collect()
}

/// The community-mind component — authored directly, present only for the
/// four settling peoples. A Solitary minded kind (a dragon) carries a
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
    ]
    .into_iter()
    .collect()
}

/// The perception component — authored directly, present for every minded
/// SPEAKING kind: the four peoples and the three chromatic dragons (The
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
        day: Some(0.0),
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

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Fact, Seed};

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
        // exactly one key-set — the four peoples — every one of which also
        // carries a biosphere row.
        let bio = biosphere_registry();
        let fam = family_of();
        let psy = psyche_registry();
        let per = perception_registry();

        assert_eq!(
            bio.len(),
            29,
            "twenty-nine kinds compete for space (The Vacancy T7 added seven, T8 added five, T9 added the gnoll)"
        );
        let bio_ids: Vec<_> = bio.ids().collect();
        let fam_ids: Vec<_> = fam.ids().collect();
        assert_eq!(bio_ids, fam_ids, "family covers exactly the biosphere set");

        // Capacities nest (The Eremite, tightened by The Vigil): perception ⊆
        // psyche, and since The Vigil every minded SPEAKER also perceives, so
        // the two stores again share one key-set — seven kinds, not the four
        // peoples.
        for kind in per.ids() {
            assert!(
                psy.contains(kind),
                "perceiver {kind:?} carries a mind (perception ⊆ psyche)"
            );
        }
        assert_eq!(psy.len(), 8, "five peoples + three minded dragons");
        assert_eq!(
            per.len(),
            8,
            "perception is the five peoples + the three dragons (The Vigil)"
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
        // amphibious giant crocodile), and T9 (the gnoll, the fifth people);
        // ComponentStore key order is lexicographic.
        assert_eq!(
            names,
            vec![
                "black-dragon",
                "bugbear",
                "carrion-crawler",
                "dire-wolf",
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
                "hobgoblin",
                "killer-whale",
                "kobold",
                "otyugh",
                "owlbear",
                "red-dragon",
                "reef-shark",
                "rhinoceros",
                "rust-monster",
                "shrieker",
                "treant",
                "twig-blight",
                "white-dragon",
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
        let settlement = w.ledger.mint_entity();
        people(&mut w, settlement, "kobold").unwrap();
        assert_eq!(species_of(&w, settlement).as_deref(), Some("kobold"));

        // `species_entity` resolves a committed SPECIES_NAME fact back to its
        // entity (the fact worldgen's genesis commits at world build).
        let kobold = w.ledger.mint_entity();
        w.ledger
            .commit(
                Fact {
                    subject: kobold,
                    predicate: SPECIES_NAME.to_string(),
                    object: Value::Text("kobold".to_string()),
                    place: None,
                    day: Some(0.0),
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
        use MetabolicClass::*;
        let bio = biosphere_registry();
        let mc = |n: &'static str| bio.get(&KindId(n)).unwrap().metabolic_class;
        assert_eq!(mc("goblin"), Endotherm);
        assert_eq!(mc("hobgoblin"), Endotherm);
        assert_eq!(mc("bugbear"), Endotherm);
        assert_eq!(mc("kobold"), Ectotherm); // reptilian/draconic SRD lineage
    }

    #[test]
    fn split_preserves_biosphere_and_peopled_presence() {
        let bio = biosphere_registry();
        let psy = psyche_registry();
        // biosphere authored intact
        let goblin = bio.get(&KindId("goblin")).unwrap();
        assert_eq!(goblin.mass, Mass::new(18.1).unwrap());
        assert_eq!(goblin.potency, 0.0);
        // the four peoples all speak/settle (carry a psyche row)
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
            vec!["bugbear", "gnoll", "goblin", "hobgoblin", "kobold"]
        );
        // dragons are minded (psyche) but not Settled — no society vector
        assert!(society_registry().get(&KindId("red-dragon")).is_none());
        assert!(psyche_registry().get(&KindId("red-dragon")).is_some());
    }
}
