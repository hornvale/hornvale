//! Species, tier 1: the authored body/mind/taxonomy component registries —
//! the universal biosphere component (mass, metabolic class, resource + climate
//! niche, potency), a closed six-dimension psychology vector, a closed
//! three-dimension perception vector, and each kind's family label. Kinds are
//! keyed by `KindId`; each component authors its own rows directly (the former
//! authored god-struct was dissolved in ECS c3). Species are data; the
//! social grammar stays code (spec §2). Goblin is the baseline: scalars 0.5,
//! default enum variants; every downstream modulation is the identity function
//! at this vector. The peopled speech data (articulation vector, lexicon,
//! family proto) is language-owned and lives in `hornvale-language`.
#![warn(missing_docs)]

use hornvale_kernel::{
    ANIMAL_PREY, Component, ComponentStore, ConceptDef, ConceptKind, ConceptRegistry,
    ConditionResponse, Correspondent, DETRITUS, EntityId, Fact, KindId, Ledger, LedgerError,
    MINERAL, Manifest, Mass, PHOTOSYNTHATE, PLANT_FORAGE, RegistryError, ResourceVector, Value,
    Void, World,
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
    /// Awake by day (the goblin baseline).
    Diurnal,
    /// Awake by night.
    Nocturnal,
    /// Awake at the boundaries (idle this campaign; authored now so a
    /// future species is a data change).
    Crepuscular,
}

/// The closed six-dimension psychology vector (spec §3). Scalars are bare
/// ratios in `[0, 1]` with 0.5 ≡ the goblin baseline; widening the vector
/// requires its own campaign.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PsychVector {
    /// How a society answers threat: flee 0 ↔ stand 1.
    pub threat_response: f64,
    /// How slowly decisions are made (idle this campaign; banked).
    pub deliberation_latency: f64,
    /// How wide "us" is drawn: insular 0 ↔ expansive 1.
    pub in_group_radius: f64,
    /// How far ahead works are planned: immediate 0 ↔ generational 1.
    pub time_horizon: f64,
    /// Authority shape.
    pub sociality: Sociality,
    /// What earns standing.
    pub status_basis: StatusBasis,
}

/// The closed three-dimension perception vector (spec §4). Scalars are bare
/// ratios in `[0, 1]` with 0.5 ≡ the goblin baseline; widening the vector
/// requires its own campaign. Every dimension is authored — nothing drawn.
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

/// A species' condition-tolerance profile: one response curve per v1
/// environmental axis. v1 fixes the four axes; a later campaign generalizes
/// to an open axis registry.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConditionNiche {
    /// Response curve over temperature, axis value in °C.
    pub temperature: ConditionResponse,
    /// Response curve over moisture, axis value in the climate moisture unit.
    pub moisture: ConditionResponse,
    /// Response curve over insolation, axis value in the annual-mean
    /// insolation unit.
    pub insolation: ConditionResponse,
    /// Response curve over elevation, axis value in the terrain elevation
    /// unit.
    pub elevation: ConditionResponse,
}

/// Kobold condition niche: cool HIGHLANDER — dark-adapted (consistent with
/// cool/polar), wide/indifferent on moisture, and staked to high elevation as
/// its exclusive, hard-excluding stronghold axis (Task B2b re-authoring: the
/// original B2 optima wanted cold+low-light cells that are also food-poor on
/// this world; elevation is a geographically independent axis the lowland
/// species can't contest). Authored within the measured seed-42 land ranges;
/// see the species chapter's model card for the ecological rationale.
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
        // HIGHLANDS — its exclusive niche
        elevation: ConditionResponse {
            optimum: 2600.0,
            width: 1200.0,
            devotion: 0.95,
        },
    }
}

/// Goblin condition niche: a warm-marginal GENERALIST with wide tolerance on
/// every axis (the cosmopolitan weed that fills margins/ecotones between the
/// three specialists). Authored within the measured seed-42 land ranges (Task
/// B2b re-authoring keeps this helper's shape, values retuned slightly to sit
/// alongside the corrected specialists); see the species chapter's model card
/// for the ecological rationale.
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
        elevation: ConditionResponse {
            optimum: 500.0,
            width: 3000.0,
            devotion: 0.35,
        },
    }
}

/// Hobgoblin condition niche: temperate, DRIER, mid-elevation open plains —
/// moisture and elevation separate it from bugbear's wet lowlands and
/// kobold's highlands (Task B2b re-authoring). Authored within the measured
/// seed-42 land ranges; see the species chapter's model card for the
/// ecological rationale.
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
        // low-mid
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
/// framing did (Task B2b re-authoring). Authored within the measured seed-42
/// land ranges; see the species chapter's model card for the ecological
/// rationale.
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
        // lowland
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
/// shape), cool mountain temperature rather than arctic cold. Authored
/// within the measured seed-42 land ranges.
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
        // HIGH MOUNTAIN — its exclusive niche, near the measured ceiling.
        elevation: ConditionResponse {
            optimum: 3000.0,
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
    /// Phototroph (plant-folk/fungal analogue). Energy from light; its basal
    /// rate is SURFACE/area-limited, so the §4 universal exponent does NOT
    /// apply — activating this class is its own modelling decision. Unused seam.
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
}

// The biosphere / psyche / perception / family authoring lives in the four
// component registries below (`biosphere_registry` / `psyche_registry` /
// `perception_registry` / `family_of`). The former authored god-struct and
// its `registry()` are gone (ECS c3): kinds are keyed by `KindId`, and each
// component authors its own rows directly. The peopled speech data
// (articulation, lexicon, family proto) lives in `hornvale_language`.

impl Component for BiosphereTraits {}
impl Component for PsychVector {}
impl Component for PerceptionVector {}

/// The universal biosphere component, authored directly (one row per kind).
/// Every kind that competes for space has a biosphere row; this is the
/// canonical entity set. Mass is D&D 5E canon (kg); niche is a sparse
/// utilization profile over the resource-axis basis; each kind's climate-tile
/// rationale lives in its `*_condition_niche` helper above. Potency is the
/// creature's 5E adult Challenge Rating over 30 (`CR/30`), nonzero only for the
/// supernatural set (dragons, treant, xorn); mundane beasts and the four
/// peoples carry 0.
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
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// The peopled psychology component — authored directly, present only for the
/// four settling, speaking peoples (goblin is the baseline: scalars 0.5,
/// default enum variants).
/// type-audit: bare-ok(identifier-text)
pub fn psyche_registry() -> ComponentStore<KindId, PsychVector> {
    [
        (
            KindId("goblin"),
            PsychVector {
                threat_response: 0.5,
                deliberation_latency: 0.5,
                in_group_radius: 0.5,
                time_horizon: 0.5,
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
            },
        ),
        (
            KindId("kobold"),
            PsychVector {
                threat_response: 0.8,
                deliberation_latency: 0.7,
                in_group_radius: 0.2,
                time_horizon: 0.8,
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Knowledge,
            },
        ),
        (
            KindId("hobgoblin"),
            PsychVector {
                threat_response: 0.7,
                deliberation_latency: 0.6,
                in_group_radius: 0.3,
                time_horizon: 0.5,
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
            },
        ),
        (
            KindId("bugbear"),
            PsychVector {
                threat_response: 0.8,
                deliberation_latency: 0.4,
                in_group_radius: 0.3,
                time_horizon: 0.3,
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Rank,
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// The peopled perception component — authored directly, present only for the
/// four peoples (goblin is the baseline: diurnal, 0.5/0.5).
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

    for (name, doc) in [
        ("goblin-kind", "a goblin"),
        ("kobold-kind", "a kobold"),
        ("hobgoblin-kind", "a hobgoblin"),
        ("bugbear-kind", "a bugbear"),
    ] {
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

        assert_eq!(bio.len(), 16, "sixteen kinds compete for space");
        let bio_ids: Vec<_> = bio.ids().collect();
        let fam_ids: Vec<_> = fam.ids().collect();
        assert_eq!(bio_ids, fam_ids, "family covers exactly the biosphere set");

        let psy_ids: Vec<_> = psy.ids().collect();
        let per_ids: Vec<_> = per.ids().collect();
        assert_eq!(psy_ids, per_ids, "psyche and perception share one key-set");
        assert_eq!(psy.len(), 4, "the four peoples");
        for kind in psy.ids() {
            assert!(bio.contains(kind), "people {kind:?} has a biosphere row");
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
        ] {
            let c = r
                .concept(name)
                .unwrap_or_else(|| panic!("missing concept {name}"));
            assert_eq!(c.domain, "species");
            assert_eq!(c.kind, ConceptKind::Living);
        }
    }

    #[test]
    fn goblin_is_the_baseline_vector() {
        let psy = psyche_registry();
        let g = psy.get(&KindId("goblin")).unwrap();
        for v in [
            g.threat_response,
            g.deliberation_latency,
            g.in_group_radius,
            g.time_horizon,
        ] {
            assert_eq!(v, 0.5, "goblin scalars must sit exactly at baseline");
        }
        assert_eq!(g.sociality, Sociality::Hierarchic);
        assert_eq!(g.status_basis, StatusBasis::Rank);
    }

    #[test]
    fn registry_is_ordered_alphabetically_and_kobold_contrasts() {
        let bio = biosphere_registry();
        let names: Vec<&str> = bio.ids().map(|k| k.0).collect();
        // The roster grew with the Task 4 menagerie (12 biosphere-only fauna
        // alongside the four peoples); ComponentStore key order is lexicographic.
        assert_eq!(
            names,
            vec![
                "black-dragon",
                "bugbear",
                "giant-elk",
                "giant-goat",
                "goblin",
                "hobgoblin",
                "kobold",
                "otyugh",
                "owlbear",
                "red-dragon",
                "rust-monster",
                "treant",
                "twig-blight",
                "white-dragon",
                "woolly-mammoth",
                "xorn",
            ]
        );
        let psy = psyche_registry();
        let k = psy.get(&KindId("kobold")).unwrap();
        assert_eq!(k.sociality, Sociality::Communal);
        assert_eq!(k.status_basis, StatusBasis::Knowledge);
        assert!(k.in_group_radius < 0.5 && k.time_horizon > 0.5 && k.threat_response > 0.5);
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

    #[test]
    fn goblin_perception_is_the_baseline_and_kobold_contrasts() {
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
    fn the_four_peoples_have_distinct_temperature_optima() {
        let bio = biosphere_registry();
        let opts: Vec<f64> = ["kobold", "goblin", "hobgoblin", "bugbear"]
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
            assert!(!psy.contains(&KindId(name)), "{name} is fauna: no psyche");
            assert!(
                !per.contains(&KindId(name)),
                "{name} is fauna: no perception"
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
}
