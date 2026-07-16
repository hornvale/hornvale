//! Species, tier 1: authored species definitions — a closed six-dimension
//! psychology vector, a closed three-dimension perception vector, a closed
//! seven-dimension articulation vector, and vocabulary stopgaps. Species are
//! data; the social grammar stays code (spec §2). Goblin is the baseline:
//! scalars 0.5, default enum variants; every downstream modulation is the
//! identity function at this vector. Placeholder name syllables (the
//! pre-Tongues stopgap) are retired — names are generated from the
//! articulation vector by `hornvale-language`, wired at the composition
//! root (spec §7).
#![warn(missing_docs)]

use std::collections::BTreeMap;

use hornvale_kernel::{
    ANIMAL_PREY, ConceptKind, ConceptRegistry, ConditionResponse, DETRITUS, EntityId, Fact, KindId,
    LedgerError, MINERAL, Mass, PHOTOSYNTHATE, PLANT_FORAGE, RegistryError, ResourceVector, Value,
    World,
};

mod allometry;
pub use allometry::{
    LifeHistory, age_at_maturity, basal_metabolic_rate_w, life_history, lifespan,
    reproductive_tempo,
};

/// Predicate: a species entity's name (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_NAME: &str = "species-name";
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

/// An exotic manner of articulation found in a species' phonology.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExoticManner {
    /// No exotic manner (the goblin baseline).
    None,
    /// Trill: rapid vibration of an articulator.
    Trill,
    /// Click: sharp ingressive oral sound.
    Click,
    /// Ejective: sharp egressive sound made with trapped air.
    Ejective,
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

/// The closed seven-dimension articulation vector (spec §5, extended by the
/// phonology epoch with `tonality`). Scalars are bare ratios in `[0, 1]` with
/// 0.5 ≡ the goblin baseline (tonality 0.0 ≡ atonal, the humanoid default);
/// widening the vector requires its own campaign. Every dimension is
/// authored — nothing drawn.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ArticulationVector {
    /// Lip-rounding and jaw-closure degree: unrounded 0 ↔ rounded 1.
    pub labiality: f64,
    /// Vowel-space size: compressed 0 ↔ expanded 1.
    pub vowel_space: f64,
    /// Voicing emphasis: voiceless 0 ↔ voiced 1.
    pub voicing: f64,
    /// Sibilance emphasis: minimal 0 ↔ pronounced 1.
    pub sibilance: f64,
    /// Voice-loudness range: quiet 0 ↔ loud 1.
    pub voice_loudness: f64,
    /// Tonal propensity, authored from body plan: atonal 0 (humanoid default)
    /// ↔ fully tonal 1. Maps to a tone-inventory size in `draw_phonology`
    /// (1 = atonal Neutral-only, 2–3 tone-capable) and makes tonogenesis
    /// effective. The shipped humanoids stay 0.0; the value earns its keep as
    /// the bestiary grows (serpentine, avian).
    pub tonality: f64,
    /// Exotic manner of articulation.
    pub exotic: ExoticManner,
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
    /// creatures buffer environmental constraint. The four material peoples
    /// carry 0.
    /// type-audit: bare-ok(ratio: potency)
    pub potency: f64,
}

/// The peopled component: present only for settling, speaking peoples.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct PeopledTraits {
    /// The settlement noun ("village", "warren").
    pub noun: &'static str,
    /// The psychology vector.
    pub psych: PsychVector,
    /// The perception vector.
    pub perception: PerceptionVector,
    /// The articulation vector.
    pub articulation: ArticulationVector,
    /// Worker-role override; `None` = the subsistence worker word.
    pub worker_override: Option<&'static str>,
    /// The warrior-rung word.
    pub warrior: &'static str,
    /// The artisan-rung word.
    pub artisan: &'static str,
    /// The shaman-rung word.
    pub shaman: &'static str,
    /// The top-rung word ("chief", "elders").
    pub top: &'static str,
}

/// One authored species: vector, vocabulary stopgaps (deleted by The
/// Tongues), and a placeholder syllable pool for names.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct SpeciesDef {
    /// The species name ("goblin", "kobold").
    pub name: &'static str,
    /// The family this species descends from ("goblinoid", "kobold"); a
    /// singleton family's name equals its lone member's name. Looked up in
    /// [`family_registry`] for the family's proto ancestral vector.
    pub family: &'static str,
    /// The universal biosphere component: every entity has one.
    pub biosphere: BiosphereTraits,
    /// The peopled component: present only for settling, speaking peoples.
    pub peopled: Option<PeopledTraits>,
}

/// The authored species registry, ordered alphabetically by name (`BTreeMap`
/// key order). Kobold, hobgoblin, and bugbear values are derived from D&D
/// 5E SRD lore — see the species chapter's model card for each derivation.
/// type-audit: bare-ok(identifier-text)
pub fn registry() -> BTreeMap<KindId, SpeciesDef> {
    let mut reg = BTreeMap::new();
    // Mass: D&D 5E average weights (2014 Volo's Guide / PHB tables), lb -> kg:
    //   kobold 30 lb = 13.6 kg, goblin 40 lb = 18.1 kg,
    //   hobgoblin 165 lb = 74.8 kg, bugbear 291 lb = 132.0 kg.
    // Niche: 5E ecology — bugbear a carnivore (hunts, eats humanoids), hobgoblin
    //   the most agricultural (cultivates + hunts), kobold/goblin opportunistic
    //   omnivore-scavengers. All omnivores here (both axes > 0); the true
    //   scavenger/autotroph/apex arrive with the Stage-B menagerie.
    reg.insert(
        KindId("goblin"),
        SpeciesDef {
            name: "goblin",
            family: "goblinoid",
            biosphere: BiosphereTraits {
                mass: Mass::new(18.1).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.50), (ANIMAL_PREY, 0.50)]).unwrap(),
                condition_niche: goblin_condition_niche(),
                potency: 0.0,
            },
            peopled: Some(PeopledTraits {
                noun: "village",
                psych: PsychVector {
                    threat_response: 0.5,
                    deliberation_latency: 0.5,
                    in_group_radius: 0.5,
                    time_horizon: 0.5,
                    sociality: Sociality::Hierarchic,
                    status_basis: StatusBasis::Rank,
                },
                perception: PerceptionVector {
                    activity: ActivityCycle::Diurnal,
                    night_vision: 0.5,
                    sky_attention: 0.5,
                },
                articulation: ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.5,
                    voicing: 0.5,
                    sibilance: 0.5,
                    voice_loudness: 0.5,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
                worker_override: None,
                warrior: "warrior",
                artisan: "artisan",
                shaman: "shaman",
                top: "chief",
            }),
        },
    );
    reg.insert(
        KindId("kobold"),
        SpeciesDef {
            name: "kobold",
            family: "kobold",
            biosphere: BiosphereTraits {
                mass: Mass::new(13.6).unwrap(),
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.55), (ANIMAL_PREY, 0.45)]).unwrap(),
                condition_niche: kobold_condition_niche(),
                potency: 0.0,
            },
            peopled: Some(PeopledTraits {
                noun: "warren",
                psych: PsychVector {
                    threat_response: 0.8,
                    deliberation_latency: 0.7,
                    in_group_radius: 0.2,
                    time_horizon: 0.8,
                    sociality: Sociality::Communal,
                    status_basis: StatusBasis::Knowledge,
                },
                perception: PerceptionVector {
                    activity: ActivityCycle::Nocturnal,
                    night_vision: 0.9,
                    sky_attention: 0.8,
                },
                articulation: ArticulationVector {
                    labiality: 0.1,
                    vowel_space: 0.3,
                    voicing: 0.6,
                    sibilance: 0.9,
                    voice_loudness: 0.2,
                    tonality: 0.0,
                    exotic: ExoticManner::Trill,
                },
                worker_override: Some("digger"),
                warrior: "warden",
                artisan: "shaper",
                shaman: "keeper",
                top: "elders",
            }),
        },
    );
    reg.insert(
        KindId("hobgoblin"),
        SpeciesDef {
            name: "hobgoblin",
            family: "goblinoid",
            biosphere: BiosphereTraits {
                mass: Mass::new(74.8).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.65), (ANIMAL_PREY, 0.35)]).unwrap(),
                condition_niche: hobgoblin_condition_niche(),
                potency: 0.0,
            },
            peopled: Some(PeopledTraits {
                noun: "legion",
                psych: PsychVector {
                    threat_response: 0.7,
                    deliberation_latency: 0.6,
                    in_group_radius: 0.3,
                    time_horizon: 0.5,
                    sociality: Sociality::Hierarchic,
                    status_basis: StatusBasis::Rank,
                },
                perception: PerceptionVector {
                    activity: ActivityCycle::Diurnal,
                    night_vision: 0.6,
                    sky_attention: 0.5,
                },
                articulation: ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.5,
                    voicing: 0.6,
                    sibilance: 0.4,
                    voice_loudness: 0.8,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
                worker_override: Some("laborer"),
                warrior: "soldier",
                artisan: "smith",
                shaman: "augur",
                top: "warlord",
            }),
        },
    );
    reg.insert(
        KindId("bugbear"),
        SpeciesDef {
            name: "bugbear",
            family: "goblinoid",
            biosphere: BiosphereTraits {
                mass: Mass::new(132.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.15), (ANIMAL_PREY, 0.85)]).unwrap(),
                condition_niche: bugbear_condition_niche(),
                potency: 0.0,
            },
            peopled: Some(PeopledTraits {
                noun: "lair",
                psych: PsychVector {
                    threat_response: 0.8,
                    deliberation_latency: 0.4,
                    in_group_radius: 0.3,
                    time_horizon: 0.3,
                    sociality: Sociality::Communal,
                    status_basis: StatusBasis::Rank,
                },
                perception: PerceptionVector {
                    activity: ActivityCycle::Nocturnal,
                    night_vision: 0.7,
                    sky_attention: 0.3,
                },
                articulation: ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.4,
                    voicing: 0.7,
                    sibilance: 0.2,
                    voice_loudness: 0.3,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
                worker_override: Some("forager"),
                warrior: "mauler",
                artisan: "tanner",
                shaman: "omen-reader",
                top: "headman",
            }),
        },
    );
    // The menagerie (Task 4, decision-ledger #26): the first non-peopled
    // (biosphere-only, `peopled: None`) kinds the world holds — fauna, not
    // peoples. Mass is D&D 5E canon (kg), fixed by #26; niche is a single
    // dominant resource axis per creature so the twelve genuinely PARTITION
    // the resource-axis basis rather than reading as four more omnivores
    // (photosynthate: treant/twig-blight; plant-forage: giant-elk/woolly-
    // mammoth/giant-goat; animal-prey: the three dragons + owlbear; detritus:
    // otyugh; mineral: xorn/rust-monster). Potency > 0 only for the mighty
    // (dragons, treant, xorn); see each `*_condition_niche` helper above for
    // the climate-tile rationale.
    reg.insert(
        KindId("treant"),
        SpeciesDef {
            name: "treant",
            family: "plant",
            biosphere: BiosphereTraits {
                mass: Mass::new(1800.0).unwrap(),
                metabolic_class: MetabolicClass::Autotroph,
                niche: ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap(),
                condition_niche: treant_condition_niche(),
                potency: 0.6,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("twig-blight"),
        SpeciesDef {
            name: "twig-blight",
            family: "plant",
            biosphere: BiosphereTraits {
                mass: Mass::new(5.0).unwrap(),
                metabolic_class: MetabolicClass::Autotroph,
                niche: ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap(),
                condition_niche: twig_blight_condition_niche(),
                potency: 0.0,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("giant-elk"),
        SpeciesDef {
            name: "giant-elk",
            family: "giant-elk",
            biosphere: BiosphereTraits {
                mass: Mass::new(450.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_elk_condition_niche(),
                potency: 0.0,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("woolly-mammoth"),
        SpeciesDef {
            name: "woolly-mammoth",
            family: "woolly-mammoth",
            biosphere: BiosphereTraits {
                mass: Mass::new(6000.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: woolly_mammoth_condition_niche(),
                potency: 0.0,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("giant-goat"),
        SpeciesDef {
            name: "giant-goat",
            family: "giant-goat",
            biosphere: BiosphereTraits {
                mass: Mass::new(140.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
                condition_niche: giant_goat_condition_niche(),
                potency: 0.0,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("otyugh"),
        SpeciesDef {
            name: "otyugh",
            family: "otyugh",
            biosphere: BiosphereTraits {
                mass: Mass::new(260.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap(),
                condition_niche: otyugh_condition_niche(),
                potency: 0.0,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("xorn"),
        SpeciesDef {
            name: "xorn",
            family: "xorn",
            biosphere: BiosphereTraits {
                mass: Mass::new(55.0).unwrap(),
                metabolic_class: MetabolicClass::Ametabolic,
                niche: ResourceVector::new(&[(MINERAL, 1.0)]).unwrap(),
                condition_niche: xorn_condition_niche(),
                potency: 0.5,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("rust-monster"),
        SpeciesDef {
            name: "rust-monster",
            family: "rust-monster",
            biosphere: BiosphereTraits {
                mass: Mass::new(90.0).unwrap(),
                metabolic_class: MetabolicClass::Ectotherm,
                niche: ResourceVector::new(&[(MINERAL, 1.0)]).unwrap(),
                condition_niche: rust_monster_condition_niche(),
                potency: 0.0,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("white-dragon"),
        SpeciesDef {
            name: "white-dragon",
            family: "draconic",
            biosphere: BiosphereTraits {
                mass: Mass::new(2200.0).unwrap(), // 5E adult white dragon
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(), // obligate apex
                condition_niche: white_dragon_condition_niche(),
                potency: 0.85, // mighty
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("red-dragon"),
        SpeciesDef {
            name: "red-dragon",
            family: "draconic",
            biosphere: BiosphereTraits {
                mass: Mass::new(2700.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: red_dragon_condition_niche(),
                potency: 0.95, // mightiest of the three chromatics
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("black-dragon"),
        SpeciesDef {
            name: "black-dragon",
            family: "draconic",
            biosphere: BiosphereTraits {
                mass: Mass::new(2200.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: black_dragon_condition_niche(),
                potency: 0.85,
            },
            peopled: None,
        },
    );
    reg.insert(
        KindId("owlbear"),
        SpeciesDef {
            name: "owlbear",
            family: "owlbear",
            biosphere: BiosphereTraits {
                mass: Mass::new(450.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(),
                condition_niche: owlbear_condition_niche(),
                potency: 0.0,
            },
            peopled: None,
        },
    );
    reg
}

/// Proto ancestral articulation vectors, keyed by family, for families with
/// more than one member (a singleton's proto is itself and is absent here).
/// Each is a distinct point equal to no daughter's vector (spec §3).
/// type-audit: bare-ok(identifier-text)
pub fn family_registry() -> BTreeMap<KindId, ArticulationVector> {
    let mut m = BTreeMap::new();
    m.insert(
        KindId("goblinoid"),
        ArticulationVector {
            labiality: 0.5,
            vowel_space: 0.5,
            voicing: 0.55,
            sibilance: 0.45,
            voice_loudness: 0.55,
            tonality: 0.0,
            exotic: ExoticManner::None,
        },
    );
    // Fauna families (Task 4): the three chromatic dragons and the two plant
    // kinds are `peopled: None` and never speak, so this proto vector is
    // inert data today — it exists solely to satisfy the same-shaped
    // consistency guard every multi-member family carries
    // (`every_multi_member_family_has_a_proto`), keeping a future language
    // extension to fauna a data change rather than a new invariant.
    m.insert(
        KindId("draconic"),
        ArticulationVector {
            labiality: 0.3,
            vowel_space: 0.6,
            voicing: 0.7,
            sibilance: 0.6,
            voice_loudness: 0.8,
            tonality: 0.0,
            exotic: ExoticManner::None,
        },
    );
    m.insert(
        KindId("plant"),
        ArticulationVector {
            labiality: 0.5,
            vowel_space: 0.4,
            voicing: 0.4,
            sibilance: 0.3,
            voice_loudness: 0.3,
            tonality: 0.0,
            exotic: ExoticManner::None,
        },
    );
    m
}

/// Every seed-derivation label this crate uses (none — species are authored).
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register species' contribution to the concept registry.
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

    registry.register_concept("goblin-kind", "species", ConceptKind::Living, "a goblin")?;
    registry.register_concept("kobold-kind", "species", ConceptKind::Living, "a kobold")?;
    registry.register_concept(
        "hobgoblin-kind",
        "species",
        ConceptKind::Living,
        "a hobgoblin",
    )?;
    registry.register_concept("bugbear-kind", "species", ConceptKind::Living, "a bugbear")?;
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

/// Mint one entity per species in `roster` (in slice order) and commit its
/// authored vector as facts — the roster-scoped form of [`genesis`]. Species
/// entities carry facts ONLY under this crate's predicates (the superset
/// contract, spec §8, depends on it).
/// type-audit: bare-ok(identifier-text)
pub fn genesis_in(
    world: &mut World,
    roster: &[SpeciesDef],
) -> Result<BTreeMap<String, EntityId>, LedgerError> {
    let mut ids = BTreeMap::new();
    for def in roster {
        let name = def.name;
        let id = world.ledger.mint_entity();
        world.ledger.commit(
            fact(id, SPECIES_NAME, Value::Text(name.to_string())),
            &world.registry,
        )?;
        if let Some(peopled) = &def.peopled {
            let p = peopled.psych;
            let sociality = match p.sociality {
                Sociality::Hierarchic => "hierarchic",
                Sociality::Communal => "communal",
            };
            let status = match p.status_basis {
                StatusBasis::Rank => "rank",
                StatusBasis::Knowledge => "knowledge",
                StatusBasis::Generosity => "generosity",
            };
            world.ledger.commit(
                fact(id, THREAT_RESPONSE, Value::Number(p.threat_response)),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(
                    id,
                    DELIBERATION_LATENCY,
                    Value::Number(p.deliberation_latency),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(id, IN_GROUP_RADIUS, Value::Number(p.in_group_radius)),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(id, TIME_HORIZON, Value::Number(p.time_horizon)),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(id, SOCIALITY_MODE, Value::Text(sociality.to_string())),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(id, STATUS_BASIS, Value::Text(status.to_string())),
                &world.registry,
            )?;
            let activity = match peopled.perception.activity {
                ActivityCycle::Diurnal => "diurnal",
                ActivityCycle::Nocturnal => "nocturnal",
                ActivityCycle::Crepuscular => "crepuscular",
            };
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_ACTIVITY_CYCLE,
                    Value::Text(activity.to_string()),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_NIGHT_VISION,
                    Value::Number(peopled.perception.night_vision),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_SKY_ATTENTION,
                    Value::Number(peopled.perception.sky_attention),
                ),
                &world.registry,
            )?;
            let exotic = match peopled.articulation.exotic {
                ExoticManner::None => "none",
                ExoticManner::Trill => "trill",
                ExoticManner::Click => "click",
                ExoticManner::Ejective => "ejective",
            };
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_LABIALITY,
                    Value::Number(peopled.articulation.labiality),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_VOWEL_SPACE,
                    Value::Number(peopled.articulation.vowel_space),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_VOICING,
                    Value::Number(peopled.articulation.voicing),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_SIBILANCE,
                    Value::Number(peopled.articulation.sibilance),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_VOICE_LOUDNESS,
                    Value::Number(peopled.articulation.voice_loudness),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(
                    id,
                    SPECIES_TONALITY,
                    Value::Number(peopled.articulation.tonality),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                fact(id, SPECIES_EXOTIC_MANNER, Value::Text(exotic.to_string())),
                &world.registry,
            )?;
        }
        ids.insert(name.to_string(), id);
    }
    Ok(ids)
}

/// Mint one entity per shipped species (registry order) and commit its
/// authored vector as facts.
/// type-audit: bare-ok(identifier-text)
pub fn genesis(world: &mut World) -> Result<BTreeMap<String, EntityId>, LedgerError> {
    let roster: Vec<SpeciesDef> = registry().into_values().collect();
    genesis_in(world, &roster)
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

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

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
        let reg = registry();
        let g = &reg[&KindId("goblin")].peopled.as_ref().unwrap().psych;
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
        let reg = registry();
        let names: Vec<&str> = reg.keys().map(|k| k.0).collect();
        // The roster grew with the Task 4 menagerie (12 biosphere-only fauna
        // alongside the four peoples); BTreeMap key order is lexicographic.
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
        let k = &reg[&KindId("kobold")].peopled.as_ref().unwrap().psych;
        assert_eq!(k.sociality, Sociality::Communal);
        assert_eq!(k.status_basis, StatusBasis::Knowledge);
        assert!(k.in_group_radius < 0.5 && k.time_horizon > 0.5 && k.threat_response > 0.5);
        assert_eq!(
            reg[&KindId("kobold")].peopled.as_ref().unwrap().noun,
            "warren"
        );
        assert_eq!(
            reg[&KindId("kobold")].peopled.as_ref().unwrap().top,
            "elders"
        );
    }

    #[test]
    fn genesis_commits_vector_facts_and_people_links_settlements() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let settlement = w.ledger.mint_entity();
        let ids = genesis(&mut w).unwrap();
        // 4 peoples + the Task 4 menagerie's 12 biosphere-only fauna: genesis
        // mints an entity per species in the roster regardless of `peopled`.
        assert_eq!(ids.len(), 16);
        people(&mut w, settlement, "kobold").unwrap();
        assert_eq!(species_of(&w, settlement).as_deref(), Some("kobold"));
        // The species entity carries its vector under species predicates.
        let kobold = ids["kobold"];
        assert!(matches!(
            w.ledger.value_of(kobold, THREAT_RESPONSE),
            Some(Value::Number(n)) if *n > 0.5
        ));
        assert_eq!(w.ledger.text_of(kobold, SPECIES_NAME), Some("kobold"));
    }

    #[test]
    fn species_facts_touch_no_pre_existing_predicate() {
        let mut w = World::new(Seed(1));
        register_concepts(&mut w.registry).unwrap();
        genesis(&mut w).unwrap();
        // No species fact may land under the kernel NAME predicate (or any
        // other pre-C1 predicate) — the superset contract depends on it.
        for f in w.ledger.iter() {
            assert!(
                f.predicate.starts_with("species-") || f.predicate == PEOPLED_BY,
                "unexpected predicate {}",
                f.predicate
            );
        }
    }

    #[test]
    fn goblin_perception_is_the_baseline_and_kobold_contrasts() {
        let reg = registry();
        let g = &reg[&KindId("goblin")].peopled.as_ref().unwrap().perception;
        assert_eq!(g.activity, ActivityCycle::Diurnal);
        assert_eq!(g.night_vision, 0.5);
        assert_eq!(g.sky_attention, 0.5);
        let k = &reg[&KindId("kobold")].peopled.as_ref().unwrap().perception;
        assert_eq!(k.activity, ActivityCycle::Nocturnal);
        assert!(k.night_vision > 0.5 && k.sky_attention > 0.5);
    }

    #[test]
    fn genesis_commits_perception_facts() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let ids = genesis(&mut w).unwrap();
        let kobold = ids["kobold"];
        assert_eq!(
            w.ledger.text_of(kobold, SPECIES_ACTIVITY_CYCLE),
            Some("nocturnal")
        );
        assert!(matches!(
            w.ledger.value_of(kobold, SPECIES_NIGHT_VISION),
            Some(Value::Number(n)) if *n > 0.5
        ));
        assert_eq!(species_entity(&w, "kobold"), Some(kobold));
    }

    #[test]
    fn goblin_articulation_is_baseline_kobold_hisses_and_is_quiet() {
        let reg = registry();
        let g = &reg[&KindId("goblin")]
            .peopled
            .as_ref()
            .unwrap()
            .articulation;
        assert_eq!(g.labiality, 0.5);
        assert_eq!(g.voice_loudness, 0.5);
        assert_eq!(g.exotic, ExoticManner::None);
        let k = &reg[&KindId("kobold")]
            .peopled
            .as_ref()
            .unwrap()
            .articulation;
        assert!(k.sibilance > 0.5 && k.labiality < 0.5 && k.voice_loudness < 0.5);
        assert_eq!(k.exotic, ExoticManner::Trill);
    }

    #[test]
    fn genesis_commits_articulation_facts() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let ids = genesis(&mut w).unwrap();
        let k = ids["kobold"];
        assert_eq!(w.ledger.text_of(k, SPECIES_EXOTIC_MANNER), Some("trill"));
        assert!(
            matches!(w.ledger.value_of(k, SPECIES_SIBILANCE), Some(Value::Number(n)) if *n > 0.5)
        );
    }

    #[test]
    fn registry_has_the_goblinoid_triad_and_kobold() {
        let r = registry();
        for name in ["goblin", "hobgoblin", "bugbear", "kobold"] {
            assert!(r.contains_key(&KindId(name)), "{name} missing");
        }
        assert_eq!(r[&KindId("hobgoblin")].family, "goblinoid");
        assert_eq!(r[&KindId("bugbear")].family, "goblinoid");
        assert_eq!(r[&KindId("kobold")].family, "kobold");
    }

    #[test]
    fn family_divides_along_voice_loudness() {
        let r = registry();
        let l = |n: &'static str| {
            r[&KindId(n)]
                .peopled
                .as_ref()
                .unwrap()
                .articulation
                .voice_loudness
        };
        assert!(l("bugbear") < l("goblin") && l("goblin") < l("hobgoblin"));
    }

    #[test]
    fn proto_goblinoid_vector_equals_no_daughter() {
        let proto = family_registry()[&KindId("goblinoid")];
        let r = registry();
        for d in ["goblin", "hobgoblin", "bugbear"] {
            assert_ne!(
                proto,
                r[&KindId(d)].peopled.as_ref().unwrap().articulation,
                "proto must differ from {d}"
            );
        }
    }

    #[test]
    fn every_multi_member_family_has_a_proto() {
        // CONSISTENCY GUARD (matters as more families are added). A species'
        // `family` field points into `family_registry` by name; a lookup MISS
        // falls through to the singleton path, so a typo'd or forgotten family
        // would silently demote a would-be family member to an isolated language
        // with no cognates. Assert every family shared by ≥2 species has a proto
        // entry — the miss can then never be silent.
        let r = registry();
        let mut counts: BTreeMap<&str, usize> = BTreeMap::new();
        for def in r.values() {
            *counts.entry(def.family).or_default() += 1;
        }
        let fams = family_registry();
        for (family, n) in counts {
            if n >= 2 {
                assert!(
                    fams.contains_key(&KindId(family)),
                    "family {family} has {n} members but no proto vector"
                );
            }
        }
    }

    #[test]
    fn goblinoids_carry_mass_and_a_nonzero_omnivore_niche() {
        let r = registry();
        for name in ["goblin", "kobold", "hobgoblin", "bugbear"] {
            let s = &r[&KindId(name)];
            assert!(s.biosphere.mass.kilograms() > 0.0, "{name} has mass");
            assert!(!s.biosphere.niche.is_zero(), "{name} eats something");
            // omnivores: both plant-forage and animal-prey present
            assert!(s.biosphere.niche.weight(hornvale_kernel::PLANT_FORAGE) > 0.0);
            assert!(s.biosphere.niche.weight(hornvale_kernel::ANIMAL_PREY) > 0.0);
        }
        // strict, modest, monotone mass band: kobold < goblin < hobgoblin < bugbear
        assert!(
            r[&KindId("kobold")].biosphere.mass.kilograms()
                < r[&KindId("goblin")].biosphere.mass.kilograms()
        );
        assert!(
            r[&KindId("goblin")].biosphere.mass.kilograms()
                < r[&KindId("hobgoblin")].biosphere.mass.kilograms()
        );
        assert!(
            r[&KindId("hobgoblin")].biosphere.mass.kilograms()
                < r[&KindId("bugbear")].biosphere.mass.kilograms()
        );
    }

    #[test]
    fn every_species_has_a_finite_condition_niche() {
        for (name, def) in registry() {
            let name = name.0;
            for r in [
                def.biosphere.condition_niche.temperature,
                def.biosphere.condition_niche.moisture,
                def.biosphere.condition_niche.insolation,
                def.biosphere.condition_niche.elevation,
            ] {
                assert!(r.optimum.is_finite(), "{name} optimum finite");
                assert!(
                    r.width.is_finite() && r.width > 0.0,
                    "{name} width positive"
                );
                assert!(r.devotion.is_finite(), "{name} devotion finite");
            }
            assert!(
                def.biosphere.potency >= 0.0 && def.biosphere.potency.is_finite(),
                "{name} potency >= 0"
            );
        }
    }

    #[test]
    fn the_four_peoples_have_distinct_temperature_optima() {
        let reg = registry();
        let opts: Vec<f64> = ["kobold", "goblin", "hobgoblin", "bugbear"]
            .iter()
            .map(|n| {
                reg[&KindId(n)]
                    .biosphere
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
        let r = registry();
        assert_eq!(r[&KindId("goblin")].biosphere.metabolic_class, Endotherm);
        assert_eq!(r[&KindId("hobgoblin")].biosphere.metabolic_class, Endotherm);
        assert_eq!(r[&KindId("bugbear")].biosphere.metabolic_class, Endotherm);
        assert_eq!(r[&KindId("kobold")].biosphere.metabolic_class, Ectotherm); // reptilian/draconic SRD lineage
    }

    #[test]
    fn split_preserves_biosphere_and_peopled_presence() {
        let reg = registry();
        let goblin = &reg[&KindId("goblin")];
        // biosphere moved intact
        assert_eq!(goblin.biosphere.mass, Mass::new(18.1).unwrap());
        assert_eq!(goblin.biosphere.potency, 0.0);
        // the four peoples all speak/settle
        for name in ["goblin", "kobold", "hobgoblin", "bugbear"] {
            assert!(
                reg[&KindId(name)].peopled.is_some(),
                "{name} must carry PeopledTraits"
            );
        }
    }

    #[test]
    fn genesis_in_with_registry_slice_matches_genesis_exactly() {
        let roster: Vec<SpeciesDef> = registry().into_values().collect();
        let mut a = World::new(Seed(42));
        register_concepts(&mut a.registry).unwrap();
        let ids_a = genesis(&mut a).unwrap();

        let mut b = World::new(Seed(42));
        register_concepts(&mut b.registry).unwrap();
        let ids_b = genesis_in(&mut b, &roster).unwrap();

        assert_eq!(ids_a, ids_b, "same ids in same order");
        let fa: Vec<_> = a.ledger.iter().collect();
        let fb: Vec<_> = b.ledger.iter().collect();
        assert_eq!(
            fa, fb,
            "genesis_in over the registry slice must be byte-identical to genesis"
        );
    }

    #[test]
    fn menagerie_is_biosphere_only_and_spans_axes() {
        let reg = registry();
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
            let d = &reg[&KindId(name)];
            assert!(d.peopled.is_none(), "{name} is fauna: no PeopledTraits");
            // `Mass` has no PartialOrd, so read the raw kilograms rather
            // than comparing against `Mass::new(0.0)`.
            assert!(d.biosphere.mass.kilograms() > 0.0, "{name} has mass");
        }
        // mighty creatures carry potency
        assert!(reg[&KindId("red-dragon")].biosphere.potency > 0.0);
        assert!(reg[&KindId("treant")].biosphere.potency > 0.0);
        assert!(reg[&KindId("xorn")].biosphere.potency > 0.0);
        // the material, non-mighty fauna carry none
        assert_eq!(reg[&KindId("owlbear")].biosphere.potency, 0.0);
        assert_eq!(reg[&KindId("rust-monster")].biosphere.potency, 0.0);

        // resource niches are partitioned, not four omnivores: the distinct
        // dominant axis differs across creatures. `ResourceVector::overlap`
        // is the packer's Pianka overlap; disjoint axes overlap 0.
        let overlap = reg[&KindId("treant")]
            .biosphere
            .niche
            .overlap(&reg[&KindId("white-dragon")].biosphere.niche);
        assert!(
            overlap < 0.5,
            "photosynthate vs apex niches must barely overlap"
        );

        // Directly assert the basis-constant partition the brief calls for.
        assert_eq!(
            reg[&KindId("treant")].biosphere.niche.weight(PHOTOSYNTHATE),
            1.0
        );
        assert_eq!(
            reg[&KindId("twig-blight")]
                .biosphere
                .niche
                .weight(PHOTOSYNTHATE),
            1.0
        );
        for name in ["giant-elk", "woolly-mammoth", "giant-goat"] {
            assert_eq!(reg[&KindId(name)].biosphere.niche.weight(PLANT_FORAGE), 1.0);
        }
        for name in ["white-dragon", "red-dragon", "black-dragon", "owlbear"] {
            assert_eq!(reg[&KindId(name)].biosphere.niche.weight(ANIMAL_PREY), 1.0);
        }
        assert_eq!(reg[&KindId("otyugh")].biosphere.niche.weight(DETRITUS), 1.0);
        for name in ["xorn", "rust-monster"] {
            assert_eq!(reg[&KindId(name)].biosphere.niche.weight(MINERAL), 1.0);
        }
    }

    #[test]
    fn menagerie_families_registered_and_climate_tiles_distinct() {
        // The three chromatics and the two plant kinds are multi-member
        // families; `every_multi_member_family_has_a_proto` already guards
        // this generically, but pin the two new families explicitly so a
        // future edit that deletes one fails loudly here too.
        let fams = family_registry();
        assert!(fams.contains_key(&KindId("draconic")));
        assert!(fams.contains_key(&KindId("plant")));

        let reg = registry();
        for name in ["white-dragon", "red-dragon", "black-dragon"] {
            assert_eq!(reg[&KindId(name)].family, "draconic");
        }
        for name in ["treant", "twig-blight"] {
            assert_eq!(reg[&KindId(name)].family, "plant");
        }

        // The three chromatics claim distinct climate tiles even though they
        // share the animal-prey axis: white owns the cold, and red/black —
        // both warm — separate on moisture (volcanic-arid vs. swamp-wet).
        let temp = |n: &'static str| {
            reg[&KindId(n)]
                .biosphere
                .condition_niche
                .temperature
                .optimum
        };
        let moisture = |n: &'static str| reg[&KindId(n)].biosphere.condition_niche.moisture.optimum;
        assert!(temp("white-dragon") < temp("red-dragon"));
        assert!(temp("white-dragon") < temp("black-dragon"));
        assert!(
            moisture("red-dragon") < moisture("black-dragon") - 0.3,
            "volcanic-arid red must sit well below swamp-wet black on moisture"
        );
    }
}
