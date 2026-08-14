//! The environment-axis assignment for this domain's named communities.
//!
//! **This is a DERIVATION, not a roster** (decision 0094). The kernel owns the
//! axis vocabulary; what a formation *means* on those axes is climate's own,
//! and no other domain is bound by it.
//!
//! # How the values were derived
//!
//! Not by taste. `classify_land` (`crate::biome`) is a decision tree over
//! temperature and moisture with an alpine gate, and `classify_marine_expr` is
//! a precedence chain over depth, surface temperature, seafloor feature and
//! upwelling. Those two functions are the authority for what the 21 formations
//! mean, and the values below are read off them. The consequence is stated
//! plainly in the campaign spec §3.2 and is not hidden here: **on land the
//! decomposition is a factoring, not a fidelity gain**, because every value is
//! a function of inputs `classify_land` already reads. Fidelity can only come
//! from `SUBSTRATE` (which reads rock, slope and depth) and from the realm
//! column.
//!
//! # Scales
//!
//! Five axes are occupied, quantised to fixed levels so the decomposition is
//! *compressive* rather than merely *total* — an axis per name would reconstruct
//! the corpus perfectly and buy nothing.
//!
//! - `PHYSIOGNOMY` six levels, bare to closed-canopy.
//! - `ENERGY` five levels, tracking `classify_land`'s temperature bands.
//! - `WATER` six levels, tracking its moisture cuts.
//! - `SUBSTRATE` six levels, nominal: the index is a class, never a magnitude.
//! - `LIGHT` six levels; in the sea this **is** the depth rung.
//!
//! # A formation is a genus, so it leaves its variants' disagreements open
//!
//! The vector is **sparse**, and that is load-bearing rather than incidental: a
//! formation assigns only the axes it actually determines, and leaves
//! unassigned any axis its own variants disagree on. `Desert` does not fix
//! `SUBSTRATE` — `variant_pool` branches a desert into erg, playa, hamada and
//! reg precisely *by* ground — and `Ice` does not fix `LIGHT`, because its three
//! variants are surface forms that scatter light differently.
//!
//! This rule was adopted during the fit, after the collision check reported
//! `ice`/`snowfield` and `desert`/`erg` sharing vectors. That is the fit stage
//! working as licensed (spec §4.4): the *criteria* are frozen, the assignment is
//! not. Pinning a genus to one species' values was a modelling error, and the
//! fix is not a threshold move.
//!
//! # The sixth axis is empty, and that is the finding
//!
//! `DISTURBANCE` takes no value anywhere in this corpus. It is the basis's only
//! `Rate` axis and a whole-community name is a state, so any name needing it
//! meets the campaign's operational definition of *resistance*: assignment
//! would require a value no other name uses. Those names carry the unassigned
//! vector instead. See `tests/preregistration.rs`.

use hornvale_kernel::{
    ENERGY, EnvironmentAxis, EnvironmentVector, LIGHT, PHYSIOGNOMY, SUBSTRATE, WATER,
};
use std::sync::OnceLock;

// PHYSIOGNOMY — vegetation structure, bare to closed.
const BARE: f64 = 0.0;
const CRUST: f64 = 0.2;
const HERB: f64 = 0.4;
const SHRUB: f64 = 0.6;
const OPEN_WOOD: f64 = 0.8;
const CLOSED: f64 = 1.0;

// ENERGY — five levels over `classify_land`'s temperature bands.
const FROZEN: f64 = 0.0;
const COLD: f64 = 0.25;
const COOL: f64 = 0.5;
const WARM: f64 = 0.75;
const HOT: f64 = 1.0;

// WATER — six levels over `classify_land`'s moisture cuts.
const W_NONE: f64 = 0.0;
const W_ARID: f64 = 0.2;
const W_SEMI: f64 = 0.4;
const W_MESIC: f64 = 0.6;
const W_WET: f64 = 0.8;
const W_SAT: f64 = 1.0;

// SUBSTRATE — NOMINAL. The index names a class; it is not ordered.
const S_SOIL: f64 = 0.0;
const S_SAND: f64 = 0.2;
const S_EVAPORITE: f64 = 0.4;
const S_ROCK: f64 = 0.6;
const S_ICE: f64 = 0.8;
const S_ORGANIC: f64 = 1.0;

// LIGHT — six levels. In the sea this is the depth rung, not a separate axis.
const L_DARK: f64 = 0.0;
const L_DIM: f64 = 0.2;
const L_SHADED: f64 = 0.4;
const L_DAPPLED: f64 = 0.6;
const L_OPEN: f64 = 0.8;
const L_GLARE: f64 = 1.0;

/// The ten names that resist assignment: phases, not states. Frozen as a
/// prediction in the campaign spec §6.2 before this assignment was written.
const RESISTERS: &[&str] = &[
    "forest-gap",
    "mossy-deadfall",
    "burn",
    "fire-scrub",
    "reef-rubble",
    "urchin-barren",
    "pressure-ridge",
    "ice-lead",
    "rafted-floe",
    "melt-pond",
];

/// One corpus name and the vector assigned to it.
/// type-audit: bare-ok(identifier-text: name)
#[derive(Debug, Clone, PartialEq)]
pub struct AssignedName {
    /// The name's registry spelling — `Variant::concept_name()` where one
    /// exists, otherwise the kebab-case of the `Formation` variant.
    pub name: &'static str,
    /// Its position in the basis. Unassigned means the name resisted.
    pub vector: EnvironmentVector,
}

fn v(values: &[(EnvironmentAxis, f64)]) -> EnvironmentVector {
    EnvironmentVector::new(values).expect("authored axis values are within [0, 1]")
}

/// A name the axes cannot place. Not an error — a counted finding.
fn resists(name: &'static str) -> AssignedName {
    AssignedName {
        name,
        vector: EnvironmentVector::new(&[]).expect("the empty vector is legal"),
    }
}

/// A fully-assigned name: all five occupied axes.
fn a(
    name: &'static str,
    physiognomy: f64,
    energy: f64,
    water: f64,
    substrate: f64,
    light: f64,
) -> AssignedName {
    AssignedName {
        name,
        vector: v(&[
            (PHYSIOGNOMY, physiognomy),
            (ENERGY, energy),
            (WATER, water),
            (SUBSTRATE, substrate),
            (LIGHT, light),
        ]),
    }
}

/// A genus that declines one axis its own variants disagree on.
fn genus(name: &'static str, values: &[(EnvironmentAxis, f64)]) -> AssignedName {
    AssignedName {
        name,
        vector: v(values),
    }
}

fn build() -> Vec<AssignedName> {
    let mut named = vec![
        // ---- The 12 land formations, read off `classify_land`'s tree. ----
        // LIGHT declined: snowfield, crevasse-field and scoured-ice are surface
        // forms that scatter light differently, so the genus does not fix it.
        genus(
            "ice",
            &[
                (PHYSIOGNOMY, BARE),
                (ENERGY, FROZEN),
                (WATER, W_NONE),
                (SUBSTRATE, S_ICE),
            ],
        ),
        a("tundra", CRUST, COLD, W_ARID, S_ORGANIC, L_OPEN),
        a("taiga", OPEN_WOOD, COLD, W_MESIC, S_ORGANIC, L_SHADED),
        a("temperate-grassland", HERB, COOL, W_ARID, S_SOIL, L_OPEN),
        a("shrubland", SHRUB, COOL, W_SEMI, S_SOIL, L_OPEN),
        a("temperate-forest", CLOSED, COOL, W_MESIC, S_SOIL, L_SHADED),
        a("temperate-rainforest", CLOSED, COOL, W_SAT, S_SOIL, L_DIM),
        // SUBSTRATE declined: `variant_pool` branches a desert into erg, playa,
        // hamada and reg *by ground*, so the genus cannot fix it.
        genus(
            "desert",
            &[
                (PHYSIOGNOMY, BARE),
                (ENERGY, HOT),
                (WATER, W_NONE),
                (LIGHT, L_GLARE),
            ],
        ),
        a("savanna", OPEN_WOOD, HOT, W_SEMI, S_SOIL, L_OPEN),
        a(
            "tropical-seasonal-forest",
            CLOSED,
            HOT,
            W_MESIC,
            S_SOIL,
            L_SHADED,
        ),
        a("tropical-rainforest", CLOSED, HOT, W_SAT, S_SOIL, L_DARK),
        // Alpine is the tree-line gate, not a temperature band: it shares
        // tundra's structure and energy and is separated by SUBSTRATE (thin
        // rock over slope) — precisely an input `classify_land` does not read.
        a("alpine", CRUST, COLD, W_SEMI, S_ROCK, L_GLARE),
        // ---- The 6 marine formations. LIGHT is the depth rung. ----
        a("sea-ice", BARE, FROZEN, W_SAT, S_ICE, L_OPEN),
        a("reef", CLOSED, HOT, W_SAT, S_ROCK, L_GLARE),
        a("kelp-forest", CLOSED, COLD, W_SAT, S_ROCK, L_OPEN),
        a("vent", SHRUB, HOT, W_SAT, S_ROCK, L_DARK),
        a("upwelling", HERB, COOL, W_SAT, S_SOIL, L_DAPPLED),
        a("open-water", BARE, COOL, W_SAT, S_SOIL, L_OPEN),
        // ---- The 3 cave formations. LIGHT is constant zero underground. ----
        a("karst-cave", BARE, COLD, W_WET, S_ROCK, L_DARK),
        a("lava-tube", BARE, COLD, W_ARID, S_ROCK, L_DARK),
        a("fracture-cave", BARE, COLD, W_SEMI, S_ROCK, L_DARK),
        // ---- Desert variants: SUBSTRATE is what separates them. ----
        a("erg", BARE, HOT, W_NONE, S_SAND, L_GLARE),
        a("playa", BARE, HOT, W_ARID, S_EVAPORITE, L_GLARE),
        a("hamada", BARE, HOT, W_NONE, S_ROCK, L_GLARE),
        a("reg", CRUST, HOT, W_NONE, S_ROCK, L_GLARE),
        // ---- Temperate-forest variants. ----
        a("old-growth", CLOSED, COOL, W_MESIC, S_ORGANIC, L_DARK),
        a("damp-hollow", SHRUB, COOL, W_SAT, S_ORGANIC, L_DIM),
        // ---- Taiga variants. ----
        a("boreal-stand", OPEN_WOOD, COLD, W_MESIC, S_ORGANIC, L_DIM),
        a("muskeg", HERB, COLD, W_SAT, S_ORGANIC, L_OPEN),
        // ---- Tundra variants. ----
        a("frost-heave", CRUST, COLD, W_MESIC, S_SOIL, L_OPEN),
        a("felsenmeer", BARE, COLD, W_ARID, S_ROCK, L_OPEN),
        a("wind-scour", BARE, COLD, W_NONE, S_ROCK, L_GLARE),
        // ---- Grassland variants. ----
        a("grass-sward", HERB, COOL, W_SEMI, S_SOIL, L_GLARE),
        a("wooded-grassland", SHRUB, COOL, W_SEMI, S_SOIL, L_DAPPLED),
        // ---- Tropical-rainforest variants. ----
        a("closed-canopy", CLOSED, HOT, W_SAT, S_ORGANIC, L_DARK),
        a("liana-forest", CLOSED, HOT, W_WET, S_ORGANIC, L_DARK),
        a("gallery-forest", CLOSED, HOT, W_SAT, S_SOIL, L_DIM),
        // ---- Ice variants. ----
        a("snowfield", BARE, FROZEN, W_NONE, S_ICE, L_GLARE),
        a("crevasse-field", BARE, FROZEN, W_ARID, S_ICE, L_SHADED),
        a("scoured-ice", BARE, FROZEN, W_NONE, S_ICE, L_OPEN),
        // ---- Shrubland variants. ----
        a("thorn-scrub", SHRUB, HOT, W_ARID, S_SOIL, L_GLARE),
        a("sclerophyll-scrub", SHRUB, WARM, W_SEMI, S_SOIL, L_OPEN),
        // ---- Reef variants. ----
        a("coral-head", CLOSED, HOT, W_SAT, S_ROCK, L_OPEN),
        a("spur-and-groove", SHRUB, HOT, W_SAT, S_ROCK, L_GLARE),
        a("staghorn-stand", OPEN_WOOD, HOT, W_SAT, S_ROCK, L_GLARE),
        // ---- Kelp variants. ----
        a("kelp-canopy", CLOSED, COLD, W_SAT, S_ROCK, L_GLARE),
        a("holdfast-tangle", SHRUB, COLD, W_SAT, S_ROCK, L_SHADED),
        // ---- Vent variants. ----
        a("smoker-field", CRUST, HOT, W_SAT, S_ROCK, L_DARK),
        a("tubeworm-thicket", SHRUB, WARM, W_SAT, S_ROCK, L_DARK),
        a("vent-plume", BARE, HOT, W_SAT, S_ROCK, L_DARK),
        // ---- Upwelling variants. ----
        a("plankton-bloom", HERB, COOL, W_SAT, S_SOIL, L_OPEN),
        a("cold-upwelling", BARE, COLD, W_SAT, S_SOIL, L_DAPPLED),
        a("bait-ball", SHRUB, COOL, W_SAT, S_SOIL, L_DAPPLED),
        // ---- Open-water variants. LIGHT descends the pelagic ladder. ----
        a("open-blue", BARE, WARM, W_SAT, S_SOIL, L_GLARE),
        a("sargassum-drift", HERB, WARM, W_SAT, S_ORGANIC, L_GLARE),
        a("fish-shoal", SHRUB, WARM, W_SAT, S_SOIL, L_GLARE),
        a("twilight-water", BARE, COOL, W_SAT, S_SOIL, L_SHADED),
        a("scattering-layer", CRUST, COOL, W_SAT, S_SOIL, L_SHADED),
        a("lightless-water", BARE, COLD, W_SAT, S_SOIL, L_DARK),
        a("marine-snow", CRUST, COLD, W_SAT, S_ORGANIC, L_DARK),
        a("abyssal-plain", BARE, COLD, W_SAT, S_SOIL, L_DIM),
        a("nodule-field", CRUST, COLD, W_SAT, S_ROCK, L_DIM),
        a("trench-wall", BARE, FROZEN, W_SAT, S_ROCK, L_DARK),
        a("trench-floor", CRUST, FROZEN, W_SAT, S_SOIL, L_DARK),
    ];
    named.extend(RESISTERS.iter().copied().map(resists));
    named
}

/// The full assignment over this domain's 74-name corpus.
pub fn assignment() -> &'static [AssignedName] {
    static A: OnceLock<Vec<AssignedName>> = OnceLock::new();
    A.get_or_init(build)
}
