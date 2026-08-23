//! The underworld's communities as points in The Axes' environment basis
//! (spec §4.4).
//!
//! The sibling of [`crate::axes`], and deliberately the same shape: a corpus
//! of named communities, each a point in `hornvale_kernel`'s six-axis
//! environment basis, hand-authored and then *measured* rather than asserted.
//! Where `axes.rs` reads its values off `classify_land` and
//! `classify_marine_expr`, this module has no classifier to read off — the
//! underworld has no `Formation` enum — so **every value here is AUTHORED**,
//! and each carries a comment naming the physical input that drove it. That
//! difference is the honest statement of this corpus's status: `axes.rs` is a
//! factoring of an existing decision tree; this is a fresh authoring against
//! fields terrain already owns.
//!
//! # Layering — why the drivers are named in prose and not in types
//!
//! `hornvale-climate` may not import `hornvale-terrain` (constitutional; see
//! `domains/CLAUDE.md`). `CaveKind`, `RockClass`, `MaterialBuffer`,
//! `Commodity`, `WaterKind` and `DelveRung` therefore appear here as **words**,
//! never as types. [`DelveZone`] is the one exception and is a *mirrored
//! roster* under decision 0094: the five habitation rung NAMES and their
//! order are shared, the derivation that maps a ΔT to a rung stays in
//! `hornvale_terrain::delve` and is not duplicated. `cli/tests/
//! delve_roster_mirror.rs` fails if the two rosters drift.
//!
//! # The scales are the surface corpus's scales
//!
//! Every level below is numerically identical to the one `axes.rs` authored,
//! and `tests/underworld.rs::the_underworld_uses_the_surface_corpus_value_grid`
//! asserts that no value here is off that grid. This is load-bearing rather
//! than tidy: The Axes' keystone is *one space at every grain and in every
//! realm*, and a distance between an underworld community and a surface
//! formation is only a distance if both were placed on the same rulers. The
//! constants are re-declared rather than imported because `axes.rs` keeps its
//! own private, and widening them to `pub` would export a surface-flavoured
//! vocabulary (`HERB`, `OPEN_WOOD`) that means something else down here.
//!
//! # What each axis means underground
//!
//! Four of the five are the same quantity, differently *derived*; none is a
//! different quantity.
//!
//! - `PHYSIOGNOMY` — the kernel calls it "how the living cover is built, open
//!   to closed". Underground the cover is the void's own form plus what
//!   encrusts it, and **process is what sets both** (spec §4.4): karst
//!   dissolves into rounded, speleothem-rich galleries with a large wetted
//!   surface; a lava tube is a smooth low-surface-area pipe; a fracture is
//!   angular and rubble-floored. The ordinal reading survives intact — it is
//!   still "how much structure stands between bare rock and a closed
//!   habitat". **This is a regrounding and is stated as one:** the three cave
//!   formations in `axes.rs` all read `BARE`, so this corpus's karst names
//!   sit structurally *above* their own genus. See the task report.
//! - `ENERGY` — "available energy for primary production", the kernel's own
//!   words, unchanged. `axes.rs` proxied it by temperature band because on
//!   land insolation and warmth are the supply. Underground the supply is
//!   **detrital import** near the surface and **chemolithotrophy** off the
//!   geothermal gradient at depth, so the axis *inverts* with depth rather
//!   than falling with it (spec §4.4). That inversion is the corpus's central
//!   claim and is measured, not asserted, by
//!   `tests/underworld.rs::energy_is_not_monotone_in_depth`.
//! - `WATER` — unchanged, and the richest discriminator here: `carbonate`
//!   says whether dissolution was possible at all, `porosity` what the rock
//!   holds, overhead `drainage` what arrives, the water table whether the
//!   void is vadose or phreatic, and a `SaltBasin` overhead means brine.
//! - `SUBSTRATE` — unchanged, and still `Nominal`: the same six classes
//!   `axes.rs` uses, with the same meanings. Rock, evaporite, organic, soil,
//!   sand and ice all occur underground and none needed reinterpreting.
//! - `LIGHT` — the kernel already declares it "constant zero underground".
//!   Spec §5's H5 predicted at most two distinct values across this corpus
//!   before it was authored; it measures two.
//!
//! `DISTURBANCE` takes no value here, exactly as in `axes.rs`, and for the
//! same reason: it is the basis's only `Rate` axis. That is what the two
//! resisters at the end of [`build`] run into.
//!
//! # What the corpus is not
//!
//! Not compressive in The Axes' sense, and this is stated rather than
//! discovered later. Realised cardinalities are 5/5/6/6/2, summing to 24 over
//! 22 assigned names — a decomposition that costs slightly *more* symbols than
//! the enum it describes. The Axes' 74-name corpus could claim compression
//! (sum 24 against 74 names); a 22-name corpus cannot, and this module does
//! not pretend to. What it buys instead is **commensurability**: these 22 are
//! points in the same space as the 74, which is what `EnvironmentNiche` needs
//! and what a name-per-community enum could never give.

use hornvale_kernel::{
    ENERGY, EnvironmentAxis, EnvironmentVector, LIGHT, PHYSIOGNOMY, SUBSTRATE, WATER,
};
use std::sync::OnceLock;

// PHYSIOGNOMY — void form and its encrustation, bare rock to closed habitat.
// AUTHORED; the same six levels `axes.rs` uses.
/// Smooth, unencrusted wall; nothing stands between rock and open space.
const P_SMOOTH: f64 = 0.0;
/// A film or crust on rock — biofilm, mineral rind, a scoured shaft.
const P_CRUST: f64 = 0.2;
/// A broken, blocky or mat-covered floor: structure underfoot only.
const P_BROKEN: f64 = 0.4;
/// Standing structure — speleothem stands, gypsum curtains, fungal thickets.
const P_STANDS: f64 = 0.6;
/// A decorated gallery: columns and curtains, the richest form karst reaches.
const P_GALLERY: f64 = 0.8;

// ENERGY — available energy for primary production. AUTHORED; the same five
// levels `axes.rs` uses (0, 0.25, 0.5, 0.75, 1.0).
/// No import from above and no exploitable chemical gradient.
const E_INERT: f64 = 0.0;
/// Seepage-borne traces only.
const E_LEAN: f64 = 0.25;
/// A working base — a stream's organic load, or a modest chemical one.
const E_FED: f64 = 0.5;
/// A strong base: direct detrital delivery, or sulphide oxidation at depth.
const E_RICH: f64 = 0.75;
/// The richest the underworld reaches — a whole channel's load at one point,
/// or full chemolithotrophy on a hot gradient.
const E_TEEMING: f64 = 1.0;

// WATER — AUTHORED; the same six levels `axes.rs` uses.
/// Dry: no carbonate, negligible porosity, no drainage overhead.
const W_NONE: f64 = 0.0;
/// Free-draining; water passes and does not stay.
const W_ARID: f64 = 0.2;
/// Fracture-borne seepage.
const W_SEMI: f64 = 0.4;
/// Persistent seepage or condensation; damp walls.
const W_MESIC: f64 = 0.6;
/// An active watercourse, still vadose.
const W_WET: f64 = 0.8;
/// Below the water table: phreatic, flooded, or brine.
const W_SAT: f64 = 1.0;

// SUBSTRATE — NOMINAL. AUTHORED; the same six classes `axes.rs` uses, with
// the same meanings. The index names a class and is never a magnitude.
/// Clastic fill — washed-in mud and silt.
const S_SOIL: f64 = 0.0;
/// Loose granular ground — scoria grit, alluvial sand.
const S_SAND: f64 = 0.2;
/// Evaporite — halite and replacement gypsum.
const S_EVAPORITE: f64 = 0.4;
/// Bare rock, of whatever `RockClass`.
const S_ROCK: f64 = 0.6;
/// Ice — a cold-trap floor.
const S_ICE: f64 = 0.8;
/// Organic ground — buried carbon, coal measures, accumulated detritus.
const S_ORGANIC: f64 = 1.0;

// LIGHT — AUTHORED; two of the six levels `axes.rs` uses, and spec §5's H5
// predicted no more than two before this corpus existed.
/// Aphotic. Every community below the surface-breaching rung.
const L_DARK: f64 = 0.0;
/// The twilight of a rung that breaks the surface — a mouth, a shaft, a
/// skylight, a slot.
const L_DIM: f64 = 0.2;

/// The five habitation rungs of terrain's delve ladder, **mirrored as a
/// roster** (decision 0094): the names and their shallow-to-deep order are
/// shared, the derivation is not.
///
/// `hornvale_terrain::delve::DelveRung` computes which rung a ΔT lands in;
/// nothing here duplicates that, and nothing here may. What this enum is for
/// is stating *at what depth class a community occurs*, which is the axis the
/// energy inversion is measured against.
///
/// **One deliberate roster difference.** `DelveRung` also carries a `Surface`
/// variant, because the overworld is a rung of the same ladder (spec §4.6).
/// This enum has none: no underworld community is at the surface, and a
/// variant no row could ever take would be a hole for a later reader to fall
/// into. `cli/tests/delve_roster_mirror.rs` encodes exactly that expectation,
/// so the difference is asserted rather than assumed.
///
/// The derived `Ord` is load-bearing: "deeper than" is a comparison here, the
/// same way it is on `DelveRung`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DelveZone {
    /// Cave mouths and the first few tens of metres of worked rock.
    Undercroft,
    /// Shallow inhabited depth: the top of the karst and lava-tube population.
    Shallows,
    /// The ladder's broad middle — worked or walked, still temperate.
    Deeps,
    /// Deep habitation, warm enough that living here is a choice with a cost.
    Underdeep,
    /// Past the habitable ceiling: hot, and the deepest a cave reaches.
    ///
    /// **Named `Sunless` until The Stope** (spec amendment B.3). The mirror
    /// rule (decision 0094) is what makes the rename land in both crates at
    /// once: `hornvale_terrain::DelveRung::Nadir` moved and this roster moves
    /// with it, or `cli/tests/delve_roster_mirror.rs` reddens.
    Nadir,
}

/// One underworld community and the vector assigned to it.
///
/// Deliberately `axes::AssignedName`'s three fields plus [`UnderworldName::zone`].
/// It is not that type because the energy inversion is a claim *about depth*,
/// and a claim about depth needs the depth on the row: a parallel
/// name-to-zone table is precisely the silently-drifting duplicate decision
/// 0094 exists to prevent.
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: genera)
#[derive(Debug, Clone, PartialEq)]
pub struct UnderworldName {
    /// The community's registry spelling, kebab-case.
    pub name: &'static str,
    /// Its position in the basis. Unassigned means the name resisted.
    pub vector: EnvironmentVector,
    /// The cave formation(s) from [`crate::axes`] this community is a
    /// community *of* — never empty, because a community with no genus could
    /// not be scored against the cave region at all.
    pub genera: &'static [&'static str],
    /// The delve zone it occurs in.
    pub zone: DelveZone,
}

/// The three cave formations in [`crate::axes`], as genus pointers.
const KARST: &[&str] = &["karst-cave"];
const TUBE: &[&str] = &["lava-tube"];
const FRACTURE: &[&str] = &["fracture-cave"];
/// A collapse belongs to both the dissolved and the fractured underworld.
const KARST_AND_FRACTURE: &[&str] = &["karst-cave", "fracture-cave"];

fn v(values: &[(EnvironmentAxis, f64)]) -> EnvironmentVector {
    EnvironmentVector::new(values).expect("authored axis values are within [0, 1]")
}

/// A fully-assigned community: all five occupied axes.
#[allow(clippy::too_many_arguments)]
fn c(
    name: &'static str,
    genera: &'static [&'static str],
    zone: DelveZone,
    physiognomy: f64,
    energy: f64,
    water: f64,
    substrate: f64,
    light: f64,
) -> UnderworldName {
    UnderworldName {
        name,
        vector: v(&[
            (PHYSIOGNOMY, physiognomy),
            (ENERGY, energy),
            (WATER, water),
            (SUBSTRATE, substrate),
            (LIGHT, light),
        ]),
        genera,
        zone,
    }
}

/// A name the axes cannot place. Not an error — a counted finding, exactly as
/// in [`crate::axes::assignment`].
fn resists(name: &'static str, genera: &'static [&'static str], zone: DelveZone) -> UnderworldName {
    UnderworldName {
        name,
        vector: EnvironmentVector::new(&[]).expect("the empty vector is legal"),
        genera,
        zone,
    }
}

/// The authored corpus.
///
/// Read the comment above each row as the derivation: it names, per axis, the
/// terrain- or climate-owned field that drove the value. Nothing here is
/// computed — the whole table is AUTHORED, and the comments are what make it
/// auditable rather than tasteful.
fn build() -> Vec<UnderworldName> {
    use DelveZone::*;
    vec![
        // ================= karst: carbonate dissolution =================
        // Requires `MaterialBuffer::carbonate` high; the wettest and most
        // structurally elaborate of the three processes, and the only one
        // whose reach is not clamped near the surface.

        // PHYSIOGNOMY: a mouth keeps rooted, mossy growth on entrance rubble
        //   — broken floor, no standing cave structure yet.
        // ENERGY: light spill plus the heaviest litter fall in the corpus
        //   short of a sinking channel — detrital import at its source.
        // WATER: overhead `drainage` arrives and drains through; vadose.
        // SUBSTRATE: washed-in clastic fill, not rock.
        // LIGHT: the rung breaks the surface. One of only four rows above
        //   `L_DARK`, and the reason H5's bound is 2 rather than 1.
        c(
            "cave-mouth",
            KARST,
            Undercroft,
            P_BROKEN,
            E_RICH,
            W_MESIC,
            S_SOIL,
            L_DIM,
        ),
        // PHYSIOGNOMY: a vertical shaft is scoured wall and almost no floor.
        // ENERGY: the corpus maximum. A `ChannelNetwork` sink delivers a whole
        //   catchment's organic load through one opening.
        // WATER: an active sinking stream.
        // SUBSTRATE: scoured limestone, swept clean of fill.
        // LIGHT: a doline is open to the sky.
        c(
            "sinkhole-shaft",
            KARST,
            Undercroft,
            P_CRUST,
            E_TEEMING,
            W_WET,
            S_ROCK,
            L_DIM,
        ),
        // PHYSIOGNOMY: the classic rounded, decorated dissolution gallery.
        // ENERGY: the sink's load, attenuated by distance from the sink.
        // WATER: an active vadose watercourse.
        // SUBSTRATE: scoured carbonate rock.
        // LIGHT: aphotic — every row from here down.
        c(
            "stream-gallery",
            KARST,
            Shallows,
            P_STANDS,
            E_FED,
            W_WET,
            S_ROCK,
            L_DARK,
        ),
        // PHYSIOGNOMY: the corpus maximum. An abandoned decorated gallery is
        //   columns and curtains — the most surface area any void here has.
        // ENERGY: seepage only; the stream that made it has gone elsewhere.
        // WATER: `porosity`-borne seepage, no watercourse.
        // SUBSTRATE: flowstone over carbonate rock.
        c(
            "flowstone-hall",
            KARST,
            Shallows,
            P_GALLERY,
            E_LEAN,
            W_SEMI,
            S_ROCK,
            L_DARK,
        ),
        // PHYSIOGNOMY: a phreatic tube is smooth-walled with a thin biofilm.
        // ENERGY: the corpus MINIMUM among wet rows, and the trough the
        //   inversion turns on — below detrital reach, above the depth where
        //   the geothermal gradient pays.
        // WATER: below the water table (Task 2's `is_phreatic`); flooded.
        // SUBSTRATE: dissolution-scoured carbonate.
        c(
            "sump-gallery",
            KARST,
            Deeps,
            P_CRUST,
            E_INERT,
            W_SAT,
            S_ROCK,
            L_DARK,
        ),
        // PHYSIOGNOMY: a fill-choked tube has no void structure left at all.
        // ENERGY: buried organics in the clastic fill; lean but not nothing.
        // WATER: phreatic.
        // SUBSTRATE: the fill itself — `porosity`-high clastic mud.
        c(
            "mud-sump", KARST, Deeps, P_SMOOTH, E_LEAN, W_SAT, S_SOIL, L_DARK,
        ),
        // PHYSIOGNOMY: halite-walled and salt-crusted; no standing structure.
        // ENERGY: halophilic chemolithotrophy on the brine chemocline — the
        //   first row where the energy base is chemical rather than imported.
        // WATER: `WaterKind::SaltBasin` overhead means brine, not fresh.
        // SUBSTRATE: `RockClass::Evaporite`.
        c(
            "brine-chamber",
            KARST,
            Deeps,
            P_SMOOTH,
            E_FED,
            W_SAT,
            S_EVAPORITE,
            L_DARK,
        ),
        // PHYSIOGNOMY: hypogene karst grows gypsum crusts and curtains.
        // ENERGY: sulphide oxidation driven from below by the geothermal
        //   gradient — chemolithotrophy proper, and richer than anything at
        //   `Deeps`.
        // WATER: condensation-fed above the table, not flooded.
        // SUBSTRATE: replacement gypsum, so evaporite rather than carbonate.
        c(
            "sulphuric-hall",
            KARST,
            Underdeep,
            P_STANDS,
            E_RICH,
            W_MESIC,
            S_EVAPORITE,
            L_DARK,
        ),
        // PHYSIOGNOMY: at the reach ceiling the void is crusted, not decorated.
        // ENERGY: the corpus maximum again, now from the other end of the
        //   mechanism — full chemolithotrophy on a hot gradient. Deliberately
        //   the same value as `sinkhole-shaft`: the deep is not poorer, it is
        //   differently powered (spec §4.4).
        // WATER: below any water table at this depth; hot mineralised water.
        // SUBSTRATE: carbonate rock.
        c(
            "deep-karst-void",
            KARST,
            Nadir,
            P_CRUST,
            E_TEEMING,
            W_SAT,
            S_ROCK,
            L_DARK,
        ),
        // ================= lava tube: a drained basaltic pipe ==============
        // `LAVATUBE_CEILING_M = 200.0` clamps a lava tube's reach, so this
        // genus occupies `Undercroft` and `Shallows` and CANNOT occur deeper.
        // That absence is derived, not an authoring gap.

        // PHYSIOGNOMY: a skylight collapse leaves a scoria cone, no more.
        // ENERGY: litter falls straight through the skylight.
        // WATER: basalt — `carbonate` 0, `porosity` low; free-draining.
        // SUBSTRATE: scoria grit, a loose granular floor.
        // LIGHT: a skylight is open to the sky.
        c(
            "tube-mouth",
            TUBE,
            Undercroft,
            P_CRUST,
            E_RICH,
            W_ARID,
            S_SAND,
            L_DIM,
        ),
        // PHYSIOGNOMY: the corpus's smoothest form — a glazed intact pipe.
        // ENERGY: only what creeps in from the nearest skylight.
        // WATER: the corpus's driest row. No dissolution, low `porosity`, and
        //   no water table intersection at this depth.
        // SUBSTRATE: bare basalt.
        c(
            "smooth-tube",
            TUBE,
            Shallows,
            P_SMOOTH,
            E_LEAN,
            W_NONE,
            S_ROCK,
            L_DARK,
        ),
        // PHYSIOGNOMY: roof fall leaves a blocky floor under an intact ceiling.
        // ENERGY: an internal breach between tube levels admits import from
        //   the level above; more than the intact pipe, less than a skylight.
        // WATER: free-draining basalt.
        // SUBSTRATE: fallen basalt blocks.
        c(
            "collapse-section",
            TUBE,
            Shallows,
            P_BROKEN,
            E_FED,
            W_ARID,
            S_ROCK,
            L_DARK,
        ),
        // PHYSIOGNOMY: an ice floor is a smooth, sealed form.
        // ENERGY: sealed and frozen; nothing arrives and nothing reacts.
        // WATER: `W_NONE`, matching `axes.rs`'s `ice` — frozen water is not
        //   available water, and reading it as saturated would put an ice cave
        //   next to a sump.
        // SUBSTRATE: ice. Needs a cold surface cell, which is climate's own
        //   input rather than terrain's — the one row driven from this side.
        // ZONE: a cold trap is a downward pocket near the surface.
        c(
            "tube-ice-trap",
            TUBE,
            Undercroft,
            P_SMOOTH,
            E_INERT,
            W_NONE,
            S_ICE,
            L_DARK,
        ),
        // ================= fracture: fault-controlled voids ================
        // Angular, fault-aligned, and the genus that reaches deepest in
        // indurated rock — `induration` and `metamorphic_grade` high.

        // PHYSIOGNOMY: an open fissure is a crusted slot, not a gallery.
        // ENERGY: litter and seepage down a surface-breaching crack.
        // WATER: fracture porosity carries seepage without holding it.
        // SUBSTRATE: bare indurated rock.
        // LIGHT: the slot reaches daylight.
        c(
            "fault-slot",
            FRACTURE,
            Undercroft,
            P_CRUST,
            E_FED,
            W_SEMI,
            S_ROCK,
            L_DIM,
        ),
        // PHYSIOGNOMY: an angular fault-aligned network; crusts, no structure.
        // ENERGY: seepage-borne only, one remove from the surface.
        // WATER: fracture seepage.
        // SUBSTRATE: bare indurated rock.
        c(
            "fissure-labyrinth",
            FRACTURE,
            Shallows,
            P_CRUST,
            E_LEAN,
            W_SEMI,
            S_ROCK,
            L_DARK,
        ),
        // PHYSIOGNOMY: interstitial voids in a boulder pile — a jumbled floor.
        // ENERGY: high. Litter falls between the blocks with nothing in the
        //   way; this is the shallow subterranean habitat at its richest.
        // WATER: a boulder pile is the most free-draining ground there is.
        // SUBSTRATE: rock, block on block.
        c(
            "talus-void",
            FRACTURE,
            Undercroft,
            P_BROKEN,
            E_RICH,
            W_ARID,
            S_ROCK,
            L_DARK,
        ),
        // The corpus's null community, and the "barren gneiss" spec §4.4 names.
        // PHYSIOGNOMY: nothing grows and nothing has fallen in.
        // ENERGY: `metamorphic_grade` ~1 means no reactive sulphide and no
        //   buried carbon; `porosity` ~0 means no fluid. Zero on both halves
        //   of the mechanism at once.
        // WATER: none — the pore space is gone.
        // SUBSTRATE: bare rock.
        c(
            "gneiss-void",
            FRACTURE,
            Deeps,
            P_SMOOTH,
            E_INERT,
            W_NONE,
            S_ROCK,
            L_DARK,
        ),
        // The contrast partner spec §4.4 asks for, and the place where its
        // sentence is only three-quarters right — see the module note in the
        // task report. `deposit_at` returns a hydrothermal `Copper`/`Gold`/
        // `LeadZinc` body here.
        // PHYSIOGNOMY: sulphide and gangue crusts on the vein walls.
        // ENERGY: sulphide oxidation is a genuine chemolithotroph base, at a
        //   depth where nothing arrives from above. This is where an ore body
        //   actually separates from barren gneiss.
        // WATER: a vein is a fossil fluid conduit; elevated `porosity`.
        // SUBSTRATE: rock. It does NOT separate from gneiss here — a metallic
        //   ore body in a silicate host is still, nominally, rock.
        c(
            "ore-vein-chamber",
            FRACTURE,
            Deeps,
            P_CRUST,
            E_RICH,
            W_MESIC,
            S_ROCK,
            L_DARK,
        ),
        // `deposit_at` returns `Commodity::Coal` — biogenic sediment, and the
        // one commodity that DOES move the nominal substrate class.
        // PHYSIOGNOMY: crusted; a worked or collapsed seam void.
        // ENERGY: buried organic carbon is a fossil detrital store, fermentable
        //   long after the forest that made it. A *third* energy base, and the
        //   reason the mechanism is not cleanly two-valued.
        // WATER: shale and coal hold water; seam voids are damp.
        // SUBSTRATE: organic.
        c(
            "coal-measure-void",
            FRACTURE,
            Deeps,
            P_CRUST,
            E_FED,
            W_MESIC,
            S_ORGANIC,
            L_DARK,
        ),
        // PHYSIOGNOMY: mineral mats and sinter on a broken floor.
        // ENERGY: the corpus maximum. A fault carrying hot mineralised water
        //   up the geothermal gradient is the richest chemolithotroph setting
        //   the model has.
        // WATER: an active hydrothermal watercourse.
        // SUBSTRATE: rock, sinter-coated.
        c(
            "thermal-fissure",
            FRACTURE,
            Underdeep,
            P_BROKEN,
            E_TEEMING,
            W_WET,
            S_ROCK,
            L_DARK,
        ),
        // PHYSIOGNOMY: smooth and thin — the aperture is closing.
        // ENERGY: radiolytic and water-rock chemistry only. Real but meagre;
        //   this row is why `Nadir` does not simply out-score `Underdeep`.
        // WATER: fractures close under lithostatic load and squeeze fluid out.
        // SUBSTRATE: rock.
        c(
            "deep-fracture-void",
            FRACTURE,
            Nadir,
            P_SMOOTH,
            E_FED,
            W_ARID,
            S_ROCK,
            L_DARK,
        ),
        // PHYSIOGNOMY: a basalt-hosted fracture void, crusted.
        // ENERGY: H2 from water-basalt reaction feeds a lithoautotrophic base
        //   with no photosynthetic input anywhere in its history.
        // WATER: low `porosity` limits what the fracture can hold.
        // SUBSTRATE: rock. Filed under fracture, not tube: `LAVATUBE_CEILING_M`
        //   forbids a lava tube at this depth, so a deep void in basalt is a
        //   fracture that happens to be in basalt.
        c(
            "basalt-fissure-void",
            FRACTURE,
            Nadir,
            P_SMOOTH,
            E_RICH,
            W_SEMI,
            S_ROCK,
            L_DARK,
        ),
        // ================= the names the axes cannot place =================
        // NOT preregistered. Spec §5 froze a prediction about `LIGHT`, not
        // about resisters, so these two are a finding this task reports rather
        // than a prediction it confirms.
        //
        // Both meet The Axes' own operational definition of resistance —
        // assignment would require an axis no other name uses — and both fail
        // on the same axis for the same reason its ten resisters did:
        // `DISTURBANCE` is the basis's only `Rate`, a whole-community name is
        // a state, and *a phase is not a point in a state space*. The Axes'
        // A-4 records `DISTURBANCE` as a declared axis nothing can occupy yet.
        //
        // A collapse: the underworld's `burn`. What distinguishes it is the
        // rockfall return frequency, which is a rate.
        resists("breakdown-fall", KARST_AND_FRACTURE, Shallows),
        // A seasonal flood pulse through a vadose passage. Its identity is how
        // often the passage floods, not how wet it is between floods — again a
        // rate, and the reason `stream-gallery` can be placed and this cannot.
        resists("flood-pulse", KARST, Shallows),
    ]
}

/// The full assignment over the underworld's 24-name corpus: 22 communities
/// placed in the basis, and 2 the axes could not place.
pub fn underworld_assignment() -> &'static [UnderworldName] {
    static A: OnceLock<Vec<UnderworldName>> = OnceLock::new();
    A.get_or_init(build)
}
