//! The material buffer (The Ground, spec §2): a per-vertex petrogenetic
//! property vector and the projections over it. Pure functions of existing
//! terrain fields — no new draws, no new stream labels.
//!
//! `RockClass` declares nineteen variants (spec §4), but `classify_rock`'s
//! current tectonic ranges do not make every one reachable — some are
//! intentional (or at least currently accepted) headroom for a later terrain
//! epoch rather than a gap. Surveyed across seeds 0..15 at `Geosphere::new(5)`
//! (and cross-checked at `Geosphere::new(4)`), and re-surveyed for the
//! Alluvium/Coal recalibration across seeds `[1, 7, 42, 99]` at the canonical
//! `Geosphere::new(6)`, sixteen classes are realized and three are not:
//!
//! - **Realized today**: Granite, Basalt, Andesite, Rhyolite, Sandstone,
//!   Shale, Conglomerate, Evaporite, Ironstone, ReefLimestone, Slate, Schist,
//!   Gneiss, Marble, Coal, Alluvium. `Coal` and `Alluvium` were recalibrated
//!   against the buffer's actual ranges rather than imagined absolutes: see
//!   [`ALLUVIUM_DRAINAGE_MIN`] (drainage maxes out around 338 at level 6, not
//!   a round number like 1000 — the gate now sits just above the observed
//!   p90) and [`COAL_SOIL_DEPTH_MIN`]/[`COAL_GRAIN_MAX`] (waterlogged
//!   high-soil-depth lowland, not an absolute grain floor below the
//!   continental formula's own minimum).
//! - **Reserved, deferred to Sculpting (v3)**: `Gabbro` — this buffer has no
//!   intrusive-vs-extrusive (exhumation/depth-of-crystallization) axis yet,
//!   so mafic continental vertices always read as extrusive Basalt/Andesite
//!   rather than plutonic Gabbro; that axis is Sculpting's to add. `Chert` —
//!   gated on abyssal very-low-porosity ocean vertices, a niche combination the
//!   current ocean-floor ranges rarely produce. `Quartzite` — gated on
//!   `induration > 0.7` inside the low-metamorphic-grade band, which the
//!   current induration formula rarely reaches; widening the induration
//!   range would make it reachable without a new axis.

use crate::boundaries::BoundaryKind;
use crate::elevation::TrailSeamount;
use crate::globe::TectonicGlobe;
use crate::pins::Metaphysics;
use crate::plates::{Plate, dot, normalize, sub, velocity_at};
use hornvale_kernel::color::{Mixture, Reflectance};
use hornvale_kernel::{Fbm, Geosphere, Vertex, VertexMap, math};

/// Regolith thickness in metres.
/// type-audit: newtype
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SoilDepth(f64);

impl SoilDepth {
    /// Wrap a non-negative, finite depth (metres).
    /// type-audit: bare-ok(constructor-edge)
    pub fn new(m: f64) -> SoilDepth {
        debug_assert!(m.is_finite() && m >= 0.0);
        SoilDepth(m.max(0.0))
    }
    /// The depth in metres.
    /// type-audit: bare-ok(constructor-edge)
    pub fn get(&self) -> f64 {
        self.0
    }
}

/// The rock beneath the surface cover — the shallow 2-layer column
/// (spec §2, round 4). Derived from crust thickness; serves deep mining,
/// well depth, and the underdark vertical axis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Basement {
    /// Continental basement: granite/gneiss.
    Continental,
    /// Oceanic floor: gabbro/basalt.
    Oceanic,
}

/// Continental-margin polarity relative to plate motion (spec §2, round 3).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MarginPolarity {
    /// Leading edge of plate motion: arcs, narrow shelf.
    Active,
    /// Trailing edge: wide shelf, thick sediment.
    Passive,
    /// Continental interior, far from a leading/trailing edge.
    Interior,
    /// Oceanic crust (below the continental threshold).
    Oceanic,
}

/// A per-vertex petrogenetic property vector — the material buffer.
/// type-audit: bare-ok(ratio: silica), bare-ok(ratio: grain), bare-ok(ratio: induration), bare-ok(ratio: carbonate), bare-ok(ratio: metamorphic_grade), bare-ok(ratio: porosity), bare-ok(ratio: thaumic)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MaterialBuffer {
    /// Felsic index, `[0,1]`: 0 mafic (basalt/gabbro) → 1 felsic (granite).
    pub silica: f64,
    /// Grain/crystallinity, `[0,1]`: 0 fine (volcanic/clay) → 1 coarse (plutonic).
    pub grain: f64,
    /// Induration/hardness, `[0,1]`: 0 soft (shale/soil) → 1 hard (quartzite).
    pub induration: f64,
    /// Carbonate content, `[0,1]`.
    pub carbonate: f64,
    /// Metamorphic grade, `[0,1]`: 0 unaltered → 1 gneiss.
    pub metamorphic_grade: f64,
    /// Porosity/permeability, `[0,1]`.
    pub porosity: f64,
    /// Continental-margin polarity.
    pub margin: MarginPolarity,
    /// Regolith thickness.
    pub soil_depth: SoilDepth,
    /// The rock beneath the cover — shallow 2-layer column.
    pub basement: Basement,
    /// Thaumic saturation. Reserved (spec §2/§8); identically 0 in the
    /// metaphysically-inert tier this campaign builds.
    pub thaumic: f64,
}

/// Metamorphic grade rises within this many graph hops of a boundary.
///
/// **This is a LENGTH now, not a hop count** (The Trencher, Task 13). It was
/// read as an integer hop ladder — `1 - hops/OROGEN_REACH` — which made
/// `metamorphic_grade` take exactly five values (`{0, .25, .5, .75, 1}`),
/// 45.8% of them zero on the population the underworld's metabolites read.
/// A hop count is a *quantizer*, not a physical fact: the aureole around a
/// collision has a width, and the grid's rounding of that width to whole
/// graph steps was the only reason the axis was discrete. It is now converted
/// to a distance — `OROGEN_REACH * mean_edge_chord(geo)`, see
/// [`orogen_reach_chord`] — so the aureole keeps exactly the reach it has
/// always had (the value is unchanged, and its reach still scales with mesh
/// resolution the way a hop-defined one did) while the grade *inside* it
/// varies continuously with true distance to the boundary.
/// plumb: pending(wave-1)
const OROGEN_REACH: u32 = 4;

/// Peak metamorphic grade contributed by the orogenic aureole itself, at a
/// boundary vertex (proximity 1), before the burial background is added.
///
/// Below 1.0 so that the two additive terms of [`metamorphic_grade_at`] sum
/// to the axis ceiling rather than either one saturating it alone:
/// `0.80 + 0.20 * crust_age` reaches `0.998` on the oldest measured crust
/// (`crust_age` max 0.992 over land, 6 seeds at level 6), so a gneiss-grade
/// reading is the *conjunction* of an orogen and an old craton — which is
/// what the highest metamorphic grades geologically are.
/// plumb: pending(wave-1)
const OROGENIC_AUREOLE_PEAK: f64 = 0.80;

/// Metamorphic grade contributed by burial alone, per unit `crust_age` —
/// the broad, weak overprint every continental basin accumulates as it
/// subsides, independent of any orogen.
///
/// **This term exists because the axis had a 45.8% point mass at exactly
/// zero**, and zero is not a true statement about buried continental rock:
/// burial diagenesis grades continuously into anchimetamorphism with no
/// sharp onset. Modelling only contact/regional metamorphism asserted that
/// every vertex more than four hops from a boundary is petrologically
/// pristine.
///
/// **Its size is not free — it is bounded by the thresholds this axis is
/// read at.** The lowest is [`classify_rock`]'s `>= 0.25` (Slate); with
/// `crust_age` capped at 1.0, any gain `>= 0.25` would let age *alone* cross
/// that band and silently delete the orogenic clause from every consumer
/// downstream of it — measured directly: at gain `0.30` the `>= 0.25` share
/// of land jumps from 0.320 to 0.447 while the `> 0.3` share barely moves,
/// because the background has swallowed the Slate band whole. `0.20` is the
/// largest round value strictly under that bound (0.20 × 0.992 = 0.198, a
/// fifth below the band floor), so it buys the most spread available without
/// making any consumer's metamorphic test redundant with its age test.
/// `features.rs`'s exhumed-BIF gate (which ANDs `crust_age > 0.75` with
/// `metamorphic_grade > 0.3`) is the sharpest case: at `0.20` it still
/// requires real orogenic proximity, and stays the three-clause conjunction
/// it was written as.
/// plumb: pending(wave-1)
const BURIAL_OVERPRINT_GAIN: f64 = 0.20;

/// Carbonate content of rock outside the shelf factory — oceanic floor, and
/// the continental limit as the platform dies out. Not zero: pelagic and
/// detrital carbonate is everywhere in trace amounts. Unchanged in value
/// from the pre-Trencher two-valued `carbonate_at`, which returned exactly
/// this on 87.4% of land.
/// plumb: pending(wave-1)
const CARBONATE_FLOOR: f64 = 0.05;

/// Carbonate content of an ideal platform rock: equatorial, sitting on crust
/// exactly at the continental threshold. Unchanged in value from the
/// pre-Trencher `carbonate_at`'s high branch, deliberately — the axis is read
/// at `> 0.4` and `> 0.5` gates ([`KARST_MIN_POROSITY`]'s neighbours in
/// [`classify_rock`], [`hydrogeology`], and `features.rs`'s carbonate-hosted
/// lead-zinc), so holding the ceiling fixed is what lets the axis be widened
/// underneath it without moving the populations those gates select.
/// plumb: pending(wave-1)
const CARBONATE_SHELF_PEAK: f64 = 0.7;

/// Crustal thickening, in km above [`crate::crust::CONTINENTAL_THRESHOLD_KM`],
/// over which the carbonate platform fades to [`CARBONATE_FLOOR`].
///
/// The physical story the old `thickness < threshold + 6.0` test told badly:
/// a carbonate factory needs accommodation space and clear water. Crust at
/// the continental threshold is stretched shelf crust standing at or just
/// below sea level — an epeiric sea, the platform's home. Thick crust stands
/// high, sheds siliciclastic mud, and smothers the factory. `20.0` puts the
/// platform dead at **40 km — where the crust has doubled**, which is the
/// real transition from stretched shelf (~20-25 km on Earth) to full
/// cratonic/orogenic thickness (~35-45 km); measured land thickness here runs
/// p50 29.8, p90 37.4, max 44.6 km over 6 seeds at level 6, so the factory
/// dies on the thickest few percent of land and nowhere else.
///
/// **Measured against the criterion that governs a widening**: the existing
/// `carbonate > 0.5` gate selected 12.59% of land before and selects 13.69%
/// after, while the point mass at [`CARBONATE_FLOOR`] falls from 87.4% of
/// land to 4.4% and the realized-value count goes from 2 to ~87,000. The
/// neighbouring reaches were measured too (16 km → 10.61% over the gate,
/// 18 → 12.13%, 22 → 15.32%); 20 km is the one whose gate population is
/// closest to unchanged while leaving the smallest residue at the floor.
/// plumb: pending(wave-1)
const CARBONATE_SHELF_REACH_KM: f64 = 20.0;

/// Drainage (flow-accumulation, upstream land-vertex count) above which a land
/// vertex reads as `Alluvium` rather than its ordinary clastic/igneous class.
/// Calibrated against the real distribution, not an imagined absolute: a
/// 4-seed (`[1, 7, 42, 99]`) survey at the canonical `Geosphere::new(6)`
/// found land drainage maxing out at 338 (p99 47, p95 18, p90 12, p50 3) —
/// nowhere near a round number like 1000. `20.0` sits just above p90 and
/// selects roughly the top 4-5% of land vertices by accumulation: genuinely
/// high-flow valley bottoms, not merely-damp lowland.
/// plumb: pending(wave-1)
const ALLUVIUM_DRAINAGE_MIN: f64 = 20.0;

/// Carve-deposited sediment thickness (metres) above which a land vertex
/// reads as `Alluvium` regardless of drainage (spec §2 stage 8, Sculpting
/// Task 10): a vertex the carve's routing/wedge/delta buried under real
/// alluvium is alluvial even when its flow accumulation alone would not
/// clear [`ALLUVIUM_DRAINAGE_MIN`] — a floodplain a river no longer
/// actively occupies still reads as its deposit.
/// plumb: pending(wave-1)
const ALLUVIUM_SEDIMENT_MIN_M: f64 = 2.0;

/// Regolith depth (metres) above which waterlogged lowland accumulation
/// reads as `Coal` rather than ordinary clastic sediment, paired with
/// [`COAL_GRAIN_MAX`] so only the finer end of the grain range qualifies
/// (coal-forming peat accumulates in fine floodplain/deltaic muck, not
/// coarse gravel). A 4-seed survey at `Geosphere::new(6)` found roughly
/// 0.6% of clastic-eligible land vertices clear both this and the grain gate —
/// present but appropriately rare for a biogenic, waterlogged-basin rock.
/// plumb: pending(wave-1)
const COAL_SOIL_DEPTH_MIN: f64 = 1.25;

/// Grain ceiling paired with [`COAL_SOIL_DEPTH_MIN`]: excludes the coarse
/// (near-`Conglomerate`) tail of the clastic range from `Coal`, keeping the
/// two rocks' inputs (a slow, fine-sediment sink vs. relief-proximal debris)
/// distinct even though both draw from the same `grain <= 0.6` band.
/// plumb: pending(wave-1)
const COAL_GRAIN_MAX: f64 = 0.55;

/// The fine rock taxonomy (spec §4), a projection over the buffer.
///
/// `Ord`/`PartialOrd` (declaration order below) exist only so callers can
/// collect distinct classes into a `BTreeSet` (the project bans
/// `HashSet`) — there is no meaningful ranking between rock classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RockClass {
    /// Felsic intrusive — craton cores, collision roots.
    Granite,
    /// Mafic intrusive — deep oceanic/rift.
    Gabbro,
    /// Mafic extrusive — ridges, hotspots, ocean floor.
    Basalt,
    /// Intermediate extrusive — subduction arcs.
    Andesite,
    /// Felsic extrusive — continental arc/caldera.
    Rhyolite,
    /// Clastic — near-orogen lowland, coast.
    Sandstone,
    /// Clastic — quiet deep basin.
    Shale,
    /// Clastic — proximal to uplift.
    Conglomerate,
    /// Chemical precipitate — arid endorheic basin.
    Evaporite,
    /// Chemical precipitate — abyssal pelagic.
    Chert,
    /// Chemical precipitate that is also iron ore (round 2 bridge).
    Ironstone,
    /// Biogenic — warm shallow shelf.
    ReefLimestone,
    /// Biogenic — waterlogged organic (round 2).
    Coal,
    /// Metamorphic, low grade.
    Slate,
    /// Metamorphic, medium grade.
    Schist,
    /// Metamorphic, high grade — collision core.
    Gneiss,
    /// Metamorphic carbonate.
    Marble,
    /// Metamorphic sandstone.
    Quartzite,
    /// Unconsolidated river silt — fertile floodplains.
    Alluvium,
}

/// Project the buffer (plus grid context) onto a rock class (spec §4).
/// `sediment_m` is the carve's deposited sediment thickness at the vertex
/// (Sculpting Task 10): a vertex the carve buried under real alluvium reads
/// as `Alluvium` even below the ordinary drainage gate.
/// type-audit: bare-ok(count: drainage), bare-ok(flag: endorheic), bare-ok(flag: ocean), bare-ok(ratio: sediment_m)
pub fn classify_rock(
    buf: &MaterialBuffer,
    drainage: f64,
    endorheic: bool,
    ocean: bool,
    sediment_m: f64,
) -> RockClass {
    // Metamorphics: graded, carbonate parent -> marble.
    if buf.metamorphic_grade >= 0.75 {
        return RockClass::Gneiss;
    }
    if buf.metamorphic_grade >= 0.5 {
        return if buf.carbonate > 0.5 {
            RockClass::Marble
        } else {
            RockClass::Schist
        };
    }
    if buf.metamorphic_grade >= 0.25 {
        return if buf.induration > 0.7 {
            RockClass::Quartzite
        } else {
            RockClass::Slate
        };
    }
    if ocean {
        // Abyssal siliceous ooze vs ridge/floor basalt vs BIF.
        if buf.porosity < 0.2 && buf.silica > 0.5 {
            return RockClass::Chert;
        }
        if buf.carbonate > 0.4 {
            return RockClass::Ironstone; // BIF forms in shelf-adjacent anoxic water
        }
        return RockClass::Basalt;
    }
    // Land, unmetamorphosed.
    if endorheic {
        return RockClass::Evaporite;
    }
    if drainage >= ALLUVIUM_DRAINAGE_MIN || sediment_m > ALLUVIUM_SEDIMENT_MIN_M {
        return RockClass::Alluvium;
    }
    if buf.carbonate > 0.5 {
        return RockClass::ReefLimestone;
    }
    // Arc magmatism (active margin) is checked before the plutonic arm below:
    // arc vertices are extrusive/intermediate by genesis, so they resolve to
    // Andesite/Rhyolite even when locally coarse and hard, never Granite/Gabbro
    // (those stay the stable-continental-interior read).
    if matches!(buf.margin, MarginPolarity::Active) {
        return if buf.silica > 0.55 {
            RockClass::Rhyolite
        } else {
            RockClass::Andesite
        };
    }
    // Igneous by silica/grain when hard & crystalline; else clastic.
    if buf.grain > 0.5 && buf.induration > 0.5 {
        return if buf.silica > 0.55 {
            RockClass::Granite
        } else {
            RockClass::Gabbro
        };
    }
    // Clastic sediments by relief proxy (grain), with waterlogged fine
    // lowland (deep soil, finer-than-conglomerate grain) diverted to Coal
    // before the ordinary Sandstone/Shale split.
    if buf.grain > 0.6 {
        RockClass::Conglomerate
    } else if buf.soil_depth.get() > COAL_SOIL_DEPTH_MIN && buf.grain < COAL_GRAIN_MAX {
        RockClass::Coal
    } else if buf.grain > 0.35 {
        RockClass::Sandstone
    } else {
        RockClass::Shale
    }
}

/// Hydrogeologic behavior (spec §3, round 2 rock×water).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Hydro {
    /// Porous, holds water: wells, oases.
    Aquifer,
    /// Impermeable: perched water, seeps.
    Aquitard,
    /// Where an aquifer meets the surface with flow. Never produced by
    /// [`hydrogeology`] itself (The Witness, Task 5b) — `hydrogeology` is a
    /// pointwise matrix-petrophysics read and a spring is not a property of
    /// a single vertex's rock, it is a property of a *contact*: water flowing
    /// over from an `Aquifer` vertex was the shipped model at F5, but land
    /// drainage at production resolution (L6) maxes at 219 against the old
    /// 500 threshold, so that gate was unreachable regardless. `Spring` is
    /// promoted from `Aquifer` by `GeneratedTerrain::hydro_at` (decision
    /// 0085's precedent: the pointwise petrophysics is the durable signal,
    /// the geometric promotion is derived from it), when some neighbouring
    /// vertex is not itself an `Aquifer` and sits lower — the descending
    /// contact a spring geologically is.
    Spring,
    /// Sheds water: thin-soil runoff.
    Runoff,
    /// Dissolving carbonate: caves, sinkholes.
    Karst,
}

impl Hydro {
    /// Every variant, so the reachability assertion
    /// (`windows/lab/tests/calibration.rs`'s
    /// `every_hydro_variant_is_reachable_somewhere_in_the_census`, The Assay
    /// Task 8 — formerly `domains/terrain/tests/hydro_witness.rs`'s 8-seed
    /// sweep, The Witness Task 6, retired once the census carried the same
    /// coverage over 1,000 worlds) derives its checklist from the type
    /// rather than from an author re-typing the enum's members by hand.
    /// `Hydro::Spring`/`Hydro::Aquifer` were unreachable from the real
    /// derivation on every seed for this model's entire life (F5) and no
    /// hand-built checklist would have caught that on its own — adding a
    /// variant here enrolls it in the guard automatically, which is the
    /// property a hand-maintained list cannot offer. `PartialOrd`/`Ord`
    /// (declaration order, derived above) exist only so the guard can
    /// collect witnessed variants into a `BTreeSet`/`BTreeMap` (the project
    /// bans `HashSet`/`HashMap`) — as with `RockClass`, there is no
    /// meaningful ranking between hydrogeologic classes.
    pub const ALL: [Hydro; 5] = [
        Hydro::Aquifer,
        Hydro::Aquitard,
        Hydro::Spring,
        Hydro::Runoff,
        Hydro::Karst,
    ];

    /// This variant's stable name, used by the census's
    /// `hydro-variant-coverage` column and by the reachability assertion in
    /// `windows/lab/tests/calibration.rs`.
    ///
    /// These strings are COLUMN CONTENT in a committed artifact, which makes
    /// them a save-format-adjacent contract: never rename one. If a VARIANT is
    /// ever renamed, keep the name it already emitted here — the enum's
    /// identifier and its census name are allowed to diverge, and the census's
    /// history is worth more than their agreeing.
    ///
    /// It lives on the type rather than in `windows/lab` so there is one
    /// definition and a new variant cannot be added without naming it.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn name(&self) -> &'static str {
        match self {
            Hydro::Aquifer => "aquifer",
            Hydro::Aquitard => "aquitard",
            Hydro::Spring => "spring",
            Hydro::Runoff => "runoff",
            Hydro::Karst => "karst",
        }
    }
}

/// Classify hydrogeology from porosity/carbonate (spec §3). Pointwise matrix
/// petrophysics only — `Aquifer`/`Aquitard`/`Runoff`/`Karst`, never
/// `Spring` (The Witness, Task 5b): `Spring` is a property of a contact
/// between vertices, not of one vertex's rock, and is promoted separately by
/// `GeneratedTerrain::hydro_at`. No longer takes a `drainage` argument —
/// its only use was the retired flowing-vs-still split below.
/// type-audit: bare-ok(flag: ocean)
pub fn hydrogeology(buf: &MaterialBuffer, ocean: bool) -> Hydro {
    if ocean {
        return Hydro::Aquitard;
    }
    if buf.carbonate > 0.5 && buf.porosity > KARST_MIN_POROSITY {
        return Hydro::Karst;
    }
    if buf.porosity < AQUITARD_MAX_POROSITY {
        return Hydro::Aquitard;
    }
    if buf.porosity > CLASTIC_AQUIFER_MIN_POROSITY {
        return Hydro::Aquifer;
    }
    Hydro::Runoff
}

/// Porosity above which carbonate rock (`carbonate > 0.5`) reads as `Karst`
/// rather than falling through to the branches below — the CARBONATE scale.
/// Measured against 8 seeds of continental land vertices (The Witness, F5): the
/// carbonate class ran `n=1095 min=0.350 p50=0.425 p75=0.575 max=0.650`, so
/// `0.4` sat just above the class floor and inside its normal range — most
/// carbonate vertices clear it. Unchanged by F5; that change only gave this
/// constant a name and its calibration record.
///
/// **Re-measured after The Trencher widened `carbonate` (Task 13), value
/// unchanged and the claim still holds.** The carbonate class now runs
/// `n=10348 min=0.278 p25=0.587 p50=0.658 p75=0.682 p95=0.711 max=0.744`
/// (level 6, 6 seeds) — higher and wider, because the axis feeding
/// `0.5 * carbonate` is continuous — and `0.4` selects **90.9%** of it.
/// "Most carbonate vertices clear it" was true at F5 and is more true now,
/// so this constant is left alone rather than re-placed: it is a floor, and
/// it is still doing the job its record claims.
/// plumb: pending(wave-1)
const KARST_MIN_POROSITY: f64 = 0.4;

/// Porosity below which any rock (carbonate or clastic) reads as
/// impermeable `Aquitard`. Measured against the same 8-seed sweep: the
/// clastic (non-carbonate) class runs `n=4666 min=0.025 p50=0.100
/// p75=0.250 p95=0.325 max=0.325`, quantised in ~0.075 steps (0.025, 0.100,
/// 0.175, 0.250, 0.325) — `0.15` fell between the two lowest bands, so it
/// selected roughly the bottom fifth to two-fifths of clastic vertices
/// (`0.025`, and about half of `0.100`) as `Aquitard`. Unchanged by F5.
///
/// **That share is now 2.17%, and this constant is deliberately NOT
/// re-placed** (The Trencher, Task 13). Clastic land porosity now runs
/// `min=0.074 p05=0.197 p25=0.374 p50=0.473 p95=0.614 max=0.645` (level 6,
/// 6 seeds), so the whole distribution has moved up and away from `0.15`;
/// `Aquitard` reads on 2.37% of land at seed 0 where the record above
/// describes a fifth to two-fifths of the clastic class. Two reasons to
/// record rather than retune. First, nothing authorized it: The Trencher's
/// Task 13 was authorized to re-place [`CLASTIC_AQUIFER_MIN_POROSITY`],
/// which a committed guard was about to fail on, and this one no guard
/// covers — moving it would be an unmeasured retune riding along with a
/// measured one. Second, an impermeable-rock class that is *rare* is not
/// obviously wrong; whether 2% or 20% of land should be aquitard is a
/// question about what the class is for, which wants its own measurement
/// against a consumer, not a number picked to restore a fraction from a
/// world that no longer exists.
/// plumb: pending(wave-1)
const AQUITARD_MAX_POROSITY: f64 = 0.15;

/// Porosity above which non-carbonate (clastic) rock reads as `Aquifer` —
/// the CLASTIC scale, distinct from [`KARST_MIN_POROSITY`]'s carbonate
/// scale. **Task 5's `0.25` was itself mismeasured** (The Witness, Task
/// 5b): it was pinned to a p75 measured at `Geosphere::new(4)` filtered by
/// `basement == Continental`, but the model runs at `Geosphere::new(6)` and
/// classifies on `elevation > sea_level`. Re-measured on the correct
/// population *before* the grain term existed, clastic land porosity was
/// one value end to end (`p25..=p95 == 0.325`, `n=52207`) because
/// `carbonate` is binary and `metamorphic_grade` is 0 outside an orogen —
/// no threshold could partition it, and `0.25` shipped **69.64% of land as
/// Aquifer**.
///
/// With [`GRAIN_POROSITY_GAIN`] added, clastic land porosity became a
/// continuous function of crust age spanning the band `[0.416, 0.494]`
/// (measured, `k_g = 0.40`, level 6, 4 seeds), and The Witness placed `0.46`
/// at 56% of it — mid-band, explicitly rejecting a tidier-looking
/// `k_g=0.30 / thr=0.44` that sat at 80% of a narrower band, "two hundredths
/// of porosity from reading zero". Measured aquifer share there: **16.4% of
/// land**, with the promoted `Spring` contact (see [`Hydro::Spring`]) at
/// **3.69%**.
///
/// # Re-placed by The Trencher (Task 13), with The Witness's own method
///
/// Widening `carbonate` and `metamorphic_grade` moved porosity, because
/// `assemble_material` sums `0.5 * carbonate + … + 0.3 * (1 - grade)`. At
/// `0.46` unchanged the aquifer share went to **36.6% of land** and `Spring`
/// to **10.1%** (seed 0, level 6) — straight through the ceiling
/// `a_real_world_produces_a_porous_non_carbonate_vertex_in_bounded_shares`
/// exists to hold. Nathan authorized the re-placement; the method below is
/// The Witness's, not a new one.
///
/// **Their criterion, generalised — and the literal reading is what fails.**
/// "Mid-band, farthest from flipping to select everything or nothing" worked
/// on their distribution because a 0.078-wide ramp with no tails made
/// *inside the realized range* and *selects a non-degenerate share* the same
/// statement. The widened axis has long thin tails — clastic land porosity
/// now runs `n=80566 min=0.074 p05=0.197 p25=0.374 p50=0.473 p75=0.539
/// p95=0.614 max=0.645` (level 6, 6 seeds) — so the range is 7.3x the
/// interval of thresholds that select anything sane, and the two statements
/// come apart: the literal mid-range, `0.360`, would make **over half of all
/// land** an aquifer. The *purpose* attaches to the second statement, so the
/// band measured here is the interval of thresholds keeping the aquifer
/// share inside the committed guard's `[0.05, 0.35]`, swept at 0.002:
///
/// ```text
/// seed  0 -> [0.466, 0.574]
/// seed  7 -> [0.458, 0.578]
/// seed 42 -> [0.486, 0.586]
/// intersection -> [0.486, 0.574]
/// ```
///
/// `0.53` is that intersection's midpoint to within a thousandth — **50.0%
/// of the band**, the same place in it `0.46` held in the old one. The
/// aquifer population it selects is also nearly the one it selected before:
/// **15.77% of land at seed 0**, against The Witness's 16.4%.
///
/// **The `Spring` share is the finding, and it is not a placement problem.**
/// At `0.53` it reads **7.36% at seed 0** (inside the guard's `[0.005, 0.08]`,
/// but twice its old 3.69%), 8.71% at seed 7 and 10.54% at seed 42 — the
/// latter two above that ceiling. This is not fixable by moving this
/// constant: `Spring` is a *contact* (`promote_to_spring` — an `Aquifer`
/// vertex with a lower non-`Aquifer` neighbour), so its share tracks the
/// aquifer set's **perimeter**, and a continuous porosity field fragments
/// that set where a near-constant one left large contiguous blobs. Measured:
/// **no threshold anywhere in `[0.40, 0.60]` puts seed 42 inside both bands
/// while keeping seed 0 inside both** — the two seeds' joint-admissible
/// intervals, `[0.576, 0.586]` and `[0.512, 0.574]`, are disjoint. The
/// spring ceiling was calibrated against a porosity axis that no longer
/// exists; re-placing it needs its own measurement and is deliberately not
/// done here.
///
/// # THE FOUR NUMBERS THE TWO PARAGRAPHS ABOVE STATE DO NOT REPRODUCE
///
/// Task 14 re-ran the sweep at the shipped value, on the same three seeds, at
/// the same mesh level, and got a different answer every time. The value
/// `0.53` is **not** what moved and is left alone; what follows corrects the
/// RECORD beside it, which a future author would otherwise act on.
///
/// ```text
///                        recorded here    re-measured (Task 14)
///   aquifer, seed  0        15.77%            19.78%
///   spring,  seed  0         7.36%             7.82%
///   spring,  seed  7         8.71%             9.29%
///   spring,  seed 42        10.54%            11.10%
///   admissible band, s0  [0.466, 0.574]    [0.476, 0.592]
///   admissible band, s7  [0.458, 0.578]    [0.470, 0.588]
///   admissible band, s42 [0.486, 0.586]    [0.496, 0.600]
/// ```
///
/// **The re-measurement is the one with a positive control.** It is
/// `aquifer_shape_probe.rs`'s two arms: the first computes the guard's own two
/// shares with the guard's own expressions, and the second cross-checks its
/// reproduced copy of the three private constants above against the shipped
/// [`hydrogeology`] on every land vertex before it sweeps a single threshold.
/// Run against the PRE-Task-13 tree it reproduces the committed census's
/// `aquifer-fraction` column to all eight significant digits at seeds 0 and 7
/// (0.21615415, 0.26039727) — so the instrument is pinned to an artifact this
/// campaign did not author. The recorded column has no such anchor; its
/// admissible bands are low by a consistent 0.010-0.014 across all three
/// seeds, which is an offset rather than noise, so the harness that produced
/// it was measuring a slightly different population.
///
/// **Two consequences, and only the second is comfortable.** First, the
/// spring share at seed 0 is **0.0782 against the guard's 0.08 ceiling** —
/// 2.3% of headroom, where the record above promised 8%. A future change that
/// moves porosity at all reds
/// `a_real_world_produces_a_porous_non_carbonate_vertex_in_bounded_shares`
/// with no warning from this doc. Second, `0.53` survives the correction on
/// its merits: the re-measured intersection is `[0.496, 0.588]` and `0.53`
/// sits at 37% of it — off-centre where the record claimed 50.0%, but
/// comfortably inside, so the placement stands and no retune is owed.
///
/// **The "disjoint intervals" claim is also not quite right, and the
/// conclusion it supports survives anyway.** Re-measured, the three seeds'
/// aquifer-admissible bands intersect in `[0.496, 0.588]` and their
/// spring-admissible bands in `[0.586, 0.620]`, so `[0.586, 0.588]` satisfies
/// all six constraints at once. That is a two-thousandth-wide window, which is
/// a coincidence rather than a placement — a constant put there is one
/// re-measurement from falling out of it. So the ruling stands exactly as
/// written: the spring share is not fixable by moving this constant.
///
/// **And the mechanism sentence above is half right, which is why it is kept
/// rather than deleted.** `Spring` does track the aquifer set's perimeter, and
/// that perimeter did roughly double (margin/area 0.25 -> 0.44-0.55). But
/// "fragments that set" invites a picture of speckle that is false: the set
/// broke into more, raggeder bodies of ~70-100 vertices, not into dust.
/// Isolated vertices are 0.3-0.5% of it. See `aquifer_shape_probe.rs`'s module
/// doc for the before/after table and for why an extent rule on
/// `promote_to_spring` cannot recover the old share.
/// plumb: pending(wave-1)
const CLASTIC_AQUIFER_MIN_POROSITY: f64 = 0.53;

/// How much loose, uncemented coarse grain contributes to `porosity` (in
/// `assemble_material`), via `GRAIN_POROSITY_GAIN * grain * (1 -
/// induration)`. Exists for **dynamic range, not to cross a gate**: without
/// it, clastic land porosity is a single value (0.325) on ~90% of land, so
/// no threshold on [`CLASTIC_AQUIFER_MIN_POROSITY`] could ever partition
/// it. With it, porosity spanned `[0.416, 0.494]` as a continuous function
/// of crust age. (That span is the F5 measurement and is now historical:
/// since The Trencher widened the other two terms, clastic land porosity
/// runs `[0.074, 0.645]` and this term is one of three continuous
/// contributions rather than the only one. Its value is unchanged and its
/// calibration argument below is unaffected — the sweep was over *this*
/// term's own contribution.) Calibrated by sweep (The Witness, Task 5b) against the
/// *width* of that band across `k_g ∈ {0.20, 0.30, 0.40}`: `0.40` gives the
/// widest band (`0.078`), so [`CLASTIC_AQUIFER_MIN_POROSITY`] has the most
/// room before a terrain change flips it to select everything or nothing —
/// a retune of this value is a retune, not a cleanup (decision 0057).
/// plumb: pending(wave-1)
const GRAIN_POROSITY_GAIN: f64 = 0.40;

// ---------------------------------------------------------------------------
// The metaphysics-gated overlay tier (The Ground, spec §8).
//
// Six constants, every one of them the SHAPE of a charged world rather than a
// fact about a mundane one, and none of them read at all under the default
// (inert) tier. They are tagged `per-world` deliberately: the intensity and
// reach of a world's magic is a property of that world, and the pin is a tier
// FLAG today only because a tier parameterisation would have needed draws this
// stage is forbidden (and does not need). Tagging them `universal` would claim
// every charged world's ley-lines run exactly three hops wide, which is
// precisely the inherited fixedness `tools/plumb` exists to make visible.
// ---------------------------------------------------------------------------

/// Graph hops from the nearest same-plate boundary within which the ley term
/// contributes. A fault is a *line*, so this is deliberately tighter than
/// [`OROGEN_REACH`] (4): metamorphism is a broad aureole around a collision,
/// whereas a ley-line that reads as a line at all must be narrower than the
/// orogen it runs through.
/// plumb: per-world(the width of a world's ley-lines is a property of that world's magic, not of this code)
const LEY_REACH: u32 = 3;

/// Peak ley contribution, at a boundary vertex itself, falling linearly to
/// zero at [`LEY_REACH`] hops. Below [`WELL_GAIN`] because a fault is a
/// conduit and a plume is a source: flux passing through is weaker than flux
/// welling up.
/// plumb: per-world(ley intensity is a property of a world's magic, not of this code)
const LEY_GAIN: f64 = 0.6;

/// Angular radius of a mana-well's aureole around a live hotspot dome,
/// radians (~0.12 rad, about 7°). An order of magnitude tighter than
/// [`crate::elevation::TRAIL_LENGTH_RAD`] (0.35, the whole trail's length),
/// so a well is a place and not a province.
/// plumb: per-world(the reach of a mana-well is a property of a world's magic, not of this code)
const WELL_RADIUS_RAD: f64 = 0.12;

/// Peak mana-well contribution, at the dome itself, falling linearly in
/// angle to zero at [`WELL_RADIUS_RAD`]. The largest of the three: the plume
/// conduit is the one place the mantle reaches the surface directly.
/// plumb: per-world(mana-well intensity is a property of a world's magic, not of this code)
const WELL_GAIN: f64 = 0.8;

/// Crust age (`[0,1]`, the winning craton's age) above which deep time reads
/// as hallowed or cursed ground. Set high on purpose: the term is meant to
/// select the oldest cratonic shields — the ground that was *there* for the
/// deep-time cataclysms — not merely old-ish continental interior. Oceanic
/// crust has age 0 and so never qualifies, which is the intended reading.
/// plumb: per-world(how much deep time it takes to charge ground is a property of a world's magic, not of this code)
const HALLOW_AGE_MIN: f64 = 0.75;

/// Peak deep-time contribution, at maximal crust age. The weakest of the
/// three: accumulated history is a residue, not a source, so on its own it
/// can never saturate a vertex — hallowed ground needs a fault or a well
/// beneath it to read as a nexus.
/// plumb: per-world(the residue deep time leaves is a property of a world's magic, not of this code)
const HALLOW_GAIN: f64 = 0.4;

/// Thaumic saturation at one vertex, `[0, 1]` — the metaphysics-gated
/// overlay tier on the material buffer (The Ground, spec §8; `MAP-40` /
/// `MAP-53`).
///
/// **An inert world returns exactly `0.0` on the first line**, before a
/// single input is read, by the same path the pre-gate code took: the
/// mundane substrate *is* the charged tier's floor, so the overlay refines
/// the inert world and never contradicts it (coarse-constrains-fine, applied
/// to metaphysics). `TerrainPins::default()` is inert, so every world
/// generated before this gate existed is byte-identical to the same world
/// generated after it.
///
/// The charged tier sums the three hooks §8 names — "ley-lines from faults,
/// mana-wells from hotspots, hallowed/cursed ground from deep-time
/// cataclysms" — each a pure read over world-state terrain already owns:
///
/// - **Ley-lines from faults.** `hops_to_boundary` is the graph distance to
///   the nearest same-plate boundary vertex (`TectonicGlobe::boundary_
///   distance`), already computed for [`assemble_material`]'s metamorphic
///   term. Flux runs *along* the fault, so saturation falls linearly with
///   distance from it and vanishes past [`LEY_REACH`].
/// - **Mana-wells from hotspots.** `hotspot_domes` is the retained trail-
///   seamount list; the term reads only `age_index == 0` entries, the LIVE
///   domes. The fossil trail upstream of a dome is where the plume *was*,
///   and a well is where it *is* — a distinction the trail list makes for
///   free and which no other terrain field records. Saturation falls
///   linearly in angular distance from the nearest live dome, vanishing past
///   [`WELL_RADIUS_RAD`]; wells superpose by maximum, not by sum, because
///   two plumes do not make a deeper conduit.
/// - **Hallowed/cursed ground from deep time.** `crust_age` is the winning
///   craton's age in `[0,1]`, zero on oceanic floor. Only the oldest shields
///   (past [`HALLOW_AGE_MIN`]) contribute, and they contribute the least
///   ([`HALLOW_GAIN`]): the ground remembers, but memory is residue.
///
/// The three **sum** and then clamp, rather than taking a maximum, so that a
/// live plume sitting on a fault under an ancient shield saturates — a
/// **nexus** is the coincidence of the three generators, which is exactly
/// `MAP-53`'s centrality claim read pointwise. Each term alone is bounded
/// well below 1, so saturation is reachable only by coincidence and never by
/// any single hook.
///
/// **No draw.** Every input is existing world-state (a graph distance, a
/// retained seamount list, a sampled crust age) and every parameter is an
/// authored constant, so activating the tier consumes no stream, perturbs no
/// draw order, and forces no epoch — the additive, no-epoch activation The
/// Ground reserved the `thaumic` slot for.
/// type-audit: bare-ok(count: hops_to_boundary), bare-ok(ratio: position), bare-ok(ratio: crust_age), bare-ok(ratio: return)
pub(crate) fn thaumic_at(
    metaphysics: Metaphysics,
    hops_to_boundary: Option<u32>,
    hotspot_domes: &[TrailSeamount],
    position: [f64; 3],
    crust_age: f64,
) -> f64 {
    if !metaphysics.is_charged() {
        return 0.0;
    }

    // Ley-lines from faults: linear falloff in graph hops.
    let ley = match hops_to_boundary {
        Some(h) if h <= LEY_REACH => LEY_GAIN * (1.0 - h as f64 / LEY_REACH as f64),
        _ => 0.0,
    };

    // Mana-wells from hotspots: the nearest LIVE dome only (`age_index == 0`),
    // linear falloff in angular distance. Superposed by maximum.
    let mut well: f64 = 0.0;
    for dome in hotspot_domes.iter().filter(|d| d.age_index == 0) {
        let cos_sep = dot(position, dome.position).clamp(-1.0, 1.0);
        let sep = math::acos(cos_sep);
        if sep < WELL_RADIUS_RAD {
            well = well.max(WELL_GAIN * (1.0 - sep / WELL_RADIUS_RAD));
        }
    }

    // Hallowed/cursed ground from deep time: the oldest shields only.
    let hallow = if crust_age > HALLOW_AGE_MIN {
        HALLOW_GAIN * ((crust_age - HALLOW_AGE_MIN) / (1.0 - HALLOW_AGE_MIN)).clamp(0.0, 1.0)
    } else {
        0.0
    };

    (ley + well + hallow).clamp(0.0, 1.0)
}

/// Drainage scale for [`cave_proneness`]'s wetting term. Formerly shared
/// with `hydrogeology`'s flowing-vs-still `Spring` gate (named
/// `SPRING_DRAINAGE_THRESHOLD`); that gate is retired (The Witness, Task
/// 5b) — `drainage` measures water flowing *over* a vertex, but a spring is
/// water emerging *from* one, so `hydrogeology` never needed `drainage` in
/// the first place, and the retired gate was unreachable regardless (land
/// drainage at production `GLOBE_LEVEL` (6) maxes at 219 against this
/// value of 500). `Spring` is now a geometric contact promoted by
/// `GeneratedTerrain::hydro_at`, entirely independent of this constant.
/// This constant's sole remaining consumer is the wetting term below;
/// value unchanged, only the name and the surviving justification.
/// plumb: pending(wave-1)
const CAVE_WETNESS_DRAINAGE_SCALE: f64 = 500.0;

/// Void-proneness (caves/sinkholes), `[0,1]` (spec §3, negation "solid → void").
/// Dominated by the carbonate/porosity product (dissolution needs both
/// soluble rock and connected voids); drainage is a secondary modifier —
/// caves also form under slow diffuse flow, so wetting nudges rather than
/// gates the base rate.
/// type-audit: bare-ok(ratio: return), bare-ok(count: drainage)
pub fn cave_proneness(buf: &MaterialBuffer, drainage: f64) -> f64 {
    let wetting = (drainage / CAVE_WETNESS_DRAINAGE_SCALE).min(1.0);
    (buf.carbonate * buf.porosity * (0.85 + 0.15 * wetting)).clamp(0.0, 1.0)
}

/// Mean graph-edge length of `geo`, as a **chord** on the unit sphere.
///
/// Chord rather than arc throughout the orogen derivation: it is monotone in
/// arc, so it reparameterises the aureole without reordering any vertex, and
/// over a reach of ~0.075 rad the two agree to better than one part in 10^4.
/// It also costs no transcendental, which keeps a per-vertex derivation off
/// `math.rs` entirely.
fn mean_edge_chord(geo: &Geosphere) -> f64 {
    let mut total = 0.0;
    let mut count = 0usize;
    for vertex in geo.vertices() {
        let p = geo.position(vertex);
        for &neighbor in geo.neighbors(vertex) {
            let q = geo.position(neighbor);
            let d = [p[0] - q[0], p[1] - q[1], p[2] - q[2]];
            total += (d[0] * d[0] + d[1] * d[1] + d[2] * d[2]).sqrt();
            count += 1;
        }
    }
    total / count as f64
}

/// The orogenic aureole's reach on `geo`, as a chord length: [`OROGEN_REACH`]
/// graph steps measured properly instead of counted.
///
/// Derived from the mesh rather than authored as an angle, deliberately: the
/// reach has always been resolution-relative (four hops is four hops at every
/// level), and pinning an absolute angle here would silently change the
/// aureole's footprint at every level but the production one. Hoist this
/// above any per-vertex loop — it is a whole-mesh scan.
pub(crate) fn orogen_reach_chord(geo: &Geosphere) -> f64 {
    OROGEN_REACH as f64 * mean_edge_chord(geo)
}

/// Continuous orogenic proximity at a vertex, `[0,1]`: 1 on a boundary, 0 at
/// [`orogen_reach_chord`] and beyond, falling linearly with true distance to
/// the nearest same-plate boundary vertex.
///
/// `nearest` is `TectonicGlobe::boundary_distance`'s entry — `(hops, source)`.
/// **Only the source is read**; the hop count is exactly the quantizer this
/// replaces. `None` (no reachable same-plate boundary) is 0, matching the old
/// `hops.unwrap_or(OROGEN_REACH)`.
///
/// The old membership test also had a kind clause — `matches!(kind,
/// ContinentalCollision | CoastalRange) || hops <= OROGEN_REACH` — and it is
/// gone because it was **strictly subsumed**, not because the kind stopped
/// mattering: `boundaries::boundary_distance` seeds its BFS with `(0, self)`
/// for every vertex that *has* a boundary, so a collision vertex always had
/// `hops == 0` and satisfied the right-hand clause anyway. The aureole was
/// kind-agnostic in effect before this change and still is.
/// type-audit: bare-ok(ratio: reach_chord), bare-ok(count: nearest), bare-ok(ratio: return)
pub(crate) fn orogen_proximity(
    geo: &Geosphere,
    reach_chord: f64,
    vertex: Vertex,
    nearest: Option<(u32, Vertex)>,
) -> f64 {
    let Some((_, source)) = nearest else {
        return 0.0;
    };
    let p = geo.position(vertex);
    let q = geo.position(source);
    let d = [p[0] - q[0], p[1] - q[1], p[2] - q[2]];
    let chord = (d[0] * d[0] + d[1] * d[1] + d[2] * d[2]).sqrt();
    (1.0 - chord / reach_chord).clamp(0.0, 1.0)
}

/// Metamorphic grade at a vertex, `[0,1]`: 0 unaltered → 1 gneiss.
///
/// **Two additive processes, because there are two** (The Trencher, Task 13).
/// Regional/contact metamorphism is a narrow, intense aureole around a
/// collision — [`OROGENIC_AUREOLE_PEAK`] times [`orogen_proximity`], linear in
/// distance across the belt, which is roughly how a collisional zonation
/// (chlorite → biotite → garnet → staurolite → kyanite → sillimanite) maps
/// across its width. Burial diagenesis is a broad, weak overprint that every
/// continental basin accumulates as it subsides —
/// [`BURIAL_OVERPRINT_GAIN`] times `crust_age`. The pre-Trencher derivation
/// modelled only the first and asserted the second was *identically zero*,
/// which is what put a 45.8% point mass at 0 on the axis
/// `EnergySource::SulphideOxidation` multiplies by.
///
/// Linear in proximity, not squared or smoothed: the derivation this replaces
/// was linear in hops, so making the distance continuous is the whole change
/// and no second one is smuggled in with it. Oceanic crust stays flat 0 — it
/// is young, thin, and unburied, and the buffer's oceanic regime is
/// deliberately flat across every axis in this file.
///
/// Extracted as a shared function rather than written twice: it feeds
/// [`induration_at`] (which the globe computes *before* elevation) and the
/// buffer's own axis, and those two must never diverge.
/// type-audit: bare-ok(ratio: crust_age), bare-ok(flag: continental), bare-ok(ratio: orogen_proximity), bare-ok(ratio: return)
pub(crate) fn metamorphic_grade_at(
    crust_age: f64,
    continental: bool,
    orogen_proximity: f64,
) -> f64 {
    if !continental {
        return 0.0;
    }
    (orogenic_overprint(orogen_proximity) + BURIAL_OVERPRINT_GAIN * crust_age).clamp(0.0, 1.0)
}

/// The orogenic half of [`metamorphic_grade_at`] on its own: recrystallisation
/// driven by a collision, with no burial term.
///
/// Named and shared because [`induration_at`] must read **this** rather than
/// the whole grade, and the reason is physical rather than a workaround.
/// Induration already prices burial — its `grain` term is `0.4 + 0.5 *
/// crust_age`, whose stated content is that old crust is more evolved and
/// coarser. Feeding it the full grade would add
/// `0.4 * BURIAL_OVERPRINT_GAIN * crust_age` on top of that: **the same age,
/// priced twice, in the same sum**. Not a small effect, and it was caught
/// rather than reasoned about —
/// away from any orogen `induration` would have gone from `0.35 + 0.2 *
/// grain` to `0.286 + 0.36 * grain`, crossing [`classify_rock`]'s
/// `induration > 0.5` igneous gate at `grain = 0.594` rather than at
/// `grain = 0.75`. That closes the entire `grain ∈ (0.6, 0.75]` window
/// `RockClass::Conglomerate` occupies, and
/// `alluvium_and_coal_are_reachable_across_seeds` went red on exactly that:
/// a rock class deleted from every world by a change that was about
/// metamorphic grade and had no business touching hardness.
///
/// Recrystallisation, by contrast, genuinely *is* a hardness story — a gneiss
/// is hard because its minerals interlocked — so induration reads the aureole
/// and not the background, and at zero proximity its formula is **unchanged**
/// from before The Trencher.
fn orogenic_overprint(orogen_proximity: f64) -> f64 {
    OROGENIC_AUREOLE_PEAK * orogen_proximity.clamp(0.0, 1.0)
}

/// Induration/hardness at a vertex, `[0,1]`: 0 soft (shale/soil) → 1 hard
/// (quartzite/gneiss). The Sculpting/Ground seam (spec §4): pulled out of
/// `assemble_material` as a standalone pre-elevation function so the globe
/// can compute it before `generate_elevation` runs, ahead of any elevation
/// carve that later wants to read hardness. Its `grain` expression mirrors
/// `assemble_material`'s local, and its metamorphic term is
/// [`orogenic_overprint`] — the aureole half of [`metamorphic_grade_at`],
/// shared as one derivation rather than written twice. **Not the whole
/// grade**, and [`orogenic_overprint`]'s own doc argues why: the burial
/// background is age, and induration already prices age through `grain`.
///
/// Total at the extremes (spec §4): defined for the full `[0,1]` input
/// range; the gated metaphysics overlay may inject sentinel values later
/// without a formula change.
/// type-audit: bare-ok(ratio: return), bare-ok(ratio: crust_age), bare-ok(flag: continental), bare-ok(ratio: orogen_proximity)
pub fn induration_at(crust_age: f64, continental: bool, orogen_proximity: f64) -> f64 {
    // Old cratons are more evolved/coarse; young crust finer.
    let grain = if continental {
        0.4 + 0.5 * crust_age
    } else {
        0.2
    };
    // The AUREOLE half of the grade only — see `orogenic_overprint`.
    let overprint = if continental {
        orogenic_overprint(orogen_proximity)
    } else {
        0.0
    };
    // Induration: metamorphics/old plutons hard; young/soft sediments low.
    (0.35 + 0.4 * overprint + 0.2 * grain).clamp(0.0, 1.0)
}

/// Carbonate content at a vertex, `[0,1]` (spec §2/§4 pre-elevation seam,
/// mirroring [`induration_at`]): the warm shallow shelf factory, as a product
/// of two smooth falloffs. Pointwise inputs only (continental flag, crust
/// thickness, latitude), all available before elevation runs, so the globe can
/// build a `carbonate_pre` field the carve reads ahead of the carve's own
/// elevation output. `assemble_material` calls the same function so the
/// buffer's `carbonate` axis and the pre-carve field can never diverge.
///
/// **It used to be a boolean conjunction flattened to two numbers** — `0.7` on
/// `continental && thickness < threshold + 6 && lat < 0.6`, else `0.05` — and
/// it took exactly those two values across 82,135 underworld readings (The
/// Trencher, Task 12). Both gates it ANDed together are continuous quantities,
/// and both are kept, now as factors:
///
/// - **`cos(lat)`** — the carbonate factory is temperature-limited, and mean
///   annual insolation at latitude φ falls as `cos φ`. This is the whole
///   warmth argument: no fitted width, no free parameter, the same cosine the
///   climate domain's own insolation is proportional to. The old hard cut at
///   `lat < 0.6` rad asserted a platform at 34°N is in full production and one
///   at 35° produces nothing; a cool-water (heterozoan) factory in fact runs
///   weakly to high latitude, which is what the tail of a cosine says.
/// - **the shelf falloff** — see [`CARBONATE_SHELF_REACH_KM`].
///
/// The step at the continental threshold is kept and is not an artefact: a
/// carbonate platform really does end abruptly at the shelf break, and
/// oceanic crust is the far side of that break.
/// type-audit: bare-ok(flag: continental), bare-ok(ratio: thickness_km), bare-ok(ratio: lat), bare-ok(ratio: return)
pub(crate) fn carbonate_at(continental: bool, thickness_km: f64, lat: f64) -> f64 {
    if !continental {
        return CARBONATE_FLOOR;
    }
    let warmth = math::cos(lat).clamp(0.0, 1.0);
    let shelf = (1.0
        - (thickness_km - crate::crust::CONTINENTAL_THRESHOLD_KM).max(0.0)
            / CARBONATE_SHELF_REACH_KM)
        .clamp(0.0, 1.0);
    (CARBONATE_FLOOR + (CARBONATE_SHELF_PEAK - CARBONATE_FLOOR) * warmth * shelf).clamp(0.0, 1.0)
}

/// Assemble the material buffer over the canonical grid (spec §2). Pointwise
/// axes derive from crust age/thickness and plate motion; grid-bound terms
/// (metamorphic grade near boundaries, soil depth from slope/drainage) use
/// the globe's boundary-distance and drainage fields. No draws.
pub fn assemble_material(geo: &Geosphere, globe: &TectonicGlobe) -> VertexMap<MaterialBuffer> {
    // Built once, not per vertex: the seed (`globe.lithology_noise_seed()`)
    // does not vary by vertex, so `Fbm::new` (which precomputes per-octave
    // seeds from the base seed) constructing a fresh instance on every one
    // of `VertexMap::from_fn`'s per-vertex calls was pure waste. `Fbm::sample`
    // is byte-identical to `fbm_2d` with the same seed/octaves (`kernel/
    // src/noise.rs`'s `fbm_2d` is literally `Fbm::new(seed,
    // octaves).sample(x, y)`), so hoisting the construction changes no
    // output. Same "build the sampler once above the loop" discipline
    // `domains/terrain/CLAUDE.md` documents for `SphereFbm`.
    let lithology_noise = Fbm::new(globe.lithology_noise_seed(), 3);
    // Hoisted for the same reason the sampler above is: `orogen_reach_chord`
    // is a whole-mesh scan and does not vary by vertex.
    let reach_chord = orogen_reach_chord(geo);
    VertexMap::from_fn(geo, |vertex| {
        let thickness = *globe.crust.get(vertex);
        let continental = thickness >= crate::crust::CONTINENTAL_THRESHOLD_KM;
        let age = *globe.crust_age.get(vertex);
        let p = geo.position(vertex);
        // Margin polarity is needed before silica: arc (active-margin)
        // magmatism is intermediate, not felsic (see base_silica below).
        let plate = &globe.plates[*globe.plate_of.get(vertex) as usize];
        let margin = margin_polarity(plate, p, continental);

        // Felsic index: continental crust is felsic (granitic) except at
        // active (arc) margins, where subduction magmatism is petrologically
        // intermediate (andesitic); oceanic crust is mafic.
        let base_silica = if !continental {
            0.15
        } else if matches!(margin, MarginPolarity::Active) {
            0.5
        } else {
            0.7
        };
        // Old cratons are more evolved/coarse; young crust finer.
        let grain = if continental { 0.4 + 0.5 * age } else { 0.2 };
        // Boundary influence, as a continuous distance rather than a hop count
        // (see `orogen_proximity` / `metamorphic_grade_at`).
        let hops = globe.boundary_distance.get(vertex).map(|(h, _)| h);
        let proximity = orogen_proximity(
            geo,
            reach_chord,
            vertex,
            *globe.boundary_distance.get(vertex),
        );
        let metamorphic_grade = metamorphic_grade_at(age, continental, proximity);

        // Sub-vertex patchiness from existing noise (no draws): perturb silica.
        let patch = lithology_noise.sample(p[0] * 6.0, p[1] * 6.0) - 0.5;
        let silica = (base_silica + 0.15 * patch).clamp(0.0, 1.0);

        // Carbonate favors warm shallow shelves — same pre-elevation
        // function the globe computes ahead of the carve (the Sculpting/
        // Ground seam) — kept identical here so the buffer's axis and the
        // pre-carve field never diverge. Atoll vertices (Sculpting Task 9/10:
        // reef caps grown over a drowned seamount the carve capped) always
        // override to a high carbonate reading, a biogenic reef regardless
        // of the pointwise shelf test.
        let lat = math::asin(p[2].clamp(-1.0, 1.0)).abs();
        let carbonate = if globe.atoll_vertices.contains(&vertex) {
            0.9
        } else {
            carbonate_at(continental, thickness, lat)
        };

        // Induration: same pre-elevation function the globe computes ahead
        // of elevation (the Sculpting/Ground seam) — kept identical here so
        // the buffer's axis and the globe's standalone field never diverge.
        let induration = induration_at(age, continental, proximity);
        // Porosity: dissolution in carbonate (karst), packing in loose coarse
        // grain, and recrystallisation closing pores in metamorphics. The
        // grain term (The Witness, Task 5b) exists to give the axis DYNAMIC
        // RANGE, not to cross any particular gate: at the time it was added,
        // `carbonate` was binary (0.05 or 0.7-0.9) and `metamorphic_grade`
        // was 0 outside an orogen, so clastic land porosity was *exactly*
        // 0.325 on ~90% of land (measured at production L6: p25 through p95
        // all 0.325) — the axis carried almost no information and no
        // threshold could partition it.
        //
        // **The other two terms carry their own weight now** (The Trencher,
        // Task 13): `carbonate_at` and `metamorphic_grade_at` are both
        // continuous, so this sum is a three-term continuum rather than one
        // continuous term plus two step functions. The grain term stays —
        // its physics (loose, uncemented coarse sediment holds pore space)
        // is independent of the other two, and its calibration is still the
        // widest-band choice The Witness measured — but the *reason it was
        // urgent* has been removed at its source, and
        // `CLASTIC_AQUIFER_MIN_POROSITY` was re-placed against the band this
        // widening produces.
        let porosity = (0.5 * carbonate
            + GRAIN_POROSITY_GAIN * grain * (1.0 - induration)
            + 0.3 * (1.0 - metamorphic_grade))
            .clamp(0.0, 1.0);

        let sediment_m = *globe.sediment_thickness.get(vertex);
        let soil_depth = soil_depth_at(geo, globe, vertex, sediment_m);
        let basement = if continental {
            Basement::Continental
        } else {
            Basement::Oceanic
        };

        MaterialBuffer {
            silica,
            grain,
            induration,
            carbonate,
            metamorphic_grade,
            porosity,
            margin,
            soil_depth,
            basement,
            thaumic: thaumic_at(globe.metaphysics, hops, &globe.trail_seamounts, p, age),
        }
    })
}

/// Active if the plate's surface motion at the vertex points *outward* (leading
/// edge), passive if inward (trailing), Interior if neither dominates.
/// Pointwise inputs only (mirroring [`induration_at`]/[`carbonate_at`]): a
/// plate reference and a vertex position, both available before elevation
/// runs, so the globe can build a `margins` field the carve reads ahead of
/// the carve's own elevation output. `assemble_material` calls the same
/// function so the buffer's `margin` axis and the pre-carve field can never
/// diverge. `pub(crate)`: the carve (Sculpting) reads this directly.
/// type-audit: bare-ok(flag: continental)
pub(crate) fn margin_polarity(plate: &Plate, pos: [f64; 3], continental: bool) -> MarginPolarity {
    if !continental {
        return MarginPolarity::Oceanic;
    }
    let vel = velocity_at(plate, pos);
    let speed = (vel[0] * vel[0] + vel[1] * vel[1] + vel[2] * vel[2]).sqrt();
    if speed < 1e-6 {
        return MarginPolarity::Interior;
    }
    // Outward component = velocity · (direction from plate seed to vertex).
    let seed_dir = normalize(sub(pos, plate.seed_position));
    let outward = dot(vel, seed_dir) / speed;
    if outward > 0.25 {
        MarginPolarity::Active
    } else if outward < -0.25 {
        MarginPolarity::Passive
    } else {
        MarginPolarity::Interior
    }
}

/// Regolith thickness: accumulates in high-drainage lowlands and wherever
/// the carve deposited real sediment, strips on steep slopes. Metres. Pure
/// function of drainage, the carve's `sediment_m` (spec §2 stage 8's
/// coordinated Ground formula change — Sculpting Task 10), and local
/// elevation range.
/// type-audit: bare-ok(ratio: sediment_m)
fn soil_depth_at(
    geo: &Geosphere,
    globe: &TectonicGlobe,
    vertex: Vertex,
    sediment_m: f64,
) -> SoilDepth {
    if *globe.elevation.get(vertex) < globe.sea_level {
        return SoilDepth::new(0.0);
    }
    let here = globe.elevation.get(vertex).get();
    let max_drop = geo
        .neighbors(vertex)
        .iter()
        .map(|n| here - globe.elevation.get(*n).get())
        .fold(0.0_f64, f64::max);
    let drainage = *globe.drainage.get(vertex);
    // Accumulation ~ log(drainage) plus the carve's real deposited sediment
    // (capped at 10 m so an extreme delta/wedge fill doesn't dominate).
    let accum = 0.5 * math::ln(1.0 + drainage) + 0.8 * sediment_m.min(10.0);
    let strip = (max_drop / 300.0).min(3.0);
    SoilDepth::new((accum - strip).max(0.0))
}

/// Soil orders (spec §4), climate-dominated and parent-modulated.
///
/// `Ord`/`PartialOrd` (declaration order below) exist only so callers can
/// collect distinct orders into a `BTreeSet` (the project bans `HashSet`) —
/// there is no meaningful ranking between soil orders.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SoilOrder {
    /// Hot + wet, leached.
    Laterite,
    /// Cold conifer, acidic.
    Podzol,
    /// Grassland, fertile.
    Chernozem,
    /// Desert; salt-flat over evaporite.
    Aridisol,
    /// Temperate forest, good farmland.
    Loam,
    /// Fresh volcanic, very fertile.
    Andosol,
    /// Thin rocky, steep/young peaks.
    Leptosol,
    /// Waterlogged organic.
    Histosol,
    /// Poorly-drained mineral.
    Gley,
}

/// A soil's suitability vector (spec §3, round 1). Each `[0,1]`.
/// type-audit: bare-ok(ratio: grain_suit), bare-ok(ratio: moisture_suit), bare-ok(ratio: depth_suit)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Fertility {
    /// Workability from texture.
    pub grain_suit: f64,
    /// Moisture-holding suitability.
    pub moisture_suit: f64,
    /// Rooting depth suitability.
    pub depth_suit: f64,
}

/// Classify soil order (spec §4). `slope_m` is the local elevation drop (m).
/// type-audit: pending(wave-2: mean_temp_c), bare-ok(ratio: moisture), bare-ok(ratio: slope_m)
pub fn classify_soil(
    parent: RockClass,
    mean_temp_c: f64,
    moisture: f64,
    slope_m: f64,
    depth: &SoilDepth,
) -> SoilOrder {
    if depth.get() < 0.25 || slope_m > 300.0 {
        return SoilOrder::Leptosol;
    }
    if matches!(
        parent,
        RockClass::Basalt | RockClass::Andesite | RockClass::Rhyolite
    ) && mean_temp_c > 5.0
    {
        return SoilOrder::Andosol;
    }
    if moisture < 0.2 {
        return SoilOrder::Aridisol;
    }
    if moisture > 0.85 {
        return if mean_temp_c > 22.0 {
            SoilOrder::Laterite
        } else {
            SoilOrder::Histosol
        };
    }
    if mean_temp_c < 3.0 {
        return SoilOrder::Podzol;
    }
    if mean_temp_c > 22.0 {
        return SoilOrder::Laterite;
    }
    if moisture < 0.5 {
        SoilOrder::Chernozem
    } else if moisture > 0.7 {
        SoilOrder::Gley
    } else {
        SoilOrder::Loam
    }
}

/// Walk-facing appearance vector (spec §3, round 1 paint/color). Each `[0,1]`.
/// type-audit: bare-ok(ratio: albedo), bare-ok(ratio: hue), bare-ok(ratio: coarseness), bare-ok(ratio: hardness)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Appearance {
    /// Lightness under foot: 0 black basalt → 1 white chalk.
    pub albedo: f64,
    /// Hue proxy: 0 grey/black → 1 red/ochre (iron).
    pub hue: f64,
    /// Grain visible under foot.
    pub coarseness: f64,
    /// Hardness under foot.
    pub hardness: f64,
}

/// Rock classes that read fine-grained underfoot regardless of the buffer's
/// noise term: extrusive/glassy igneous (rapid cooling suppresses crystal
/// growth) and fine clastic/chemical rocks. `appearance` halves their
/// coarseness so a fast-cooled basalt never reads as coarse as a slow-cooled
/// granite even when the buffer's `grain` axis happens to agree.
fn reads_fine_grained(rock: RockClass) -> bool {
    matches!(
        rock,
        RockClass::Basalt
            | RockClass::Andesite
            | RockClass::Rhyolite
            | RockClass::Chert
            | RockClass::Shale
            | RockClass::Evaporite
            | RockClass::Slate
    )
}

/// Project appearance from the buffer + class (spec §3).
pub fn appearance(buf: &MaterialBuffer, rock: RockClass) -> Appearance {
    let albedo = (0.25 + 0.6 * buf.silica + 0.3 * buf.carbonate).clamp(0.0, 1.0);
    let hue = match rock {
        RockClass::Ironstone => 0.9,
        RockClass::Basalt | RockClass::Gabbro => 0.1,
        _ => 0.4,
    };
    let coarseness = if reads_fine_grained(rock) {
        buf.grain * 0.5
    } else {
        buf.grain
    };
    Appearance {
        albedo,
        hue,
        coarseness,
        hardness: buf.induration,
    }
}

/// Mineral endmember reflectances on the kernel's band grid.
///
/// Four curves stand in for the mineralogy the buffer already tracks:
/// felsic (quartz and feldspar — bright, faintly warm), mafic (pyroxene and
/// olivine — dark and flat), carbonate (bright and flat), and iron oxide
/// (dark in the short bands, strongly reflective in the long ones, which is
/// the entire visual signature of rust and ochre).
///
/// Declared approximations. They are the reason a granite reads pale and a
/// basalt reads dark, and the campaign's claims rest on those relations
/// rather than on laboratory accuracy.
mod endmembers {
    use hornvale_kernel::color::BANDS;

    /// Quartz and feldspar.
    /// type-audit: bare-ok(ratio)
    pub const FELSIC: [f64; BANDS] = [0.38, 0.45, 0.52, 0.56, 0.58, 0.60, 0.62, 0.63, 0.64, 0.64];
    /// Pyroxene and olivine.
    /// type-audit: bare-ok(ratio)
    pub const MAFIC: [f64; BANDS] = [0.05, 0.06, 0.08, 0.09, 0.10, 0.11, 0.12, 0.12, 0.13, 0.13];
    /// Calcite and dolomite.
    /// type-audit: bare-ok(ratio)
    pub const CARBONATE: [f64; BANDS] =
        [0.55, 0.68, 0.76, 0.80, 0.82, 0.83, 0.84, 0.84, 0.85, 0.85];
    /// Hematite and goethite — the red one.
    /// type-audit: bare-ok(ratio)
    pub const IRON_OXIDE: [f64; BANDS] =
        [0.03, 0.04, 0.05, 0.06, 0.09, 0.16, 0.42, 0.58, 0.63, 0.65];
}

/// Rock classes whose iron oxide dominates their appearance. Mirrors the
/// structure of [`appearance`]'s own `hue` match, so the two projections of
/// the buffer cannot disagree about which rocks read red.
fn is_iron_rich(rock: RockClass) -> bool {
    matches!(rock, RockClass::Ironstone)
}

/// Rock classes whose appearance is dominated by dark mafic minerals
/// regardless of the buffer's silica term. Mirrors [`appearance`]'s `hue`
/// match for the same reason as [`is_iron_rich`].
fn is_mafic_dominated(rock: RockClass) -> bool {
    matches!(rock, RockClass::Basalt | RockClass::Gabbro)
}

/// Project the material buffer to a reflectance **mixture** — the second
/// projection of the same axes [`appearance`] projects (spec "The Pigment"
/// §5.1). No new data: `silica`, `carbonate` and the rock class are all
/// already stored.
///
/// Returns a [`Mixture`] rather than a [`Reflectance`] so a later texture
/// layer can arrange the components spatially instead of re-deriving them.
/// Call [`Mixture::integrate`] for the single reflectance.
pub fn reflectance(buf: &MaterialBuffer, rock: RockClass) -> Mixture {
    let carbonate = buf.carbonate.clamp(0.0, 1.0);
    let silicate_share = 1.0 - carbonate;

    // Within the silicate fraction, silica splits felsic from mafic —
    // except where the rock class says the mafic minerals dominate anyway.
    let felsic_fraction = if is_mafic_dominated(rock) {
        0.1
    } else {
        buf.silica.clamp(0.0, 1.0)
    };

    let iron = if is_iron_rich(rock) { 0.55 } else { 0.03 };
    let remaining = silicate_share * (1.0 - iron);

    let components = vec![
        Reflectance::new(endmembers::FELSIC).expect("authored endmember is within [0, 1]"),
        Reflectance::new(endmembers::MAFIC).expect("authored endmember is within [0, 1]"),
        Reflectance::new(endmembers::CARBONATE).expect("authored endmember is within [0, 1]"),
        Reflectance::new(endmembers::IRON_OXIDE).expect("authored endmember is within [0, 1]"),
    ];
    let weights = vec![
        remaining * felsic_fraction,
        remaining * (1.0 - felsic_fraction),
        carbonate,
        silicate_share * iron,
    ];
    // Every weight is non-negative and, since `carbonate` and
    // `silicate_share` sum to 1 with `iron < 1`, the total is strictly
    // positive for every buffer.
    Mixture::new(components, weights).expect("weights are non-negative with a positive total")
}

/// Mineral prospectivity (spec §3, round 1 distribution). The deposits
/// campaign turns this field into point bodies; here it is a probability.
/// type-audit: bare-ok(ratio: return), bare-ok(ratio: unrest)
pub fn prospectivity(buf: &MaterialBuffer, boundary: Option<BoundaryKind>, unrest: f64) -> f64 {
    let setting = match boundary {
        Some(BoundaryKind::IslandArc) | Some(BoundaryKind::CoastalRange) => 0.7,
        Some(BoundaryKind::ContinentalRift) | Some(BoundaryKind::OceanicRidge) => 0.5,
        Some(BoundaryKind::ContinentalCollision) => 0.4,
        _ => 0.1,
    };
    (0.6 * setting + 0.3 * unrest + 0.1 * buf.metamorphic_grade).clamp(0.0, 1.0)
}

/// The fertility vector for a soil order at a depth (spec §3).
pub fn fertility(order: SoilOrder, depth: &SoilDepth) -> Fertility {
    let depth_suit = (depth.get() / 2.0).clamp(0.0, 1.0);
    let (grain_suit, moisture_suit) = match order {
        SoilOrder::Chernozem => (0.9, 0.8),
        SoilOrder::Loam | SoilOrder::Andosol => (0.85, 0.7),
        SoilOrder::Gley | SoilOrder::Histosol => (0.4, 0.95),
        SoilOrder::Podzol => (0.4, 0.5),
        SoilOrder::Laterite => (0.3, 0.6),
        SoilOrder::Aridisol => (0.3, 0.1),
        SoilOrder::Leptosol => (0.2, 0.2),
    };
    Fertility {
        grain_suit,
        moisture_suit,
        depth_suit,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globe::generate;
    use crate::pins::TerrainPins;
    use hornvale_kernel::color::BANDS;
    use hornvale_kernel::{Geosphere, Seed};

    /// A mid-valued buffer for exercising `classify_rock` in isolation.
    fn flat_buffer() -> MaterialBuffer {
        MaterialBuffer {
            silica: 0.5,
            grain: 0.5,
            induration: 0.5,
            carbonate: 0.0,
            metamorphic_grade: 0.0,
            porosity: 0.5,
            margin: MarginPolarity::Interior,
            soil_depth: SoilDepth::new(1.0),
            basement: Basement::Continental,
            thaumic: 0.0,
        }
    }

    #[test]
    fn classify_covers_the_setting_diagnostics() {
        // Metamorphic core -> gneiss.
        let mut b = flat_buffer();
        b.metamorphic_grade = 0.9;
        assert_eq!(classify_rock(&b, 1.0, false, false, 0.0), RockClass::Gneiss);
        // Warm carbonate shelf -> reef limestone.
        let mut b = flat_buffer();
        b.carbonate = 0.8;
        assert_eq!(
            classify_rock(&b, 1.0, false, false, 0.0),
            RockClass::ReefLimestone
        );
        // Arid endorheic basin -> evaporite.
        let b = flat_buffer();
        assert_eq!(
            classify_rock(&b, 1.0, true, false, 0.0),
            RockClass::Evaporite
        );
        // High-drainage lowland -> alluvium.
        let b = flat_buffer();
        assert_eq!(
            classify_rock(&b, 5000.0, false, false, 0.0),
            RockClass::Alluvium
        );
        // Mafic ocean floor -> basalt.
        let mut b = flat_buffer();
        b.silica = 0.1;
        assert_eq!(classify_rock(&b, 0.0, false, true, 0.0), RockClass::Basalt);
    }

    #[test]
    fn heavy_carve_sediment_reads_as_alluvium_even_below_the_drainage_gate() {
        // Low drainage, no carbonate/metamorphism — would otherwise resolve
        // to Sandstone/Shale/Conglomerate by grain alone — but the carve
        // buried it deep, so it reads as Alluvium (spec §2 stage 8).
        let b = flat_buffer();
        assert_eq!(
            classify_rock(&b, 1.0, false, false, 3.0),
            RockClass::Alluvium
        );
        // At the gate boundary (not strictly above it), the ordinary
        // classification still applies.
        assert_ne!(
            classify_rock(&b, 1.0, false, false, 2.0),
            RockClass::Alluvium
        );
    }

    #[test]
    fn every_seed_produces_at_least_three_rock_classes() {
        use std::collections::BTreeSet;
        let geo = Geosphere::new(4);
        let outcome = generate(Seed(7), &geo, &TerrainPins::default()).unwrap();
        let terrain = crate::GeneratedTerrain::new(geo.clone(), outcome);
        let classes: BTreeSet<_> = geo.vertices().map(|c| terrain.rock_at(c)).collect();
        assert!(classes.len() >= 3, "world felt monolithic: {classes:?}");
    }

    /// claim: reachability(seed: union over [1,7,42,99], not census-eligible)
    #[test]
    fn alluvium_and_coal_are_reachable_across_seeds() {
        use std::collections::BTreeSet;
        let mut classes = BTreeSet::new();
        for seed in [1u64, 7, 42, 99] {
            let geo = Geosphere::new(6);
            let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
            let terrain = crate::GeneratedTerrain::new(geo.clone(), outcome);
            for vertex in geo.vertices() {
                if !terrain.is_ocean(vertex) {
                    classes.insert(terrain.rock_at(vertex));
                }
            }
        }
        assert!(
            classes.contains(&RockClass::Alluvium),
            "no Alluvium across seeds [1, 7, 42, 99] at Geosphere::new(6): {classes:?}"
        );
        assert!(
            classes.contains(&RockClass::Coal),
            "no Coal across seeds [1, 7, 42, 99] at Geosphere::new(6): {classes:?}"
        );
        // Guard the neighboring clastic classes the recalibration must not
        // have starved.
        assert!(
            classes.contains(&RockClass::Sandstone),
            "no Sandstone: {classes:?}"
        );
        assert!(classes.contains(&RockClass::Shale), "no Shale: {classes:?}");
        assert!(
            classes.contains(&RockClass::Conglomerate),
            "no Conglomerate: {classes:?}"
        );
    }

    /// claim: reachability(seed: union over [1,7,42,99], not census-eligible)
    #[test]
    fn andesite_is_reachable_across_seeds() {
        use std::collections::BTreeSet;
        let mut classes = BTreeSet::new();
        for seed in [1u64, 7, 42, 99] {
            let geo = Geosphere::new(4);
            let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
            let terrain = crate::GeneratedTerrain::new(geo.clone(), outcome);
            for vertex in geo.vertices() {
                classes.insert(terrain.rock_at(vertex));
            }
        }
        assert!(
            classes.contains(&RockClass::Andesite),
            "no Andesite across seeds [1, 7, 42, 99]: {classes:?}"
        );
        // Guard the neighboring classes the active-margin reorder must not
        // have starved: Granite (stable continental interior), Basalt
        // (oceanic floor), and Rhyolite (the felsic end of the same arc
        // split) must all still appear.
        assert!(
            classes.contains(&RockClass::Granite),
            "no Granite: {classes:?}"
        );
        assert!(
            classes.contains(&RockClass::Basalt),
            "no Basalt: {classes:?}"
        );
        assert!(
            classes.contains(&RockClass::Rhyolite),
            "no Rhyolite: {classes:?}"
        );
    }

    /// claim: reachability(seed: union over [1,7,42,99], not census-eligible —
    /// domain-crate unit test, not a Settlements/Full census row)
    #[test]
    fn active_and_passive_margins_both_appear_across_seeds() {
        let mut saw_active = false;
        let mut saw_passive = false;
        for seed in [1u64, 7, 42, 99] {
            let geo = Geosphere::new(4);
            let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
            let lith = assemble_material(&geo, &outcome.value);
            for vertex in geo.vertices() {
                match lith.get(vertex).margin {
                    MarginPolarity::Active => saw_active = true,
                    MarginPolarity::Passive => saw_passive = true,
                    _ => {}
                }
            }
        }
        assert!(saw_active, "no Active margin across seeds [1, 7, 42, 99]");
        assert!(saw_passive, "no Passive margin across seeds [1, 7, 42, 99]");
    }

    #[test]
    fn buffer_axes_are_bounded_and_thaumic_is_zero() {
        let geo = Geosphere::new(4);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let lith = assemble_material(&geo, &outcome.value);
        for vertex in geo.vertices() {
            let b = *lith.get(vertex);
            for v in [
                b.silica,
                b.grain,
                b.induration,
                b.carbonate,
                b.metamorphic_grade,
                b.porosity,
            ] {
                assert!((0.0..=1.0).contains(&v), "axis out of range: {v}");
            }
            assert_eq!(
                b.thaumic, 0.0,
                "inert-tier thaumic must be identically zero"
            );
            assert!(b.soil_depth.get() >= 0.0);
        }
    }

    #[test]
    fn soil_orders_key_off_climate_then_parent() {
        let d = SoilDepth::new(1.5);
        // Hot + wet -> laterite regardless of parent.
        assert_eq!(
            classify_soil(RockClass::Granite, 27.0, 0.9, 5.0, &d),
            SoilOrder::Laterite
        );
        // Cold + moist -> podzol.
        assert_eq!(
            classify_soil(RockClass::Granite, -2.0, 0.6, 5.0, &d),
            SoilOrder::Podzol
        );
        // Temperate grassland moisture -> chernozem.
        assert_eq!(
            classify_soil(RockClass::Shale, 12.0, 0.45, 5.0, &d),
            SoilOrder::Chernozem
        );
        // Arid -> aridisol.
        assert_eq!(
            classify_soil(RockClass::Sandstone, 22.0, 0.1, 5.0, &d),
            SoilOrder::Aridisol
        );
        // Fresh volcanic parent -> andosol (fertile).
        assert_eq!(
            classify_soil(RockClass::Basalt, 15.0, 0.5, 5.0, &d),
            SoilOrder::Andosol
        );
        // Steep/thin -> leptosol.
        let thin = SoilDepth::new(0.1);
        assert_eq!(
            classify_soil(RockClass::Granite, 15.0, 0.5, 400.0, &thin),
            SoilOrder::Leptosol
        );
    }

    #[test]
    fn hydrogeology_reads_porosity_and_carbonate() {
        // NOTE: this buffer is hand-built and synthetic — it does not
        // certify that the real derivation can reach these branches (that
        // was F5's defect: the branches below were unreachable from
        // `assemble_material` for a thousand census seeds even though these
        // pure-function tests were green). See
        // `a_real_world_produces_a_porous_non_carbonate_vertex` for the
        // world-derived check, and the census's
        // `every_hydro_variant_is_reachable_somewhere_in_the_census`
        // (`windows/lab/tests/calibration.rs`, The Assay Task 8) for the
        // cross-seed reachability guard — formerly `hydro_witness.rs`'s
        // 8-seed sweep (Task 6), retired once the census carried the same
        // coverage over 1,000 worlds. `hydrogeology` is pointwise matrix
        // petrophysics only (The Witness, Task 5b) — it never returns
        // `Spring`; that promotion is a geometric contact tested in
        // `provider.rs`'s `promote_to_spring_only_touches_aquifer_with_a_lower_non_aquifer_neighbor`.
        // High carbonate + porosity -> karst; cave-proneness high.
        let mut b = flat_buffer();
        b.carbonate = 0.8;
        b.porosity = 0.8;
        assert_eq!(hydrogeology(&b, false), Hydro::Karst);
        assert!(cave_proneness(&b, 10.0) > 0.5);
        // Porous non-carbonate -> aquifer.
        let mut b = flat_buffer();
        b.porosity = 0.7;
        b.carbonate = 0.05;
        assert_eq!(hydrogeology(&b, false), Hydro::Aquifer);
        // Impermeable -> aquitard, near-zero cave-proneness.
        let mut b = flat_buffer();
        b.porosity = 0.05;
        assert_eq!(hydrogeology(&b, false), Hydro::Aquitard);
        assert!(cave_proneness(&b, 10.0) < 0.1);
    }

    /// Every `Hydro::name` is distinct and non-empty — the property the census's
    /// joined coverage string depends on, since two variants sharing a name would
    /// make one of them permanently invisible to the coverage assertion in
    /// `windows/lab/tests/calibration.rs`.
    #[test]
    fn hydro_names_are_distinct_and_nonempty() {
        let mut seen: std::collections::BTreeSet<&'static str> = std::collections::BTreeSet::new();
        for variant in Hydro::ALL {
            let name = variant.name();
            assert!(!name.is_empty(), "{variant:?} has an empty name");
            assert!(seen.insert(name), "{variant:?} reuses the name {name:?}");
        }
        assert_eq!(seen.len(), Hydro::ALL.len());
    }

    #[test]
    fn a_real_world_produces_a_porous_non_carbonate_vertex_in_bounded_shares() {
        // The defect this closes has two halves, and Task 5 shipped only a
        // fix for the first: `hydrogeology_reads_porosity_and_carbonate`
        // passed on hand-built `MaterialBuffer`s the real derivation could
        // not emit — `porosity` was gated at a carbonate-scale `0.5`, but
        // the derivation's clastic (non-carbonate) porosity maxed at 0.325
        // (The Witness, F5), so no land vertex on any seed could ever clear
        // it. Fixing that (a floor: `Aquifer`/`Spring` become reachable) is
        // NOT sufficient — Task 5's own fix, measured on the wrong
        // population, made 69.64% of land Aquifer, and a floor-only test
        // ("found >= 1") is exactly as green on that world as on this one.
        // The MISSING CEILING is why 69.64% shipped without reddening
        // anything (The Witness, Task 5b). This test therefore asserts a
        // floor AND a ceiling on both variants, at the production mesh
        // level (`GLOBE_LEVEL`, 6) real worlds actually build at — not a
        // golden (the exact share moves with terrain, seed, and mesh), but
        // a band wide enough to hold and tight enough to catch "ate the
        // world" or "regressed to unreachable." Measured at k_g=0.40,
        // thr=0.46 (The Witness, Task 5b): aquifer ~16.4% of land, spring
        // ~3.69% of land, forming lines along aquifer margins.
        //
        // AND THE BAND EARNED ITS KEEP (The Trencher, Task 13). Widening
        // `carbonate` and `metamorphic_grade` moved porosity; at thr=0.46
        // unchanged this read aquifer 36.6% / spring 10.1% and went RED on
        // both clauses, which is exactly the "ate the world" case the
        // ceiling was added for. Re-placed to thr=0.53 by The Witness's own
        // mid-band method (see `CLASTIC_AQUIFER_MIN_POROSITY`): aquifer
        // 15.77%, spring 7.36%. The spring figure has doubled and the
        // constant's doc records why — `Spring` tracks the aquifer set's
        // PERIMETER, and a continuous porosity field fragments that set.
        //
        // **THOSE TWO FIGURES ARE WRONG AND THIS TEST IS NEARER RED THAN THEY
        // SAY** (The Trencher, Task 14). Re-measured at the shipped `0.53`,
        // with the expressions below and nothing else, seed 0 reads **aquifer
        // 0.1978, spring 0.0782**. The spring ceiling is `0.08`: the margin is
        // 2.3%, not the 8% "7.36%" implies. The correction, its positive
        // control (the committed census's `aquifer-fraction` column, matched
        // to eight significant digits on the pre-Task-13 tree) and the reason
        // `0.53` nonetheless stands are in `CLASTIC_AQUIFER_MIN_POROSITY`'s
        // doc. Neither band is moved here — the spring ceiling was calibrated
        // against a porosity axis that no longer exists, and re-placing it is
        // a campaign decision, not a repair. Anything that moves porosity
        // should expect to red this assertion.
        let geo = Geosphere::new(6);
        let outcome = generate(Seed(0), &geo, &TerrainPins::default()).unwrap();
        let terrain = crate::GeneratedTerrain::new(geo.clone(), outcome);
        let land: Vec<Vertex> = geo.vertices().filter(|&c| !terrain.is_ocean(c)).collect();
        let land_count = land.len() as f64;
        let aquifer = land
            .iter()
            .filter(|&&c| terrain.hydro_at(c) == Hydro::Aquifer)
            .count() as f64;
        let spring = land
            .iter()
            .filter(|&&c| terrain.hydro_at(c) == Hydro::Spring)
            .count() as f64;
        let aquifer_share = aquifer / land_count;
        let spring_share = spring / land_count;
        assert!(
            (0.05..=0.35).contains(&aquifer_share),
            "aquifer share {aquifer_share:.4} outside the loose band [0.05, 0.35] \
             (0 means the branch regressed to unreachable; near 1 means it ate the world)"
        );
        assert!(
            (0.005..=0.08).contains(&spring_share),
            "spring share {spring_share:.4} outside the loose band [0.005, 0.08]"
        );
    }

    #[test]
    fn appearance_tracks_the_material_and_prospectivity_favors_arcs() {
        // Basalt reads dark (low albedo), fine, hard.
        let ap = appearance(
            &{
                let mut b = flat_buffer();
                b.silica = 0.1;
                b
            },
            RockClass::Basalt,
        );
        assert!(ap.albedo < 0.4 && ap.coarseness < 0.5);
        // Granite reads pale, coarse.
        let ap = appearance(
            &{
                let mut b = flat_buffer();
                b.silica = 0.8;
                b.grain = 0.8;
                b
            },
            RockClass::Granite,
        );
        assert!(ap.albedo > 0.5 && ap.coarseness > 0.5);
        // Prospectivity: island-arc + unrest scores higher than quiet interior.
        let b = flat_buffer();
        let arc = prospectivity(&b, Some(BoundaryKind::IslandArc), 0.8);
        let quiet = prospectivity(&b, None, 0.0);
        assert!(arc > quiet);
    }

    #[test]
    fn oceanic_vertices_are_mafic_active_margins_are_labeled() {
        let geo = Geosphere::new(4);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let lith = assemble_material(&geo, &outcome.value);
        // Oceanic floor (thin crust) reads low-silica (mafic) and Oceanic margin.
        let ocean = geo
            .vertices()
            .find(|c| *outcome.value.crust.get(*c) < crate::crust::CONTINENTAL_THRESHOLD_KM)
            .unwrap();
        assert!(lith.get(ocean).silica < 0.5);
        assert_eq!(lith.get(ocean).margin, MarginPolarity::Oceanic);
        // At least one continental vertex is a non-Oceanic margin.
        assert!(
            geo.vertices()
                .any(|c| lith.get(c).margin != MarginPolarity::Oceanic)
        );
    }

    // --- The reflectance projection (spec "The Pigment" §5.1) ---
    //
    // These reuse `flat_buffer()` above. `MaterialBuffer` is `Copy` and does
    // NOT derive `Default`, so struct-update syntax against `flat_buffer()`
    // is the only construction that compiles.

    #[test]
    fn a_felsic_rock_is_brighter_than_a_mafic_one() {
        let felsic = MaterialBuffer {
            silica: 0.95,
            ..flat_buffer()
        };
        let mafic = MaterialBuffer {
            silica: 0.05,
            ..flat_buffer()
        };
        let f = reflectance(&felsic, RockClass::Granite).integrate();
        let m = reflectance(&mafic, RockClass::Basalt).integrate();
        let f_mean: f64 = f.get().iter().sum::<f64>() / BANDS as f64;
        let m_mean: f64 = m.get().iter().sum::<f64>() / BANDS as f64;
        assert!(
            f_mean > m_mean,
            "felsic {f_mean} was not brighter than mafic {m_mean}"
        );
    }

    #[test]
    fn silica_alone_brightens_the_rock() {
        // The companion to the test above, holding the rock class FIXED so
        // the buffer's silica axis is the only thing that moved. Without
        // this, `a_felsic_rock_is_brighter_than_a_mafic_one` can be
        // satisfied entirely by `is_mafic_dominated` and the projection
        // could ignore `buf.silica` outright.
        let quartz_rich = MaterialBuffer {
            silica: 0.95,
            ..flat_buffer()
        };
        let quartz_poor = MaterialBuffer {
            silica: 0.05,
            ..flat_buffer()
        };
        let hi = reflectance(&quartz_rich, RockClass::Granite).integrate();
        let lo = reflectance(&quartz_poor, RockClass::Granite).integrate();
        let hi_mean: f64 = hi.get().iter().sum::<f64>() / BANDS as f64;
        let lo_mean: f64 = lo.get().iter().sum::<f64>() / BANDS as f64;
        assert!(
            hi_mean > lo_mean,
            "silica 0.95 gave {hi_mean}, silica 0.05 gave {lo_mean}"
        );
    }

    #[test]
    fn ironstone_leans_long_wavelength() {
        let buf = flat_buffer();
        let iron = reflectance(&buf, RockClass::Ironstone).integrate();
        let plain = reflectance(&buf, RockClass::Sandstone).integrate();
        // Long band over short band: iron oxide's whole visual signature.
        let iron_ratio = iron.get()[8] / iron.get()[2];
        let plain_ratio = plain.get()[8] / plain.get()[2];
        assert!(
            iron_ratio > plain_ratio,
            "ironstone long/short = {iron_ratio}, sandstone = {plain_ratio}"
        );
    }

    #[test]
    fn carbonate_brightens_the_whole_curve() {
        let none = MaterialBuffer {
            carbonate: 0.0,
            ..flat_buffer()
        };
        let lots = MaterialBuffer {
            carbonate: 0.9,
            ..flat_buffer()
        };
        let a = reflectance(&none, RockClass::Sandstone).integrate();
        let b = reflectance(&lots, RockClass::ReefLimestone).integrate();
        for band in 0..BANDS {
            assert!(
                b.get()[band] >= a.get()[band],
                "band {band}: carbonate darkened the rock"
            );
        }
    }

    #[test]
    fn every_reflectance_is_physically_valid_across_the_buffer_space() {
        // Reflectance::new rejects out-of-range bands, so a panic here is a
        // real energy-conservation break, not a test artifact.
        for silica in [0.0, 0.5, 1.0] {
            for carbonate in [0.0, 0.5, 1.0] {
                for rock in [RockClass::Granite, RockClass::Basalt, RockClass::Ironstone] {
                    let buf = MaterialBuffer {
                        silica,
                        carbonate,
                        ..flat_buffer()
                    };
                    let r = reflectance(&buf, rock).integrate();
                    for band in 0..BANDS {
                        assert!((0.0..=1.0).contains(&r.get()[band]));
                    }
                }
            }
        }
    }

    #[test]
    fn the_mixture_keeps_its_components_for_the_texture_layer() {
        // The producer must not collapse early: a later texture layer needs
        // the components to arrange them spatially. So assert on the
        // components themselves, not on the integrated result — an
        // integrate-only assertion would pass for a `Mixture` that had
        // already thrown its endmembers away.
        let m = reflectance(&flat_buffer(), RockClass::Granite);
        assert_eq!(m.components().len(), 4, "the four mineral endmembers");
        assert_eq!(m.weights().len(), m.components().len());
        // Weights come back unnormalized, and a flat buffer with no
        // carbonate leaves the two silicate endmembers carrying the rock.
        assert!(
            m.weights()[0] > 0.0 && m.weights()[1] > 0.0,
            "felsic and mafic both present: {:?}",
            m.weights()
        );
        assert_eq!(m.weights()[2], 0.0, "flat_buffer has no carbonate");
    }
}
