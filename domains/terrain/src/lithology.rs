//! The material buffer (The Ground, spec §2): a per-cell petrogenetic
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
//!   so mafic continental cells always read as extrusive Basalt/Andesite
//!   rather than plutonic Gabbro; that axis is Sculpting's to add. `Chert` —
//!   gated on abyssal very-low-porosity ocean cells, a niche combination the
//!   current ocean-floor ranges rarely produce. `Quartzite` — gated on
//!   `induration > 0.7` inside the low-metamorphic-grade band, which the
//!   current induration formula rarely reaches; widening the induration
//!   range would make it reachable without a new axis.

use crate::boundaries::BoundaryKind;
use crate::globe::TectonicGlobe;
use crate::plates::{Plate, dot, normalize, sub, velocity_at};
use hornvale_kernel::{CellId, CellMap, Geosphere, math, noise::fbm_2d};

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

/// A per-cell petrogenetic property vector — the material buffer.
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
const OROGEN_REACH: u32 = 4;

/// Drainage (flow-accumulation, upstream land-cell count) above which a land
/// cell reads as `Alluvium` rather than its ordinary clastic/igneous class.
/// Calibrated against the real distribution, not an imagined absolute: a
/// 4-seed (`[1, 7, 42, 99]`) survey at the canonical `Geosphere::new(6)`
/// found land drainage maxing out at 338 (p99 47, p95 18, p90 12, p50 3) —
/// nowhere near a round number like 1000. `20.0` sits just above p90 and
/// selects roughly the top 4-5% of land cells by accumulation: genuinely
/// high-flow valley bottoms, not merely-damp lowland.
const ALLUVIUM_DRAINAGE_MIN: f64 = 20.0;

/// Carve-deposited sediment thickness (metres) above which a land cell
/// reads as `Alluvium` regardless of drainage (spec §2 stage 8, Sculpting
/// Task 10): a cell the carve's routing/wedge/delta buried under real
/// alluvium is alluvial even when its flow accumulation alone would not
/// clear [`ALLUVIUM_DRAINAGE_MIN`] — a floodplain a river no longer
/// actively occupies still reads as its deposit.
const ALLUVIUM_SEDIMENT_MIN_M: f64 = 2.0;

/// Regolith depth (metres) above which waterlogged lowland accumulation
/// reads as `Coal` rather than ordinary clastic sediment, paired with
/// [`COAL_GRAIN_MAX`] so only the finer end of the grain range qualifies
/// (coal-forming peat accumulates in fine floodplain/deltaic muck, not
/// coarse gravel). A 4-seed survey at `Geosphere::new(6)` found roughly
/// 0.6% of clastic-eligible land cells clear both this and the grain gate —
/// present but appropriately rare for a biogenic, waterlogged-basin rock.
const COAL_SOIL_DEPTH_MIN: f64 = 1.25;

/// Grain ceiling paired with [`COAL_SOIL_DEPTH_MIN`]: excludes the coarse
/// (near-`Conglomerate`) tail of the clastic range from `Coal`, keeping the
/// two rocks' inputs (a slow, fine-sediment sink vs. relief-proximal debris)
/// distinct even though both draw from the same `grain <= 0.6` band.
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
/// `sediment_m` is the carve's deposited sediment thickness at the cell
/// (Sculpting Task 10): a cell the carve buried under real alluvium reads
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
    // arc cells are extrusive/intermediate by genesis, so they resolve to
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hydro {
    /// Porous, holds water: wells, oases.
    Aquifer,
    /// Impermeable: perched water, seeps.
    Aquitard,
    /// Where an aquifer meets the surface with flow.
    Spring,
    /// Sheds water: thin-soil runoff.
    Runoff,
    /// Dissolving carbonate: caves, sinkholes.
    Karst,
}

/// Classify hydrogeology from porosity/carbonate × drainage (spec §3).
/// type-audit: bare-ok(count: drainage), bare-ok(flag: ocean)
pub fn hydrogeology(buf: &MaterialBuffer, drainage: f64, ocean: bool) -> Hydro {
    if ocean {
        return Hydro::Aquitard;
    }
    if buf.carbonate > 0.5 && buf.porosity > 0.4 {
        return Hydro::Karst;
    }
    if buf.porosity < 0.15 {
        return Hydro::Aquitard;
    }
    if buf.porosity > 0.5 {
        return if drainage > SPRING_DRAINAGE_THRESHOLD {
            Hydro::Spring
        } else {
            Hydro::Aquifer
        };
    }
    Hydro::Runoff
}

/// Drainage above which a porous cell expresses as a flowing `Spring` rather
/// than a still `Aquifer`. Shares scale with `cave_proneness`'s wetting term.
const SPRING_DRAINAGE_THRESHOLD: f64 = 500.0;

/// Void-proneness (caves/sinkholes), `[0,1]` (spec §3, negation "solid → void").
/// Dominated by the carbonate/porosity product (dissolution needs both
/// soluble rock and connected voids); drainage is a secondary modifier —
/// caves also form under slow diffuse flow, so wetting nudges rather than
/// gates the base rate.
/// type-audit: bare-ok(ratio: return), bare-ok(count: drainage)
pub fn cave_proneness(buf: &MaterialBuffer, drainage: f64) -> f64 {
    let wetting = (drainage / SPRING_DRAINAGE_THRESHOLD).min(1.0);
    (buf.carbonate * buf.porosity * (0.85 + 0.15 * wetting)).clamp(0.0, 1.0)
}

/// Induration/hardness at a cell, `[0,1]`: 0 soft (shale/soil) → 1 hard
/// (quartzite/gneiss). The Sculpting/Ground seam (spec §4): pulled out of
/// `assemble_material` as a standalone pre-elevation function so the globe
/// can compute it before `generate_elevation` runs, ahead of any elevation
/// carve that later wants to read hardness. Exact expressions mirror
/// `assemble_material`'s `grain`/`metamorphic_grade`/`induration` locals —
/// this function and that buffer axis must never diverge.
///
/// Total at the extremes (spec §4): defined for the full `[0,1]` input
/// range; the gated metaphysics overlay may inject sentinel values later
/// without a formula change.
/// type-audit: bare-ok(ratio: return), bare-ok(ratio: crust_age), bare-ok(flag: continental), bare-ok(count: boundary_hops)
pub fn induration_at(
    crust_age: f64,
    continental: bool,
    boundary_kind: Option<BoundaryKind>,
    boundary_hops: Option<u32>,
) -> f64 {
    // Old cratons are more evolved/coarse; young crust finer.
    let grain = if continental {
        0.4 + 0.5 * crust_age
    } else {
        0.2
    };
    // Boundary influence.
    let near_orogen = matches!(
        boundary_kind,
        Some(BoundaryKind::ContinentalCollision) | Some(BoundaryKind::CoastalRange)
    ) || boundary_hops.is_some_and(|h| h <= OROGEN_REACH);
    let metamorphic_grade = if continental && near_orogen {
        (1.0 - boundary_hops.unwrap_or(OROGEN_REACH) as f64 / OROGEN_REACH as f64).clamp(0.0, 1.0)
    } else {
        0.0
    };
    // Induration: metamorphics/old plutons hard; young/soft sediments low.
    (0.35 + 0.4 * metamorphic_grade + 0.2 * grain).clamp(0.0, 1.0)
}

/// Carbonate content at a cell, `[0,1]` (spec §2/§4 pre-elevation seam,
/// mirroring [`induration_at`]): favors warm shallow shelves —
/// approximated by shallow continental crust (within 6 km of the
/// continental threshold) at low absolute latitude. Pointwise inputs only
/// (continental flag, crust thickness, latitude), all available before
/// elevation runs, so the globe can build a `carbonate_pre` field the carve
/// reads ahead of the carve's own elevation output. `assemble_material`
/// calls the same function so the buffer's `carbonate` axis and the pre-carve
/// field can never diverge.
/// type-audit: bare-ok(flag: continental), bare-ok(ratio: thickness_km), bare-ok(ratio: lat), bare-ok(ratio: return)
pub(crate) fn carbonate_at(continental: bool, thickness_km: f64, lat: f64) -> f64 {
    let shallow_shelf = continental && thickness_km < crate::crust::CONTINENTAL_THRESHOLD_KM + 6.0;
    if shallow_shelf && lat < 0.6 {
        0.7
    } else {
        0.05
    }
}

/// Assemble the material buffer over the canonical grid (spec §2). Pointwise
/// axes derive from crust age/thickness and plate motion; grid-bound terms
/// (metamorphic grade near boundaries, soil depth from slope/drainage) use
/// the globe's boundary-distance and drainage fields. No draws.
pub fn assemble_material(geo: &Geosphere, globe: &TectonicGlobe) -> CellMap<MaterialBuffer> {
    CellMap::from_fn(geo, |cell| {
        let thickness = *globe.crust.get(cell);
        let continental = thickness >= crate::crust::CONTINENTAL_THRESHOLD_KM;
        let age = *globe.crust_age.get(cell);
        let p = geo.position(cell);
        // Margin polarity is needed before silica: arc (active-margin)
        // magmatism is intermediate, not felsic (see base_silica below).
        let plate = &globe.plates[*globe.plate_of.get(cell) as usize];
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
        // Boundary influence.
        let boundary = *globe.boundary.get(cell);
        let hops = globe.boundary_distance.get(cell).map(|(h, _)| h);
        let near_orogen = matches!(
            boundary.map(|b| b.kind),
            Some(BoundaryKind::ContinentalCollision) | Some(BoundaryKind::CoastalRange)
        ) || hops.is_some_and(|h| h <= OROGEN_REACH);
        let metamorphic_grade = if continental && near_orogen {
            (1.0 - hops.unwrap_or(OROGEN_REACH) as f64 / OROGEN_REACH as f64).clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Sub-cell patchiness from existing noise (no draws): perturb silica.
        let patch = fbm_2d(globe.lithology_noise_seed(), p[0] * 6.0, p[1] * 6.0, 3) - 0.5;
        let silica = (base_silica + 0.15 * patch).clamp(0.0, 1.0);

        // Carbonate favors warm shallow shelves — same pre-elevation
        // function the globe computes ahead of the carve (the Sculpting/
        // Ground seam) — kept identical here so the buffer's axis and the
        // pre-carve field never diverge. Atoll cells (Sculpting Task 9/10:
        // reef caps grown over a drowned seamount the carve capped) always
        // override to a high carbonate reading, a biogenic reef regardless
        // of the pointwise shelf test.
        let lat = math::asin(p[2].clamp(-1.0, 1.0)).abs();
        let carbonate = if globe.atoll_cells.contains(&cell) {
            0.9
        } else {
            carbonate_at(continental, thickness, lat)
        };

        // Induration: same pre-elevation function the globe computes ahead
        // of elevation (the Sculpting/Ground seam) — kept identical here so
        // the buffer's axis and the globe's standalone field never diverge.
        let induration = induration_at(age, continental, boundary.map(|b| b.kind), hops);
        // Porosity: high in carbonate (karst) and young oceanic basalt, low in shale/gneiss.
        let porosity = (0.5 * carbonate + 0.3 * (1.0 - metamorphic_grade)).clamp(0.0, 1.0);

        let sediment_m = *globe.sediment_thickness.get(cell);
        let soil_depth = soil_depth_at(geo, globe, cell, sediment_m);
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
            thaumic: 0.0,
        }
    })
}

/// Active if the plate's surface motion at the cell points *outward* (leading
/// edge), passive if inward (trailing), Interior if neither dominates.
/// Pointwise inputs only (mirroring [`induration_at`]/[`carbonate_at`]): a
/// plate reference and a cell position, both available before elevation
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
    // Outward component = velocity · (direction from plate seed to cell).
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
    cell: CellId,
    sediment_m: f64,
) -> SoilDepth {
    if *globe.elevation.get(cell) < globe.sea_level {
        return SoilDepth::new(0.0);
    }
    let here = globe.elevation.get(cell).get();
    let max_drop = geo
        .neighbors(cell)
        .iter()
        .map(|n| here - globe.elevation.get(*n).get())
        .fold(0.0_f64, f64::max);
    let drainage = *globe.drainage.get(cell);
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
        let classes: BTreeSet<_> = geo.cells().map(|c| terrain.rock_at(c)).collect();
        assert!(classes.len() >= 3, "world felt monolithic: {classes:?}");
    }

    #[test]
    fn alluvium_and_coal_are_reachable_across_seeds() {
        use std::collections::BTreeSet;
        let mut classes = BTreeSet::new();
        for seed in [1u64, 7, 42, 99] {
            let geo = Geosphere::new(6);
            let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
            let terrain = crate::GeneratedTerrain::new(geo.clone(), outcome);
            for cell in geo.cells() {
                if !terrain.is_ocean(cell) {
                    classes.insert(terrain.rock_at(cell));
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

    #[test]
    fn andesite_is_reachable_across_seeds() {
        use std::collections::BTreeSet;
        let mut classes = BTreeSet::new();
        for seed in [1u64, 7, 42, 99] {
            let geo = Geosphere::new(4);
            let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
            let terrain = crate::GeneratedTerrain::new(geo.clone(), outcome);
            for cell in geo.cells() {
                classes.insert(terrain.rock_at(cell));
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

    #[test]
    fn active_and_passive_margins_both_appear_across_seeds() {
        let mut saw_active = false;
        let mut saw_passive = false;
        for seed in [1u64, 7, 42, 99] {
            let geo = Geosphere::new(4);
            let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
            let lith = assemble_material(&geo, &outcome.globe);
            for cell in geo.cells() {
                match lith.get(cell).margin {
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
        let lith = assemble_material(&geo, &outcome.globe);
        for cell in geo.cells() {
            let b = *lith.get(cell);
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
        // High carbonate + porosity -> karst; cave-proneness high.
        let mut b = flat_buffer();
        b.carbonate = 0.8;
        b.porosity = 0.8;
        assert_eq!(hydrogeology(&b, 10.0, false), Hydro::Karst);
        assert!(cave_proneness(&b, 10.0) > 0.5);
        // Porous non-carbonate + flow -> aquifer.
        let mut b = flat_buffer();
        b.porosity = 0.7;
        b.carbonate = 0.05;
        assert_eq!(hydrogeology(&b, 200.0, false), Hydro::Aquifer);
        // Impermeable -> aquitard, near-zero cave-proneness.
        let mut b = flat_buffer();
        b.porosity = 0.05;
        assert_eq!(hydrogeology(&b, 10.0, false), Hydro::Aquitard);
        assert!(cave_proneness(&b, 10.0) < 0.1);
    }

    #[test]
    fn high_porosity_with_flow_and_low_carbonate_reads_as_spring() {
        // Porous, non-carbonate (so not Karst), with drainage above the
        // spring threshold -> Spring rather than the still-water Aquifer.
        let mut b = flat_buffer();
        b.porosity = 0.7;
        b.carbonate = 0.05;
        assert_eq!(hydrogeology(&b, 600.0, false), Hydro::Spring);
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
    fn oceanic_cells_are_mafic_active_margins_are_labeled() {
        let geo = Geosphere::new(4);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let lith = assemble_material(&geo, &outcome.globe);
        // Oceanic floor (thin crust) reads low-silica (mafic) and Oceanic margin.
        let ocean = geo
            .cells()
            .find(|c| *outcome.globe.crust.get(*c) < crate::crust::CONTINENTAL_THRESHOLD_KM)
            .unwrap();
        assert!(lith.get(ocean).silica < 0.5);
        assert_eq!(lith.get(ocean).margin, MarginPolarity::Oceanic);
        // At least one continental cell is a non-Oceanic margin.
        assert!(
            geo.cells()
                .any(|c| lith.get(c).margin != MarginPolarity::Oceanic)
        );
    }
}
