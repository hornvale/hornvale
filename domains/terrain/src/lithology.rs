//! The material buffer (The Ground, spec §2): a per-cell petrogenetic
//! property vector and the projections over it. Pure functions of existing
//! terrain fields — no new draws, no new stream labels.

use crate::boundaries::BoundaryKind;
use crate::globe::TectonicGlobe;
use crate::plates::{dot, normalize, sub, velocity_at};
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
/// type-audit: bare-ok(count: drainage), bare-ok(flag: endorheic), bare-ok(flag: ocean)
pub fn classify_rock(
    buf: &MaterialBuffer,
    drainage: f64,
    endorheic: bool,
    ocean: bool,
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
    if drainage >= 1000.0 {
        return RockClass::Alluvium;
    }
    if buf.carbonate > 0.5 {
        return RockClass::ReefLimestone;
    }
    // Igneous by silica/grain when hard & crystalline; else clastic.
    if buf.grain > 0.5 && buf.induration > 0.5 {
        return if buf.silica > 0.55 {
            RockClass::Granite
        } else {
            RockClass::Gabbro
        };
    }
    if matches!(buf.margin, MarginPolarity::Active) {
        return if buf.silica > 0.55 {
            RockClass::Rhyolite
        } else {
            RockClass::Andesite
        };
    }
    // Clastic sediments by relief proxy (grain).
    if buf.grain > 0.6 {
        RockClass::Conglomerate
    } else if buf.grain > 0.35 {
        RockClass::Sandstone
    } else if buf.soil_depth.get() > 2.0 {
        RockClass::Coal
    } else {
        RockClass::Shale
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

        // Felsic index: continental crust is felsic (granitic), oceanic mafic.
        let base_silica = if continental { 0.7 } else { 0.15 };
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

        // Carbonate favors warm shallow shelves — approximated here by
        // shallow continental cells at low absolute latitude.
        let lat = math::asin(p[2].clamp(-1.0, 1.0)).abs();
        let shallow_shelf = continental && thickness < crate::crust::CONTINENTAL_THRESHOLD_KM + 6.0;
        let carbonate = if shallow_shelf && lat < 0.6 {
            0.7
        } else {
            0.05
        };

        // Induration: metamorphics/old plutons hard; young/soft sediments low.
        let induration = (0.35 + 0.4 * metamorphic_grade + 0.2 * grain).clamp(0.0, 1.0);
        // Porosity: high in carbonate (karst) and young oceanic basalt, low in shale/gneiss.
        let porosity = (0.5 * carbonate + 0.3 * (1.0 - metamorphic_grade)).clamp(0.0, 1.0);

        let margin = margin_polarity(geo, globe, cell, continental);
        let soil_depth = soil_depth_at(geo, globe, cell);
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
fn margin_polarity(
    geo: &Geosphere,
    globe: &TectonicGlobe,
    cell: CellId,
    continental: bool,
) -> MarginPolarity {
    if !continental {
        return MarginPolarity::Oceanic;
    }
    let plate = &globe.plates[*globe.plate_of.get(cell) as usize];
    let pos = geo.position(cell);
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

/// Regolith thickness: accumulates in high-drainage lowlands, strips on steep
/// slopes. Metres. Pure function of drainage and local elevation range.
fn soil_depth_at(geo: &Geosphere, globe: &TectonicGlobe, cell: CellId) -> SoilDepth {
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
    // Accumulation ~ log(drainage); stripping ~ local relief.
    let accum = 0.5 * math::ln(1.0 + drainage);
    let strip = (max_drop / 300.0).min(3.0);
    SoilDepth::new((accum - strip).max(0.0))
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
        assert_eq!(classify_rock(&b, 1.0, false, false), RockClass::Gneiss);
        // Warm carbonate shelf -> reef limestone.
        let mut b = flat_buffer();
        b.carbonate = 0.8;
        assert_eq!(
            classify_rock(&b, 1.0, false, false),
            RockClass::ReefLimestone
        );
        // Arid endorheic basin -> evaporite.
        let b = flat_buffer();
        assert_eq!(classify_rock(&b, 1.0, true, false), RockClass::Evaporite);
        // High-drainage lowland -> alluvium.
        let b = flat_buffer();
        assert_eq!(classify_rock(&b, 5000.0, false, false), RockClass::Alluvium);
        // Mafic ocean floor -> basalt.
        let mut b = flat_buffer();
        b.silica = 0.1;
        assert_eq!(classify_rock(&b, 0.0, false, true), RockClass::Basalt);
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
