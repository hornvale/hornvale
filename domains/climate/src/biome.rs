//! Biomes: a queryable field over the globe, derived per cell from
//! temperature, moisture, elevation, and (for the sea) depth, surface
//! temperature, and seafloor features. Land follows a Whittaker lookup with
//! ice/alpine specials; marine follows depth/SST/boundary/upwelling. Biomes
//! are never committed as facts (spec §3, §6) — the tier-0 `biome` fact stays
//! with the Vale.

use crate::facets::{BiomeExpr, Formation, Realm, Stratum};
use hornvale_kernel::{ReferenceElevation, Temperature};

/// A seafloor tectonic feature at an ocean cell (climate-owned; the
/// composition root maps `terrain::BoundaryKind` into this so climate imports
/// no domain).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SeafloorFeature {
    /// No notable boundary feature.
    None,
    /// A deep trench (ocean–ocean convergent subduction).
    Trench,
    /// A spreading ridge with hydrothermal vents (oceanic divergent).
    Ridge,
}

/// A biome class — terrestrial or marine.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Biome {
    /// Permanent land ice.
    Ice,
    /// Treeless cold ground.
    Tundra,
    /// Boreal coniferous forest.
    Taiga,
    /// Temperate grassland / steppe.
    TemperateGrassland,
    /// Dry temperate shrubland.
    Shrubland,
    /// Temperate broadleaf forest.
    TemperateForest,
    /// Wet temperate rainforest.
    TemperateRainforest,
    /// Hot desert.
    Desert,
    /// Tropical grassland with scattered trees.
    Savanna,
    /// Tropical forest with a dry season.
    TropicalSeasonalForest,
    /// Tropical rainforest.
    TropicalRainforest,
    /// Bare high ground above the tree line.
    Alpine,
    /// Frozen sea surface.
    SeaIce,
    /// Warm shallow reef.
    CoralReef,
    /// Cold shallow kelp forest.
    KelpForest,
    /// A hydrothermal-vent field on a spreading ridge.
    HydrothermalVent,
    /// A hadal ocean trench.
    HadalTrench,
    /// A wind-driven coastal upwelling zone (high productivity).
    Upwelling,
    /// Sunlit surface waters (0–200 m).
    Epipelagic,
    /// Twilight waters (200–1000 m).
    Mesopelagic,
    /// Dark waters (1000–4000 m).
    Bathypelagic,
    /// The abyssal plain (4000–6000 m).
    Abyssal,
}

/// Every biome variant, in declaration order — the single source of truth
/// for anything that must enumerate all biomes (name-uniqueness tests,
/// concept registration) rather than hand-listing them twice.
pub const ALL: &[Biome] = &[
    Biome::Ice,
    Biome::Tundra,
    Biome::Taiga,
    Biome::TemperateGrassland,
    Biome::Shrubland,
    Biome::TemperateForest,
    Biome::TemperateRainforest,
    Biome::Desert,
    Biome::Savanna,
    Biome::TropicalSeasonalForest,
    Biome::TropicalRainforest,
    Biome::Alpine,
    Biome::SeaIce,
    Biome::CoralReef,
    Biome::KelpForest,
    Biome::HydrothermalVent,
    Biome::HadalTrench,
    Biome::Upwelling,
    Biome::Epipelagic,
    Biome::Mesopelagic,
    Biome::Bathypelagic,
    Biome::Abyssal,
];

/// The tree line in meters at a latitude: 4000 m at the equator, falling
/// 40 m per degree, floored at 0.
/// type-audit: pending(wave-2)
pub fn tree_line_m(latitude_deg: f64) -> f64 {
    (4000.0 - 40.0 * latitude_deg.abs()).max(0.0)
}

/// Ice threshold: annual-mean below this is permanent ice.
const ICE_C: f64 = -20.0;

impl Biome {
    /// True for the marine variants.
    /// type-audit: bare-ok(flag)
    pub fn is_marine(self) -> bool {
        matches!(
            self,
            Biome::SeaIce
                | Biome::CoralReef
                | Biome::KelpForest
                | Biome::HydrothermalVent
                | Biome::HadalTrench
                | Biome::Upwelling
                | Biome::Epipelagic
                | Biome::Mesopelagic
                | Biome::Bathypelagic
                | Biome::Abyssal
        )
    }

    /// Every biome, in declaration order — the stable legend order for
    /// `scene/tiles` (scene-protocol spec §2). Appending a variant appends
    /// a legend entry; never reorder.
    pub fn catalog() -> &'static [Biome] {
        const CATALOG: [Biome; 22] = [
            Biome::Ice,
            Biome::Tundra,
            Biome::Taiga,
            Biome::TemperateGrassland,
            Biome::Shrubland,
            Biome::TemperateForest,
            Biome::TemperateRainforest,
            Biome::Desert,
            Biome::Savanna,
            Biome::TropicalSeasonalForest,
            Biome::TropicalRainforest,
            Biome::Alpine,
            Biome::SeaIce,
            Biome::CoralReef,
            Biome::KelpForest,
            Biome::HydrothermalVent,
            Biome::HadalTrench,
            Biome::Upwelling,
            Biome::Epipelagic,
            Biome::Mesopelagic,
            Biome::Bathypelagic,
            Biome::Abyssal,
        ];
        &CATALOG
    }

    /// Compile-time tripwire: adding a `Biome` variant fails this match
    /// until `catalog()` above gains the new entry. Never remove.
    #[allow(dead_code)]
    fn catalog_must_grow_with_the_enum(b: Biome) {
        match b {
            Biome::Ice
            | Biome::Tundra
            | Biome::Taiga
            | Biome::TemperateGrassland
            | Biome::Shrubland
            | Biome::TemperateForest
            | Biome::TemperateRainforest
            | Biome::Desert
            | Biome::Savanna
            | Biome::TropicalSeasonalForest
            | Biome::TropicalRainforest
            | Biome::Alpine
            | Biome::SeaIce
            | Biome::CoralReef
            | Biome::KelpForest
            | Biome::HydrothermalVent
            | Biome::HadalTrench
            | Biome::Upwelling
            | Biome::Epipelagic
            | Biome::Mesopelagic
            | Biome::Bathypelagic
            | Biome::Abyssal => (),
        }
    }

    /// The canonical kebab-case name (Lab metrics, CSV, book prose).
    /// type-audit: bare-ok(identifier-text)
    pub fn name(self) -> &'static str {
        match self {
            Biome::Ice => "ice",
            Biome::Tundra => "tundra",
            Biome::Taiga => "taiga",
            Biome::TemperateGrassland => "temperate-grassland",
            Biome::Shrubland => "shrubland",
            Biome::TemperateForest => "temperate-forest",
            Biome::TemperateRainforest => "temperate-rainforest",
            Biome::Desert => "desert",
            Biome::Savanna => "savanna",
            Biome::TropicalSeasonalForest => "tropical-seasonal-forest",
            Biome::TropicalRainforest => "tropical-rainforest",
            Biome::Alpine => "alpine",
            Biome::SeaIce => "sea-ice",
            Biome::CoralReef => "coral-reef",
            Biome::KelpForest => "kelp-forest",
            Biome::HydrothermalVent => "hydrothermal-vent",
            Biome::HadalTrench => "hadal-trench",
            Biome::Upwelling => "upwelling",
            Biome::Epipelagic => "epipelagic",
            Biome::Mesopelagic => "mesopelagic",
            Biome::Bathypelagic => "bathypelagic",
            Biome::Abyssal => "abyssal",
        }
    }

    /// The concept-registry name for this biome (kebab-case; same string as
    /// [`Biome::name`], named separately so concept registration reads as
    /// its own concern rather than reaching into rendering).
    /// type-audit: bare-ok(identifier-text)
    pub fn concept_name(self) -> &'static str {
        self.name()
    }

    /// A single ASCII glyph for the REPL biome map.
    /// type-audit: bare-ok(artifact)
    pub fn glyph(self) -> char {
        match self {
            Biome::Ice | Biome::SeaIce => '*',
            Biome::Tundra => ',',
            Biome::Taiga => 't',
            Biome::TemperateGrassland => '"',
            Biome::Shrubland => ';',
            Biome::TemperateForest => 'f',
            Biome::TemperateRainforest => 'F',
            Biome::Desert => '.',
            Biome::Savanna => ':',
            Biome::TropicalSeasonalForest => 'w',
            Biome::TropicalRainforest => 'W',
            Biome::Alpine => '^',
            Biome::CoralReef => 'o',
            Biome::KelpForest => 'k',
            Biome::HydrothermalVent => 'v',
            Biome::HadalTrench => '#',
            Biome::Upwelling => '=',
            Biome::Epipelagic => '~',
            Biome::Mesopelagic => '-',
            Biome::Bathypelagic => '_',
            Biome::Abyssal => ' ',
        }
    }

    /// An RGB color for the PNG biome map.
    /// type-audit: bare-ok(artifact)
    pub fn color(self) -> [u8; 3] {
        match self {
            Biome::Ice => [235, 235, 245],
            Biome::Tundra => [170, 175, 155],
            Biome::Taiga => [70, 105, 80],
            Biome::TemperateGrassland => [160, 180, 100],
            Biome::Shrubland => [155, 150, 95],
            Biome::TemperateForest => [60, 130, 70],
            Biome::TemperateRainforest => [35, 100, 60],
            Biome::Desert => [210, 195, 130],
            Biome::Savanna => [180, 165, 85],
            Biome::TropicalSeasonalForest => [90, 150, 65],
            Biome::TropicalRainforest => [25, 110, 45],
            Biome::Alpine => [150, 140, 135],
            Biome::SeaIce => [220, 230, 240],
            Biome::CoralReef => [230, 150, 160],
            Biome::KelpForest => [40, 90, 95],
            Biome::HydrothermalVent => [120, 60, 90],
            Biome::HadalTrench => [10, 15, 45],
            Biome::Upwelling => [60, 160, 170],
            Biome::Epipelagic => [70, 140, 200],
            Biome::Mesopelagic => [45, 95, 160],
            Biome::Bathypelagic => [25, 55, 110],
            Biome::Abyssal => [12, 30, 70],
        }
    }
}

/// Classify a land cell. Specials first (ice below `ICE_C`, alpine above the
/// tree line), then a Whittaker lookup on (annual-mean temperature, moisture).
/// type-audit: bare-ok(ratio: moisture), pending(wave-2: latitude_deg)
pub fn classify_land(
    temp_c: Temperature,
    moisture: f64,
    elevation_m: ReferenceElevation,
    sea_level_m: ReferenceElevation,
    latitude_deg: f64,
) -> Biome {
    let ice_c = Temperature::new(ICE_C).expect("ice threshold is finite");
    if temp_c < ice_c {
        return Biome::Ice;
    }
    if elevation_m - sea_level_m > tree_line_m(latitude_deg) {
        return Biome::Alpine;
    }
    let freeze_c = Temperature::new(0.0).expect("freeze threshold is finite");
    let taiga_c = Temperature::new(7.0).expect("taiga threshold is finite");
    let temperate_c = Temperature::new(20.0).expect("temperate threshold is finite");
    if temp_c < freeze_c {
        // Cold: dry tundra, wetter taiga.
        if moisture < 0.35 {
            Biome::Tundra
        } else {
            Biome::Taiga
        }
    } else if temp_c < taiga_c {
        if moisture < 0.3 {
            Biome::Tundra
        } else {
            Biome::Taiga
        }
    } else if temp_c < temperate_c {
        // Temperate.
        if moisture < 0.25 {
            Biome::TemperateGrassland
        } else if moisture < 0.4 {
            Biome::Shrubland
        } else if moisture < 0.75 {
            Biome::TemperateForest
        } else {
            Biome::TemperateRainforest
        }
    } else {
        // Hot.
        if moisture < 0.2 {
            Biome::Desert
        } else if moisture < 0.45 {
            Biome::Savanna
        } else if moisture < 0.7 {
            Biome::TropicalSeasonalForest
        } else {
            Biome::TropicalRainforest
        }
    }
}

/// Frozen-surface threshold (°C).
const SEA_ICE_C: f64 = -2.0;

/// Classify a marine cell by depth, surface temperature, seafloor feature,
/// and upwelling, in precedence order (see the task's interface note).
/// type-audit: pending(wave-2: depth_m), bare-ok(flag: upwelling)
pub fn classify_marine(
    depth_m: f64,
    sst_c: Temperature,
    feature: SeafloorFeature,
    upwelling: bool,
) -> Biome {
    classify_marine_expr(depth_m, sst_c, feature, upwelling).biome()
}

/// [`classify_marine`] as a faceted expression. The legacy function delegates
/// to this, so the two cannot drift apart.
///
/// The precedence chain below is the legacy one, deliberately unchanged. Two
/// arms look like bugs and are not: a deep trench is tested BEFORE a ridge (so
/// a cell that is both is hadal open water, not a vent), and the shallow band
/// matches reef above 20 °C and kelp below 12 °C, leaving 12–20 °C to fall
/// through to the arms beneath. Both are current behaviour, and the seed-42
/// world fixture will catch any tidying of either.
///
/// What *has* changed is that depth no longer competes with community for the
/// single return slot: the stratum is derived independently, so a vent is a
/// community AT a depth rather than one that displaced a depth.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(flag: upwelling)
pub fn classify_marine_expr(
    depth_m: f64,
    sst_c: Temperature,
    feature: SeafloorFeature,
    upwelling: bool,
) -> BiomeExpr {
    let stratum = Stratum::at_depth_m(depth_m);
    let sea_ice_c = Temperature::new(SEA_ICE_C).expect("sea-ice threshold is finite");
    let reef_c = Temperature::new(20.0).expect("reef threshold is finite");
    let kelp_c = Temperature::new(12.0).expect("kelp threshold is finite");
    let formation = if sst_c < sea_ice_c {
        Formation::SeaIce
    } else if feature == SeafloorFeature::Trench && depth_m > 6000.0 {
        Formation::OpenWater
    } else if feature == SeafloorFeature::Ridge {
        Formation::Vent
    } else if depth_m < 200.0 && sst_c > reef_c {
        Formation::Reef
    } else if depth_m < 200.0 && sst_c < kelp_c {
        Formation::KelpForest
    } else if upwelling && depth_m < 1000.0 {
        Formation::Upwelling
    } else {
        Formation::OpenWater
    };
    BiomeExpr {
        realm: Realm::WATERWORLD,
        formation,
        stratum,
    }
}

/// Classify any cell: marine when below sea level (depth = sea_level − elev),
/// otherwise land. `sst_c` is the surface temperature used for marine cells.
/// type-audit: bare-ok(ratio: moisture), pending(wave-2: latitude_deg), bare-ok(flag: upwelling)
#[allow(clippy::too_many_arguments)]
pub fn classify(
    temp_c: Temperature,
    moisture: f64,
    sst_c: Temperature,
    elevation_m: ReferenceElevation,
    sea_level_m: ReferenceElevation,
    latitude_deg: f64,
    feature: SeafloorFeature,
    upwelling: bool,
) -> Biome {
    classify_expr(
        temp_c,
        moisture,
        sst_c,
        elevation_m,
        sea_level_m,
        latitude_deg,
        feature,
        upwelling,
    )
    .biome()
}

/// [`classify`] as a faceted expression; the legacy function delegates here.
/// The marine/land split and the land lookup are the legacy body verbatim —
/// only the return type changes.
/// type-audit: bare-ok(ratio: moisture), pending(wave-2: latitude_deg), bare-ok(flag: upwelling)
#[allow(clippy::too_many_arguments)]
pub fn classify_expr(
    temp_c: Temperature,
    moisture: f64,
    sst_c: Temperature,
    elevation_m: ReferenceElevation,
    sea_level_m: ReferenceElevation,
    latitude_deg: f64,
    feature: SeafloorFeature,
    upwelling: bool,
) -> BiomeExpr {
    if elevation_m < sea_level_m {
        classify_marine_expr(sea_level_m - elevation_m, sst_c, feature, upwelling)
    } else {
        let land = classify_land(temp_c, moisture, elevation_m, sea_level_m, latitude_deg);
        BiomeExpr {
            realm: Realm::OVERWORLD,
            formation: crate::facets::land_formation(land),
            stratum: Stratum::Surface,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn catalog_is_complete_and_distinct() {
        let catalog = Biome::catalog();
        assert_eq!(catalog.len(), 22);
        let names: std::collections::BTreeSet<&str> = catalog.iter().map(|b| b.name()).collect();
        assert_eq!(names.len(), catalog.len(), "duplicate biome in catalog");
    }

    /// Test-only helper: a validated `ReferenceElevation`.
    fn e(m: f64) -> ReferenceElevation {
        ReferenceElevation::new(m).unwrap()
    }

    /// Test-only helper: a validated `Temperature`.
    fn t(c: f64) -> Temperature {
        Temperature::new(c).unwrap()
    }

    #[test]
    fn whittaker_hits_known_corners() {
        // Hot & wet → tropical rainforest; hot & dry → desert.
        assert_eq!(
            classify_land(t(27.0), 0.9, e(300.0), e(0.0), 0.0),
            Biome::TropicalRainforest
        );
        assert_eq!(
            classify_land(t(27.0), 0.05, e(300.0), e(0.0), 10.0),
            Biome::Desert
        );
        // Temperate mid-moisture → temperate forest.
        assert_eq!(
            classify_land(t(12.0), 0.5, e(200.0), e(0.0), 45.0),
            Biome::TemperateForest
        );
        // Cold → taiga/tundra.
        assert_eq!(
            classify_land(t(-2.0), 0.4, e(100.0), e(0.0), 60.0),
            Biome::Taiga
        );
    }

    #[test]
    fn specials_take_precedence() {
        // Below the ice threshold → Ice regardless of moisture.
        assert_eq!(
            classify_land(t(-25.0), 0.8, e(100.0), e(0.0), 80.0),
            Biome::Ice
        );
        // Above the tree line → Alpine.
        assert_eq!(
            classify_land(t(5.0), 0.5, e(4500.0), e(0.0), 0.0),
            Biome::Alpine
        );
    }

    #[test]
    fn names_are_kebab_and_unique() {
        let mut names: Vec<&str> = ALL.iter().map(|b| b.name()).collect();
        for n in &names {
            assert!(
                n.chars().all(|c| c.is_ascii_lowercase() || c == '-'),
                "not kebab: {n}"
            );
        }
        let len = names.len();
        names.sort();
        names.dedup();
        assert_eq!(names.len(), len, "duplicate biome names");
        assert_eq!(len, 22);
    }

    #[test]
    fn marine_precedence_is_correct() {
        // Warm shallow → reef; cold shallow → kelp.
        assert_eq!(
            classify_marine(50.0, t(25.0), SeafloorFeature::None, false),
            Biome::CoralReef
        );
        assert_eq!(
            classify_marine(50.0, t(8.0), SeafloorFeature::None, false),
            Biome::KelpForest
        );
        // Frozen surface beats everything.
        assert_eq!(
            classify_marine(50.0, t(-3.0), SeafloorFeature::Ridge, false),
            Biome::SeaIce
        );
        // Ridge → vent; ocean-ocean trench (deep) → hadal.
        assert_eq!(
            classify_marine(3000.0, t(4.0), SeafloorFeature::Ridge, false),
            Biome::HydrothermalVent
        );
        assert_eq!(
            classify_marine(7000.0, t(2.0), SeafloorFeature::Trench, false),
            Biome::HadalTrench
        );
        // Upwelling on a productive shelf.
        assert_eq!(
            classify_marine(300.0, t(15.0), SeafloorFeature::None, true),
            Biome::Upwelling
        );
        // Plain depth zones.
        assert_eq!(
            classify_marine(500.0, t(10.0), SeafloorFeature::None, false),
            Biome::Mesopelagic
        );
        assert_eq!(
            classify_marine(5000.0, t(3.0), SeafloorFeature::None, false),
            Biome::Abyssal
        );
    }

    #[test]
    fn classify_dispatches_land_and_sea() {
        // Below sea level → marine.
        let m = classify(
            t(10.0),
            0.5,
            t(22.0),
            e(-50.0),
            e(0.0),
            20.0,
            SeafloorFeature::None,
            false,
        );
        assert!(m.is_marine());
        // Above sea level → land.
        let l = classify(
            t(25.0),
            0.9,
            t(25.0),
            e(300.0),
            e(0.0),
            0.0,
            SeafloorFeature::None,
            false,
        );
        assert_eq!(l, Biome::TropicalRainforest);
    }

    #[test]
    fn every_biome_has_a_distinct_enough_glyph_and_a_color() {
        for b in [
            Biome::Desert,
            Biome::TropicalRainforest,
            Biome::Abyssal,
            Biome::CoralReef,
        ] {
            let _ = b.glyph();
            let _ = b.color();
        }
    }

    #[test]
    fn the_expression_path_reproduces_legacy_marine_classification_exactly() {
        // A dense sweep across every branch of the legacy precedence chain,
        // including the 12..=20 °C shallow gap that matches neither reef nor
        // kelp, and the trench-and-ridge overlap where the trench wins. This
        // test is what licenses calling the campaign a pure refactor.
        let features = [
            SeafloorFeature::None,
            SeafloorFeature::Trench,
            SeafloorFeature::Ridge,
        ];
        let depths = [
            0.0, 50.0, 199.0, 200.0, 500.0, 999.0, 1000.0, 3999.0, 4000.0, 5999.0, 6000.0, 6001.0,
            9000.0,
        ];
        let ssts = [-5.0, 0.0, 5.0, 11.9, 12.0, 15.0, 20.0, 20.1, 30.0];
        let mut checked = 0usize;
        for f in features {
            for d in depths {
                for sc in ssts {
                    for up in [false, true] {
                        let sst = t(sc);
                        let expr = classify_marine_expr(d, sst, f, up);
                        assert_eq!(
                            expr.biome(),
                            legacy_classify_marine(d, sst, f, up),
                            "depth {d} sst {sc} feature {f:?} upwelling {up}"
                        );
                        assert_eq!(expr.stratum, Stratum::at_depth_m(d));
                        assert_eq!(expr.realm, Realm::WATERWORLD);
                        checked += 1;
                    }
                }
            }
        }
        assert!(checked > 600, "sweep must be dense; checked {checked}");
    }

    /// The pre-campaign `classify_marine`, transcribed verbatim, as the oracle
    /// the sweep above compares against. Keeping a copy here is the only way
    /// the equivalence claim can be tested at all — delegating to the shipped
    /// function would compare it with itself.
    fn legacy_classify_marine(
        depth_m: f64,
        sst_c: Temperature,
        feature: SeafloorFeature,
        upwelling: bool,
    ) -> Biome {
        let sea_ice_c = Temperature::new(SEA_ICE_C).expect("sea-ice threshold is finite");
        if sst_c < sea_ice_c {
            return Biome::SeaIce;
        }
        if feature == SeafloorFeature::Trench && depth_m > 6000.0 {
            return Biome::HadalTrench;
        }
        if feature == SeafloorFeature::Ridge {
            return Biome::HydrothermalVent;
        }
        if depth_m < 200.0 {
            let reef_c = Temperature::new(20.0).expect("reef threshold is finite");
            if sst_c > reef_c {
                return Biome::CoralReef;
            }
            let kelp_c = Temperature::new(12.0).expect("kelp threshold is finite");
            if sst_c < kelp_c {
                return Biome::KelpForest;
            }
        }
        if upwelling && depth_m < 1000.0 {
            return Biome::Upwelling;
        }
        if depth_m < 200.0 {
            Biome::Epipelagic
        } else if depth_m < 1000.0 {
            Biome::Mesopelagic
        } else if depth_m < 4000.0 {
            Biome::Bathypelagic
        } else if depth_m < 6000.0 {
            Biome::Abyssal
        } else {
            Biome::HadalTrench
        }
    }

    #[test]
    fn a_trench_outranks_a_ridge_exactly_as_it_did() {
        // Rule 2 fires before rule 3: a deep trench that is also a ridge is
        // hadal open water, not a vent. Preserved deliberately.
        let deep = classify_marine_expr(9000.0, t(4.0), SeafloorFeature::Trench, false);
        assert_eq!(deep.formation, Formation::OpenWater);
        assert_eq!(deep.stratum, Stratum::Hadal);
        assert_eq!(deep.biome(), Biome::HadalTrench);
    }

    #[test]
    fn a_vent_is_now_a_community_at_a_depth() {
        // The disentangling, made visible: the vent keeps its stratum instead
        // of displacing it.
        let e = classify_marine_expr(3000.0, t(4.0), SeafloorFeature::Ridge, false);
        assert_eq!(e.formation, Formation::Vent);
        assert_eq!(e.stratum, Stratum::Bathypelagic);
        assert_eq!(e.biome(), Biome::HydrothermalVent);
    }

    #[test]
    fn land_cells_are_overworld_surface_and_project_to_themselves() {
        for (temp, moist, elev, lat) in [
            (25.0, 0.8, 100.0, 5.0),
            (5.0, 0.5, 200.0, 50.0),
            (-30.0, 0.3, 300.0, 80.0),
            (30.0, 0.05, 150.0, 20.0),
        ] {
            let expr = classify_expr(
                t(temp),
                moist,
                t(15.0),
                e(elev),
                e(0.0),
                lat,
                SeafloorFeature::None,
                false,
            );
            assert_eq!(expr.realm, Realm::OVERWORLD);
            assert_eq!(expr.stratum, Stratum::Surface);
            assert_eq!(
                expr.biome(),
                classify_land(t(temp), moist, e(elev), e(0.0), lat)
            );
        }
    }
}
