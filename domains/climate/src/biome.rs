//! Biomes: a queryable field over the globe, derived per cell from
//! temperature, moisture, elevation, and (for the sea) depth, surface
//! temperature, and seafloor features. Land follows a Whittaker lookup with
//! ice/alpine specials; marine follows depth/SST/boundary/upwelling. Biomes
//! are never committed as facts (spec §3, §6) — the tier-0 `biome` fact stays
//! with the Vale.

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

/// The tree line in meters at a latitude: 4000 m at the equator, falling
/// 40 m per degree, floored at 0.
pub fn tree_line_m(latitude_deg: f64) -> f64 {
    (4000.0 - 40.0 * latitude_deg.abs()).max(0.0)
}

/// Ice threshold: annual-mean below this is permanent ice.
const ICE_C: f64 = -20.0;

impl Biome {
    /// True for the marine variants.
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

    /// The canonical kebab-case name (Lab metrics, CSV, book prose).
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

    /// A single ASCII glyph for the REPL biome map.
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
pub fn classify_land(
    temp_c: f64,
    moisture: f64,
    elevation_m: f64,
    sea_level_m: f64,
    latitude_deg: f64,
) -> Biome {
    if temp_c < ICE_C {
        return Biome::Ice;
    }
    if elevation_m - sea_level_m > tree_line_m(latitude_deg) {
        return Biome::Alpine;
    }
    if temp_c < 0.0 {
        // Cold: dry tundra, wetter taiga.
        if moisture < 0.35 {
            Biome::Tundra
        } else {
            Biome::Taiga
        }
    } else if temp_c < 7.0 {
        if moisture < 0.3 {
            Biome::Tundra
        } else {
            Biome::Taiga
        }
    } else if temp_c < 20.0 {
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
pub fn classify_marine(
    depth_m: f64,
    sst_c: f64,
    feature: SeafloorFeature,
    upwelling: bool,
) -> Biome {
    if sst_c < SEA_ICE_C {
        return Biome::SeaIce;
    }
    if feature == SeafloorFeature::Trench && depth_m > 6000.0 {
        return Biome::HadalTrench;
    }
    if feature == SeafloorFeature::Ridge {
        return Biome::HydrothermalVent;
    }
    if depth_m < 200.0 {
        if sst_c > 20.0 {
            return Biome::CoralReef;
        }
        if sst_c < 12.0 {
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

/// Classify any cell: marine when below sea level (depth = sea_level − elev),
/// otherwise land. `sst_c` is the surface temperature used for marine cells.
#[allow(clippy::too_many_arguments)]
pub fn classify(
    temp_c: f64,
    moisture: f64,
    sst_c: f64,
    elevation_m: f64,
    sea_level_m: f64,
    latitude_deg: f64,
    feature: SeafloorFeature,
    upwelling: bool,
) -> Biome {
    if elevation_m < sea_level_m {
        classify_marine(sea_level_m - elevation_m, sst_c, feature, upwelling)
    } else {
        classify_land(temp_c, moisture, elevation_m, sea_level_m, latitude_deg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn whittaker_hits_known_corners() {
        // Hot & wet → tropical rainforest; hot & dry → desert.
        assert_eq!(
            classify_land(27.0, 0.9, 300.0, 0.0, 0.0),
            Biome::TropicalRainforest
        );
        assert_eq!(classify_land(27.0, 0.05, 300.0, 0.0, 10.0), Biome::Desert);
        // Temperate mid-moisture → temperate forest.
        assert_eq!(
            classify_land(12.0, 0.5, 200.0, 0.0, 45.0),
            Biome::TemperateForest
        );
        // Cold → taiga/tundra.
        assert_eq!(classify_land(-2.0, 0.4, 100.0, 0.0, 60.0), Biome::Taiga);
    }

    #[test]
    fn specials_take_precedence() {
        // Below the ice threshold → Ice regardless of moisture.
        assert_eq!(classify_land(-25.0, 0.8, 100.0, 0.0, 80.0), Biome::Ice);
        // Above the tree line → Alpine.
        assert_eq!(classify_land(5.0, 0.5, 4500.0, 0.0, 0.0), Biome::Alpine);
    }

    #[test]
    fn names_are_kebab_and_unique() {
        let all = [
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
        let mut names: Vec<&str> = all.iter().map(|b| b.name()).collect();
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
            classify_marine(50.0, 25.0, SeafloorFeature::None, false),
            Biome::CoralReef
        );
        assert_eq!(
            classify_marine(50.0, 8.0, SeafloorFeature::None, false),
            Biome::KelpForest
        );
        // Frozen surface beats everything.
        assert_eq!(
            classify_marine(50.0, -3.0, SeafloorFeature::Ridge, false),
            Biome::SeaIce
        );
        // Ridge → vent; ocean-ocean trench (deep) → hadal.
        assert_eq!(
            classify_marine(3000.0, 4.0, SeafloorFeature::Ridge, false),
            Biome::HydrothermalVent
        );
        assert_eq!(
            classify_marine(7000.0, 2.0, SeafloorFeature::Trench, false),
            Biome::HadalTrench
        );
        // Upwelling on a productive shelf.
        assert_eq!(
            classify_marine(300.0, 15.0, SeafloorFeature::None, true),
            Biome::Upwelling
        );
        // Plain depth zones.
        assert_eq!(
            classify_marine(500.0, 10.0, SeafloorFeature::None, false),
            Biome::Mesopelagic
        );
        assert_eq!(
            classify_marine(5000.0, 3.0, SeafloorFeature::None, false),
            Biome::Abyssal
        );
    }

    #[test]
    fn classify_dispatches_land_and_sea() {
        // Below sea level → marine.
        let m = classify(
            10.0,
            0.5,
            22.0,
            -50.0,
            0.0,
            20.0,
            SeafloorFeature::None,
            false,
        );
        assert!(m.is_marine());
        // Above sea level → land.
        let l = classify(
            25.0,
            0.9,
            25.0,
            300.0,
            0.0,
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
}
