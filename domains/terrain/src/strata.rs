//! The stratigraphic column — the world's vertical dimension and its deep-time
//! archive (campaign The Deep). A pure projection over fields terrain already
//! owns: no new stream draws, no committed facts, no `TectonicGlobe` state.
//! Extends The Ground's shallow 2-layer column (soil over `Basement`) into a
//! named band profile, stamps each band with the era it records, and derives
//! the deep's geothermal energy base. Deliberately purely geological — the
//! `thaumic` axis stays reserved (spec §1/§10).

use hornvale_kernel::Temperature;

/// Geothermal gradient, kelvin per kilometre of depth — the deep's energy base
/// (the "inner sun" a later chemo/lithotroph ecology reads), not merely a rock
/// temperature.
/// type-audit: newtype
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct GeothermalGradient(f64);

/// Coolest continental-shield gradient (K/km).
const CRATONIC_GRADIENT_K_PER_KM: f64 = 15.0;
/// Hottest young/thin-crust gradient (K/km).
const OCEANIC_GRADIENT_K_PER_KM: f64 = 30.0;

impl GeothermalGradient {
    /// Wrap a finite, positive gradient (K/km), clamped to the physical band.
    /// type-audit: bare-ok(constructor-edge)
    pub fn new(k_per_km: f64) -> GeothermalGradient {
        debug_assert!(k_per_km.is_finite() && k_per_km > 0.0);
        GeothermalGradient(k_per_km.clamp(CRATONIC_GRADIENT_K_PER_KM, OCEANIC_GRADIENT_K_PER_KM))
    }
    /// The gradient in kelvin per kilometre.
    /// type-audit: bare-ok(constructor-edge)
    pub fn get(self) -> f64 {
        self.0
    }
}

/// The geothermal gradient for a cell: hot under young/thin crust, cool under
/// ancient thick cratons. Pure function of the crust fields terrain owns.
/// Oceanic crust runs the hot end regardless of thickness (`thickness_norm` is
/// 0 there); thickness only cools the continental side.
/// type-audit: bare-ok(ratio: crust_thickness_km), bare-ok(ratio: crust_age), bare-ok(flag: continental)
pub fn geothermal_gradient(
    crust_thickness_km: f64,
    crust_age: f64,
    continental: bool,
) -> GeothermalGradient {
    let thickness_norm = if continental {
        ((crust_thickness_km - crate::crust::CONTINENTAL_THRESHOLD_KM) / 50.0).clamp(0.0, 1.0)
    } else {
        0.0
    };
    let coolness = (0.5 * crust_age + 0.5 * thickness_norm).clamp(0.0, 1.0);
    let g = OCEANIC_GRADIENT_K_PER_KM
        - (OCEANIC_GRADIENT_K_PER_KM - CRATONIC_GRADIENT_K_PER_KM) * coolness;
    GeothermalGradient::new(g)
}

/// Temperature at a depth below the surface, given the surface datum and the
/// cell's gradient. Pure; the surface datum is the caller's (climate's) to
/// supply, so terrain stays climate-free.
/// type-audit: bare-ok(ratio: depth_km)
pub fn temperature_at_depth(
    surface: Temperature,
    gradient: GeothermalGradient,
    depth_km: f64,
) -> Temperature {
    Temperature::new(surface.get() + gradient.get() * depth_km)
        .expect("finite geothermal temperature")
}

use crate::RockClass;
use crate::lithology::Basement;

/// The era a band records, coarsening downward — the archive's time-ordering.
/// Ordinal (deeper is older); normalized, not absolute years (crust age is a
/// `[0,1]` field, not a year count).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Era {
    /// Youngest: recent cover, active deposition.
    Recent,
    /// Consolidated older cover / young basement.
    Ancient,
    /// Deep crystalline crust.
    Deep,
    /// The primordial substrate — pre-life; the deepest band.
    Primordial,
}

/// The named bands, top → bottom; resolution coarsens downward.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BandKind {
    /// The living skin: soil / weathered regolith.
    Regolith,
    /// Deposited / volcanic surface rock — the legible archive.
    Cover,
    /// Crystalline craton (terrain's inherited `Basement`).
    Basement,
    /// Deep crust: hot, high-pressure.
    Roots,
    /// The primordial substrate / threshold to the not-here.
    Underneath,
}

/// One band of a cell's column.
/// type-audit: bare-ok(diagnostic-value: top_depth_m)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BandSample {
    /// Which band this is.
    pub kind: BandKind,
    /// The era it records (deeper = older).
    pub era: Era,
    /// Depth to the top of this band, metres below the surface.
    pub top_depth_m: f64,
    /// A representative rock for the band.
    pub rock: RockClass,
}

/// A cell's coarse stratigraphic column: the vertical dimension and the
/// deep-time archive, as a pure projection over terrain's fields.
/// type-audit: bare-ok(diagnostic-value: depth_to_basement_m), bare-ok(flag: unconformity)
#[derive(Debug, Clone, PartialEq)]
pub struct StratigraphicColumn {
    /// The five bands, top → bottom.
    pub bands: [BandSample; 5],
    /// Depth to crystalline basement (soil + sedimentary cover), metres.
    pub depth_to_basement_m: f64,
    /// A nonconformity — thin/young cover directly on ancient basement: the
    /// archive's floating gap (missing time that left no record).
    pub unconformity: bool,
}

/// Depth to crystalline basement: the soil cover plus the sedimentary cover.
/// type-audit: bare-ok(ratio: soil_depth_m), bare-ok(ratio: sediment_m), bare-ok(diagnostic-value: return)
pub fn depth_to_basement(soil_depth_m: f64, sediment_m: f64) -> f64 {
    soil_depth_m + sediment_m
}

/// Minimum cover (m) below which thin cover on old rock reads as a gap.
const UNCONFORMITY_COVER_M: f64 = 200.0;
/// Minimum normalized basement age for a thin-cover gap to count.
const UNCONFORMITY_AGE: f64 = 0.6;

/// A nonconformity: thin cover directly on ancient basement.
/// type-audit: bare-ok(ratio: depth_to_basement_m), bare-ok(ratio: crust_age), bare-ok(flag: return)
pub fn unconformity(depth_to_basement_m: f64, crust_age: f64) -> bool {
    depth_to_basement_m < UNCONFORMITY_COVER_M && crust_age > UNCONFORMITY_AGE
}

/// Build a cell's column from fields terrain already owns. Pure; no draws.
/// Bands are stamped so that era is monotone non-decreasing with depth (the
/// archive's ordering), the deepest band always `Primordial`. Band top-depths
/// assume `moho_m >= depth_to_basement_m` — always true for real crust (the
/// Moho is kilometres deep, the cover at most a few hundred metres); were it
/// violated the Roots and Underneath tops would coincide, but the era ordering
/// still holds.
/// type-audit: bare-ok(ratio: crust_thickness_km), bare-ok(ratio: crust_age), bare-ok(flag: continental), bare-ok(ratio: sediment_m), bare-ok(ratio: soil_depth_m)
pub fn column(
    crust_thickness_km: f64,
    crust_age: f64,
    continental: bool,
    sediment_m: f64,
    soil_depth_m: f64,
    surface_rock: RockClass,
    basement: Basement,
) -> StratigraphicColumn {
    let _ = continental; // reserved for future facies refinement; keeps the signature stable
    let dtb = depth_to_basement(soil_depth_m, sediment_m);
    let moho_m = crust_thickness_km * 1000.0;
    let basement_rock = match basement {
        Basement::Continental => RockClass::Granite,
        Basement::Oceanic => RockClass::Gabbro,
    };
    // Shallow eras from deposition recency; deep eras fixed by depth. Capped so
    // the ordering never inverts (cover never older than basement, etc.).
    let cover_era = if crust_age >= 0.5 {
        Era::Ancient
    } else {
        Era::Recent
    };
    let basement_era = if crust_age >= 0.5 {
        Era::Deep
    } else {
        Era::Ancient
    };
    let roots_top = dtb + (moho_m - dtb).max(0.0) * 0.5;
    let bands = [
        BandSample {
            kind: BandKind::Regolith,
            era: Era::Recent,
            top_depth_m: 0.0,
            rock: surface_rock,
        },
        BandSample {
            kind: BandKind::Cover,
            era: cover_era,
            top_depth_m: soil_depth_m,
            rock: surface_rock,
        },
        BandSample {
            kind: BandKind::Basement,
            era: basement_era,
            top_depth_m: dtb,
            rock: basement_rock,
        },
        BandSample {
            kind: BandKind::Roots,
            era: Era::Deep,
            top_depth_m: roots_top,
            rock: RockClass::Gneiss,
        },
        BandSample {
            kind: BandKind::Underneath,
            era: Era::Primordial,
            top_depth_m: moho_m.max(dtb),
            rock: RockClass::Gabbro,
        },
    ];
    StratigraphicColumn {
        bands,
        depth_to_basement_m: dtb,
        unconformity: unconformity(dtb, crust_age),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gradient_is_hotter_under_young_thin_crust() {
        let hot = geothermal_gradient(8.0, 0.0, false).get(); // oceanic, young
        let cool = geothermal_gradient(65.0, 1.0, true).get(); // ancient thick craton
        assert!(hot > cool, "hot {hot} !> cool {cool}");
        assert!((15.0..=30.0).contains(&hot));
        assert!((15.0..=30.0).contains(&cool));
    }

    #[test]
    fn temperature_rises_with_depth() {
        let surface = hornvale_kernel::Temperature::new(10.0).unwrap();
        let g = geothermal_gradient(40.0, 0.5, true);
        let shallow = temperature_at_depth(surface, g, 1.0).get();
        let deep = temperature_at_depth(surface, g, 5.0).get();
        assert!(deep > shallow && shallow > 10.0);
    }

    use crate::RockClass;
    use crate::lithology::Basement;

    #[test]
    fn band_eras_are_monotone_non_decreasing_with_depth() {
        for &age in &[0.0, 0.3, 0.6, 1.0] {
            let col = column(
                40.0,
                age,
                true,
                500.0,
                2.0,
                RockClass::Sandstone,
                Basement::Continental,
            );
            for w in col.bands.windows(2) {
                assert!(
                    w[0].era <= w[1].era,
                    "age {age}: {:?} !<= {:?}",
                    w[0].era,
                    w[1].era
                );
            }
            assert_eq!(
                col.bands[4].era,
                Era::Primordial,
                "deepest band is primordial"
            );
        }
    }

    #[test]
    fn thin_cover_on_ancient_basement_is_an_unconformity() {
        assert!(unconformity(50.0, 0.9));
        assert!(!unconformity(1500.0, 0.9)); // thick cover — no gap
        assert!(!unconformity(50.0, 0.2)); // young basement — no gap
    }

    #[test]
    fn depth_to_basement_sums_the_cover() {
        assert_eq!(depth_to_basement(3.0, 400.0), 403.0);
    }

    #[test]
    fn basement_band_rock_follows_the_basement_variant() {
        let cont = column(
            40.0,
            0.8,
            true,
            100.0,
            2.0,
            RockClass::Shale,
            Basement::Continental,
        );
        assert_eq!(cont.bands[2].rock, RockClass::Granite);
        let ocean = column(
            8.0,
            0.0,
            false,
            50.0,
            0.0,
            RockClass::Basalt,
            Basement::Oceanic,
        );
        assert_eq!(ocean.bands[2].rock, RockClass::Gabbro);
    }
}
