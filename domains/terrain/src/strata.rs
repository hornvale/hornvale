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
}
