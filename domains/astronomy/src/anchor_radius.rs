//! Physical bulk radius of the anchor world, derived from its mass.

use crate::units::{EarthMasses, Megameters, UnitError};

/// Supported anchor-mass interval for `earthlike-rocky-zeng2019-linear/v1`.
/// plumb: pending(wave-1)
const SUPPORTED_MASS_MIN: f64 = 0.5;
/// Supported anchor-mass interval for `earthlike-rocky-zeng2019-linear/v1`.
/// plumb: pending(wave-1)
const SUPPORTED_MASS_MAX: f64 = 2.0;

/// Earth's volumetric mean radius, in megameters (NASA reference: 6371 km).
/// plumb: pending(wave-1)
const EARTH_REFERENCE_RADIUS_MM: f64 = 6.371;

/// Frozen covering subset of Li Zeng's 32.5% Fe, 67.5% MgSiO3 numerical
/// Earth-like rocky mass-radius curve, in (Earth masses, Earth radii).
///
/// Source file SHA-256:
/// `dcc5080f2186983b7e36200373878dc06a8d8083ec21ce1c4f670659c0404b38`.
const EARTHLIKE_MASS_RADIUS: [(f64, f64); 8] = [
    (0.4093, 0.7725),
    (0.5304, 0.8330),
    (0.6835, 0.8964),
    (0.8756, 0.9625),
    (1.1150, 1.0309),
    (1.4114, 1.1015),
    (1.7763, 1.1741),
    (2.2233, 1.2485),
];

/// Derive the anchor world's spherical bulk radius from its mass.
///
/// `earthlike-rocky-zeng2019-linear/v1` linearly interpolates a frozen
/// numerical curve for an assumed Earth-like rocky composition. Evaluation
/// is limited to the anchor generator's 0.5–2 Earth-mass range.
pub fn anchor_radius(mass: EarthMasses) -> Result<Megameters, UnitError> {
    let mass = mass.get();
    if !(SUPPORTED_MASS_MIN..=SUPPORTED_MASS_MAX).contains(&mass) {
        return Err(UnitError {
            unit: "Earth masses for anchor radius",
            value: mass,
            reason: "earthlike-rocky-zeng2019-linear/v1 supports only 0.5–2 Earth masses",
        });
    }

    let [(mass_0, radius_0), (mass_1, radius_1)] = EARTHLIKE_MASS_RADIUS
        .windows(2)
        .find(|points| points[0].0 <= mass && mass <= points[1].0)
        .expect("the frozen curve covers the supported mass range")
    else {
        unreachable!("windows(2) always yields pairs")
    };
    let radius_earth = radius_0 + (mass - mass_0) * (radius_1 - radius_0) / (mass_1 - mass_0);
    Megameters::new(radius_earth * EARTH_REFERENCE_RADIUS_MM)
}
