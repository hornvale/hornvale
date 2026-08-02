//! The star's light as a spectrum on the kernel's band grid.
//!
//! A blackbody curve at the star's effective temperature, sampled into ten
//! bands, plus a cheap atmospheric-path attenuation that reddens the light
//! as the sun approaches the horizon. Both are declared approximations:
//! the campaign's claims rest on the *direction* of these effects, not on
//! radiometric accuracy.

use crate::star::Star;
use hornvale_kernel::color::{BAND_CENTERS_NM, BANDS, Illuminant};
use hornvale_kernel::math;

/// Planck's second radiation constant, `hc/k`, in nanometre-kelvin. Used in
/// the exponential term of the spectral radiance law.
/// type-audit: bare-ok(ratio)
const C2_NM_K: f64 = 1.438_776_877e7;

/// Spectral radiance of a blackbody at `t_kelvin`, at `wavelength_nm`, up to
/// a constant factor. The leading `c1` is omitted because every consumer
/// works in ratios or renormalizes — carrying it would only scale all ten
/// bands together.
fn planck_relative(wavelength_nm: f64, t_kelvin: f64) -> f64 {
    let l5 = wavelength_nm.powi(5);
    let x = C2_NM_K / (wavelength_nm * t_kelvin);
    1.0 / (l5 * (math::exp(x) - 1.0))
}

/// The star's light at the top of the atmosphere, sampled into the band
/// grid and normalized so the brightest band is 1.0.
///
/// **A midpoint sample, not an integral.** Each band is 40 nm wide (edges
/// 340–740; see [`BAND_CENTERS_NM`]), and this evaluates Planck's law once
/// at the band *centre* rather than integrating the curve across the band.
/// That is the deliberate choice, not an oversight: the Planck curve is
/// smooth and monotone within 40 nm everywhere on this grid at
/// main-sequence temperatures, so the midpoint rule preserves the ordering
/// between bands and between stars, which is all the campaign's claims
/// rest on. A band integral would change the numbers slightly and change
/// none of the directions.
///
/// Normalizing here means downstream code compares *colour*, not distance
/// from the star — insolation is climate's business, and this function is
/// forbidden from influencing it (the containment rule on [`Star::t_eff`]).
pub fn daylight(star: &Star) -> Illuminant {
    let mut bands = [0.0f64; BANDS];
    let mut peak = 0.0f64;
    for (band, center) in bands.iter_mut().zip(BAND_CENTERS_NM.iter()) {
        let value = planck_relative(*center, star.t_eff.get());
        *band = value;
        if value > peak {
            peak = value;
        }
    }
    // `peak` is strictly positive for any finite positive temperature, so
    // this division is total; the guard is defensive, not a live path.
    if peak > 0.0 {
        for value in bands.iter_mut() {
            *value /= peak;
        }
    }
    Illuminant::new(bands).expect("a normalized Planck curve is finite and non-negative")
}

/// Redden and dim `base` for a sun at `sun_elevation_deg` above the horizon.
///
/// Declared approximation: air mass grows roughly as `1/sin(elevation)`, and
/// Rayleigh scattering removes short wavelengths in proportion to `λ⁻⁴`, so
/// the surviving fraction per band is `exp(-k · airmass · (550/λ)⁴)`. The
/// result is that a low sun loses its blue first, which is the direction the
/// campaign's second falsifiable claim depends on.
///
/// A sun at or below the horizon is clamped to the largest air mass rather
/// than diverging.
/// type-audit: pending(wave-1: sun_elevation_deg)
pub fn at_elevation(base: &Illuminant, sun_elevation_deg: f64) -> Illuminant {
    /// Optical-depth scale at the reference wavelength.
    const K: f64 = 0.10;
    /// Reference wavelength, nanometres — roughly the middle of the grid.
    const REFERENCE_NM: f64 = 550.0;
    /// Air-mass ceiling, standing in for the horizon limit.
    const MAX_AIRMASS: f64 = 38.0;

    let sin_elevation = math::sin(sun_elevation_deg.clamp(-90.0, 90.0).to_radians());
    let airmass = if sin_elevation <= 1.0 / MAX_AIRMASS {
        MAX_AIRMASS
    } else {
        1.0 / sin_elevation
    };

    let mut bands = [0.0f64; BANDS];
    for ((out, center), incoming) in bands
        .iter_mut()
        .zip(BAND_CENTERS_NM.iter())
        .zip(base.get().iter())
    {
        let ratio = REFERENCE_NM / center;
        let scattering = ratio * ratio * ratio * ratio;
        *out = incoming * math::exp(-K * airmass * scattering);
    }
    Illuminant::new(bands).expect("attenuating a valid illuminant leaves it valid")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::star::{Star, generate_star};
    use crate::units::Kelvin;
    use hornvale_kernel::Seed;

    /// A star at an exact temperature, with every other field irrelevant to
    /// these tests. Built through `generate_star` and overridden, so the
    /// test cannot drift from the real constructor.
    fn star_at(t_eff: f64) -> Star {
        let mut s = generate_star(Seed(42));
        s.t_eff = Kelvin(t_eff);
        s
    }

    #[test]
    fn a_hot_star_is_bluer_than_a_cool_one() {
        let cool = daylight(&star_at(4000.0));
        let hot = daylight(&star_at(7000.0));
        // Compare short-band share against long-band share. Absolute
        // radiance differs hugely between the two; the ratio is the claim.
        let cool_ratio = cool.get()[2] / cool.get()[8];
        let hot_ratio = hot.get()[2] / hot.get()[8];
        assert!(
            hot_ratio > cool_ratio,
            "hot star short/long = {hot_ratio}, cool = {cool_ratio}"
        );
    }

    #[test]
    fn every_band_of_a_daylight_illuminant_is_positive_and_finite() {
        let light = daylight(&star_at(5772.0));
        for (b, value) in light.get().iter().enumerate() {
            assert!(value.is_finite() && *value > 0.0, "band {b}");
        }
    }

    #[test]
    fn a_low_sun_is_redder_than_a_high_sun() {
        let noon_light = daylight(&star_at(5772.0));
        let dusk_light = at_elevation(&noon_light, 3.0);
        let high = at_elevation(&noon_light, 80.0);
        let dusk_ratio = dusk_light.get()[8] / dusk_light.get()[2];
        let high_ratio = high.get()[8] / high.get()[2];
        assert!(
            dusk_ratio > high_ratio,
            "dusk long/short = {dusk_ratio}, high sun = {high_ratio}"
        );
    }

    #[test]
    fn a_sun_below_the_horizon_still_yields_a_valid_illuminant() {
        let base = daylight(&star_at(5772.0));
        let night = at_elevation(&base, -10.0);
        for (b, value) in night.get().iter().enumerate() {
            assert!(value.is_finite() && *value >= 0.0, "band {b}");
        }
    }

    #[test]
    fn sampling_is_bit_identical_across_repeated_calls() {
        let a = daylight(&star_at(5772.0));
        let b = daylight(&star_at(5772.0));
        assert_eq!(a.get(), b.get());
    }
}
