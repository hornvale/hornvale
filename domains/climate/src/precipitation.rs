//! Precipitation derivations: pure scalar functions that turn the moisture
//! field (`crate::moisture`) and annual-mean temperature into an Earth-ranged
//! mm/yr total, a snow fraction, and a seasonal regime label. Kept pure (no
//! `Geosphere`/`CellMap`) so each is unit-tested without building a world;
//! the provider (`crate::provider`) precomputes them per cell and exposes
//! `precip_at`/`snow_fraction_at`/`regime_at`.

use crate::circulation::is_rising_band;
use hornvale_kernel::{Precipitation, math};

/// Map dimensionless moisture `[0, 1]` to an Earth-ranged annual
/// precipitation total, mm/yr. Monotone increasing: `0` moisture lands near
/// `0` mm (desert), `1` moisture lands above Earth's wettest rainforest
/// belts (`>1500` mm/yr). The `1.5` exponent is a documented approximation
/// (spec §5 model card) shaping the curve so mid-range moisture (temperate
/// climates) doesn't already read as tropical.
/// type-audit: bare-ok(ratio: moisture)
pub fn precip_mm_yr(moisture: f64) -> Precipitation {
    let m = moisture.clamp(0.0, 1.0);
    let mm = 2000.0 * math::powf(m, 1.5);
    Precipitation::new(mm).expect("clamped input keeps the result finite and non-negative")
}

/// The fraction of precipitation that falls as snow, `[0, 1]`, from
/// annual-mean temperature, °C. A logistic centered on freezing: `~1` well
/// below `0 °C`, `~0` well above, smooth (not a step) across the boundary —
/// real snow lines blur with local variability the annual mean can't
/// resolve. The `0.5` steepness constant is a documented approximation
/// (spec §5 model card): the fraction crosses `0.5` at exactly `0 °C` and is
/// within `1%` of its asymptote by about `±20 °C`.
/// type-audit: bare-ok(diagnostic-value: mean_temp_c), bare-ok(ratio: return)
pub fn snow_fraction(mean_temp_c: f64) -> f64 {
    const STEEPNESS: f64 = 0.5;
    1.0 / (1.0 + math::exp(STEEPNESS * mean_temp_c))
}

/// A seasonal precipitation regime label.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrecipRegime {
    /// Rain spread evenly across the year (the equatorial rising belt: the
    /// ITCZ migrates seasonally but nets close to uniform).
    Uniform,
    /// Precipitation peaks in the local summer (mid-latitude convective
    /// maximum away from any strong continental monsoon signal).
    SummerMax,
    /// Precipitation peaks in the local winter (a storm track migrating
    /// equatorward under a subtropical high — the Mediterranean pattern).
    WinterMax,
    /// A strong summer-max regime driven by continental heating overwhelming
    /// the storm track (the monsoon pattern).
    Monsoon,
}

/// Classify the seasonal precipitation regime at a cell from its circulation
/// band, continentality, and hemisphere. A documented heuristic (spec §5
/// model card), not a simulated monsoon circulation:
/// - The equatorial rising belt (`band == 0`) is `Uniform` — its ITCZ
///   migration averages out over the year.
/// - Elsewhere, continentality at or above a threshold yields `Monsoon`
///   (continental convective heating dominates). The threshold is lower in
///   the hemisphere with `hemisphere_sign > 0.0` (the Northern-Hemisphere
///   analog), mirroring Earth's larger boreal landmasses and the resulting
///   stronger monsoon circulation (the calibration point: the Asian
///   monsoon).
/// - Of the remaining bands, rising (even) bands get `SummerMax` (a
///   mid-latitude convective summer peak); sinking (odd, subtropical
///   horse-latitude) bands get `WinterMax` (the storm track migrates
///   equatorward in winter, under a dry subtropical high in summer).
///
/// type-audit: bare-ok(count: band), bare-ok(ratio: continentality), bare-ok(ratio: hemisphere_sign)
pub fn precip_regime(band: u32, continentality: f64, hemisphere_sign: f64) -> PrecipRegime {
    if band == 0 {
        return PrecipRegime::Uniform;
    }
    let monsoon_threshold = if hemisphere_sign > 0.0 { 0.6 } else { 0.75 };
    if continentality >= monsoon_threshold {
        return PrecipRegime::Monsoon;
    }
    if is_rising_band(band) {
        PrecipRegime::SummerMax
    } else {
        PrecipRegime::WinterMax
    }
}

/// Baseline cloud fraction contributed regardless of uplift or band (a
/// diagnostic floor: even flat, sinking terrain shows some cloud when moist).
const CLOUD_BASE: f64 = 0.3;
/// Additional contribution when the cell sits in a rising circulation band
/// (convective cloud from the same ascent that makes those bands wet).
const CLOUD_RISING: f64 = 0.3;
/// Orographic-uplift coefficient, scaled by `uplift_m / CLOUD_UPLIFT_SCALE_M`
/// (mirrors the orographic sink `moisture::carried_water` applies to the
/// moisture budget — the same terrain rise that rains moisture out also
/// forces the cloud that does the raining).
const CLOUD_UPLIFT_K: f64 = 0.6;
/// Elevation scale (m) normalizing uplift for the cloud-fraction term.
const CLOUD_UPLIFT_SCALE_M: f64 = 3000.0;

/// Diagnostic cloud fraction at a cell, `[0, 1]`: **feeds nothing** (no
/// insolation or temperature term reads it) — a readable field only, one
/// data point short of a full cloud model (spec §5 declared approximation).
/// Monotone increasing in `moisture` and in `uplift_m`; higher in a rising
/// circulation band than a sinking one at the same moisture and uplift.
/// `uplift_m` is the local along-wind terrain rise (the elevation gained
/// over the immediate upwind hop; see `provider::local_uplift_m`), zero or
/// negative (downhill / no upwind source) contributing nothing beyond the
/// base and rising terms.
///
/// type-audit: bare-ok(ratio: moisture), bare-ok(diagnostic-value: uplift_m), bare-ok(flag: rising_band), bare-ok(ratio: return)
pub fn cloud_fraction(moisture: f64, uplift_m: f64, rising_band: bool) -> f64 {
    let m = moisture.clamp(0.0, 1.0);
    let rising = if rising_band { 1.0 } else { 0.0 };
    let uplift_term = CLOUD_UPLIFT_K * (uplift_m.max(0.0) / CLOUD_UPLIFT_SCALE_M);
    (m * (CLOUD_BASE + CLOUD_RISING * rising + uplift_term)).clamp(0.0, 1.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn precip_mm_yr_is_near_zero_at_zero_moisture() {
        assert!(precip_mm_yr(0.0).get() < 1.0);
    }

    #[test]
    fn precip_mm_yr_exceeds_earths_wettest_belts_at_full_moisture() {
        assert!(precip_mm_yr(1.0).get() > 1500.0);
    }

    #[test]
    fn precip_mm_yr_is_monotone_increasing() {
        let samples = [0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0];
        for pair in samples.windows(2) {
            let lo = precip_mm_yr(pair[0]).get();
            let hi = precip_mm_yr(pair[1]).get();
            assert!(
                lo < hi,
                "precip_mm_yr must be monotone: {}mm at {} vs {}mm at {}",
                lo,
                pair[0],
                hi,
                pair[1]
            );
        }
    }

    #[test]
    fn precip_mm_yr_clamps_out_of_range_moisture() {
        assert_eq!(precip_mm_yr(-1.0).get(), precip_mm_yr(0.0).get());
        assert_eq!(precip_mm_yr(2.0).get(), precip_mm_yr(1.0).get());
    }

    #[test]
    fn snow_fraction_is_near_one_well_below_freezing() {
        assert!(snow_fraction(-25.0) > 0.99);
    }

    #[test]
    fn snow_fraction_is_near_zero_well_above_freezing() {
        assert!(snow_fraction(25.0) < 0.01);
    }

    #[test]
    fn snow_fraction_is_smooth_across_freezing() {
        assert_eq!(snow_fraction(0.0), 0.5);
        // Monotone decreasing, no discontinuity: a fine sweep never jumps.
        let mut prev = snow_fraction(-10.0);
        let mut t = -10.0;
        while t <= 10.0 {
            let cur = snow_fraction(t);
            assert!(
                cur <= prev + 1e-9,
                "snow_fraction must not increase with warming"
            );
            assert!((cur - prev).abs() < 0.2, "no discontinuous jump across {t}");
            prev = cur;
            t += 0.5;
        }
    }

    #[test]
    fn precip_regime_equatorial_band_is_uniform() {
        assert_eq!(precip_regime(0, 0.9, 1.0), PrecipRegime::Uniform);
        assert_eq!(precip_regime(0, 0.1, -1.0), PrecipRegime::Uniform);
    }

    #[test]
    fn precip_regime_high_continentality_midlatitude_is_monsoon() {
        assert_eq!(precip_regime(1, 0.9, 1.0), PrecipRegime::Monsoon);
    }

    #[test]
    fn precip_regime_rising_nonequatorial_band_is_summer_max() {
        assert_eq!(precip_regime(2, 0.2, 1.0), PrecipRegime::SummerMax);
    }

    #[test]
    fn precip_regime_sinking_low_continentality_band_is_winter_max() {
        assert_eq!(precip_regime(1, 0.2, 1.0), PrecipRegime::WinterMax);
    }

    #[test]
    fn cloud_fraction_is_high_for_moist_rising_uplifted() {
        assert!(cloud_fraction(0.9, 3000.0, true) > 0.8);
    }

    #[test]
    fn cloud_fraction_is_low_for_dry_sinking_flat() {
        assert!(cloud_fraction(0.05, 0.0, false) < 0.1);
    }

    #[test]
    fn cloud_fraction_is_bounded() {
        for &(m, u, r) in &[
            (0.0, 0.0, false),
            (1.0, 0.0, false),
            (0.0, 100_000.0, true),
            (1.0, 100_000.0, true),
            (-5.0, -1000.0, false),
            (2.0, 1_000_000.0, true),
        ] {
            let cf = cloud_fraction(m, u, r);
            assert!(
                (0.0..=1.0).contains(&cf),
                "cloud_fraction out of [0,1]: {cf}"
            );
        }
    }

    #[test]
    fn cloud_fraction_is_monotone_increasing_in_moisture() {
        let samples = [0.0, 0.1, 0.25, 0.5, 0.75, 1.0];
        for pair in samples.windows(2) {
            let lo = cloud_fraction(pair[0], 0.0, false);
            let hi = cloud_fraction(pair[1], 0.0, false);
            assert!(
                lo < hi,
                "cloud_fraction must be monotone in moisture: {lo} at {} vs {hi} at {}",
                pair[0],
                pair[1]
            );
        }
    }

    #[test]
    fn cloud_fraction_is_monotone_increasing_in_uplift() {
        let samples = [0.0, 500.0, 1000.0, 1500.0, 2000.0];
        for pair in samples.windows(2) {
            let lo = cloud_fraction(0.5, pair[0], true);
            let hi = cloud_fraction(0.5, pair[1], true);
            assert!(
                lo < hi,
                "cloud_fraction must be monotone in uplift: {lo} at {} vs {hi} at {}",
                pair[0],
                pair[1]
            );
        }
    }

    #[test]
    fn cloud_fraction_is_higher_in_rising_band_than_sinking() {
        let rising = cloud_fraction(0.6, 500.0, true);
        let sinking = cloud_fraction(0.6, 500.0, false);
        assert!(rising > sinking);
    }
}
