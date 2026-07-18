//! Diurnal (day/night) temperature swing (spec §2). A pure seam module: the
//! per-cell half-range amplitude (dryness × continentality × elevation), a
//! zero-mean normalized waveform (afternoon-peaked, damped by thermal
//! inertia and zeroed in polar night), and the composed anomaly. Preserves
//! `mean_temperature` exactly because the waveform integrates to ~0 over one
//! rotation — this module never touches the annual-mean baseline.

use hornvale_kernel::TempAnomaly;
use hornvale_kernel::math;

/// One full rotation in radians-of-phase (`std::f64::consts::TAU`).
const TAU: f64 = std::f64::consts::TAU;

/// Half the diurnal range at fully-dry, fully-humid air (°C); scaled down by
/// moisture, continentality, and up by elevation to give `A_climate`.
const BASE_DTR_HALF_C: f64 = 15.0;

/// The floor on dryness at full moisture (a saturated cell still swings a
/// little — water never damps the range to exactly zero).
const DRY_FLOOR: f64 = 0.15;

/// Fractional gain in swing per meter of elevation above sea level (thin,
/// dry mountain air amplifies the diurnal range).
const ELEV_GAIN_PER_M: f64 = 3.0e-5;

/// Thermal-inertia time constant (in rotations): how much of a rotation's
/// length it takes the ground to fully express the day's swing.
const TAU_THERMAL: f64 = 0.5;

/// The fraction of the day (0..1) at which the waveform peaks — afternoon,
/// after solar noon, reflecting thermal lag.
const PEAK_FRAC: f64 = 0.60;

/// The per-cell diurnal half-range `A_climate` in °C (always `>= 0`):
/// drier, more continental, higher cells swing harder between day and night.
/// type-audit: bare-ok(ratio: moisture), bare-ok(ratio: continentality), bare-ok(diagnostic-value: elevation_above_sea_m), bare-ok(diagnostic-value: return)
pub fn diurnal_amplitude(moisture: f64, continentality: f64, elevation_above_sea_m: f64) -> f64 {
    let dryness = DRY_FLOOR + (1.0 - DRY_FLOOR) * (1.0 - moisture.clamp(0.0, 1.0));
    let elev = 1.0 + ELEV_GAIN_PER_M * elevation_above_sea_m.max(0.0);
    BASE_DTR_HALF_C * dryness * continentality.clamp(0.0, 1.0) * elev
}

/// The normalized diurnal waveform `D`: zero-mean over `day_fraction ∈
/// [0,1)`, afternoon-peaked, damped by thermal inertia, and zero when the
/// sun never rises (polar night).
/// type-audit: bare-ok(diagnostic-value: latitude_deg), bare-ok(diagnostic-value: obliquity_deg), bare-ok(ratio: year_phase), bare-ok(ratio: day_fraction), bare-ok(diagnostic-value: day_length_std), bare-ok(ratio: return)
pub fn diurnal_waveform(
    latitude_deg: f64,
    obliquity_deg: f64,
    year_phase: f64,
    day_fraction: f64,
    day_length_std: f64,
) -> f64 {
    let decl_deg = obliquity_deg * math::sin(TAU * year_phase);
    let lat = latitude_deg.to_radians();
    let dec = decl_deg.to_radians();
    let noon_sin = math::sin(lat) * math::sin(dec) + math::cos(lat) * math::cos(dec);
    let a_geo = noon_sin.max(0.0);
    let inertia = 1.0 - math::exp(-day_length_std.max(0.0) / TAU_THERMAL);
    a_geo * inertia * math::cos(TAU * (day_fraction - PEAK_FRAC))
}

/// The diurnal temperature anomaly at a cell and moment: `amplitude *
/// diurnal_waveform(...)`, wrapped as a [`TempAnomaly`].
/// type-audit: bare-ok(diagnostic-value: amplitude), bare-ok(diagnostic-value: latitude_deg), bare-ok(diagnostic-value: obliquity_deg), bare-ok(ratio: year_phase), bare-ok(ratio: day_fraction), bare-ok(diagnostic-value: day_length_std)
#[allow(clippy::too_many_arguments)]
pub fn diurnal_anomaly(
    amplitude: f64,
    latitude_deg: f64,
    obliquity_deg: f64,
    year_phase: f64,
    day_fraction: f64,
    day_length_std: f64,
) -> TempAnomaly {
    let d = diurnal_waveform(
        latitude_deg,
        obliquity_deg,
        year_phase,
        day_fraction,
        day_length_std,
    );
    TempAnomaly::from_offset_c(amplitude * d)
}

#[cfg(test)]
mod tests {
    use super::*;

    // The keystone: the waveform integrates to ~0 over one rotation (so the daily
    // mean, and thus mean_temperature, is preserved). Sample 1000 fractions.
    #[test]
    fn waveform_is_zero_mean_over_a_rotation() {
        let n = 1000;
        let sum: f64 = (0..n)
            .map(|i| diurnal_waveform(23.4, 23.4, 0.25, i as f64 / n as f64, 1.0))
            .sum();
        assert!(
            (sum / n as f64).abs() < 1e-3,
            "diurnal waveform must be zero-mean; got {}",
            sum / n as f64
        );
    }

    // Anti-vacuity: a nonzero-amplitude equatorial day actually swings.
    #[test]
    fn equatorial_day_has_a_real_swing() {
        let peak = (0..100)
            .map(|i| diurnal_waveform(0.0, 23.4, 0.0, i as f64 / 100.0, 1.0))
            .fold(f64::MIN, f64::max);
        assert!(peak > 0.2, "equatorial day should swing; peak D = {peak}");
    }

    // Phase: the warmest moment is in the local afternoon (day_fraction > 0.5).
    #[test]
    fn peak_is_in_the_afternoon() {
        let (mut best_frac, mut best) = (0.0, f64::MIN);
        for i in 0..1000 {
            let f = i as f64 / 1000.0;
            let d = diurnal_waveform(0.0, 0.0, 0.0, f, 1.0);
            if d > best {
                best = d;
                best_frac = f;
            }
        }
        assert!(
            best_frac > 0.5 && best_frac < 0.75,
            "peak should be early afternoon; got frac {best_frac}"
        );
    }

    // Polar night: sun never rises -> no diurnal swing. North pole at northern-winter solstice.
    #[test]
    fn polar_night_has_no_swing() {
        for i in 0..100 {
            let d = diurnal_waveform(89.0, 23.4, 0.75, i as f64 / 100.0, 1.0);
            assert!(d.abs() < 1e-6, "polar night must not swing; got {d}");
        }
    }

    // A longer rotation day swings harder (thermal inertia has more time to lose).
    #[test]
    fn longer_day_swings_harder() {
        let amp = |dl: f64| {
            (0..200)
                .map(|i| diurnal_waveform(0.0, 0.0, 0.0, i as f64 / 200.0, dl))
                .fold(f64::MIN, f64::max)
        };
        assert!(
            amp(2.0) > amp(0.5),
            "a 2-day rotation should swing more than a half-day one"
        );
    }

    // Amplitude drivers are monotone: drier > humid; interior > coastal; higher > lower.
    #[test]
    fn amplitude_is_monotone_in_its_drivers() {
        assert!(
            diurnal_amplitude(0.1, 1.0, 0.0) > diurnal_amplitude(0.9, 1.0, 0.0),
            "drier swings more"
        );
        assert!(
            diurnal_amplitude(0.5, 1.0, 0.0) > diurnal_amplitude(0.5, 0.2, 0.0),
            "interior swings more than coast"
        );
        assert!(
            diurnal_amplitude(0.5, 1.0, 3000.0) > diurnal_amplitude(0.5, 1.0, 0.0),
            "altitude swings more"
        );
        assert!(
            diurnal_amplitude(0.5, 0.0, 0.0).abs() < 1e-9,
            "pure ocean (continentality 0) barely swings"
        );
    }

    // Calibration sanity: a hot dry equatorial desert reads an Earth-like range.
    #[test]
    fn desert_range_is_earth_like() {
        let a = diurnal_amplitude(0.05, 1.0, 0.0); // A_climate (half-range)
        let geo_peak = (0..200)
            .map(|i| diurnal_waveform(0.0, 0.0, 0.0, i as f64 / 200.0, 1.0))
            .fold(f64::MIN, f64::max);
        let peak_to_peak = 2.0 * a * geo_peak;
        assert!(
            (15.0..35.0).contains(&peak_to_peak),
            "desert DTR should be ~20-30 C; got {peak_to_peak}"
        );
    }
}
