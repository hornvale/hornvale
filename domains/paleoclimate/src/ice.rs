//! The nonlinear global ice sheet: one volume fraction marched forward with
//! hysteresis. Ice grows slowly when the caloric-summer index falls below a
//! low threshold and melts fast when it rises above a high threshold; the dead
//! band between produces the glacial sawtooth (the mechanism that lets the weak
//! 100-kyr eccentricity term dominate the record). Volume drives a global
//! albedo cooling offset and an eustatic sea-level fall. Derived from forcing —
//! no RNG draws.

use crate::units::{IceVolume, SeaLevelChange, TempAnomaly};

/// Index below which ice grows.
const GROWTH_THRESHOLD: f64 = -0.5;
/// Index above which ice melts.
const MELT_THRESHOLD: f64 = 0.5;
/// Slow growth rate, volume-fraction per thousand years.
const GROWTH_PER_KYR: f64 = 0.05;
/// Fast melt rate, volume-fraction per thousand years.
const MELT_PER_KYR: f64 = 0.20;
/// Full-ice global cooling, °C. Calibrated together with the composition
/// root's absolute snowline threshold (`FREEZE_C` in
/// `windows/worldgen/src/lib.rs`) so a typical generated-sky world's
/// glacial maximum advances ice over roughly a quarter of its land — a
/// realistic Last Glacial Maximum extent — see that crate's
/// `climate_at_era`.
pub const ALBEDO_GAIN_C: f64 = 42.0;
/// Full-ice eustatic sea-level fall, metres.
const EUSTATIC_M: f64 = 120.0;
/// Standard days per thousand years.
const DAYS_PER_KYR: f64 = 1_000.0 * 365.25;

/// Global cooling offset (≤ 0) from an ice-volume fraction, as a
/// [`TempAnomaly`] — the albedo-cooling ΔT applied to the world's present
/// temperature field to get an era's absolute reading (see `Celsius`'s
/// `Add` impl). Built via [`TempAnomaly::from_offset_c`], the in-crate
/// production path for a computed (not measured) anomaly. `pub(crate)`, not
/// `pub`: this is the sole way to fabricate a `TempAnomaly` from a bare
/// `f64`, so exposing it outside the crate would defeat the newtype's
/// external-fabrication guarantee (see `TempAnomaly::from_offset_c`).
/// Callers outside this crate read `IceState.temp_offset` from the
/// integrated history instead.
pub(crate) fn temp_offset(volume: f64) -> TempAnomaly {
    TempAnomaly::from_offset_c(-ALBEDO_GAIN_C * volume)
}

/// Eustatic sea-level change (metres, ≤ 0) from an ice-volume fraction.
pub fn sea_level_change_m(volume: f64) -> f64 {
    -EUSTATIC_M * volume
}

/// One integrated moment of the ice history.
#[derive(Debug, Clone, PartialEq)]
pub struct IceState {
    /// Absolute standard day of this sample.
    pub day: f64,
    /// Global ice volume fraction at this day.
    pub volume: IceVolume,
    /// Global temperature offset from albedo feedback (≤ 0), as an anomaly
    /// applied to the world's present temperature via `Celsius`'s `Add`.
    pub temp_offset: TempAnomaly,
    /// Eustatic sea-level change (metres, ≤ 0).
    pub sea_level_change: SeaLevelChange,
}

/// March the ice sheet over `(day, caloric_index)` samples in ascending day
/// order, starting from ice-free. One `IceState` out per sample in.
pub fn integrate_ice(samples: &[(f64, f64)]) -> Vec<IceState> {
    let mut volume = 0.0_f64;
    let mut out = Vec::with_capacity(samples.len());
    let mut prev_day: Option<f64> = None;
    for &(day, g) in samples {
        let dt_kyr = match prev_day {
            Some(p) => (day - p) / DAYS_PER_KYR,
            None => 0.0,
        };
        if g < GROWTH_THRESHOLD {
            volume += GROWTH_PER_KYR * dt_kyr;
        } else if g > MELT_THRESHOLD {
            volume -= MELT_PER_KYR * dt_kyr;
        }
        volume = volume.clamp(0.0, 1.0);
        out.push(IceState {
            day,
            volume: IceVolume::new(volume).expect("clamped to [0,1]"),
            temp_offset: temp_offset(volume),
            sea_level_change: SeaLevelChange::new(sea_level_change_m(volume)).expect("finite"),
        });
        prev_day = Some(day);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn constant_forcing(g: f64, steps: usize) -> Vec<(f64, f64)> {
        (0..steps)
            .map(|k| (k as f64 * DAYS_PER_KYR * 2.0, g))
            .collect()
    }

    #[test]
    fn cold_summers_grow_ice_warm_summers_melt_it() {
        let cold = integrate_ice(&constant_forcing(-1.0, 200));
        assert!(
            cold.last().unwrap().volume.get() > 0.5,
            "sustained cold must build ice"
        );
        // Feed the built-up ice a warm signal and watch it melt faster than it grew.
        let mut warm_samples = constant_forcing(-1.0, 200);
        for k in 200..260 {
            warm_samples.push((k as f64 * DAYS_PER_KYR * 2.0, 1.0));
        }
        let warm = integrate_ice(&warm_samples);
        assert!(warm.last().unwrap().volume.get() < cold.last().unwrap().volume.get());
    }

    #[test]
    fn dead_band_holds_volume_steady() {
        // An index between the thresholds neither grows nor melts.
        let held = integrate_ice(&constant_forcing(0.0, 100));
        assert_eq!(held.last().unwrap().volume.get(), 0.0);
    }

    #[test]
    fn couplings_are_signed_and_bounded() {
        assert_eq!(temp_offset(0.0).get(), 0.0);
        assert_eq!(temp_offset(1.0).get(), -ALBEDO_GAIN_C);
        assert_eq!(sea_level_change_m(1.0), -120.0);
    }

    #[test]
    fn integration_is_deterministic() {
        let s = constant_forcing(-0.8, 300);
        assert_eq!(integrate_ice(&s), integrate_ice(&s));
    }
}
