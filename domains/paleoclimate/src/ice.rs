//! The nonlinear global ice sheet: one volume fraction marched forward with
//! hysteresis. Ice grows slowly when the caloric-summer index falls below a
//! low threshold and melts fast when it rises above a high threshold; the dead
//! band between produces the glacial sawtooth (the mechanism that lets the weak
//! 100-kyr eccentricity term dominate the record). Volume drives a global
//! albedo cooling offset and an eustatic sea-level fall. Derived from forcing —
//! no RNG draws.

use crate::units::{IceVolume, SeaLevelChange};
use hornvale_kernel::{TempAnomaly, WorldTime};

/// Index below which ice grows.
/// plumb: pending(wave-1)
const GROWTH_THRESHOLD: f64 = -0.5;
/// Index above which ice melts.
/// plumb: pending(wave-1)
const MELT_THRESHOLD: f64 = 0.5;
/// Slow growth rate, volume-fraction per thousand years.
/// plumb: pending(wave-1)
const GROWTH_PER_KYR: f64 = 0.05;
/// Fast melt rate, volume-fraction per thousand years.
/// plumb: pending(wave-1)
const MELT_PER_KYR: f64 = 0.20;
/// Full-ice global cooling, °C. Calibrated together with the composition
/// root's absolute snowline threshold (`FREEZE_C` in
/// `windows/worldgen/src/lib.rs`) so a typical generated-sky world's
/// glacial maximum advances ice over roughly a quarter of its land — a
/// realistic Last Glacial Maximum extent — see that crate's
/// `climate_at_era`.
/// type-audit: pending(wave-2)
/// plumb: pending(wave-1)
pub const ALBEDO_GAIN_C: f64 = 42.0;
/// Full-ice eustatic sea-level fall, metres.
/// plumb: pending(wave-1)
const EUSTATIC_M: f64 = 120.0;
/// Standard days per thousand years.
/// plumb: pending(wave-1)
const DAYS_PER_KYR: f64 = 1_000.0 * 365.25;

/// Global cooling offset (≤ 0) from an ice-volume fraction, as a
/// [`TempAnomaly`] — the albedo-cooling ΔT applied to the world's present
/// temperature field to get an era's absolute reading (see `Temperature`'s
/// `Add` impl). Built via [`TempAnomaly::from_offset_c`] (a kernel-public
/// constructor: see `hornvale_kernel::units`), expressing the ice sheet's
/// feedback as a computed (not measured) anomaly. `pub(crate)`, not `pub`:
/// this function is this crate's own convenience wrapper around that
/// constructor, not a fabrication boundary — callers outside this crate
/// read `IceState.temp_offset` from the integrated history instead of
/// computing their own offset here.
pub(crate) fn temp_offset(volume: f64) -> TempAnomaly {
    TempAnomaly::from_offset_c(-ALBEDO_GAIN_C * volume)
}

/// Eustatic sea-level change (metres, ≤ 0) from an ice-volume fraction.
/// type-audit: bare-ok(ratio: volume), pending(wave-2: return)
pub fn sea_level_change_m(volume: f64) -> f64 {
    -EUSTATIC_M * volume
}

/// One integrated moment of the ice history.
///
/// **This `day` is a standard day on every path, and a typed [`WorldTime`]
/// since The Hallmark's Task 15** — both producers sample `-k *
/// ICE_STEP_DAYS` and compare against a day-valued `era_day`. It was
/// unambiguous even while [`crate::strata::EraClimate::day`] was not, and was
/// `pending` only because Task 5 scoped the three paleoclimate day fields as
/// one migration and stopped on the ambiguous one. Task 13 repaired that one
/// (`DOM-era-day-axis`, ledger entries #10 and #15) and Task 15 retyped all
/// three together.
#[derive(Debug, Clone, PartialEq)]
pub struct IceState {
    /// Absolute standard day of this sample.
    pub day: WorldTime,
    /// Global ice volume fraction at this day.
    pub volume: IceVolume,
    /// Global temperature offset from albedo feedback (≤ 0), as an anomaly
    /// applied to the world's present temperature via `Temperature`'s `Add`.
    pub temp_offset: TempAnomaly,
    /// Eustatic sea-level change (metres, ≤ 0).
    pub sea_level_change: SeaLevelChange,
}

/// March the ice sheet over `(day, caloric_index)` samples in ascending day
/// order, starting from ice-free. One `IceState` out per sample in.
///
/// The `day` half of each sample carries [`IceState::day`]'s standard-day
/// axis, typed since The Hallmark's Task 15 (`DOM-era-day-axis`, ledger #10,
/// #15, #17); the caloric-index half stays a bare dimensionless ratio.
/// type-audit: bare-ok(ratio: samples)
pub fn integrate_ice(samples: &[(WorldTime, f64)]) -> Vec<IceState> {
    let mut volume = 0.0_f64;
    let mut out = Vec::with_capacity(samples.len());
    let mut prev_day: Option<WorldTime> = None;
    for &(day, g) in samples {
        let dt_kyr = match prev_day {
            Some(p) => (day - p).as_std_days() / DAYS_PER_KYR,
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

    fn constant_forcing(g: f64, steps: usize) -> Vec<(WorldTime, f64)> {
        (0..steps)
            .map(|k| {
                (
                    WorldTime::from_std_days(k as f64 * DAYS_PER_KYR * 2.0)
                        .expect("test day within tick range"),
                    g,
                )
            })
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
            warm_samples.push((
                WorldTime::from_std_days(k as f64 * DAYS_PER_KYR * 2.0)
                    .expect("test day within tick range"),
                1.0,
            ));
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
