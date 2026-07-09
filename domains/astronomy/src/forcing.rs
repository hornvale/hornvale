//! Deep-time orbital forcing (SKY-1/2/4/21): the Milankovitch triad as slow
//! functions of `WorldTime`. Produced at genesis, anchored so that every
//! element at `t = 0` equals its genesis draw. Consumed by paleoclimate
//! (Campaign 20), not here.
#![warn(missing_docs)]

use std::f64::consts::TAU;

/// Obliquity oscillation period, standard days (~41 kyr).
pub const P_OBLIQUITY: f64 = 41_000.0 * 365.25;
/// Eccentricity oscillation period, standard days (~100 kyr).
pub const P_ECCENTRICITY: f64 = 100_000.0 * 365.25;
/// Axial-precession period, standard days (~21 kyr).
pub const P_PRECESSION: f64 = 21_000.0 * 365.25;

/// The deep-time forcing of one world: means, amplitudes, and phases for the
/// Milankovitch triad. All fields are set at genesis; the `*_at` methods are
/// pure functions of absolute standard days.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OrbitalForcing {
    /// Mean obliquity, degrees (equals the anchor's genesis obliquity ε₀).
    pub obliquity_mean: f64,
    /// Obliquity oscillation amplitude, degrees (moon-coupled, Task 3).
    pub obliquity_amp: f64,
    /// Obliquity oscillation phase, radians.
    pub obliquity_phase: f64,
    /// Mean orbital eccentricity, dimensionless.
    pub ecc_mean: f64,
    /// Eccentricity oscillation amplitude, dimensionless.
    pub ecc_amp: f64,
    /// Eccentricity oscillation phase, radians.
    pub ecc_phase: f64,
    /// Axial-precession phase at genesis, radians.
    pub precession_phase: f64,
}

impl OrbitalForcing {
    /// Obliquity at absolute time `t` (standard days), anchored so
    /// `obliquity_at(0.0) == obliquity_mean` exactly.
    pub fn obliquity_at(&self, t: f64) -> f64 {
        self.obliquity_mean
            + self.obliquity_amp
                * ((TAU * t / P_OBLIQUITY + self.obliquity_phase).sin()
                    - self.obliquity_phase.sin())
    }
    /// Eccentricity at `t`; a genuinely new element, so `eccentricity_at(0.0)`
    /// is `ecc_mean + ecc_amp*sin(ecc_phase)` (the present gains eccentricity).
    pub fn eccentricity_at(&self, t: f64) -> f64 {
        (self.ecc_mean + self.ecc_amp * (TAU * t / P_ECCENTRICITY + self.ecc_phase).sin()).max(0.0)
    }
    /// Precession phase at `t`, radians, wrapping over `P_PRECESSION`.
    pub fn precession_at(&self, t: f64) -> f64 {
        self.precession_phase + TAU * t / P_PRECESSION
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample() -> OrbitalForcing {
        OrbitalForcing {
            obliquity_mean: 23.5,
            obliquity_amp: 1.3,
            obliquity_phase: 0.7,
            ecc_mean: 0.017,
            ecc_amp: 0.02,
            ecc_phase: 1.1,
            precession_phase: 0.4,
        }
    }

    #[test]
    fn obliquity_is_anchored_at_t0() {
        assert_eq!(sample().obliquity_at(0.0), 23.5);
    }

    #[test]
    fn obliquity_oscillates_within_amplitude() {
        let f = sample();
        for k in 0..1000 {
            let t = k as f64 * P_OBLIQUITY / 500.0;
            let d = (f.obliquity_at(t) - f.obliquity_mean).abs();
            // Tolerance accounts for: (1) the formula allows oscillations proportional to
            // |sin(θ) - sin(phase)|, which can exceed 1 when phase is large; (2) floating-point
            // precision at large t values. The bound is roughly 2 * amp for typical phases.
            assert!(
                d <= 2.2 * f.obliquity_amp + 1e-6,
                "obliquity left its band at t={t}"
            );
        }
    }

    #[test]
    fn eccentricity_never_negative() {
        let f = OrbitalForcing {
            ecc_mean: 0.01,
            ecc_amp: 0.05,
            ..sample()
        };
        for k in 0..1000 {
            let t = k as f64 * P_ECCENTRICITY / 500.0;
            assert!(f.eccentricity_at(t) >= 0.0);
        }
    }

    #[test]
    fn precession_advances_one_turn_per_period() {
        let f = sample();
        let d = f.precession_at(P_PRECESSION) - f.precession_at(0.0);
        assert!((d - TAU).abs() < 1e-9);
    }
}
