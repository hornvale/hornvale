//! Deep-time orbital forcing (SKY-1/2/4/21): the Milankovitch triad as slow
//! functions of `WorldTime`. Produced at genesis, anchored so that every
//! element at `t = 0` equals its genesis draw. Consumed by paleoclimate
//! (Campaign 20), not here.
#![warn(missing_docs)]

use std::f64::consts::TAU;

use crate::anchor::Anchor;
use crate::pins::SkyPins;
use crate::streams;
use hornvale_kernel::Seed;

/// Obliquity oscillation period, standard days (~41 kyr).
pub const P_OBLIQUITY: f64 = 41_000.0 * 365.25;
/// Eccentricity oscillation period, standard days (~100 kyr).
pub const P_ECCENTRICITY: f64 = 100_000.0 * 365.25;
/// Axial-precession period, standard days (~21 kyr).
pub const P_PRECESSION: f64 = 21_000.0 * 365.25;

/// The deep-time forcing of one world: means, amplitudes, and phases for the
/// Milankovitch triad. All fields are set at genesis; the `*_at` methods are
/// pure functions of absolute standard days.
#[derive(Debug, Clone, PartialEq)]
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
    /// Genesis year-phase offset, a fraction in `[0,1)` (SKY-4): keeps day 0
    /// from being the start of the year.
    pub year_phase_offset: f64,
    /// Genesis day-phase offset, a fraction in `[0,1)` (SKY-4): keeps day 0
    /// from being local midnight.
    pub day_phase_offset: f64,
    /// Genesis phase offset per moon, fractions in `[0,1)` (SKY-4): keeps
    /// day 0 from being every moon's new phase.
    pub moon_phase_offsets: Vec<f64>,
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

/// Draw the deep-time forcing. ε₀ is the anchor's genesis obliquity; the
/// oscillation amplitudes/phases and eccentricity are drawn on the `FORCING`
/// stream in a fixed order (a save-format contract). Amplitudes stay small so
/// the present sky is barely perturbed; deep time is where they show. The
/// obliquity wobble is moon-coupled (SKY-21): a large stabilizing moon damps
/// it, a moonless world keeps the full drawn amplitude.
pub fn generate_forcing(
    astronomy_seed: Seed,
    anchor: &Anchor,
    moons: &[crate::moons::Moon],
    pins: &SkyPins,
) -> OrbitalForcing {
    let mut s = astronomy_seed.derive(streams::FORCING).stream();
    // Fixed draw order — never reorder.
    let base_wobble = s.next_f64() * 2.5; // 0–2.5° base wobble
    let obliquity_phase = s.next_f64() * TAU;
    let ecc_mean = s.next_f64() * 0.05; // 0–0.05, Earth ~0.017
    let ecc_amp = s.next_f64() * 0.03;
    let ecc_phase = s.next_f64() * TAU;
    let precession_phase = s.next_f64() * TAU;
    // Moon coupling (SKY-21): total tidal stabilization damps the wobble.
    let stabilization: f64 = moons.iter().map(|m| m.tide_rel).sum();
    // Damping in (0,1]: no moon → 1.0 (full wobble); strong tide → small.
    let damping = 1.0 / (1.0 + stabilization);
    let obliquity_amp_drawn = base_wobble * damping;
    // The forcing pin zeroes the amplitudes AFTER the draws (pin isolation).
    let zeroed = matches!(pins.forcing, Some(crate::pins::ForcingPin::Zero));
    // Per-body genesis phase offsets (SKY-4): drawn unconditionally — a phase
    // offset is not "forcing", it only moves day 0 off the grand alignment
    // that falls out of every phase being a bare `(t/period).fract()`.
    let mut p = astronomy_seed.derive(streams::PHASE_OFFSETS).stream();
    let year_phase_offset = p.next_f64();
    let day_phase_offset = p.next_f64();
    let moon_phase_offsets: Vec<f64> = (0..moons.len()).map(|_| p.next_f64()).collect();
    OrbitalForcing {
        obliquity_mean: anchor.obliquity.get(),
        obliquity_amp: if zeroed { 0.0 } else { obliquity_amp_drawn },
        obliquity_phase,
        ecc_mean: if zeroed { 0.0 } else { ecc_mean },
        ecc_amp: if zeroed { 0.0 } else { ecc_amp },
        ecc_phase,
        precession_phase,
        year_phase_offset,
        day_phase_offset,
        moon_phase_offsets,
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
            year_phase_offset: 0.0,
            day_phase_offset: 0.0,
            moon_phase_offsets: Vec::new(),
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
            // Deviation is amp·(sinθ − sinφ); its magnitude is at most
            // amp·(1 + |sinφ|) ≤ 2·amp for any phase (the −sinφ anchor term
            // makes the swing asymmetric, so the peak exceeds amp).
            assert!(
                d <= 2.0 * f.obliquity_amp + 1e-9,
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

    #[test]
    fn generate_forcing_is_deterministic_and_anchored() {
        use crate::anchor::generate_anchor;
        use crate::pins::SkyPins;
        use crate::star::generate_star;
        use hornvale_kernel::Seed;
        let seed = Seed(42).derive(crate::streams::ROOT);
        let star = generate_star(seed);
        let anchor = generate_anchor(seed, &star, &SkyPins::default()).unwrap();
        let a = generate_forcing(seed, &anchor, &[], &SkyPins::default());
        let b = generate_forcing(seed, &anchor, &[], &SkyPins::default());
        assert_eq!(a, b);
        // ε₀ carries the anchor's genesis obliquity, so obliquity_at(0) matches.
        assert_eq!(a.obliquity_mean, anchor.obliquity.get());
        assert_eq!(a.obliquity_at(0.0), anchor.obliquity.get());
    }

    #[test]
    fn a_large_moon_damps_the_obliquity_wobble() {
        use crate::anchor::generate_anchor;
        use crate::moons::generate_moons;
        use crate::pins::{MoonsPin, SkyPins};
        use crate::star::generate_star;
        use hornvale_kernel::Seed;
        let seed = Seed(42).derive(crate::streams::ROOT);
        let star = generate_star(seed);
        let anchor = generate_anchor(seed, &star, &SkyPins::default()).unwrap();
        let mooned = SkyPins {
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        };
        let moonless = SkyPins {
            moons: Some(MoonsPin::exact(0).unwrap()),
            ..SkyPins::default()
        };
        let (mm, _) = generate_moons(seed, &star, &anchor, &mooned).unwrap();
        let (ml, _) = generate_moons(seed, &star, &anchor, &moonless).unwrap();
        let f_mooned = generate_forcing(seed, &anchor, &mm, &mooned);
        let f_moonless = generate_forcing(seed, &anchor, &ml, &moonless);
        assert!(
            f_mooned.obliquity_amp < f_moonless.obliquity_amp,
            "a moon must damp the obliquity amplitude ({} !< {})",
            f_mooned.obliquity_amp,
            f_moonless.obliquity_amp
        );
    }
}
