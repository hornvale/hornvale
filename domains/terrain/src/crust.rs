//! Crust: drawn cratons and the stateless crust fields (Crust spec §2).
//! Everything here is a pure function of position — no per-cell draws —
//! so any grid at any level samples the same underlying world and
//! coarse-constrains-fine is exact.

use crate::plates::dot;
use hornvale_kernel::{Seed, noise};

/// Continental crust thickness in kilometers.
/// type-audit: newtype
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CrustKm(f64);

impl CrustKm {
    /// Validate and wrap a thickness: finite, within [0, 100] km.
    /// type-audit: bare-ok(constructor-edge)
    pub fn new(km: f64) -> Result<CrustKm, String> {
        if !km.is_finite() || !(0.0..=100.0).contains(&km) {
            return Err(format!("crust thickness {km} km outside [0, 100]"));
        }
        Ok(CrustKm(km))
    }

    /// The thickness in kilometers.
    /// type-audit: bare-ok(constructor-edge)
    pub fn get(&self) -> f64 {
        self.0
    }
}

/// Base spatial frequency of the lobing noise (features ~1/6 rad).
#[allow(dead_code)]
const LOBE_FREQ: f64 = 6.0;
/// fBm octaves for the lobing noise.
#[allow(dead_code)]
const LOBE_OCTAVES: u32 = 4;
/// Lobe amplitude: the rim radius varies in [1 - AMP, 1 + AMP] x radius,
/// i.e. [0.5, 1.5] x radius_rad at this value.
#[allow(dead_code)]
const LOBE_AMP: f64 = 0.5;

/// Contrast gain compensating the three-slice averaging in
/// `sphere_fbm01` (which compresses variance toward 0.5): the raw
/// average rarely reaches the extremes of [0, 1], so the centered
/// value is amplified before it drives the rim. The subsequent
/// `.clamp(-0.5, 0.5)` keeps the rim provably inside
/// [1 - LOBE_AMP, 1 + LOBE_AMP] x radius_rad (i.e. [0.5, 1.5] x
/// radius_rad) regardless of gain, by construction — increasing this
/// constant only spends more of that fixed range on smaller inputs,
/// it can never push the rim outside the bound.
///
/// Calibrated against 5000 seeds over the rim-spread test's exact ring
/// geometry (radius 0.3, angle 0.3, 64 azimuths): the smallest gain
/// giving >= 95% of seeds a rim-spread > 0.2 was 6.0 (95.1% pass
/// rate); see the Task 4 review fix report for the full sweep table.
#[allow(dead_code)]
const REBALANCE_GAIN: f64 = 6.0;

/// Seam-free fBm in [0, 1) on the unit sphere: the mean of three
/// orthogonal coordinate-plane slices (the `coast_render` construction).
#[allow(dead_code)]
pub(crate) fn sphere_fbm01(seed: Seed, p: [f64; 3], frequency: f64, octaves: u32) -> f64 {
    let slices = [
        (seed.derive("slice-0"), p[0], p[1]),
        (seed.derive("slice-1"), p[1], p[2]),
        (seed.derive("slice-2"), p[2], p[0]),
    ];
    slices
        .iter()
        .map(|(s, a, b)| noise::fbm_2d(*s, frequency * a, frequency * b, octaves))
        .sum::<f64>()
        / 3.0
}

/// Lobed envelope of one craton: 1 at the center, tapering to 0 at a rim
/// whose radius varies with direction (the fBm lobing), hard 0 beyond
/// 1.5x the nominal radius. Flat-topped quartic taper.
#[allow(dead_code)]
pub(crate) fn lobed_envelope(seed: Seed, center: [f64; 3], p: [f64; 3], radius_rad: f64) -> f64 {
    let angle = dot(center, p).clamp(-1.0, 1.0).acos();
    if angle >= 1.5 * radius_rad {
        return 0.0;
    }
    let n = sphere_fbm01(seed, p, LOBE_FREQ, LOBE_OCTAVES);
    let centered = ((n - 0.5) * REBALANCE_GAIN).clamp(-0.5, 0.5);
    let rim = radius_rad * (1.0 + 2.0 * LOBE_AMP * centered);
    let x = (angle / rim).clamp(0.0, 1.0);
    (1.0 - x * x).powi(2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crust_km_validates() {
        assert!(CrustKm::new(35.0).is_ok());
        assert!(CrustKm::new(-1.0).is_err());
        assert!(CrustKm::new(f64::NAN).is_err());
        assert!(CrustKm::new(101.0).is_err());
        assert_eq!(CrustKm::new(7.0).unwrap().get(), 7.0);
    }

    #[test]
    fn envelope_is_one_at_center_zero_far_and_monotone_on_average() {
        let seed = Seed(42).derive("test-craton");
        let center = [0.0, 0.0, 1.0];
        assert!((lobed_envelope(seed, center, center, 0.3) - 1.0).abs() < 1e-9);
        // Far beyond any possible lobed rim (rim <= 1.5 * radius).
        let far = [0.0, 0.0, -1.0];
        assert_eq!(lobed_envelope(seed, center, far, 0.3), 0.0);
        // Radially: average envelope over a ring shrinks as rings move out.
        let ring_mean = |angle: f64| -> f64 {
            (0..64)
                .map(|i| {
                    let az = std::f64::consts::TAU * (i as f64) / 64.0;
                    let s = angle.sin();
                    let p = [s * az.cos(), s * az.sin(), angle.cos()];
                    lobed_envelope(seed, center, p, 0.3)
                })
                .sum::<f64>()
                / 64.0
        };
        assert!(ring_mean(0.1) > ring_mean(0.25));
        assert!(ring_mean(0.25) > ring_mean(0.40));
    }

    #[test]
    fn envelope_rims_are_lobed_not_circular() {
        // At a fixed angular distance near the rim, envelope varies with
        // azimuth: some directions are inside a lobe, others outside.
        // Swept across seeds (not just one) so the assertion isn't riding
        // on a single lucky/unlucky draw.
        for seed_val in [1u64, 7, 42, 99] {
            let seed = Seed(seed_val).derive("test-craton");
            let center = [0.0, 0.0, 1.0];
            let values: Vec<f64> = (0..64)
                .map(|i| {
                    let az = std::f64::consts::TAU * (i as f64) / 64.0;
                    let s = 0.3f64.sin();
                    let p = [s * az.cos(), s * az.sin(), 0.3f64.cos()];
                    lobed_envelope(seed, center, p, 0.3)
                })
                .collect();
            let (min, max) = values
                .iter()
                .fold((f64::INFINITY, f64::NEG_INFINITY), |(lo, hi), v| {
                    (lo.min(*v), hi.max(*v))
                });
            assert!(
                max - min > 0.2,
                "rim too circular for seed {seed_val}: spread {}",
                max - min
            );
        }
    }
}
