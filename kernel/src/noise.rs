//! Coherent noise with random access: deterministic, evaluable at any
//! point without evaluating neighbors, locally coherent.

use crate::seed::{Seed, StreamLabel};
use crate::streams::OCTAVE_LABELS;

/// Hash a lattice point to [0, 1). Stable forever (save-format contract).
fn lattice(seed: Seed, xi: i64, yi: i64) -> f64 {
    let mut h = seed.0
        ^ (xi as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15)
        ^ (yi as u64).wrapping_mul(0xc2b2_ae3d_27d4_eb4f);
    h = (h ^ (h >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    h = (h ^ (h >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    h ^= h >> 31;
    (h >> 11) as f64 / (1u64 << 53) as f64
}

fn smoothstep(t: f64) -> f64 {
    t * t * (3.0 - 2.0 * t)
}

/// Bilinear value noise in [0, 1). Random access: cost is O(1) per sample.
/// type-audit: pending(wave-1: x), pending(wave-1: y), bare-ok(ratio: return)
pub fn value_noise_2d(seed: Seed, x: f64, y: f64) -> f64 {
    let x0 = x.floor();
    let y0 = y.floor();
    let (xi, yi) = (x0 as i64, y0 as i64);
    let tx = smoothstep(x - x0);
    let ty = smoothstep(y - y0);
    let v00 = lattice(seed, xi, yi);
    let v10 = lattice(seed, xi + 1, yi);
    let v01 = lattice(seed, xi, yi + 1);
    let v11 = lattice(seed, xi + 1, yi + 1);
    let top = v00 + (v10 - v00) * tx;
    let bot = v01 + (v11 - v01) * tx;
    top + (bot - top) * ty
}

/// Derive octave `n`'s seed from a base `seed`, exactly as `fbm_2d` always
/// has: the seed directly for octave 0, else `seed.derive(streams::OCTAVE_LABELS[n])`
/// (via the `OCTAVE_LABELS` table when in range, else
/// `seed.derive(StreamLabel::dynamic(&format!("octave-{n}")))`). Isolated so
/// the sampler and the free function share one definition and cannot drift.
fn octave_seed(seed: Seed, octave: u32) -> Seed {
    if octave == 0 {
        seed
    } else if (octave as usize) < OCTAVE_LABELS.len() {
        seed.derive(OCTAVE_LABELS[octave as usize])
    } else {
        seed.derive(StreamLabel::dynamic(&format!("octave-{octave}")))
    }
}

/// A precomputed fBm sampler: derives the per-octave seeds **once**, then
/// evaluates many points with no further derivation. Bit-identical to
/// `fbm_2d` — same derived seeds, same accumulation order, same math — it
/// only hoists the per-octave `Seed::derive` out of the per-sample loop.
///
/// Build one per field (the seed and octave count are loop-invariant) and
/// call [`Fbm::sample`] per cell; a `Seed::derive`-dominated profile
/// (world generation) collapses `O(cells × octaves)` derivations to
/// `O(octaves)`.
#[derive(Clone, Debug)]
pub struct Fbm {
    /// Octave `i`'s seed, precomputed by [`octave_seed`]; `len() == octaves`.
    octave_seeds: Vec<Seed>,
}

impl Fbm {
    /// Precompute the octave seeds for `octaves` octaves rooted at `seed`.
    /// Panics if `octaves == 0` (fBm needs at least one octave).
    /// type-audit: bare-ok(count: octaves)
    #[must_use]
    pub fn new(seed: Seed, octaves: u32) -> Self {
        assert!(octaves > 0, "Fbm::new: octaves must be >= 1");
        let octave_seeds = (0..octaves).map(|o| octave_seed(seed, o)).collect();
        Self { octave_seeds }
    }

    /// Sample the fractal at `(x, y)`, normalized to [0, 1). Gain 0.5,
    /// lacunarity 2.0 — identical to `fbm_2d` with the same base seed.
    /// type-audit: pending(wave-1: x), pending(wave-1: y), bare-ok(ratio: return)
    #[must_use]
    pub fn sample(&self, x: f64, y: f64) -> f64 {
        let mut sum = 0.0;
        let mut amplitude = 1.0;
        let mut frequency = 1.0;
        let mut max = 0.0;
        for &s in &self.octave_seeds {
            sum += amplitude * value_noise_2d(s, x * frequency, y * frequency);
            max += amplitude;
            amplitude *= 0.5;
            frequency *= 2.0;
        }
        sum / max
    }
}

/// Fractal Brownian motion over `value_noise_2d`, normalized to [0, 1).
/// Gain 0.5, lacunarity 2.0. Octave 0 uses the seed directly so that
/// one-octave fbm equals plain value noise. Panics if octaves == 0.
/// Internally derives per-octave streams labeled "octave-{n}" (n ≥ 1).
///
/// This is the random-access convenience form; for a hot per-cell loop
/// with a fixed seed, build an [`Fbm`] once and reuse it.
/// type-audit: pending(wave-1: x), pending(wave-1: y), bare-ok(count: octaves), bare-ok(ratio: return)
pub fn fbm_2d(seed: Seed, x: f64, y: f64, octaves: u32) -> f64 {
    Fbm::new(seed, octaves).sample(x, y)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_noise_is_deterministic() {
        let s = Seed(42);
        assert_eq!(value_noise_2d(s, 3.7, -2.1), value_noise_2d(s, 3.7, -2.1));
    }

    #[test]
    fn value_noise_differs_by_seed() {
        assert_ne!(
            value_noise_2d(Seed(1), 3.7, 2.1),
            value_noise_2d(Seed(2), 3.7, 2.1)
        );
    }

    #[test]
    fn value_noise_is_in_unit_interval() {
        let s = Seed(7);
        for i in 0..50 {
            for j in 0..50 {
                let v = value_noise_2d(s, f64::from(i) * 0.37 - 9.0, f64::from(j) * 0.53 - 13.0);
                assert!((0.0..1.0).contains(&v), "out of range: {v}");
            }
        }
    }

    #[test]
    fn value_noise_is_locally_coherent() {
        // Nearby samples differ by less than distant samples do on average.
        let s = Seed(11);
        let base = value_noise_2d(s, 10.25, 10.25);
        let near = value_noise_2d(s, 10.26, 10.25);
        assert!((base - near).abs() < 0.05);
    }

    #[test]
    fn fbm_is_deterministic_and_bounded() {
        let s = Seed(5);
        let a = fbm_2d(s, 1.5, 2.5, 4);
        assert_eq!(a, fbm_2d(s, 1.5, 2.5, 4));
        assert!((0.0..1.0).contains(&a));
    }

    #[test]
    fn fbm_with_one_octave_matches_value_noise() {
        let s = Seed(5);
        assert_eq!(fbm_2d(s, 1.5, 2.5, 1), value_noise_2d(s, 1.5, 2.5));
    }

    #[test]
    fn precomputed_fbm_is_bit_identical_to_the_free_function() {
        // The save-format contract: `Fbm::sample` must return the exact bits
        // `fbm_2d` returns, for every octave count and sample point, or worlds
        // built through the hoisted sampler would diverge from history.
        for octaves in 1..=18u32 {
            let seed = Seed(0x51ED ^ u64::from(octaves));
            let fbm = Fbm::new(seed, octaves);
            for i in -7..7 {
                for j in -7..7 {
                    let x = f64::from(i) * 1.37 + 0.11;
                    let y = f64::from(j) * 0.53 - 0.29;
                    assert_eq!(
                        fbm.sample(x, y),
                        fbm_2d(seed, x, y, octaves),
                        "divergence at octaves={octaves}, x={x}, y={y}"
                    );
                }
            }
        }
    }
}
