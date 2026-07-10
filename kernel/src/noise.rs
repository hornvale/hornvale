//! Coherent noise with random access: deterministic, evaluable at any
//! point without evaluating neighbors, locally coherent.

use crate::seed::Seed;

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

/// Fractal Brownian motion over `value_noise_2d`, normalized to [0, 1).
/// Gain 0.5, lacunarity 2.0. Octave 0 uses the seed directly so that
/// one-octave fbm equals plain value noise. Panics if octaves == 0.
/// Internally derives per-octave streams labeled "octave-{n}" (n ≥ 1).
/// type-audit: pending(wave-1: x), pending(wave-1: y), bare-ok(count: octaves), bare-ok(ratio: return)
pub fn fbm_2d(seed: Seed, x: f64, y: f64, octaves: u32) -> f64 {
    assert!(octaves > 0, "fbm_2d: octaves must be >= 1");
    let mut sum = 0.0;
    let mut amplitude = 1.0;
    let mut frequency = 1.0;
    let mut max = 0.0;
    for octave in 0..octaves {
        let s = if octave == 0 {
            seed
        } else {
            seed.derive(&format!("octave-{octave}"))
        };
        sum += amplitude * value_noise_2d(s, x * frequency, y * frequency);
        max += amplitude;
        amplitude *= 0.5;
        frequency *= 2.0;
    }
    sum / max
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
}
