//! Background starfield: a catalog of faint stars derived on demand, never
//! serialized. This is texture only — no genesis draws from this stream.

use crate::streams;
use hornvale_kernel::Seed;
use hornvale_kernel::math;

/// A faint background star: position and brightness class only. Class 1
/// is the brightest tier, 5 the faintest (dim-heavy, like a real sky).
/// type-audit: pending(wave-1: ra_deg), pending(wave-1: dec_deg), bare-ok(count: magnitude_class)
#[derive(Debug, Clone, PartialEq)]
pub struct FieldStar {
    /// Right ascension in degrees, [0, 360).
    pub ra_deg: f64,
    /// Declination in degrees, [-90, 90] (sphere-uniform).
    pub dec_deg: f64,
    /// Brightness class, 1 (brightest) ..= 5 (faintest).
    pub magnitude_class: u8,
}

/// Generate a background starfield from an astronomy seed. The catalog is
/// derived on demand and never serialized — a field texture rather than a
/// genesis product. One stream consumed in fixed order: count, then per-star
/// declination (sphere-uniform), right ascension (uniform), and magnitude
/// class (1–5, dim-heavy).
pub fn starfield(astronomy_seed: Seed) -> Vec<FieldStar> {
    let mut stream = astronomy_seed.derive(streams::STARFIELD).stream();
    let count = stream.range_u32(100, 300);

    (0..count)
        .map(|_| {
            // Sphere-uniform declination: asin(2u - 1) matches neighborhood.rs.
            let dec_deg = math::asin(stream.next_f64() * 2.0 - 1.0).to_degrees();
            // Uniform right ascension.
            let ra_deg = stream.next_f64() * 360.0;
            // Magnitude roll: 1–5, dim-heavy distribution.
            let mag_roll = stream.range_u32(1, 100);
            let magnitude_class = match mag_roll {
                1..=5 => 1,
                6..=15 => 2,
                16..=35 => 3,
                36..=65 => 4,
                _ => 5,
            };
            FieldStar {
                ra_deg,
                dec_deg,
                magnitude_class,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::SkyPins;
    use crate::system::generate;

    #[test]
    fn starfield_is_deterministic() {
        let seed = Seed(42).derive_typed(streams::ROOT);
        let a = starfield(seed);
        let b = starfield(seed);
        assert_eq!(a, b);
    }

    #[test]
    fn starfield_count_is_in_range() {
        let seed = Seed(42).derive_typed(streams::ROOT);
        let stars = starfield(seed);
        assert!((100..=300).contains(&stars.len()));
    }

    #[test]
    fn star_positions_are_valid() {
        let seed = Seed(42).derive_typed(streams::ROOT);
        let stars = starfield(seed);
        for star in &stars {
            assert!(
                (-90.0..=90.0).contains(&star.dec_deg),
                "dec {} out of range",
                star.dec_deg
            );
            assert!(
                (0.0..360.0).contains(&star.ra_deg),
                "ra {} out of range",
                star.ra_deg
            );
            assert!((1..=5).contains(&star.magnitude_class));
        }
    }

    #[test]
    fn starfield_is_roughly_isotropic() {
        // Partition declination into six 30° bands and assert each is nonempty.
        // With sphere-uniform sampling and ≥100 stars, equatorial bands hold
        // most stars; polar bands ~3+ in expectation.
        let seed = Seed(42).derive_typed(streams::ROOT);
        let stars = starfield(seed);

        let mut bands = [0; 6];
        for star in &stars {
            let band_idx = ((star.dec_deg + 90.0) / 30.0) as usize;
            if band_idx < 6 {
                bands[band_idx] += 1;
            }
        }

        for (i, &count) in bands.iter().enumerate() {
            assert!(
                count > 0,
                "band {} (dec [{}, {}]) has {} stars",
                i,
                -90.0 + i as f64 * 30.0,
                -90.0 + (i + 1) as f64 * 30.0,
                count
            );
        }
    }

    #[test]
    fn genesis_is_untouched_by_starfield() {
        // Calling starfield() must not consume any stream that genesis
        // reads. Generate two systems with the same seed, one preceded by
        // a starfield call; they must be byte-equal.
        let world_seed = Seed(42);

        // First generation: without starfield call.
        let system_a = generate(world_seed, &SkyPins::default()).expect("first generation failed");

        // Second generation: with a starfield call before it.
        let _stars = starfield(world_seed.derive_typed(streams::ROOT));
        let system_b = generate(world_seed, &SkyPins::default()).expect("second generation failed");

        assert_eq!(system_a, system_b, "genesis was affected by starfield call");
    }
}
