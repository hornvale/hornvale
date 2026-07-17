//! The substellar geometry of a tidally-locked world: one home for the
//! `SUBSTELLAR` point and the substellar-cosine every substellar-driven
//! field (temperature, moisture, insolation) shares. Extracting it here
//! makes "a new field forgot to branch on the locked regime" (SKY-24)
//! structurally hard: the geometry has one definition.

/// The substellar point of a locked world (spec convention: `+x`,
/// latitude 0 on the substellar axis).
/// type-audit: bare-ok(ratio)
pub const SUBSTELLAR: [f64; 3] = [1.0, 0.0, 0.0];

/// Cosine of the angle from the substellar point: `dot(p, SUBSTELLAR)`.
/// +1 at the substellar point, 0 on the terminator meridian, -1 at the
/// antistellar point. Positive iff `p` is on the day side.
/// type-audit: bare-ok(ratio: p), bare-ok(ratio: return)
pub fn substellar_cosine(p: [f64; 3]) -> f64 {
    p[0] * SUBSTELLAR[0] + p[1] * SUBSTELLAR[1] + p[2] * SUBSTELLAR[2]
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::math;

    #[test]
    fn substellar_cosine_is_one_at_substellar_zero_at_terminator_neg_at_antistellar() {
        assert!((substellar_cosine([1.0, 0.0, 0.0]) - 1.0).abs() < 1e-12);
        assert!(substellar_cosine([0.0, 1.0, 0.0]).abs() < 1e-12); // terminator meridian
        assert!((substellar_cosine([-1.0, 0.0, 0.0]) + 1.0).abs() < 1e-12);
    }

    /// The standard golden-ratio spiral: a deterministic, evenly-spread
    /// sample of `n` points on the unit sphere, used to check the helper
    /// against the inline arithmetic it replaced over a broad sweep of
    /// directions (not just the three axis-aligned cases above).
    fn fib_sphere(n: usize) -> Vec<[f64; 3]> {
        let golden_angle = std::f64::consts::PI * (3.0 - math::powf(5.0, 0.5));
        (0..n)
            .map(|i| {
                let y = 1.0 - 2.0 * (i as f64) / ((n - 1) as f64);
                let radius = math::powf((1.0 - y * y).max(0.0), 0.5);
                let theta = golden_angle * i as f64;
                let x = math::cos(theta) * radius;
                let z = math::sin(theta) * radius;
                [x, y, z]
            })
            .collect()
    }

    #[test]
    fn substellar_cosine_matches_inline_arithmetic_over_fibonacci_sphere() {
        for p in fib_sphere(200) {
            let inline = p[0] * SUBSTELLAR[0] + p[1] * SUBSTELLAR[1] + p[2] * SUBSTELLAR[2];
            assert_eq!(substellar_cosine(p), inline);
        }
    }
}
