//! The substellar geometry of a tidally-locked world: one home for the
//! `SUBSTELLAR` point and the substellar-cosine every substellar-driven
//! field (temperature, moisture, insolation) shares. Extracting it here
//! makes "a new field forgot to branch on the locked regime" (SKY-24)
//! structurally hard: the geometry has one definition.

use hornvale_kernel::math;

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

/// Unit substellar direction at latitude `lat_deg`, longitude 0 — the
/// locked world's sub-solar point as it librates over the year. At
/// latitude 0 this is `SUBSTELLAR`.
/// type-audit: pending(wave-2: lat_deg), bare-ok(ratio: return)
pub fn substellar_at(lat_deg: f64) -> [f64; 3] {
    let lat = lat_deg.to_radians();
    [math::cos(lat), 0.0, math::sin(lat)]
}

/// Cosine of the angle between `p` and an arbitrary substellar direction.
/// type-audit: bare-ok(ratio: p), bare-ok(ratio: dir), bare-ok(ratio: return)
pub fn substellar_cosine_dir(p: [f64; 3], dir: [f64; 3]) -> f64 {
    p[0] * dir[0] + p[1] * dir[1] + p[2] * dir[2]
}

/// The locked-world surface temperature (°C) from the substellar cosine, the
/// insolation `scale` (S^{1/4}), and the lapse cooling — the one mapping
/// `mean_temperature` and the librating `temperature_at` both use.
/// type-audit: bare-ok(ratio: cos_theta), bare-ok(ratio: scale), pending(wave-2: lapse), pending(wave-2: return)
pub fn locked_cell_temperature(cos_theta: f64, scale: f64, lapse: f64) -> f64 {
    if cos_theta > 0.0 {
        (-18.0 + 78.0 * math::powf(cos_theta, 0.3) * scale) - lapse
    } else {
        -60.0 - lapse
    }
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
