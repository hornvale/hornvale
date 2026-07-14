//! Wandering planets (sibling worlds): deterministically drawn with real Kepler orbits.
//! Observational only (declared approximation): no physical effect on the anchor.

use crate::anchor::Anchor;
use crate::pins::SkyPins;
use crate::star::Star;
use crate::streams;
use crate::units::{Au, StdDays};
use hornvale_kernel::Seed;
use hornvale_kernel::math;

/// A wandering sibling planet.
/// type-audit: bare-ok(ratio: albedo), bare-ok(ratio: apparent_brightness), pending(wave-1: max_elongation_deg)
#[derive(Debug, Clone, PartialEq)]
pub struct Wanderer {
    /// Orbital semi-major axis in AU (drawn, region-dependent).
    pub orbit: Au,
    /// Orbital period in standard days (derived: Kepler III).
    pub period: StdDays,
    /// Planetary class (drawn: rock or giant).
    pub class: WandererClass,
    /// Bond albedo (drawn 0.1–0.7).
    pub albedo: f64,
    /// Maximum elongation from star in degrees (inner only; outer None).
    /// type-audit: pending(wave-1)
    pub max_elongation_deg: Option<f64>,
    /// Synodic period relative to anchor in standard days (derived).
    pub synodic_period: StdDays,
    /// Apparent brightness relative to the anchor at closest approach, arbitrary units.
    pub apparent_brightness: f64,
}

/// Planetary classification.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WandererClass {
    /// Small, rocky world.
    Rock,
    /// Gas or ice giant.
    Giant,
}

/// Generate wandering sibling planets: drawn on fixed order with no redraw,
/// sorted by orbit (innermost first).
pub fn generate_wanderers(
    astronomy_seed: Seed,
    star: &Star,
    anchor: &Anchor,
    _pins: &SkyPins,
) -> Vec<Wanderer> {
    let count_roll = astronomy_seed
        .derive(streams::WANDERER_COUNT)
        .stream()
        .range_u32(1, 100);
    let count = match count_roll {
        1..=10 => 0,
        11..=35 => 1,
        36..=65 => 2,
        66..=90 => 3,
        _ => 4,
    };

    let mut wanderers: Vec<Wanderer> = Vec::new();
    let mut stream = astronomy_seed.derive(streams::WANDERERS).stream();

    for _ in 0..count {
        let region_roll = stream.range_u32(1, 100);
        let is_inner = region_roll <= 40;
        let f = stream.next_f64();

        // Compute orbital semi-major axis
        let orbit_au = if is_inner {
            // Inner: a = a_anchor * (0.25 + 0.5*f)
            anchor.orbit.0 * (0.25 + 0.5 * f)
        } else {
            // Outer: log-uniform a = a_anchor * exp(f * ln(20/1.8)) * 1.8
            anchor.orbit.0 * math::exp(f * math::ln(20.0 / 1.8)) * 1.8
        };

        // Class: inner always Rock; outer Giant if class_roll <= 60 else Rock
        let class_roll = stream.range_u32(1, 100);
        let class = if is_inner {
            WandererClass::Rock
        } else {
            if class_roll <= 60 {
                WandererClass::Giant
            } else {
                WandererClass::Rock
            }
        };

        // Albedo: 0.1 + 0.6*draw
        let albedo = 0.1 + 0.6 * stream.next_f64();

        // Period: Kepler III form (same as anchor)
        let period_days = 365.25 * (orbit_au.powi(3) / star.mass.0).sqrt();

        // Synodic period: |1/(1/P_w - 1/P_anchor)|
        let anchor_period = anchor.year.0;
        let synodic_inv = 1.0 / period_days - 1.0 / anchor_period;
        let synodic_days = if synodic_inv.abs() > 1e-12 {
            (1.0 / synodic_inv).abs()
        } else {
            f64::INFINITY
        };

        // Max elongation for inner; None for outer
        let max_elongation_deg = if is_inner {
            Some(math::asin(orbit_au / anchor.orbit.0).to_degrees())
        } else {
            None
        };

        // Apparent brightness = albedo * r² / (a² * Δ²)
        // r = 0.5 for Rock, 4.0 for Giant
        // Δ = |a - a_anchor| (closest approach)
        let radius = match class {
            WandererClass::Rock => 0.5,
            WandererClass::Giant => 4.0,
        };
        let delta = (orbit_au - anchor.orbit.0).abs();
        let apparent_brightness = if delta > 1e-12 {
            albedo * radius * radius / (orbit_au * orbit_au * delta * delta)
        } else {
            0.0
        };

        wanderers.push(Wanderer {
            orbit: Au(orbit_au),
            period: StdDays(period_days),
            class,
            albedo,
            max_elongation_deg,
            synodic_period: StdDays(synodic_days),
            apparent_brightness,
        });
    }

    // Sort by orbit ascending with total_cmp
    wanderers.sort_by(|a, b| a.orbit.0.total_cmp(&b.orbit.0));
    wanderers
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::anchor::generate_anchor;
    use crate::star::generate_star;

    #[test]
    fn wanderers_are_deterministic_and_bounded() {
        let star = generate_star(Seed(42).derive(crate::streams::ROOT));
        let anchor = generate_anchor(
            Seed(42).derive(crate::streams::ROOT),
            &star,
            &SkyPins::default(),
        )
        .unwrap();
        let a = generate_wanderers(
            Seed(42).derive(crate::streams::ROOT),
            &star,
            &anchor,
            &SkyPins::default(),
        );
        let b = generate_wanderers(
            Seed(42).derive(crate::streams::ROOT),
            &star,
            &anchor,
            &SkyPins::default(),
        );
        assert_eq!(a, b);
        assert!(a.len() <= 4);
    }

    #[test]
    fn kepler_holds_and_periods_are_monotone_in_axis() {
        for seed in 0..64u64 {
            let s = Seed(seed).derive(crate::streams::ROOT);
            let star = generate_star(s);
            let anchor = generate_anchor(s, &star, &SkyPins::default()).unwrap();
            let ws = generate_wanderers(s, &star, &anchor, &SkyPins::default());
            for pair in ws.windows(2) {
                assert!(pair[0].orbit.0 < pair[1].orbit.0, "innermost first");
                assert!(pair[0].period.0 < pair[1].period.0, "Kepler is monotone");
            }
            for w in &ws {
                let expected = 365.25 * (w.orbit.0.powi(3) / star.mass.0).sqrt();
                assert!((w.period.0 - expected).abs() < 1e-9);
                match w.max_elongation_deg {
                    Some(e) => {
                        assert!(w.orbit.0 < anchor.orbit.0, "elongation-bound = inner");
                        assert!(e > 0.0 && e < 90.0, "inner elongation under 90°: {e}");
                    }
                    None => assert!(w.orbit.0 > anchor.orbit.0, "outer loops at opposition"),
                }
            }
        }
    }

    #[test]
    fn no_wanderer_crowds_the_anchor() {
        for seed in 0..64u64 {
            let s = Seed(seed).derive(crate::streams::ROOT);
            let star = generate_star(s);
            let anchor = generate_anchor(s, &star, &SkyPins::default()).unwrap();
            for w in generate_wanderers(s, &star, &anchor, &SkyPins::default()) {
                let ratio = w.orbit.0 / anchor.orbit.0;
                assert!(
                    !(0.75..=1.8).contains(&ratio),
                    "exclusion band violated: {ratio}"
                );
            }
        }
    }
}
