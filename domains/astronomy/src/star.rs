//! Main-sequence star generation: mass drawn, everything else derived
//! (model card: L = M^3.5; habitable zone 0.95√L–1.37√L AU).

use crate::streams;
use crate::units::{Au, HabitableZone, SolarLuminosities, SolarMasses};
use hornvale_kernel::Seed;

/// A main-sequence star: mass drawn, everything else derived.
/// type-audit: bare-ok(identifier-text)
#[derive(Debug, Clone, PartialEq)]
pub struct Star {
    /// Mass in solar masses (drawn, 0.6–1.4).
    pub mass: SolarMasses,
    /// Luminosity in solar units (derived: M^3.5).
    pub luminosity: SolarLuminosities,
    /// Human-readable spectral character.
    pub class_name: String,
    /// Habitable-zone bounds in AU (derived: 0.95√L inner, 1.37√L outer).
    pub habitable_zone: HabitableZone,
}

/// Generate the star from the astronomy domain seed.
pub fn generate_star(astronomy_seed: Seed) -> Star {
    let mut stream = astronomy_seed.derive(streams::STAR_MASS).stream();
    let mass = SolarMasses(0.6 + stream.next_f64() * 0.8);
    let luminosity = SolarLuminosities(mass.0.powf(3.5));
    let sqrt_l = luminosity.0.sqrt();
    let class_name = if mass.0 < 0.8 {
        "orange dwarf (K)"
    } else if mass.0 < 1.05 {
        "yellow dwarf (G)"
    } else {
        "yellow-white dwarf (F)"
    }
    .to_string();
    Star {
        mass,
        luminosity,
        class_name,
        habitable_zone: HabitableZone::new(Au(0.95 * sqrt_l), Au(1.37 * sqrt_l))
            .expect("0.95√L < 1.37√L for all L > 0"),
    }
}

/// Apparent angular diameter of the star seen from `orbit`, relative to
/// Sol from 1 AU (SKY-7). The stellar radius comes from the main-sequence
/// mass–radius relation R = M^0.8 (declared approximation, model card) —
/// the disc a moon must cover to eclipse it.
/// type-audit: pending(wave-1)
pub fn sun_angular_diameter_rel(star: &Star, orbit: Au) -> f64 {
    star.mass.0.powf(0.8) / orbit.0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn star_is_deterministic_and_in_range() {
        let a = generate_star(Seed(42));
        assert_eq!(a, generate_star(Seed(42)));
        assert!((0.6..=1.4).contains(&a.mass.get()));
    }

    #[test]
    fn luminosity_and_zone_are_derived() {
        let s = generate_star(Seed(7));
        let expected_l = s.mass.get().powf(3.5);
        assert!((s.luminosity.get() - expected_l).abs() < 1e-12);
        let zone = s.habitable_zone;
        assert!((zone.inner().get() - 0.95 * expected_l.sqrt()).abs() < 1e-12);
        assert!((zone.outer().get() - 1.37 * expected_l.sqrt()).abs() < 1e-12);
        assert!(zone.inner().get() < zone.outer().get());
    }

    /// SKY-7: the sun finally has an angular size — Sol from 1 AU is the
    /// unit, and the main-sequence mass–radius relation (R = M^0.8) sets
    /// the disc.
    #[test]
    fn sun_angular_diameter_follows_the_main_sequence_radius() {
        let sol = Star {
            mass: SolarMasses::new(1.0).unwrap(),
            luminosity: SolarLuminosities::new(1.0).unwrap(),
            class_name: "yellow dwarf (G)".to_string(),
            habitable_zone: HabitableZone::new(Au::new(0.95).unwrap(), Au::new(1.37).unwrap())
                .unwrap(),
        };
        assert!((sun_angular_diameter_rel(&sol, Au::new(1.0).unwrap()) - 1.0).abs() < 1e-12);
        // A heavier star seen from a wider orbit: θ = M^0.8 / a.
        let heavy = Star {
            mass: SolarMasses::new(1.4).unwrap(),
            ..sol.clone()
        };
        let expected = 1.4_f64.powf(0.8) / 1.2;
        assert!((sun_angular_diameter_rel(&heavy, Au::new(1.2).unwrap()) - expected).abs() < 1e-12);
    }

    #[test]
    fn class_names_track_mass() {
        for seed in 0..32 {
            let s = generate_star(Seed(seed));
            let expected = if s.mass.get() < 0.8 {
                "orange dwarf (K)"
            } else if s.mass.get() < 1.05 {
                "yellow dwarf (G)"
            } else {
                "yellow-white dwarf (F)"
            };
            assert_eq!(s.class_name, expected);
        }
    }
}
