//! Main-sequence star generation: mass drawn, everything else derived
//! (model card: L = M^3.5; habitable zone 0.95√L–1.37√L AU).

use crate::streams;
use hornvale_kernel::Seed;

/// A main-sequence star: mass drawn, everything else derived.
#[derive(Debug, Clone, PartialEq)]
pub struct Star {
    /// Mass in solar masses (drawn, 0.6–1.4).
    pub mass_solar: f64,
    /// Luminosity in solar units (derived: M^3.5).
    pub luminosity_solar: f64,
    /// Human-readable spectral character.
    pub class_name: String,
    /// Habitable-zone bounds in AU (derived: 0.95√L, 1.37√L).
    pub habitable_zone_au: (f64, f64),
}

/// Generate the star from the astronomy domain seed.
pub fn generate_star(astronomy_seed: Seed) -> Star {
    let mut stream = astronomy_seed.derive(streams::STAR_MASS).stream();
    let mass_solar = 0.6 + stream.next_f64() * 0.8;
    let luminosity_solar = mass_solar.powf(3.5);
    let sqrt_l = luminosity_solar.sqrt();
    let class_name = if mass_solar < 0.8 {
        "orange dwarf (K)"
    } else if mass_solar < 1.05 {
        "yellow dwarf (G)"
    } else {
        "yellow-white dwarf (F)"
    }
    .to_string();
    Star {
        mass_solar,
        luminosity_solar,
        class_name,
        habitable_zone_au: (0.95 * sqrt_l, 1.37 * sqrt_l),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn star_is_deterministic_and_in_range() {
        let a = generate_star(Seed(42));
        assert_eq!(a, generate_star(Seed(42)));
        assert!((0.6..=1.4).contains(&a.mass_solar));
    }

    #[test]
    fn luminosity_and_zone_are_derived() {
        let s = generate_star(Seed(7));
        let expected_l = s.mass_solar.powf(3.5);
        assert!((s.luminosity_solar - expected_l).abs() < 1e-12);
        let (inner, outer) = s.habitable_zone_au;
        assert!((inner - 0.95 * expected_l.sqrt()).abs() < 1e-12);
        assert!((outer - 1.37 * expected_l.sqrt()).abs() < 1e-12);
        assert!(inner < outer);
    }

    #[test]
    fn class_names_track_mass() {
        for seed in 0..32 {
            let s = generate_star(Seed(seed));
            let expected = if s.mass_solar < 0.8 {
                "orange dwarf (K)"
            } else if s.mass_solar < 1.05 {
                "yellow dwarf (G)"
            } else {
                "yellow-white dwarf (F)"
            };
            assert_eq!(s.class_name, expected);
        }
    }
}
