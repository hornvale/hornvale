//! Main-sequence star generation: mass drawn, everything else derived
//! (model card: L = M^3.5; habitable zone 0.95√L–1.37√L AU).

use crate::streams;
use crate::units::{Au, Gyr, HabitableZone, SolarLuminosities, SolarMasses};
use hornvale_kernel::Seed;
use hornvale_kernel::math;

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
    /// Stellar age in Gyr (drawn, The Reckoning). Bounded by
    /// [`main_sequence_lifetime`] and [`T_MAX`]. **Does not feed
    /// `luminosity` or `habitable_zone`** (spec §3, the containment rule) —
    /// those stay exactly `M^3.5`-derived so age can never move a world's
    /// insolation, orbit admission, or climate.
    pub age: Gyr,
}

/// The main-sequence bound on a drawn age. **Not 13.8 Gyr**: this is a bound,
/// not a cosmology. Capping at the age of *our* universe would settle a
/// metaphysical question (UNI-2) that the project deliberately leaves open, as
/// a side effect of a generator constant. 15 Gyr bounds the draw without
/// dating the universe.
/// type-audit: bare-ok(constant)
pub const T_MAX: Gyr = Gyr(15.0);

/// Main-sequence lifetime: t_MS = 10 Gyr · M^-2.5 (declared approximation —
/// the Sol-calibrated scaling already implicit in `brightening_per_gyr`, made
/// explicit; not a stellar-structure model).
pub fn main_sequence_lifetime(star: &Star) -> Gyr {
    Gyr(10.0 * math::powf(star.mass.0, -2.5))
}

/// The planet's age: terrestrial accretion completes within ~30–100 Myr of the
/// star, so the planet trails it by a token 0.05 Gyr. Derived, not drawn — the
/// difference is under 1% and below anything the sim observes; it is modelled
/// only so the number exists and is honest about its own precision.
pub fn planet_age(star: &Star) -> Gyr {
    Gyr((star.age.0 - 0.05).max(0.0))
}

/// Generate the star from the astronomy domain seed.
pub fn generate_star(astronomy_seed: Seed) -> Star {
    let mut stream = astronomy_seed.derive(streams::STAR_MASS).stream();
    let mass = SolarMasses(0.6 + stream.next_f64() * 0.8);
    let luminosity = SolarLuminosities(math::powf(mass.0, 3.5));
    let sqrt_l = luminosity.0.sqrt();
    let class_name = if mass.0 < 0.8 {
        "orange dwarf (K)"
    } else if mass.0 < 1.05 {
        "yellow dwarf (G)"
    } else {
        "yellow-white dwarf (F)"
    }
    .to_string();
    let ceiling = (10.0 * math::powf(mass.0, -2.5)).min(T_MAX.0);
    let age =
        Gyr((0.05 + astronomy_seed.derive(streams::STAR_AGE).stream().next_f64() * 0.90) * ceiling);
    Star {
        mass,
        luminosity,
        class_name,
        habitable_zone: HabitableZone::new(Au(0.95 * sqrt_l), Au(1.37 * sqrt_l))
            .expect("0.95√L < 1.37√L for all L > 0"),
        age,
    }
}

/// Apparent angular diameter of the star seen from `orbit`, relative to
/// Sol from 1 AU (SKY-7). The stellar radius comes from the main-sequence
/// mass–radius relation R = M^0.8 (declared approximation, model card) —
/// the disc a moon must cover to eclipse it.
/// type-audit: pending(wave-1)
pub fn sun_angular_diameter_rel(star: &Star, orbit: Au) -> f64 {
    math::powf(star.mass.0, 0.8) / orbit.0
}

/// Top-of-atmosphere stellar flux at the anchor's orbit, relative to Earth's
/// (L / a², Earth = 1). Global annual mean; a genesis-time scalar that does
/// not carry the seasonal (obliquity) or deep-time (eccentricity) variation
/// the forcing parameters model. This is the single definition of insolation
/// the whole workspace shares (SKY-15).
/// type-audit: pending(wave-1)
pub fn insolation_rel(star: &Star, anchor: &crate::anchor::Anchor) -> f64 {
    star.luminosity.0 / (anchor.orbit.0 * anchor.orbit.0)
}

/// Standard days per gigayear — the timescale secular brightening lives on.
/// type-audit: pending(wave-1)
pub const GYR_DAYS: f64 = 1.0e9 * 365.25;

/// Fractional main-sequence brightening per gigayear: b = 0.10 · M^2.5
/// (declared approximation, model card — Sol-calibrated at ~10%/Gyr,
/// scaled by the main-sequence lifetime t_MS ∝ M⁻²·⁵). Draw-free: mass
/// drawn, everything else derived.
/// type-audit: bare-ok(ratio)
pub fn brightening_per_gyr(star: &Star) -> f64 {
    0.10 * math::powf(star.mass.0, 2.5)
}

/// Luminosity at absolute time `t`, anchored so `luminosity_at(star, 0)`
/// equals the genesis luminosity exactly. The habitable zone remains a
/// genesis-epoch derivation from L₀ (the world lives on kiloyear scales,
/// where this slope is honestly negligible — deep time is where it shows).
pub fn luminosity_at(star: &Star, t: crate::units::StdDays) -> SolarLuminosities {
    SolarLuminosities(star.luminosity.0 * (1.0 + brightening_per_gyr(star) * t.0 / GYR_DAYS))
}

/// Time-aware insolation: `luminosity_at / a²` — [`insolation_rel`]'s
/// deep-time sibling (SKY-15's shared definition, evaluated at `t`).
/// type-audit: pending(wave-1)
pub fn insolation_rel_at(
    star: &Star,
    anchor: &crate::anchor::Anchor,
    t: crate::units::StdDays,
) -> f64 {
    luminosity_at(star, t).0 / (anchor.orbit.0 * anchor.orbit.0)
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
        let expected_l = math::powf(s.mass.get(), 3.5);
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
            age: Gyr::new(4.5).unwrap(),
        };
        assert!((sun_angular_diameter_rel(&sol, Au::new(1.0).unwrap()) - 1.0).abs() < 1e-12);
        // A heavier star seen from a wider orbit: θ = M^0.8 / a.
        let heavy = Star {
            mass: SolarMasses::new(1.4).unwrap(),
            ..sol.clone()
        };
        let expected = math::powf(1.4_f64, 0.8) / 1.2;
        assert!((sun_angular_diameter_rel(&heavy, Au::new(1.2).unwrap()) - expected).abs() < 1e-12);
    }

    #[test]
    fn insolation_is_luminosity_over_orbit_squared() {
        use crate::anchor::generate_anchor;
        use crate::pins::SkyPins;
        let star = generate_star(Seed(42));
        let anchor = generate_anchor(Seed(42), &star, &SkyPins::default()).unwrap();
        let expected = star.luminosity.get() / (anchor.orbit.get() * anchor.orbit.get());
        assert!((insolation_rel(&star, &anchor) - expected).abs() < 1e-12);
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

    /// The Long Count (SKY-1 close-out): luminosity is anchored at the
    /// present and brightens on the main-sequence slope — 10%/Gyr at one
    /// solar mass, faster for heavier stars (b = 0.10·M^2.5).
    #[test]
    fn luminosity_brightens_on_the_main_sequence_slope() {
        use crate::units::StdDays;
        let s = generate_star(Seed(42));
        assert_eq!(luminosity_at(&s, StdDays(0.0)), s.luminosity);
        let after_gyr = luminosity_at(&s, StdDays(GYR_DAYS));
        let expected = s.luminosity.get() * (1.0 + brightening_per_gyr(&s));
        assert!((after_gyr.get() - expected).abs() < 1e-12);
        let b = brightening_per_gyr(&s);
        assert!((b - 0.10 * math::powf(s.mass.get(), 2.5)).abs() < 1e-15);
    }

    /// Heavier stars age faster, and insolation follows luminosity.
    #[test]
    fn brightening_scales_with_mass_and_reaches_insolation() {
        use crate::pins::SkyPins;
        use crate::units::StdDays;
        let mut light = generate_star(Seed(42));
        light.mass = SolarMasses::new(0.7).unwrap();
        let mut heavy = light.clone();
        heavy.mass = SolarMasses::new(1.3).unwrap();
        assert!(brightening_per_gyr(&heavy) > brightening_per_gyr(&light));
        let star = generate_star(Seed(42));
        let anchor = crate::anchor::generate_anchor(Seed(42), &star, &SkyPins::default()).unwrap();
        assert_eq!(
            insolation_rel_at(&star, &anchor, StdDays(0.0)),
            insolation_rel(&star, &anchor)
        );
        assert!(
            insolation_rel_at(&star, &anchor, StdDays(GYR_DAYS)) > insolation_rel(&star, &anchor)
        );
    }

    #[test]
    fn main_sequence_lifetime_scales_as_mass_to_the_minus_five_halves() {
        // Sol-calibrated: 1.0 Msun -> 10 Gyr.
        let sol = generate_star(Seed(1));
        let t = main_sequence_lifetime(&Star {
            mass: SolarMasses(1.0),
            ..sol.clone()
        });
        assert!((t.0 - 10.0).abs() < 1e-9, "{}", t.0);
        // A heavier star burns out faster; a lighter one outlives the bound.
        let heavy = main_sequence_lifetime(&Star {
            mass: SolarMasses(1.4),
            ..sol.clone()
        });
        let light = main_sequence_lifetime(&Star {
            mass: SolarMasses(0.6),
            ..sol
        });
        assert!(heavy.0 < 5.0, "1.4 Msun t_MS = {}", heavy.0);
        assert!(light.0 > 30.0, "0.6 Msun t_MS = {}", light.0);
    }

    #[test]
    fn age_stays_inside_the_guard_rails_and_the_bound() {
        for seed in 0..200u64 {
            let star = generate_star(Seed(seed));
            let t_ms = main_sequence_lifetime(&star);
            let ceiling = t_ms.0.min(T_MAX.0);
            assert!(
                star.age.0 >= 0.05 * ceiling,
                "seed {seed}: age {} below rail",
                star.age.0
            );
            assert!(
                star.age.0 <= 0.95 * ceiling,
                "seed {seed}: age {} above rail",
                star.age.0
            );
            assert!(
                star.age.0 <= T_MAX.0,
                "seed {seed}: age {} exceeds T_MAX",
                star.age.0
            );
        }
    }

    /// The emergent result the 15 Gyr bound buys (spec §4): a 0.6 Msun star's
    /// t_MS is ~35.9 Gyr, so T_MAX caps it at ~42% of its life — it has
    /// necessarily brightened little. This is true of real K dwarfs.
    #[test]
    fn a_k_dwarf_is_bounded_young_in_main_sequence_terms() {
        let sol = generate_star(Seed(1));
        let k = Star {
            mass: SolarMasses(0.6),
            ..sol
        };
        let t_ms = main_sequence_lifetime(&k);
        let max_fraction = T_MAX.0 / t_ms.0;
        assert!(
            max_fraction < 0.45,
            "K dwarf can reach {max_fraction} of its life"
        );
    }

    #[test]
    fn planet_age_trails_the_star_by_the_accretion_interval() {
        let star = generate_star(Seed(42));
        let p = planet_age(&star);
        assert!((star.age.0 - p.0 - 0.05).abs() < 1e-9);
        assert!(p.0 >= 0.0);
    }

    #[test]
    fn age_is_deterministic() {
        assert_eq!(generate_star(Seed(42)).age, generate_star(Seed(42)).age);
    }
}
