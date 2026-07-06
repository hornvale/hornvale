//! The anchor world: placed in the habitable zone by construction
//! (spec §2.1). Rotation and obliquity drawn or pinned.

use crate::pins::{GenesisError, RotationPin, SkyPins};
use crate::star::Star;
use crate::streams;
use hornvale_kernel::Seed;

/// Rotation regime of the anchor world.
#[derive(Debug, Clone, PartialEq)]
pub enum Rotation {
    /// Ordinary spin with a solar day of this many standard days.
    Spinning {
        /// Day length in standard days.
        day_std_days: f64,
    },
    /// Tidally locked: no local solar day exists.
    Locked,
}

/// The habitable anchor world.
#[derive(Debug, Clone, PartialEq)]
pub struct Anchor {
    /// Mass in Earth masses (drawn, 0.5–2.0).
    pub mass_earths: f64,
    /// Orbital distance in AU (in the habitable zone by construction).
    pub orbit_au: f64,
    /// Year length in standard days (derived: Kepler III).
    pub year_std_days: f64,
    /// Rotation regime (drawn or pinned).
    pub rotation: Rotation,
    /// Axial tilt in degrees (drawn 0–35 or pinned).
    pub obliquity_deg: f64,
}

fn year_from_orbit(orbit_au: f64, star_mass: f64) -> f64 {
    365.25 * (orbit_au.powi(3) / star_mass).sqrt()
}

/// Generate the anchor: in-zone by construction, pins conditioned on.
pub fn generate_anchor(
    astronomy_seed: Seed,
    star: &Star,
    pins: &SkyPins,
) -> Result<Anchor, GenesisError> {
    let mass_earths = 0.5
        + astronomy_seed
            .derive(streams::ANCHOR_MASS)
            .stream()
            .next_f64()
            * 1.5;

    let rotation = match &pins.rotation {
        Some(RotationPin::Locked) => Rotation::Locked,
        Some(RotationPin::PeriodHours(h)) => {
            if !(4.0..=100.0).contains(h) {
                return Err(GenesisError::InvalidPin {
                    pin: "day-hours".to_string(),
                    reason: format!("{h} hours is outside the legal range 4–100"),
                });
            }
            Rotation::Spinning {
                day_std_days: h / 24.0,
            }
        }
        Some(RotationPin::Normal) | None => {
            let mut stream = astronomy_seed.derive(streams::ROTATION).stream();
            let locked = pins.rotation.is_none() && stream.next_f64() < 0.05;
            if locked {
                Rotation::Locked
            } else {
                Rotation::Spinning {
                    day_std_days: (16.0 + stream.next_f64() * 24.0) / 24.0,
                }
            }
        }
    };

    let (inner, outer) = star.habitable_zone_au;
    let (orbit_au, year_std_days) = match pins.year_local_days {
        Some(local_days) => {
            let Rotation::Spinning { day_std_days } = rotation else {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "year-days".to_string(),
                    reason: "a tidally locked world has no local days to count a year in"
                        .to_string(),
                });
            };
            if !local_days.is_finite() || local_days <= 0.0 {
                return Err(GenesisError::InvalidPin {
                    pin: "year-days".to_string(),
                    reason: format!("{local_days} local days is not a positive, finite year"),
                });
            }
            let year_std = local_days * day_std_days;
            let orbit = (star.mass_solar * (year_std / 365.25).powi(2)).powf(1.0 / 3.0);
            if !(inner..=outer).contains(&orbit) {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "year-days".to_string(),
                    reason: format!(
                        "a {local_days}-local-day year places the anchor at {orbit:.2} AU, \
                         outside the habitable zone ({inner:.2}–{outer:.2} AU)"
                    ),
                });
            }
            (orbit, year_std)
        }
        None => {
            let orbit =
                inner + astronomy_seed.derive(streams::ORBIT).stream().next_f64() * (outer - inner);
            (orbit, year_from_orbit(orbit, star.mass_solar))
        }
    };

    let obliquity_deg = match pins.obliquity_deg {
        Some(deg) => {
            if !(0.0..=35.0).contains(&deg) {
                return Err(GenesisError::InvalidPin {
                    pin: "obliquity".to_string(),
                    reason: format!("{deg}° is outside the legal range 0–35"),
                });
            }
            deg
        }
        None => {
            astronomy_seed
                .derive(streams::OBLIQUITY)
                .stream()
                .next_f64()
                * 35.0
        }
    };

    Ok(Anchor {
        mass_earths,
        orbit_au,
        year_std_days,
        rotation,
        obliquity_deg,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::star::generate_star;

    fn star() -> Star {
        generate_star(Seed(42))
    }

    #[test]
    fn anchor_is_deterministic_and_in_zone() {
        let s = star();
        let a = generate_anchor(Seed(42), &s, &SkyPins::default()).unwrap();
        assert_eq!(
            a,
            generate_anchor(Seed(42), &s, &SkyPins::default()).unwrap()
        );
        let (inner, outer) = s.habitable_zone_au;
        assert!((inner..=outer).contains(&a.orbit_au));
        assert!((0.5..=2.0).contains(&a.mass_earths));
        assert!((0.0..=35.0).contains(&a.obliquity_deg));
    }

    #[test]
    fn year_satisfies_kepler() {
        let s = star();
        let a = generate_anchor(Seed(7), &s, &SkyPins::default()).unwrap();
        let expected = 365.25 * (a.orbit_au.powi(3) / s.mass_solar).sqrt();
        assert!((a.year_std_days - expected).abs() < 1e-9);
    }

    #[test]
    fn rotation_pins_are_honored() {
        let s = star();
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        };
        let a = generate_anchor(Seed(1), &s, &pins).unwrap();
        assert_eq!(a.rotation, Rotation::Locked);

        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(30.0)),
            ..SkyPins::default()
        };
        let a = generate_anchor(Seed(1), &s, &pins).unwrap();
        assert_eq!(a.rotation, Rotation::Spinning { day_std_days: 1.25 });
    }

    #[test]
    fn rotation_period_pin_validates_range() {
        let s = star();
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(2.0)),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::InvalidPin { .. })
        ));
    }

    #[test]
    fn pinned_year_places_orbit_by_kepler_or_fails_loudly() {
        let s = star();
        // Round-trip: pin the year a default anchor actually has (converted
        // to local days at a pinned 24h day) and the derived orbit must land
        // where Kepler puts it — inside the zone, near the unpinned orbit.
        // Seed-robust: works whatever star the seed drew.
        let default_anchor = generate_anchor(Seed(1), &s, &SkyPins::default()).unwrap();
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            year_local_days: Some(default_anchor.year_std_days), // 24h day => local == std
            ..SkyPins::default()
        };
        let a = generate_anchor(Seed(1), &s, &pins).unwrap();
        let (inner, outer) = s.habitable_zone_au;
        assert!((inner..=outer).contains(&a.orbit_au));
        assert!((a.orbit_au - default_anchor.orbit_au).abs() < 1e-9);
        // An absurd year lands far outside the zone for ANY legal star
        // (a >= (0.6·(4000/365.25)²)^(1/3) ≈ 4.2 AU; max outer ≈ 2.5 AU).
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            year_local_days: Some(4000.0),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::UnsatisfiablePin { .. })
        ));
    }

    #[test]
    fn pinned_year_on_locked_world_is_a_conflict() {
        let s = star();
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            year_local_days: Some(300.0),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::UnsatisfiablePin { .. })
        ));
    }

    #[test]
    fn year_pin_rejects_non_positive_values() {
        let s = star();
        for bad in [-365.0, 0.0, f64::NAN] {
            let pins = SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                year_local_days: Some(bad),
                ..SkyPins::default()
            };
            assert!(
                matches!(
                    generate_anchor(Seed(1), &s, &pins),
                    Err(GenesisError::InvalidPin { .. })
                ),
                "expected InvalidPin for year_local_days = {bad}"
            );
        }
    }

    #[test]
    fn obliquity_pin_is_honored_and_validated() {
        let s = star();
        let pins = SkyPins {
            obliquity_deg: Some(0.0),
            ..SkyPins::default()
        };
        assert_eq!(
            generate_anchor(Seed(1), &s, &pins).unwrap().obliquity_deg,
            0.0
        );
        let pins = SkyPins {
            obliquity_deg: Some(60.0),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::InvalidPin { .. })
        ));
    }
}
