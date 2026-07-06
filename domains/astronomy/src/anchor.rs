//! The anchor world: placed in the habitable zone by construction
//! (spec §2.1). Rotation and obliquity drawn or pinned.

use crate::pins::{GenesisError, RotationPin, SkyPins};
use crate::star::Star;
use crate::streams;
use crate::units::{Au, Degrees, EarthMasses, SolarMasses, StdDays};
use hornvale_kernel::Seed;

/// Rotation regime of the anchor world.
#[derive(Debug, Clone, PartialEq)]
pub enum Rotation {
    /// Ordinary spin with a solar day of this many standard days.
    Spinning {
        /// Day length in standard days.
        day: StdDays,
    },
    /// Tidally locked: no local solar day exists.
    Locked,
}

/// The habitable anchor world.
#[derive(Debug, Clone, PartialEq)]
pub struct Anchor {
    /// Mass in Earth masses (drawn, 0.5–2.0).
    pub mass: EarthMasses,
    /// Orbital distance in AU (in the habitable zone by construction).
    pub orbit: Au,
    /// Year length in standard days (derived: Kepler III).
    pub year: StdDays,
    /// Rotation regime (drawn or pinned).
    pub rotation: Rotation,
    /// Axial tilt in degrees (drawn 0–35 or pinned).
    pub obliquity: Degrees,
}

fn year_from_orbit(orbit: Au, star_mass: SolarMasses) -> StdDays {
    StdDays(365.25 * (orbit.0.powi(3) / star_mass.0).sqrt())
}

/// Generate the anchor: in-zone by construction, pins conditioned on.
pub fn generate_anchor(
    astronomy_seed: Seed,
    star: &Star,
    pins: &SkyPins,
) -> Result<Anchor, GenesisError> {
    let mass = EarthMasses(
        0.5 + astronomy_seed
            .derive(streams::ANCHOR_MASS)
            .stream()
            .next_f64()
            * 1.5,
    );

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
                day: StdDays(h / 24.0),
            }
        }
        Some(RotationPin::Normal) | None => {
            let mut stream = astronomy_seed.derive(streams::ROTATION).stream();
            let lock_roll = stream.next_f64();
            let locked = pins.rotation.is_none() && lock_roll < 0.05;
            if locked {
                Rotation::Locked
            } else {
                Rotation::Spinning {
                    day: StdDays((16.0 + stream.next_f64() * 24.0) / 24.0),
                }
            }
        }
    };

    let (inner, outer) = (star.habitable_zone.inner(), star.habitable_zone.outer());
    let (orbit, year) = match pins.year_local_days {
        Some(local_days) => {
            let Rotation::Spinning { day } = rotation else {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "year-days".to_string(),
                    reason: "a tidally locked world has no local days to count a year in"
                        .to_string(),
                });
            };
            // Zero is a valid time POINT (LocalDays permits it) but not a
            // valid year length; negative/non-finite values are already
            // unrepresentable — LocalDays::new rejects them at construction.
            if local_days.get() <= 0.0 {
                return Err(GenesisError::InvalidPin {
                    pin: "year-days".to_string(),
                    reason: format!(
                        "{} local days is not a positive, finite year",
                        local_days.get()
                    ),
                });
            }
            let year_std = StdDays(local_days.0 * day.0);
            let orbit = Au((star.mass.0 * (year_std.0 / 365.25).powi(2)).powf(1.0 / 3.0));
            if !(inner.0..=outer.0).contains(&orbit.0) {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "year-days".to_string(),
                    reason: format!(
                        "a {}-local-day year places the anchor at {:.2} AU, \
                         outside the habitable zone ({:.2}–{:.2} AU)",
                        local_days.get(),
                        orbit.0,
                        inner.0,
                        outer.0
                    ),
                });
            }
            (orbit, year_std)
        }
        None => {
            let orbit = Au(inner.0
                + astronomy_seed.derive(streams::ORBIT).stream().next_f64() * (outer.0 - inner.0));
            (orbit, year_from_orbit(orbit, star.mass))
        }
    };

    let obliquity = match pins.obliquity {
        Some(deg) => {
            if !(0.0..=35.0).contains(&deg.get()) {
                return Err(GenesisError::InvalidPin {
                    pin: "obliquity".to_string(),
                    reason: format!("{}° is outside the legal range 0–35", deg.get()),
                });
            }
            deg
        }
        None => Degrees(
            astronomy_seed
                .derive(streams::OBLIQUITY)
                .stream()
                .next_f64()
                * 35.0,
        ),
    };

    Ok(Anchor {
        mass,
        orbit,
        year,
        rotation,
        obliquity,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::star::generate_star;
    use crate::units::LocalDays;

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
        let (inner, outer) = (s.habitable_zone.inner(), s.habitable_zone.outer());
        assert!((inner.get()..=outer.get()).contains(&a.orbit.get()));
        assert!((0.5..=2.0).contains(&a.mass.get()));
        assert!((0.0..=35.0).contains(&a.obliquity.get()));
    }

    #[test]
    fn year_satisfies_kepler() {
        let s = star();
        let a = generate_anchor(Seed(7), &s, &SkyPins::default()).unwrap();
        let expected = 365.25 * (a.orbit.get().powi(3) / s.mass.get()).sqrt();
        assert!((a.year.get() - expected).abs() < 1e-9);
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
        assert_eq!(a.rotation, Rotation::Spinning { day: StdDays(1.25) });
    }

    #[test]
    fn normal_pin_matches_the_unpinned_draw_for_spinning_worlds() {
        let s = star();
        let default_anchor = generate_anchor(Seed(42), &s, &SkyPins::default()).unwrap();
        assert!(
            matches!(default_anchor.rotation, Rotation::Spinning { .. }),
            "seed 42's default anchor must be Spinning for this test to be meaningful"
        );
        let pins = SkyPins {
            rotation: Some(RotationPin::Normal),
            ..SkyPins::default()
        };
        let pinned_anchor = generate_anchor(Seed(42), &s, &pins).unwrap();
        assert_eq!(pinned_anchor, default_anchor);
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
            year_local_days: Some(LocalDays::new(default_anchor.year.get()).unwrap()), // 24h day => local == std
            ..SkyPins::default()
        };
        let a = generate_anchor(Seed(1), &s, &pins).unwrap();
        let (inner, outer) = (s.habitable_zone.inner(), s.habitable_zone.outer());
        assert!((inner.get()..=outer.get()).contains(&a.orbit.get()));
        assert!((a.orbit.get() - default_anchor.orbit.get()).abs() < 1e-9);
        // An absurd year lands far outside the zone for ANY legal star
        // (a >= (0.6·(4000/365.25)²)^(1/3) ≈ 4.2 AU; max outer ≈ 2.5 AU).
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            year_local_days: Some(LocalDays::new(4000.0).unwrap()),
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
            year_local_days: Some(LocalDays::new(300.0).unwrap()),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::UnsatisfiablePin { .. })
        ));
    }

    #[test]
    fn year_pin_rejects_a_zero_length_year() {
        // Zero is a valid LocalDays time POINT (constructible), but the
        // anchor still refuses it as a year length.
        let s = star();
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            year_local_days: Some(LocalDays::new(0.0).unwrap()),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::InvalidPin { .. })
        ));
    }

    #[test]
    fn negative_and_non_finite_years_are_unrepresentable() {
        // Negative/NaN years can no longer reach the anchor at all: LocalDays
        // rejects them at construction, the CLI boundary for pins.
        assert!(LocalDays::new(-365.0).is_err());
        assert!(LocalDays::new(f64::NAN).is_err());
    }

    #[test]
    fn obliquity_pin_is_honored_and_validated() {
        let s = star();
        let pins = SkyPins {
            obliquity: Some(Degrees::new(0.0).unwrap()),
            ..SkyPins::default()
        };
        assert_eq!(
            generate_anchor(Seed(1), &s, &pins).unwrap().obliquity.get(),
            0.0
        );
        // Degrees permits up to 360, but the anchor keeps its own 0-35 check.
        let pins = SkyPins {
            obliquity: Some(Degrees::new(60.0).unwrap()),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::InvalidPin { .. })
        ));
    }
}
