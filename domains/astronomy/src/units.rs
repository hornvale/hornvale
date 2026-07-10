//! Typed quantities (spec §2.5): coherent units as validating newtypes so
//! invalid states are unrepresentable at API boundaries. In-crate formulas
//! use the pub(crate) inner field; dimensionless ratios stay bare f64.

use std::fmt;

/// Why a quantity constructor refused a value.
/// type-audit: bare-ok(identifier-text: unit), bare-ok(diagnostic-value: value), bare-ok(identifier-text: reason)
#[derive(Debug, Clone, PartialEq)]
pub struct UnitError {
    /// Human name of the unit ("solar masses").
    pub unit: &'static str,
    /// The rejected value.
    pub value: f64,
    /// The rule it violates.
    pub reason: &'static str,
}

impl fmt::Display for UnitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} is not a valid quantity of {}: {}",
            self.value, self.unit, self.reason
        )
    }
}

impl std::error::Error for UnitError {}

macro_rules! quantity {
    ($name:ident, $label:literal, positive, $doc:literal) => {
        #[doc = $doc]
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
        pub struct $name(pub(crate) f64);
        impl $name {
            /// Validating constructor: finite and strictly positive.
            pub fn new(value: f64) -> Result<Self, UnitError> {
                if !value.is_finite() {
                    return Err(UnitError {
                        unit: $label,
                        value,
                        reason: "must be finite",
                    });
                }
                if value <= 0.0 {
                    return Err(UnitError {
                        unit: $label,
                        value,
                        reason: "must be positive",
                    });
                }
                Ok(Self(value))
            }
            /// The raw value.
            pub fn get(self) -> f64 {
                self.0
            }
        }
    };
    ($name:ident, $label:literal, non_negative, $doc:literal) => {
        #[doc = $doc]
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
        pub struct $name(pub(crate) f64);
        impl $name {
            /// Validating constructor: finite and non-negative (time points may be zero).
            pub fn new(value: f64) -> Result<Self, UnitError> {
                if !value.is_finite() {
                    return Err(UnitError {
                        unit: $label,
                        value,
                        reason: "must be finite",
                    });
                }
                if value < 0.0 {
                    return Err(UnitError {
                        unit: $label,
                        value,
                        reason: "must be non-negative",
                    });
                }
                Ok(Self(value))
            }
            /// The raw value.
            pub fn get(self) -> f64 {
                self.0
            }
        }
    };
}

quantity!(
    SolarMasses,
    "solar masses",
    positive,
    "Stellar mass in solar masses."
);
quantity!(
    SolarLuminosities,
    "solar luminosities",
    positive,
    "Luminosity in solar units."
);
quantity!(
    EarthMasses,
    "Earth masses",
    positive,
    "Planetary mass in Earth masses."
);
quantity!(
    LunarMasses,
    "lunar masses",
    positive,
    "Moon mass in lunar masses (Luna = 1)."
);
quantity!(
    Au,
    "astronomical units",
    positive,
    "Orbital distance in AU."
);
quantity!(
    Megameters,
    "megameters",
    positive,
    "Distance in Mm (1000 km; Luna orbits at 384.4)."
);
quantity!(
    LightYears,
    "light-years",
    positive,
    "Interstellar distance in light-years."
);
quantity!(
    StdDays,
    "standard days",
    non_negative,
    "Absolute time or duration in standard days."
);
quantity!(
    LocalDays,
    "local days",
    non_negative,
    "Time or duration in a world's own days."
);

/// An angle in degrees, valid in [0, 360).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Degrees(pub(crate) f64);

impl Degrees {
    /// Validating constructor: finite, 0 <= v < 360.
    /// type-audit: bare-ok(constructor-edge)
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError {
                unit: "degrees",
                value,
                reason: "must be finite",
            });
        }
        if !(0.0..360.0).contains(&value) {
            return Err(UnitError {
                unit: "degrees",
                value,
                reason: "must be in [0, 360)",
            });
        }
        Ok(Self(value))
    }
    /// The raw value.
    /// type-audit: bare-ok(constructor-edge)
    pub fn get(self) -> f64 {
        self.0
    }
}

/// A star's circumstellar habitable zone: the annulus (in AU) where liquid
/// water is possible. Inner strictly precedes outer, enforced at
/// construction — anchor-first genesis makes a zone-less world
/// unrepresentable, so this is never `Option`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HabitableZone {
    inner: Au,
    outer: Au,
}

impl HabitableZone {
    /// Validating constructor: both bounds finite and `inner < outer`.
    pub fn new(inner: Au, outer: Au) -> Result<HabitableZone, UnitError> {
        if inner.0 >= outer.0 {
            return Err(UnitError {
                unit: "habitable zone",
                value: inner.0,
                reason: "inner bound must be strictly less than outer",
            });
        }
        Ok(HabitableZone { inner, outer })
    }
    /// The inner (hot) bound.
    pub fn inner(self) -> Au {
        self.inner
    }
    /// The outer (cold) bound.
    pub fn outer(self) -> Au {
        self.outer
    }
    /// The arithmetic center of the zone.
    pub fn center(self) -> Au {
        Au((self.inner.0 + self.outer.0) / 2.0)
    }
    /// Whether `orbit` lies within `[inner, outer]`.
    /// type-audit: bare-ok(flag)
    pub fn contains(self, orbit: Au) -> bool {
        (self.inner.0..=self.outer.0).contains(&orbit.0)
    }
}

impl StdDays {
    /// A duration given in standard hours.
    /// type-audit: bare-ok(constructor-edge)
    pub fn from_hours(hours: f64) -> Result<StdDays, UnitError> {
        StdDays::new(hours / 24.0)
    }
    /// Express this absolute time/duration in a world's own days.
    pub fn in_local(self, day_length: StdDays) -> LocalDays {
        LocalDays(self.0 / day_length.0)
    }
}

impl LocalDays {
    /// Express local days back in standard days.
    pub fn in_std(self, day_length: StdDays) -> StdDays {
        StdDays(self.0 * day_length.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructors_accept_valid_and_reject_invalid() {
        assert!(SolarMasses::new(1.0).is_ok());
        assert!(SolarMasses::new(0.0).is_err());
        assert!(SolarMasses::new(-1.0).is_err());
        assert!(SolarMasses::new(f64::NAN).is_err());
        assert!(Au::new(1.0).is_ok());
        assert!(Au::new(f64::INFINITY).is_err());
        assert!(StdDays::new(0.0).is_ok(), "time points may be zero");
        assert!(StdDays::new(-0.5).is_err());
        assert!(LocalDays::new(0.0).is_ok());
        assert!(Degrees::new(0.0).is_ok());
        assert!(Degrees::new(359.9).is_ok());
        assert!(Degrees::new(360.0).is_err());
        assert!(Degrees::new(-1.0).is_err());
    }

    #[test]
    fn errors_name_the_unit_and_reason() {
        let e = SolarMasses::new(-2.0).unwrap_err();
        let text = e.to_string();
        assert!(text.contains("solar masses"));
        assert!(text.contains("-2"));
    }

    #[test]
    fn hours_and_local_conversions_round_trip() {
        let day = StdDays::from_hours(24.0).unwrap();
        assert_eq!(day.get(), 1.0);
        let day30 = StdDays::from_hours(30.0).unwrap();
        let local = StdDays::new(2.5).unwrap().in_local(day30);
        assert_eq!(local.get(), 2.0);
        assert_eq!(local.in_std(day30).get(), 2.5);
    }

    #[test]
    fn habitable_zone_enforces_inner_before_outer_and_answers_containment() {
        let zone = HabitableZone::new(Au::new(0.9).unwrap(), Au::new(1.4).unwrap()).unwrap();
        assert_eq!(zone.inner().get(), 0.9);
        assert_eq!(zone.outer().get(), 1.4);
        assert!(zone.contains(Au::new(1.0).unwrap()));
        assert!(!zone.contains(Au::new(2.0).unwrap()));
        assert!((zone.center().get() - 1.15).abs() < 1e-12);
        // inner must strictly precede outer
        assert!(HabitableZone::new(Au::new(1.4).unwrap(), Au::new(0.9).unwrap()).is_err());
        assert!(HabitableZone::new(Au::new(1.0).unwrap(), Au::new(1.0).unwrap()).is_err());
    }
}
