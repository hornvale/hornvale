//! Typed quantities (spec §2.5): coherent units as validating newtypes so
//! invalid states are unrepresentable at API boundaries. In-crate formulas
//! use the pub(crate) inner field; dimensionless ratios stay bare f64.

use std::fmt;

/// Why a quantity constructor refused a value.
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
    Mm,
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
    pub fn get(self) -> f64 {
        self.0
    }
}

impl StdDays {
    /// A duration given in standard hours.
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
}
