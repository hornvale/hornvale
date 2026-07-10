//! Typed quantities for paleoclimate (decision 0008): coherent units as
//! validating newtypes; dimensionless ratios stay bare f64.

/// Why a quantity constructor refused a value.
#[derive(Debug, Clone, PartialEq)]
pub struct UnitError {
    /// Human name of the unit.
    pub unit: &'static str,
    /// The rejected value.
    pub value: f64,
    /// The rule it violates.
    pub reason: &'static str,
}

/// Global ice volume as a dimensionless fraction of the maximum, in `[0, 1]`.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct IceVolume(f64);

impl IceVolume {
    /// Validating constructor: finite and within `[0, 1]`.
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError {
                unit: "ice volume",
                value,
                reason: "must be finite",
            });
        }
        if !(0.0..=1.0).contains(&value) {
            return Err(UnitError {
                unit: "ice volume",
                value,
                reason: "must be within [0,1]",
            });
        }
        Ok(Self(value))
    }
    /// The raw fraction.
    pub fn get(self) -> f64 {
        self.0
    }
}

/// A change in eustatic sea level, metres (negative = the sea fell).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct SeaLevelChange(f64);

impl SeaLevelChange {
    /// Validating constructor: finite (sign and magnitude unconstrained).
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError {
                unit: "sea-level change",
                value,
                reason: "must be finite",
            });
        }
        Ok(Self(value))
    }
    /// The raw metres.
    pub fn get(self) -> f64 {
        self.0
    }
}

/// An absolute temperature, degrees Celsius.
///
/// Distinguished at the type level from [`TempAnomaly`] (decision 0008):
/// the two were previously both bare `CellMap<f64>`, and code has twice
/// mixed up "absolute reading" with "difference from present" when feeding
/// the same function. A `Celsius` is a reading; it cannot be compared to a
/// threshold meant for a difference, because there is no such comparison —
/// only [`std::ops::Sub`] converts it into the one type that can be.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Celsius(f64);

impl Celsius {
    /// Validating constructor: finite (any absolute reading is physically
    /// plausible somewhere in a simulated world, so magnitude is
    /// unconstrained).
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError {
                unit: "temperature",
                value,
                reason: "must be finite",
            });
        }
        Ok(Self(value))
    }
    /// The raw absolute degrees Celsius.
    pub fn get(self) -> f64 {
        self.0
    }
}

impl std::ops::Sub for Celsius {
    type Output = TempAnomaly;
    /// The key safety property: a [`TempAnomaly`] can only ever be produced
    /// by subtracting two [`Celsius`] readings. There is no other
    /// constructor that turns an absolute temperature into an anomaly, so
    /// it is impossible to accidentally hand an absolute reading to code
    /// that expects a difference from present (decision 0008).
    fn sub(self, rhs: Celsius) -> TempAnomaly {
        TempAnomaly(self.0 - rhs.0)
    }
}

/// A temperature difference relative to the world's present climate,
/// degrees Celsius (e.g. an era's reading minus the present reading at the
/// same cell). Only producible via [`Celsius`] subtraction — see that impl.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct TempAnomaly(f64);

impl TempAnomaly {
    /// Validating constructor: finite (sign and magnitude unconstrained).
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError {
                unit: "temperature anomaly",
                value,
                reason: "must be finite",
            });
        }
        Ok(Self(value))
    }
    /// The raw degrees Celsius, relative to present.
    pub fn get(self) -> f64 {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ice_volume_rejects_out_of_range_and_nonfinite() {
        assert!(IceVolume::new(-0.1).is_err());
        assert!(IceVolume::new(1.1).is_err());
        assert!(IceVolume::new(f64::NAN).is_err());
        assert_eq!(IceVolume::new(0.5).unwrap().get(), 0.5);
    }

    #[test]
    fn sea_level_change_allows_negative_but_not_nonfinite() {
        assert_eq!(SeaLevelChange::new(-120.0).unwrap().get(), -120.0);
        assert!(SeaLevelChange::new(f64::INFINITY).is_err());
    }

    #[test]
    fn celsius_rejects_nonfinite() {
        assert!(Celsius::new(f64::NAN).is_err());
        assert!(Celsius::new(f64::INFINITY).is_err());
        assert_eq!(Celsius::new(-3.5).unwrap().get(), -3.5);
    }

    #[test]
    fn temp_anomaly_rejects_nonfinite() {
        assert!(TempAnomaly::new(f64::NAN).is_err());
        assert_eq!(TempAnomaly::new(2.0).unwrap().get(), 2.0);
    }

    #[test]
    fn celsius_subtraction_yields_the_anomaly() {
        let era = Celsius::new(10.0).unwrap();
        let present = Celsius::new(14.0).unwrap();
        let anomaly = era - present;
        assert_eq!(anomaly.get(), -4.0);
    }

    #[test]
    fn temp_anomaly_orders_by_magnitude() {
        let cold = TempAnomaly::new(-5.0).unwrap();
        let warm = TempAnomaly::new(1.0).unwrap();
        assert!(cold < warm);
    }
}
