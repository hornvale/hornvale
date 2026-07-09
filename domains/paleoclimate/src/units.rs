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
}
