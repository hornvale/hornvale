//! Typed quantities for paleoclimate (decision 0008): coherent units as
//! validating newtypes; dimensionless ratios stay bare f64.

/// Why a quantity constructor refused a value.
/// type-audit: bare-ok(identifier-text: unit), bare-ok(diagnostic-value: value), bare-ok(identifier-text: reason)
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
    /// type-audit: bare-ok(constructor-edge)
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
    /// type-audit: bare-ok(constructor-edge)
    pub fn get(self) -> f64 {
        self.0
    }
}

/// A change in eustatic sea level, metres (negative = the sea fell).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct SeaLevelChange(f64);

impl SeaLevelChange {
    /// Validating constructor: finite (sign and magnitude unconstrained).
    /// type-audit: bare-ok(constructor-edge)
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
    /// type-audit: bare-ok(constructor-edge)
    pub fn get(self) -> f64 {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{TempAnomaly, Temperature};

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
    fn temperature_rejects_nonfinite() {
        assert!(Temperature::new(f64::NAN).is_err());
        assert!(Temperature::new(f64::INFINITY).is_err());
        assert_eq!(Temperature::new(-3.5).unwrap().get(), -3.5);
    }

    #[test]
    fn addition_and_subtraction_round_trip() {
        let present = Temperature::new(14.0).unwrap();
        let era = Temperature::new(8.0).unwrap();
        let offset = era - present;
        assert_eq!((present + offset).get(), era.get());
    }

    #[test]
    fn temp_anomaly_from_offset_c_matches_raw_value() {
        let offset = TempAnomaly::from_offset_c(-3.5);
        assert_eq!(offset.get(), -3.5);
    }

    #[test]
    fn temp_anomaly_orders_by_magnitude() {
        let cold = TempAnomaly::from_offset_c(-5.0);
        let warm = TempAnomaly::from_offset_c(1.0);
        assert!(cold < warm);
    }
}
