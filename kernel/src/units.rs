//! Shared physical-quantity newtypes that cross domain boundaries
//! (decision `shared-units-live-in-the-kernel`: coherent quantities crossing domain boundaries live in the
//! kernel). A domain depends on the kernel and nothing else, so a quantity
//! spoken by more than one domain has its only legal home here.

use std::cmp::Ordering;
use std::fmt;
use std::ops::Sub;

/// Why a quantity constructor refused a value.
/// type-audit: bare-ok(identifier-text: unit), bare-ok(diagnostic-value: value), bare-ok(identifier-text: reason)
#[derive(Debug, Clone, PartialEq)]
pub struct UnitError {
    /// Human name of the unit ("reference elevation").
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

/// Metres of elevation relative to the isostatic reference datum
/// (0 m = a reference-thickness crust floating at equilibrium). Planet-
/// independent: 0 m means the same physical thing on every world. This is NOT
/// height above sea level — sea level is itself a value of this type, derived
/// from the elevation field. Deep ocean floor is strongly negative; any finite
/// value of either sign is valid.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct ReferenceElevation(f64);

impl ReferenceElevation {
    /// Validating constructor: rejects non-finite values (either sign is valid).
    /// type-audit: bare-ok(constructor-edge: value), bare-ok(constructor-edge: return)
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError {
                unit: "reference elevation",
                value,
                reason: "must be finite",
            });
        }
        Ok(Self(value))
    }

    /// The raw value in metres.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(self) -> f64 {
        self.0
    }

    /// Deterministic total order via `f64::total_cmp` (no NaN ambiguity).
    pub fn total_cmp(self, other: Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }

    /// The higher of two elevations (deterministic tie-break via `total_cmp`).
    pub fn max(self, other: Self) -> Self {
        if self.total_cmp(other) == Ordering::Less {
            other
        } else {
            self
        }
    }

    /// The lower of two elevations (deterministic tie-break via `total_cmp`).
    pub fn min(self, other: Self) -> Self {
        if self.total_cmp(other) == Ordering::Greater {
            other
        } else {
            self
        }
    }
}

/// The signed metre difference between two elevations. A local intermediate
/// (lapse rate, depth shading) — a height-above-a-datum earns its own type
/// only if it crosses a pub boundary (spec "The Datum" / decision `shared-units-live-in-the-kernel`).
impl Sub for ReferenceElevation {
    type Output = f64;
    fn sub(self, rhs: Self) -> f64 {
        self.0 - rhs.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_rejects_non_finite() {
        assert!(ReferenceElevation::new(f64::NAN).is_err());
        assert!(ReferenceElevation::new(f64::INFINITY).is_err());
        assert!(ReferenceElevation::new(f64::NEG_INFINITY).is_err());
    }

    #[test]
    fn new_accepts_finite_either_sign() {
        assert_eq!(ReferenceElevation::new(-4200.0).unwrap().get(), -4200.0);
        assert_eq!(ReferenceElevation::new(8848.0).unwrap().get(), 8848.0);
        assert_eq!(ReferenceElevation::new(0.0).unwrap().get(), 0.0);
    }

    #[test]
    fn sub_yields_signed_metre_delta() {
        let peak = ReferenceElevation::new(8848.0).unwrap();
        let sea = ReferenceElevation::new(0.0).unwrap();
        assert_eq!(peak - sea, 8848.0);
        assert_eq!(sea - peak, -8848.0);
    }

    #[test]
    fn min_max_match_total_cmp() {
        let a = ReferenceElevation::new(-100.0).unwrap();
        let b = ReferenceElevation::new(100.0).unwrap();
        assert_eq!(a.max(b), b);
        assert_eq!(a.min(b), a);
        assert_eq!(a.max(a), a);
    }

    #[test]
    fn partial_ord_matches_bare_f64() {
        let a = ReferenceElevation::new(-100.0).unwrap();
        let b = ReferenceElevation::new(100.0).unwrap();
        assert!(a < b);
        assert!(b >= a);
    }
}
