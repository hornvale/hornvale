//! Shared physical-quantity newtypes that cross domain boundaries
//! (decision 0044 (`shared-units-live-in-the-kernel`): coherent quantities crossing domain boundaries live in the
//! kernel). A domain depends on the kernel and nothing else, so a quantity
//! spoken by more than one domain has its only legal home here.

use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, Sub};

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
    /// type-audit: bare-ok(constructor-edge: value)
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
/// only if it crosses a pub boundary (spec "The Datum" / decision 0044 (`shared-units-live-in-the-kernel`)).
impl Sub for ReferenceElevation {
    type Output = f64;
    fn sub(self, rhs: Self) -> f64 {
        self.0 - rhs.0
    }
}

/// An absolute temperature, degrees Celsius.
///
/// Distinguished at the type level from [`TempAnomaly`] (decision 0008):
/// the two were previously both bare `CellMap<f64>`, and code has twice
/// mixed up "absolute reading" with "difference from present" when feeding
/// the same function. A `Temperature` is a reading; it cannot be compared to a
/// threshold meant for a difference, because there is no such comparison —
/// only [`std::ops::Sub`] converts it into the one type that can be.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Temperature(f64);

impl Temperature {
    /// Validating constructor: finite (any absolute reading is physically
    /// plausible somewhere in a simulated world, so magnitude is
    /// unconstrained).
    /// type-audit: bare-ok(constructor-edge: value)
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
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(self) -> f64 {
        self.0
    }

    /// This reading in kelvin (`get()` is the canonical raw degrees Celsius).
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn kelvin(self) -> f64 {
        self.get() + 273.15
    }
}

impl Sub for Temperature {
    type Output = TempAnomaly;
    /// One of three production paths that produce a [`TempAnomaly`] (the
    /// others are [`TempAnomaly::from_offset_c`] and [`Add`](std::ops::Add)
    /// for `TempAnomaly`): subtracting two [`Temperature`] readings. There
    /// is no other constructor that turns an absolute temperature into an
    /// anomaly, so it is impossible to accidentally hand an absolute
    /// reading to code that expects a difference from present (decision
    /// 0008).
    fn sub(self, rhs: Temperature) -> TempAnomaly {
        TempAnomaly(self.0 - rhs.0)
    }
}

impl Add<TempAnomaly> for Temperature {
    type Output = Temperature;
    /// Apply a computed offset (e.g. the ice sheet's albedo-cooling ΔT) to
    /// an absolute reading: `present + offset = era_temp`. The counterpart
    /// to [`Sub`](std::ops::Sub): together they are the sole production
    /// paths across the `Temperature`/`TempAnomaly` boundary (decision 0008,
    /// extended for the ice-advance model).
    fn add(self, rhs: TempAnomaly) -> Temperature {
        Temperature(self.0 + rhs.0)
    }
}

/// A temperature difference relative to the world's present climate,
/// degrees Celsius (e.g. an era's reading minus the present reading at the
/// same cell). Only producible via [`Temperature`] subtraction — see that impl.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct TempAnomaly(f64);

impl TempAnomaly {
    /// Builds a `TempAnomaly` directly from a computed ΔT, rather than from
    /// a difference of two readings. Fully `pub` since the kernel promotion
    /// (was `pub(crate)` in paleoclimate): the kernel is a shared home, so
    /// any domain may call this constructor directly. Finiteness is checked
    /// only by a `debug_assert!` — release builds accept a non-finite value
    /// without error, so callers must supply a finite offset themselves.
    /// [`Sub`](std::ops::Sub) for [`Temperature`] and [`Add`](std::ops::Add)
    /// for `TempAnomaly` are the other production paths.
    /// type-audit: bare-ok(constructor-edge: value)
    pub fn from_offset_c(value: f64) -> Self {
        debug_assert!(value.is_finite(), "temperature offset must be finite");
        Self(value)
    }

    /// The raw degrees Celsius, relative to present.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(self) -> f64 {
        self.0
    }
}

impl Add for TempAnomaly {
    type Output = TempAnomaly;
    /// Sum two temperature anomalies. The combined effect of two independent
    /// temperature deviations.
    fn add(self, rhs: TempAnomaly) -> TempAnomaly {
        TempAnomaly(self.0 + rhs.0)
    }
}

#[cfg(test)]
mod temperature_tests {
    use super::*;

    #[test]
    fn get_is_raw_celsius_and_kelvin_offsets() {
        let t = Temperature::new(25.0).unwrap();
        assert_eq!(t.get(), 25.0);
        assert_eq!(t.kelvin(), 25.0 + 273.15);
    }

    #[test]
    fn difference_of_two_temperatures_is_an_anomaly() {
        let warm = Temperature::new(20.0).unwrap();
        let cool = Temperature::new(5.0).unwrap();
        let delta: TempAnomaly = warm - cool;
        assert_eq!(delta.get(), 15.0);
    }

    #[test]
    fn temperature_plus_anomaly_round_trips() {
        let base = Temperature::new(10.0).unwrap();
        let a = TempAnomaly::from_offset_c(-4.0);
        assert_eq!((base + a).get(), 6.0);
    }

    #[test]
    fn anomalies_add() {
        let a = TempAnomaly::from_offset_c(3.0);
        let b = TempAnomaly::from_offset_c(-1.0);
        assert_eq!((a + b).get(), 2.0);
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
