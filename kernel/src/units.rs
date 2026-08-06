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

    /// This reading's height above `datum` — the named conversion from an
    /// absolute isostatic reading to a per-world [`SeaLevelHeight`], and the
    /// only one besides [`SeaLevelHeight::from_metres`].
    ///
    /// Pass the world's derived sea level as `datum`. It is a named method
    /// rather than [`Sub`](std::ops::Sub) because subtracting two elevations
    /// means different things depending on what the right-hand one *is* (see
    /// that impl); naming the conversion is how the caller says which meaning
    /// it intends, per decision 0008.
    ///
    /// ```
    /// use hornvale_kernel::ReferenceElevation;
    /// // Seed 42's sea level sits near -2936 m on the isostatic datum, so a
    /// // shoreline forest reads as a large negative number until you re-datum it.
    /// let ground = ReferenceElevation::new(-2836.0).unwrap();
    /// let sea = ReferenceElevation::new(-2936.0).unwrap();
    /// assert_eq!(ground.above(sea).get(), 100.0);
    /// ```
    pub fn above(self, datum: Self) -> SeaLevelHeight {
        SeaLevelHeight(self.0 - datum.0)
    }
}

/// Metres above this world's sea level. Signed: negative below.
///
/// Distinguished at the type level from [`ReferenceElevation`], which is an
/// absolute reading on the planet-independent isostatic datum. A
/// `SeaLevelHeight` is *per-world* — its zero is a derived value of that other
/// type — so two of these from different worlds are comparable to each other in
/// a way their `ReferenceElevation`s are not, and vice versa. Decision 0044's
/// doctrine requires an interval type to carry its datum; this type's name is
/// that datum.
///
/// Produced by subtracting two [`ReferenceElevation`]s (see
/// [`Sub`](std::ops::Sub) for that type), or via
/// [`from_metres`](Self::from_metres) for a caller with no pair to subtract.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct SeaLevelHeight(f64);

impl SeaLevelHeight {
    /// Builds a height directly from metres rather than from a difference of
    /// two readings. It exists for one reason: a caller deserializing a
    /// document has a number, not the pair of elevations it came from.
    ///
    /// **This is the hole through which the datum-confusion class returns.** A
    /// caller holding two [`ReferenceElevation`]s should subtract them instead —
    /// that path cannot be wrong about which datum it is on. Finiteness is a
    /// `debug_assert!` only, matching [`TempAnomaly::from_offset_c`].
    /// type-audit: bare-ok(constructor-edge: value)
    pub fn from_metres(value: f64) -> Self {
        debug_assert!(value.is_finite(), "sea-level height must be finite");
        Self(value)
    }

    /// The raw signed metres above sea level.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(self) -> f64 {
        self.0
    }

    /// Metres *below* sea level — the positive-downward reading, and the
    /// negation of [`get`](Self::get). This accessor exists so that a consumer
    /// wanting depth never writes the negation by hand: a stray sign is the
    /// same confusion class this type exists to remove.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn depth(self) -> f64 {
        -self.0
    }

    /// Deterministic total order via `f64::total_cmp` (no NaN ambiguity).
    pub fn total_cmp(self, other: Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}

/// The signed metre difference between two elevations, as a bare `f64`.
///
/// **Deliberately not a [`SeaLevelHeight`].** Subtracting two elevations is
/// polymorphic in *meaning*: `cell - sea_level` is a height above sea level,
/// but `cell - upwind_neighbour` is an orographic rise between two places
/// (`domains/climate`'s `moisture.rs` and `provider.rs` both do exactly that),
/// and a terrain-detail delta is neither. Only the first has anything to do
/// with a datum, so typing this operator's output as a datum-named quantity
/// would make the type system assert something false about the other two.
///
/// The named conversion [`ReferenceElevation::above`] is the sea-level path,
/// which is what decision 0008 prescribes — "validating constructors and
/// *named conversions*". The Benchmark tried retyping this operator first and
/// the compiler found the counterexamples.
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

/// Mass in kilograms, as an absolute positive quantity.
///
/// Used across domain boundaries (ecology, species, demography); all
/// domains normalize to this kernel-level type. Only non-negative values
/// are physically meaningful.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Mass(f64);

impl Mass {
    /// Validating constructor: rejects non-finite and negative values.
    /// type-audit: bare-ok(constructor-edge: value)
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError {
                unit: "mass",
                value,
                reason: "must be finite",
            });
        }
        if value < 0.0 {
            return Err(UnitError {
                unit: "mass",
                value,
                reason: "must not be negative",
            });
        }
        Ok(Self(value))
    }

    /// The raw value in kilograms.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn kilograms(self) -> f64 {
        self.0
    }

    /// The ratio of this mass to another (self / other), dimensionless.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn ratio_to(self, other: Mass) -> f64 {
        self.0 / other.0
    }
}

/// A non-negative absolute duration in Julian years (365.25 standard days).
/// The coarse biological/historical span type — distinct from astronomy's
/// `StdDays` sub-day time-point, and reachable from any domain because it
/// lives in the kernel. Used by life-history allometry (lifespan,
/// age-at-maturity, generation length).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Years(f64);

impl Years {
    /// Standard days per Julian year.
    pub const DAYS_PER_YEAR: f64 = 365.25;

    /// Validating constructor: rejects non-finite and negative values.
    /// type-audit: bare-ok(constructor-edge: value)
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError {
                unit: "years",
                value,
                reason: "must be finite",
            });
        }
        if value < 0.0 {
            return Err(UnitError {
                unit: "years",
                value,
                reason: "must not be negative",
            });
        }
        Ok(Self(value))
    }

    /// Build from a span in standard days.
    /// type-audit: bare-ok(constructor-edge: days)
    pub fn from_days(days: f64) -> Result<Self, UnitError> {
        Years::new(days / Self::DAYS_PER_YEAR)
    }

    /// The span in years.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(self) -> f64 {
        self.0
    }

    /// The span in standard days.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn days(self) -> f64 {
        self.0 * Self::DAYS_PER_YEAR
    }
}

/// Mean annual precipitation, millimetres per year, as an absolute
/// non-negative quantity. 0 mm/yr is valid (a desert); only non-negative,
/// finite values are physically meaningful.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Precipitation(f64);

impl Precipitation {
    /// Validating constructor: rejects non-finite and negative values.
    /// type-audit: bare-ok(constructor-edge: mm_per_year)
    pub fn new(mm_per_year: f64) -> Result<Self, UnitError> {
        if !mm_per_year.is_finite() {
            return Err(UnitError {
                unit: "precipitation",
                value: mm_per_year,
                reason: "must be finite",
            });
        }
        if mm_per_year < 0.0 {
            return Err(UnitError {
                unit: "precipitation",
                value: mm_per_year,
                reason: "must not be negative",
            });
        }
        Ok(Self(mm_per_year))
    }

    /// The raw value in millimetres per year.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(self) -> f64 {
        self.0
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

    #[test]
    fn mass_rejects_negative_and_reports_ratio() {
        assert!(Mass::new(-1.0).is_err());
        assert!(Mass::new(f64::NAN).is_err());
        let goblin = Mass::new(40.0).unwrap();
        let dragon = Mass::new(4000.0).unwrap();
        assert_eq!(goblin.kilograms(), 40.0);
        assert!((dragon.ratio_to(goblin) - 100.0).abs() < 1e-9);
    }

    #[test]
    fn years_construct_and_convert() {
        assert!(Years::new(-1.0).is_err());
        assert!(Years::new(f64::NAN).is_err());
        let life = Years::new(80.0).unwrap();
        assert_eq!(life.get(), 80.0);
        // 1 year == 365.25 standard days
        assert!((Years::new(1.0).unwrap().days() - 365.25).abs() < 1e-9);
        assert!((Years::from_days(365.25).unwrap().get() - 1.0).abs() < 1e-9);
    }

    #[test]
    fn precip_accepts_zero_and_positive() {
        assert_eq!(Precipitation::new(1200.0).unwrap().get(), 1200.0);
        assert_eq!(Precipitation::new(0.0).unwrap().get(), 0.0);
    }

    #[test]
    fn precip_rejects_negative_and_non_finite() {
        assert!(Precipitation::new(-1.0).is_err());
        assert!(Precipitation::new(f64::NAN).is_err());
        assert!(Precipitation::new(f64::INFINITY).is_err());
    }

    #[test]
    fn a_sea_level_height_reports_its_metres() {
        let h = SeaLevelHeight::from_metres(1200.5);
        assert!((h.get() - 1200.5).abs() < 1e-12);
    }

    #[test]
    fn depth_is_the_positive_downward_reading() {
        let below = SeaLevelHeight::from_metres(-3000.0);
        assert!(
            (below.depth() - 3000.0).abs() < 1e-12,
            "depth reads positive downward"
        );
        assert!(
            (below.depth() + below.get()).abs() < 1e-12,
            "depth is exactly -height"
        );
        let above = SeaLevelHeight::from_metres(800.0);
        assert!(above.depth() < 0.0, "above sea level, depth is negative");
    }

    #[test]
    fn heights_order_deterministically() {
        let a = SeaLevelHeight::from_metres(-10.0);
        let b = SeaLevelHeight::from_metres(10.0);
        assert_eq!(a.total_cmp(b), std::cmp::Ordering::Less);
        assert_eq!(b.total_cmp(a), std::cmp::Ordering::Greater);
        assert_eq!(a.total_cmp(a), std::cmp::Ordering::Equal);
    }

    #[test]
    fn the_named_conversion_yields_a_height_not_a_number() {
        let ground = ReferenceElevation::new(-2936.0).unwrap();
        let sea = ReferenceElevation::new(-2936.17).unwrap();
        // The binding's type is the assertion: this must be a SeaLevelHeight.
        let h: SeaLevelHeight = ground.above(sea);
        assert!((h.get() - 0.17).abs() < 1e-9);
    }

    #[test]
    fn subtraction_stays_a_bare_number_because_it_is_polymorphic() {
        // `cell - upwind_neighbour` is an orographic rise, not a height above
        // any datum, and `domains/climate` computes exactly that. Typing this
        // operator's output as a SeaLevelHeight would make it assert a datum
        // that isn't there.
        let here = ReferenceElevation::new(1200.0).unwrap();
        let upwind = ReferenceElevation::new(900.0).unwrap();
        let rise: f64 = here - upwind;
        assert_eq!(rise, 300.0);
    }

    #[test]
    fn a_sea_floor_reading_yields_a_positive_depth() {
        let sea = ReferenceElevation::new(-2936.17).unwrap();
        let floor = ReferenceElevation::new(-4000.0).unwrap();
        let h = floor.above(sea);
        assert!(h.get() < 0.0, "the sea floor is below sea level");
        assert!(h.depth() > 1000.0, "and its depth reads positive");
    }
}
