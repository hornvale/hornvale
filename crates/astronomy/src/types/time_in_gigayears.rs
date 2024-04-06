use super::time_in_earth_years::TimeInEarthYears;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `TimeInGigayears` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct TimeInGigayears(pub f64);

impl TimeInGigayears {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<TimeInEarthYears> for TimeInGigayears {
  fn from(original: TimeInEarthYears) -> Self {
    Self(original.0 / 1_000_000_000.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(TimeInGigayears(-1.0).abs(), 1.0);
    assert_approx_eq!(TimeInGigayears(0.0).abs(), 0.0);
    assert_approx_eq!(TimeInGigayears(1.0).abs(), 1.0);
  }

  #[test]
  fn from_time_in_earth_years() {
    assert_approx_eq!(
      TimeInGigayears::from(TimeInEarthYears(1.0)),
      TimeInGigayears(1.0 / 1_000_000_000.0)
    );
  }
}
