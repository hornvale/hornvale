use super::time_in_earth_days::TimeInEarthDays;
use super::time_in_earth_years::TimeInEarthYears;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `TimeInEarthHours` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct TimeInEarthHours(pub f64);

impl TimeInEarthHours {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<TimeInEarthDays> for TimeInEarthHours {
  fn from(original: TimeInEarthDays) -> Self {
    Self(original.0 * EARTH_HOURS_PER_DAY.0)
  }
}

impl From<TimeInEarthYears> for TimeInEarthHours {
  fn from(original: TimeInEarthYears) -> Self {
    Self(original.0 * EARTH_HOURS_PER_DAY.0 * EARTH_DAYS_PER_YEAR.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn from_time_in_earth_days() {
    assert_approx_eq!(
      TimeInEarthHours::from(TimeInEarthDays(1.0)),
      TimeInEarthHours(1.0 * EARTH_HOURS_PER_DAY.0)
    );
  }

  #[test]
  fn from_time_in_earth_years() {
    assert_approx_eq!(
      TimeInEarthHours::from(TimeInEarthYears(1.0)),
      TimeInEarthHours(1.0 * EARTH_HOURS_PER_DAY.0 * EARTH_DAYS_PER_YEAR.0)
    );
  }
}
