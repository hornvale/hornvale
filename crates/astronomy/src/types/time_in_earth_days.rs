use super::time_in_earth_hours::TimeInEarthHours;
use super::time_in_earth_years::TimeInEarthYears;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `TimeInEarthDays` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct TimeInEarthDays(pub f64);

impl TimeInEarthDays {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<TimeInEarthHours> for TimeInEarthDays {
  fn from(original: TimeInEarthHours) -> Self {
    Self(original.0 / EARTH_HOURS_PER_DAY.0)
  }
}

impl From<TimeInEarthYears> for TimeInEarthDays {
  fn from(original: TimeInEarthYears) -> Self {
    Self(original.0 * EARTH_DAYS_PER_YEAR.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn from_time_in_earth_hours() {
    assert_approx_eq!(
      TimeInEarthDays::from(TimeInEarthHours(1.0)),
      TimeInEarthDays(1.0 / EARTH_HOURS_PER_DAY.0)
    );
  }

  #[test]
  fn from_time_in_earth_years() {
    assert_approx_eq!(
      TimeInEarthDays::from(TimeInEarthYears(1.0)),
      TimeInEarthDays(1.0 * EARTH_DAYS_PER_YEAR.0)
    );
  }
}
