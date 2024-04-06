use super::time_in_earth_days::TimeInEarthDays;
use super::time_in_earth_hours::TimeInEarthHours;
use super::time_in_gigayears::TimeInGigayears;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `TimeInEarthYears` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct TimeInEarthYears(pub f64);

impl TimeInEarthYears {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<TimeInEarthHours> for TimeInEarthYears {
  fn from(original: TimeInEarthHours) -> Self {
    Self(original.0 / EARTH_HOURS_PER_DAY.0 / EARTH_DAYS_PER_YEAR.0)
  }
}

impl From<TimeInEarthDays> for TimeInEarthYears {
  fn from(original: TimeInEarthDays) -> Self {
    Self(original.0 / EARTH_DAYS_PER_YEAR.0)
  }
}

impl From<TimeInGigayears> for TimeInEarthYears {
  fn from(original: TimeInGigayears) -> Self {
    Self(original.0 * 1_000_000_000.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn from_time_in_earth_hours() {
    assert_approx_eq!(
      TimeInEarthYears::from(TimeInEarthHours(1.0)),
      TimeInEarthYears(1.0 / EARTH_HOURS_PER_DAY.0 / EARTH_DAYS_PER_YEAR.0)
    );
  }

  #[test]
  fn from_time_in_earth_days() {
    assert_approx_eq!(
      TimeInEarthYears::from(TimeInEarthDays(1.0)),
      TimeInEarthYears(1.0 / EARTH_DAYS_PER_YEAR.0)
    );
  }

  #[test]
  fn from_time_in_gigayears() {
    assert_approx_eq!(
      TimeInEarthYears::from(TimeInGigayears(1.0)),
      TimeInEarthYears(1.0 * 1_000_000_000.0)
    );
  }
}
