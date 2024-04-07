use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `AccelerationInMetersPerSec2` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct AccelerationInMetersPerSec2(pub f64);

impl AccelerationInMetersPerSec2 {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

#[cfg(test)]
mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_acceleration_in_meters_per_sec2() {
    init();
    let actual = AccelerationInMetersPerSec2(1.0);
    assert_eq!(actual.0, 1.0);
  }
}
