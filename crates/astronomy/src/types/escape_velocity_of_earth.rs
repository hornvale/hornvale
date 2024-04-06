use super::speed_in_km_per_sec::SpeedInKmPerSec;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `EscapeVelocityOfEarth` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct EscapeVelocityOfEarth(pub f64);

impl EscapeVelocityOfEarth {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<SpeedInKmPerSec> for EscapeVelocityOfEarth {
  fn from(original: SpeedInKmPerSec) -> Self {
    EscapeVelocityOfEarth(original.0 / ESCAPE_VELOCITY_EARTH.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(EscapeVelocityOfEarth(-1.0).abs(), 1.0);
    assert_approx_eq!(EscapeVelocityOfEarth(0.0).abs(), 0.0);
    assert_approx_eq!(EscapeVelocityOfEarth(1.0).abs(), 1.0);
  }

  #[test]
  fn from_escape_velocity_of_earth() {
    assert_approx_eq!(
      EscapeVelocityOfEarth::from(SpeedInKmPerSec(ESCAPE_VELOCITY_EARTH.0)),
      EscapeVelocityOfEarth(1.0)
    );
  }
}
