use super::escape_velocity_of_earth::EscapeVelocityOfEarth;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `SpeedInKmPerSec` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct SpeedInKmPerSec(pub f64);

impl SpeedInKmPerSec {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<EscapeVelocityOfEarth> for SpeedInKmPerSec {
  fn from(original: EscapeVelocityOfEarth) -> Self {
    Self(ESCAPE_VELOCITY_EARTH.0 * original.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(SpeedInKmPerSec(-1.0).abs(), 1.0);
    assert_approx_eq!(SpeedInKmPerSec(0.0).abs(), 0.0);
    assert_approx_eq!(SpeedInKmPerSec(1.0).abs(), 1.0);
  }

  #[test]
  fn from_escape_velocity_of_earth() {
    assert_approx_eq!(
      SpeedInKmPerSec::from(EscapeVelocityOfEarth(1.0)),
      SpeedInKmPerSec(ESCAPE_VELOCITY_EARTH.0)
    );
  }
}
