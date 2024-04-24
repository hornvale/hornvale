use super::gravity_of_earth::GravityOfEarth;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `GravityOfLuna` newtype, representing gravitational acceleration on the Moon.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct GravityOfLuna(pub f64);

impl GravityOfLuna {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<GravityOfLuna> for GravityOfEarth {
  fn from(original: GravityOfLuna) -> Self {
    GravityOfEarth(original.0 * GRAVITATIONAL_ACCELERATION_LUNA.0 / GRAVITATIONAL_ACCELERATION_EARTH.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_eq!(GravityOfLuna(-1.0).abs(), 1.0);
    assert_eq!(GravityOfLuna(0.0).abs(), 0.0);
    assert_eq!(GravityOfLuna(1.0).abs(), 1.0);
  }

  #[test]
  fn test_from_gravity_of_luna_to_gravity_of_earth() {
    assert_approx_eq!(GravityOfEarth::from(GravityOfLuna(0.0)), GravityOfEarth(0.0));
    assert_approx_eq!(
      GravityOfEarth::from(GravityOfLuna(1.0)),
      GravityOfEarth(1.0 / 6.0),
      1e-1
    );
    assert_approx_eq!(
      GravityOfEarth::from(GravityOfLuna(2.0)),
      GravityOfEarth(2.0 / 6.0),
      1e-1
    );
  }
}
