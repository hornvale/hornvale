use super::gravity_of_luna::GravityOfLuna;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `GravityOfEarth` newtype, representing gravitational acceleration on Earth.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct GravityOfEarth(pub f64);

impl GravityOfEarth {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<GravityOfEarth> for GravityOfLuna {
  fn from(original: GravityOfEarth) -> Self {
    GravityOfLuna(original.0 * GRAVITATIONAL_ACCELERATION_EARTH.0 / GRAVITATIONAL_ACCELERATION_LUNA.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(GravityOfEarth(-1.0).abs(), 1.0);
    assert_approx_eq!(GravityOfEarth(0.0).abs(), 0.0);
    assert_approx_eq!(GravityOfEarth(1.0).abs(), 1.0);
  }

  #[test]
  fn test_from_gravity_of_earth_to_gravity_of_luna() {
    assert_approx_eq!(GravityOfLuna::from(GravityOfEarth(0.0)), GravityOfLuna(0.0), 1e-3);
    assert_approx_eq!(GravityOfLuna::from(GravityOfEarth(1.0)), GravityOfLuna(6.0), 1e-1);
    assert_approx_eq!(GravityOfLuna::from(GravityOfEarth(2.0)), GravityOfLuna(12.0), 1e-1);
  }
}
