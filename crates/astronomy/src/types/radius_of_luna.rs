use super::radius_of_earth::RadiusOfEarth;
use super::radius_of_jupiter::RadiusOfJupiter;
use super::radius_of_sol::RadiusOfSol;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `RadiusOfLuna` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct RadiusOfLuna(pub f64);

impl RadiusOfLuna {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<RadiusOfEarth> for RadiusOfLuna {
  fn from(original: RadiusOfEarth) -> Self {
    Self(original.0 * KM_PER_EARTH_RADIUS.0 / KM_PER_LUNA_RADIUS.0)
  }
}

impl From<RadiusOfJupiter> for RadiusOfLuna {
  fn from(original: RadiusOfJupiter) -> Self {
    Self(original.0 * KM_PER_JUPITER_RADIUS.0 / KM_PER_LUNA_RADIUS.0)
  }
}

impl From<RadiusOfSol> for RadiusOfLuna {
  fn from(original: RadiusOfSol) -> Self {
    Self(original.0 * KM_PER_SOL_RADIUS.0 / KM_PER_LUNA_RADIUS.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(RadiusOfLuna(-1.0).abs(), 1.0);
    assert_approx_eq!(RadiusOfLuna(0.0).abs(), 0.0);
    assert_approx_eq!(RadiusOfLuna(1.0).abs(), 1.0);
  }

  #[test]
  fn from_radius_of_earth() {
    assert_approx_eq!(
      RadiusOfLuna::from(RadiusOfEarth(1.0)),
      RadiusOfLuna(1.0 * KM_PER_EARTH_RADIUS.0 / KM_PER_LUNA_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_jupiter() {
    assert_approx_eq!(
      RadiusOfLuna::from(RadiusOfJupiter(1.0)),
      RadiusOfLuna(1.0 * KM_PER_JUPITER_RADIUS.0 / KM_PER_LUNA_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_sol() {
    assert_approx_eq!(
      RadiusOfLuna::from(RadiusOfSol(1.0)),
      RadiusOfLuna(1.0 * KM_PER_SOL_RADIUS.0 / KM_PER_LUNA_RADIUS.0)
    );
  }
}
