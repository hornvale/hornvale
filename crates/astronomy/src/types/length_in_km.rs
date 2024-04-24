use super::length_in_au::LengthInAu;
use super::length_in_lyr::LengthInLyr;
use super::radius_of_earth::RadiusOfEarth;
use super::radius_of_jupiter::RadiusOfJupiter;
use super::radius_of_luna::RadiusOfLuna;
use super::radius_of_sol::RadiusOfSol;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `LengthInKm` newtype, representing length in kilometers.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct LengthInKm(pub f64);

impl LengthInKm {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<LengthInAu> for LengthInKm {
  fn from(original: LengthInAu) -> Self {
    Self(original.0 * KM_PER_AU.0)
  }
}

impl From<LengthInLyr> for LengthInKm {
  fn from(original: LengthInLyr) -> Self {
    Self(original.0 * KM_PER_LYR.0)
  }
}

impl From<RadiusOfEarth> for LengthInKm {
  fn from(original: RadiusOfEarth) -> Self {
    Self(original.0 * KM_PER_EARTH_RADIUS.0)
  }
}

impl From<RadiusOfJupiter> for LengthInKm {
  fn from(original: RadiusOfJupiter) -> Self {
    Self(original.0 * KM_PER_JUPITER_RADIUS.0)
  }
}

impl From<RadiusOfLuna> for LengthInKm {
  fn from(original: RadiusOfLuna) -> Self {
    Self(original.0 * KM_PER_LUNA_RADIUS.0)
  }
}

impl From<RadiusOfSol> for LengthInKm {
  fn from(original: RadiusOfSol) -> Self {
    Self(original.0 * KM_PER_SOL_RADIUS.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(LengthInKm(-1.0).abs(), 1.0);
    assert_approx_eq!(LengthInKm(0.0).abs(), 0.0);
    assert_approx_eq!(LengthInKm(1.0).abs(), 1.0);
  }

  #[test]
  fn from_length_in_au() {
    assert_approx_eq!(LengthInKm::from(LengthInAu(1.0)), LengthInKm(1.0 * KM_PER_AU.0));
  }

  #[test]
  fn from_length_in_lyr() {
    assert_approx_eq!(LengthInKm::from(LengthInLyr(1.0)), LengthInKm(1.0 * KM_PER_LYR.0));
  }

  #[test]
  fn from_radius_of_earth() {
    assert_approx_eq!(
      LengthInKm::from(RadiusOfEarth(1.0)),
      LengthInKm(1.0 * KM_PER_EARTH_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_jupiter() {
    assert_approx_eq!(
      LengthInKm::from(RadiusOfJupiter(1.0)),
      LengthInKm(1.0 * KM_PER_JUPITER_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_luna() {
    assert_approx_eq!(
      LengthInKm::from(RadiusOfLuna(1.0)),
      LengthInKm(1.0 * KM_PER_LUNA_RADIUS.0)
    );
  }
}
