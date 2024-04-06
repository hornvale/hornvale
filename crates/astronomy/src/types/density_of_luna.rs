use super::density_in_grams_per_cm3::DensityInGramsPerCm3;
use super::density_of_earth::DensityOfEarth;
use super::density_of_jupiter::DensityOfJupiter;
use super::density_of_sol::DensityOfSol;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `DensityOfLuna` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct DensityOfLuna(pub f64);

impl DensityOfLuna {
  /// Returns the absolute value of the density.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<DensityInGramsPerCm3> for DensityOfLuna {
  fn from(original: DensityInGramsPerCm3) -> Self {
    Self(original.0 / DENSITY_OF_LUNA.0)
  }
}

impl From<DensityOfEarth> for DensityOfLuna {
  fn from(original: DensityOfEarth) -> Self {
    Self(original.0 * DENSITY_OF_EARTH.0 / DENSITY_OF_LUNA.0)
  }
}

impl From<DensityOfJupiter> for DensityOfLuna {
  fn from(original: DensityOfJupiter) -> Self {
    Self(original.0 * DENSITY_OF_JUPITER.0 / DENSITY_OF_LUNA.0)
  }
}

impl From<DensityOfSol> for DensityOfLuna {
  fn from(original: DensityOfSol) -> Self {
    Self(original.0 * DENSITY_OF_SOL.0 / DENSITY_OF_LUNA.0)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn from_density_in_grams_per_cm3() {
    assert_approx_eq!(
      DensityOfLuna::from(DensityInGramsPerCm3(1.0)),
      DensityOfLuna(1.0 / DENSITY_OF_LUNA.0)
    );
  }

  #[test]
  fn from_density_of_earth() {
    assert_approx_eq!(
      DensityOfLuna::from(DensityOfEarth(1.0)),
      DensityOfLuna(1.0 * DENSITY_OF_EARTH.0 / DENSITY_OF_LUNA.0)
    );
  }

  #[test]
  fn from_density_of_jupiter() {
    assert_approx_eq!(
      DensityOfLuna::from(DensityOfJupiter(1.0)),
      DensityOfLuna(1.0 * DENSITY_OF_JUPITER.0 / DENSITY_OF_LUNA.0)
    );
  }

  #[test]
  fn from_density_of_sol() {
    assert_approx_eq!(
      DensityOfLuna::from(DensityOfSol(1.0)),
      DensityOfLuna(1.0 * DENSITY_OF_SOL.0 / DENSITY_OF_LUNA.0)
    );
  }
}
