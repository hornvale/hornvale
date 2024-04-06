use super::density_of_earth::DensityOfEarth;
use super::density_of_jupiter::DensityOfJupiter;
use super::density_of_luna::DensityOfLuna;
use super::density_of_sol::DensityOfSol;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `DensityInGramsPerCm3` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct DensityInGramsPerCm3(pub f64);

impl DensityInGramsPerCm3 {
  /// Returns the absolute value of the density.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<DensityOfJupiter> for DensityInGramsPerCm3 {
  fn from(original: DensityOfJupiter) -> Self {
    Self(DENSITY_OF_JUPITER.0 * original.0)
  }
}

impl From<DensityOfEarth> for DensityInGramsPerCm3 {
  fn from(original: DensityOfEarth) -> Self {
    Self(DENSITY_OF_EARTH.0 * original.0)
  }
}

impl From<DensityOfLuna> for DensityInGramsPerCm3 {
  fn from(original: DensityOfLuna) -> Self {
    Self(DENSITY_OF_LUNA.0 * original.0)
  }
}

impl From<DensityOfSol> for DensityInGramsPerCm3 {
  fn from(original: DensityOfSol) -> Self {
    Self(DENSITY_OF_SOL.0 * original.0)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn from_density_of_jupiter() {
    assert_approx_eq!(
      DensityInGramsPerCm3::from(DensityOfJupiter(1.0)),
      DensityInGramsPerCm3(DENSITY_OF_JUPITER.0)
    );
  }

  #[test]
  fn from_density_of_earth() {
    assert_approx_eq!(
      DensityInGramsPerCm3::from(DensityOfEarth(1.0)),
      DensityInGramsPerCm3(DENSITY_OF_EARTH.0)
    );
  }

  #[test]
  fn from_density_of_luna() {
    assert_approx_eq!(
      DensityInGramsPerCm3::from(DensityOfLuna(1.0)),
      DensityInGramsPerCm3(DENSITY_OF_LUNA.0)
    );
  }

  #[test]
  fn from_density_of_sol() {
    assert_approx_eq!(
      DensityInGramsPerCm3::from(DensityOfSol(1.0)),
      DensityInGramsPerCm3(DENSITY_OF_SOL.0)
    );
  }
}
