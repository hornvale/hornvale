use super::density_in_grams_per_cm3::DensityInGramsPerCm3;
use super::density_of_earth::DensityOfEarth;
use super::density_of_jupiter::DensityOfJupiter;
use super::density_of_luna::DensityOfLuna;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `DensityOfSol` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct DensityOfSol(pub f64);

impl DensityOfSol {
  /// Returns the absolute value of the density.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<DensityInGramsPerCm3> for DensityOfSol {
  fn from(original: DensityInGramsPerCm3) -> Self {
    Self(original.0 / DENSITY_OF_SOL.0)
  }
}

impl From<DensityOfEarth> for DensityOfSol {
  fn from(original: DensityOfEarth) -> Self {
    Self(original.0 * DENSITY_OF_EARTH.0 / DENSITY_OF_SOL.0)
  }
}

impl From<DensityOfJupiter> for DensityOfSol {
  fn from(original: DensityOfJupiter) -> Self {
    Self(original.0 * DENSITY_OF_JUPITER.0 / DENSITY_OF_SOL.0)
  }
}

impl From<DensityOfLuna> for DensityOfSol {
  fn from(original: DensityOfLuna) -> Self {
    Self(original.0 * DENSITY_OF_LUNA.0 / DENSITY_OF_SOL.0)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn from_density_in_grams_per_cm3() {
    assert_approx_eq!(
      DensityOfSol::from(DensityInGramsPerCm3(1.0)),
      DensityOfSol(1.0 / DENSITY_OF_SOL.0)
    );
  }

  #[test]
  fn from_density_of_earth() {
    assert_approx_eq!(
      DensityOfSol::from(DensityOfEarth(1.0)),
      DensityOfSol(DENSITY_OF_EARTH.0 / DENSITY_OF_SOL.0)
    );
  }

  #[test]
  fn from_density_of_jupiter() {
    assert_approx_eq!(
      DensityOfSol::from(DensityOfJupiter(1.0)),
      DensityOfSol(DENSITY_OF_JUPITER.0 / DENSITY_OF_SOL.0)
    );
  }

  #[test]
  fn from_density_of_luna() {
    assert_approx_eq!(
      DensityOfSol::from(DensityOfLuna(1.0)),
      DensityOfSol(DENSITY_OF_LUNA.0 / DENSITY_OF_SOL.0)
    );
  }
}
