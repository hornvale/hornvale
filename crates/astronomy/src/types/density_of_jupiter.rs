use super::density_in_grams_per_cm3::DensityInGramsPerCm3;
use super::density_of_earth::DensityOfEarth;
use super::density_of_luna::DensityOfLuna;
use super::density_of_sol::DensityOfSol;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `DensityOfJupiter` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct DensityOfJupiter(pub f64);

impl DensityOfJupiter {
  /// Returns the absolute value of the density.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<DensityInGramsPerCm3> for DensityOfJupiter {
  fn from(original: DensityInGramsPerCm3) -> Self {
    Self(original.0 / DENSITY_OF_JUPITER.0)
  }
}

impl From<DensityOfEarth> for DensityOfJupiter {
  fn from(original: DensityOfEarth) -> Self {
    Self(original.0 * DENSITY_OF_EARTH.0 / DENSITY_OF_JUPITER.0)
  }
}

impl From<DensityOfLuna> for DensityOfJupiter {
  fn from(original: DensityOfLuna) -> Self {
    Self(original.0 * DENSITY_OF_LUNA.0 / DENSITY_OF_JUPITER.0)
  }
}

impl From<DensityOfSol> for DensityOfJupiter {
  fn from(original: DensityOfSol) -> Self {
    Self(original.0 * DENSITY_OF_SOL.0 / DENSITY_OF_JUPITER.0)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn from_density_in_grams_per_cm3() {
    assert_approx_eq!(
      DensityOfJupiter::from(DensityInGramsPerCm3(1.0)),
      DensityOfJupiter(1.0 / DENSITY_OF_JUPITER.0)
    );
  }

  #[test]
  fn from_density_of_earth() {
    assert_approx_eq!(
      DensityOfJupiter::from(DensityOfEarth(1.0)),
      DensityOfJupiter(1.0 * DENSITY_OF_EARTH.0 / DENSITY_OF_JUPITER.0)
    );
  }

  #[test]
  fn from_density_of_luna() {
    assert_approx_eq!(
      DensityOfJupiter::from(DensityOfLuna(1.0)),
      DensityOfJupiter(1.0 * DENSITY_OF_LUNA.0 / DENSITY_OF_JUPITER.0)
    );
  }

  #[test]
  fn from_density_of_sol() {
    assert_approx_eq!(
      DensityOfJupiter::from(DensityOfSol(1.0)),
      DensityOfJupiter(1.0 * DENSITY_OF_SOL.0 / DENSITY_OF_JUPITER.0)
    );
  }
}
