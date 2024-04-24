use super::density_in_grams_per_cm3::DensityInGramsPerCm3;
use super::density_of_jupiter::DensityOfJupiter;
use super::density_of_luna::DensityOfLuna;
use super::density_of_sol::DensityOfSol;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `DensityOfEarth` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct DensityOfEarth(pub f64);

impl DensityOfEarth {
  /// Returns the absolute value of the density.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<DensityInGramsPerCm3> for DensityOfEarth {
  fn from(original: DensityInGramsPerCm3) -> Self {
    Self(original.0 / DENSITY_OF_EARTH.0)
  }
}

impl From<DensityOfJupiter> for DensityOfEarth {
  fn from(original: DensityOfJupiter) -> Self {
    Self(original.0 / DENSITY_OF_JUPITER.0)
  }
}

impl From<DensityOfLuna> for DensityOfEarth {
  fn from(original: DensityOfLuna) -> Self {
    Self(original.0 / DENSITY_OF_LUNA.0)
  }
}

impl From<DensityOfSol> for DensityOfEarth {
  fn from(original: DensityOfSol) -> Self {
    Self(original.0 / DENSITY_OF_SOL.0)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn from_density_in_grams_per_cm3() {
    assert_approx_eq!(
      DensityOfEarth::from(DensityInGramsPerCm3(1.0)),
      DensityOfEarth(1.0 / DENSITY_OF_EARTH.0)
    );
  }

  #[test]
  fn from_density_of_jupiter() {
    assert_approx_eq!(
      DensityOfEarth::from(DensityOfJupiter(1.0)),
      DensityOfEarth(1.0 / DENSITY_OF_JUPITER.0)
    );
  }

  #[test]
  fn from_density_of_luna() {
    assert_approx_eq!(
      DensityOfEarth::from(DensityOfLuna(1.0)),
      DensityOfEarth(1.0 / DENSITY_OF_LUNA.0)
    );
  }

  #[test]
  fn from_density_of_sol() {
    assert_approx_eq!(
      DensityOfEarth::from(DensityOfSol(1.0)),
      DensityOfEarth(1.0 / DENSITY_OF_SOL.0)
    );
  }
}
