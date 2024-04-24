use super::length_in_au::LengthInAu;
use super::length_in_km::LengthInKm;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `LengthInLyr` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct LengthInLyr(pub f64);

impl LengthInLyr {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<LengthInKm> for LengthInLyr {
  fn from(original: LengthInKm) -> Self {
    Self(original.0 / KM_PER_LYR.0)
  }
}

impl From<LengthInAu> for LengthInLyr {
  fn from(original: LengthInAu) -> Self {
    Self(original.0 / AU_PER_LYR.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(LengthInLyr(-1.0).abs(), 1.0);
    assert_approx_eq!(LengthInLyr(0.0).abs(), 0.0);
    assert_approx_eq!(LengthInLyr(1.0).abs(), 1.0);
  }

  #[test]
  fn from_length_in_km() {
    assert_approx_eq!(LengthInLyr::from(LengthInKm(1.0)), LengthInLyr(1.0 / KM_PER_LYR.0));
  }

  #[test]
  fn from_length_in_au() {
    assert_approx_eq!(LengthInLyr::from(LengthInAu(1.0)), LengthInLyr(1.0 / AU_PER_LYR.0));
  }
}
