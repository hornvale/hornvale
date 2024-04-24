use super::length_in_km::LengthInKm;
use super::length_in_lyr::LengthInLyr;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `LengthInAu` newtype, representing a length in astronomical units.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct LengthInAu(pub f64);

impl LengthInAu {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<LengthInKm> for LengthInAu {
  fn from(original: LengthInKm) -> Self {
    Self(original.0 / KM_PER_AU.0)
  }
}

impl From<LengthInLyr> for LengthInAu {
  fn from(original: LengthInLyr) -> Self {
    Self(original.0 * AU_PER_LYR.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(LengthInAu(-1.0).abs(), 1.0);
    assert_approx_eq!(LengthInAu(0.0).abs(), 0.0);
    assert_approx_eq!(LengthInAu(1.0).abs(), 1.0);
  }

  #[test]
  fn from_length_in_km() {
    assert_approx_eq!(LengthInAu::from(LengthInKm(1.0)), LengthInAu(1.0 / KM_PER_AU.0));
  }

  #[test]
  fn from_length_in_lyr() {
    assert_approx_eq!(LengthInAu::from(LengthInLyr(1.0)), LengthInAu(1.0 * AU_PER_LYR.0));
  }
}
