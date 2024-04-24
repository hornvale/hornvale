use crate::core::prelude::*;
use std::ops::Neg;

impl Neg for CorridorDirection {
  type Output = Self;

  fn neg(self) -> Self::Output {
    Self(-self.0)
  }
}
