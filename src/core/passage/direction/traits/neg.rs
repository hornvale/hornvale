use crate::core::prelude::*;
use std::ops::Neg;

impl Neg for PassageDirection {
  type Output = PassageDirection;

  fn neg(self) -> Self::Output {
    Self(-self.0)
  }
}
