use crate::core::prelude::*;

impl From<(i64, i64, i64)> for Region {
  fn from(tuple: (i64, i64, i64)) -> Self {
    let (x, y, z) = tuple;
    Region { w: 0, x, y, z }
  }
}

impl From<(i64, i64, i64, i64)> for Region {
  fn from(tuple: (i64, i64, i64, i64)) -> Self {
    let (w, x, y, z) = tuple;
    Region { w, x, y, z }
  }
}
