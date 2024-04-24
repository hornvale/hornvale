use crate::core::prelude::*;

impl From<(i64, i64, i64)> for Room {
  fn from(tuple: (i64, i64, i64)) -> Self {
    let (x, y, z) = tuple;
    Room { w: 0, x, y, z }
  }
}

impl From<(i64, i64, i64, i64)> for Room {
  fn from(tuple: (i64, i64, i64, i64)) -> Self {
    let (w, x, y, z) = tuple;
    Room { w, x, y, z }
  }
}
