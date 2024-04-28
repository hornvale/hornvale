use crate::geometry::*;

impl From<(i64, i64, i64, i64)> for Point4D {
  fn from(tuple: (i64, i64, i64, i64)) -> Self {
    let (w, x, y, z) = tuple;
    Self { w, x, y, z }
  }
}
