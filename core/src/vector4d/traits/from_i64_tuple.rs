use crate::prelude::*;

impl TryFrom<(i64, i64, i64, i64)> for Vector4D {
  type Error = ();

  fn try_from(tuple: (i64, i64, i64, i64)) -> Result<Self, Self::Error> {
    let (w, x, y, z) = tuple;
    Ok(Self { w, x, y, z })
  }
}
