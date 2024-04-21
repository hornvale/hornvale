use crate::prelude::*;

impl From<i64> for Vector4D {
  fn from(value: i64) -> Self {
    Vector4D {
      w: value,
      x: value,
      y: value,
      z: value,
    }
  }
}

impl From<(i64, i64, i64)> for Vector4D {
  fn from((x, y, z): (i64, i64, i64)) -> Self {
    Vector4D { w: 0, x, y, z }
  }
}

impl From<(i64, i64, i64, i64)> for Vector4D {
  fn from((w, x, y, z): (i64, i64, i64, i64)) -> Self {
    Vector4D { w, x, y, z }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_from_i64() {
    init();
    let vector = Vector4D::from(1);
    assert_eq!(vector, Vector4D::from((1, 1, 1, 1)));
  }

  #[test]
  fn test_from_i64_triple() {
    init();
    let vector = Vector4D::from((1, 2, 3));
    assert_eq!(vector, Vector4D::from((0, 1, 2, 3)));
  }

  #[test]
  fn test_from_i64_quad() {
    init();
    let vector = Vector4D::from((1, 2, 3, 4));
    assert_eq!(vector, Vector4D::from((1, 2, 3, 4)));
  }
}
