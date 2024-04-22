use crate::prelude::*;

impl From<(i64, i64, i64)> for Region {
  fn from((x, y, z): (i64, i64, i64)) -> Self {
    Region { x, y, z }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_from_i64_triple() {
    init();
    let region = Region::from((1, 2, 3));
    assert_eq!(region, Region { x: 1, y: 2, z: 3 });
  }
}
