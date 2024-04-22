use crate::prelude::*;

impl From<(i64, i64, i64, i64)> for Room {
  fn from((w, x, y, z): (i64, i64, i64, i64)) -> Self {
    Room { w, x, y, z }
  }
}

impl From<(i64, i64, i64)> for Room {
  fn from((x, y, z): (i64, i64, i64)) -> Self {
    Room { w: 0, x, y, z }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_from_i64_tuple() {
    init();
    let room = Room::from((1, 2, 3, 4));
    assert_eq!(room, Room { w: 1, x: 2, y: 3, z: 4 });
  }

  #[test]
  fn test_from_i64_triple() {
    init();
    let room = Room::from((2, 3, 4));
    assert_eq!(room, Room { w: 0, x: 2, y: 3, z: 4 });
  }
}
