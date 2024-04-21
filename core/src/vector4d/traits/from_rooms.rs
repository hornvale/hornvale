use crate::prelude::*;

impl From<(Room, Room)> for Vector4D {
  fn from((from, to): (Room, Room)) -> Self {
    Vector4D {
      w: to.w - from.w,
      x: to.x - from.x,
      y: to.y - from.y,
      z: to.z - from.z,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_from_rooms() {
    init();
    assert_eq!(
      Vector4D::from((Room { w: 0, x: 0, y: 0, z: 0 }, Room { w: 0, x: 0, y: 0, z: 0 })),
      Vector4D { w: 0, x: 0, y: 0, z: 0 }
    );
    assert_eq!(
      Vector4D::from((Room { w: 0, x: 0, y: 0, z: 0 }, Room { w: 0, x: 1, y: 0, z: 0 })),
      Vector4D { w: 0, x: 1, y: 0, z: 0 }
    );
    assert_eq!(
      Vector4D::from((Room { w: 0, x: 0, y: 0, z: 0 }, Room { w: 0, x: 0, y: 1, z: 0 })),
      Vector4D { w: 0, x: 0, y: 1, z: 0 }
    );
    assert_eq!(
      Vector4D::from((Room { w: 0, x: 0, y: 0, z: 0 }, Room { w: 0, x: 0, y: 0, z: 1 })),
      Vector4D { w: 0, x: 0, y: 0, z: 1 }
    );
    assert_eq!(
      Vector4D::from((Room { w: 0, x: 0, y: 0, z: 0 }, Room { w: 1, x: 0, y: 0, z: 0 })),
      Vector4D { w: 1, x: 0, y: 0, z: 0 }
    );
    assert_eq!(
      Vector4D::from((Room { w: 0, x: 0, y: 0, z: 0 }, Room { w: 0, x: 0, y: 0, z: 0 })),
      Vector4D { w: 0, x: 0, y: 0, z: 0 }
    );
  }
}
