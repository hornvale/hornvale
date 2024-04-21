use crate::prelude::*;

impl From<(Region, Region)> for Vector4D {
  fn from((from, to): (Region, Region)) -> Self {
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
  fn test_from_regions() {
    init();
    assert_eq!(
      Vector4D::from((Region { w: 0, x: 0, y: 0, z: 0 }, Region { w: 0, x: 0, y: 0, z: 0 })),
      Vector4D { w: 0, x: 0, y: 0, z: 0 }
    );
    assert_eq!(
      Vector4D::from((Region { w: 0, x: 0, y: 0, z: 0 }, Region { w: 0, x: 1, y: 0, z: 0 })),
      Vector4D { w: 0, x: 1, y: 0, z: 0 }
    );
    assert_eq!(
      Vector4D::from((Region { w: 0, x: 0, y: 0, z: 0 }, Region { w: 0, x: 0, y: 1, z: 0 })),
      Vector4D { w: 0, x: 0, y: 1, z: 0 }
    );
    assert_eq!(
      Vector4D::from((Region { w: 0, x: 0, y: 0, z: 0 }, Region { w: 0, x: 0, y: 0, z: 1 })),
      Vector4D { w: 0, x: 0, y: 0, z: 1 }
    );
    assert_eq!(
      Vector4D::from((Region { w: 0, x: 0, y: 0, z: 0 }, Region { w: 1, x: 0, y: 0, z: 0 })),
      Vector4D { w: 1, x: 0, y: 0, z: 0 }
    );
    assert_eq!(
      Vector4D::from((Region { w: 0, x: 0, y: 0, z: 0 }, Region { w: 0, x: 0, y: 0, z: 0 })),
      Vector4D { w: 0, x: 0, y: 0, z: 0 }
    );
  }
}
