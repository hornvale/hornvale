use crate::world::prelude::*;

impl From<Vector4D> for CorridorKind {
  fn from(vector: Vector4D) -> Self {
    if vector.z > 0 {
      CorridorKind::Ascend(vector.into())
    } else {
      CorridorKind::Default(vector.into())
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_from() {
    init();
    assert_eq!(
      CorridorKind::from(Vector4D::from((0, 0, 0, 0))),
      CorridorKind::Default(Region::from((0, 0, 0)))
    );
    assert_eq!(
      CorridorKind::from(Vector4D::from((0, 0, 0, 1))),
      CorridorKind::Ascend(Region::from((0, 0, 1)))
    );
  }
}
