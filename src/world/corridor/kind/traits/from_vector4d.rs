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
