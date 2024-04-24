use crate::core::prelude::*;

impl TryFrom<Vector4D> for PassageDirection {
  type Error = ();

  fn try_from(vector: Vector4D) -> Result<Self, Self::Error> {
    Ok(Self(Direction::try_from(vector)?))
  }
}
