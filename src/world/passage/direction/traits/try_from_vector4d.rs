use crate::world::prelude::*;

impl TryFrom<Vector4D> for PassageDirection {
  type Error = ();

  fn try_from(vector: Vector4D) -> Result<Self, Self::Error> {
    Ok(Self(Direction::try_from(vector)?))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_try_from() {
    init();
    assert_eq!(
      PassageDirection::try_from(Vector4D::from((0, 0, 0, 1))),
      Ok(PassageDirection(Direction::Up))
    );
  }
}
