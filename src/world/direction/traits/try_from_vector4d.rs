use crate::world::prelude::*;

impl TryFrom<Vector4D> for Direction {
  type Error = ();

  fn try_from(vector: Vector4D) -> Result<Self, Self::Error> {
    match vector {
      Vector4D { w: 0, x: 0, y: 1, z: 0 } => Ok(Direction::North),
      Vector4D { w: 0, x: 1, y: 1, z: 0 } => Ok(Direction::Northeast),
      Vector4D { w: 0, x: 1, y: 0, z: 0 } => Ok(Direction::East),
      Vector4D {
        w: 0,
        x: 1,
        y: -1,
        z: 0,
      } => Ok(Direction::Southeast),
      Vector4D {
        w: 0,
        x: 0,
        y: -1,
        z: 0,
      } => Ok(Direction::South),
      Vector4D {
        w: 0,
        x: -1,
        y: -1,
        z: 0,
      } => Ok(Direction::Southwest),
      Vector4D {
        w: 0,
        x: -1,
        y: 0,
        z: 0,
      } => Ok(Direction::West),
      Vector4D {
        w: 0,
        x: -1,
        y: 1,
        z: 0,
      } => Ok(Direction::Northwest),
      Vector4D { w: 0, x: 0, y: 0, z: 1 } => Ok(Direction::Up),
      Vector4D {
        w: 0,
        x: 0,
        y: 0,
        z: -1,
      } => Ok(Direction::Down),
      Vector4D { w: 1, x: 0, y: 0, z: 0 } => Ok(Direction::In),
      Vector4D {
        w: -1,
        x: 0,
        y: 0,
        z: 0,
      } => Ok(Direction::Out),
      _ => Err(()),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_try_from() {
    init();
    assert_eq!(Direction::try_from(Vector4D::from((0, 0, 1, 0))), Ok(Direction::North));
    assert_eq!(
      Direction::try_from(Vector4D::from((0, 1, 1, 0))),
      Ok(Direction::Northeast)
    );
    assert_eq!(Direction::try_from(Vector4D::from((0, 1, 0, 0))), Ok(Direction::East));
    assert_eq!(
      Direction::try_from(Vector4D::from((0, 1, -1, 0))),
      Ok(Direction::Southeast)
    );
    assert_eq!(Direction::try_from(Vector4D::from((0, 0, -1, 0))), Ok(Direction::South));
    assert_eq!(
      Direction::try_from(Vector4D::from((0, -1, -1, 0))),
      Ok(Direction::Southwest)
    );
    assert_eq!(Direction::try_from(Vector4D::from((0, -1, 0, 0))), Ok(Direction::West));
    assert_eq!(
      Direction::try_from(Vector4D::from((0, -1, 1, 0))),
      Ok(Direction::Northwest)
    );
    assert_eq!(Direction::try_from(Vector4D::from((0, 0, 0, 1))), Ok(Direction::Up));
    assert_eq!(Direction::try_from(Vector4D::from((0, 0, 0, -1))), Ok(Direction::Down));
    assert_eq!(Direction::try_from(Vector4D::from((1, 0, 0, 0))), Ok(Direction::In));
    assert_eq!(Direction::try_from(Vector4D::from((-1, 0, 0, 0))), Ok(Direction::Out));
    assert_eq!(Direction::try_from(Vector4D::from((0, 0, 0, 0))), Err(()));
  }
}
