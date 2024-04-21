use crate::prelude::*;

impl From<Vector4D> for Direction {
  fn from(vector: Vector4D) -> Self {
    match vector {
      Vector4D { w: 0, x: 0, y: 1, z: 0 } => Direction::North,
      Vector4D { w: 0, x: 1, y: 1, z: 0 } => Direction::Northeast,
      Vector4D { w: 0, x: 1, y: 0, z: 0 } => Direction::East,
      Vector4D {
        w: 0,
        x: 1,
        y: -1,
        z: 0,
      } => Direction::Southeast,
      Vector4D {
        w: 0,
        x: 0,
        y: -1,
        z: 0,
      } => Direction::South,
      Vector4D {
        w: 0,
        x: -1,
        y: -1,
        z: 0,
      } => Direction::Southwest,
      Vector4D {
        w: 0,
        x: -1,
        y: 0,
        z: 0,
      } => Direction::West,
      Vector4D {
        w: 0,
        x: -1,
        y: 1,
        z: 0,
      } => Direction::Northwest,
      Vector4D { w: 0, x: 0, y: 0, z: 1 } => Direction::Up,
      Vector4D {
        w: 0,
        x: 0,
        y: 0,
        z: -1,
      } => Direction::Down,
      Vector4D { w: 1, x: 0, y: 0, z: 0 } => Direction::In,
      Vector4D {
        w: -1,
        x: 0,
        y: 0,
        z: 0,
      } => Direction::Out,
      _ => panic!("Invalid vector: {:?}", vector),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_from_vector4d() {
    let vector = Vector4D { w: 0, x: 0, y: 1, z: 0 };
    let result = Direction::from(vector);
    assert_eq!(Direction::North, result);
  }
}
