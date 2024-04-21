use crate::prelude::*;

impl From<Vector4D> for CorridorDirection {
  fn from(value: Vector4D) -> Self {
    Self(Direction::from(value))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_from_vector4d() {
    let vector = Vector4D { w: 0, x: 0, y: 1, z: 0 };
    let corridor_direction = CorridorDirection::from(vector);
    let result = Direction::from(corridor_direction);
    assert_eq!(Direction::North, result);
  }
}
