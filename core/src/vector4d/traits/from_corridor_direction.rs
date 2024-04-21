use crate::prelude::*;

impl From<CorridorDirection> for Vector4D {
  fn from(value: CorridorDirection) -> Self {
    Self::from(value.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_from_corridor_direction() {
    init();
    let vector = Vector4D::from(CorridorDirection(Direction::North));
    assert_eq!(vector, Vector4D { w: 0, x: 0, y: 1, z: 0 });
  }
}
