use crate::core::prelude::*;

impl From<Direction> for Vector4D {
  fn from(direction: Direction) -> Self {
    use Direction::*;
    match direction {
      North => Vector4D { w: 0, x: 0, y: 1, z: 0 },
      Northeast => Vector4D { w: 0, x: 1, y: 1, z: 0 },
      East => Vector4D { w: 0, x: 1, y: 0, z: 0 },
      Southeast => Vector4D {
        w: 0,
        x: 1,
        y: -1,
        z: 0,
      },
      South => Vector4D {
        w: 0,
        x: 0,
        y: -1,
        z: 0,
      },
      Southwest => Vector4D {
        w: 0,
        x: -1,
        y: -1,
        z: 0,
      },
      West => Vector4D {
        w: 0,
        x: -1,
        y: 0,
        z: 0,
      },
      Northwest => Vector4D {
        w: 0,
        x: -1,
        y: 1,
        z: 0,
      },
      Up => Vector4D { w: 0, x: 0, y: 0, z: 1 },
      Down => Vector4D {
        w: 0,
        x: 0,
        y: 0,
        z: -1,
      },
      In => Vector4D { w: 1, x: 0, y: 0, z: 0 },
      Out => Vector4D {
        w: -1,
        x: 0,
        y: 0,
        z: 0,
      },
    }
  }
}
