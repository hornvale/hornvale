use crate::prelude::*;

impl From<Direction> for Vector4D {
  fn from(value: Direction) -> Self {
    match value {
      Direction::North => Vector4D { w: 0, x: 0, y: 1, z: 0 },
      Direction::Northeast => Vector4D { w: 0, x: 1, y: 1, z: 0 },
      Direction::East => Vector4D { w: 0, x: 1, y: 0, z: 0 },
      Direction::Southeast => Vector4D {
        w: 0,
        x: 1,
        y: -1,
        z: 0,
      },
      Direction::South => Vector4D {
        w: 0,
        x: 0,
        y: -1,
        z: 0,
      },
      Direction::Southwest => Vector4D {
        w: 0,
        x: -1,
        y: -1,
        z: 0,
      },
      Direction::West => Vector4D {
        w: 0,
        x: -1,
        y: 0,
        z: 0,
      },
      Direction::Northwest => Vector4D {
        w: 0,
        x: -1,
        y: 1,
        z: 0,
      },
      Direction::Up => Vector4D { w: 0, x: 0, y: 0, z: 1 },
      Direction::Down => Vector4D {
        w: 0,
        x: 0,
        y: 0,
        z: -1,
      },
      Direction::In => Vector4D { w: 1, x: 0, y: 0, z: 0 },
      Direction::Out => Vector4D {
        w: -1,
        x: 0,
        y: 0,
        z: 0,
      },
    }
  }
}
