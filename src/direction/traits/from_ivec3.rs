use crate::direction::prelude::Direction;
use bevy::prelude::*;

impl From<IVec3> for Direction {
  fn from(vec3: IVec3) -> Self {
    match vec3 {
      IVec3 { x: 0, y: 1, z: 0 } => Direction::North,
      IVec3 { x: 1, y: 1, z: 0 } => Direction::Northeast,
      IVec3 { x: 1, y: 0, z: 0 } => Direction::East,
      IVec3 { x: 1, y: -1, z: 0 } => Direction::Southeast,
      IVec3 { x: 0, y: -1, z: 0 } => Direction::South,
      IVec3 { x: -1, y: -1, z: 0 } => Direction::Southwest,
      IVec3 { x: -1, y: 0, z: 0 } => Direction::West,
      IVec3 { x: -1, y: 1, z: 0 } => Direction::Northwest,
      IVec3 { x: 0, y: 0, z: 1 } => Direction::Up,
      IVec3 { x: 0, y: 0, z: -1 } => Direction::Down,
      _ => Direction::None,
    }
  }
}
