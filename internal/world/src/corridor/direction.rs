use super::kind::CorridorKind;
use crate::prelude::Point;
use serde::{Deserialize, Serialize};
use std::ops::Neg;
use strum::{Display, EnumIter};

/// A `CorridorDirection` is a direction in which a corridor can be built.
#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum CorridorDirection {
  /// The `North` direction (y+1).
  North,
  /// The `South` direction (y-1).
  South,
  /// The `East` direction (x+1).
  East,
  /// The `West` direction (x-1).
  West,
  /// The `Up` direction (z+1).
  Up,
  /// The `Down` direction (z-1).
  Down,
}

impl CorridorDirection {
  /// Transform the given direction into a point.
  pub fn to_point(&self) -> Point {
    (*self).into()
  }

  /// Get the opposite direction of the given direction.
  pub fn opposite(&self) -> Self {
    self.to_point().neg().try_into().expect("Invalid corridor direction")
  }

  /// Build a corridor in the given direction from the given point.
  pub fn get_corridor(&self, point: Point) -> Option<CorridorKind> {
    use CorridorDirection::*;
    use CorridorKind::*;
    let destination = self.to_point() + point;
    match self {
      Up if point.z >= 0 => Some(Ascend(destination)),
      _ => Some(Default(destination)),
    }
  }
}

impl Neg for CorridorDirection {
  type Output = Self;

  fn neg(self) -> Self::Output {
    self.opposite()
  }
}

impl TryFrom<Point> for CorridorDirection {
  type Error = ();

  fn try_from(point: Point) -> Result<Self, Self::Error> {
    match (point.x, point.y, point.z) {
      (0, y, 0) if y > 0 => Ok(CorridorDirection::North),
      (0, y, 0) if y < 0 => Ok(CorridorDirection::South),
      (x, 0, 0) if x > 0 => Ok(CorridorDirection::East),
      (x, 0, 0) if x < 0 => Ok(CorridorDirection::West),
      (0, 0, z) if z > 0 => Ok(CorridorDirection::Up),
      (0, 0, z) if z < 0 => Ok(CorridorDirection::Down),
      _ => Err(()),
    }
  }
}

impl TryFrom<(Point, Point)> for CorridorDirection {
  type Error = ();

  fn try_from((from, to): (Point, Point)) -> Result<Self, Self::Error> {
    let point: Point = (to.x - from.x, to.y - from.y, to.z - from.z).into();
    CorridorDirection::try_from(point)
  }
}
