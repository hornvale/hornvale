use super::direction::CorridorDirection;
use super::kind::CorridorKind;
use crate::prelude::Point;

/// A `CorridorFinder` is an algorithm to find corridors between regions.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CorridorFinder {
  /// A very densely packed pattern; every region is connected in every way.
  #[default]
  Simple,
}

impl CorridorFinder {
  /// Get the corridor at the given point in the given direction.
  pub fn get_corridor(&self, point: Point, direction: CorridorDirection) -> Option<CorridorKind> {
    match self {
      CorridorFinder::Simple => direction.get_corridor(point),
    }
  }

  /// Determine whether a corridor exists at the given point in the given direction.
  pub fn has_corridor(&self, point: Point, direction: CorridorDirection) -> bool {
    match self {
      CorridorFinder::Simple => self.get_corridor(point, direction).is_some(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;
  use strum::IntoEnumIterator;

  #[test]
  fn test_simple_has_corridor() {
    init();
    let corridor_finder = CorridorFinder::Simple;
    for z in -1..=1 {
      for y in -1..=1 {
        for x in -1..=1 {
          for direction in CorridorDirection::iter() {
            assert!(corridor_finder.has_corridor(Point { x, y, z }, direction));
          }
        }
      }
    }
  }
}
