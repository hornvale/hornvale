use super::direction::CorridorDirection;
use super::kind::CorridorKind;
use crate::prelude::*;

/// A `CorridorFinder` is an algorithm to find corridors between regions.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CorridorFinder {
  /// A very densely packed pattern; every region is connected in every way.
  #[default]
  Simple,
}

impl CorridorFinder {
  /// Get the corridor at the given region in the given direction.
  pub fn get_corridor(&self, region: Region, direction: CorridorDirection) -> Option<CorridorKind> {
    match self {
      CorridorFinder::Simple => Some((Vector4D::from(direction) + Vector4D::from(region)).into()),
    }
  }

  /// Determine whether a corridor exists at the given region in the given direction.
  pub fn has_corridor(&self, region: Region, direction: CorridorDirection) -> bool {
    match self {
      CorridorFinder::Simple => self.get_corridor(region, direction).is_some(),
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
          for w in -1..=1 {
            for direction in Direction::iter().map(CorridorDirection::from) {
              assert!(corridor_finder.has_corridor(Region { w, x, y, z }, direction));
            }
          }
        }
      }
    }
  }

  #[test]
  fn test_simple_get_corridor() {
    init();
    let corridor_finder = CorridorFinder::Simple;
    for z in -1..=1 {
      for y in -1..=1 {
        for x in -1..=1 {
          for w in -1..=1 {
            for direction in Direction::iter().map(CorridorDirection::from) {
              assert!(corridor_finder.get_corridor(Region { w, x, y, z }, direction).is_some());
            }
          }
        }
      }
    }
  }
}