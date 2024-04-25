use crate::world::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Traits and trait implementations.
pub mod traits;

/// A `PassageDirection` is a direction in which a passage can be built.
///
/// Note that this is a perfect match for the `Direction` enum in the `core` crate.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct PassageDirection(pub Direction);

impl PassageDirection {
  /// Get the opposite direction of the given direction.
  pub fn opposite(&self) -> Self {
    Self(-self.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_opposite() {
    init();
    assert_eq!(
      PassageDirection(Direction::Up).opposite(),
      PassageDirection(Direction::Down)
    );
  }
}
