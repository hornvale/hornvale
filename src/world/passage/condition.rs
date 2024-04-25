use hecs::World;
use serde::{Deserialize, Serialize};

/// A condition that must be met to traverse a passage.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum PassageCondition {
  /// The passage is always traversable.
  Always,
  /// The passage is never traversable.
  Never,
}

impl PassageCondition {
  /// Check if the condition is met.
  pub fn is_met(&self, _world: &World) -> bool {
    use PassageCondition::*;
    match self {
      Always => true,
      Never => false,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_is_met() {
    init();
    let world = World::new();
    assert!(PassageCondition::Always.is_met(&world));
    assert!(!PassageCondition::Never.is_met(&world));
  }
}
