use crate::database::prelude::*;
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
  pub fn is_met(&self, _database: &Database) -> bool {
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
    let database = Database::default();
    assert!(PassageCondition::Always.is_met(&database));
    assert!(!PassageCondition::Never.is_met(&database));
  }
}
