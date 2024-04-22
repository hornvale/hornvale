use serde::{Deserialize, Serialize};
use strum::Display;

/// A conjunction.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Conjunction {
  /// "And".
  And,
  /// "But".
  But,
  /// "Or".
  Or,
}

impl Conjunction {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
  }
}

impl TryFrom<&str> for Conjunction {
  type Error = ();

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "and" => Ok(Self::And),
      "but" => Ok(Self::But),
      "or" => Ok(Self::Or),
      _ => Err(()),
    }
  }
}
