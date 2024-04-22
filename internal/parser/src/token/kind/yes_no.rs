use serde::{Deserialize, Serialize};
use strum::Display;

/// An affirmative or negative grammatical particle.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum YesNo {
  /// "No".
  No,
  /// "Yes".
  Yes,
}

impl YesNo {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
  }

  /// Get the boolean value of this token.
  pub fn as_bool(&self) -> bool {
    matches!(self, Self::Yes)
  }
}

impl TryFrom<&str> for YesNo {
  type Error = ();

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "no" | "n" => Ok(Self::No),
      "yes" | "y" => Ok(Self::Yes),
      _ => Err(()),
    }
  }
}
