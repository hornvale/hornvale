use serde::{Deserialize, Serialize};
use strum::Display;

/// An affirmative or negative boolean value.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Boolean {
  /// "False".
  False,
  /// "True".
  True,
}

impl Boolean {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
  }

  /// Get the boolean value of this token.
  pub fn as_bool(&self) -> bool {
    matches!(self, Self::True)
  }
}

impl TryFrom<&str> for Boolean {
  type Error = ();

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "false" | "f" => Ok(Self::False),
      "true" | "t" => Ok(Self::True),
      _ => Err(()),
    }
  }
}
