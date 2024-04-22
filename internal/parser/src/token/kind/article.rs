use serde::{Deserialize, Serialize};
use strum::Display;

/// An article, definite or indefinite.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Article {
  /// "A/An".
  A,
  /// "The".
  The,
}

impl Article {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
  }
}

impl TryFrom<&str> for Article {
  type Error = ();

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "a" | "an" => Ok(Self::A),
      "the" => Ok(Self::The),
      _ => Err(()),
    }
  }
}
