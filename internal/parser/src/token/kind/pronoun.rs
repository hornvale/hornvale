use serde::{Deserialize, Serialize};
use strum::Display;

/// A pronoun.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Pronoun {
  /// "Him".
  Him,
  /// "Her".
  Her,
  /// "Me".
  Me,
  /// "It".
  It,
  /// "Them".
  Them,
  /// "You".
  You,
}

impl Pronoun {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
  }
}

impl TryFrom<&str> for Pronoun {
  type Error = ();

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "him" => Ok(Self::Him),
      "her" => Ok(Self::Her),
      "me" => Ok(Self::Me),
      "it" => Ok(Self::It),
      "them" => Ok(Self::Them),
      "you" => Ok(Self::You),
      _ => Err(()),
    }
  }
}
