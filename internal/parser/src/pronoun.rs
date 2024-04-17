use serde::{Deserialize, Serialize};

/// A pronoun.
///
/// Pronouns are words that can function by themselves as nouns that refer to
/// entities that have already been mentioned.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Pronoun {
  /// `I`.
  I,
  /// `you`.
  You,
  /// `him`.
  Him,
  /// `her`.
  Her,
  /// `it`.
  It,
  /// `them`.
  Them,
}

impl Pronoun {
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for Pronoun {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "I" | "me" => Ok(Self::I),
      "you" => Ok(Self::You),
      "him" => Ok(Self::Him),
      "her" => Ok(Self::Her),
      "it" => Ok(Self::It),
      "them" => Ok(Self::Them),
      _ => Err(()),
    }
  }
}
