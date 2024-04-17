use serde::{Deserialize, Serialize};

/// An adjective.
///
/// Adjectives are words that modify nouns or pronouns. They are used to provide
/// additional information about the object being described.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Adjective {
  /// `big`.
  Big,
  /// `small`.
  Small,
}

impl Adjective {
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for Adjective {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "big" => Ok(Self::Big),
      "small" => Ok(Self::Small),
      _ => Err(()),
    }
  }
}
