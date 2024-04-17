use serde::{Deserialize, Serialize};

/// A demonstrative determiner.
///
/// Demonstrative determiners are words that modify nouns to indicate which
/// entities are being referred to.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum DemonstrativeDeterminer {
  /// `this`.
  This,
  /// `that`.
  That,
  /// `these`.
  These,
  /// `those`.
  Those,
}

impl DemonstrativeDeterminer {
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for DemonstrativeDeterminer {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "this" => Ok(Self::This),
      "that" => Ok(Self::That),
      "these" => Ok(Self::These),
      "those" => Ok(Self::Those),
      _ => Err(()),
    }
  }
}
