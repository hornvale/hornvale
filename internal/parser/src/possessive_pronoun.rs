use serde::{Deserialize, Serialize};

/// A possessive pronoun.
///
/// Possessive pronouns are pronouns that indicate possession or ownership.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum PossessivePronoun {
  /// `mine`.
  Mine,
  /// `yours`.
  Yours,
  /// `his`.
  His,
  /// `hers`.
  Hers,
  /// `its`.
  Its,
  /// `ours`.
  Ours,
  /// `theirs`.
  Theirs,
}

impl PossessivePronoun {
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for PossessivePronoun {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "mine" => Ok(Self::Mine),
      "yours" => Ok(Self::Yours),
      "his" => Ok(Self::His),
      "hers" => Ok(Self::Hers),
      "its" => Ok(Self::Its),
      "ours" => Ok(Self::Ours),
      "theirs" => Ok(Self::Theirs),
      _ => Err(()),
    }
  }
}
