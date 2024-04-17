use serde::{Deserialize, Serialize};

/// A possessive determiner.
///
/// Possessive determiners are words that modify nouns or pronouns to indicate
/// possession or ownership.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum PossessiveDeterminer {
  /// `my`.
  My,
  /// `your`.
  Your,
  /// `his`.
  His,
  /// `her`.
  Her,
  /// `its`.
  Its,
  /// `our`.
  Our,
  /// `their`.
  Their,
}

impl PossessiveDeterminer {
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for PossessiveDeterminer {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "my" => Ok(Self::My),
      "your" => Ok(Self::Your),
      "his" => Ok(Self::His),
      "her" => Ok(Self::Her),
      "its" => Ok(Self::Its),
      "our" => Ok(Self::Our),
      "their" => Ok(Self::Their),
      _ => Err(()),
    }
  }
}
