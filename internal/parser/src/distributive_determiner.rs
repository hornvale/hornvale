use serde::{Deserialize, Serialize};

/// A distributive determiner.
///
/// Distributive determiners are words that modify nouns to indicate that the
/// entities are being referred to individually or in groups.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum DistributiveDeterminer {
  /// `each`.
  Each,
  /// `every`.
  Every,
  /// `either`.
  Either,
  /// `neither`.
  Neither,
  /// `any`.
  Any,
  /// `all`.
  All,
  /// `some`.
  Some,
  /// `no`.
  No,
}

impl DistributiveDeterminer {
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for DistributiveDeterminer {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "each" => Ok(Self::Each),
      "every" => Ok(Self::Every),
      "either" => Ok(Self::Either),
      "neither" => Ok(Self::Neither),
      "any" => Ok(Self::Any),
      "all" => Ok(Self::All),
      "some" => Ok(Self::Some),
      "no" => Ok(Self::No),
      _ => Err(()),
    }
  }
}
