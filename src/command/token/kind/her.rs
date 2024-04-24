use serde::{Deserialize, Serialize};

/// Different kinds of `Her`.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Her {
  /// Unclassified.
  #[default]
  Unclassified,
  /// Pronoun.
  Pronoun,
  /// Possessive determiner.
  PossessiveDeterminer,
}
