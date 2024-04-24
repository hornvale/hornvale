use serde::{Deserialize, Serialize};

/// Different kinds of adjectives or nouns.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Word {
  /// Unclassified.
  #[default]
  Unclassified,
  /// Verb.
  Verb,
  /// Noun.
  Noun,
  /// Adjective.
  Adjective,
  /// Ambiguous.
  Ambiguous,
}
