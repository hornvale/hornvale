use serde::{Deserialize, Serialize};
use strum::Display;

/// Adverbs.
pub mod adverb;
use adverb::Adverb;
/// Prepositions.
pub mod preposition;
use preposition::Preposition;

/// A modifier that can be applied to a verb.
///
/// This is generally either an adverb or a preposition.
#[derive(Clone, Copy, Debug, Default, Display, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum CommandModifier {
  /// The base form of a command, without any modifier.
  #[default]
  None,
  /// Adverbial forms.
  Adverb(Adverb),
  /// Prepositional forms.
  Preposition(Preposition),
}
