use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

/// A dictionary mapping words to their parts of speech.
pub mod dictionary;

/// Represents the part of speech of a word.
#[derive(Debug, Clone, Copy, Display, EnumIter, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum PartOfSpeech {
  /// A word that represents a person, place, thing, or idea (e.g. "cat",
  /// "house", "love").
  Noun,
  /// A word that takes the place of a noun (e.g. "I", "you", "he").
  Pronoun,
  /// A word that represents an action or state of being (e.g. "run", "is").
  Verb,
  /// A word that describes a noun or pronoun (e.g. "big", "happy").
  Adjective,
  /// A word that describes a verb, adjective, or another adverb (e.g.
  /// "quickly", "very").
  Adverb,
  /// A word that shows the relationship between a noun or pronoun and another
  /// word in the sentence (e.g. "in", "on").
  Preposition,
  /// A word that connects words, phrases, or clauses (e.g. "and", "but").
  Conjunction,
  /// A word that expresses strong emotion or surprise (e.g. "wow", "ouch").
  Interjection,
  /// A determiner combines with a noun to express its reference in the context
  /// of the sentence (e.g. "the", "a", "that", "my", "both").
  Determiner,
}
