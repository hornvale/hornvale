use hornvale_core::prelude::*;
use serde::{Deserialize, Serialize};
use strum::Display;

/// An enumeration of the possibilities for single-character tokens.
pub mod character;
use character::Character;
/// An enumeration of the possibilities for `Her`.
pub mod her;
use her::Her;
/// An enumeration of the possibilities for any given `MagicWord` token.
pub mod magic_word;
use magic_word::MagicWord;
/// Trait implementations.
pub mod traits;
/// An enumeration of the possibilities for any given `Word` token.
pub mod word;
use word::Word;

/// Different kinds of tokens for the scanner.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum TokenKind {
  /// Single-character tokens.
  Character(Character),
  /// A string literal, e.g. in `say "hello"`.
  StringLiteral,
  /// A number literal, e.g. in `take 5 coins`. We use `u32` for simplicity.
  NumberLiteral,
  /// An ordinal, e.g. in `take 1st coin`.
  Ordinal,
  /// A general possessive determiner ("someone's").
  NounPossessiveDeterminer,
  /// Words beginning with special characters.
  MagicWord(MagicWord),
  /// Command modifiers (adverbs, prepositions, etc).
  CommandModifier(CommandModifier),
  /// Directions (north, south, up, down, in, out, etc).
  Direction(Direction),

  // Some tokens that can be directions or modifiers.
  /// e.g. `go down` or `look down the road`.
  Down,
  /// e.g. `go in` or `look in the box`.
  In,
  /// e.g. `go out` or `look out the window`.
  Out,
  /// e.g. `go up` or `look up at the sky`.
  Up,

  // Demonstrative determiners.
  /// "This", e.g. in `take this`.
  This,
  /// "That", e.g. in `take that`.
  That,
  /// "These", e.g. in `take these`.
  These,
  /// "Those", e.g. in `take those`.
  Those,

  // Distributive determiners.
  /// "All", e.g. in `take all`.
  All,
  Any,
  Each,
  Either,
  Every,
  Neither,
  Some,

  // Possessive determiners.
  /// `my`.
  My,
  /// `your`.
  Your,
  /// `his`.
  His,
  /// `her` (can also act as a pronoun).
  Her(Her),
  /// `its`.
  Its,
  /// `our`.
  Our,
  /// `their`.
  Their,

  // Pronouns.
  /// `I`/`me`/`myself`.
  Me,
  /// `you`.
  You,
  /// `he`.
  Him,
  // `her`.
  // Her (also possessive determiner)
  /// `it`.
  It,
  /// `them`.
  Them,

  /// Articles.
  A, // Also `an`.
  The,

  // Conjunctions.
  /// "And", e.g. in `take sword and shield`.
  And,
  /// "Or", e.g. in `take sword or shield`.
  Or,
  /// "But", e.g. in `take all but shield`.
  But,

  // Yes/No.
  /// e.g. in `yes` or `y`.
  Yes,
  /// e.g. in `no` or `n`.
  No,

  /// Any other word.
  Word(Word),

  //
  // Any other tokens.
  //
  /// End of input.
  EndOfInput,
}

impl TokenKind {
  /// Is this token a single-character token?
  pub fn is_single_character(&self) -> bool {
    matches!(self, Self::Character(_))
  }

  /// Is this token punctuation (that we will use in the parser)?
  ///
  /// Currently, the only valid punctuation character is the comma.
  pub fn is_punctuation(&self) -> bool {
    matches!(self, Self::Character(Character::Comma))
  }

  /// Is this token a special character?
  ///
  /// This may be punctuation in real life, but we treat it as a special
  /// character in the parser.
  ///
  /// So _not_:
  /// - `Comma`
  /// - `SingleQuote`
  pub fn is_special_character(&self) -> bool {
    self.is_single_character() && !matches!(self, Self::Character(Character::Comma | Character::SingleQuote))
  }

  /// Is this token a direction?
  pub fn is_direction(&self) -> bool {
    matches!(self, Self::Direction(_))
  }

  /// Is this token a command modifier?
  pub fn is_command_modifier(&self) -> bool {
    matches!(self, Self::CommandModifier(_))
  }

  /// Is this token a demonstrative determiner?
  pub fn is_demonstrative_determiner(&self) -> bool {
    matches!(self, Self::This | Self::That | Self::These | Self::Those)
  }

  /// Is this token a distributive determiner?
  pub fn is_distributive_determiner(&self) -> bool {
    matches!(
      self,
      Self::All | Self::Any | Self::Each | Self::Either | Self::Every | Self::Neither | Self::Some
    )
  }

  /// Is this token a possessive determiner?
  pub fn is_possessive_determiner(&self) -> bool {
    self.is_noun_possessive_determiner() || self.is_personal_possessive_determiner()
  }

  /// Is this token a definite (or pronominal) possessive determiner?
  pub fn is_personal_possessive_determiner(&self) -> bool {
    matches!(
      self,
      Self::My | Self::Your | Self::His | Self::Her(Her::PossessiveDeterminer) | Self::Its | Self::Our | Self::Their
    )
  }

  /// Is this token a noun possessive determiner?
  pub fn is_noun_possessive_determiner(&self) -> bool {
    matches!(self, Self::NounPossessiveDeterminer)
  }

  /// Is this token a pronoun?
  pub fn is_pronoun(&self) -> bool {
    matches!(
      self,
      Self::Me | Self::You | Self::Him | Self::Her(Her::Pronoun) | Self::It | Self::Them
    )
  }

  /// Is this token an adjective?
  pub fn is_adjective(&self) -> bool {
    matches!(self, Self::Word(Word::Adjective))
  }

  /// Is this token an article?
  pub fn is_article(&self) -> bool {
    matches!(self, Self::A | Self::The)
  }

  /// Is this token a conjunction?
  pub fn is_conjunction(&self) -> bool {
    matches!(self, Self::And | Self::Or | Self::But)
  }

  /// Is this token a verb?
  pub fn is_verb(&self) -> bool {
    matches!(self, Self::Word(Word::Verb))
  }

  /// Is this token a noun?
  pub fn is_noun(&self) -> bool {
    self.is_direction() || self.is_pronoun() || matches!(self, Self::All | Self::Word(Word::Noun))
  }

  /// Could this token be a noun?
  pub fn could_be_noun(&self) -> bool {
    self.is_noun() || matches!(self, Self::Word(Word::Unclassified | Word::Noun | Word::Ambiguous))
  }

  /// Could this token be an adjective?
  pub fn could_be_adjective(&self) -> bool {
    matches!(self, Self::Word(Word::Unclassified | Word::Adjective | Word::Ambiguous))
  }

  /// Could this token be a verb?
  pub fn could_be_verb(&self) -> bool {
    self.is_verb() || matches!(self, Self::Word(Word::Unclassified | Word::Verb | Word::Ambiguous))
  }

  /// Does this token accept adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    (self.is_noun() && !self.is_pronoun() && !self.is_direction() && !self.is_distributive_determiner())
      || self.is_noun_possessive_determiner()
      || matches!(self, Self::Character(Character::Comma) | Self::Word(Word::Adjective))
  }

  /// Is this token a magic word?
  pub fn is_magic_word(&self) -> bool {
    matches!(self, Self::MagicWord(_))
  }

  /// Is this token a yes/no token?
  pub fn is_yes_no(&self) -> bool {
    matches!(self, Self::Yes | Self::No)
  }

  /// Get the boolean value of this token.
  pub fn as_bool(&self) -> Option<bool> {
    match self {
      Self::Yes => Some(true),
      Self::No => Some(false),
      _ => None,
    }
  }
}
