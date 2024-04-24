use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

#[cfg(test)]
mod tests;

/// An enumeration of the possibilities for `Her`.
pub mod her_token;
use her_token::HerToken;
/// Trait implementations.
pub mod traits;
/// An enumeration of the possibilities for any given `Word` token.
pub mod word_token;
use word_token::WordToken;

/// Different kinds of tokens for the scanner.
#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum TokenKind {
  //
  // Single-character tokens.
  //
  /// A single quote, e.g. in `say 'hello'` or `take the thief's treasure`.
  SingleQuote,
  /// A comma, e.g. in `take sword, shield`.
  Comma,
  /// A period, e.g. in `east. attack troll with sword. west`.
  Period,
  /// A semicolon, e.g. in `east; attack troll with sword; west`.
  Semicolon,
  /// An exclamation point, e.g. in `!`.
  Bang,
  /// A question mark, e.g. in `?`.
  Question,
  /// At sign, e.g. in `@`.
  AtSign,
  /// Hash sign, e.g. in `#`.
  Hash,
  /// Dollar sign, e.g. in `$`.
  Dollar,
  /// Percent sign, e.g. in `%`.
  Percent,
  /// Caret, e.g. in `^`.
  Caret,
  /// Ampersand, e.g. in `&`.
  Ampersand,
  /// Asterisk, e.g. in `*`.
  Asterisk,
  /// Forward slash, e.g. in `/`.
  ForwardSlash,
  /// Backward slash, e.g. in `\`.
  BackSlash,
  /// Left parenthesis, e.g. in `(`.
  LeftParenthesis,
  /// Right parenthesis, e.g. in `)`.
  RightParenthesis,
  /// Left square bracket, e.g. in `[`.
  LeftSquareBracket,
  /// Right square bracket, e.g. in `]`.
  RightSquareBracket,
  /// Left curly brace, e.g. in `{`.
  LeftCurlyBrace,
  /// Right curly brace, e.g. in `}`.
  RightCurlyBrace,
  /// Less than sign, e.g. in `<`.
  LessThan,
  /// Greater than sign, e.g. in `>`.
  GreaterThan,
  /// Equals sign, e.g. in `=`.
  Equals,
  /// Plus sign, e.g. in `+`.
  Plus,
  /// Minus sign, e.g. in `-`.
  Minus,
  /// Pipe, e.g. in `|`.
  Pipe,
  /// Colon, e.g. in `:`.
  Colon,
  /// Underscore, e.g. in `_`.
  Underscore,
  /// Tilde, e.g. in `~`.
  Tilde,
  /// Backtick, e.g. in `\``.
  Backtick,

  //
  // Multi-character tokens.
  //
  /// A string literal, e.g. in `say "hello"`.
  StringLiteral,
  /// A number literal, e.g. in `take 5 coins`. We use `u32` for simplicity.
  NumberLiteral,
  /// An ordinal, e.g. in `take 1st coin`.
  Ordinal,
  /// A general possessive determiner ("someone's").
  NounPossessiveDeterminer,

  //
  // Word tokens.
  //
  // Compass directions.
  /// "North", e.g. in `go north`.
  North,
  /// "Northeast", e.g. in `go northeast`.
  Northeast,
  /// "East", e.g. in `go east`.
  East,
  /// "Southeast", e.g. in `go southeast`.
  Southeast,
  /// "South", e.g. in `go south`.
  South,
  /// "Southwest", e.g. in `go southwest`.
  Southwest,
  /// "West", e.g. in `go west`.
  West,
  /// "Northwest", e.g. in `go northwest`.
  Northwest,

  // Command modifiers.
  /// "About", e.g. in `ask man about the goat`.
  About,
  /// "Above", e.g. in `look above the shelf`.
  Above,
  /// "Across", e.g. in `go across the bridge`.
  Across,
  /// "Against", e.g. in `lean against the wall`.
  Against,
  /// "Along", e.g. in `walk along the path`.
  Along,
  /// "Among", e.g. in `look among the trees`.
  Among,
  /// "Around", e.g. in `look around`.
  Around,
  /// "As", e.g. in `take it as a gift`.
  As,
  /// "At", e.g. in `look at the sky`.
  At,
  /// "Before", e.g. in `go before the king`.
  Before,
  /// "Behind", e.g. in `hide behind the tree`.
  Behind,
  /// "Below", e.g. in `look below the table`.
  Below,
  /// "Beside", e.g. in `stand beside the knight`.
  Beside,
  /// "Between", e.g. in `walk between the trees`.
  Between,
  /// "Beyond", e.g. in `go beyond the mountains`.
  Beyond,
  /// "By", e.g. in `stand by the door`.
  By,
  /// "Down", e.g. in `climb down the ladder`.
  Down,
  /// "For", e.g. in `fight for the king`.
  For,
  /// "From", e.g. in `take it from the chest`.
  From,
  /// "Here", e.g. in `look here`.
  Here,
  /// "In", e.g. in `put the pie in the oven`.
  In,
  /// "Into", e.g. in `go into the forest`.
  Into,
  /// "Of", e.g. in `take the sword of the king`.
  Of,
  /// "Off", e.g. in `take the hat off`.
  Off,
  /// "On", e.g. in `put the book on the table`.
  On,
  /// "Out", e.g. in `go out of the cave`.
  Out,
  /// "Over", e.g. in `jump over the fence`.
  Over,
  /// "To", e.g. in `go to the castle`.
  To,
  /// "Toward", e.g. in `walk toward the light`.
  Toward,
  /// "Under", e.g. in `hide under the bed`.
  Under,
  /// "Up", e.g. in `climb up the mountain`.
  Up,
  /// "Upon", e.g. in `stand upon the hill`.
  Upon,
  /// "With", e.g. in `fight with the sword`.
  With,
  /// "Without", e.g. in `go without the sword`.
  Without,

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
  Her(HerToken),
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

  //
  // Words beginning with special characters.
  //
  /// An exclamation point, e.g. in `!`.
  BangWord,
  /// A question mark, e.g. in `?`.
  QuestionWord,
  /// At sign, e.g. in `@`.
  AtSignWord,
  /// Hash sign, e.g. in `#`.
  HashWord,
  /// Dollar sign, e.g. in `$`.
  DollarWord,
  /// Percent sign, e.g. in `%`.
  PercentWord,
  /// Caret, e.g. in `^`.
  CaretWord,
  /// Ampersand, e.g. in `&`.
  AmpersandWord,
  /// Asterisk, e.g. in `*`.
  AsteriskWord,
  /// Forward slash, e.g. in `/`.
  ForwardSlashWord,
  /// Backward slash, e.g. in `\`.
  BackSlashWord,
  /// Left parenthesis, e.g. in `(`.
  LeftParenthesisWord,
  /// Right parenthesis, e.g. in `)`.
  RightParenthesisWord,
  /// Left square bracket, e.g. in `[`.
  LeftSquareBracketWord,
  /// Right square bracket, e.g. in `]`.
  RightSquareBracketWord,
  /// Left curly brace, e.g. in `{`.
  LeftCurlyBraceWord,
  /// Right curly brace, e.g. in `}`.
  RightCurlyBraceWord,
  /// Less than sign, e.g. in `<`.
  LessThanWord,
  /// Greater than sign, e.g. in `>`.
  GreaterThanWord,
  /// Equals sign, e.g. in `=`.
  EqualsWord,
  /// Plus sign, e.g. in `+`.
  PlusWord,
  /// Minus sign, e.g. in `-`.
  MinusWord,
  /// Pipe, e.g. in `|`.
  PipeWord,
  /// Colon, e.g. in `:`.
  ColonWord,
  /// Underscore, e.g. in `_`.
  UnderscoreWord,
  /// Tilde, e.g. in `~`.
  TildeWord,

  /// Any other word.
  Word(WordToken),

  //
  // Any other tokens.
  //
  /// End of input.
  EndOfInput,
}

impl TokenKind {
  /// Is this token a single-character token?
  pub fn is_single_character(&self) -> bool {
    matches!(
      self,
      Self::SingleQuote
        | Self::Comma
        | Self::Period
        | Self::Semicolon
        | Self::Bang
        | Self::Question
        | Self::AtSign
        | Self::Hash
        | Self::Dollar
        | Self::Percent
        | Self::Caret
        | Self::Ampersand
        | Self::Asterisk
        | Self::ForwardSlash
        | Self::BackSlash
        | Self::LeftParenthesis
        | Self::RightParenthesis
        | Self::LeftSquareBracket
        | Self::RightSquareBracket
        | Self::LeftCurlyBrace
        | Self::RightCurlyBrace
        | Self::LessThan
        | Self::GreaterThan
        | Self::Equals
        | Self::Plus
        | Self::Minus
        | Self::Pipe
        | Self::Colon
        | Self::Underscore
        | Self::Tilde
        | Self::Backtick
    )
  }

  /// Is this token punctuation (that we will use in the parser)?
  ///
  /// Currently, the only valid punctuation character is the comma.
  pub fn is_punctuation(&self) -> bool {
    matches!(self, Self::Comma)
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
    self.is_single_character() && !matches!(self, Self::Comma | Self::SingleQuote)
  }

  /// Is this token a direction?
  pub fn is_direction(&self) -> bool {
    matches!(
      self,
      Self::North
        | Self::Northeast
        | Self::East
        | Self::Southeast
        | Self::South
        | Self::Southwest
        | Self::West
        | Self::Northwest
        | Self::Up
        | Self::Down
        | Self::In
        | Self::Out
    )
  }

  /// Is this token a preposition?
  pub fn is_preposition(&self) -> bool {
    matches!(
      self,
      Self::About
        | Self::Above
        | Self::Across
        | Self::Against
        | Self::Along
        | Self::Among
        | Self::As
        | Self::At
        | Self::Before
        | Self::Behind
        | Self::Below
        | Self::Beside
        | Self::Between
        | Self::Beyond
        | Self::By
        | Self::For
        | Self::From
        | Self::In
        | Self::Into
        | Self::Of
        | Self::Off
        | Self::On
        | Self::Out
        | Self::Over
        | Self::To
        | Self::Toward
        | Self::Under
        | Self::Upon
        | Self::With
        | Self::Without
    )
  }

  /// Is this token an adverb?
  pub fn is_adverb(&self) -> bool {
    matches!(
      self,
      Self::Around | Self::Down | Self::Here | Self::In | Self::Off | Self::On | Self::Out | Self::Up
    )
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
      Self::My
        | Self::Your
        | Self::His
        | Self::Her(HerToken::PossessiveDeterminer)
        | Self::Its
        | Self::Our
        | Self::Their
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
      Self::Me | Self::You | Self::Him | Self::Her(HerToken::Pronoun) | Self::It | Self::Them
    )
  }

  /// Is this token an adjective?
  pub fn is_adjective(&self) -> bool {
    matches!(self, Self::Word(WordToken::Adjective))
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
    matches!(self, Self::Word(WordToken::Verb))
  }

  /// Is this token a noun?
  pub fn is_noun(&self) -> bool {
    self.is_direction() || self.is_pronoun() || matches!(self, Self::All | Self::Word(WordToken::Noun))
  }

  /// Could this token be a noun?
  pub fn could_be_noun(&self) -> bool {
    self.is_noun()
      || matches!(
        self,
        Self::Word(WordToken::Unclassified | WordToken::Noun | WordToken::Ambiguous)
      )
  }

  /// Could this token be an adjective?
  pub fn could_be_adjective(&self) -> bool {
    matches!(
      self,
      Self::Word(WordToken::Unclassified | WordToken::Adjective | WordToken::Ambiguous)
    )
  }

  /// Could this token be a verb?
  pub fn could_be_verb(&self) -> bool {
    self.is_verb()
      || matches!(
        self,
        Self::Word(WordToken::Unclassified | WordToken::Verb | WordToken::Ambiguous)
      )
  }

  /// Does this token accept adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    (self.is_noun() && !self.is_pronoun() && !self.is_direction() && !self.is_distributive_determiner())
      || self.is_noun_possessive_determiner()
      || matches!(self, Self::Comma | Self::Word(WordToken::Adjective))
  }

  /// Is this token a magic word?
  pub fn is_magic_word(&self) -> bool {
    matches!(
      self,
      Self::BangWord
        | Self::QuestionWord
        | Self::AtSignWord
        | Self::HashWord
        | Self::DollarWord
        | Self::PercentWord
        | Self::CaretWord
        | Self::AmpersandWord
        | Self::AsteriskWord
        | Self::ForwardSlashWord
        | Self::BackSlashWord
        | Self::LeftParenthesisWord
        | Self::RightParenthesisWord
        | Self::LeftSquareBracketWord
        | Self::RightSquareBracketWord
        | Self::LeftCurlyBraceWord
        | Self::RightCurlyBraceWord
        | Self::LessThanWord
        | Self::GreaterThanWord
        | Self::EqualsWord
        | Self::PlusWord
        | Self::MinusWord
        | Self::PipeWord
        | Self::ColonWord
        | Self::UnderscoreWord
        | Self::TildeWord
    )
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
