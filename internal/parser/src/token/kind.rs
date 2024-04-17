use serde::{Deserialize, Serialize};
use strum::Display;

/// Different kinds of tokens for the scanner.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
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
  At,
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
  /// "And", e.g. in `take sword and shield`.
  And,
  /// Yes, e.g. in `yes` or `y`.
  Yes,
  /// No, e.g. in `no` or `n`.
  No,
  /// Indefinite article.
  A,
  /// Definite article.
  The,
  /// A string literal, e.g. in `say "hello"`.
  StringLiteral,
  /// A number literal, e.g. in `take 5 coins`. We use `u32` for simplicity.
  NumberLiteral,
  /// An ordinal, e.g. in `take 1st coin`.
  Ordinal,
  /// Then, e.g. in `take sword then take shield`.
  Then,
  /// Any other word.
  Word,

  //
  // Specialized words. These are used by the classifier after scanning.
  //
  /// A direction, e.g. in `go north`.
  Direction,
  /// A preposition, e.g. in `look behind the curtain`.
  Preposition,
  /// An adverb, e.g. in `turn lantern on`.
  Adverb,
  /// An adjective, e.g. in `take shiny sword`.
  Adjective,
  /// A pronoun, e.g. in `attack him`.
  Pronoun,
  /// Possessive pronoun, e.g. in `take hers` or `get mine`.
  PossessivePronoun,
  /// A demonstrative determiner, e.g. in `take this sword`.
  DemonstrativeDeterminer,
  /// A distributive determiner, e.g. in `take each sword`.
  DistributiveDeterminer,
  /// A generic possessive determiner, e.g. in `take the goblin's sword`.
  PossessiveDeterminer,
  /// A verb, e.g. in `quit`.
  Verb,
  /// A direct object, e.g. in `take sword`.
  DirectObject,
  /// An indirect object, e.g. in `give sword to goblin`.
  IndirectObject,

  //
  // Other tokens.
  //
  /// End of input.
  EndOfInput,
}

impl TryFrom<char> for TokenKind {
  type Error = ();

  fn try_from(character: char) -> Result<Self, Self::Error> {
    match character {
      ',' => Ok(Self::Comma),
      '.' => Ok(Self::Period),
      ';' => Ok(Self::Semicolon),
      '!' => Ok(Self::Bang),
      '?' => Ok(Self::Question),
      '@' => Ok(Self::At),
      '#' => Ok(Self::Hash),
      '$' => Ok(Self::Dollar),
      '%' => Ok(Self::Percent),
      '^' => Ok(Self::Caret),
      '&' => Ok(Self::Ampersand),
      '*' => Ok(Self::Asterisk),
      '/' => Ok(Self::ForwardSlash),
      '\\' => Ok(Self::BackSlash),
      '(' => Ok(Self::LeftParenthesis),
      ')' => Ok(Self::RightParenthesis),
      '[' => Ok(Self::LeftSquareBracket),
      ']' => Ok(Self::RightSquareBracket),
      '{' => Ok(Self::LeftCurlyBrace),
      '}' => Ok(Self::RightCurlyBrace),
      '<' => Ok(Self::LessThan),
      '>' => Ok(Self::GreaterThan),
      '=' => Ok(Self::Equals),
      '+' => Ok(Self::Plus),
      '-' => Ok(Self::Minus),
      '|' => Ok(Self::Pipe),
      ':' => Ok(Self::Colon),
      '_' => Ok(Self::Underscore),
      '~' => Ok(Self::Tilde),
      '`' => Ok(Self::Backtick),
      _ => Err(()),
    }
  }
}

impl TryFrom<&str> for TokenKind {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "and" => Ok(Self::And),
      "yes" => Ok(Self::Yes),
      "no" => Ok(Self::No),
      "a" | "an" => Ok(Self::A),
      "the" => Ok(Self::The),
      "then" => Ok(Self::Then),
      _ => Err(()),
    }
  }
}
