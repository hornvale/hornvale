use crate::prelude::*;
use serde::{Deserialize, Serialize};
use strum::Display;

/// A single-character token.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Character {
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
}

impl Character {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
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
    !matches!(self, Self::Comma | Self::SingleQuote)
  }

  /// Get the character value of this token.
  pub fn as_char(&self) -> char {
    match self {
      Self::SingleQuote => '\'',
      Self::Comma => ',',
      Self::Period => '.',
      Self::Semicolon => ';',
      Self::Bang => '!',
      Self::Question => '?',
      Self::AtSign => '@',
      Self::Hash => '#',
      Self::Dollar => '$',
      Self::Percent => '%',
      Self::Caret => '^',
      Self::Ampersand => '&',
      Self::Asterisk => '*',
      Self::ForwardSlash => '/',
      Self::BackSlash => '\\',
      Self::LeftParenthesis => '(',
      Self::RightParenthesis => ')',
      Self::LeftSquareBracket => '[',
      Self::RightSquareBracket => ']',
      Self::LeftCurlyBrace => '{',
      Self::RightCurlyBrace => '}',
      Self::LessThan => '<',
      Self::GreaterThan => '>',
      Self::Equals => '=',
      Self::Plus => '+',
      Self::Minus => '-',
      Self::Pipe => '|',
      Self::Colon => ':',
      Self::Underscore => '_',
      Self::Tilde => '~',
      Self::Backtick => '`',
    }
  }

  /// Get the magic word token if a word matches this character.
  pub fn as_magic_word(&self) -> Option<MagicWord> {
    match self {
      Self::Bang => Some(MagicWord::BangWord),
      Self::Question => Some(MagicWord::QuestionWord),
      Self::AtSign => Some(MagicWord::AtSignWord),
      Self::Hash => Some(MagicWord::HashWord),
      Self::Dollar => Some(MagicWord::DollarWord),
      Self::Percent => Some(MagicWord::PercentWord),
      Self::Caret => Some(MagicWord::CaretWord),
      Self::Ampersand => Some(MagicWord::AmpersandWord),
      Self::Asterisk => Some(MagicWord::AsteriskWord),
      Self::ForwardSlash => Some(MagicWord::ForwardSlashWord),
      Self::BackSlash => Some(MagicWord::BackSlashWord),
      Self::LeftParenthesis => Some(MagicWord::LeftParenthesisWord),
      Self::RightParenthesis => Some(MagicWord::RightParenthesisWord),
      Self::LeftSquareBracket => Some(MagicWord::LeftSquareBracketWord),
      Self::RightSquareBracket => Some(MagicWord::RightSquareBracketWord),
      Self::LeftCurlyBrace => Some(MagicWord::LeftCurlyBraceWord),
      Self::RightCurlyBrace => Some(MagicWord::RightCurlyBraceWord),
      Self::LessThan => Some(MagicWord::LessThanWord),
      Self::GreaterThan => Some(MagicWord::GreaterThanWord),
      Self::Equals => Some(MagicWord::EqualsWord),
      Self::Plus => Some(MagicWord::PlusWord),
      Self::Minus => Some(MagicWord::MinusWord),
      Self::Pipe => Some(MagicWord::PipeWord),
      Self::Colon => Some(MagicWord::ColonWord),
      Self::Underscore => Some(MagicWord::UnderscoreWord),
      Self::Tilde => Some(MagicWord::TildeWord),
      _ => None,
    }
  }
}

impl TryFrom<char> for Character {
  type Error = ();

  fn try_from(character: char) -> Result<Self, Self::Error> {
    match character {
      ',' => Ok(Self::Comma),
      '.' => Ok(Self::Period),
      ';' => Ok(Self::Semicolon),
      '!' => Ok(Self::Bang),
      '?' => Ok(Self::Question),
      '@' => Ok(Self::AtSign),
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
