use crate::prelude::*;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

/// Different kinds of single-character tokens.
#[derive(Clone, Copy, Debug, Deserialize, Display, EnumIter, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum CharacterToken {
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

impl CharacterToken {
  /// Can this token begin a magic word?
  pub fn can_begin_magic_word(self) -> bool {
    !matches!(
      self,
      Self::SingleQuote | Self::Backtick | Self::Comma | Self::Period | Self::Semicolon
    )
  }

  /// What is the corresponding magic word for this token?
  pub fn to_magic_word(self) -> Option<MagicWordToken> {
    match self {
      Self::Bang => Some(MagicWordToken::BangWord),
      Self::Question => Some(MagicWordToken::QuestionWord),
      Self::AtSign => Some(MagicWordToken::AtSignWord),
      Self::Hash => Some(MagicWordToken::HashWord),
      Self::Dollar => Some(MagicWordToken::DollarWord),
      Self::Percent => Some(MagicWordToken::PercentWord),
      Self::Caret => Some(MagicWordToken::CaretWord),
      Self::Ampersand => Some(MagicWordToken::AmpersandWord),
      Self::Asterisk => Some(MagicWordToken::AsteriskWord),
      Self::ForwardSlash => Some(MagicWordToken::ForwardSlashWord),
      Self::BackSlash => Some(MagicWordToken::BackSlashWord),
      Self::LeftParenthesis => Some(MagicWordToken::LeftParenthesisWord),
      Self::RightParenthesis => Some(MagicWordToken::RightParenthesisWord),
      Self::LeftSquareBracket => Some(MagicWordToken::LeftSquareBracketWord),
      Self::RightSquareBracket => Some(MagicWordToken::RightSquareBracketWord),
      Self::LeftCurlyBrace => Some(MagicWordToken::LeftCurlyBraceWord),
      Self::RightCurlyBrace => Some(MagicWordToken::RightCurlyBraceWord),
      Self::LessThan => Some(MagicWordToken::LessThanWord),
      Self::GreaterThan => Some(MagicWordToken::GreaterThanWord),
      Self::Equals => Some(MagicWordToken::EqualsWord),
      Self::Plus => Some(MagicWordToken::PlusWord),
      Self::Minus => Some(MagicWordToken::MinusWord),
      Self::Pipe => Some(MagicWordToken::PipeWord),
      Self::Colon => Some(MagicWordToken::ColonWord),
      Self::Underscore => Some(MagicWordToken::UnderscoreWord),
      Self::Tilde => Some(MagicWordToken::TildeWord),
      _ => None,
    }
  }
}

impl TryFrom<char> for CharacterToken {
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
