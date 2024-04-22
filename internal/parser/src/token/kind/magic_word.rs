use crate::prelude::*;
use serde::{Deserialize, Serialize};
use strum::Display;

/// A magic word, i.e. beginning with a special character.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum MagicWord {
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
}

impl MagicWord {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
  }
}

impl TryFrom<&str> for MagicWord {
  type Error = ();

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    let character = value.chars().next();
    if character.is_none() {
      return Err(());
    }
    let character = character.unwrap();
    let character_token = Character::try_from(character)?;
    match character_token {
      Character::Bang => Ok(Self::BangWord),
      Character::Question => Ok(Self::QuestionWord),
      Character::AtSign => Ok(Self::AtSignWord),
      Character::Hash => Ok(Self::HashWord),
      Character::Dollar => Ok(Self::DollarWord),
      Character::Percent => Ok(Self::PercentWord),
      Character::Caret => Ok(Self::CaretWord),
      Character::Ampersand => Ok(Self::AmpersandWord),
      Character::Asterisk => Ok(Self::AsteriskWord),
      Character::ForwardSlash => Ok(Self::ForwardSlashWord),
      Character::BackSlash => Ok(Self::BackSlashWord),
      Character::LeftParenthesis => Ok(Self::LeftParenthesisWord),
      Character::RightParenthesis => Ok(Self::RightParenthesisWord),
      Character::LeftSquareBracket => Ok(Self::LeftSquareBracketWord),
      Character::RightSquareBracket => Ok(Self::RightSquareBracketWord),
      Character::LeftCurlyBrace => Ok(Self::LeftCurlyBraceWord),
      Character::RightCurlyBrace => Ok(Self::RightCurlyBraceWord),
      Character::LessThan => Ok(Self::LessThanWord),
      Character::GreaterThan => Ok(Self::GreaterThanWord),
      Character::Equals => Ok(Self::EqualsWord),
      Character::Plus => Ok(Self::PlusWord),
      Character::Minus => Ok(Self::MinusWord),
      Character::Pipe => Ok(Self::PipeWord),
      Character::Colon => Ok(Self::ColonWord),
      Character::Underscore => Ok(Self::UnderscoreWord),
      Character::Tilde => Ok(Self::TildeWord),
      _ => Err(()),
    }
  }
}
