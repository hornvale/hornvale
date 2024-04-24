use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

/// Different kinds of `Her`.
#[derive(
  Clone, Copy, Debug, Default, Deserialize, Display, EnumIter, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize,
)]
pub enum MagicWord {
  /// An exclamation point, e.g. in `!`.
  #[default]
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
