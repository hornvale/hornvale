use crate::token::error::TokenError;
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use strum::Display;

/// The kind of the token.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub enum TokenKind {
  /// A left parenthesis.
  LeftParenthesis,
  /// A right parenthesis.
  RightParenthesis,
  /// A left brace.
  LeftBrace,
  /// A right brace.
  RightBrace,
  /// A comma.
  Comma,
  /// A dot.
  Dot,
  /// A minus.
  Minus,
  /// A plus.
  Plus,
  /// A semicolon.
  Semicolon,
  /// A slash.
  Slash,
  /// A star.
  Star,
  /// An exclamation point.
  Bang,
  /// An exclamation point followed by an equal sign.
  BangEqual,
  /// An equal sign.
  Equal,
  /// Two equal signs.
  EqualEqual,
  /// A greater than sign.
  GreaterThan,
  /// A greater than sign followed by an equal sign.
  GreaterThanOrEqual,
  /// A less than sign.
  LessThan,
  /// A less than sign followed by an equal sign.
  LessThanOrEqual,
  /// An identifier.
  Identifier,
  /// A string literal.
  String,
  /// A number literal.
  Number,
  /// The `and` keyword.
  And,
  /// The `class` keyword.
  Class,
  /// The `else` keyword.
  Else,
  /// The `false` keyword.
  False,
  /// The `fun` keyword.
  Function,
  /// The `for` keyword.
  For,
  /// The `if` keyword.
  If,
  /// The `nil` keyword.
  Nil,
  /// The `or` keyword.
  Or,
  /// The `print` keyword.
  Print,
  /// The `return` keyword.
  Return,
  /// The `super` keyword.
  Super,
  /// The `this` keyword.
  This,
  /// The `true` keyword.
  True,
  /// The `var` keyword.
  Var,
  /// The `while` keyword.
  While,
  /// The end of the file.
  Eof,
  /// An error.
  Error,
  /// An empty string.
  EmptyString,
}

impl TokenKind {
  /// Get all the types.
  pub fn get_all() -> Vec<Self> {
    use TokenKind::*;
    vec![
      LeftParenthesis,
      RightParenthesis,
      LeftBrace,
      RightBrace,
      Comma,
      Dot,
      Minus,
      Plus,
      Semicolon,
      Slash,
      Star,
      Bang,
      BangEqual,
      Equal,
      EqualEqual,
      GreaterThan,
      GreaterThanOrEqual,
      LessThan,
      LessThanOrEqual,
      Identifier,
      String,
      Number,
      And,
      Class,
      Else,
      False,
      Function,
      For,
      If,
      Nil,
      Or,
      Print,
      Return,
      Super,
      This,
      True,
      Var,
      While,
      Eof,
      Error,
      EmptyString,
    ]
  }
}

impl FromStr for TokenKind {
  type Err = TokenError;

  fn from_str(string: &str) -> Result<Self, Self::Err> {
    use TokenKind::*;
    match string {
      "and" | "&&" => Ok(And),
      "class" => Ok(Class),
      "else" => Ok(Else),
      "false" => Ok(False),
      "fun" => Ok(Function),
      "for" => Ok(For),
      "if" => Ok(If),
      "nil" => Ok(Nil),
      "or" | "||" => Ok(Or),
      "print" => Ok(Print),
      "return" => Ok(Return),
      "super" => Ok(Super),
      "this" => Ok(This),
      "true" => Ok(True),
      "var" => Ok(Var),
      "while" => Ok(While),
      unknown => Err(TokenError::UnknownKeyword(unknown.to_string())),
    }
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  pub fn test() {
    init();
    let all = TokenKind::get_all();
    println!("{:#?}", all.last().unwrap());
  }
}
