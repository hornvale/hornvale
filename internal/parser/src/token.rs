use derive_more::Display;
use hornvale_command::prelude::*;
use serde::{Deserialize, Serialize};

/// Different kinds of tokens for the scanner.
pub mod kind;
use kind::TokenKind;

/// Tokens for the scanner.
#[derive(Clone, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[display(fmt = "{:?} {}", kind, lexeme)]
pub struct Token {
  /// The kind of token.
  pub kind: TokenKind,
  /// The lexeme.
  pub lexeme: String,
}

impl TryFrom<&Token> for CommandArgument {
  type Error = ();
  fn try_from(token: &Token) -> Result<Self, ()> {
    match token {
      token if token.kind.is_direction() => token.kind.try_into(),
      _ => Err(()),
    }
  }
}
