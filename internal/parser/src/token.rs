use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Different kinds of tokens for the scanner.
pub mod kind;
use kind::TokenKind;

/// Tokens for the scanner.
#[derive(Clone, Debug, Default, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[display(fmt = "{:?} {}", kind, lexeme)]
pub struct Token {
  /// The kind of token.
  pub kind: TokenKind,
  /// The lexeme.
  pub lexeme: String,
}
