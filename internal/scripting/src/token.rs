/// An error type for tokens.
pub mod error;
/// The token kind.
pub mod kind;
use derive_more::Display;
use kind::TokenKind;
use thiserror::Error;

/// The `Token` type.
#[derive(Clone, Copy, Debug, Display, Eq, Error, Hash, PartialEq)]
#[display(fmt = "kind: {}, lexeme: {}", kind, lexeme)]
pub struct Token<'source> {
  /// The type of this token.
  pub kind: TokenKind,
  /// The lexeme.
  pub lexeme: &'source str,
  /// The line number.
  pub line_number: usize,
}

impl<'source> Token<'source> {
  /// Constructor.
  pub fn synthesize(lexeme: &'source str) -> Self {
    let kind = TokenKind::Error;
    let line_number = 0;
    Self {
      kind,
      lexeme,
      line_number,
    }
  }
}
