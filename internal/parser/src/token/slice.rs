use crate::prelude::{Token, TokenKind};

/// A slice of tokens.
pub type TokenSlice<'a> = &'a [Token];

/// A mutable slice of tokens.
pub type TokenSliceMut<'a> = &'a mut [Token];

/// Extension methods for token slices.
pub trait TokenSliceExt {
  /// Find the first token that matches the given kind.
  fn find(&self, kind: TokenKind) -> Option<usize>;
  /// Find the last token that matches the given kind.
  fn rfind(&self, kind: TokenKind) -> Option<usize>;
  /// Find the first token that matches the given kind, starting from the
  /// specified index and moving forward.
  fn find_from(&self, kind: TokenKind, index: usize) -> Option<usize>;
  /// Find the last token that matches the given kind, starting from the
  /// specified index and moving forward.
  fn rfind_from(&self, kind: TokenKind, index: usize) -> Option<usize>;
  /// Find the first token matching the condition.
  fn find_matching<F>(&self, condition: F) -> Option<usize>
  where
    F: Fn(&Token) -> bool;
  /// Find the last token matching the condition.
  fn rfind_matching<F>(&self, condition: F) -> Option<usize>
  where
    F: Fn(&Token) -> bool;
  /// Find the end of input token.
  fn find_eoi(&self) -> Option<usize> {
    self.rfind(TokenKind::EndOfInput)
  }
  /// Find the last token that is not the end of input.
  fn rfind_not_eoi(&self) -> Option<usize> {
    self.rfind_matching(|t| t.kind != TokenKind::EndOfInput)
  }
}

/// Extension methods for mutable token slices.
pub trait TokenSliceMutExt {
  /// Set the kind of the specified token.
  fn set_kind(&mut self, index: usize, kind: TokenKind);
}

impl<'a> TokenSliceExt for TokenSlice<'a> {
  /// Find the first token that matches the given kind.
  fn find(&self, kind: TokenKind) -> Option<usize> {
    self.iter().position(|t| t.kind == kind)
  }
  /// Find the last token that matches the given kind.
  fn rfind(&self, kind: TokenKind) -> Option<usize> {
    self.iter().rposition(|t| t.kind == kind)
  }
  /// Find the first token that matches the given kind, starting from the
  /// specified index and moving forward.
  fn find_from(&self, kind: TokenKind, index: usize) -> Option<usize> {
    self.iter().skip(index).position(|t| t.kind == kind)
  }
  /// Find the last token that matches the given kind, starting from the
  /// specified index and moving backward.
  fn rfind_from(&self, kind: TokenKind, index: usize) -> Option<usize> {
    self.iter().take(index + 1).rposition(|t| t.kind == kind)
  }
  /// Find the first token matching the condition.
  fn find_matching<F>(&self, condition: F) -> Option<usize>
  where
    F: Fn(&Token) -> bool,
  {
    self.iter().position(condition)
  }
  /// Find the last token matching the condition.
  fn rfind_matching<F>(&self, condition: F) -> Option<usize>
  where
    F: Fn(&Token) -> bool,
  {
    self.iter().rposition(condition)
  }
}

impl<'a> TokenSliceExt for TokenSliceMut<'a> {
  /// Find the first token that matches the given kind.
  fn find(&self, kind: TokenKind) -> Option<usize> {
    self.iter().position(|t| t.kind == kind)
  }
  /// Find the last token that matches the given kind.
  fn rfind(&self, kind: TokenKind) -> Option<usize> {
    self.iter().rposition(|t| t.kind == kind)
  }
  /// Find the first token that matches the given kind, starting from the
  /// specified index and moving forward.
  fn find_from(&self, kind: TokenKind, index: usize) -> Option<usize> {
    self.iter().skip(index).position(|t| t.kind == kind)
  }
  /// Find the last token that matches the given kind, starting from the
  /// specified index and moving backward.
  fn rfind_from(&self, kind: TokenKind, index: usize) -> Option<usize> {
    self.iter().take(index + 1).rposition(|t| t.kind == kind)
  }
  /// Find the first token matching the condition.
  fn find_matching<F>(&self, condition: F) -> Option<usize>
  where
    F: Fn(&Token) -> bool,
  {
    self.iter().position(condition)
  }
  /// Find the last token matching the condition.
  fn rfind_matching<F>(&self, condition: F) -> Option<usize>
  where
    F: Fn(&Token) -> bool,
  {
    self.iter().rposition(condition)
  }
}

impl<'a> TokenSliceMutExt for TokenSliceMut<'a> {
  /// Set the kind of the specified token.
  fn set_kind(&mut self, index: usize, kind: TokenKind) {
    self[index].kind = kind;
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_find() {
    init();
    let tokens = vec![
      Token {
        kind: TokenKind::Word,
        lexeme: "hello".to_string(),
      },
      Token {
        kind: TokenKind::EndOfInput,
        lexeme: "".to_string(),
      },
    ];
    let slice = &*tokens;
    assert_eq!(slice.find(TokenKind::Word), Some(0));
    assert_eq!(slice.find(TokenKind::EndOfInput), Some(1));
  }

  #[test]
  fn test_rfind() {
    let tokens = vec![
      Token {
        kind: TokenKind::Word,
        lexeme: "hello".to_string(),
      },
      Token {
        kind: TokenKind::EndOfInput,
        lexeme: "".to_string(),
      },
    ];
    let slice = &*tokens;
    assert_eq!(slice.rfind(TokenKind::Word), Some(0));
    assert_eq!(slice.rfind(TokenKind::EndOfInput), Some(1));
  }

  #[test]
  fn test_find_first_from() {
    let tokens = vec![
      Token {
        kind: TokenKind::Word,
        lexeme: "hello".to_string(),
      },
      Token {
        kind: TokenKind::EndOfInput,
        lexeme: "".to_string(),
      },
    ];
    let slice = &*tokens;
    assert_eq!(slice.find_from(TokenKind::Word, 1), None);
    assert_eq!(slice.find_from(TokenKind::EndOfInput, 0), Some(1));
  }

  #[test]
  fn test_rfind_from() {
    let tokens = vec![
      Token {
        kind: TokenKind::Word,
        lexeme: "hello".to_string(),
      },
      Token {
        kind: TokenKind::EndOfInput,
        lexeme: "".to_string(),
      },
    ];
    let slice = &*tokens;
    assert_eq!(slice.rfind_from(TokenKind::Word, 0), Some(0));
    assert_eq!(slice.rfind_from(TokenKind::EndOfInput, 1), Some(1));
  }

  #[test]
  fn test_find_eoi() {
    let tokens = vec![
      Token {
        kind: TokenKind::Word,
        lexeme: "hello".to_string(),
      },
      Token {
        kind: TokenKind::EndOfInput,
        lexeme: "".to_string(),
      },
    ];
    let slice = &*tokens;
    assert_eq!(slice.find_eoi(), Some(1));
  }
}
