use crate::prelude::{ParserError, Token, TokenKind};
use crate::token::slice::*;

/// A classifier that can be used to determine the type of some words.
///
/// The parser can call this classifier to get hints about the type of a word
/// based on its position in the sentence. This is useful for disambiguating
/// words that can be multiple parts of speech (e.g. "mine" can be a possessive
/// pronoun, noun, verb, etc, depending on context), parsing performantly, and
/// for providing better error messages.
#[derive(Clone, Copy, Debug, Default)]
pub struct Classifier;

impl Classifier {
  /// Create a new classifier.
  pub fn new() -> Self {
    Self
  }

  /// Classify the tokens in a sentence and return "hints".
  pub fn classify_tokens(&self, tokens: &mut [Token]) -> Result<(), ParserError> {
    if tokens.is_empty() || tokens.find_eoi() == Some(0) {
      return Err(ParserError::NoInput);
    }
    tokens[0] = Token {
      kind: TokenKind::Verb,
      lexeme: tokens[0].lexeme.clone(),
    };
    self.find_prepositions(tokens)?;
    Ok(())
  }

  /// Find the prepositions in the tokens and classify accordingly.
  pub fn find_prepositions(&self, tokens: &mut [Token]) -> Result<(), ParserError> {
    let mut index = 1;
    let mut first_found = false;
    while index < tokens.len() {
      if !self.match_word(&tokens[index]) {
        index += 1;
        continue;
      }
      let preposition_index = self.find_first_preposition(&tokens[index..]);
      if preposition_index.is_none() {
        break;
      }
      let preposition_index = index + preposition_index.unwrap();
      if !first_found {
        self.set_token_kind(tokens, preposition_index - 1, TokenKind::DirectObject)?;
        first_found = true;
      }
      index = preposition_index + 1;
    }
    Ok(())
  }

  /// Does this token match a word?
  pub fn match_word(&self, token: &Token) -> bool {
    token.kind == TokenKind::Word
  }

  /// Does this token match the end of input?
  pub fn match_end_of_input(&self, token: &Token) -> bool {
    token.kind == TokenKind::EndOfInput
  }

  /// Set the kind of a token.
  pub fn set_token_kind(&self, tokens: &mut [Token], index: usize, kind: TokenKind) -> Result<(), ParserError> {
    if index >= tokens.len() {
      return Err(ParserError::IndexOutOfBounds);
    }
    tokens[index] = Token {
      kind,
      lexeme: tokens[index].lexeme.clone(),
    };
    Ok(())
  }

  /// Find the first preposition in the tokens.
  pub fn find_first_preposition(&self, tokens: &[Token]) -> Option<usize> {
    for (index, token) in tokens.iter().enumerate() {
      if token.kind.is_preposition() {
        return Some(index);
      }
    }
    None
  }
}
