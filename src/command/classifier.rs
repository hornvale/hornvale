use crate::command::prelude::*;
use crate::core::prelude::*;

#[cfg(test)]
pub mod tests;

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
  pub fn classify_tokens(&self, tokens: &mut [Token]) -> Result<(), CommandError> {
    // We should always have at least one token.
    self.assert_non_empty(tokens)?;
    // The first token should always be a verb.
    self.set_first_token_to_verb(tokens)?;
    // Loop through the tokens and classify any unclassified tokens.
    let mut limit: i8 = 3;
    let mut unclassified_count = self.get_unclassified_count(tokens);
    while limit > 0 && unclassified_count > 0 {
      self.classify_tokens_inner(tokens)?;
      unclassified_count = self.get_unclassified_count(tokens);
      log::error!("Unclassified tokens: {}", unclassified_count);
      limit -= 1;
    }
    println!("Exiting with {} unclassified tokens.", unclassified_count);
    // Assume success.
    Ok(())
  }

  /// Classify the tokens in the input slice.
  pub fn classify_tokens_inner(&self, tokens: &mut [Token]) -> Result<(), CommandError> {
    let mut index = 0;
    while let Some(i) = self.get_next_unclassified_index(tokens, index) {
      let kind = self.peek(tokens, i).unwrap();
      match kind {
        TokenKind::Word(_) => self.classify_word(tokens, i)?,
        TokenKind::Her(_) => self.classify_her(tokens, i)?,
        TokenKind::Up | TokenKind::Down | TokenKind::In | TokenKind::Out => self.classify_direction_word(tokens, i)?,
        _ => unreachable!(),
      }
      index += 1;
    }
    // Assume success.
    Ok(())
  }

  /// Classify a generic word.
  pub fn classify_word(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    // If it's the last token, it's a noun.
    if self.is_last_word_token(tokens, index) {
      tokens[index].kind = TokenKind::Word(Word::Noun);
      return Ok(());
    }
    match self.peek(tokens, index + 1) {
      Some(TokenKind::And | TokenKind::Character(Character::Comma) | TokenKind::NumberLiteral) => {
        tokens[index].kind = TokenKind::Word(Word::Noun);
        Ok(())
      },
      Some(TokenKind::CommandModifier(_)) => {
        tokens[index].kind = TokenKind::Word(Word::Noun);
        Ok(())
      },
      Some(TokenKind::Word(Word::Noun) | TokenKind::NounPossessiveDeterminer | TokenKind::Word(Word::Adjective)) => {
        tokens[index].kind = TokenKind::Word(Word::Adjective);
        Ok(())
      },
      _ => Ok(()),
    }
  }

  /// Classify a "her" token.
  pub fn classify_her(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    match self.is_last_token(tokens, index) {
      true => tokens[index].kind = TokenKind::Her(Her::Pronoun),
      false => tokens[index].kind = TokenKind::Her(Her::PossessiveDeterminer),
    }
    Ok(())
  }

  /// Classify a direction word.
  pub fn classify_direction_word(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    match self.is_last_token(tokens, index) {
      true => {
        let previous_word = tokens[index - 1].kind;
        if previous_word.is_noun() {
          tokens[index].kind = TokenKind::CommandModifier(CommandModifier::try_from(&*tokens[index].lexeme).unwrap());
        } else {
          tokens[index].kind = TokenKind::Direction(Direction::try_from(&*tokens[index].lexeme).unwrap());
        }
      },
      false => {
        tokens[index].kind = TokenKind::CommandModifier(CommandModifier::try_from(&*tokens[index].lexeme).unwrap())
      },
    }
    Ok(())
  }

  /// Assert that the slice of tokens is non-empty and ends with an EndOfInput
  /// token.
  pub fn assert_non_empty(&self, tokens: &[Token]) -> Result<(), CommandError> {
    match tokens.len() {
      0 => Err(CommandError::NoInput),
      1 if tokens[0].kind == TokenKind::EndOfInput => Err(CommandError::NoVerb),
      1 if tokens[0].kind != TokenKind::EndOfInput => Err(CommandError::InvalidTokenSequence(
        "did not end with end-of-input token".to_string(),
      )),
      _ => Ok(()),
    }
  }

  /// Get the index of the next unclassified token in the slice.
  pub fn get_next_unclassified_index(&self, tokens: &[Token], start: usize) -> Option<usize> {
    tokens[start..]
      .iter()
      .position(|t| t.kind.is_unclassified())
      .map(|i| start + i)
  }

  /// Count the unclassified tokens in the slice.
  pub fn get_unclassified_count(&self, tokens: &[Token]) -> usize {
    tokens.iter().filter(|t| t.kind.is_unclassified()).count()
  }

  /// Set the first token to a verb.
  pub fn set_first_token_to_verb(&self, tokens: &mut [Token]) -> Result<(), CommandError> {
    if !tokens[0].kind.could_be_verb() {
      return Err(CommandError::NoVerb);
    }
    tokens[0].kind = TokenKind::Word(Word::Verb);
    Ok(())
  }

  /// Is this the last word token in the slice?
  pub fn is_last_word_token(&self, tokens: &[Token], index: usize) -> bool {
    index == self.get_last_word_token_index(tokens)
  }

  /// Is this the last token in the slice?
  pub fn is_last_token(&self, tokens: &[Token], index: usize) -> bool {
    index == self.get_last_token_index(tokens)
  }

  /// Get the max index of word tokens in the slice.
  pub fn get_last_word_token_index(&self, tokens: &[Token]) -> usize {
    tokens
      .iter()
      .rposition(|t| matches!(t.kind, TokenKind::Word(_)))
      .unwrap()
  }

  /// Get the max index of actual meaningful tokens (read: not EndOfInput) in the slice.
  pub fn get_last_token_index(&self, tokens: &[Token]) -> usize {
    tokens
      .iter()
      .rposition(|t| !matches!(t.kind, TokenKind::EndOfInput))
      .unwrap()
  }

  /// Peek at a specified index.
  pub fn peek(&self, tokens: &[Token], index: usize) -> Option<TokenKind> {
    tokens.get(index).map(|t| t.kind)
  }
}
