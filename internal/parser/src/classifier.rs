use crate::prelude::{ParserError, Token, TokenKind};

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
  pub fn classify_tokens(&self, tokens: &mut [Token]) -> Result<(), ParserError> {
    if tokens.is_empty() || tokens.iter().position(|t| t.kind == TokenKind::EndOfInput) == Some(0) {
      return Err(ParserError::NoInput);
    }
    // The first token should always be a verb.
    tokens[0].kind = TokenKind::Verb;
    // Find the prepositions in the tokens and classify accordingly.
    self.find_prepositions(tokens)?;
    Ok(())
  }

  /// Find the prepositions in the tokens and classify accordingly.
  pub fn find_prepositions(&self, tokens: &mut [Token]) -> Result<(), ParserError> {
    // If this sentence contains anything that looks like a preposition, we'll
    // need to process it.
    if let Some(index) = tokens.iter().position(|t| t.kind.is_preposition()) {
      self.process_first_preposition(tokens, index)?;
    }
    Ok(())
  }

  /// Process the first preposition in the tokens.
  pub fn process_first_preposition(&self, tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    // Find the last non-EOI token.
    // If it's the same as the first preposition, then it's actually an adverb.
    // Otherwise, this token should be an indirect object.
    let lnei = tokens.iter().rposition(|t| t.kind != TokenKind::EndOfInput).unwrap();
    if index == lnei {
      // It's actually an adverb.
      return Ok(());
    }
    // The last token should be an indirect object.
    self.process_presumed_noun(tokens, lnei)?;
    // The token before the first preposition should be the direct object.
    self.process_presumed_direct_object(tokens, index - 1)?;
    // Now that we've processed this preposition, we can move on to the next.
    self.process_other_prepositions(tokens, index + 1)?;
    Ok(())
  }

  /// Process the presumed direct object.
  pub fn process_presumed_direct_object(&self, tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    // This index had better always be a token.
    let lnk = tokens.get(index).map(|t| t.kind).unwrap();
    // Unless it's already something other than a noun, it should be a noun.
    if lnk.could_be_noun() {
      // If it's not a noun, make it one.
      tokens[index].kind = TokenKind::DirectObject;
      // The previous token is presumably an adjective, unless it's not.
      self.process_presumed_adjectives(tokens, index - 1)?;
    }
    Ok(())
  }

  /// Process other prepositions.
  pub fn process_other_prepositions(&self, tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    // Find the next preposition in the tokens.
    if let Some(rel_index) = tokens.iter().skip(index).position(|t| t.kind.is_preposition()) {
      // The index of the next preposition, corrected for the slice.
      let index = index + rel_index;
      self.process_secondary_preposition(tokens, index)?;
      self.process_other_prepositions(tokens, index + 1)?;
    }
    Ok(())
  }

  /// Process secondary preposition.
  pub fn process_secondary_preposition(&self, tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    self.process_presumed_noun(tokens, index - 1)?;
    Ok(())
  }

  /// Process the presumed noun.
  pub fn process_presumed_noun(&self, tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    if let Some(lnk) = tokens.get(index).map(|t| t.kind) {
      // Don't override things that already work as nouns.
      if lnk.could_be_noun() && !lnk.is_noun() {
        tokens[index].kind = TokenKind::Noun;
      }
      if lnk.can_follow_adjective() {
        println!("Processing presumed adjectives before token {:#?}", lnk);
        self.process_presumed_adjectives(tokens, index - 1)?;
      }
    }
    Ok(())
  }

  /// Process the presumed adjectives.
  pub fn process_presumed_adjectives(&self, tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    // We start at the specified index and work our way back.
    for i in (0..=index).rev() {
      // This should always be a valid index.
      let lnk = tokens.get(i).map(|t| t.kind).unwrap();
      if lnk.could_be_adjective() {
        tokens[i].kind = TokenKind::Adjective;
      } else if lnk.can_follow_adjective() {
        continue;
      } else {
        break;
      }
    }
    Ok(())
  }
}
