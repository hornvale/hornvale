use crate::prelude::*;

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
    self.assert_non_empty(tokens)?;
    // The first token should always be a verb or a magic word.
    if !tokens[0].kind.is_magic_word() {
      if !tokens[0].kind.could_be_verb() {
        return Err(CommandError::NoVerb);
      }
      tokens[0].kind = TokenKind::Word(Word::Verb);
    }
    // Find the prepositions in the tokens and classify accordingly.
    let prepositions_found = self.find_prepositions(tokens)?;
    if !prepositions_found {
      // If none were found, we likely don't have an indirect object; so the
      // last token that can be a noun should be the direct object.
      if let Some(lni) = tokens.iter().rposition(|t| t.kind.could_be_noun()) {
        self.process_presumed_direct_object(tokens, lni)?;
      }
    }
    if tokens.len() == 4 {
      // This might also be a sentence of the form <verb> <noun> <adverb> where
      // the adverb is also a direction (e.g. "turn radio up", "turn lamp on").
      self.find_directional_adverbs(tokens)?;
    }
    Ok(())
  }

  /// Find the prepositions in the tokens and classify accordingly.
  pub fn find_prepositions(&self, tokens: &mut [Token]) -> Result<bool, CommandError> {
    // If this sentence contains anything that looks like a preposition, we'll
    // need to process it.
    if let Some(index) = tokens.iter().position(|t| t.kind.is_command_modifier()) {
      self.process_first_preposition(tokens, index)?;
      return Ok(true);
    }
    Ok(false)
  }

  /// Process the first preposition in the tokens.
  pub fn process_first_preposition(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    // Find the last non-EOI token.
    let lnei = tokens.iter().rposition(|t| t.kind != TokenKind::EndOfInput).unwrap();
    // If it's the same as the first preposition, then it's actually an adverb.
    if index == lnei {
      // It's actually an adverb, and the token before it is likely the direct object.
      self.process_presumed_direct_object(tokens, index - 1)?;
      return Ok(());
    }
    // Otherwise, this token should usually be an indirect object, but there
    // are some exceptions; so we find the last word after the preposition that
    // could be a noun and mark it.
    let lni = tokens.iter().rposition(|t| t.kind.could_be_noun()).unwrap();
    if lni > index {
      self.process_presumed_noun(tokens, lni)?;
    }
    // The token before the first preposition should be the direct object.
    self.process_presumed_direct_object(tokens, index - 1)?;
    // Now that we've processed this preposition, we can move on to the next.
    self.process_other_prepositions(tokens, index + 1)?;
    Ok(())
  }

  /// Process the presumed direct object.
  pub fn process_presumed_direct_object(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    // This index had better always be a token.
    let lnk = tokens.get(index).map(|t| t.kind).unwrap();
    // Unless it's already something other than a noun, it should be a noun.
    if lnk.could_be_noun() && !lnk.is_noun() {
      // If it's not a noun, make it one.
      tokens[index].kind = TokenKind::Word(Word::Noun);
      // The previous token is presumably an adjective, unless it's not.
      self.process_presumed_adjectives(tokens, index - 1)?;
    }
    Ok(())
  }

  /// Process other prepositions.
  pub fn process_other_prepositions(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    // Find the next preposition in the tokens.
    if let Some(rel_index) = tokens.iter().skip(index).position(|t| t.kind.is_command_modifier()) {
      // The index of the next preposition, corrected for the slice.
      let index = index + rel_index;
      self.process_secondary_preposition(tokens, index)?;
      self.process_other_prepositions(tokens, index + 1)?;
    }
    Ok(())
  }

  /// Process secondary preposition.
  pub fn process_secondary_preposition(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    self.process_presumed_noun(tokens, index - 1)?;
    Ok(())
  }

  /// Process the presumed noun.
  pub fn process_presumed_noun(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    if let Some(lnk) = tokens.get(index).map(|t| t.kind) {
      // Don't override things that already work as nouns.
      if lnk.could_be_noun() && !lnk.is_noun() {
        tokens[index].kind = TokenKind::Word(Word::Noun);
      }
      // We need to recheck the kind of this token.
      if tokens[index].kind.can_follow_adjective() {
        self.process_presumed_adjectives(tokens, index - 1)?;
      }
    }
    Ok(())
  }

  /// Process the presumed adjectives.
  pub fn process_presumed_adjectives(&self, tokens: &mut [Token], index: usize) -> Result<(), CommandError> {
    // We start at the specified index and work our way back.
    for i in (0..=index).rev() {
      // This should always be a valid index.
      let lnk = tokens.get(i).map(|t| t.kind).unwrap();
      if lnk.could_be_adjective() {
        tokens[i].kind = TokenKind::Word(Word::Adjective);
      } else if lnk.is_conjunction() || lnk == TokenKind::Character(Character::Comma) {
        // The word prior to this should be a noun, but we need to check for an
        // Oxford comma.
        if i > 0 && tokens[i - 1].kind == TokenKind::Character(Character::Comma) {
          self.process_presumed_noun(tokens, i - 2)?;
        } else {
          self.process_presumed_noun(tokens, i - 1)?;
        }
        break;
      } else if lnk.can_follow_adjective() {
        continue;
      } else {
        break;
      }
    }
    Ok(())
  }

  /// Find directional adverbs.
  pub fn find_directional_adverbs(&self, tokens: &mut [Token]) -> Result<(), CommandError> {
    if tokens.len() != 4 {
      // This only works for sentences of the form <verb> <noun> <adverb> <eoi>.
      return Ok(());
    }
    // This token should already have the information necessary to classify it.
    if tokens[2].kind.is_direction() && tokens[2].kind.is_command_modifier() {
      self.process_presumed_direct_object(tokens, 1)?;
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
}
