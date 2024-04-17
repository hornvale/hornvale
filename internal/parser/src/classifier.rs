use crate::prelude::{
  Adjective, Adverb, DemonstrativeDeterminer, Direction, DistributiveDeterminer, ParserError, PossessiveDeterminer,
  PossessivePronoun, Preposition, Pronoun, Token, TokenKind,
};

/// A classifier that can be used to determine the type of a word.
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
    if tokens.is_empty() {
      return Err(ParserError::NoInput);
    }
    if tokens[0].kind != TokenKind::Word {
      return Err(ParserError::NoVerb);
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
      if !self.might_be_preposition(&tokens[preposition_index].lexeme) {
        index += 1;
        continue;
      }
      if !first_found {
        self.set_token_kind(tokens, preposition_index - 1, TokenKind::DirectObject)?;
        first_found = true;
      }
      self.set_token_kind(tokens, preposition_index, TokenKind::Preposition)?;
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
      if token.kind == TokenKind::Word && Preposition::try_from(&*token.lexeme).is_ok() {
        return Some(index);
      }
    }
    None
  }

  /// Might this word be an adjective?
  pub fn might_be_adjective(&self, word: &str) -> bool {
    Adjective::fits(word)
  }

  /// Might this word be an adverb?
  pub fn might_be_adverb(&self, word: &str) -> bool {
    Adverb::fits(word)
  }

  /// Might this word be a demonstrative determiner?
  pub fn might_be_demonstrative_determiner(&self, word: &str) -> bool {
    DemonstrativeDeterminer::fits(word)
  }

  /// Might this word be a direction?
  pub fn might_be_direction(&self, word: &str) -> bool {
    Direction::fits(word)
  }

  /// Might this word be a distributive determiner?
  pub fn might_be_distributive_determiner(&self, word: &str) -> bool {
    DistributiveDeterminer::fits(word)
  }

  /// Might this word be a possessive determiner?
  pub fn might_be_possessive_determiner(&self, word: &str) -> bool {
    PossessiveDeterminer::fits(word)
  }

  /// Might this word be a possessive pronoun?
  pub fn might_be_possessive_pronoun(&self, word: &str) -> bool {
    PossessivePronoun::fits(word)
  }

  /// Might this word be a preposition?
  pub fn might_be_preposition(&self, word: &str) -> bool {
    Preposition::fits(word)
  }

  /// Might this word be a pronoun?
  pub fn might_be_pronoun(&self, word: &str) -> bool {
    Pronoun::fits(word)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_classifier() {
    let classifier = Classifier::new();
    // assert!(classifier.might_be_adjective("quick"));
    // assert!(classifier.might_be_adverb("quickly"));
    assert!(classifier.might_be_demonstrative_determiner("this"));
    assert!(classifier.might_be_direction("north"));
    assert!(classifier.might_be_distributive_determiner("each"));
    assert!(classifier.might_be_possessive_determiner("my"));
    assert!(classifier.might_be_possessive_pronoun("mine"));
    assert!(classifier.might_be_preposition("in"));
    assert!(classifier.might_be_pronoun("I"));
  }

  #[test]
  fn test_classifier_find_first_preposition() {
    let classifier = Classifier::new();
    let tokens = vec![
      Token {
        kind: TokenKind::Word,
        lexeme: "I".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "am".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "in".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "the".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "house".to_string(),
      },
      Token {
        kind: TokenKind::EndOfInput,
        lexeme: "".to_string(),
      },
    ];
    assert_eq!(classifier.find_first_preposition(&tokens), Some(2));
  }

  #[test]
  fn test_classifier_find_first_preposition_none() {
    let classifier = Classifier::new();
    let tokens = vec![
      Token {
        kind: TokenKind::Word,
        lexeme: "I".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "am".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "the".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "house".to_string(),
      },
      Token {
        kind: TokenKind::EndOfInput,
        lexeme: "".to_string(),
      },
    ];
    assert_eq!(classifier.find_first_preposition(&tokens), None);
  }
}
