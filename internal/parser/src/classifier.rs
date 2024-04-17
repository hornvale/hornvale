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
  pub fn classify_tokens(&self, mut tokens: &mut [Token]) -> Result<(), ParserError> {
    if tokens.is_empty() || tokens.find_eoi() == Some(0) {
      return Err(ParserError::NoInput);
    }
    tokens.set_kind(0, TokenKind::Verb);
    self.find_prepositions(tokens)?;
    Ok(())
  }

  /// Find the prepositions in the tokens and classify accordingly.
  pub fn find_prepositions(&self, tokens: &mut [Token]) -> Result<(), ParserError> {
    if let Some(fp) = tokens.find_matching(|t| t.kind.is_preposition()) {
      self.process_first_preposition(tokens, fp)?;
    }
    Ok(())
  }

  /// Process the first preposition in the tokens.
  pub fn process_first_preposition(&self, tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    let lnei = tokens.rfind_not_eoi().unwrap();
    if index == lnei {
      return Ok(()); // It's actually an adverb.
    }
    self.process_presumed_direct_object(tokens, index - 1)?;
    self.process_presumed_noun(tokens, lnei)?;
    self.process_other_prepositions(tokens, index + 1)?;
    Ok(())
  }

  /// Process the presumed direct object.
  pub fn process_presumed_direct_object(&self, mut tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    if let Some(lnk) = tokens.get(index).map(|t| t.kind) {
      if lnk.could_be_noun() {
        tokens.set_kind(index, TokenKind::DirectObject);
        self.process_presumed_adjectives(tokens, index - 1)?;
      }
    }
    Ok(())
  }

  /// Process other prepositions.
  pub fn process_other_prepositions(&self, tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    if let Some(index) = tokens.iter().skip(index).position(|t| t.kind.is_preposition()) {
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
  pub fn process_presumed_noun(&self, mut tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    if let Some(lnk) = tokens.get(index).map(|t| t.kind) {
      // Don't override things that already work as nouns.
      if lnk.could_be_noun() && !lnk.is_noun() {
        tokens.set_kind(index, TokenKind::Noun);
      }
    }
    self.process_presumed_adjectives(tokens, index - 1)?;
    Ok(())
  }

  /// Process the presumed adjectives.
  pub fn process_presumed_adjectives(&self, mut tokens: &mut [Token], index: usize) -> Result<(), ParserError> {
    for i in (0..index).rev() {
      if let Some(lnk) = tokens.get(i).map(|t| t.kind) {
        eprintln!("{:#?}", lnk);
        if lnk.could_be_adjective() {
          tokens.set_kind(i, TokenKind::Adjective);
        } else if lnk.can_follow_adjective() {
          continue;
        } else {
          break;
        }
      } else {
        break;
      }
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::prelude::Scanner;
  use hornvale_test_utilities::prelude::*;

  fn test_string_classification(string: &str, expected: &[TokenKind]) {
    let mut scanner = Scanner::new(string);
    let mut tokens = scanner.scan_tokens().unwrap();
    let classifier = Classifier::new();
    classifier.classify_tokens(&mut tokens).unwrap();
    // println!("Expected: {:#?}", expected);
    // println!("Actual: {:#?}", tokens);
    for (i, kind) in expected.iter().enumerate() {
      assert_eq!(
        tokens[i].kind, *kind,
        "Token {} ({:?}) should be a {:?}",
        i, tokens[i], kind
      );
    }
  }

  #[test]
  fn test_classify_tokens1() {
    init();
    test_string_classification("quit", &[TokenKind::Verb]);
  }

  #[test]
  fn test_classify_tokens2() {
    init();
    test_string_classification("say 'hello'", &[TokenKind::Verb, TokenKind::StringLiteral]);
  }

  #[test]
  fn test_classify_tokens3() {
    init();
    test_string_classification("say \"hello\"", &[TokenKind::Verb, TokenKind::StringLiteral]);
  }

  #[test]
  fn test_classify_tokens4() {
    init();
    test_string_classification(
      "say 'hello' to farmer",
      &[
        TokenKind::Verb,
        TokenKind::StringLiteral,
        TokenKind::To,
        TokenKind::Noun,
      ],
    );
  }

  #[test]
  fn test_classify_tokens5() {
    init();
    test_string_classification(
      "say \"hello\" to farmer",
      &[
        TokenKind::Verb,
        TokenKind::StringLiteral,
        TokenKind::To,
        TokenKind::Noun,
      ],
    );
  }

  #[test]
  fn test_classify_tokens6() {
    init();
    test_string_classification(
      "say 'hello' to farmer's cow",
      &[
        TokenKind::Verb,
        TokenKind::StringLiteral,
        TokenKind::To,
        TokenKind::PossessiveDeterminer,
        TokenKind::Noun,
      ],
    );
  }

  #[test]
  fn test_classify_tokens7() {
    init();
    test_string_classification(
      "say \"hello\" to farmer's cow",
      &[
        TokenKind::Verb,
        TokenKind::StringLiteral,
        TokenKind::To,
        TokenKind::PossessiveDeterminer,
        TokenKind::Noun,
      ],
    );
  }

  #[test]
  fn test_classify_tokens8() {
    init();
    test_string_classification(
      "say to farmer \"hello\"",
      &[
        TokenKind::Verb,
        TokenKind::To,
        TokenKind::Word, // Probably fix this... should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens9() {
    init();
    test_string_classification(
      "say to nice farmer \"hello\"",
      &[
        TokenKind::Verb,
        TokenKind::To,
        TokenKind::Adjective,
        TokenKind::Word, // Probably fix this... should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens10() {
    init();
    test_string_classification(
      "say to lily-livered farmer's cow \"hello\"",
      &[
        TokenKind::Verb,
        TokenKind::To,
        TokenKind::Adjective,
        TokenKind::PossessiveDeterminer,
        TokenKind::Word, // Probably fix this... should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens11() {
    init();
    test_string_classification(
      "say to nice lily-livered farmer's cow \"hello\"",
      &[
        TokenKind::Verb,
        TokenKind::To,
        TokenKind::Adjective,
        TokenKind::Adjective,
        TokenKind::PossessiveDeterminer,
        TokenKind::Word, // Probably fix this... should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens12() {
    init();
    test_string_classification(
      "say to nice, lily-livered farmer's cow \"hello\"",
      &[
        TokenKind::Verb,
        TokenKind::To,
        TokenKind::Adjective,
        TokenKind::Comma,
        TokenKind::Adjective,
        TokenKind::PossessiveDeterminer,
        TokenKind::Word, // Probably fix this... should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens13() {
    init();
    test_string_classification(
      "say to lily-livered nice farmer's cow \"hello\"",
      &[
        TokenKind::Verb,
        TokenKind::To,
        TokenKind::Adjective,
        TokenKind::Adjective,
        TokenKind::PossessiveDeterminer,
        TokenKind::Word, // Probably fix this... should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens14() {
    init();
    test_string_classification(
      "say to lily-livered, nice farmer's cow \"hello\"",
      &[
        TokenKind::Verb,
        TokenKind::To,
        TokenKind::Adjective,
        TokenKind::Comma,
        TokenKind::Adjective,
        TokenKind::PossessiveDeterminer,
        TokenKind::Word, // Probably fix this... should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens15() {
    init();
    test_string_classification(
      "say to farmer's cow 'hello'",
      &[
        TokenKind::Verb,
        TokenKind::To,
        TokenKind::PossessiveDeterminer,
        TokenKind::Word, // Should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens16() {
    init();
    test_string_classification(
      "tell farmer \"I know about you and the chicken.\"",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be a noun.
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens17() {
    init();
    test_string_classification(
      "tell farmer, \"I know about you and the pumpkin.\"",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be a noun.
        TokenKind::Comma,
        TokenKind::StringLiteral,
      ],
    );
  }

  #[test]
  fn test_classify_tokens18() {
    init();
    test_string_classification(
      "take sword",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens19() {
    init();
    test_string_classification(
      "take sword and shield",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be a noun.
        TokenKind::And,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens20() {
    init();
    test_string_classification(
      "take sword, shield",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be a noun.
        TokenKind::Comma,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens21() {
    init();
    test_string_classification(
      "take 5 coins",
      &[
        TokenKind::Verb,
        TokenKind::NumberLiteral,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens22() {
    init();
    test_string_classification(
      "take 3rd coin",
      &[
        TokenKind::Verb,
        TokenKind::Ordinal,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens23() {
    init();
    test_string_classification("take all", &[TokenKind::Verb, TokenKind::All]);
  }

  #[test]
  fn test_classify_tokens24() {
    init();
    test_string_classification("take all here", &[TokenKind::Verb, TokenKind::All, TokenKind::Here]);
  }

  #[test]
  fn test_classify_tokens25() {
    init();
    test_string_classification("north", &[TokenKind::Verb]);
  }

  #[test]
  fn test_classify_tokens26() {
    init();
    test_string_classification("go north", &[TokenKind::Verb, TokenKind::North]);
  }

  #[test]
  fn test_classify_tokens27() {
    init();
    test_string_classification("go to north", &[TokenKind::Verb, TokenKind::To, TokenKind::North]);
  }

  #[test]
  fn test_classify_tokens28() {
    init();
    test_string_classification("look around", &[TokenKind::Verb, TokenKind::Around]);
  }

  #[test]
  fn test_classify_tokens29() {
    init();
    test_string_classification("look north", &[TokenKind::Verb, TokenKind::North]);
  }

  #[test]
  fn test_classify_tokens30() {
    init();
    test_string_classification("look at north", &[TokenKind::Verb, TokenKind::At, TokenKind::North]);
  }

  #[test]
  fn test_classify_tokens31() {
    init();
    test_string_classification(
      "look behind curtain",
      &[TokenKind::Verb, TokenKind::Behind, TokenKind::Noun],
    );
  }

  #[test]
  fn test_classify_tokens32() {
    init();
    test_string_classification(
      "look under stove",
      &[TokenKind::Verb, TokenKind::Under, TokenKind::Noun],
    );
  }

  #[test]
  fn test_classify_tokens33() {
    init();
    test_string_classification("look in box", &[TokenKind::Verb, TokenKind::In, TokenKind::Noun]);
  }

  #[test]
  fn test_classify_tokens34() {
    init();
    test_string_classification(
      "turn lantern on",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be a noun.
        TokenKind::On,
      ],
    );
  }

  #[test]
  fn test_classify_tokens35() {
    init();
    test_string_classification(
      "turn radio up",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be a noun.
        TokenKind::Up,
      ],
    );
  }

  #[test]
  fn test_classify_tokens36() {
    init();
    test_string_classification("turn on lantern", &[TokenKind::Verb, TokenKind::On, TokenKind::Noun]);
  }

  #[test]
  fn test_classify_tokens37() {
    init();
    test_string_classification(
      "turn up radio",
      &[
        TokenKind::Verb,
        TokenKind::Up,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens38() {
    init();
    test_string_classification("attack him", &[TokenKind::Verb, TokenKind::Him]);
  }

  #[test]
  fn test_classify_tokens39() {
    init();
    test_string_classification(
      "take hers",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Correct; this is too vague.
      ],
    );
  }

  #[test]
  fn test_classify_tokens40() {
    init();
    test_string_classification(
      "get mine",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Correct; this is too vague.
      ],
    );
  }

  #[test]
  fn test_classify_tokens41() {
    init();
    test_string_classification(
      "take this sword",
      &[
        TokenKind::Verb,
        TokenKind::This,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens42() {
    init();
    test_string_classification(
      "take each sword",
      &[
        TokenKind::Verb,
        TokenKind::Each,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens43() {
    init();
    test_string_classification(
      "take every sword",
      &[
        TokenKind::Verb,
        TokenKind::Every,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens44() {
    init();
    test_string_classification(
      "kill troll",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be a noun.
      ],
    );
  }

  #[test]
  fn test_classify_tokens45() {
    init();
    test_string_classification(
      "kill troll with sword",
      &[
        TokenKind::Verb,
        TokenKind::DirectObject,
        TokenKind::With,
        TokenKind::Noun,
      ],
    );
  }

  #[test]
  fn test_classify_tokens46() {
    init();
    test_string_classification(
      "give money to elf",
      &[TokenKind::Verb, TokenKind::DirectObject, TokenKind::To, TokenKind::Noun],
    );
  }

  #[test]
  fn test_classify_tokens47() {
    init();
    test_string_classification(
      "take red cube and green cylinder",
      &[
        TokenKind::Verb,
        TokenKind::Word, // Should be an adjective.
        TokenKind::Word, // Should be a noun.
        TokenKind::And,
        TokenKind::Word, // Should be an adjective.
        TokenKind::Word, // Should be a noun.
      ],
    );
  }
}

// Test cases:
// turn up radio
// attack him
// take hers
// get mine
// take this sword
// take each sword
// take every sword
// kill troll
// kill troll with sword
// give money to elf
// take red cube and green cylinder
// take red cube, green cylinder
// take red cube, green cylinder, and yellow prism
// look at red-eyed goblin
// look at goblin's club
// look at red-eyed goblin's club
// read print
// read print on kettle
// read print on underside of kettle
// read print on underside of kettle on stove
// read fine print on glowing underside of whistling kettle on hot stove
// remember goblin as franklin
// remember red-eyed, shining-haired goblin as franklin
// steal from elf
// steal gold from elf's moneybag
// give elf coin
// give elf poisoned coin
