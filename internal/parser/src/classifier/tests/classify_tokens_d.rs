use super::*;

#[test]
fn test_classify_tokens61() {
  init();
  test_string_classification("steal from elf", &[TokenKind::Verb, TokenKind::From, TokenKind::Noun]);
}

#[test]
fn test_classify_tokens62() {
  init();
  test_string_classification(
    "steal gold from elf's moneybag",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::From,
      TokenKind::PossessiveDeterminer,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens63() {
  init();
  test_string_classification(
    "give elf coin",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
      TokenKind::Word, // Should be a noun.
    ],
  );
}

#[test]
fn test_classify_tokens64() {
  init();
  test_string_classification(
    "give elf poisoned coin",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
      TokenKind::Word, // Should be an adjective.
      TokenKind::Word, // Should be a noun.
    ],
  );
}

#[test]
fn test_classify_tokens65() {
  init();
  test_string_classification(
    "give poisoned coin to elf",
    &[
      TokenKind::Verb,
      TokenKind::Adjective,
      TokenKind::DirectObject,
      TokenKind::To,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens66() {
  init();
  test_string_classification(
    "sneak poisoned coin into elf's moneybag",
    &[
      TokenKind::Verb,
      TokenKind::Adjective,
      TokenKind::DirectObject,
      TokenKind::Into,
      TokenKind::PossessiveDeterminer,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens67() {
  init();
  test_string_classification(
    "look at farmer's neighbor's wife's cat",
    &[
      TokenKind::Verb,
      TokenKind::At,
      TokenKind::PossessiveDeterminer,
      TokenKind::PossessiveDeterminer,
      TokenKind::PossessiveDeterminer,
      TokenKind::Noun,
    ],
  );
}
